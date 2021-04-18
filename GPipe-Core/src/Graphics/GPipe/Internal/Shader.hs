{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Graphics.GPipe.Internal.Shader (
    Shader(..),
    ShaderM(..),
    ShaderState(..),
    CompiledShader,
    Render(..),
    getNewName,
    tellDrawcall,
    askUniformAlignment,
    modifyRenderIO,
    compileShader,
    mapShader,
    guard',
    maybeShader,
    chooseShader,
    silenceShader
) where

import           Control.Applicative              (Alternative, (<|>))
import           Control.Monad                    (MonadPlus, forM)
import           Control.Monad.Exception          (MonadException)
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.List         (ListT (..))
import           Control.Monad.Trans.Reader       (ReaderT (..), ask)
import           Control.Monad.Trans.State        (State, get, modify, put,
                                                   runState)
import           Control.Monad.Trans.Writer.Lazy  (WriterT (..), execWriterT,
                                                   tell)
import           Data.Either                      (isLeft, isRight)
import           Data.List                        (find)
import           Data.Maybe                       (fromJust, isJust, isNothing)
import           Data.Monoid                      (All (..))
import           Graphics.GPipe.Internal.Buffer   (UniformAlignment,
                                                   getUniformAlignment)
import           Graphics.GPipe.Internal.Compiler (CompiledShader, Drawcall,
                                                   RenderIOState,
                                                   compileDrawcalls,
                                                   mapDrawcall,
                                                   mapRenderIOState,
                                                   newRenderIOState)
import           Graphics.GPipe.Internal.Context  (ContextHandler, ContextT,
                                                   Render (..),
                                                   liftNonWinContextIO)

{- Some wording & structure:

    Shader (with a majuscule) = "GPipeShader" => [(OpenGL program made of OpenGL shaders, condition)]

    When a Shader is compiled, it means that it is translated into OpenGL shader
    sources (with a context) which are compiled then linked in programs when
    wrapped into a rendering action which select the appropriate shader at
    runtime.
-}

-- More a "GPipeShaderEnvironmentContextConnector".
data ShaderState s = ShaderState
    Int -- Next name.
    (RenderIOState s)

newShaderState :: ShaderState s
newShaderState = ShaderState 1 newRenderIOState

{-
I'm not a big fan of the ShaderM monad, because its important part is its side
effects. A the end of the day, we always end up with a ShaderM os () and are
only interested in the 'told' drawcalls with the 'put' ShaderState. When a
ShaderM is not returning a () value, there are no drawcall (yet), the returned
value is mostly a work in progress constructed and the context of the ShaderM is
not really used (WriterT is not used in any cases). I'm probably talking outside
my level of profiency here, but I suspect it would have been cleaner & clearer
to separate the WriterT.
-}
newtype ShaderM s a = ShaderM
    (ReaderT
        UniformAlignment -- Meant to be retrieved using askUniformAlignment.
        (WriterT
            (   [IO (Drawcall s)] -- Produce a list of drawcalls (the IO is only here for SNMap)
            ,   s -> All -- Condition to execute the drawcalls (need to be a monoid such as the tuple could be too).
            )
            (ListT -- Needed to automatically derive MonadPlus and Application, but what else?
                (State
                    (ShaderState s)
                )
            )
        )
        a
    )
    deriving (MonadPlus, Monad, Alternative, Applicative, Functor)

-- Return a new name to be used as a key in RenderIOState. for a program,
-- shader, uniform, texture unit (sampler)… or something not related to a named
-- OpenGL object.
getNewName :: ShaderM s Int
getNewName = do
    ShaderState n r <- ShaderM $ lift $ lift $ lift get
    ShaderM $ lift $ lift $ lift $ put $ ShaderState (n+1) r
    return n

askUniformAlignment :: ShaderM s UniformAlignment
askUniformAlignment = ShaderM ask

modifyRenderIO :: (RenderIOState s -> RenderIOState s) -> ShaderM s ()
modifyRenderIO f = ShaderM $ lift $ lift $ lift $ modify (\(ShaderState a s) -> ShaderState a (f s))

tellDrawcall :: IO (Drawcall s) -> ShaderM s ()
tellDrawcall drawcall = ShaderM $ lift $ tell ([drawcall], mempty)

-- | The monad in which all GPU computations are done. 'Shader os s a' lives in
-- an object space 'os' and a context with format 'f', closing over an
-- environent of type 's'.
newtype Shader os s a = Shader (ShaderM s a)
    deriving (MonadPlus, Monad, Alternative, Applicative, Functor)

-- | Map the environment to a different environment and run a Shader in that sub
-- environment, returning it's result.
mapShader :: (s -> s') -> Shader os s' a -> Shader os s a
mapShader f (Shader (ShaderM m)) = Shader $ ShaderM $ do
    uniAl <- ask
    lift $ WriterT $ ListT $ do
        ShaderState x s <- get
        let (conditionalDrawcalls, ShaderState x' s') = runState (runListT (runWriterT (runReaderT m uniAl))) (ShaderState x newRenderIOState)
        put $ ShaderState x' (mapRenderIOState f s' s)
        return $ map (\(a, (drawcalls, test)) -> (a, (map (>>= (return . mapDrawcall f)) drawcalls, test . f))) conditionalDrawcalls

-- | Conditionally run the effects of a shader when a 'Maybe' value is 'Just'
-- something.
maybeShader :: (s -> Maybe s') -> Shader os s' () -> Shader os s ()
maybeShader f m = (guard' (isJust . f) >> mapShader (fromJust . f) m) <|> guard' (isNothing . f)

-- | Select one of two 'Shader' actions based on whether an 'Either' value is
-- 'Left' or 'Right'.
chooseShader :: (s -> Either s' s'') -> Shader os s' a -> Shader os s'' a -> Shader os s a
chooseShader f a b = (guard' (isLeft . f) >> mapShader (fromLeft . f) a) <|> (guard' (isRight . f) >> mapShader (fromRight . f) b) where
    fromLeft (Left x) = x
    fromRight (Right x) = x

-- | Discard all effects of a 'Shader' action (i.e., dont draw anything) and
-- just return the resulting value.
silenceShader :: Shader os s a -> Shader os s a
silenceShader (Shader (ShaderM m)) = Shader $ ShaderM $ do
    uniAl <- ask
    lift $ WriterT $ ListT $ do
        s <- get
        let (conditionalDrawcalls, s') = runState (runListT (runWriterT (runReaderT m uniAl))) s
        put s'
        return $ map (\ (a, (_, test)) -> (a, ([], test))) conditionalDrawcalls

-- | Like 'guard', but dependent on the 'Shaders' environment value. Since this
--   will be evaluated at shader run time, as opposed to shader compile time for
--   'guard', using this to do recursion will make 'compileShader' diverge. You
--   can break that divergence by combining it with a normal 'guard' and a
--   maximum loop count.
guard' :: (s -> Bool) -> Shader os s ()
guard' f = Shader $ ShaderM $ lift $ tell (mempty, All . f)

-- | Compiles a shader into a 'CompiledShader'. This action will usually take a
-- second or more, so put it during a loading sequence or something.
--
-- May throw a 'GPipeException' if the graphics driver doesn't support something
-- in this shader (e.g. too many interpolated floats sent between a vertex and a
-- fragment shader)
compileShader :: (ContextHandler ctx, MonadIO m, MonadException m) => Shader os s () -> ContextT ctx os m (CompiledShader os s)
compileShader (Shader (ShaderM m)) = do

    uniformAlignment <- liftNonWinContextIO getUniformAlignment
    let (conditionalDrawcalls, ShaderState _ state) = runState (runListT (execWriterT (runReaderT m uniformAlignment))) newShaderState
    conditionalRenderers <- forM conditionalDrawcalls $ \ (drawcalls, test) -> do
        renderer <- compileDrawcalls drawcalls state
        return (renderer, test)

    -- Return a wrapping renderer which select the first renderer for the
    -- environment before using it. Remember: renderer <=> CompiledDrawcall <=> CompiledShader
    return $ \ environment -> case find (\ (_, test) -> getAll (test environment)) conditionalRenderers of
        Nothing -> error "render: Shader evaluated to mzero (no Shader selected)"
        Just (renderer, _) -> renderer environment
