{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE TupleSections              #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Graphics.GPipe.Internal.PrimitiveStream where

import           Control.Arrow                          (Arrow (arr, first),
                                                         Kleisli (Kleisli),
                                                         returnA)
import           Control.Monad.Trans.Class              (MonadTrans (lift))
import           Control.Monad.Trans.Reader             (Reader, ask, runReader)
import           Control.Monad.Trans.State.Strict       (State,
                                                         StateT (runStateT),
                                                         execState, get, modify,
                                                         put)
import           Data.Text.Lazy                         (Text)
import           Graphics.GPipe.Internal.Buffer         (B (..), B2 (..),
                                                         B3 (..), B4 (..),
                                                         Buffer (bufTransformFeedback),
                                                         BufferFormat (HostFormat),
                                                         Normalized (..))
import           Graphics.GPipe.Internal.Compiler       (Binding,
                                                         RenderIOState (inputArrayToRenderIO))
import           Graphics.GPipe.Internal.Context        (VAOKey (VAOKey))
import           Graphics.GPipe.Internal.Expr           (ExprM, S (S),
                                                         SType (..), V, VFloat,
                                                         VInt, VWord, scalarS',
                                                         useUniform, useVInput,
                                                         vec2S, vec3S, vec4S)
import           Graphics.GPipe.Internal.PrimitiveArray (IndexArray (iArrName, indexArrayLength, indexType, offset, restart),
                                                         Points,
                                                         PrimitiveArray (getPrimitiveArray),
                                                         PrimitiveArrayInt (..),
                                                         PrimitiveTopology (toGLtopology, toPrimitiveSize))
import           Graphics.GPipe.Internal.Shader         (Shader (..), ShaderM,
                                                         askUniformAlignment,
                                                         getNewName,
                                                         modifyRenderIO)
import           Prelude                                hiding (id, length, (.))
-- This import is only needed for the unused uniform alternate implementation.
import           Control.Category                       (Category (..))
import           Control.Monad                          (void, when)
import           Graphics.GPipe.Internal.Uniform        (OffsetToSType,
                                                         buildUDecl)
#if __GLASGOW_HASKELL__ < 804
import           Data.Semigroup                         (Semigroup (..))
#endif
import           Data.Int                               (Int16, Int32, Int8)
import qualified Data.IntMap                            as Map
import           Data.IntMap.Lazy                       (insert)
import           Data.Word                              (Word16, Word32, Word8)
import           Foreign.Marshal                        (alloca)
import           Foreign.Ptr                            (Ptr, castPtr,
                                                         intPtrToPtr, plusPtr)
import           Foreign.Storable                       (Storable (peek, poke, sizeOf))

import           Data.IORef                             (readIORef)
import           Data.Maybe                             (fromMaybe)
import           Foreign.Marshal.Utils                  (fromBool)
import           Graphics.GL.Core45
import           Graphics.GL.Types                      (GLuint)
import           Linear.Affine                          (Point (..))
import           Linear.Plucker                         (Plucker (..))
import           Linear.Quaternion                      (Quaternion (..))
import           Linear.V0                              (V0 (..))
import           Linear.V1                              (V1 (..))
import           Linear.V2                              (V2 (..))
import           Linear.V3                              (V3 (..))
import           Linear.V4                              (V4 (..))

-- Originally named DrawCallName and later in the code as PrimN. I've sticked
-- with the latter, because it's more logical.
type PrimitiveName = Int

-- The size of the special uniform buffer object used for uniform inputs No such
-- buffer is allocated if not used and it is only used when literal values are
-- used in a primitive stream transmitted using a dedicated uniform buffer
-- object.
type USize = Int

data PrimitiveStreamData = PrimitiveStreamData PrimitiveName USize

-- Should be renamed as VertexStream. There a reason why the underlying VAO
-- behind is called a VAO. Beside, it forces me to name GeometryStream the real
-- PrimitiveStream. I don’t need why 't' was carried alongside but it is now
-- required for adding Geometry shaders.
--
-- | A @'PrimitiveStream' t a @ is a stream of primitives of
-- type @t@ where the vertices are values of type @a@. You
--   can operate a stream's vertex values using the 'Functor' instance (this will result in a shader running on the GPU).
--   You may also append 'PrimitiveStream's using the 'Monoid' instance, but if possible append the origin 'PrimitiveArray's instead, as this will create more optimized
--   draw calls.
newtype PrimitiveStream t a = PrimitiveStream [(a, (Maybe PointSize, PrimitiveStreamData))] deriving (Semigroup, Monoid)

instance Functor (PrimitiveStream t) where
        fmap f (PrimitiveStream xs) = PrimitiveStream $ map (first f) xs

-- | This class constraints which buffer types can be turned into vertex values, and what type those values have.
class VertexInput a where
    -- | The type the buffer value will be turned into once it becomes a vertex value.
    type VertexFormat a
    -- | An arrow action that turns a value from it's buffer representation to it's vertex representation. Use 'toVertex' from
    --   the GPipe provided instances to operate in this arrow. Also note that this arrow needs to be able to return a value
    --   lazily, so ensure you use
    --
    --  @proc ~pattern -> do ...@.
    toVertex :: ToVertex a (VertexFormat a)

type UniOffset = Int

-- | The arrow type for 'toVertex'.
data ToVertex a b = ToVertex

    -- To set the uniform buffer content.
    -- It is only used by the "uniform vertex" which is deprecated/experimental (see toUniformVertex).
    -- As it, it is not currently in use.
    -- Note: output value 'b' is not relevant.
    !(  Kleisli
        (   StateT (Ptr ()) IO
        ) a b)

    -- To declare the input variable in the shader.
    !(  Kleisli
        (   StateT
            (   Int -- The number of components in the stored type (eg. would 2 for a B2/V2).
            ,   UniOffset -- Offset to the uniform?
            ,   OffsetToSType -- Offset -> SType?
            )
            (   Reader
                (   Int {- offset -} -> ExprM Text -- Ends up calling useVInput and returning the input variable name (eg. 'in123').
                )
            )
        ) a b)

    -- To bind the underlying VAO for the vertex buffer (see PrimitiveArray).
    -- Note: output value 'b' is not relevant.
    !(  Kleisli
        (   State
            [   Binding -> (IO VAOKey, IO ())
            ]
        ) a b)

instance Category ToVertex where
    {-# INLINE id #-}
    id = ToVertex id id id
    {-# INLINE (.) #-}
    ToVertex a b c . ToVertex x y z = ToVertex (a.x) (b.y) (c.z)

instance Arrow ToVertex where
    {-# INLINE arr #-}
    arr f = ToVertex (arr f) (arr f) (arr f)
    {-# INLINE first #-}
    first (ToVertex a b c) = ToVertex (first a) (first b) (first c)

-- | Create a primitive stream from a primitive array provided from the shader environment.
-- TODO No way to constraint 'b' a bit more?
toPrimitiveStream :: forall os f s a p. (PrimitiveTopology p, VertexInput a) => (s -> PrimitiveArray p a) -> Shader os s (PrimitiveStream p (VertexFormat a))
toPrimitiveStream = toPrimitiveStream' Nothing

toPrimitiveStream' :: forall os f s a b p. (PrimitiveTopology p, VertexInput a) => Maybe (s -> Buffer os b) -> (s -> PrimitiveArray p a) -> Shader os s (PrimitiveStream p (VertexFormat a))
toPrimitiveStream' getFeedbackBuffer sf = Shader $ do

    -- Get a unique (OpenGL) name for this shader by updating the 'ShaderState' (a pair of the next name and a 'RenderIOState s') from the ReaderT/WriterT/ListM/State.
    n <- getNewName

    -- Get the RO uniform alignment from the ReaderT
    uniAl <- askUniformAlignment -- uniAl is not used around…

    let
        -- The explosive input value is only here to ensure that the mf arrow is lazy.
        err = error "toPrimitiveStream is creating values that are dependant on the actual HostFormat values, this is not allowed since it doesn't allow static creation of shaders"
        -- Is 'offToStype' really built this way or it is not used at all?
        (x, (_, uSize, offToStype)) = runReader
            (runStateT (makeV err) (0, 0, mempty))
            (useUniform (buildUDecl offToStype) 0) -- 0 is special blockname for the one used by primitive stream

    -- Register the actual OpenGL bind and draw commands for this shader name.
    doForInputArray n $ \s ->
        let
            fb = getFeedbackBuffer >>= \g -> return (g s)
            ps = getPrimitiveArray (sf s)
        in
            map (drawcall . (fb,)) ps

    return $ PrimitiveStream [(x, (Nothing, PrimitiveStreamData n uSize))]

    where
        ToVertex
            (Kleisli uWriter) -- To set the uniform content (for the literal values, not the one buffered).
            (Kleisli makeV) -- To create (and declare) the input variable in the shader.
            (Kleisli makeBind) -- To construct the VAO.
            = toVertex :: ToVertex a (VertexFormat a) -- Select the ToVertex to translate 'a' into a 'VertexFormat a'.

        drawcall (Just feedbackBuffer, PrimitiveArraySimple p l s a) binds = (attribs a binds, do
            -- liftIO $ hPutStrLn stderr $ "drawcall 1"
            Just (tfName, tfqName) <- readIORef (bufTransformFeedback feedbackBuffer)
            if False
                -- The bigger the amount of vertice, the faster a "*ERROR* Waiting for fences timed out!" will happen…
                -- For small amounts, works fine.
                then glDrawTransformFeedback (toGLtopology p) tfName
                else do
                    -- Is it costly too do it repeatedly?
                    l' <- (fromIntegral (toPrimitiveSize p) *) <$> alloca (\ptr -> do
                        glGetQueryObjectiv tfqName GL_QUERY_RESULT ptr
                        peek ptr)
                    -- liftIO $ hPutStrLn stderr $ "queried vertice count: " ++ show l'
                    when (l' > 0) $ do
                        glDrawArrays (toGLtopology p) (fromIntegral s) l'
            )
        drawcall (Just feedbackBuffer, PrimitiveArrayInstanced p il l s a) binds = (attribs a binds, do
            -- liftIO $ hPutStrLn stderr $ "drawcall 2"
            Just (tfName, _) <- readIORef (bufTransformFeedback feedbackBuffer)
            glDrawTransformFeedbackInstanced (toGLtopology p) tfName (fromIntegral il))

        drawcall (Nothing, PrimitiveArraySimple p l s a) binds = (attribs a binds, do
            -- liftIO $ hPutStrLn stderr $ "drawcall 3"
            glDrawArrays (toGLtopology p) (fromIntegral s) (fromIntegral l))
        drawcall (Nothing, PrimitiveArrayIndexed p i s a) binds = (attribs a binds, do
            -- liftIO $ hPutStrLn stderr $ "drawcall 4"
            bindIndexBuffer i
            glDrawElementsBaseVertex (toGLtopology p) (fromIntegral $ indexArrayLength i) (indexType i) (intPtrToPtr $ fromIntegral $ offset i * glSizeOf (indexType i)) (fromIntegral s))
        drawcall (Nothing, PrimitiveArrayInstanced p il l s a) binds = (attribs a binds, do
            -- liftIO $ hPutStrLn stderr $ "drawcall 5"
            glDrawArraysInstanced (toGLtopology p) (fromIntegral s) (fromIntegral l) (fromIntegral il))
        drawcall (Nothing, PrimitiveArrayIndexedInstanced p i il s a) binds = (attribs a binds, do
            -- liftIO $ hPutStrLn stderr $ "drawcall 6"
            bindIndexBuffer i
            glDrawElementsInstancedBaseVertex (toGLtopology p) (fromIntegral $ indexArrayLength i) (indexType i) (intPtrToPtr $ fromIntegral $ offset i * glSizeOf (indexType i)) (fromIntegral il) (fromIntegral s))

        bindIndexBuffer i = do
            case restart i of
                Just x -> do
                    glEnable GL_PRIMITIVE_RESTART
                    glPrimitiveRestartIndex (fromIntegral x)
                Nothing ->
                    glDisable GL_PRIMITIVE_RESTART
            bname <- readIORef (iArrName i)
            glBindBuffer GL_ELEMENT_ARRAY_BUFFER bname
        glSizeOf GL_UNSIGNED_INT = 4
        glSizeOf GL_UNSIGNED_SHORT = 2
        glSizeOf GL_UNSIGNED_BYTE = 1
        glSizeOf _ = error "toPrimitiveStream: Unknown indexArray type"

        assignIxs :: Int -> Binding -> [Int] -> [Binding -> (IO VAOKey, IO ())] -> [(IO VAOKey, IO ())]
        assignIxs n ix xxs@(x:xs) (f:fs) | x == n    = f ix : assignIxs (n+1) (ix+1) xs fs
                                         | otherwise = assignIxs (n+1) ix xxs fs
        assignIxs _ _ [] _ = []
        assignIxs _ _ _ _ = error "Too few attributes generated in toPrimitiveStream"

        attribs a (binds, uBname, uSize) = let
                              bindsAssoc = execState (makeBind a) [] -- (_,bindsAssoc) = runState (makeBind a) []
                              (ioVaokeys, ios) = unzip $ assignIxs 0 0 binds $ reverse bindsAssoc
                          in (writeUBuffer uBname uSize a >> sequence ioVaokeys, sequence_ ios)

        -- Modify the (OpenGL) shader state which is the set of OpenGL commands to run to draw this shader.
        doForInputArray :: Int -> (s -> [([Binding], GLuint, Int) -> ((IO [VAOKey], IO ()), IO ())]) -> ShaderM s ()
        doForInputArray n io = modifyRenderIO (\s -> s { inputArrayToRenderIO = insert n io (inputArrayToRenderIO s) } ) -- modifyRenderIO changes the ShaderState

        writeUBuffer _ 0 _ = return () -- If the uniform buffer is size 0 there is no buffer
        writeUBuffer bname size a = do
            _ <- fail "Cannot happen!"
            glBindBuffer GL_COPY_WRITE_BUFFER bname
            ptr <- glMapBufferRange GL_COPY_WRITE_BUFFER 0 (fromIntegral size) (GL_MAP_WRITE_BIT + GL_MAP_INVALIDATE_BUFFER_BIT)
            void $ runStateT (uWriter a) ptr
            void $ glUnmapBuffer GL_COPY_WRITE_BUFFER

data InputIndices = InputIndices {
        inputVertexID   :: VInt,
        inputInstanceID :: VInt
    }

-- | Like 'fmap', but where the vertex and instance IDs are provided as arguments as well.
withInputIndices :: (a -> InputIndices -> b) -> PrimitiveStream p a -> PrimitiveStream p b
withInputIndices f = fmap (\a -> f a (InputIndices (scalarS' "gl_VertexID") (scalarS' "gl_InstanceID")))

type PointSize = VFloat
-- | Like 'fmap', but where each point's size is provided as arguments as well, and a new point size is set for each point in addition to the new vertex value.
--
--   When a 'PrimitiveStream' of 'Points' is created, all points will have the default size of 1.
withPointSize :: (a -> PointSize -> (b, PointSize)) -> PrimitiveStream Points a -> PrimitiveStream Points b
withPointSize f (PrimitiveStream xs) = PrimitiveStream $ map (\(a, (ps, d)) -> let (b, ps') = f a (fromMaybe (scalarS' "1") ps) in (b, (Just ps', d))) xs

append :: Monad m => a -> StateT [a] m ()
append x = modify (x:)

-- Why x which is not needed? Is n < x true?
makeVertexF x f styp _ = do
    (n, uoffset, m) <- get
    put (n + 1, uoffset, m)
    return (f styp $ useVInput styp n)

makeBindVertexFx norm x typ b = do
    let combOffset = bStride b * bSkipElems b + bOffset b
    append (\ix ->
        (   do  bn <- readIORef $ bName b
                return $ VAOKey bn combOffset x norm (bInstanceDiv b)
        ,   do  bn <- readIORef $ bName b
                let ix' = fromIntegral ix
                glEnableVertexAttribArray ix'
                glBindBuffer GL_ARRAY_BUFFER bn
                glVertexAttribDivisor ix' (fromIntegral $ bInstanceDiv b)
                glVertexAttribPointer ix' x typ (fromBool norm) (fromIntegral $ bStride b) (intPtrToPtr $ fromIntegral combOffset))
        )
    return undefined

makeBindVertexFnorm = makeBindVertexFx True
makeBindVertexF = makeBindVertexFx False

makeVertexI x f styp _ = do
    (n, uoffset,m) <- get
    put (n + 1, uoffset,m)
    return (f styp $ useVInput styp n)

makeBindVertexI x typ b = do
    let combOffset = bStride b * bSkipElems b + bOffset b
    append (\ix ->
        (   do  bn <- readIORef $ bName b
                return $ VAOKey bn combOffset x False (bInstanceDiv b)
        ,   do  bn <- readIORef $ bName b
                let ix' = fromIntegral ix
                glEnableVertexAttribArray ix'
                glBindBuffer GL_ARRAY_BUFFER bn
                glVertexAttribDivisor ix' (fromIntegral $ bInstanceDiv b)
                glVertexAttribIPointer ix' x typ (fromIntegral $ bStride b) (intPtrToPtr $ fromIntegral combOffset))
        )
    return undefined

noWriter = Kleisli (const $ return undefined)

-- Uniform vertex values? Some kind of alternate implementation for uniforms not
-- currently used (the regular uniforms are handled in the Uniform module). It
-- don’t know if it is something deprecated or experimental. I don’t even know
-- how it could be used. Obviously, a buffer cannot contains both varying and
-- uniform data…
--
-- [begin]
toUniformVertex :: forall a b. Storable a => SType -> ToVertex a (S V b)
toUniformVertex styp = ToVertex (Kleisli uWriter) (Kleisli makeV) (Kleisli makeBind)
    where
        size = sizeOf (undefined :: a)
        uWriter a = do
            ptr <- get
            put (ptr `plusPtr` size)
            lift $ poke (castPtr ptr) a
            return undefined
        makeV a = do
            (n, uoffset,m) <- get
            put (n, uoffset + size, Map.insert uoffset styp m)
            useF <- lift ask
            return $ S $ useF uoffset
        makeBind a =
            return undefined

instance VertexInput Float where
    type VertexFormat Float = VFloat
    toVertex = toUniformVertex STypeFloat

instance VertexInput Int32 where
    type VertexFormat Int32 = VInt
    toVertex = toUniformVertex STypeInt

instance VertexInput Word32 where
    type VertexFormat Word32 = VWord
    toVertex = toUniformVertex STypeUInt
-- [end]

-- scalars

unBnorm :: Normalized t -> t
unBnorm (Normalized a) = a

instance VertexInput (B Float) where
    type VertexFormat (B Float) = VFloat
    toVertex = ToVertex noWriter (Kleisli $ makeVertexF 1 (const S) STypeFloat) (Kleisli $ makeBindVertexF 1 GL_FLOAT)
instance VertexInput (Normalized (B Int32)) where
    type VertexFormat (Normalized (B Int32)) = VFloat
    toVertex = ToVertex noWriter (Kleisli $ makeVertexF 1 (const S) STypeFloat . unBnorm) (Kleisli $ makeBindVertexFnorm 1 GL_INT . unBnorm)
instance VertexInput (Normalized (B Word32)) where
    type VertexFormat (Normalized (B Word32)) = VFloat
    toVertex = ToVertex noWriter (Kleisli $ makeVertexF 1 (const S) STypeFloat . unBnorm) (Kleisli $ makeBindVertexFnorm 1 GL_UNSIGNED_INT . unBnorm)
instance VertexInput (B Int32) where
    type VertexFormat (B Int32) = VInt
    toVertex = ToVertex noWriter (Kleisli $ makeVertexI 1 (const S) STypeInt) (Kleisli $ makeBindVertexI 1 GL_INT)
instance VertexInput (B Word32) where
    type VertexFormat (B Word32) = VWord
    toVertex = ToVertex noWriter (Kleisli $ makeVertexI 1 (const S) STypeUInt) (Kleisli $ makeBindVertexI 1 GL_UNSIGNED_INT)


-- B2

instance VertexInput (B2 Float) where
    type VertexFormat (B2 Float) = V2 VFloat
    toVertex = ToVertex noWriter (Kleisli $ makeVertexF 2 vec2S (STypeVec 2) . unB2) (Kleisli $ makeBindVertexF 2 GL_FLOAT . unB2)
instance VertexInput (Normalized (B2 Int32)) where
    type VertexFormat (Normalized (B2 Int32)) = V2 VFloat
    toVertex = ToVertex noWriter (Kleisli $ makeVertexF 2 vec2S (STypeVec 2) . unB2 . unBnorm) (Kleisli $ makeBindVertexFnorm 2 GL_INT . unB2 . unBnorm)
instance VertexInput (Normalized (B2 Int16)) where
    type VertexFormat (Normalized (B2 Int16)) = V2 VFloat
    toVertex = ToVertex noWriter (Kleisli $ makeVertexF 2 vec2S (STypeVec 2) . unB2 . unBnorm) (Kleisli $ makeBindVertexFnorm 2 GL_SHORT . unB2 . unBnorm)
instance VertexInput (Normalized (B2 Word32)) where
    type VertexFormat (Normalized (B2 Word32)) = V2 VFloat
    toVertex = ToVertex noWriter (Kleisli $ makeVertexF 2 vec2S (STypeVec 2) . unB2 . unBnorm) (Kleisli $ makeBindVertexFnorm 2 GL_UNSIGNED_INT . unB2 . unBnorm)
instance VertexInput (Normalized (B2 Word16)) where
    type VertexFormat (Normalized (B2 Word16)) = V2 VFloat
    toVertex = ToVertex noWriter (Kleisli $ makeVertexF 2 vec2S (STypeVec 2) . unB2 . unBnorm) (Kleisli $ makeBindVertexFnorm 2 GL_UNSIGNED_SHORT . unB2 . unBnorm)
instance VertexInput (B2 Int32) where
    type VertexFormat (B2 Int32) = V2 VInt
    toVertex = ToVertex noWriter (Kleisli $ makeVertexI 2 vec2S (STypeIVec 2) . unB2) (Kleisli $ makeBindVertexI 2 GL_INT . unB2)
instance VertexInput (B2 Int16) where
    type VertexFormat (B2 Int16) = V2 VInt
    toVertex = ToVertex noWriter (Kleisli $ makeVertexI 2 vec2S (STypeIVec 2) . unB2) (Kleisli $ makeBindVertexI 2 GL_SHORT . unB2)
instance VertexInput (B2 Word32) where
    type VertexFormat (B2 Word32) = V2 VWord
    toVertex = ToVertex noWriter (Kleisli $ makeVertexI 2 vec2S (STypeUVec 2) . unB2) (Kleisli $ makeBindVertexI 2 GL_UNSIGNED_INT . unB2)
instance VertexInput (B2 Word16) where
    type VertexFormat (B2 Word16) = V2 VWord
    toVertex = ToVertex noWriter (Kleisli $ makeVertexI 2 vec2S (STypeUVec 2) . unB2) (Kleisli $ makeBindVertexI 2 GL_UNSIGNED_SHORT . unB2)

-- B3

instance VertexInput (B3 Float) where
    type VertexFormat (B3 Float) = V3 VFloat
    toVertex = ToVertex noWriter (Kleisli $ makeVertexF 3 vec3S (STypeVec 3) . unB3) (Kleisli $ makeBindVertexF 3 GL_FLOAT . unB3)
instance VertexInput (Normalized (B3 Int32)) where
    type VertexFormat (Normalized (B3 Int32)) = V3 VFloat
    toVertex = ToVertex noWriter (Kleisli $ makeVertexF 3 vec3S (STypeVec 3) . unB3 . unBnorm) (Kleisli $ makeBindVertexFnorm 3 GL_INT . unB3 . unBnorm)
instance VertexInput (Normalized (B3 Int16)) where
    type VertexFormat (Normalized (B3 Int16)) = V3 VFloat
    toVertex = ToVertex noWriter (Kleisli $ makeVertexF 3 vec3S (STypeVec 3) . unB3 . unBnorm) (Kleisli $ makeBindVertexFnorm 3 GL_SHORT . unB3 . unBnorm)
instance VertexInput (Normalized (B3 Int8)) where
    type VertexFormat (Normalized (B3 Int8)) = V3 VFloat
    toVertex = ToVertex noWriter (Kleisli $ makeVertexF 3 vec3S (STypeVec 3) . unB3 . unBnorm) (Kleisli $ makeBindVertexFnorm 3 GL_BYTE . unB3 . unBnorm)
instance VertexInput (Normalized (B3 Word32)) where
    type VertexFormat (Normalized (B3 Word32)) = V3 VFloat
    toVertex = ToVertex noWriter (Kleisli $ makeVertexF 3 vec3S (STypeVec 3) . unB3 . unBnorm) (Kleisli $ makeBindVertexFnorm 3 GL_UNSIGNED_INT . unB3 . unBnorm)
instance VertexInput (Normalized (B3 Word16)) where
    type VertexFormat (Normalized (B3 Word16)) = V3 VFloat
    toVertex = ToVertex noWriter (Kleisli $ makeVertexF 3 vec3S (STypeVec 3) . unB3 . unBnorm) (Kleisli $ makeBindVertexFnorm 3 GL_UNSIGNED_SHORT . unB3 . unBnorm)
instance VertexInput (Normalized (B3 Word8)) where
    type VertexFormat (Normalized (B3 Word8)) = V3 VFloat
    toVertex = ToVertex noWriter (Kleisli $ makeVertexF 3 vec3S (STypeVec 3) . unB3 . unBnorm) (Kleisli $ makeBindVertexFnorm 3 GL_UNSIGNED_BYTE . unB3 . unBnorm)
instance VertexInput (B3 Int32) where
    type VertexFormat (B3 Int32) = V3 VInt
    toVertex = ToVertex noWriter (Kleisli $ makeVertexI 3 vec3S (STypeIVec 3) . unB3) (Kleisli $ makeBindVertexI 3 GL_INT . unB3)
instance VertexInput (B3 Int16) where
    type VertexFormat (B3 Int16) = V3 VInt
    toVertex = ToVertex noWriter (Kleisli $ makeVertexI 3 vec3S (STypeIVec 3) . unB3) (Kleisli $ makeBindVertexI 3 GL_SHORT . unB3)
instance VertexInput (B3 Int8) where
    type VertexFormat (B3 Int8) = V3 VInt
    toVertex = ToVertex noWriter (Kleisli $ makeVertexI 3 vec3S (STypeIVec 3) . unB3) (Kleisli $ makeBindVertexI 3 GL_BYTE . unB3)
instance VertexInput (B3 Word32) where
    type VertexFormat (B3 Word32) = V3 VWord
    toVertex = ToVertex noWriter (Kleisli $ makeVertexI 3 vec3S (STypeUVec 3) . unB3) (Kleisli $ makeBindVertexI 3 GL_UNSIGNED_INT . unB3)
instance VertexInput (B3 Word16) where
    type VertexFormat (B3 Word16) = V3 VWord
    toVertex = ToVertex noWriter (Kleisli $ makeVertexI 3 vec3S (STypeUVec 3) . unB3) (Kleisli $ makeBindVertexI 3 GL_UNSIGNED_SHORT . unB3)
instance VertexInput (B3 Word8) where
    type VertexFormat (B3 Word8) = V3 VWord
    toVertex = ToVertex noWriter (Kleisli $ makeVertexI 3 vec3S (STypeUVec 3) . unB3) (Kleisli $ makeBindVertexI 3 GL_UNSIGNED_BYTE . unB3)

-- B4

instance VertexInput (B4 Float) where
    type VertexFormat (B4 Float) = V4 VFloat
    toVertex = ToVertex noWriter (Kleisli $ makeVertexF 4 vec4S (STypeVec 4) . unB4) (Kleisli $ makeBindVertexF 4 GL_FLOAT . unB4)
instance VertexInput (Normalized (B4 Int32)) where
    type VertexFormat (Normalized (B4 Int32)) = V4 VFloat
    toVertex = ToVertex noWriter (Kleisli $ makeVertexF 4 vec4S (STypeVec 4) . unB4 . unBnorm) (Kleisli $ makeBindVertexFnorm 4 GL_INT . unB4 . unBnorm)
instance VertexInput (Normalized (B4 Int16)) where
    type VertexFormat (Normalized (B4 Int16)) = V4 VFloat
    toVertex = ToVertex noWriter (Kleisli $ makeVertexF 4 vec4S (STypeVec 4) . unB4 . unBnorm) (Kleisli $ makeBindVertexFnorm 4 GL_SHORT . unB4 . unBnorm)
instance VertexInput (Normalized (B4 Int8)) where
    type VertexFormat (Normalized (B4 Int8)) = V4 VFloat
    toVertex = ToVertex noWriter (Kleisli $ makeVertexF 4 vec4S (STypeVec 4) . unB4 . unBnorm) (Kleisli $ makeBindVertexFnorm 4 GL_BYTE . unB4 . unBnorm)
instance VertexInput (Normalized (B4 Word32)) where
    type VertexFormat (Normalized (B4 Word32)) = V4 VFloat
    toVertex = ToVertex noWriter (Kleisli $ makeVertexF 4 vec4S (STypeVec 4) . unB4 . unBnorm) (Kleisli $ makeBindVertexFnorm 4 GL_UNSIGNED_INT . unB4 . unBnorm)
instance VertexInput (Normalized (B4 Word16)) where
    type VertexFormat (Normalized (B4 Word16)) = V4 VFloat
    toVertex = ToVertex noWriter (Kleisli $ makeVertexF 4 vec4S (STypeVec 4) . unB4 . unBnorm) (Kleisli $ makeBindVertexFnorm 4 GL_UNSIGNED_SHORT . unB4 . unBnorm)
instance VertexInput (Normalized (B4 Word8)) where
    type VertexFormat (Normalized (B4 Word8)) = V4 VFloat
    toVertex = ToVertex noWriter (Kleisli $ makeVertexF 4 vec4S (STypeVec 4) . unB4 . unBnorm) (Kleisli $ makeBindVertexFnorm 4 GL_UNSIGNED_BYTE . unB4 . unBnorm)
instance VertexInput (B4 Int32) where
    type VertexFormat (B4 Int32) = V4 VInt
    toVertex = ToVertex noWriter (Kleisli $ makeVertexI 4 vec4S (STypeIVec 4) . unB4) (Kleisli $ makeBindVertexI 4 GL_INT . unB4)
instance VertexInput (B4 Int16) where
    type VertexFormat (B4 Int16) = V4 VInt
    toVertex = ToVertex noWriter (Kleisli $ makeVertexI 4 vec4S (STypeIVec 4) . unB4) (Kleisli $ makeBindVertexI 4 GL_SHORT . unB4)
instance VertexInput (B4 Int8) where
    type VertexFormat (B4 Int8) = V4 VInt
    toVertex = ToVertex noWriter (Kleisli $ makeVertexI 4 vec4S (STypeIVec 4) . unB4) (Kleisli $ makeBindVertexI 4 GL_BYTE . unB4)
instance VertexInput (B4 Word32) where
    type VertexFormat (B4 Word32) = V4 VWord
    toVertex = ToVertex noWriter (Kleisli $ makeVertexI 4 vec4S (STypeUVec 4) . unB4) (Kleisli $ makeBindVertexI 4 GL_UNSIGNED_INT . unB4)
instance VertexInput (B4 Word16) where
    type VertexFormat (B4 Word16) = V4 VWord
    toVertex = ToVertex noWriter (Kleisli $ makeVertexI 4 vec4S (STypeUVec 4) . unB4) (Kleisli $ makeBindVertexI 4 GL_UNSIGNED_SHORT . unB4)
instance VertexInput (B4 Word8) where
    type VertexFormat (B4 Word8) = V4 VWord
    toVertex = ToVertex noWriter (Kleisli $ makeVertexI 4 vec4S (STypeUVec 4) . unB4) (Kleisli $ makeBindVertexI 4 GL_UNSIGNED_BYTE . unB4)

instance VertexInput () where
    type VertexFormat () = ()
    toVertex = arr (const ())

instance (VertexInput a, VertexInput b) => VertexInput (a,b) where
    type VertexFormat (a,b) = (VertexFormat a, VertexFormat b)
    toVertex = proc ~(a,b) -> do a' <- toVertex -< a
                                 b' <- toVertex -< b
                                 returnA -< (a', b')

instance (VertexInput a, VertexInput b, VertexInput c) => VertexInput (a,b,c) where
    type VertexFormat (a,b,c) = (VertexFormat a, VertexFormat b, VertexFormat c)
    toVertex = proc ~(a,b,c) -> do a' <- toVertex -< a
                                   b' <- toVertex -< b
                                   c' <- toVertex -< c
                                   returnA -< (a', b', c')

instance (VertexInput a, VertexInput b, VertexInput c, VertexInput d) => VertexInput (a,b,c,d) where
    type VertexFormat (a,b,c,d) = (VertexFormat a, VertexFormat b, VertexFormat c, VertexFormat d)
    toVertex = proc ~(a,b,c,d) -> do a' <- toVertex -< a
                                     b' <- toVertex -< b
                                     c' <- toVertex -< c
                                     d' <- toVertex -< d
                                     returnA -< (a', b', c', d')

instance (VertexInput a, VertexInput b, VertexInput c, VertexInput d, VertexInput e) => VertexInput (a,b,c,d,e) where
    type VertexFormat (a,b,c,d,e) = (VertexFormat a, VertexFormat b, VertexFormat c, VertexFormat d, VertexFormat e)
    toVertex = proc ~(a,b,c,d,e) -> do a' <- toVertex -< a
                                       b' <- toVertex -< b
                                       c' <- toVertex -< c
                                       d' <- toVertex -< d
                                       e' <- toVertex -< e
                                       returnA -< (a', b', c', d', e')

instance (VertexInput a, VertexInput b, VertexInput c, VertexInput d, VertexInput e, VertexInput f) => VertexInput (a,b,c,d,e,f) where
    type VertexFormat (a,b,c,d,e,f) = (VertexFormat a, VertexFormat b, VertexFormat c, VertexFormat d, VertexFormat e, VertexFormat f)
    toVertex = proc ~(a,b,c,d,e,f) -> do a' <- toVertex -< a
                                         b' <- toVertex -< b
                                         c' <- toVertex -< c
                                         d' <- toVertex -< d
                                         e' <- toVertex -< e
                                         f' <- toVertex -< f
                                         returnA -< (a', b', c', d', e', f')

instance (VertexInput a, VertexInput b, VertexInput c, VertexInput d, VertexInput e, VertexInput f, VertexInput g) => VertexInput (a,b,c,d,e,f,g) where
    type VertexFormat (a,b,c,d,e,f,g) = (VertexFormat a, VertexFormat b, VertexFormat c, VertexFormat d, VertexFormat e, VertexFormat f, VertexFormat g)
    toVertex = proc ~(a,b,c,d,e,f,g) -> do a' <- toVertex -< a
                                           b' <- toVertex -< b
                                           c' <- toVertex -< c
                                           d' <- toVertex -< d
                                           e' <- toVertex -< e
                                           f' <- toVertex -< f
                                           g' <- toVertex -< g
                                           returnA -< (a', b', c', d', e', f', g')

instance VertexInput a => VertexInput (V0 a) where
    type VertexFormat (V0 a) = V0 (VertexFormat a)
    toVertex = arr (const V0)

instance VertexInput a => VertexInput (V1 a) where
    type VertexFormat (V1 a) = V1 (VertexFormat a)
    toVertex = proc ~(V1 a) -> do a' <- toVertex -< a
                                  returnA -< V1 a'

instance VertexInput a => VertexInput (V2 a) where
    type VertexFormat (V2 a) = V2 (VertexFormat a)
    toVertex = proc ~(V2 a b) -> do a' <- toVertex -< a
                                    b' <- toVertex -< b
                                    returnA -< V2 a' b'

instance VertexInput a => VertexInput (V3 a) where
    type VertexFormat (V3 a) = V3 (VertexFormat a)
    toVertex = proc ~(V3 a b c) -> do a' <- toVertex -< a
                                      b' <- toVertex -< b
                                      c' <- toVertex -< c
                                      returnA -< V3 a' b' c'

instance VertexInput a => VertexInput (V4 a) where
    type VertexFormat (V4 a) = V4 (VertexFormat a)
    toVertex = proc ~(V4 a b c d) -> do a' <- toVertex -< a
                                        b' <- toVertex -< b
                                        c' <- toVertex -< c
                                        d' <- toVertex -< d
                                        returnA -< V4 a' b' c' d'


instance VertexInput a => VertexInput (Quaternion a) where
    type VertexFormat (Quaternion a) = Quaternion (VertexFormat a)
    toVertex = proc ~(Quaternion a v) -> do
                a' <- toVertex -< a
                v' <- toVertex -< v
                returnA -< Quaternion a' v'

instance (VertexInput (f a), VertexInput a, HostFormat (f a) ~ f (HostFormat a), VertexFormat (f a) ~ f (VertexFormat a)) => VertexInput (Point f a) where
    type VertexFormat (Point f a) = Point f (VertexFormat a)
    toVertex = proc ~(P a) -> do
                a' <- toVertex -< a
                returnA -< P a'

instance VertexInput a => VertexInput (Plucker a) where
    type VertexFormat (Plucker a) = Plucker (VertexFormat a)
    toVertex = proc ~(Plucker a b c d e f) -> do
                a' <- toVertex -< a
                b' <- toVertex -< b
                c' <- toVertex -< c
                d' <- toVertex -< d
                e' <- toVertex -< e
                f' <- toVertex -< f
                returnA -< Plucker a' b' c' d' e' f'
