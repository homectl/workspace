{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE PatternSynonyms #-}
module Graphics.GPipe.Internal.Compiler where

import           Control.Monad                    (forM_, void, when)
import           Control.Monad.Exception          (MonadException)
import           Control.Monad.IO.Class           (MonadIO (..))
import           Control.Monad.Trans.Class        (MonadTrans (lift))
import           Control.Monad.Trans.Except       (throwE)
import           Control.Monad.Trans.Reader       (ask)
import           Control.Monad.Trans.State.Strict (evalState, get, put)
import           Data.IntMap                      ((!))
import qualified Data.IntMap                      as Map
import qualified Data.IntSet                      as Set
import           Data.Maybe                       (fromJust, isJust, isNothing)
import qualified Data.Text.Foreign                as T
import           Data.Text.Lazy                   (Text)
import qualified Data.Text.Lazy                   as T
import           Graphics.GPipe.Internal.Context

import           Control.Exception                (throwIO)
import           Data.Either                      (partitionEithers)
import           Data.IORef                       (IORef, mkWeakIORef, newIORef,
                                                   readIORef)
import           Data.List                        (zip5)
import           Data.Word                        (Word32)
import           Foreign.C.String                 (peekCString, withCString,
                                                   withCStringLen)
import           Foreign.Marshal.Alloc            (alloca)
import           Foreign.Marshal.Array            (allocaArray, withArray)
import           Foreign.Marshal.Utils            (with)
import           Foreign.Ptr                      (nullPtr)
import           Foreign.Storable                 (peek)
import           Graphics.GL.Core45
import           Graphics.GL.Types                (GLuint)

-- public
type WinId = Int

{-
A Drawcall is an OpenGL shader program with its context. Drawcalls are produced
when evaluating a (GPipe) Shader and are intended to be "compiled" (sources
compiled and linked into a program used by a render action).
-}
-- public
data Drawcall s = Drawcall
    {   drawcallFbo :: s ->
        (   Either WinId
                (   IO FBOKeys
                ,   IO ()
                )
        ,   IO ()
        )
    ,   feedbackBuffer :: Maybe (s -> IO (GLuint, GLuint, GLuint, GLuint))
        -- Key for RenderIOState::inputArrayToRenderIOs.
    ,   primitiveName :: Int
        -- Key for RenderIOState::rasterizationNameToRenderIO.
    ,   rasterizationName :: Maybe Int
        -- Shader sources.
    ,   vertexSource :: Text
    ,   optionalGeometrySource :: Maybe Text
    ,   optionalFragmentSource :: Maybe Text
        -- Inputs.
    ,   usedInputs :: [Int]
        -- Uniforms and texture units used in each shader.
    ,   usedVUniforms :: [Int],   usedVSamplers :: [Int]
    ,   usedGUniforms :: [Int],   usedGSamplers :: [Int]
    ,   usedFUniforms :: [Int],   usedFSamplers :: [Int]
        -- The size of the uniform buffer for the primitive stream (see USize in PrimitiveStream data).
    ,   primStrUBufferSize :: Int
    }

-- public
mapDrawcall :: (s -> s') -> Drawcall s' -> Drawcall s
mapDrawcall f dc = dc{ drawcallFbo = drawcallFbo dc . f, feedbackBuffer = feedbackBuffer' }
    where
        feedbackBuffer' = case feedbackBuffer dc of
            Nothing -> Nothing
            Just b  -> Just (b . f)

-- index/binding refers to what is used in the final shader. Index space is
-- limited, usually 16 attribname is what was declared, but all might not be
-- used. Attribname share namespace with uniforms and textures (in all shaders)
-- and is unlimited(TM)
-- What? Contradiction.
-- Should be used elsewhere instead of Int (of are they pre-alloc Int?).
-- public
type Binding = Int

-- TODO: Add usedBuffers to RenderIOState, ie Map.IntMap (s -> (Binding -> IO
--       (), Int)) and the like then create a function that checks that none of
--       the input buffers are used as output, and throws if it is

{- Contains the interactions between a GPipeShader and its environment. It is
populated when creating a GPipeShader and queried when compiling it into a
rendering action. In other words, it’s not a state at all, but some kind of
environment connector or adaptor. It is simply called a state because it build
using a State monad.
-}
-- public
data RenderIOState s = RenderIOState
    {   -- Uniform buffer objects bindings. TODO Return buffer name here when we
        -- start writing to buffers during rendering (transform feedback, buffer
        -- textures) -> Ok, but uniform only?
        uniformNameToRenderIO :: Map.IntMap (s -> Binding -> IO ())
        -- Texture units bindings. IO returns texturename for validating that it
        -- isnt used as render target
    ,   samplerNameToRenderIO :: Map.IntMap (s -> Binding -> IO Int)
        -- Final rasterization operations (mostly setting the viewport).
    ,   rasterizationNameToRenderIO :: Map.IntMap (s -> IO ())
        -- Final vertex processiong stage.
    ,   transformFeedbackToRenderIO :: Map.IntMap (s -> GLuint -> IO ())
        -- VAO bindings.
    ,   inputArrayToRenderIO :: Map.IntMap (s ->
        [   (   [Binding] -- inputs (drawcall's usedInputs)
            ,   GLuint -- primitive stream uniforms buffer
            ,   Int -- primitive stream uniforms buffer size
            ) ->
            (   (   IO [VAOKey] -- VAO names?
                ,   IO () -- To bind the VAO?
                )
            ,   IO () -- To draw with it.
            )
        ])
    }

-- public
newRenderIOState :: RenderIOState s
newRenderIOState = RenderIOState Map.empty Map.empty Map.empty Map.empty Map.empty

-- Why 'map'? Wouldn’t 'inject' be a better name?
-- public
mapRenderIOState :: (s -> s') -> RenderIOState s' -> RenderIOState s -> RenderIOState s
mapRenderIOState f (RenderIOState a' b' c' d' e') (RenderIOState a b c d e) =
    let merge x x' = Map.union x $ Map.map (. f) x'
    in  RenderIOState (merge a a') (merge b b') (merge c c') (merge d d') (merge e e')

-- | May throw a GPipeException
-- The multiple drawcalls to be compiled are intended to use the same environment 's' (and only one is selected dynamically when rendering).
-- public
compileDrawcalls :: (Monad m, MonadIO m, MonadException m, ContextHandler ctx)
    => [IO (Drawcall s)] -- The proto drawcalls to generate and compile.
    -> RenderIOState s -- Interactions between the drawcalls and the environment 's'.
    -> ContextT ctx os m (s -> Render os ()) -- The compiled drawcall (OpenGL program shader actually) as a function on an environment.
compileDrawcalls protoDrawcalls state = do

    (drawcalls, limitErrors) <- liftNonWinContextIO $ safeGenerateDrawcalls protoDrawcalls
    compilationResults <- liftNonWinContextIO $ mapM (innerCompile state) drawcalls
    let (compilationErrors, compiledDrawcalls) = partitionEithers compilationResults
        (programNameAndDeleters, renderers) = unzip compiledDrawcalls
        compositeRenderer x = mapM_ ($ x) renderers
        allErrors = limitErrors ++ compilationErrors

    if null allErrors
        then do
            -- Register each deleter separately on their program finalization.
            forM_ programNameAndDeleters $ \ (programNameRef, deleter) -> do
                programName <- liftIO $ readIORef programNameRef
                addContextFinalizer programNameRef deleter
            -- Return a composite rendering action.
            return compositeRenderer
        else do
            -- Directly call all the deleters.
            liftNonWinContextAsyncIO $ forM_ programNameAndDeleters $ \ (_, deleter) -> do
                deleter
            -- Raise an error.
            liftIO $ throwIO $ GPipeException $ concat allErrors

{- Generate the drawcalls (a single one each time in fact) and check their
inputs don't exceed some OpenGL limits.
-}
-- private
safeGenerateDrawcalls :: [IO (Drawcall s)] -- The proto drawcalls to generate.
    ->  IO (
        [   (   Drawcall s -- A generated drawcall.
            ,   [Int] -- Its uniform buffers used.
            ,   [Int] -- Its textures units used.
            ,   [Int] -- Its allocated uniforms.
            ,   [Int] -- Its allocated texture units.
            )
        ]
        , [String] -- The raised errors regarding exceeded limits.
        )
safeGenerateDrawcalls protoDrawcalls = do

    -- Retrieve some limits from OpenGL.
    [   maxGUnis, maxGSamplers,
        maxVUnis, maxVSamplers,
        maxFUnis, maxFSamplers,
        maxUnis, maxSamplers ]
        <- liftIO $
            mapM (\t -> fromIntegral <$> alloca (\ ptr -> glGetIntegerv t ptr >> peek ptr))
                [ GL_MAX_GEOMETRY_UNIFORM_BLOCKS
                , GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS
                , GL_MAX_VERTEX_UNIFORM_BLOCKS
                , GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS
                , GL_MAX_FRAGMENT_UNIFORM_BLOCKS
                , GL_MAX_TEXTURE_IMAGE_UNITS
                , GL_MAX_COMBINED_UNIFORM_BLOCKS
                , GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS
                ]

    -- Build the drawcalls.
    drawcalls <- liftIO $ sequence protoDrawcalls -- IO only for SNMap

    let
        -- Collect stats from the drawcalls (bound uniforms and texture units).
        gUnisPerDrawcall = map usedGUniforms drawcalls
        gSampsPerDrawcall = map usedGSamplers drawcalls
        vUnisPerDrawcall = map usedVUniforms drawcalls
        vSampsPerDrawcall = map usedVSamplers drawcalls
        fUnisPerDrawcall = map usedFUniforms drawcalls
        fSampsPerDrawcall = map usedFSamplers drawcalls

        -- Consolidate them for the whole program.
        unisPerDrawcall = zipWith orderedUnion (zipWith orderedUnion gUnisPerDrawcall vUnisPerDrawcall) fUnisPerDrawcall
        sampsPerDrawcall = zipWith orderedUnion (zipWith orderedUnion gSampsPerDrawcall vSampsPerDrawcall) fSampsPerDrawcall

        -- Produce an error message for each limit exceeded by at least one of the drawcalls.
        limitErrors = concat
            [ ["Too many uniform blocks used in a single geometry shader\n" | any (\ xs -> length xs >= maxGUnis) gUnisPerDrawcall]
            , ["Too many textures used in a single geometry shader\n" | any (\ xs -> length xs >= maxGSamplers) gSampsPerDrawcall]
            , ["Too many uniform blocks used in a single vertex shader\n" | any (\ xs -> length xs >= maxVUnis) vUnisPerDrawcall]
            , ["Too many textures used in a single vertex shader\n" | any (\ xs -> length xs >= maxVSamplers) vSampsPerDrawcall]
            , ["Too many uniform blocks used in a single fragment shader\n" | any (\ xs -> length xs >= maxFUnis) fUnisPerDrawcall]
            , ["Too many textures used in a single fragment shader\n" | any (\ xs -> length xs >= maxFSamplers) fSampsPerDrawcall]
            , ["Too many uniform blocks used in a single shader program\n" | any (\ xs -> length xs >= maxUnis) unisPerDrawcall]
            , ["Too many textures used in a single shader program\n" | any (\ xs -> length xs >= maxSamplers) sampsPerDrawcall]
            ]

        allocatedUniforms = allocateConsecutiveIndexes maxUnis unisPerDrawcall
        allocatedSamplers = allocateConsecutiveIndexes maxSamplers sampsPerDrawcall

    return (zip5 drawcalls unisPerDrawcall sampsPerDrawcall allocatedUniforms allocatedSamplers, limitErrors)

-- private
innerCompile :: RenderIOState s -- Interactions between the drawcall and the environment 's'.
    ->  ( Drawcall s -- A drawcall with:
        , [Int] -- its uniform buffers used,
        , [Int] -- its textures units used,
        , [Int] -- its allocated uniforms,
        , [Int] -- its allocated texture units.
        )
    ->  IO
        ( Either
            String -- A failure in case the program cannot be compiled in linked.
            ( (IORef GLuint, IO ()) -- The program name and its destructor.
            , s -> Render os () -- The program's renderer as a function on a render (OpenGL) state. Upper stage called this a 'CompiledShader'.
            )
        )
innerCompile state (drawcall, unis, samps, ubinds, sbinds) = do
    let vsource = vertexSource drawcall
        ogsource = optionalGeometrySource drawcall
        ofsource = optionalFragmentSource drawcall
        inputs = usedInputs drawcall

    -- Compile and link the shader program.
    errorOrProgramName <- do
        -- Compile the vertex shader.
        vShader <- glCreateShader GL_VERTEX_SHADER
        mErrV <- compileOpenGlShader vShader vsource
        -- Compile the optional geometry shader.
        (ogShader, mErrG) <- case ogsource of
            Nothing -> return (Nothing, Nothing)
            Just gsource -> do
                gShader <- glCreateShader GL_GEOMETRY_SHADER
                mErrG <- compileOpenGlShader gShader gsource
                return (Just gShader, mErrG)
        -- Compile the fragment shader.
        (ofShader, mErrF) <- case ofsource of
            Nothing -> return (Nothing, Nothing)
            Just fsource -> do
                fShader <- glCreateShader GL_FRAGMENT_SHADER
                mErrF <- compileOpenGlShader fShader fsource
                return (Just fShader, mErrF)

        if all isNothing [mErrV, mErrG, mErrF]
            then do
                pName <- glCreateProgram
                glAttachShader pName vShader

                whenJust' ogShader $ glAttachShader pName
                whenJust' ofShader $ glAttachShader pName
                mapM_ (\(name, ix) -> withCString ("in"++ show name) $ glBindAttribLocation pName ix) $ zip inputs [0..]

                case (feedbackBuffer drawcall, rasterizationName drawcall) of
                    (Nothing, Just _) -> return ()
                    (Just _, Just geoN) -> (transformFeedbackToRenderIO state ! geoN) undefined pName

                mPErr <- linkProgram pName

                glDetachShader pName vShader
                whenJust' ogShader $ glDetachShader pName
                whenJust' ofShader $ glDetachShader pName

                glDeleteShader vShader
                whenJust' ogShader glDeleteShader
                whenJust' ofShader glDeleteShader

                case mPErr of
                    Just errP -> do
                        glDeleteProgram pName
                        return $ Left $ "Linking a GPU progam failed:\n" ++ errP ++ concat
                            [ maybe "" (\e -> "\nVertex source:\n" ++ e ++ "\n") (Just $ T.unpack vsource)
                            , maybe "" (\e -> "\nGeometry source:\n" ++ e ++ "\n") (T.unpack <$> ogsource)
                            , maybe "" (\e -> "\nFragment source:\n" ++ e ++ "\n") (T.unpack <$> ofsource)
                            ]
                    Nothing -> return $ Right pName
            else do
                glDeleteShader vShader
                whenJust' ogShader glDeleteShader
                whenJust' ofShader glDeleteShader

                let err = concat
                        [ maybe "" (\e -> "A vertex shader compilation failed:\n" ++ e ++ "\nSource:\n" ++ T.unpack vsource) mErrV
                        , maybe "" (\e -> "A geometry shader compilation failed:\n" ++ e ++ "\nSource:\n" ++ T.unpack (fromJust ogsource)) mErrG
                        , maybe "" (\e -> "A fragment shader compilation failed:\n" ++ e ++ "\nSource:\n" ++ T.unpack (fromJust ofsource)) mErrF
                        ]
                return $ Left err

    case errorOrProgramName of
        -- Left: the failure.
        Left err -> return $ Left err
        -- Right: the program wrapped in a Render monad.
        Right pName -> Right <$> case (feedbackBuffer drawcall, rasterizationName drawcall) of
            (Nothing, Just rastN) -> createRenderer state (drawcall, unis, ubinds, samps, sbinds) pName rastN
            (Just getTransformFeedbackName, Just geoN) -> createFeedbackRenderer state (drawcall, unis, ubinds, samps, sbinds) pName getTransformFeedbackName geoN
            _ -> error "No rasterization nor feedback!"

-- private
createRenderer :: RenderIOState s -- Interactions between the drawcall and the environment 's'.
    ->  ( Drawcall s -- A drawcall with:
        , [Int] -- its uniform buffers used,
        , [Int] -- its textures units used,
        , [Int] -- its allocated uniforms,
        , [Int] -- its allocated texture units.
        )
    ->  GLuint -- pName
    ->  Int
    ->  IO  ( (IORef GLuint, IO ()) -- The program name and its destructor.
            , s -> Render os () -- The program's renderer as a function on a render (OpenGL) state.
            )
createRenderer state (drawcall, unis, ubinds, samps, sbinds) pName rastN = do
    let fboSetup = drawcallFbo drawcall
        primN = primitiveName drawcall
        inputs = usedInputs drawcall
        pstrUSize = primStrUBufferSize drawcall

    let pstrUSize' = if 0 `elem` unis then pstrUSize else 0
    pstrUBuf <- createUniformBuffer pstrUSize' -- Create uniform buffer for primiveStream uniforms

    forM_ (zip unis ubinds) $ \(name, bind) -> do
        uix <- withCString ("uBlock" ++ show name) $ glGetUniformBlockIndex pName
        glUniformBlockBinding pName uix (fromIntegral bind)

    glUseProgram pName -- For setting texture uniforms
    forM_ (zip samps sbinds) $ \(name, bind) -> do
        six <- withCString ("s" ++ show name) $ glGetUniformLocation pName
        glUniform1i six (fromIntegral bind)
    pNameRef <- newIORef pName

    let uNameToRenderIOMap = uniformNameToRenderIO state
        uNameToRenderIOMap' = addPrimitiveStreamUniform pstrUBuf pstrUSize' uNameToRenderIOMap

    -- Drawing with the program.
    let renderer = \x -> Render $ do
            rs <- lift $ lift get
            renv <- lift ask
            let (mFboKeyIO, blendIO) = fboSetup x

            let inwin windowId m = do
                    case Map.lookup windowId (perWindowRenderState rs) of
                        Nothing -> return () -- Window deleted
                        Just (ws, doAsync) -> do
                            lift $ lift $ put (rs { renderLastUsedWin = windowId })
                            mErr <- liftIO $ asSync doAsync $ do
                                pName' <- readIORef pNameRef -- Cant use pName, need to touch pNameRef
                                glUseProgram pName'
                                True <- bind uNameToRenderIOMap' (zip unis ubinds) x (const $ return True)
                                isOk <- bind (samplerNameToRenderIO state) (zip samps sbinds) x (return . not . (`Set.member` renderWriteTextures rs))
                                (rasterizationNameToRenderIO state ! rastN) x
                                blendIO
                                mErr2 <- m
                                let mErr = if isOk
                                        then Nothing
                                        else Just $ "Running shader that samples from texture that currently has an image borrowed from it."
                                            ++ "Try run this shader from a separate render call where no images from the same texture are drawn to or cleared.\n"
                                return $ mErr <> mErr2
                            forM_ mErr throwE

            -- Bind the framebuffer.
            windowId <- case mFboKeyIO of
                Left wid -> do -- Bind correct context
                    inwin wid $ do
                        glBindFramebuffer GL_DRAW_FRAMEBUFFER 0
                        return Nothing
                    return wid
                Right (fboKeyIO, fboIO) -> do
                    -- Off-screen draw call, continue with last context
                    -- (something wrong here?)
                    (cwid, cd, doAsync) <- unRender getLastRenderWin
                    inwin cwid $ do
                        fbokey <- fboKeyIO
                        mfbo <- getFBO cd fbokey
                        case mfbo of
                            Just fbo -> do
                                fbo' <- readIORef fbo
                                glBindFramebuffer GL_DRAW_FRAMEBUFFER fbo'
                                return Nothing
                            Nothing -> do
                                fbo' <- alloca $ \ ptr -> glGenFramebuffers 1 ptr >> peek ptr
                                fbo <- newIORef fbo'
                                void $ mkWeakIORef fbo (doAsync $ with fbo' $ glDeleteFramebuffers 1)
                                setFBO cd fbokey fbo
                                glBindFramebuffer GL_DRAW_FRAMEBUFFER fbo'
                                glEnable GL_FRAMEBUFFER_SRGB
                                fboIO
                                let numColors = length $ fboColors fbokey
                                withArray [GL_COLOR_ATTACHMENT0 .. (GL_COLOR_ATTACHMENT0 + fromIntegral numColors - 1)] $
                                    glDrawBuffers (fromIntegral numColors)
                                getFboError
                    return cwid

            -- Draw each vertex array.
            forM_ (map ($ (inputs, pstrUBuf, pstrUSize)) ((inputArrayToRenderIO state ! primN) x)) $ \ ((keyIO, vaoIO), drawIO) -> do
                case Map.lookup windowId (perWindowRenderState rs) of
                    Nothing -> return () -- Window deleted
                    Just (ws, doAsync) ->
                        liftIO $ do
                            let cd = windowContextData ws
                            key <- keyIO
                            mvao <- getVAO cd key
                            case mvao of
                                Just vao -> do
                                    vao' <- readIORef vao
                                    glBindVertexArray vao'
                                Nothing -> do
                                    vao' <- alloca $ \ ptr -> glGenVertexArrays 1 ptr >> peek ptr
                                    vao <- newIORef vao'
                                    void $ mkWeakIORef vao (doAsync $ with vao' $ glDeleteVertexArrays 1)
                                    setVAO cd key vao
                                    glBindVertexArray vao'
                                    vaoIO
                            drawIO

    let deleter = do
            glDeleteProgram pName
            when (pstrUSize > 0) $ with pstrUBuf (glDeleteBuffers 1)

    return ((pNameRef, deleter), renderer)

-- private
createFeedbackRenderer :: RenderIOState s -- Interactions between the drawcall and the environment 's'.
    ->  ( Drawcall s -- A drawcall with:
        , [Int] -- its uniform buffers used,
        , [Int] -- its textures units used,
        , [Int] -- its allocated uniforms,
        , [Int] -- its allocated texture units.
        )
    ->  GLuint -- program name
    ->  (s -> IO (GLuint, GLuint, GLuint, GLuint)) -- transform feedback stuff
    ->  Int
    ->  IO  ( (IORef GLuint, IO ()) -- The program name and its destructor.
            , s -> Render os () -- The program's renderer as a function on a render (OpenGL) state.
            )
createFeedbackRenderer state (drawcall, unis, ubinds, samps, sbinds) pName getTransformFeedbackName geoN = do
    let fboSetup = drawcallFbo drawcall
        primN = primitiveName drawcall
        inputs = usedInputs drawcall
        pstrUSize = primStrUBufferSize drawcall

    let pstrUSize' = if 0 `elem` unis then pstrUSize else 0
    pstrUBuf <- createUniformBuffer pstrUSize' -- Create uniform buffer for primiveStream uniforms

    forM_ (zip unis ubinds) $ \(name, bind) -> do
        uix <- withCString ("uBlock" ++ show name) $ glGetUniformBlockIndex pName
        glUniformBlockBinding pName uix (fromIntegral bind)

    glUseProgram pName -- For setting texture uniforms
    forM_ (zip samps sbinds) $ \(name, bind) -> do
        six <- withCString ("s" ++ show name) $ glGetUniformLocation pName
        glUniform1i six (fromIntegral bind)
    pNameRef <- newIORef pName

    let uNameToRenderIOMap = uniformNameToRenderIO state
        uNameToRenderIOMap' = addPrimitiveStreamUniform pstrUBuf pstrUSize' uNameToRenderIOMap

    -- Drawing with the program.
    let renderer = \x -> Render $ do
            rs <- lift $ lift get
            renv <- lift ask
            let (Left windowId, blendIO) = fboSetup x
                transformFeedback = getTransformFeedbackName x

            case Map.lookup windowId (perWindowRenderState rs) of
                Nothing -> return () -- Window deleted
                Just (ws, doAsync) -> do
                    lift $ lift $ put (rs { renderLastUsedWin = windowId })
                    liftIO $ asSync doAsync $ do
                        pName' <- readIORef pNameRef -- Cant use pName, need to touch pNameRef
                        glUseProgram pName'
                        -- Too late: (transformFeedbackToRenderIO state ! geoN) x pName'
                        True <- bind uNameToRenderIOMap' (zip unis ubinds) x (const $ return True)
                        isOk <- bind (samplerNameToRenderIO state) (zip samps sbinds) x (return . not . (`Set.member` renderWriteTextures rs))
                        blendIO

                    -- Draw each vertex array.
                    forM_ (map ($ (inputs, pstrUBuf, pstrUSize)) ((inputArrayToRenderIO state ! primN) x)) $ \ ((keyIO, vaoIO), drawIO) -> liftIO $ do
                        let cd = windowContextData ws
                        key <- keyIO
                        mvao <- getVAO cd key
                        case mvao of
                            Just vao -> do
                                vao' <- readIORef vao
                                glBindVertexArray vao'
                            Nothing -> do
                                vao' <- alloca $ \ ptr -> glGenVertexArrays 1 ptr >> peek ptr
                                vao <- newIORef vao'
                                void $ mkWeakIORef vao (doAsync $ with vao' $ glDeleteVertexArrays 1)
                                setVAO cd key vao
                                glBindVertexArray vao'
                                vaoIO
                        (bName, tfName, tfqName, topology) <- transformFeedback
                        glBindTransformFeedback GL_TRANSFORM_FEEDBACK tfName
                        glBindBufferBase GL_TRANSFORM_FEEDBACK_BUFFER 0 bName
                        glBeginQuery GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN tfqName
                        -- liftIO $ hPutStrLn stderr $ "doing transform feedback"
                        glBeginTransformFeedback topology
                        glEnable GL_RASTERIZER_DISCARD
                        drawIO
                        glDisable GL_RASTERIZER_DISCARD
                        glEndTransformFeedback
                        glEndQuery GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN
                        {-
                        l <- alloca $ \ptr -> do
                            glGetQueryObjectiv tfqName GL_QUERY_RESULT ptr
                            peek ptr
                        liftIO $ hPutStrLn stderr $ "generated primitive count: " ++ show l
                        -}

    let deleter = do
            glDeleteProgram pName
            when (pstrUSize > 0) $ with pstrUBuf (glDeleteBuffers 1)

    return ((pNameRef, deleter), renderer)

-- private
compileOpenGlShader :: GLuint -> Text -> IO (Maybe String)
compileOpenGlShader name source = do
    -- writeFile ("shaders/" ++ show name ++ ".glsl") source -- For debug purposes only.
    T.withCStringLen (T.toStrict source) $ \(ptr, len) ->
        with ptr $ \pptr ->
            with (fromIntegral len) $ \plen ->
                glShaderSource name 1 pptr plen
    -- putStrLn $ "Compiling shader " ++ show name
    glCompileShader name
    -- putStrLn $ "Compiled shader " ++ show name
    compStatus <- alloca $ \ ptr -> glGetShaderiv name GL_COMPILE_STATUS ptr >> peek ptr
    if compStatus /= GL_FALSE
        then return Nothing
        else do
            logLen <- alloca $ \ ptr -> glGetShaderiv name GL_INFO_LOG_LENGTH ptr >> peek ptr
            let logLen' = fromIntegral logLen
            fmap Just $ allocaArray logLen' $ \ ptr -> do
                glGetShaderInfoLog name logLen nullPtr ptr
                peekCString ptr

-- private
linkProgram :: GLuint -> IO (Maybe String)
linkProgram name = do
    -- putStrLn $ "Linking program " ++ show name
    glLinkProgram name
    -- putStrLn $ "Linked program " ++ show name
    linkStatus <- alloca $ \ ptr -> glGetProgramiv name GL_LINK_STATUS ptr >> peek ptr
    if linkStatus /= GL_FALSE
        then return Nothing
        else do
            logLen <- alloca $ \ ptr -> glGetProgramiv name GL_INFO_LOG_LENGTH ptr >> peek ptr
            let logLen' = fromIntegral logLen
            fmap Just $ allocaArray logLen' $ \ ptr -> do
                glGetProgramInfoLog name logLen nullPtr ptr
                peekCString ptr

-- private
createUniformBuffer :: Integral a => a -> IO GLuint
createUniformBuffer 0 = return undefined
createUniformBuffer uSize = do
    bname <- alloca $ \ ptr -> glGenBuffers 1 ptr >> peek ptr
    glBindBuffer GL_COPY_WRITE_BUFFER bname
    glBufferData GL_COPY_WRITE_BUFFER (fromIntegral uSize) nullPtr GL_STREAM_DRAW
    return bname

-- private
addPrimitiveStreamUniform :: Word32 -> Int -> Map.IntMap (s -> Binding -> IO ()) -> Map.IntMap (s -> Binding -> IO ())
addPrimitiveStreamUniform _ 0 = id
addPrimitiveStreamUniform bname uSize = Map.insert 0 $ \_ bind -> glBindBufferRange GL_UNIFORM_BUFFER (fromIntegral bind) bname 0 (fromIntegral uSize)

-- private
bind :: Map.IntMap (s -> Binding -> IO x)
    -> [(Int, Int)]
    -> s
    -> (x -> IO Bool) -- Used to assert we may use textures bound as render targets
    -> IO Bool
bind iom ((n,b):xs) s a = do
    ok1 <- bind iom xs s a
    ok2 <- (iom ! n) s b >>= a
    return $ ok1 && ok2
bind _ [] _ _ = return True

-- private
orderedUnion :: Ord a => [a] -> [a] -> [a]
orderedUnion xxs@(x:xs) yys@(y:ys) | x == y    = x : orderedUnion xs ys
                                   | x < y     = x : orderedUnion xs yys
                                   | otherwise = y : orderedUnion xxs ys
orderedUnion xs [] = xs
orderedUnion [] ys = ys

-- private
oldAllocateWhichGiveStrangeResults :: Int -> [[Int]] -> [[Int]]
oldAllocateWhichGiveStrangeResults mx = allocate' Map.empty [] where
    allocate' m ys ((x:xs):xss)
        | Just a <- Map.lookup x m = allocate' m (a:ys) (xs:xss)
        | ms <- Map.size m, ms < mx = allocate' (Map.insert x ms m) (ms:ys) (xs:xss)
        | otherwise =
            let (ek,ev) = findLastUsed m mx (ys ++ xs ++ concat xss)
            in allocate' (Map.insert x ev (Map.delete ek m)) (ev:ys) (xs:xss)
    allocate' m ys (_:xss) = reverse ys : allocate' m [] xss
    allocate' _ _ [] = []

    findLastUsed m n (x:xs) | n > 1 =
        let (a, m') = Map.updateLookupWithKey (const $ const Nothing) x m
            n' = if isJust a then n-1 else n
        in findLastUsed m' n' xs
    findLastUsed m _ _ = head $ Map.toList m

{-
Map the input values into [0, mx[ with no gap. Two different values in the
input are mapped to two different values in the output as long as the `mx`
size of the output set is no reach. The `mx+1` nth value and beyond are
mapped to… I can't figure it!. Let's just raise an error instead.

Note that the fact that the values are stored in a list of lists doesn't
matter, we are just mapping a tree of values to another without caring about
the traversed structure.
-}
-- private
allocateConsecutiveIndexes :: Int -> [[Int]] -> [[Int]]
allocateConsecutiveIndexes mx values = evalState (mapM (mapM allocateIndex) values) Map.empty where
    allocateIndex n = do
        mapping <- get
        case Map.lookup n mapping of
            Nothing -> do
                let m = Map.size mapping
                if m < mx
                    then do
                        put $ Map.insert n m mapping
                        return m
                    else error "Not enough indexes available!"
            Just m -> return m

-- public
getFboError :: MonadIO m => m (Maybe String)
getFboError = do
    status <- glCheckFramebufferStatus GL_DRAW_FRAMEBUFFER
    return $ case status of
        GL_FRAMEBUFFER_COMPLETE -> Nothing
        GL_FRAMEBUFFER_UNSUPPORTED -> Just "The combination of draw images (FBO) used in the render call is unsupported by this graphics driver\n"
        _ -> error "GPipe internal FBO error"

-- | A 'whenJust' that accepts a monoidal return value.
-- private
whenJust' :: (Monad m, Monoid b) => Maybe a -> (a -> m b) -> m b
whenJust' = flip $ maybe (return mempty)
