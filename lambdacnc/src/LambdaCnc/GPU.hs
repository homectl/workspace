{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module LambdaCnc.GPU
  ( main
  ) where

import Data.Word(Word32)
import           Control.Concurrent          (threadDelay)
import           Control.Monad               (unless)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import qualified Data.Time.Clock             as Time
import qualified Graphics.Formats.STL        as STL
import           Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import           LambdaCnc.Config            (RuntimeConfig (..),
                                              defaultRuntimeConfig)
import qualified LambdaCnc.STL               as STL
import qualified LambdaCnc.Shaders           as Shaders
import           Prelude                     hiding ((<*))
import qualified System.Environment          as Env
import           System.IO                   (hFlush, stdout)

data ShaderEnvironment = ShaderEnvironment
    { envPrimitives :: PrimitiveArray Triangles ShaderInput
    , envShadowColor :: Image (Format RFloat)
    , envShadowDepth :: Image (Format Depth)
    }

type ColorTex os = Texture2D os (Format RFloat)
type ShadowColorTex os = Texture2D os (Format RFloat)
type ShadowDepthTex os = Texture2D os (Format Depth)

type ShaderInput = (B3 Float, B3 Float)
type VertexBuffer os = Buffer os ShaderInput
type UniformBuffer os = Buffer os (Uniform (RuntimeConfig (B Float)))

type ShadowShader os = CompiledShader os ShaderEnvironment
type CameraShader os = CompiledShader os (PrimitiveArray Triangles ShaderInput)

fps :: Double
fps = 24

viewPort :: V2 Int
viewPort = V2 1500 1000


compileShadowShader
    :: UniformBuffer os
    -> ContextT GLFW.Handle os IO (ShadowShader os)
compileShadowShader uniformBuffer = compileShader $ do
    vertCfg <- getUniform (const (uniformBuffer, 0))
    fragCfg <- getUniform (const (uniformBuffer, 0))

    primitiveStream <- fmap (Shaders.vertShadow vertCfg) <$>
        toPrimitiveStream envPrimitives

    fragmentStream <- fmap (Shaders.fragShadow fragCfg) <$>
        rasterize (const (Back, ViewPort (V2 0 0) (V2 1000 1000), DepthRange 0 1)) primitiveStream

    drawDepth (\s -> (NoBlending, envShadowDepth s, DepthOption Less True)) fragmentStream $
    -- draw (const NoBlending) (fst <$> fragmentStream) $
        drawColor (\s -> (envShadowColor s, True, False))


compileCameraShader
    :: Window os RGBFloat ()
    -> UniformBuffer os
    -> ShadowColorTex os
    -> ColorTex os
    -> ContextT GLFW.Handle os IO (CameraShader os)
compileCameraShader win uniformBuffer shadowTex tex = compileShader $ do
    vertCfg <- getUniform (const (uniformBuffer, 0))
    fragCfg <- getUniform (const (uniformBuffer, 0))

    primitiveStream <- fmap (Shaders.vertCamera vertCfg) <$>
        toPrimitiveStream id

    shadowSampler <- newSampler2D (const (shadowTex, SamplerNearest, (pure Repeat, undefined)))
    texSampler <- newSampler2D (const (tex, SamplerNearest, (pure Repeat, undefined)))

    let shadowSamp = sample2D shadowSampler SampleAuto Nothing Nothing
    let texSamp = sample2D texSampler SampleAuto Nothing Nothing

    fragmentStream <- fmap (Shaders.fragCamera fragCfg shadowSamp texSamp) <$>
        rasterize (const (Front, ViewPort (V2 0 0) viewPort, DepthRange 0 1)) primitiveStream

    drawWindowColor (const (win, ContextColorOption NoBlending (pure True))) fragmentStream


main :: IO ()
main = do
    Env.setEnv "GPIPE_DEBUG" "1"
    runContextT GLFW.defaultHandleConfig{GLFW.configEventPolicy = Just $ GLFW.WaitTimeout $ 1 / fps} $ do
        let V2 w h = viewPort
        win <- newWindow (WindowFormatColor RGB8) $ (GLFW.defaultWindowConfig "LambdaRay")
            { GLFW.configWidth = w
            , GLFW.configHeight = h
            , GLFW.configHints = [GLFW.WindowHint'Samples (Just 4)]
            }

        vertexBuffer <- timeIt "Loading mesh" $ do
            mesh <- liftIO $ STL.stlToMesh <$> STL.mustLoadSTL "data/models/Bed.stl"
            vertexBuffer <- newBuffer $ length mesh
            writeBuffer vertexBuffer 0 mesh
            return vertexBuffer

        uniformBuffer <- newBuffer 1
        writeBuffer uniformBuffer 0 [defaultRuntimeConfig]

        tex <- newTexture2D R8 (V2 8 8) 1
        let whiteBlack = cycle [minBound,maxBound] :: [Word32]
            blackWhite = tail whiteBlack
        writeTexture2D tex 0 0 (V2 8 8) (cycle (take 8 whiteBlack ++ take 8 blackWhite))

        shadowColorTex <- newTexture2D R8 (V2 1000 1000) 1
        shadowDepthTex <- newTexture2D Depth16 (V2 1000 1000) 1

        shadowShader <- timeIt "Compiling shadow shader..." $ compileShadowShader uniformBuffer
        cameraShader <- timeIt "Compiling camera shader..." $ compileCameraShader win uniformBuffer shadowColorTex tex

        startTime <- liftIO Time.getCurrentTime
        loop win startTime vertexBuffer uniformBuffer shadowColorTex shadowDepthTex shadowShader cameraShader


updateUniforms :: Floating a => Time.UTCTime -> IO (RuntimeConfig a)
updateUniforms startTime = do
    now <- Time.getCurrentTime
    return defaultRuntimeConfig{time = fromRational $ toRational $ Time.diffUTCTime now startTime}


loop
    :: Window os RGBFloat ()
    -> Time.UTCTime
    -> VertexBuffer os
    -> UniformBuffer os
    -> ShadowColorTex os
    -> ShadowDepthTex os
    -> ShadowShader os
    -> CameraShader os
    -> ContextT GLFW.Handle os IO ()
loop win startTime vertexBuffer uniformBuffer shadowColorTex shadowDepthTex shadowShader cameraShader = do
    closeRequested <- timeIt "Rendering..." $ do
        cfg <- liftIO $ updateUniforms startTime
        writeBuffer uniformBuffer 0 [cfg]
  
        render $ do
            vertexArray <- newVertexArray vertexBuffer
            shadowColor <- getTexture2DImage shadowColorTex 0
            shadowDepth <- getTexture2DImage shadowDepthTex 0
            clearImageColor shadowColor 0
            clearImageDepth shadowDepth 1
            shadowShader ShaderEnvironment
                { envPrimitives = toPrimitiveArray TriangleList vertexArray
                , envShadowColor = shadowColor
                , envShadowDepth = shadowDepth
                }

        render $ do
            clearWindowColor win (V3 0 0 0.8)
            vertexArray <- newVertexArray vertexBuffer
            cameraShader $ toPrimitiveArray TriangleList vertexArray
  
        swapWindowBuffers win
  
        GLFW.windowShouldClose win

    liftIO $ threadDelay 10000
    unless (closeRequested == Just True) $
        loop win startTime vertexBuffer uniformBuffer shadowColorTex shadowDepthTex shadowShader cameraShader


timeIt :: (Info a, MonadIO m) => String -> m a -> m a
timeIt text m = do
    liftIO $ putStr ("[" ++ show Running ++ "] " ++ text) >> hFlush stdout
    s <- liftIO Time.getCurrentTime
    (r, (status, info)) <- getInfo <$> m
    liftIO $ putStr (" " ++ info ++ "\t") >> hFlush stdout
    e <- liftIO Time.getCurrentTime
    liftIO $ putStr $ show $ Time.nominalDiffTimeToSeconds $ Time.diffUTCTime e s
    liftIO $ putStrLn ("\r[" ++ show status)
    return r


data Status
    = Fail
    | Done
    | Running

instance Show Status where
    show Fail    = "\027[1;31mFAIL\027[0m"
    show Done    = "\027[1;32mDONE\027[0m"
    show Running = "\027[1;33m....\027[0m"

class Info a where
    getInfo :: a -> (a, (Status, String))

instance Info (Maybe a) where
    getInfo r@Nothing = (r, (Done, ""))
    getInfo r@Just{}  = (r, (Done, "OK"))

instance Info [a] where
    getInfo r = (r, (Done, "(" ++ show (length r) ++ ")"))

instance Info b => Info (Either a b) where
    getInfo r@Left{}     = (r, (Fail, ""))
    getInfo r@(Right ok) = (r, snd $ getInfo ok)

instance Info (Texture2D a b) where
    getInfo r = (r, (Done, ""))

instance Info (a -> b) where
    getInfo r = (r, (Done, ""))

instance Info (Buffer os a) where
    getInfo r = (r, (Done, show $ bufferLength r))
