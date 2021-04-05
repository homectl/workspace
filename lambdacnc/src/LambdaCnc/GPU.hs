{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module LambdaCnc.GPU
  ( main
  ) where

import           Control.Concurrent          (threadDelay)
import           Control.Monad               (unless)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import qualified Data.Time.Clock             as Time
import           Data.Word                   (Word32)
import qualified Graphics.Formats.STL        as STL
import           Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import           LambdaCnc.Config            (RuntimeConfig (..), UniformBuffer,
                                              defaultRuntimeConfig)
import qualified LambdaCnc.STL               as STL
import qualified LambdaCnc.Shaders           as Shaders
import           Prelude                     hiding ((<*))
import qualified System.Environment          as Env
import           System.IO                   (hFlush, stdout)


fps :: Double
fps = 24

viewPort :: V2 Int
viewPort = V2 1500 1000


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
        let whiteBlack = cycle [minBound + maxBound `div` 4,maxBound - maxBound `div` 4] :: [Word32]
            blackWhite = tail whiteBlack
        writeTexture2D tex 0 0 (V2 8 8) (cycle (take 8 whiteBlack ++ take 8 blackWhite))

        shadowColorTex <- newTexture2D R8 (V2 1000 1000) 1
        shadowDepthTex <- newTexture2D Depth16 (V2 1000 1000) 1

        shadowShader <- timeIt "Compiling shadow shader..." $ Shaders.compileShadowShader uniformBuffer
        solidsShader <- timeIt "Compiling solids shader..." $ Shaders.compileSolidsShader win viewPort uniformBuffer shadowColorTex tex
        wireframeShader <- timeIt "Compiling wireframe shader..." $ Shaders.compileWireframeShader win viewPort uniformBuffer

        startTime <- liftIO Time.getCurrentTime
        loop win startTime vertexBuffer uniformBuffer shadowColorTex shadowDepthTex shadowShader solidsShader wireframeShader


updateUniforms :: Floating a => Time.UTCTime -> IO (RuntimeConfig a)
updateUniforms startTime = do
    now <- Time.getCurrentTime
    return defaultRuntimeConfig{time = fromRational $ toRational $ Time.diffUTCTime now startTime}


loop
    :: Window os RGBFloat ()
    -> Time.UTCTime
    -> Buffer os Shaders.ShaderInput
    -> UniformBuffer os
    -> Shaders.ShadowColorTex os
    -> Shaders.ShadowDepthTex os
    -> Shaders.ShadowShader os
    -> Shaders.SolidsShader os
    -> Shaders.SolidsShader os
    -> ContextT GLFW.Handle os IO ()
loop win startTime vertexBuffer uniformBuffer shadowColorTex shadowDepthTex shadowShader solidsShader wireframeShader = do
    closeRequested <- timeIt "Rendering..." $ do
        cfg <- liftIO $ updateUniforms startTime
        writeBuffer uniformBuffer 0 [cfg]

        render $ do
            vertexArray <- newVertexArray vertexBuffer
            shadowColor <- getTexture2DImage shadowColorTex 0
            shadowDepth <- getTexture2DImage shadowDepthTex 0
            clearImageColor shadowColor 0
            clearImageDepth shadowDepth 1
            shadowShader Shaders.ShadowShaderEnv
                { Shaders.envPrimitives = toPrimitiveArray TriangleList vertexArray
                , Shaders.envShadowColor = shadowColor
                , Shaders.envShadowDepth = shadowDepth
                }

        render $ do
            clearWindowColor win (V3 0 0 0.8)
            vertexArray <- newVertexArray vertexBuffer
            solidsShader $ toPrimitiveArray TriangleList vertexArray

        render $ do
            vertexArray <- newVertexArray vertexBuffer
            wireframeShader $ toPrimitiveArray TriangleList vertexArray

        swapWindowBuffers win

        GLFW.windowShouldClose win

    liftIO $ threadDelay 10000
    unless (closeRequested == Just True) $
        loop win startTime vertexBuffer uniformBuffer shadowColorTex shadowDepthTex shadowShader solidsShader wireframeShader


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
