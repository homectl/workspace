{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module LambdaCnc.GPU (main) where

import           Control.Concurrent          (threadDelay)
import           Control.Monad               (unless)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import qualified Data.Time.Clock             as Time
import           Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import           LambdaCnc.Config            (RuntimeConfig (..),
                                              defaultRuntimeConfig)
import qualified LambdaCnc.Shaders           as Shaders
import           Prelude                     hiding ((<*))
import qualified System.Environment          as Env
import           System.IO                   (hFlush, stdout)

type ShaderInput = (B4 Float, B2 Float)
type VertexBuffer os = Buffer os ShaderInput
type UniformBuffer os = Buffer os (Uniform (RuntimeConfig (B Float)))
type CncShader os = CompiledShader os (PrimitiveArray Triangles ShaderInput)

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

      vertexBuffer <- newBuffer 6
      writeBuffer vertexBuffer 0 [ (V4 (-1) (-1) 0 1, V2 0 0)
                                 , (V4 (-1) 1 0 1, V2 0 1)
                                 , (V4 1 1 0 1, V2 1 1)
                                 , (V4 (-1) (-1) 0 1, V2 0 0)
                                 , (V4 1 (-1) 0 1, V2 1 0)
                                 , (V4 1 1 0 1, V2 1 1)
                                 ]

      uniformBuffer <- newBuffer 1
      writeBuffer uniformBuffer 0 [defaultRuntimeConfig]

      shader <- timeIt "Compiling..." $ compilePipeline win uniformBuffer

      startTime <- liftIO Time.getCurrentTime
      loop win startTime vertexBuffer uniformBuffer shader


compilePipeline
    :: Window os RGBFloat ()
    -> UniformBuffer os
    -> ContextT GLFW.Handle os IO (CncShader os)
compilePipeline (win :: Window os RGBFloat ()) (uniformBuffer :: UniformBuffer os) = compileShader $ do
    vertCfg <- getUniform (const (uniformBuffer, 0))
    primitiveStream <- fmap (Shaders.vert vertCfg) <$>
        toPrimitiveStream id

    fragCfg <- getUniform (const (uniformBuffer, 0))
    fragmentStream <- fmap (Shaders.frag fragCfg) <$>
        rasterize (const (FrontAndBack, ViewPort (V2 0 0) viewPort, DepthRange 0 1)) primitiveStream

    drawWindowColor (const (win, ContextColorOption NoBlending (V3 True True True))) fragmentStream


updateUniforms :: Floating a => Time.UTCTime -> IO (RuntimeConfig a)
updateUniforms startTime = do
    now <- Time.getCurrentTime
    return defaultRuntimeConfig{time = fromRational $ toRational $ Time.diffUTCTime now startTime}


loop
    :: Window os RGBFloat ()
    -> Time.UTCTime
    -> VertexBuffer os
    -> UniformBuffer os
    -> CncShader os
    -> ContextT GLFW.Handle os IO ()
loop win startTime vertexBuffer uniformBuffer shader = do
    closeRequested <- timeIt "Rendering..." $ do
      cfg <- liftIO $ updateUniforms startTime
      writeBuffer uniformBuffer 0 [cfg]

      render $ do
        clearWindowColor win (V3 0 0 0.8)
        vertexArray <- newVertexArray vertexBuffer
        let primitiveArray = toPrimitiveArray TriangleList vertexArray
        shader primitiveArray
      swapWindowBuffers win

      GLFW.windowShouldClose win

    liftIO $ threadDelay 10000
    unless (closeRequested == Just True) $
      loop win startTime vertexBuffer uniformBuffer shader


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
