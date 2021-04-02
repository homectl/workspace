{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module LambdaRay.GPU (main) where

import           Control.Concurrent          (threadDelay)
import           Control.Monad               (unless)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import qualified Data.Time.Clock             as Time
import           Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified LambdaRay.Schwarzschild     as Schwarzschild
import           Prelude                     hiding ((<*))
import           System.IO                   (hFlush, stdout)


viewPort :: V2 Int
viewPort = V2 1500 1000


main :: IO ()
main = do
    runContextT GLFW.defaultHandleConfig $ do
      let V2 w h = viewPort
      win <- newWindow (WindowFormatColor RGB8) $ (GLFW.defaultWindowConfig "LambdaRay")
          { GLFW.configWidth = w
          , GLFW.configHeight = h
          }

      vertexBuffer :: Buffer os (B4 Float, B2 Float) <- newBuffer 6
      writeBuffer vertexBuffer 0 [ (V4 (-1) (-1) 0 1, V2 0 0)
                                 , (V4 (-1) 1 0 1, V2 0 1)
                                 , (V4 1 1 0 1, V2 1 1)
                                 , (V4 (-1) (-1) 0 1, V2 0 0)
                                 , (V4 1 (-1) 0 1, V2 1 0)
                                 , (V4 1 1 0 1, V2 1 1)
                                 ]

      uniformBuffer :: Buffer os (Uniform (B Float)) <- newBuffer 1
      writeBuffer uniformBuffer 0 [0]

      shader <- timeIt "Compiling... " $ compileShader $ do
        time <- getUniform (const (uniformBuffer, 0))
        primitiveStream <- toPrimitiveStream id
        fragmentStream <- rasterize (const (FrontAndBack, ViewPort (V2 0 0) viewPort, DepthRange 0 1)) primitiveStream
        let result = Schwarzschild.frag viewPort time <$> fragmentStream
        drawWindowColor (const (win, ContextColorOption NoBlending (V3 True True True))) result

      startTime <- liftIO Time.getCurrentTime
      loop startTime vertexBuffer uniformBuffer shader win


loop startTime vertexBuffer (uniformBuffer :: Buffer os (Uniform (B Float))) shader win = do
    closeRequested <- timeIt "Rendering... " $ do
      now <- liftIO Time.getCurrentTime
      writeBuffer uniformBuffer 0 [fromRational $ toRational $ Time.diffUTCTime now startTime]

      render $ do
        clearWindowColor win (V3 0 0 0.8)
        vertexArray <- newVertexArray vertexBuffer
        let primitiveArray = toPrimitiveArray TriangleList vertexArray
        shader primitiveArray
      swapWindowBuffers win

      GLFW.windowShouldClose win

    liftIO $ threadDelay 10000
    unless (closeRequested == Just True) $
      loop startTime vertexBuffer uniformBuffer shader win


timeIt :: MonadIO m => String -> m a -> m a
timeIt text m = do
  liftIO $ putStr text >> hFlush stdout
  s <- liftIO Time.getCurrentTime
  r <- m
  e <- liftIO Time.getCurrentTime
  liftIO $ print $ Time.nominalDiffTimeToSeconds $ Time.diffUTCTime e s
  return r
