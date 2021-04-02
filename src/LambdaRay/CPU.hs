{-# LANGUAGE ScopedTypeVariables #-}
module LambdaRay.CPU where

import           Control.Monad               (unless)
import           Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified LambdaRay.CPUImage          as CPUImage

main :: IO ()
main = do
  let (img, viewPort) = CPUImage.image

  runContextT GLFW.defaultHandleConfig $ do
    win <- newWindow (WindowFormatColor RGB8) (GLFW.defaultWindowConfig "LambdaRay: CPU")
        { GLFW.configWidth = 2000
        , GLFW.configHeight = 1000
        }
    vertexBuffer :: Buffer os (B2 Float) <- newBuffer 4
    writeBuffer vertexBuffer 0 [V2 (-1) (-1), V2 1 (-1), V2 (-1) 1, V2 1 1]
    tex <- newTexture2D RGB16F viewPort 1
    writeTexture2D tex 0 0 viewPort img
    shader <- compileShader $ do
      primitiveStream <- fmap (\(V2 x y) -> (V4 x y 0 1, V2 x y)) <$> toPrimitiveStream id

      samp <- newSampler2D (const (tex, SamplerFilter Nearest Nearest Nearest Nothing, (pure Repeat, 0)))
      let sampleTexture (V2 x y) = sample2D samp SampleAuto Nothing Nothing (V2 (x * 0.5 - 0.35) (y * 0.5 + 0.4))

      fragmentStream <- fmap sampleTexture <$> rasterize (const (FrontAndBack, ViewPort (V2 0 0) (V2 2000 1000), DepthRange 0 1)) primitiveStream

      drawWindowColor (const (win, ContextColorOption NoBlending (V3 True True True))) fragmentStream

    renderLoop win $ do
      clearWindowColor win 0.5
      vertexArray <- newVertexArray vertexBuffer
      shader (toPrimitiveArray TriangleStrip vertexArray)


renderLoop win rendering = do
  render rendering
  swapWindowBuffers win
  closeRequested <- GLFW.windowShouldClose win
  unless (closeRequested == Just True) $
    renderLoop win rendering
