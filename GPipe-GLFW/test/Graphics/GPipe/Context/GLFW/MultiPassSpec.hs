{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Graphics.GPipe.Context.GLFW.MultiPassSpec (spec) where

import           Test.Hspec                  (Spec, describe, it)

import           Control.Lens                ((^.))
import           Control.Monad               (unless)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Word                   (Word32)
import           Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Test.Common                 as C

spec :: Spec
spec = do
    describe "Multi-pass render" $ do
        it "should render a quad with another rendering on it" $ do
            C.runContext GLFW.defaultHandleConfig $ do
                win <- newWindow (WindowFormatColor RGB8) (GLFW.defaultWindowConfig "Checkers")
                vertexBuffer :: Buffer os (B2 Float) <- newBuffer 4
                writeBuffer vertexBuffer 0 [V2 0 0, V2 1 0, V2 0 1, V2 1 1]
                tex <- newTexture2D R8 (V2 8 8) 1
                let whiteBlack = cycle [minBound,maxBound] :: [Word32]
                    blackWhite = tail whiteBlack
                writeTexture2D tex 0 0 (V2 8 8) (cycle (take 8 whiteBlack ++ take 8 blackWhite))

                colorTex <- newTexture2D RG8 (V2 256 256) 1
                depthTex <- newTexture2D Depth16 (V2 256 256) 1

                shader1 <- compileShader $ do
                  texMappedFragmentStream <- getProjectedFragments 256 (V3 0.5 (-0.8) (-0.8)) (V3 0.5 0.5 0) (V3 0 1 0)  textureMappedPrimitives
                  solidFragmentStream <- getProjectedFragments 256 (V3 (-0.6) (-0.6) 0.8) (V3 0.25 0.25 0) (V3 0 1 0) solidPrimitives
                  let filterMode = SamplerFilter Nearest Nearest Nearest Nothing
                      edge = (pure ClampToEdge, 0)
                  samp <- newSampler2D (const (tex, filterMode, edge))
                  let sampleTexture = sample2D samp SampleAuto Nothing Nothing
                      texMappedFragmentStream2 = filterFragments ((>* 0.5) . sampleTexture) texMappedFragmentStream
                      texMappedFragmentStream3 = fmap (const (V2 1 0)) texMappedFragmentStream2
                      solidFragmentStream2 = fmap (const (V2 0 1)) solidFragmentStream
                      fragmentStream = solidFragmentStream2 `mappend` texMappedFragmentStream3
                      fragmentStream2 = withRasterizedInfo (\a r -> (a, rasterizedFragCoord r ^. _z)) fragmentStream
                  drawDepth (\s -> (NoBlending, depthImage s, DepthOption Less True)) fragmentStream2 $ \ a -> do
                    drawColor (\ s -> (colorImage s, pure True, False)) a

                shader2 <- compileShader $ do
                  fragmentStream <- getProjectedFragments 800 (V3 1 2 2) (V3 0.5 0.5 0) (V3 0 1 0) id

                  let filterMode = SamplerFilter Linear Linear Nearest Nothing
                      edge = (pure ClampToEdge, 0)
                  samp <- newSampler2D (const (colorTex, filterMode, edge))
                  let sampleTexture = sample2D samp SampleAuto Nothing Nothing
                      fragmentStream2 = fmap ((\(V2 r g) -> V3 r 0 g) . sampleTexture) fragmentStream
                  drawWindowColor (const (win, ContextColorOption NoBlending (pure True))) fragmentStream2

                Just start <- liftIO GLFW.getTime
                renderLoop start win [
                  do
                    vertexArray <- newVertexArray vertexBuffer
                    let singleTriangle = takeVertices 3 vertexArray
                    cImage <- getTexture2DImage colorTex 0
                    dImage <- getTexture2DImage depthTex 0
                    clearImageColor cImage 0
                    clearImageDepth dImage 1
                    shader1 $ ShaderEnvironment
                        (toPrimitiveArray TriangleStrip vertexArray)
                        (toPrimitiveArray TriangleList singleTriangle)
                        cImage
                        dImage
                  ,
                  do
                    clearWindowColor win 0.5
                    vertexArray <- newVertexArray vertexBuffer
                    shader2 (toPrimitiveArray TriangleStrip vertexArray)
                  ]

getProjectedFragments size eye center up sf = do
  primitiveStream <- toPrimitiveStream sf
  let primitiveStream2 = fmap (\pos2d -> (make3d eye center up pos2d, pos2d)) primitiveStream
  rasterize (const (FrontAndBack, PolygonFill, ViewPort (V2 0 0) (V2 size size), DepthRange 0 1)) primitiveStream2

make3d :: Floating a => V3 a -> V3 a -> V3 a -> V2 a -> V4 a
make3d eye center up (V2 x y) = projMat !*! viewMat !* V4 x y 0 1
  where
    viewMat = lookAt eye center up
    projMat = perspective (pi/3) 1 1 100

renderLoop :: Double -> Window os c ds -> [Render os ()] -> ContextT GLFW.Handle os IO ()
renderLoop start win renderings = do
  mapM_ render renderings
  swapWindowBuffers win
  closeRequested <- GLFW.windowShouldClose win
  Just now <- liftIO GLFW.getTime
  unless (now - start > 1 || closeRequested == Just True) $
    renderLoop start win renderings


data ShaderEnvironment = ShaderEnvironment
  {
    textureMappedPrimitives, solidPrimitives  :: PrimitiveArray Triangles (B2 Float),
    colorImage :: Image (Format RGFloat),
    depthImage :: Image (Format Depth)
  }
