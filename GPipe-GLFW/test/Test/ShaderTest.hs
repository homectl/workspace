{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Test.ShaderTest (main, Shaders (..)) where

import           Control.Exception           (handle)
import           Control.Lens                ((^.))
import           Control.Monad               (unless)
import           Control.Monad.IO.Class      (liftIO)
import           Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified System.Environment          as Env
import           System.Exit                 (exitFailure)

windowSize :: V2 Int
windowSize = V2 1000 1000

data Shaders sa topology = Shaders
  { topology :: topology
  , vertices :: [(V3 Float, V3 Float, V3 Float)]
  , vert :: (V3 VFloat, V3 VFloat, V3 VFloat) -> (VPos, sa VFloat)
  , maxVertices :: Int
  , geom :: GGenerativeGeometry Triangles (VPos, sa VFloat) -> Geometry topology (VPos, sa VFloat) -> GGenerativeGeometry Triangles (VPos, sa VFloat)
  , frag :: FragmentFormat (sa VFloat) -> V3 FFloat
  }

main
  :: ( FragmentInput (sa VFloat)
     , AnotherVertexInput (sa VFloat)
     , FragmentCreator (sa VFloat)
     , PrimitiveTopology topology
     , GeometryInput topology (VPos, sa VFloat)
     )
  => Shaders sa topology
  -> IO ()
main Shaders{..} = do
  handle handler $ runContextT GLFW.defaultHandleConfig $ do
    win <- newWindow (WindowFormatColor RGB8) (GLFW.defaultWindowConfig "Geometry shader test")
      { GLFW.configWidth = windowSize^._x
      , GLFW.configHeight = windowSize^._y
      }
    vertexBuffer <- newBuffer $ length vertices
    writeBuffer vertexBuffer 0 vertices

    shader <- compileShader $ do
      primitiveStream <- fmap vert
        <$> toPrimitiveStream id

      expandedGeometries <-
          fmap (geom generativeTriangleStrip)
              <$> geometrize primitiveStream

      fragmentStream <- fmap frag
        -- <$> rasterize (const (Front, PolygonFill, ViewPort (V2 0 0) windowSize, DepthRange 0 1)) primitiveStream
        <$> generateAndRasterize (const (FrontAndBack, PolygonFill, ViewPort (V2 0 0) windowSize, DepthRange 0 1)) maxVertices expandedGeometries

      drawWindowColor (const (win, ContextColorOption NoBlending (V3 True True True))) fragmentStream

    isTest <- ("HASKELL_DIST_DIR" `elem`) . map fst <$> liftIO Env.getEnvironment
    Just start <- if isTest then liftIO GLFW.getTime else return (Just 1e100)
    loop start vertexBuffer shader topology win

handler :: GPipeException -> IO ()
handler (GPipeException err) = do
  putStrLn $ "GPipeException: " <> err
  exitFailure


loop
  :: PrimitiveTopology topology
  => Double
  -> Buffer os (B3 Float, B3 Float, B3 Float)
  -> (PrimitiveArray topology (B3 Float, B3 Float, B3 Float) -> Render os ())
  -> topology
  -> Window os RGBFloat ()
  -> ContextT GLFW.Handle os IO ()
loop start vertexBuffer shader topology win = do
  render $ do
    clearWindowColor win (V3 0.7 0.7 0.7)
    vertexArray <- newVertexArray vertexBuffer
    let primitiveArray = toPrimitiveArray topology vertexArray
    shader primitiveArray
  swapWindowBuffers win

  closeRequested <- GLFW.windowShouldClose win
  Just now <- liftIO GLFW.getTime
  unless (now - start > 1 || closeRequested == Just True) $
    loop start vertexBuffer shader topology win
