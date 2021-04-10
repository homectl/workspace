{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports   #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeFamilies     #-}
-- | Simple example with a geometry shader that draws houses on points.
module Main (main) where

import           Control.Arrow               (returnA)
import           Control.Lens                ((^.))
import           Control.Monad               (unless, when)
import           Data.Function               ((&))
import           Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified System.Directory            as Dir
import           System.Environment          (setEnv)
import           System.FilePath             (takeFileName, (</>))

windowSize :: V2 Int
windowSize = V2 1000 1000

newtype ShaderAttachment a = ShaderAttachment
  { color  :: V3 a
  -- TODO: geom doesn't work with more than 1 shader attachment
  -- , normal :: V3 a
  }
  deriving (Show)

instance FragmentInput a => FragmentInput (ShaderAttachment a) where
  type FragmentFormat (ShaderAttachment a) = ShaderAttachment (FragmentFormat a)
  toFragment = proc ~(ShaderAttachment a) -> do
    a' <- toFragment -< a
    returnA -< ShaderAttachment a'

instance FragmentCreator a => FragmentCreator (ShaderAttachment a) where
  createFragment = ShaderAttachment
    <$> createFragment

instance AnotherVertexInput a => AnotherVertexInput (ShaderAttachment a) where
  toAnotherVertex = proc ~(ShaderAttachment a) -> do
    a' <- toAnotherVertex -< a
    returnA -< ShaderAttachment a'

instance AnotherFragmentInput a => AnotherFragmentInput (ShaderAttachment a) where
  toFragment2 = proc ~(ShaderAttachment a) -> do
    a' <- toFragment2 -< a
    returnA -< ShaderAttachment a'

instance GeometryExplosive a => GeometryExplosive (ShaderAttachment a) where
    exploseGeometry (ShaderAttachment x) n = exploseGeometry x n

--------------------------------------------------

vert :: (V3 VFloat, V3 VFloat) -> (VPos, ShaderAttachment VFloat)
vert (pos, clr) = (projMat !*! viewMat !* point pos, ShaderAttachment{color=clr / 10})
  where
    viewMat = lookAt (V3 2 2 1) (V3 0 0 0) (V3 0 0 1)
    projMat = perspective (pi/3) 1 1 100

--------------------------------------------------

geom
  :: GGenerativeGeometry Triangles (VPos, ShaderAttachment VFloat)
  -> Geometry Points (VPos, ShaderAttachment VFloat)
  -> GGenerativeGeometry Triangles (VPos, ShaderAttachment VFloat)
geom g (Point (pos, sa@ShaderAttachment{..})) = g
  & emitVertex (pos + V4 (-0.1) (-0.1) 0 0, sa)
  & emitVertex (pos + V4   0.1  (-0.1) 0 0, sa)
  & emitVertex (pos + V4 (-0.1)   0.1  0 0, sa{color=color * 2})
  & emitVertex (pos + V4   0.1    0.1  0 0, sa{color=color * 2})
  & emitVertex (pos + V4   0.0    0.2  0 0, sa{color=color * 2.5})

  & endPrimitive

--------------------------------------------------

frag :: ShaderAttachment FFloat -> V3 FFloat
frag ShaderAttachment{..} = color

--------------------------------------------------

cleanupShaders :: IO ()
cleanupShaders = do
  let prefix = "generated-shaders"
  files <- map (prefix </>) . filter (/= "README.md") <$> Dir.listDirectory prefix
  mapM_ Dir.removeFile files

ensureCurrentDirectory :: FilePath -> IO ()
ensureCurrentDirectory target = do
  curDir <- takeFileName <$> Dir.getCurrentDirectory
  when (curDir /= target) $ Dir.setCurrentDirectory curDir


main :: IO ()
main = do
  ensureCurrentDirectory "GPipe-GLFW"
  setEnv "GPIPE_DEBUG" "1"
  cleanupShaders

  runContextT GLFW.defaultHandleConfig $ do
    win <- newWindow (WindowFormatColor RGB8) (GLFW.defaultWindowConfig "Geometry shader test")
      { GLFW.configWidth = windowSize^._x
      , GLFW.configHeight = windowSize^._y
      }

    let points =
          [ (V3 (x / 5) (y / 5) 0, V3 0.5 1 0.5)
          | x <- [-5..5], y <- [-5..5]
          ]
    vertexBuffer <- newBuffer $ length points
    writeBuffer vertexBuffer 0 points

    shader <- compileShader $ do
      primitiveStream <- fmap vert
        <$> toPrimitiveStream id

      expandedGeometries <-
          fmap (geom generativeTriangleStrip)
              <$> geometrize primitiveStream

      fragmentStream <- fmap frag
        -- <$> rasterize (const (Front, PolygonFill, ViewPort (V2 0 0) windowSize, DepthRange 0 1)) primitiveStream
        <$> generateAndRasterize (const (FrontAndBack, PolygonFill, ViewPort (V2 0 0) windowSize, DepthRange 0 1)) 5 expandedGeometries

      drawWindowColor (const (win, ContextColorOption NoBlending (V3 True True True))) fragmentStream

    loop vertexBuffer shader win


loop
  :: Buffer os (B3 Float, B3 Float)
  -> (PrimitiveArray Points (B3 Float, B3 Float) -> Render os ())
  -> Window os RGBFloat ()
  -> ContextT GLFW.Handle os IO ()
loop vertexBuffer shader win = do
  render $ do
    clearWindowColor win (V3 0.7 0.7 0.7)
    vertexArray <- newVertexArray vertexBuffer
    let primitiveArray = toPrimitiveArray PointList vertexArray
    shader primitiveArray
  swapWindowBuffers win

  closeRequested <- GLFW.windowShouldClose win
  unless (closeRequested == Just True) $
    loop vertexBuffer shader win
