{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
-- | Geometry shader just emitting its input.
module Main (main) where

import           Control.Exception           (handle)
import           Control.Lens                ((^.))
import           Control.Monad               (unless, when)
import           Data.Function               ((&))
import           Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified System.Directory            as Dir
import           System.Environment          (setEnv)
import           System.Exit                 (exitFailure)
import           System.FilePath             (takeFileName, (</>))

windowSize :: V2 Int
windowSize = V2 1000 1000

--------------------------------------------------

vert :: (VPos, V3 VFloat) -> (VPos, V3 VFloat)
vert (pos, clr) = (point $ pos^._xyz / 2 - 0.25, clr / 10)

--------------------------------------------------

geom :: GGenerativeGeometry Triangles (VPos, V3 VFloat) -> Geometry Triangles (VPos, V3 VFloat) -> GGenerativeGeometry Triangles (VPos, V3 VFloat)
geom g (Triangle p1 p2 p3) = g
  & emitVertexPosition p1
  & emitVertexPosition p2
  & emitVertexPosition p3

  & endPrimitive

--------------------------------------------------

frag :: V3 FFloat -> V3 FFloat
frag clr = clr

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

  handle handler $ runContextT GLFW.defaultHandleConfig $ do
    win <- newWindow (WindowFormatColor RGB8) (GLFW.defaultWindowConfig "Geometry shader test")
      { GLFW.configWidth = windowSize^._x
      , GLFW.configHeight = windowSize^._y
      }
    vertexBuffer <- newBuffer 3
    writeBuffer vertexBuffer 0 [ (V4 (-1) 1 0 1, V3 1 0 0)
                               , (V4 0 (-1) 0 1, V3 0 1 0)
                               , (V4 1 1 0 1, V3 0 0 1)
                               ]

    shader <- compileShader $ do
      primitiveStream <- fmap vert
        <$> toPrimitiveStream id

      expandedGeometries <-
          fmap (geom generativeTriangleStrip)
              <$> geometrize primitiveStream

      fragmentStream <- fmap frag
        -- <$> rasterize (const (Front, PolygonFill, ViewPort (V2 0 0) windowSize, DepthRange 0 1)) primitiveStream
        <$> generateAndRasterize (const (FrontAndBack, PolygonFill, ViewPort (V2 0 0) windowSize, DepthRange 0 1)) 3 expandedGeometries

      drawWindowColor (const (win, ContextColorOption NoBlending (V3 True True True))) fragmentStream

    loop vertexBuffer shader win

handler :: GPipeException -> IO ()
handler (GPipeException err) = do
  putStrLn $ "GPipeException: " <> err
  exitFailure


loop
  :: Buffer os (B4 Float, B3 Float)
  -> (PrimitiveArray Triangles (B4 Float, B3 Float) -> Render os ())
  -> Window os RGBFloat ()
  -> ContextT GLFW.Handle os IO ()
loop vertexBuffer shader win = do
  render $ do
    clearWindowColor win (V3 0 0 0)
    vertexArray <- newVertexArray vertexBuffer
    let primitiveArray = toPrimitiveArray TriangleList vertexArray
    shader primitiveArray
  swapWindowBuffers win

  closeRequested <- GLFW.windowShouldClose win
  unless (closeRequested == Just True) $
    loop vertexBuffer shader win
