{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE ViewPatterns        #-}
module LambdaRay.Main (main) where

import           Control.Monad                            (unless, when)
import           Control.Monad.IO.Class                   (liftIO)
import           Graphics.GPipe
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW (defaultHandleConfig,
                                                                   defaultWindowConfig,
                                                                   windowShouldClose)
import qualified "GLFW-b" Graphics.UI.GLFW                as GLFW (Window,
                                                                   getError,
                                                                   getWindowPos,
                                                                   setWindowPos,
                                                                   setWindowTitle)
import qualified LambdaRay.Schwarzschild                  as Schwarzschild
import           Unsafe.Coerce                            (unsafeCoerce)

newtype UnsafeGLFWHandle = UnsafeGLFWHandle ((), ((), (), GLFW.Window, (), ()))


main :: IO ()
main = do
    runContextT GLFW.defaultHandleConfig $ do
      win <- newWindow (WindowFormatColor RGB8) $ GLFW.defaultWindowConfig "Hello world!"

      vertexBuffer :: Buffer os (B4 Float, B3 Float) <- newBuffer 3
      writeBuffer vertexBuffer 0 [ (V4 (-1) (-1) 0 1, V3 1 0 0)
                                 , (V4 (-1) 1 0 1, V3 0 1 0)
                                 , (V4 1 1 0 1,  V3 0 0 1)
                                 ]

      shader <- Schwarzschild.compile win

      loop vertexBuffer shader win


loop vertexBuffer shader win = do
    render $ do
      clearWindowColor win (V3 1 0 0)
      vertexArray <- newVertexArray vertexBuffer
      let primitiveArray = toPrimitiveArray TriangleList vertexArray
      shader primitiveArray
    swapWindowBuffers win

    withContextWindow win $ \case
      Nothing -> return ()
      Just (unsafeCoerce -> UnsafeGLFWHandle ((), ((), (), w, (), ()))) -> do
        (x, y) <- GLFW.getWindowPos w
        GLFW.getError >>= \case
          Nothing | x /= 1000 || y /= 100 -> do
            GLFW.setWindowPos w 1000 100
            GLFW.setWindowTitle w "oho"
            GLFW.getError >>= print
          Just err -> print err
          _ -> return ()

    closeRequested <- GLFW.windowShouldClose win
    unless (closeRequested == Just True) $
      loop vertexBuffer shader win
