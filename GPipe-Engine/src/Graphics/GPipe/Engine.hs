module Graphics.GPipe.Engine where

import           Control.Monad                (unless)
import           Graphics.GPipe               (ContextT, Depth, RGBFloat,
                                               V2 (..), Window,
                                               swapWindowBuffers)
import qualified Graphics.GPipe.Context.GLFW  as GLFW
import           Graphics.GPipe.Engine.TimeIt (timeItInPlace)


mainloop
    :: Window os RGBFloat Depth
    -> Bool
    -> (Window os RGBFloat Depth -> V2 Int -> a -> ContextT GLFW.Handle os IO ())
    -> a
    -> ContextT GLFW.Handle os IO ()
mainloop win timing renderer scene = loop
  where
    timeIt = if timing then timeItInPlace "Rendering..." else id

    loop = do
        closeRequested <- timeIt $ do
            windowSize <- (\(Just (w, h)) -> V2 w h) <$> GLFW.getWindowSize win

            renderer win windowSize scene

            swapWindowBuffers win

            GLFW.windowShouldClose win

        unless (closeRequested == Just True) loop

