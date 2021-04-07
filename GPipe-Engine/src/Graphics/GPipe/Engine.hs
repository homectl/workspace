module Graphics.GPipe.Engine where

import           Control.Concurrent.MVar      (MVar, readMVar)
import           Control.Monad                (unless)
import           Control.Monad.IO.Class       (liftIO)
import           Graphics.GPipe               (ContextT, Depth, RGBFloat,
                                               V2 (..), Window,
                                               swapWindowBuffers)
import qualified Graphics.GPipe.Context.GLFW  as GLFW
import           Graphics.GPipe.Engine.TimeIt (timeItInPlace)


mainloop
    :: Window os RGBFloat Depth
    -> Bool
    -> (Window os RGBFloat Depth -> V2 Int -> pipelineData -> pipelineState -> ContextT GLFW.Handle os IO ())
    -> pipelineData
    -> MVar pipelineState
    -> ContextT GLFW.Handle os IO ()
mainloop win timing renderer pipelineData pipelineState = loop
  where
    timeIt = if timing then timeItInPlace "Rendering..." else id

    loop = do
        closeRequested <- timeIt $ do
            windowSize <- (\(Just (w, h)) -> V2 w h) <$> GLFW.getWindowSize win

            liftIO (readMVar pipelineState) >>=
                renderer win windowSize pipelineData

            swapWindowBuffers win

            GLFW.windowShouldClose win

        unless (closeRequested == Just True) loop

