{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.GPipe.Engine where

import           Control.Concurrent.MVar      (MVar, putMVar, takeMVar)
import           Control.Monad                (unless)
import           Control.Monad.IO.Class       (liftIO)
import           Graphics.GPipe               (ContextT, Depth, RGBAFloat,
                                               Window, swapWindowBuffers)
import qualified Graphics.GPipe.Context.GLFW  as GLFW
import           Graphics.GPipe.Engine.TimeIt (timeItInPlace)


mainloop
    :: Window os RGBAFloat Depth
    -> Bool
    -> (pipelineState -> ContextT GLFW.Handle os IO pipelineState)
    -> (Window os RGBAFloat Depth -> pipelineData -> pipelineState -> ContextT GLFW.Handle os IO ())
    -> pipelineData
    -> MVar pipelineState
    -> ContextT GLFW.Handle os IO ()
mainloop win timing prepare render pipelineData pipelineState = loop
  where
    timeIt = if timing then timeItInPlace "Rendering..." else id

    loop = do
        closeRequested <- timeIt $ do
            -- Take the current state, make any necessary changes to it within
            -- the ContextT, then put it back so GLFW callbacks can proceed.
            state <- liftIO (takeMVar pipelineState) >>= prepare
            liftIO $ putMVar pipelineState state
            -- Render with the current state (events will be processed on the
            -- next iteration).
            render win pipelineData state

            swapWindowBuffers win

            GLFW.windowShouldClose win

        unless (closeRequested == Just True) loop

