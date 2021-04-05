{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Graphics.GPipe.Context.GLFW.FixedSpec (spec) where

import           Test.Hspec                  (Spec, describe, it)

import           Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Test.Common                 as C
import qualified Test.Control                as A

spec :: Spec
spec = do
    describe "Fixed frame count" $ do
        it ("should render a scene to a window for " ++ show maxFrames ++ " frames") $ do
            C.runContext GLFW.defaultHandleConfig $ do
                win <- newWindow (WindowFormatColorDepth RGB8 Depth16) (GLFW.defaultWindowConfig "Fixed")
                resources <- C.initRenderContext win [C.xAxis, C.yAxis, C.zAxis, C.plane]
                C.mainloop win (A.frames maxFrames) resources C.continue
            where
                maxFrames = 60
