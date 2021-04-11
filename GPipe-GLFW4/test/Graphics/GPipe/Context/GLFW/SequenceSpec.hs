{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Graphics.GPipe.Context.GLFW.SequenceSpec (spec) where

import           Test.Hspec                  (Spec, describe, it)

import           Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Test.Common                 as C
import qualified Test.Control                as A

spec :: Spec
spec = do
    describe "Multiple sequential windows" $ do
        it "should render a scene on a window and then render again on a second window" $ do
            C.runContext GLFW.defaultHandleConfig $ do

                first <- newWindow (WindowFormatColorDepth SRGB8 Depth16) (GLFW.defaultWindowConfig "Sequence 1 (of 2)")
                resources <- C.initRenderContext first [C.yAxis]
                C.mainloop first (A.frames 30) resources C.continue
                deleteWindow first

                second <- newWindow (WindowFormatColorDepth SRGB8 Depth16) (GLFW.defaultWindowConfig "Sequence 2 (of 2)")
                C.mainloop second (A.frames 30) resources C.continue
                deleteWindow second
