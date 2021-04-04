module Graphics.GPipe.Context.GLFW.MultiSpec (spec) where

import           Test.Hspec                  (Spec, describe, it)

import           Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Test.Common as C

spec :: Spec
spec = do
    describe "Multi window test" $ do
        it "should use shared contexts to load resources and render different subsets to different windows" $ do
            C.runContext GLFW.defaultHandleConfig $ do
                win <- newWindow (WindowFormatColorDepth RGB8 Depth16) (GLFW.defaultWindowConfig "Multi")
                -- TODO: in this thread render the axes
                -- TODO: fork; in other thread render just C.plane
                return ()
