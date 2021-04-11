{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Graphics.GPipe.Context.GLFW.CloseSpec (spec) where

import           Test.Hspec                  (Spec, describe, it)

import           Control.Monad               (when)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Functor                (void)
import           Data.Maybe                  (fromMaybe)
import           Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Test.Common                 as C
import qualified Test.Control                as A

spec :: Spec
spec = do
    describe "Window-should-close interfaces" $ do
        it "should render a scene using additional additional GLFW interfaces" $ do
            C.runContext handleConfig $ do
                win <- newWindow (WindowFormatColorDepth RGB8 Depth16) (GLFW.defaultWindowConfig "Window-should-close")
                void $ GLFW.setWindowCloseCallback win $ Just onCloseButton
                resources <- C.initRenderContext win [C.xAxis, C.yAxis, C.zAxis]
                C.mainloop win (A.repeat $ A.seconds 1.0) resources $ \controller -> do
                    Just t <- liftIO $ GLFW.getTime
                    when (t > 3.5) $ do
                        liftIO $ putStrLn "!! Programmatically setting window-close bit"
                        Just () <- GLFW.setWindowShouldClose win True
                        return ()
                    shouldClose <- GLFW.windowShouldClose win
                    return $ fromMaybe False shouldClose
            where
                onCloseButton = putStrLn "!! Window-close button pressed"
                handleConfig = GLFW.defaultHandleConfig {GLFW.configErrorCallback=curry print :: GLFW.Error -> String -> IO ()}
