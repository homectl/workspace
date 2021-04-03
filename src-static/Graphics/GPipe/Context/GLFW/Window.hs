-- | Window manipulation functions covering much of the GLFW __Window guide__:
-- <http://www.glfw.org/docs/latest/window_guide.html>.
-- Notably absent are the window creation functions. These are handled automatically by GPipe-GLFW.
--
-- Actions are in the GPipe 'GPipe.ContextT' monad when a window handle is required,
-- otherwise they are bare reexported IO actions which can be lifted into the 'GPipe.ContextT' monad.
-- The 'Window' taken by many of these functions is the window resource from GPipe.

module Graphics.GPipe.Context.GLFW.Window (
-- * Window objects
-- | Learn more: http://www.glfw.org/docs/latest/window_guide.html#window_object

-- * Window event processing
-- | GLFW event processing is performed by 'GPipe-GLFW' after each call to the 'GPipe' @swapBuffers@.
-- No further action is required, but additional controls are available for complex applications in
-- "Graphics.GPipe.Context.GLFW".

-- * Window properties and events
-- | Learn more: http://www.glfw.org/docs/latest/window_guide.html#window_properties

-- ** Window closing and close flag
windowShouldClose,
setWindowShouldClose,
setWindowCloseCallback,

-- ** Window size
getWindowSize,
setWindowSizeCallback,

-- ** Framebuffer size
-- | Reexported from "Graphics.GPipe.Context".
GPipe.getFrameBufferSize,

-- * Buffer swapping
-- | Buffer swapping is initiated via the 'GPipe' @swapBuffers@ function.

-- * Not supported
-- | Some GLFW functionality isn't currently exposed by "Graphics.UI.GLFW".
--
--      * `glfwSetWindowUserPointer`, `glfwGetWindowUserPointer`
) where

-- stdlib
import           Control.Monad.IO.Class               (MonadIO)
--thirdparty
import qualified Graphics.GPipe.Context               as GPipe (ContextT,
                                                                Window,
                                                                getFrameBufferSize)
--local
import qualified Graphics.GPipe.Context.GLFW.Calls    as Call
import           Graphics.GPipe.Context.GLFW.Handler  (Handle (..))
import qualified Graphics.GPipe.Context.GLFW.Wrappers as Wrappers

-- TODO: function docstrings

getWindowSize :: MonadIO m => GPipe.Window os c ds -> GPipe.ContextT Handle os m (Maybe (Int, Int))
getWindowSize = Wrappers.withWindowRPC Call.getWindowSize

setWindowSizeCallback :: MonadIO m => GPipe.Window os c ds -> Maybe (Int -> Int -> IO ()) -> GPipe.ContextT Handle os m (Maybe ())
setWindowSizeCallback = Wrappers.wrapCallbackSetter Call.setWindowSizeCallback

windowShouldClose :: MonadIO m => GPipe.Window os c ds -> GPipe.ContextT Handle os m (Maybe Bool)
windowShouldClose = Wrappers.withWindow Call.windowShouldClose

setWindowShouldClose :: MonadIO m => GPipe.Window os c ds -> Bool -> GPipe.ContextT Handle os m (Maybe ())
setWindowShouldClose w b = Wrappers.withWindow (`Call.setWindowShouldClose` b) w

setWindowCloseCallback :: MonadIO m => GPipe.Window os c ds -> Maybe (IO ()) -> GPipe.ContextT Handle os m (Maybe ())
setWindowCloseCallback = Wrappers.wrapCallbackSetter Call.setWindowCloseCallback
