-- | Internal module wrapping calls to "Graphics.UI.GLFW"
--
-- The bulletpoints on each function are copied from the GLFW documentation.
module Graphics.GPipe.Context.GLFW.Calls where

-- stdlib
import qualified Control.Concurrent                 as Conc
import           Control.Monad                      (when)
import           Data.Maybe                         (fromMaybe)
import qualified Text.Printf                        as Text
-- thirdparty
import qualified Graphics.UI.GLFW                   as GLFW
-- local
import           Graphics.GPipe.Context.GLFW.Logger (LogLevel (..), Logger,
                                                     emitLog)

-- TODO: change from using explicit OnMain functions to passing a handle which implements the appropriate class (effect or fetch result)
-- TODO: maybe an OnMain monad would be good to reduce the number of RPCS? Not really necessary, since they can already be easily sequenced with IO

-- |
-- * This function may be called from any thread.
getCurrentContext :: IO (Maybe GLFW.Window)
getCurrentContext = GLFW.getCurrentContext

-- |
-- * 2x This function may be called from any thread.
-- * Reading and writing of the internal timer offset is not atomic, so it needs to be externally synchronized with calls to glfwSetTime.
say :: Logger -> LogLevel -> String -> IO ()
say logger lvl msg = do
    t <- GLFW.getTime
    tid <- Conc.myThreadId
    c <- getCurrentContext
    emitLog logger lvl $
        Text.printf "[%03.3fs, %s has %s]: %s\n" (fromMaybe (0/0) t) (show tid) (show c) msg

type OnMain a = IO a -> IO a
type EffectMain = IO () -> IO ()

-- |
-- * This function must only be called from the main thread.
init :: OnMain Bool -> IO Bool
init onMain = onMain GLFW.init

-- |
-- * This function may be called before glfwInit.
-- * The contexts of any remaining windows must not be current on any other thread when this function is called.
-- * ~~This function must not be called from a callback.~~
-- * This function must only be called from the main thread.
terminate :: EffectMain -> IO ()
terminate onMain = onMain GLFW.terminate

-- |
-- * This function may be called before glfwInit.
-- * This function must only be called from the main thread.
setErrorCallback :: EffectMain -> Maybe GLFW.ErrorCallback -> IO ()
setErrorCallback onMain callbackHuh = onMain $ GLFW.setErrorCallback callbackHuh

-- |
-- * There are many caveats: http://www.glfw.org/docs/latest/group__window.html#ga5c336fddf2cbb5b92f65f10fb6043344
-- * ~~This function must not be called from a callback.~~
-- * This function must only be called from the main thread.
createWindow :: OnMain (Maybe GLFW.Window) -> Int -> Int -> String -> Maybe GLFW.Monitor -> [GLFW.WindowHint] -> Maybe GLFW.Window -> IO (Maybe GLFW.Window)
createWindow onMain width height title monitor hints parent = onMain $ do
    GLFW.defaultWindowHints -- This function must only be called from the main thread.
    mapM_ GLFW.windowHint hints -- This function must only be called from the main thread.
    GLFW.createWindow width height title monitor parent

-- |
-- * If the context of the specified window is current on the main thread, it is detached before being destroyed.
-- * The context of the specified window must not be current on any other thread when this function is called.
-- * ~~This function must not be called from a callback.~~
-- * This function must only be called from the main thread.
--
-- Seems like it's ok to delete any of the shared contexts any time, per:
-- https://khronos.org/registry/OpenGL/specs/gl/glspec45.core.pdf (Section 5.1.1)
destroyWindow :: EffectMain -> GLFW.Window -> IO ()
destroyWindow onMain window = onMain $ GLFW.destroyWindow window

-- |
-- * 2x This function must only be called from the main thread.
windowHints :: EffectMain -> [GLFW.WindowHint] -> IO ()
windowHints onMain hints = onMain $ GLFW.defaultWindowHints >> mapM_ GLFW.windowHint hints

-- |
-- * This function may be called from any thread.
makeContextCurrent :: Logger -> String -> Maybe GLFW.Window -> IO ()
makeContextCurrent logger reason windowHuh = do
    ccHuh <- getCurrentContext
    when (ccHuh /= windowHuh) $ do
        emitLog logger DEBUG $ Text.printf "attaching %s, reason: %s" (show windowHuh) reason
        GLFW.makeContextCurrent windowHuh

-- |
-- * A context must be current on the calling thread. Calling this function without a current context will cause a GLFW_NO_CURRENT_CONTEXT error.
-- * This function is not called during context creation, leaving the swap interval set to whatever is the default on that platform. This is done because some swap interval extensions used by GLFW do not allow the swap interval to be reset to zero once it has been set to a non-zero value.
-- * This function may be called from any thread.
swapInterval :: Int -> IO ()
swapInterval = GLFW.swapInterval

-- |
-- * EGL: The context of the specified window must be current on the calling thread.
-- * This function may be called from any thread.
swapBuffers :: GLFW.Window -> IO ()
swapBuffers = GLFW.swapBuffers

-- | This function puts the calling thread to sleep until at least one event is available in the event queue.
-- * ~~This function must not be called from a callback.~~
-- * This function must only be called from the main thread.
waitEvents :: EffectMain -> IO ()
waitEvents onMain = onMain GLFW.waitEvents

-- | This function puts the calling thread to sleep until at least one event is available in the event queue.
-- * ~~This function must not be called from a callback.~~
-- * This function must only be called from the main thread.
waitEventsTimeout :: EffectMain -> Double -> IO ()
waitEventsTimeout onMain timeout = onMain $ GLFW.waitEventsTimeout timeout

-- | This function processes only those events that are already in the event queue and then returns immediately.
-- * ~~This function must not be called from a callback.~~
-- * This function must only be called from the main thread.
pollEvents :: EffectMain -> IO ()
pollEvents onMain = onMain GLFW.pollEvents

-- | This function posts an empty event from the current thread to the event queue, causing glfwWaitEvents or glfwWaitEventsTimeout to return.
-- * This function may be called from any thread.
postEmptyEvent :: IO ()
postEmptyEvent = GLFW.postEmptyEvent

-- |
-- * This function may be called from any thread. Access is not synchronized.
windowShouldClose :: GLFW.Window -> IO Bool
windowShouldClose = GLFW.windowShouldClose

-- |
-- * This function may be called from any thread. Access is not synchronized.
setWindowShouldClose :: GLFW.Window -> Bool -> IO ()
setWindowShouldClose = GLFW.setWindowShouldClose

-- |
-- * This function must only be called from the main thread.
setWindowCloseCallback :: EffectMain -> GLFW.Window -> Maybe GLFW.WindowCloseCallback -> IO ()
setWindowCloseCallback onMain window cb = onMain $ GLFW.setWindowCloseCallback window cb

-- |
-- * This function must only be called from the main thread.
getFramebufferSize :: OnMain (Int, Int) -> GLFW.Window -> IO (Int, Int)
getFramebufferSize onMain window = onMain $ GLFW.getFramebufferSize window

-- |
-- * This function must only be called from the main thread.
setKeyCallback :: EffectMain -> GLFW.Window -> Maybe GLFW.KeyCallback -> IO ()
setKeyCallback onMain window cb = onMain $ GLFW.setKeyCallback window cb

-- |
-- * Do not use this function to implement text input.
-- * This function must only be called from the main thread.
getKey :: OnMain GLFW.KeyState -> GLFW.Window -> GLFW.Key -> IO GLFW.KeyState
getKey onMain window key = onMain $ GLFW.getKey window key

-- | Implemented with glfwSetInputMode
-- * This function must only be called from the main thread.
setStickyKeysInputMode :: EffectMain -> GLFW.Window -> GLFW.StickyKeysInputMode -> IO ()
setStickyKeysInputMode onMain window mode = onMain $ GLFW.setStickyKeysInputMode window mode

-- | Implemented with glfwGetInputMode
-- * This function must only be called from the main thread.
getStickyKeysInputMode :: OnMain GLFW.StickyKeysInputMode -> GLFW.Window -> IO GLFW.StickyKeysInputMode
getStickyKeysInputMode onMain window = onMain $ GLFW.getStickyKeysInputMode window

-- |
-- * This function must only be called from the main thread.
setCharCallback :: EffectMain -> GLFW.Window -> Maybe GLFW.CharCallback -> IO ()
setCharCallback onMain window cb = onMain $ GLFW.setCharCallback window cb

-- |
-- * This function must only be called from the main thread.
setCursorPosCallback :: EffectMain -> GLFW.Window -> Maybe GLFW.CursorPosCallback -> IO ()
setCursorPosCallback onMain window cb = onMain $ GLFW.setCursorPosCallback window cb

-- |
-- * This function must only be called from the main thread.
getCursorPos :: OnMain (Double, Double) -> GLFW.Window -> IO (Double, Double)
getCursorPos onMain window = onMain $ GLFW.getCursorPos window

-- | Implemented with glfwSetInputMode
-- * This function must only be called from the main thread.
setCursorInputMode :: EffectMain -> GLFW.Window -> GLFW.CursorInputMode -> IO ()
setCursorInputMode onMain window mode = onMain $ GLFW.setCursorInputMode window mode

-- | Implemented with glfwGetInputMode
-- * This function must only be called from the main thread.
getCursorInputMode :: OnMain GLFW.CursorInputMode -> GLFW.Window -> IO GLFW.CursorInputMode
getCursorInputMode onMain window = onMain $ GLFW.getCursorInputMode window

-- |
-- * This function must only be called from the main thread.
setCursor :: EffectMain -> GLFW.Window -> GLFW.Cursor -> IO ()
setCursor onMain window cursor = onMain $ GLFW.setCursor window cursor

-- |
-- * This function must only be called from the main thread.
setCursorEnterCallback :: EffectMain -> GLFW.Window -> Maybe GLFW.CursorEnterCallback -> IO ()
setCursorEnterCallback onMain window cb = onMain $ GLFW.setCursorEnterCallback window cb

-- |
-- * This function must only be called from the main thread.
setMouseButtonCallback :: EffectMain -> GLFW.Window -> Maybe GLFW.MouseButtonCallback -> IO ()
setMouseButtonCallback onMain window cb = onMain $ GLFW.setMouseButtonCallback window cb

-- |
-- * This function must only be called from the main thread.
getMouseButton :: OnMain GLFW.MouseButtonState -> GLFW.Window -> GLFW.MouseButton -> IO GLFW.MouseButtonState
getMouseButton onMain window button = onMain $ GLFW.getMouseButton window button

-- | Implemented with glfwSetInputMode
-- * This function must only be called from the main thread.
setStickyMouseButtonsInputMode :: EffectMain -> GLFW.Window -> GLFW.StickyMouseButtonsInputMode -> IO ()
setStickyMouseButtonsInputMode onMain window mode = onMain $ GLFW.setStickyMouseButtonsInputMode window mode

-- | Implemented with glfwGetInputMode
-- * This function must only be called from the main thread.
getStickyMouseButtonsInputMode :: OnMain GLFW.StickyMouseButtonsInputMode -> GLFW.Window -> IO GLFW.StickyMouseButtonsInputMode
getStickyMouseButtonsInputMode onMain window = onMain $ GLFW.getStickyMouseButtonsInputMode window

-- |
-- * This function must only be called from the main thread.
setScrollCallback :: EffectMain -> GLFW.Window -> Maybe GLFW.ScrollCallback -> IO ()
setScrollCallback onMain window cb = onMain $ GLFW.setScrollCallback window cb

-- |
-- * This function must only be called from the main thread.
getClipboardString :: OnMain (Maybe String) -> GLFW.Window -> IO (Maybe String)
getClipboardString onMain window = onMain $ GLFW.getClipboardString window

-- |
-- * This function must only be called from the main thread.
setClipboardString :: EffectMain -> GLFW.Window -> String -> IO ()
setClipboardString onMain window s = onMain $ GLFW.setClipboardString window s

-- |
-- * This function must only be called from the main thread.
setDropCallback :: EffectMain -> GLFW.Window -> Maybe GLFW.DropCallback -> IO ()
setDropCallback onMain window cb = onMain $ GLFW.setDropCallback window cb

-- |
-- * This function must only be called from the main thread.
getWindowSize :: OnMain (Int, Int) -> GLFW.Window -> IO (Int, Int )
getWindowSize onMain window = onMain $ GLFW.getWindowSize window

-- |
-- * This function must only be called from the main thread.
setWindowSizeCallback :: EffectMain -> GLFW.Window -> Maybe GLFW.WindowSizeCallback -> IO ()
setWindowSizeCallback onMain window cb = onMain $ GLFW.setWindowSizeCallback window cb
