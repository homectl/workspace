-- | User input functions covering much of the GLFW __Input guide__:
-- <http://www.glfw.org/docs/latest/input_guide.html>.
--
-- Actions are in the GPipe 'GPipe.ContextT' monad when a window handle is required,
-- otherwise they are bare reexported IO actions which can be lifted into the 'GPipe.ContextT' monad.
-- The 'Window' taken by many of these functions is the window resource from GPipe.

module Graphics.GPipe.Context.GLFW.Input (

 -- * Event processing
 -- | Learn more: http://www.glfw.org/docs/latest/input_guide.html#events
 --
 --     * `glfwPollEvents`
 --     * `glfwWaitEvents`
 --
 -- GLFW Events are processed after each buffer swap by default. To change
 -- event processing construct a 'HandleConfig' for 'runContextT'. For greater
 -- control use the 'mainloop' and 'mainstep' functions provided by
 -- "Graphics.GPipe.Context.GLFW".
 GLFW.postEmptyEvent,
 -- | Force wake from 'waitEvents' with a dummy event.

 -- * Keyboard input
 -- | Learn more: http://www.glfw.org/docs/latest/input_guide.html#input_keyboard

 -- ** Key input
 setKeyCallback,
 getKey,
 setStickyKeysInputMode,
 getStickyKeysInputMode,

 -- ** Text input
 setCharCallback,

 -- * Mouse input
 -- | Learn more: http://www.glfw.org/docs/latest/input_guide.html#input_mouse

 -- ** Cursor position
 setCursorPosCallback,
 getCursorPos,

 -- ** Cursor modes
 setCursorInputMode,
 getCursorInputMode,

 -- ** Cursor objects

 -- *** Custom cursor creation
 GLFW.createCursor,

 -- *** Standard cursor creation
 GLFW.createStandardCursor,

 -- *** Cursor destruction
 GLFW.destroyCursor,

 -- *** Cursor setting
 setCursor,

 -- ** Cursor enter/leave events
 setCursorEnterCallback,

 -- ** Mouse button input
 setMouseButtonCallback,
 getMouseButton,
 setStickyMouseButtonsInputMode,
 getStickyMouseButtonsInputMode,

 -- ** Scroll input
 setScrollCallback,

 -- * Joystick input
 -- | Learn more: http://www.glfw.org/docs/latest/input_guide.html#joystick
 GLFW.joystickPresent,
 -- | Is the specified 'Joystick' currently connected?

 -- ** Joystick axis states
 GLFW.getJoystickAxes,
 -- | Poll for the positions of each axis on the 'Joystick'. Positions are on the range `-1.0..1.0`.

 -- ** Joystick button states
 GLFW.getJoystickButtons,
 -- | Poll for the 'JoystickButtonState' of each button on the 'Joystick'.

 -- ** Joystick name
 GLFW.getJoystickName,
 -- | Retrieve a non-unique string identifier for the 'Joystick'.
 -- This might be the make & model name of the device.

 -- * Time input
 -- | Learn more: http://www.glfw.org/docs/latest/input_guide.html#time
 GLFW.getTime,
 -- | Poll for the number of seconds since GLFW was initialized by GPipe.
 GLFW.setTime,
 -- | Manually set the timer to a specified value.

 -- * Clipboard input and output
 -- | Learn more: http://www.glfw.org/docs/latest/input_guide.html#clipboard
 getClipboardString,
 setClipboardString,

 -- * Path drop input
 -- | Learn more: http://www.glfw.org/docs/latest/input_guide.html#path_drop
 setDropCallback,

 -- * Reexported datatypes

 -- ** Keyboard
 Key(..),
 KeyState(..),
 ModifierKeys(..),
 StickyKeysInputMode(..),

 -- ** Mouse
 CursorInputMode(..),
 StandardCursorShape(..),
 CursorState(..),
 StickyMouseButtonsInputMode(..),
 MouseButton(..),
 MouseButtonState(..),

 -- ** Joystick
 Joystick(..),
 JoystickButtonState(..),

 -- * Not supported
 -- | Some GLFW functionality isn't currently exposed by "Graphics.UI.GLFW".
 --
 --     * `glfwWaitEventsTimeout`
 --     * `glfwSetCharModsCallback`
 --     * `glfwGetKeyName`
 --     * `glfwSetJoystickCallback`
 --     * `glfwGetTimerValue`
 --     * `glfwGetTimerFrequency`
 ) where

-- stdlib
import           Control.Monad.IO.Class               (MonadIO)
import qualified Graphics.GPipe.Context               as GPipe (ContextT,
                                                                Window)
-- third party
import           Graphics.UI.GLFW                     (Cursor (..),
                                                       CursorInputMode (..),
                                                       CursorState (..),
                                                       Joystick (..),
                                                       JoystickButtonState (..),
                                                       Key (..), KeyState (..),
                                                       ModifierKeys (..),
                                                       MouseButton (..),
                                                       MouseButtonState (..),
                                                       StandardCursorShape (..),
                                                       StickyKeysInputMode (..),
                                                       StickyMouseButtonsInputMode (..))
import qualified Graphics.UI.GLFW                     as GLFW
-- local
import qualified Graphics.GPipe.Context.GLFW.Calls    as Call
import           Graphics.GPipe.Context.GLFW.Handler  (Handle (..))
import           Graphics.GPipe.Context.GLFW.Wrappers (withWindowRPC,
                                                       wrapCallbackSetter,
                                                       wrapWindowFun)



{- Keyboard -}

-- | Register or unregister a callback to receive 'KeyState' changes to any 'Key'.
setKeyCallback :: MonadIO m => GPipe.Window os c ds -> Maybe (Key -> Int -> KeyState -> ModifierKeys -> IO ()) -> GPipe.ContextT Handle os m (Maybe ())
setKeyCallback = wrapCallbackSetter Call.setKeyCallback

-- | Poll for the 'KeyState' of a 'Key'.
getKey :: MonadIO m => GPipe.Window os c ds -> Key -> GPipe.ContextT Handle os m (Maybe KeyState)
getKey = wrapWindowFun Call.getKey

-- | Polling a 'Key' for 'KeyState' may sometimes miss state transitions.
-- If you use cannot use a callback to receive 'KeyState' changes,
-- use 'getKey' in combination with GLFW's sticky-keys feature:
-- <http://www.glfw.org/docs/latest/input_guide.html#input_key>.
setStickyKeysInputMode :: MonadIO m => GPipe.Window os c ds -> StickyKeysInputMode -> GPipe.ContextT Handle os m (Maybe ())
setStickyKeysInputMode = wrapWindowFun Call.setStickyKeysInputMode

getStickyKeysInputMode :: MonadIO m => GPipe.Window os c ds -> GPipe.ContextT Handle os m (Maybe StickyKeysInputMode)
getStickyKeysInputMode = withWindowRPC Call.getStickyKeysInputMode

-- | Register or unregister a callback to receive character input obeying keyboard layouts and modifier effects.
setCharCallback :: MonadIO m => GPipe.Window os c ds -> Maybe (Char -> IO ()) -> GPipe.ContextT Handle os m (Maybe ())
setCharCallback = wrapCallbackSetter Call.setCharCallback

{- Mouse -}

-- | Register or unregister a callback to receive mouse location changes.
-- Callback receives `x` and `y` position measured in screen-coordinates relative to the top left of the GLFW window.
setCursorPosCallback :: MonadIO m => GPipe.Window os c ds -> Maybe (Double -> Double -> IO ()) -> GPipe.ContextT Handle os m (Maybe ())
setCursorPosCallback = wrapCallbackSetter Call.setCursorPosCallback

-- | Poll for the location of the mouse.
getCursorPos :: MonadIO m => GPipe.Window os c ds -> GPipe.ContextT Handle os m (Maybe (Double, Double))
getCursorPos = withWindowRPC Call.getCursorPos

-- | GLFW supports setting cursor mode to support mouselook and other advanced uses of the mouse:
-- <http://www.glfw.org/docs/latest/input_guide.html#cursor_mode>.
setCursorInputMode :: MonadIO m => GPipe.Window os c ds -> CursorInputMode -> GPipe.ContextT Handle os m (Maybe ())
setCursorInputMode = wrapWindowFun Call.setCursorInputMode

getCursorInputMode :: MonadIO m => GPipe.Window os c ds -> GPipe.ContextT Handle os m (Maybe CursorInputMode)
getCursorInputMode = withWindowRPC Call.getCursorInputMode

-- | Set the cursor to be displayed over the window while 'CursorInputMode' is `Normal`.
setCursor :: MonadIO m => GPipe.Window os c ds -> Cursor -> GPipe.ContextT Handle os m (Maybe ())
setCursor = wrapWindowFun Call.setCursor

-- | Register or unregister a callback to receive 'CursorState' changes when the cursor enters or exits the window.
setCursorEnterCallback :: MonadIO m => GPipe.Window os c ds -> Maybe (CursorState -> IO ()) -> GPipe.ContextT Handle os m (Maybe ())
setCursorEnterCallback = wrapCallbackSetter Call.setCursorEnterCallback

-- | Register or unregister a callback to receive 'MouseButtonState' changes to a 'MouseButton'.
setMouseButtonCallback :: MonadIO m => GPipe.Window os c ds -> Maybe (MouseButton -> MouseButtonState -> ModifierKeys -> IO ()) -> GPipe.ContextT Handle os m (Maybe ())
setMouseButtonCallback = wrapCallbackSetter Call.setMouseButtonCallback

-- | Poll for the 'MouseButtonState' of a 'MouseButton'.
getMouseButton :: MonadIO m => GPipe.Window os c ds -> MouseButton -> GPipe.ContextT Handle os m (Maybe MouseButtonState)
getMouseButton = wrapWindowFun Call.getMouseButton

-- | Polling a 'MouseButton' for 'MouseButtonState' may sometimes miss state transitions.
-- If you use cannot use a callback to receive 'MouseButtonState' changes,
-- use 'getMouseButton' in combination with GLFW's sticky-mouse-buttons feature:
-- <http://www.glfw.org/docs/latest/input_guide.html#input_mouse_button>.
setStickyMouseButtonsInputMode :: MonadIO m => GPipe.Window os c ds -> StickyMouseButtonsInputMode -> GPipe.ContextT Handle os m (Maybe ())
setStickyMouseButtonsInputMode = wrapWindowFun Call.setStickyMouseButtonsInputMode

getStickyMouseButtonsInputMode :: MonadIO m => GPipe.Window os c ds -> GPipe.ContextT Handle os m (Maybe StickyMouseButtonsInputMode)
getStickyMouseButtonsInputMode = withWindowRPC Call.getStickyMouseButtonsInputMode

-- | Register or unregister a callback to receive scroll offset changes.
setScrollCallback :: MonadIO m => GPipe.Window os c ds -> Maybe (Double -> Double -> IO ()) -> GPipe.ContextT Handle os m (Maybe ())
setScrollCallback = wrapCallbackSetter Call.setScrollCallback

{- Joystick -}

{- Time -}

{- Clipboard -}

-- | Poll the system clipboard for a UTF-8 encoded string, if one can be extracted.
getClipboardString :: MonadIO m => GPipe.Window os c ds -> GPipe.ContextT Handle os m (Maybe (Maybe String))
getClipboardString = withWindowRPC Call.getClipboardString

-- | Store a UTF-8 encoded string in the system clipboard.
setClipboardString :: MonadIO m => GPipe.Window os c ds -> String -> GPipe.ContextT Handle os m (Maybe ())
setClipboardString = wrapWindowFun Call.setClipboardString

{- Pathdrop -}

-- | Register or unregister a callback to receive file paths when files are dropped onto the window.
setDropCallback :: MonadIO m => GPipe.Window os c ds -> Maybe ([String] -> IO ()) -> GPipe.ContextT Handle os m (Maybe ())
setDropCallback = wrapCallbackSetter Call.setDropCallback
