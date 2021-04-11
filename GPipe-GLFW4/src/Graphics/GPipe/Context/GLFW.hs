-- | Non interactive applications only need to pass configuration defined here
-- into GPipe's 'runContextT' and 'newWindow'.
--
-- Interactive applications will need "Graphics.GPipe.Context.GLFW.Input".
module Graphics.GPipe.Context.GLFW (
-- * GPipe context handler for GLFW
Handle(),
GLFWWindow(),
-- ** Configuration
-- *** Default configs
defaultHandleConfig,
defaultWindowConfig,
-- *** Custom configs
ContextHandlerParameters(HandleConfig, configErrorCallback, configEventPolicy),
-- | Configuration for the GLFW handle.
--
-- [@'HandleConfig'@] Constructor
--
-- [@'configErrorCallback' ::  'Error' -> String -> IO () @] Specify a callback to handle errors emitted by GLFW.
--
-- [@'configEventPolicy' :: Maybe 'EventPolicy'@] Specify the 'EventPolicy' to use for automatic GLFW event processing. If 'Nothing' then automatic event processing is disabled and you'll need to call 'mainloop' or 'mainstep' somewhere.
WindowConfig(..),
WindowHint(..),
EventPolicy(..),
-- ** Exceptions
InitException(..),
CreateWindowException(..),
UnsafeWindowHintsException(..),
-- ** Mainthread hooks
mainloop,
mainstep,
-- ** Reexports
module Graphics.GPipe.Context.GLFW.Input,
module Graphics.GPipe.Context.GLFW.Window,
module Graphics.GPipe.Context.GLFW.Misc
) where

-- internal
import           Graphics.GPipe.Context.GLFW.Format
import           Graphics.GPipe.Context.GLFW.Handler
import           Graphics.GPipe.Context.GLFW.Resource
-- reexports
import           Graphics.GPipe.Context.GLFW.Input
import           Graphics.GPipe.Context.GLFW.Misc
import           Graphics.GPipe.Context.GLFW.Window
-- GLFW reexports
import           Graphics.UI.GLFW                     (WindowHint (..))
