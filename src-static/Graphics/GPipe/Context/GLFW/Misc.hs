-- | Miscellaneous wrapped calls to GLFW-b for application programmer use.
--
-- Actions are in the GPipe 'GPipe.ContextT' monad when a window handle is required,
-- otherwise they are bare reexported IO actions which can be lifted into the 'GPipe.ContextT' monad.
-- The 'Window' taken by many of these functions is the window resource from GPipe.
module Graphics.GPipe.Context.GLFW.Misc (
-- * Error handling
-- | Learn more: http://www.glfw.org/docs/latest/intro_guide.html#error_handling

-- | To set a custom error callback use 'HandleConfig' in "Graphics.GPipe.Context.GLFW".
Error(..),
) where

-- thirdparty
import           Graphics.UI.GLFW (Error (..))
