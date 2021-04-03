-- | Internal module defining resources and associated types
module Graphics.GPipe.Context.GLFW.Resource where

-- thirdparty
import qualified Graphics.UI.GLFW as GLFW (Monitor, WindowHint)

-- | Configuration for a new GLFW window and associated OpenGL context.
data WindowConfig = WindowConfig
    { configWidth        :: Int
    , configHeight       :: Int
    , configTitle        :: String
    , configMonitor      :: Maybe GLFW.Monitor
    , configHints        :: [GLFW.WindowHint]
    , configSwapInterval :: Maybe Int
    } deriving
    ( Show
    )

-- | Default window configuration for a small window on any monitor with the given title.
defaultWindowConfig :: String -> WindowConfig
defaultWindowConfig title = WindowConfig 640 480 title Nothing [] Nothing
