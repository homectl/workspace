module Graphics.GPipe.Internal.Optimizer where

import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

optimizeShader :: Text -> Text
optimizeShader = id
