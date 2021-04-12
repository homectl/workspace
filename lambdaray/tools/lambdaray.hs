module Main (main) where

import           Debug.LiveCoding (runSession)
import qualified LambdaRay.GPU

main :: IO ()
main = runSession "lambdaray" LambdaRay.GPU.main "LambdaRay.GPU"
    [ "GPipe-Core"
    , "GPipe-GLFW4"
    , "JuicyPixels"
    , "lens"
    , "time"
    ]
