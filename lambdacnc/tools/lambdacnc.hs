module Main (main) where

import           Debug.LiveCoding (runSession)
import qualified LambdaCNC.GPU            

main :: IO ()
main = runSession "lambdacnc" LambdaCNC.GPU.main "LambdaCNC.GPU"
    [ "GPipe-Core"
    , "GPipe-Engine"
    , "GPipe-GLFW4"
    , "data-default"
    , "directory"
    , "filepath"
    , "lens"
    , "text"
    , "time"
    ]
