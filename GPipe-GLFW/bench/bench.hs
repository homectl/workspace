module Main (main) where

import           Criterion.Main              (defaultMain)

import qualified Graphics.GPipe.BufferBench  as BufferBench
import qualified Graphics.GPipe.TextureBench as TextureBench

main :: IO ()
main = defaultMain =<< sequence
    [ BufferBench.suite
    , TextureBench.suite
    ]
