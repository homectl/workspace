module Main (main) where

import           Criterion.Main              (defaultMain)

import qualified Graphics.GPipe.ExprBench  as ExprBench

main :: IO ()
main = defaultMain =<< sequence
    [ ExprBench.suite
    ]
