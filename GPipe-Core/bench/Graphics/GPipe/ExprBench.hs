{-# LANGUAGE OverloadedStrings #-}
module Graphics.GPipe.ExprBench (suite) where

import           Criterion.Main               (Benchmark, bench, bgroup, nfIO)

import           Graphics.GPipe.ExprBenchSimple


suite :: IO Benchmark
suite = return $ bgroup "Expr"
    [ bgroup "raytracer"
        [ bench  "50" $ nfIO (benchRaytracer  50)
        -- , bench "100" $ nfIO (benchRaytracer 100)
        -- , bench "150" $ nfIO (benchRaytracer 150)
        -- , bench "200" $ nfIO (benchRaytracer 200)
        -- , bench "250" $ nfIO (benchRaytracer 250)
        ]
    -- , bgroup "sum [0..n]"
    --     [ bench "1000" $ nfIO (benchSum 1000)
    --     , bench "2000" $ nfIO (benchSum 2000)
    --     , bench "3000" $ nfIO (benchSum 3000)
    --     , bench "4000" $ nfIO (benchSum 4000)
    --     , bench "5000" $ nfIO (benchSum 5000)
    --     ]
    ]
