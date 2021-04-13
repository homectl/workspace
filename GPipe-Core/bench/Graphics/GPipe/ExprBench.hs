{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.GPipe.ExprBench (suite) where

import           Criterion.Main               (Benchmark, bench, bgroup, nfIO,
                                               whnfIO)

import           Graphics.GPipe.Internal.Expr
import           Graphics.GPipe.Linear        (V2 (..), norm)
import qualified LambdaRay.Config             as Config
import qualified LambdaRay.Schwarzschild      as Schwarzschild


benchRaytracer :: Int -> IO String
benchRaytracer iterations = do
    let
        config = Config.defaultConfig{Config.iterations}
        rt = Config.defaultRuntimeConfig
        expr = norm $ Schwarzschild.frag config (V2 100 100) rt (V2 30 30)
    finalSource <$> runExprM (tellGlobal "") (unS expr >>= tellAssignment' "result")


benchSum :: Int -> IO String
benchSum n = do
    let expr :: FFloat
        expr = sum $ map fromIntegral [0..n]
    finalSource <$> runExprM (tellGlobal "") (unS expr >>= tellAssignment' "result")


suite :: IO Benchmark
suite = return $ bgroup "Expr"
    [ bgroup "raytracer"
        [ bench  "50" $ whnfIO (benchRaytracer  50)
        -- , bench "100" $ whnfIO (benchRaytracer 100)
        -- , bench "150" $ whnfIO (benchRaytracer 150)
        -- , bench "200" $ whnfIO (benchRaytracer 200)
        -- , bench "250" $ whnfIO (benchRaytracer 250)
        ]
    -- , bgroup "sum [0..n]"
    --     [ bench "1000" $ nfIO (benchSum 1000)
    --     , bench "2000" $ nfIO (benchSum 2000)
    --     , bench "3000" $ nfIO (benchSum 3000)
    --     , bench "4000" $ nfIO (benchSum 4000)
    --     , bench "5000" $ nfIO (benchSum 5000)
    --     ]
    ]
