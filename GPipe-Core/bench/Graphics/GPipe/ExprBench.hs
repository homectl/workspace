{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.GPipe.ExprBench (suite) where

import           Criterion.Main               (Benchmark, bench, bgroup, whnfIO)

import           Graphics.GPipe.Internal.Expr
import           Graphics.GPipe.Linear        (V2 (..), norm)
import qualified LambdaRay.Config             as Config
import qualified LambdaRay.Schwarzschild      as Schwarzschild
import           System.Mem                   (performGC)


benchRaytracer :: Int -> IO String
benchRaytracer iterations = do
    -- performGC
    let
        config = Config.defaultConfig{Config.iterations}
        rt = Config.defaultRuntimeConfig
        expr = norm $ Schwarzschild.frag config (V2 100 100) rt (V2 30 30)
    finalSource <$> runExprM (tellGlobal "") (unS expr >>= tellAssignment' "result")


benchSum :: Int -> IO String
benchSum n = do
    performGC
    let expr :: FFloat
        expr = sum $ map fromIntegral [0..n]
    finalSource <$> runExprM (tellGlobal "") (unS expr >>= tellAssignment' "result")


suite :: IO Benchmark
suite = return $ bgroup "Expr"
    [ bgroup "raytracer"
        [ {-bench  "5" $ whnfIO (benchRaytracer  5)
        , bench "10" $ whnfIO (benchRaytracer 10)
        , bench "15" $ whnfIO (benchRaytracer 15)
        , bench "20" $ whnfIO (benchRaytracer 20)
        , -}bench "250" $ whnfIO (benchRaytracer 250)
        ]
    -- , bgroup "sum [0..n]"
    --     [ bench "1000" $ whnfIO (benchSum 1000)
    --     , bench "2000" $ whnfIO (benchSum 2000)
    --     , bench "3000" $ whnfIO (benchSum 3000)
    --     , bench "4000" $ whnfIO (benchSum 4000)
    --     , bench "5000" $ whnfIO (benchSum 5000)
    --     ]
    ]
