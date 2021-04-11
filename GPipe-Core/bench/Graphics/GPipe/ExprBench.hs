{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.GPipe.ExprBench (suite) where

import           Criterion.Main              (Benchmark, bench, bgroup, whnfIO)

import           Graphics.GPipe.Internal.Expr


benchCompileExpr :: Int -> IO String
benchCompileExpr n =
    let expr :: FFloat
        expr = sum $ map fromIntegral [0..n] in
    finalSource <$> runExprM (tellGlobal "") (unS expr >>= tellAssignment' "result")


suite :: IO Benchmark
suite = return $ bgroup "Expr"
    [ bgroup "sum [0..n]"
        [ bench "10000" $ whnfIO (benchCompileExpr 10000)
        , bench "20000" $ whnfIO (benchCompileExpr 20000)
        , bench "30000" $ whnfIO (benchCompileExpr 30000)
        , bench "40000" $ whnfIO (benchCompileExpr 40000)
        , bench "50000" $ whnfIO (benchCompileExpr 50000)
        ]
    ]
