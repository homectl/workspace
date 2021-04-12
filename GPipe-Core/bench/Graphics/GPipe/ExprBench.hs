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
        [ bench "1000" $ whnfIO (benchCompileExpr 1000)
        , bench "2000" $ whnfIO (benchCompileExpr 2000)
        , bench "3000" $ whnfIO (benchCompileExpr 3000)
        , bench "4000" $ whnfIO (benchCompileExpr 4000)
        , bench "5000" $ whnfIO (benchCompileExpr 5000)
        ]
    ]
