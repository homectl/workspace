{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.GPipe.BufferBench (suite) where

import           Criterion.Main              (Benchmark, bench, bgroup, whnfIO)

import           Graphics.GPipe.Buffer

import           Graphics.GPipe.Context      (runContextT)
import qualified Graphics.GPipe.Context.GLFW as GLFW


benchWriteBuffer :: Int -> IO Int
benchWriteBuffer l =
    runContextT GLFW.defaultHandleConfig $ do
        buf :: Buffer os (B Float) <- newBuffer l
        writeBuffer buf 0 [0..fromIntegral l]
        return $ bufferLength buf


suite :: IO Benchmark
suite = return $ bgroup "Buffer"
    [ bgroup "writeBuffer"
        [ bench "1000000"  $ whnfIO (benchWriteBuffer 1000000)
        , bench "10000000" $ whnfIO (benchWriteBuffer 10000000)
        ]
    ]
