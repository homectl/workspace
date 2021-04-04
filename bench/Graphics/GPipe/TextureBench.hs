{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.GPipe.TextureBench (suite) where

import           Criterion.Main              (Benchmark, bench, bgroup, whnfIO)

import           Graphics.GPipe.Texture

import qualified Codec.Picture               as Pic
import           Graphics.GPipe              (Format (SRGB8), V2 (..), V3 (..))
import           Graphics.GPipe.Context      (runContextT)
import qualified Graphics.GPipe.Context.GLFW as GLFW


benchWriteTexture2D :: V2 Int -> IO Int
benchWriteTexture2D size@(V2 w h) =
    let pixels = [ V3 (fromIntegral x) (fromIntegral y) 0 :: V3 Pic.Pixel8
                 | y <- [0..h], x <- [0..w]] in
    runContextT GLFW.defaultHandleConfig $ do
        tex <- newTexture2D SRGB8 size maxBound -- JPG converts to SRGB
        writeTexture2D tex 0 0 size pixels
        return $ texture2DLevels tex


suite :: IO Benchmark
suite = return $ bgroup "Texture"
    [ bgroup "writeTexture2D"
        [ bench "100x100"   $ whnfIO (benchWriteTexture2D (V2 100 100))
        , bench "1000x1000" $ whnfIO (benchWriteTexture2D (V2 1000 1000))
        ]
    ]
