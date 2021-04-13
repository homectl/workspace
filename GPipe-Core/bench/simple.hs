module Main (main) where

import           Control.Monad                  (replicateM_)
import qualified Data.Text.Lazy                 as LT
import           Graphics.GPipe.ExprBenchSimple (benchRaytracer, benchSum)

data Benchmark
    = RayTracer
    | Sum

iterations :: Int
iterations = 10

benchmark :: Benchmark
benchmark = RayTracer
-- benchmark = Sum

main :: IO ()
main = do
    let rt = benchRaytracer 1000
        sm = benchSum 50000
    case benchmark of
        RayTracer -> replicateM_ iterations $ print . LT.length =<< rt
        Sum       -> replicateM_ iterations $ print . LT.length =<< sm
