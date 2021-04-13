module Main (main) where

import qualified Data.Text.Lazy                 as LT
import           Graphics.GPipe.ExprBenchSimple (benchRaytracer, benchSum)

data Benchmark
    = RayTracer
    | Sum

benchmark :: Benchmark
-- benchmark = RayTracer
benchmark = Sum

main :: IO ()
main =
    case benchmark of
        RayTracer -> print . LT.length =<< benchRaytracer 5000
        Sum       -> print . LT.length =<< benchSum 50000
