{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.GPipe.ExprBenchSimple where

import           Data.Text.Lazy               (Text)
import           Graphics.GPipe.Internal.Expr
import           Graphics.GPipe.Linear        (V2 (..), norm)
import qualified LambdaRay.Config             as Config
import qualified LambdaRay.Schwarzschild      as Schwarzschild


benchRaytracer :: Int -> IO Text
benchRaytracer iterations =
    let
        config = Config.defaultConfig{Config.iterations}
        rt = Config.defaultRuntimeConfig
        expr = norm $ Schwarzschild.frag config (V2 100 100) rt (V2 30 30)
    in
    finalSource <$> runExprM (tellGlobal "") (unS expr >>= tellAssignment' "result")


benchSum :: Int -> IO Text
benchSum n =
    let expr :: FFloat
        expr = sum $ map fromIntegral [0..n]
    in
    finalSource <$> runExprM (tellGlobal "") (unS expr >>= tellAssignment' "result")
