{-# LANGUAGE LambdaCase #-}
module Main where

import qualified LambdaRay.CPU      as CPU
import qualified LambdaRay.Dynamic  as Dynamic
import qualified LambdaRay.GPU      as GPU
import           System.Environment as Env (getArgs)

main :: IO ()
main = do
  Env.getArgs >>= \case
    args | all (`elem` args) ["-static", "-cpu"] -> CPU.main
    args | all (`elem` args) ["-static"]         -> GPU.main
    args | all (`elem` args) ["-cpu"]            -> Dynamic.run "CPU"
    _                                            -> Dynamic.run "GPU"
