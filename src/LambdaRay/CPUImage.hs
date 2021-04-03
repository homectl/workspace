module LambdaRay.CPUImage where

import           Control.Lens            ((^.))
import           Graphics.GPipe
import           LambdaRay.Config        (defaultConfig)
import qualified LambdaRay.Schwarzschild as Schwarzschild

size :: Num a => V2 a
size = V2 200 100

image :: ([V3 Float], V2 Int)
image = (colors, size)
  where
    pixels = [V2 (x / size ^. _x) (y / size ^. _y) | y <- [0..size ^. _y], x <- [0..size ^. _x]]
    colors = map (Schwarzschild.frag defaultConfig size (0 :: Float)) pixels
