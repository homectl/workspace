module Graphics.GPipe.Fonts.Common where

import           Graphics.GPipe (V2 (..))

type OutlineCurves = [[V2 Float]]

data Mesh = Mesh
    { meshPosition :: [V2 Float]
    , meshUV       :: [V2 Float]
    , meshDistance :: [Float]
    }


turn :: V2 Float -> V2 Float
turn (V2 x y) = V2 y (-x)
