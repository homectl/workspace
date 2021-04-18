{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Language.GLSL.Runtime.Math where

import           Prelude (Float, fromIntegral, max, min, (*), (-), (.), (/),
                          (<))
import qualified Prelude

floor :: Float -> Float
floor = fromIntegral . Prelude.floor

fract :: Float -> Float
fract x = x - floor x

mod :: Float -> Float -> Float
mod x y = x - y * floor (x/y)

clamp :: Float -> Float -> Float -> Float
clamp x a = min (max x a)

saturate :: Float -> Float
saturate x = clamp x 0 1

step :: Float -> Float -> Float
step a x = if x < a then 0 else 1

smoothstep :: Float -> Float -> Float -> Float
smoothstep a b x = let t = saturate ((x-a) / (b-a)) in t*t*(3-2*t)
