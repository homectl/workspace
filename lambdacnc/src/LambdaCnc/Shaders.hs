{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LambdaCnc.Shaders (frag, vert) where

import           Control.Lens     ((^.))
import           Graphics.GPipe   hiding ( normalize)
import           LambdaCnc.Config (RuntimeConfig (..))
import           Prelude          hiding ((<*))


vert :: RuntimeConfig VFloat -> (VPos, V2 VFloat) -> (VPos, (V2 VFloat, V2 VFloat))
vert RuntimeConfig{..} (pos, col) = (screenPos, (col, col))
  where
    modelMat = rotMatrixX time
    viewMat = lookAt (V3 1 1 2) (V3 0 0 0) (V3 0 0 1)
    projMat = perspective (pi/3) 1 1 10
    screenPos = projMat !*! viewMat !*! modelMat !* pos


frag :: RuntimeConfig FFloat -> (V2 FFloat, V2 FFloat) -> V3 FFloat
frag RuntimeConfig{..} (V2 x y, _) = result
  where
    c = time `mod''` 1
    result = V3 (c + x) (-c + y) c
