{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LambdaCnc.Shaders
  ( fragShadow, vertShadow
  , fragCamera, vertCamera
  ) where

import           Control.Lens     ((^.))
import           Graphics.GPipe   hiding ( normalize)
import           LambdaCnc.Config (RuntimeConfig (..))
import           Prelude          hiding ((<*))


toV4 :: R3 t => a -> t a -> V4 a
toV4 w v = V4 (v^._x) (v^._y) (v^._z) w

modelMat = rotMatrixZ (pi/8 * 5) -- time

--------------------------------------------------

vertShadow :: RuntimeConfig VFloat -> (V3 VFloat, V3 VFloat) -> (VPos, VFloat)
vertShadow RuntimeConfig{..} (toV4 1 -> pos, col) = (screenPos, screenPos^._z * 0.5 + 0.5)
  where
    viewMat = lookAt (V3 100000 50000 50000) (V3 0 0 0) (V3 0 0 1)
    projMat = ortho (-50000) 50000 (-50000) 50000 1000 350000
    screenPos = projMat !*! viewMat !*! modelMat !* pos


fragShadow :: RuntimeConfig FFloat -> FFloat -> (FFloat, FragDepth)
fragShadow RuntimeConfig{..} c = (0.5, c)

--------------------------------------------------

vertCamera :: RuntimeConfig VFloat -> (V3 VFloat, V3 VFloat) -> (VPos, (V2 VFloat, V2 VFloat))
vertCamera RuntimeConfig{..} (toV4 1 -> pos, col) = (screenPos, (uv, col^._xy))
  where
    viewMat = lookAt (V3 40000 50000 60000) (V3 0 0 0) (V3 0 0 1)
    projMat = perspective (pi/3) 1 1000 350000
    screenPos = projMat !*! viewMat !*! modelMat !* pos
    uv = pos^._xy


fragCamera :: RuntimeConfig FFloat -> (V2 FFloat -> FFloat) -> (V2 FFloat -> FFloat) -> (V2 FFloat, V2 FFloat) -> V3 FFloat
fragCamera RuntimeConfig{..} shadowSamp texSamp (V2 u v, _) = V3 c c c
  where c = texSamp (V2 u v / 80000)
