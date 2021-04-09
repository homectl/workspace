{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}
module Graphics.SceneGraph.Matrix
  ( translateM
  , translatePostM
  , scaleM
  , rotateM
  , rotatePostM
  ) where

-- Warning: Because this matrix is going to get passed directly to GL we convert to GL space
-- here.

import           Control.Lens ((^.))
import           Linear       (M44, R1 (..), R2 (..), R3 (..), V3 (..), V4 (..),
                               (!*!))


asMatrix :: V3 Float -> M44 Float
asMatrix (V3 x y z) = V4
  (V4 1 0 0 x)
  (V4 0 1 0 y)
  (V4 0 0 1 z)
  (V4 0 0 0 1)

translateM :: V3 Float -> M44 Float -> M44 Float
translateM v m = asMatrix v !*! m

translatePostM :: V3 Float -> M44 Float -> M44 Float
translatePostM v m = m !*! asMatrix v

scaleM :: V3 Float -> M44 Float -> M44 Float
scaleM v m = m !*! V4
  (V4 (v^._x) 0 0 0)
  (V4 0 (v^._y) 0 0)
  (V4 0 0 (v^._z) 0)
  (V4 0 0 0 1)

-- | Build rotational transform matrix for rotate of ''theta'' around a vector.
rotateM' :: Float -> V3 Float -> M44 Float
rotateM' theta (V3 x y z) = V4
  (V4 (t*x*x + c) (t*x*y-s*z) (t*x*z + s*y) 0)
  (V4 (t*x*y+s*z) (t*y*y + c) (t*y*z - s*x)  0)
  (V4 (t*x*z-s*y) (t*y*z + s*x) (t*z*z+c) 0)
  (V4 0 0 0 1)
  where
    t = 1 - cos theta
    c = cos theta
    s = sin theta

rotateM :: Float -> V3 Float -> M44 Float -> M44 Float
rotateM theta v m = rotateM' theta v !*! m

rotatePostM :: Float -> V3 Float -> M44 Float -> M44 Float
rotatePostM theta v m = m !*! rotateM' theta v
