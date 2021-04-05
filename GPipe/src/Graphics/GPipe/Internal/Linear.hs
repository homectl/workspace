module Graphics.GPipe.Internal.Linear where

import           Control.Lens ((^.))
import           Linear


lookAt :: Floating a => V3 a -> V3 a -> V3 a -> V4 (V4 a)
lookAt eye center up =
  V4 (V4 (xa^._x)  (xa^._y)  (xa^._z)  xd)
     (V4 (ya^._x)  (ya^._y)  (ya^._z)  yd)
     (V4 (-za^._x) (-za^._y) (-za^._z) zd)
     (V4 0         0         0          1)
  where za = signorm $ center - eye
        xa = signorm $ cross za up
        ya = cross xa za
        xd = -dot xa eye
        yd = -dot ya eye
        zd = dot za eye


rotMatrixX phi = V4
    (V4 1          0          0          0)
    (V4 0          ( cos phi) (-sin phi) 0)
    (V4 0          ( sin phi) ( cos phi) 0)
    (V4 0          0          0          1)


rotMatrixY phi = V4
    (V4 ( cos phi) 0          ( sin phi) 0)
    (V4 0          1          0          0)
    (V4 (-sin phi) 0          ( cos phi) 0)
    (V4 0          0          0          1)


rotMatrixZ phi = V4
    (V4 ( cos phi) (-sin phi) 0          0)
    (V4 ( sin phi) ( cos phi) 0          0)
    (V4 0          0          1          0)
    (V4 0          0          0          1)
