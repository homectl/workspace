{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module LambdaRay.Schwarzschild (frag) where

import           Control.Lens                             ((^.))
import           Graphics.GPipe                           hiding (lookAt,
                                                           normalize)
import           Prelude                                  hiding ((<*))


aspectRatio :: Fractional a => V2 Int -> a
aspectRatio viewPort = fromRational $ toRational y / toRational x
  where V2 x y = viewPort


diskOuterSqr, diskInnerSqr :: Fractional a => a
diskInnerSqr = fromRational (2.6 * 2.6)
diskOuterSqr = fromIntegral (14 * 14 :: Int)


iterations :: Int
iterations = 50
stepsize :: Fractional a => a
stepsize = 0.7


viewMatrix :: Floating a => V3 a -> V3 (V3 a)
viewMatrix cameraPos = V3 leftVec nupVec frontVec
  where
    lookAt = V3 0 0 0
    upVec = V3 0.2 1 0

    frontVec = signorm (lookAt - cameraPos)
    leftVec = signorm (cross upVec frontVec)
    nupVec = cross frontVec leftVec


sqrnorm :: (Metric f, Floating a) => f a -> a
sqrnorm vec = n * n
  where n = norm vec


data Context a = Context
    { ctxPoint       :: V3 a
    , ctxVelocity    :: V3 a
    , ctxObjectColor :: V4 a
    }

xorB :: Boolean b => b -> b -> b
xorB a b = a &&* notB b ||* b &&* notB a

blendcolors :: Num a => V4 a -> V4 a -> V4 a
blendcolors (V4 ra ga ba aa) (V4 rb gb bb ab) =
    V4 (ra + rb * a) (ga + gb * a) (ba + bb * a) a
  where
    a = ab * (1 - aa)


computeDiskColor :: (IfB a, Real' a, OrdB a) => Context a -> V3 a -> BooleanOf a -> V4 a
computeDiskColor ctx newpoint diskMask = diskcolor
  where
    -- actual collision point by intersection
    lambdaa = -(newpoint ^. _y / (ctxVelocity ctx ^. _y))
    colpoint = newpoint + ctxVelocity ctx ^* lambdaa

    phi = atan2' (colpoint ^. _x) (newpoint ^. _z)
    theta = atan2' (colpoint ^. _y) (norm (newpoint ^. _xz))
    diskalpha = ifB diskMask 1 0
    diskcolor = ifB (phi `mod''` 0.52359 <* 0.261799)
        (V4 1 1 1 diskalpha)
        (V4 0 0 1 diskalpha)


computeHorizonColor :: (IfB a, Real' a, OrdB a) => V3 a -> a -> V3 a -> a -> BooleanOf a -> V4 a
computeHorizonColor oldpoint oldpointsqr newpoint newpointsqr horizonMask = horizoncolor
  where
    lambdaa = 1 - (1 - oldpointsqr) / (newpointsqr - oldpointsqr)
    colpoint = lambdaa *^ newpoint + (1 - lambdaa) *^ oldpoint
    phi = atan2' (colpoint ^. _x) (newpoint ^. _z)
    theta = atan2' (colpoint ^. _y) (norm (newpoint ^. _xz))
    horizonalpha = ifB horizonMask 1 0
    horizoncolor = ifB (((phi `mod''` 1.04719) <* 0.52359) `xorB` ((theta `mod''` 1.04719) <* 0.52359))
        (V4 1 0 0 horizonalpha)
        (V4 0 0 0 horizonalpha)


iteration :: (OrdB a, IfB a, Real' a) => a -> Context a -> Context a
iteration h2 ctx = nctx
  where
    oldpoint = ctxPoint ctx -- for intersections
    oldpointsqr = sqrnorm oldpoint

    newpoint = ctxPoint ctx + ctxVelocity ctx ^* stepsize
    newpointsqr = sqrnorm newpoint

    accel = (-1.5) * h2 *^ newpoint ^/ (sqrnorm newpoint ** 2.5)
    newvelocity = ctxVelocity ctx + accel ^* stepsize

    -- whether it just crossed the horizontal plane
    maskCrossing = (oldpoint ^. _y >* 0) `xorB` (newpoint ^. _y >* 0)
    -- whether it's close enough
    maskDistance = newpointsqr <* diskOuterSqr &&* newpointsqr >* diskInnerSqr

    diskMask = maskCrossing &&* maskDistance
    diskcolor = ifB diskMask (computeDiskColor ctx newpoint diskMask) 0

    -- event horizon
    horizonMask = newpointsqr <* 1 &&* oldpointsqr >* 1
    horizoncolor = ifB horizonMask (computeHorizonColor oldpoint oldpointsqr newpoint newpointsqr horizonMask) 0

    nctx = Context
      { ctxPoint = newpoint
      , ctxVelocity = newvelocity
      , ctxObjectColor = blendcolors (blendcolors (ctxObjectColor ctx) horizoncolor) diskcolor
      }


frag :: (Real' a, OrdB a, IfB a) => V2 Int -> a -> V2 a -> V3 a
frag viewPort time (V2 x y) = result
  where
    tanFov = 1.5

    cameraPos = V3 0 1 (-20.0 + time `mod''` 20)

    view = signorm $ viewMatrix cameraPos !* V3 (tanFov * (x - 0.5)) (tanFov * aspectRatio viewPort * (y - 0.5)) 1

    h2 = sqrnorm (cross cameraPos view)

    Context{ctxObjectColor=V4 r g b a} =
      foldr (const $ iteration h2)
        Context{ ctxPoint = cameraPos
               , ctxVelocity = view
               , ctxObjectColor = V4 0 0 0 0
               } [0..iterations]

    result = V3 r g b
