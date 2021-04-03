{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards #-}
module LambdaRay.Schwarzschild (frag) where

import           Control.Lens     ((^.))
import           Graphics.GPipe   hiding (lookAt, normalize)
import           LambdaRay.Config
import           Prelude          hiding ((<*))


aspectRatio :: Fractional a => V2 Int -> a
aspectRatio viewPort = fromRational $ toRational y / toRational x
  where V2 x y = viewPort


sqrnorm :: (Metric f, Floating a) => f a -> a
sqrnorm vec = n * n
  where n = norm vec

clip :: (IfB a, OrdB a) => a -> a -> a -> a
clip x lo hi = maxB (minB x hi) lo

xorB :: Boolean b => b -> b -> b
xorB a b = a &&* notB b ||* b &&* notB a


diskOuter, diskOuterSqr, diskInner, diskInnerSqr, diskWidth :: Fractional a => a
diskInner = 2.6
diskOuter = 14
diskWidth = fromRational (14 - 2.6)
diskInnerSqr = fromRational (2.6 * 2.6)
diskOuterSqr = fromIntegral (14 * 14 :: Int)


viewMatrix :: Floating a => V3 a -> V3 (V3 a)
viewMatrix cameraPos = V3 leftVec nupVec frontVec
  where
    lookAt = V3 0 3 0
    upVec = V3 0.2 1 0

    frontVec = signorm (lookAt - cameraPos)
    leftVec = signorm (cross upVec frontVec)
    nupVec = cross frontVec leftVec


data Context a = Context
    { ctxPoint       :: V3 a
    , ctxVelocity    :: V3 a
    , ctxObjectColor :: V4 a
    }

blendcolors :: Num a => V4 a -> V4 a -> V4 a
blendcolors ca cb =
    V4 (color ^. _x) (color ^. _y) (color ^. _z) alpha
  where
    aa = ca ^. _w
    ba = cb ^. _w
    color = ca ^. _xyz + cb ^. _xyz ^* (ba * (1 - aa))
    alpha = aa + ba * (1 - aa)


computeSkyColor :: (IfB a, Real' a, OrdB a) => SkyMode a -> V3 a -> V4 a
computeSkyColor SkyBlack _ = V4 0 0 0 1
computeSkyColor (SkyTexture samp) velocity = skycolor
  where
    vphi = atan2' (velocity ^. _x) (velocity ^. _z)
    vtheta = atan2' (velocity ^. _y) (norm (velocity ^. _xz))

    u = mod'' (vphi+4.5)(2*pi) / (2*pi)
    v = (vtheta+pi/2)/pi

    skycolor3 = samp (V2 u v)
    skycolor = V4 (skycolor3 ^. _x) (skycolor3 ^. _y) (skycolor3 ^. _z) 1


computeDiskColor :: (IfB a, Real' a, OrdB a) => a -> Context a -> V3 a -> BooleanOf a -> DiskMode a -> V4 a
computeDiskColor time ctx newpoint diskMask = compute
  where
    -- actual collision point by intersection
    lambdaa = -(newpoint ^. _y / (ctxVelocity ctx ^. _y))
    colpoint = newpoint + ctxVelocity ctx ^* lambdaa
    colpointsqr = sqrnorm colpoint

    phi = atan2' (colpoint ^. _x) (newpoint ^. _z) + time / 30

    compute DiskSolid = V4 0.5 0.5 0.5 1

    compute (DiskTexture samp) = diskcolor
      where
        u = ((phi + 2*pi) `mod''` 2*pi) / (2*pi)
        v = (sqrt colpointsqr - diskInner) / diskWidth

        diskcolor3 = samp (V2 u v)
        diskalpha = ifB diskMask 1 0 * clip (sqrnorm diskcolor3 / 3.0) 0 1
        diskcolor = V4 (diskcolor3 ^. _x) (diskcolor3 ^. _y) (diskcolor3 ^. _z) diskalpha

    compute DiskGrid = diskcolor
      where
        diskalpha = ifB diskMask 1 0
        diskcolor = ifB (phi `mod''` 0.52359 <* 0.261799)
            (V4 1 1 1 diskalpha)
            (V4 0 0 1 diskalpha)


computeHorizonColor :: (IfB a, Real' a, OrdB a) => HorizonMode -> V3 a -> a -> V3 a -> a -> BooleanOf a -> V4 a
computeHorizonColor HorizonBlack _ _ _ _ _ = V4 0 0 0 1
computeHorizonColor HorizonGrid oldpoint oldpointsqr newpoint newpointsqr horizonMask = horizoncolor
  where
    lambdaa = 1 - (1 - oldpointsqr) / (newpointsqr - oldpointsqr)
    colpoint = lambdaa *^ newpoint + (1 - lambdaa) *^ oldpoint

    phi = atan2' (colpoint ^. _x) (newpoint ^. _z)
    theta = atan2' (colpoint ^. _y) (norm (newpoint ^. _xz))

    horizonalpha = ifB horizonMask 1 0
    horizoncolor = ifB (((phi `mod''` 1.04719) <* 0.52359) `xorB` ((theta `mod''` 1.04719) <* 0.52359))
        (V4 1 0 0 horizonalpha)
        (V4 0 0 0 horizonalpha)


computeVelocity :: Floating a => DistortionMethod -> a -> a -> V3 a -> V3 a -> a -> V3 a
computeVelocity MethodNone _ _ velocity _ _ = velocity
computeVelocity MethodLeapFrog stepsize h2 velocity newpoint newpointsqr = newvelocity
  where
    accel = (-1.5) * h2 *^ newpoint ^/ (newpointsqr ** 2.5)
    newvelocity = velocity + accel ^* stepsize


iteration :: (OrdB a, IfB a, Real' a) => Config a -> a -> a -> Context a -> Context a
iteration Config{..} time h2 ctx = nctx
  where
    oldpoint = ctxPoint ctx -- for intersections
    oldpointsqr = sqrnorm oldpoint

    newpoint = ctxPoint ctx + ctxVelocity ctx ^* stepsize
    newpointsqr = sqrnorm newpoint

    newvelocity = computeVelocity distortionMethod stepsize h2 (ctxVelocity ctx) newpoint newpointsqr

    -- whether it just crossed the horizontal plane
    maskCrossing = (oldpoint ^. _y >* 0) `xorB` (newpoint ^. _y >* 0)
    -- whether it's close enough
    maskDistance = newpointsqr <* diskOuterSqr &&* newpointsqr >* diskInnerSqr

    diskMask = maskCrossing &&* maskDistance
    diskcolor = ifB diskMask (computeDiskColor time ctx newpoint diskMask diskMode) 0

    -- event horizon
    horizonMask = newpointsqr <* 1 &&* oldpointsqr >* 1
    horizoncolor = ifB horizonMask (computeHorizonColor horizonMode oldpoint oldpointsqr newpoint newpointsqr horizonMask) 0

    nctx = Context
      { ctxPoint = newpoint
      , ctxVelocity = newvelocity
      , ctxObjectColor = blendcolors (blendcolors (ctxObjectColor ctx) horizoncolor) diskcolor
      }


frag :: (Real' a, OrdB a, IfB a) => Config a -> V2 Int -> a -> V2 a -> V3 a
frag cfg@Config{iterations} viewPort time (V2 x y) = result
  where
    tanFov = 1.5

    -- cameraPos = V3 0 1 (-20.0 + time `mod''` 20)
    cameraPos = V3 0 2 (-15.0)

    view = signorm $ viewMatrix cameraPos !* V3 (tanFov * (x - 0.5)) (tanFov * aspectRatio viewPort * (y - 0.5)) 1

    h2 = sqrnorm (cross cameraPos view)

    ctx =
      foldr (const $ iteration cfg time h2)
        Context{ ctxPoint = cameraPos
               , ctxVelocity = view
               , ctxObjectColor = V4 0 0 0 0
               } [0..iterations]

    skycolor = computeSkyColor (skyMode cfg) (ctxVelocity ctx)

    result = blendcolors (ctxObjectColor ctx) skycolor ^. _xyz
