{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
clip lo hi x = maxB (minB x hi) lo

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
    lookAt = V3 0 0 0
    upVec = V3 0.2 1 0

    frontVec = signorm (lookAt - cameraPos)
    leftVec = signorm (cross upVec frontVec)
    nupVec = cross frontVec leftVec


data Context a = Context
    { ctxPoint       :: V3 a
    , ctxVelocity    :: V3 a
    , ctxStepsize    :: a
    , ctxObjectColor :: V3 a
    , ctxObjectAlpha :: a
    }

-- instance ShaderType (Context FFloat) x where
--     type ShaderBaseType (Context FFloat) = ShaderBaseType FFloat
--     toBase x ~Context{..} = (toBase x ctxI)
--     fromBase x b = undefined


blendcolors :: Num a => V3 a -> a -> V3 a -> a -> V3 a
blendcolors ca aa cb ba = ca + cb ^* (ba * (1 - aa))

blendalpha :: Num a => a -> a -> a
blendalpha aa ba = aa + ba * (1 - aa)


computeSkyColor :: Real' a => SkyMode a -> V3 a -> V3 a
computeSkyColor SkyBlack _ = V3 0 0 0
computeSkyColor (SkyTexture samp) velocity = skycolor
  where
    vphi = atan2' (velocity ^. _x) (velocity ^. _z)
    vtheta = atan2' (velocity ^. _y) (norm (velocity ^. _xz))

    u = mod'' (vphi+4.5)(2*pi) / (2*pi)
    v = (vtheta+pi/2)/pi

    skycolor = samp (V2 u v)


computeDiskColor :: FFloat -> V3 FFloat -> V3 FFloat -> DiskMode FFloat -> (V3 FFloat, FFloat)
computeDiskColor time velocity newpoint = compute
  where
    -- actual collision point by intersection
    lambdaa = -(newpoint ^. _y / (velocity ^. _y))
    colpoint = newpoint + lambdaa *^ velocity
    colpointsqr = sqrnorm colpoint

    phi = atan2' (colpoint ^. _x) (newpoint ^. _z) + time / 30

    compute DiskSolid = (V3 0.5 0.5 0.5, 1)

    compute (DiskTexture samp) = (diskcolor, diskalpha)
      where
        u = ((phi + 2*pi) `mod''` 2*pi) / (2*pi)
        v = (sqrt colpointsqr - diskInner) / diskWidth

        diskcolor = samp (V2 u v)
        diskalpha = clip 0 1 (sqrnorm diskcolor / 3.0)

    compute DiskGrid = (diskcolor, 1)
      where
        diskcolor = ifThenElse' (phi `mod''` 0.52359 <* 0.261799)
            (V3 1 1 1)
            (V3 0 0 1)


computeHorizonColor :: HorizonMode -> V3 FFloat -> FFloat -> V3 FFloat -> FFloat -> (V3 FFloat, FFloat)
computeHorizonColor HorizonBlack _ _ _ _ = (V3 0 0 0, 1)
computeHorizonColor HorizonGrid oldpoint oldpointsqr newpoint newpointsqr = (horizoncolor, 1)
  where
    lambdaa = 1 - (1 - oldpointsqr) / (newpointsqr - oldpointsqr)
    colpoint = lambdaa *^ newpoint + (1 - lambdaa) *^ oldpoint

    phi = atan2' (colpoint ^. _x) (newpoint ^. _z)
    theta = atan2' (colpoint ^. _y) (norm (newpoint ^. _xz))

    horizoncolor = ifThenElse' (((phi `mod''` 1.04719) <* 0.52359) `xorB` ((theta `mod''` 1.04719) <* 0.52359))
        (V3 1 0 0)
        (V3 0 0 0)


computeVelocity :: Floating a => DistortionMethod -> a -> a -> a -> V3 a -> V3 a -> a -> (V3 a, a)
computeVelocity MethodNone stepsize _ _ velocity _ _ = (velocity, stepsize)
computeVelocity MethodLeapFrog stepsize stepfactor h2 velocity newpoint newpointsqr = (newvelocity, newstepsize)
  where
    accel = (-1.5) * h2 *^ newpoint ^/ (newpointsqr ** 2.5)
    newvelocity = velocity + accel ^* stepsize
    -- step = abs $ norm velocity - norm newvelocity
    newstepsize = stepsize * stepfactor


iteration :: Config FFloat -> RuntimeConfig FFloat -> FFloat -> Context FFloat -> Context FFloat
iteration Config{distortionMethod,diskMode,horizonMode} RuntimeConfig{..} (h2 :: FFloat) ctx = nctx
  where
    oldpoint = ctxPoint ctx -- for intersections
    oldpointsqr = sqrnorm oldpoint

    newpoint = ctxPoint ctx + ctxVelocity ctx ^* ctxStepsize ctx
    newpointsqr = sqrnorm newpoint

    (newvelocity, newstepsize) =
      computeVelocity distortionMethod (ctxStepsize ctx) stepfactor h2 (ctxVelocity ctx) newpoint newpointsqr

    -- whether it just crossed the horizontal plane
    maskCrossing = (oldpoint ^. _y >* 0) `xorB` (newpoint ^. _y >* 0)
    -- whether it's close enough
    maskDistance = newpointsqr <* diskOuterSqr &&* newpointsqr >* diskInnerSqr

    diskMask = maskCrossing &&* maskDistance
    (diskcolor, diskalpha) = ifThenElse' diskMask (computeDiskColor time (ctxVelocity ctx) newpoint diskMode) (0, 0)

    -- event horizon
    horizonMask = newpointsqr <* 1 &&* oldpointsqr >* 1
    (horizoncolor, horizonalpha) = ifThenElse' horizonMask (computeHorizonColor horizonMode oldpoint oldpointsqr newpoint newpointsqr) (0, 0)

    c1 = blendcolors (ctxObjectColor ctx) (ctxObjectAlpha ctx) horizoncolor horizonalpha
    a1 = blendalpha (ctxObjectAlpha ctx) horizonalpha
    c2 = blendcolors c1 a1 diskcolor diskalpha
    a2 = blendalpha a1 diskalpha

    nctx = Context
      { ctxPoint = newpoint
      , ctxVelocity = newvelocity
      , ctxStepsize = newstepsize
      , ctxObjectColor = c2
      , ctxObjectAlpha = a2
      }


frag :: Config FFloat -> V2 Int -> RuntimeConfig FFloat -> V2 FFloat -> V3 FFloat
frag cfg@Config{iterations} viewPort rt@RuntimeConfig{..} (V2 x y) = result
  where
    tanFov = 1.5

    pos = cameraPos + V3 0 0 (time / 3 `mod''` 21)
    -- pos = V3 0 2 (-15.0)

    view = signorm $ viewMatrix pos !* V3 (tanFov * (x - 0.5)) (tanFov * aspectRatio viewPort * (y - 0.5)) 1

    h2 = sqrnorm (cross pos view)

    Context{ctxVelocity, ctxObjectColor, ctxObjectAlpha} =
      foldr (const $ iteration cfg rt h2)
        Context { ctxPoint = pos
                , ctxVelocity = view
                , ctxStepsize = stepsize
                , ctxObjectColor = V3 0 0 0
                , ctxObjectAlpha = 0
                } [0..iterations]

    skycolor = computeSkyColor (skyMode cfg) ctxVelocity

    result = blendcolors ctxObjectColor ctxObjectAlpha skycolor 1
