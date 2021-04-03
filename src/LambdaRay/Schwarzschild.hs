{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LambdaRay.Schwarzschild (frag, fragGPU) where

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


type Context a = (a, V3 a, V3 a, V4 a)

-- instance ShaderType (Context FFloat) x where
--     type ShaderBaseType (Context FFloat) = ShaderBaseType FFloat
--     toBase x ~Context{..} = (toBase x ctxI)
--     fromBase x b = undefined


blendcolors :: Num a => V4 a -> V4 a -> V4 a
blendcolors ca cb =
    V4 (color ^. _x) (color ^. _y) (color ^. _z) alpha
  where
    aa = ca ^. _w
    ba = cb ^. _w
    color = ca ^. _xyz + cb ^. _xyz ^* (ba * (1 - aa))
    alpha = aa + ba * (1 - aa)


computeSkyColor :: Real' a => SkyMode a -> V3 a -> V4 a
computeSkyColor SkyBlack _ = V4 0 0 0 1
computeSkyColor (SkyTexture samp) velocity = skycolor
  where
    vphi = atan2' (velocity ^. _x) (velocity ^. _z)
    vtheta = atan2' (velocity ^. _y) (norm (velocity ^. _xz))

    u = mod'' (vphi+4.5)(2*pi) / (2*pi)
    v = (vtheta+pi/2)/pi

    skycolor3 = samp (V2 u v)
    skycolor = V4 (skycolor3 ^. _x) (skycolor3 ^. _y) (skycolor3 ^. _z) 1


computeDiskColor :: FFloat -> Context FFloat -> V3 FFloat -> FBool -> DiskMode FFloat -> V4 FFloat
computeDiskColor time (_ctxI, _ctxPoint, ctxVelocity, _ctxObjectColor) newpoint diskMask = compute
  where
    -- actual collision point by intersection
    lambdaa = -(newpoint ^. _y / (ctxVelocity ^. _y))
    colpoint = newpoint + ctxVelocity ^* lambdaa
    colpointsqr = sqrnorm colpoint

    phi = atan2' (colpoint ^. _x) (newpoint ^. _z) + time / 30

    compute DiskSolid = V4 0.5 0.5 0.5 1

    compute (DiskTexture samp) = diskcolor
      where
        u = ((phi + 2*pi) `mod''` 2*pi) / (2*pi)
        v = (sqrt colpointsqr - diskInner) / diskWidth

        diskcolor3 = samp (V2 u v)
        diskalpha = ifThenElse' diskMask 1 0 * clip (sqrnorm diskcolor3 / 3.0) 0 1
        diskcolor = V4 (diskcolor3 ^. _x) (diskcolor3 ^. _y) (diskcolor3 ^. _z) diskalpha

    compute DiskGrid = diskcolor
      where
        diskalpha = ifThenElse' diskMask 1 0
        diskcolor = ifThenElse' (phi `mod''` 0.52359 <* 0.261799)
            (V4 1 1 1 diskalpha)
            (V4 0 0 1 diskalpha)


computeHorizonColor :: HorizonMode -> V3 FFloat -> FFloat -> V3 FFloat -> FFloat -> FBool -> V4 FFloat
computeHorizonColor HorizonBlack _ _ _ _ _ = V4 0 0 0 1
computeHorizonColor HorizonGrid oldpoint oldpointsqr newpoint newpointsqr horizonMask = horizoncolor
  where
    lambdaa = 1 - (1 - oldpointsqr) / (newpointsqr - oldpointsqr)
    colpoint = lambdaa *^ newpoint + (1 - lambdaa) *^ oldpoint

    phi = atan2' (colpoint ^. _x) (newpoint ^. _z)
    theta = atan2' (colpoint ^. _y) (norm (newpoint ^. _xz))

    horizonalpha = ifThenElse' horizonMask 1 0
    horizoncolor = ifThenElse' (((phi `mod''` 1.04719) <* 0.52359) `xorB` ((theta `mod''` 1.04719) <* 0.52359))
        (V4 1 0 0 horizonalpha)
        (V4 0 0 0 horizonalpha)


computeVelocity :: Floating a => DistortionMethod -> a -> a -> V3 a -> V3 a -> a -> V3 a
computeVelocity MethodNone _ _ velocity _ _ = velocity
computeVelocity MethodLeapFrog stepsize h2 velocity newpoint newpointsqr = newvelocity
  where
    accel = (-1.5) * h2 *^ newpoint ^/ (newpointsqr ** 2.5)
    newvelocity = velocity + accel ^* stepsize


isDone :: (EqB a, Num a) => Context a -> BooleanOf a
isDone (ctxI, _, _, _) = ctxI /=* 0

iteration :: Config FFloat -> FFloat -> FFloat -> (FFloat, V3 FFloat, V3 FFloat, V4 FFloat) -> (FFloat, V3 FFloat, V3 FFloat, V4 FFloat)
iteration Config{..} time (h2 :: FFloat) ctx@(ctxI, ctxPoint, ctxVelocity, ctxObjectColor) = nctx
  where
    oldpoint = ctxPoint -- for intersections
    oldpointsqr = sqrnorm oldpoint

    newpoint = ctxPoint + ctxVelocity ^* stepsize
    newpointsqr = sqrnorm newpoint

    newvelocity = computeVelocity distortionMethod stepsize h2 ctxVelocity newpoint newpointsqr

    -- whether it just crossed the horizontal plane
    maskCrossing = (oldpoint ^. _y >* 0) `xorB` (newpoint ^. _y >* 0)
    -- whether it's close enough
    maskDistance = newpointsqr <* diskOuterSqr &&* newpointsqr >* diskInnerSqr

    diskMask = maskCrossing &&* maskDistance
    diskcolor = ifThenElse' diskMask (computeDiskColor time ctx newpoint diskMask diskMode) 0

    -- event horizon
    horizonMask = newpointsqr <* 1 &&* oldpointsqr >* 1
    horizoncolor = ifThenElse' horizonMask (computeHorizonColor horizonMode oldpoint oldpointsqr newpoint newpointsqr horizonMask) 0

    nctx =
      ( ctxI - 1
      , newpoint
      , newvelocity
      , blendcolors (blendcolors ctxObjectColor horizoncolor) diskcolor
      )


frag :: Config FFloat -> V2 Int -> FFloat -> V2 FFloat -> V3 FFloat
frag cfg@Config{iterations} viewPort time (V2 x y) = result
  where
    tanFov = 1.5

    -- cameraPos = V3 0 1 (-20.0 + time `mod''` 20)
    cameraPos = V3 0 2 (-15.0)

    view = signorm $ viewMatrix cameraPos !* V3 (tanFov * (x - 0.5)) (tanFov * aspectRatio viewPort * (y - 0.5)) 1

    h2 = sqrnorm (cross cameraPos view)

    (_ctxI, _ctxPoint, ctxVelocity, ctxObjectColor) =
      foldr (const $ iteration cfg time h2)
        ( fromIntegral iterations
        , cameraPos
        , view
        , V4 0 0 0 0
        ) [0..iterations]

    skycolor = computeSkyColor (skyMode cfg) ctxVelocity

    result = blendcolors ctxObjectColor skycolor ^. _xyz


fragGPU :: Config FFloat -> V2 Int -> FFloat -> V2 FFloat -> V3 FFloat
fragGPU cfg@Config{iterations} viewPort time (V2 x y) = result
  where
    tanFov = 1.5

    -- cameraPos = V3 0 1 (-20.0 + time `mod''` 21)
    cameraPos = V3 0 2 (-15.0)

    view = signorm $ viewMatrix cameraPos !* V3 (tanFov * (x - 0.5)) (tanFov * aspectRatio viewPort * (y - 0.5)) 1

    h2 = sqrnorm (cross cameraPos view)

    (_ctxI, _ctxPoint, ctxVelocity, ctxObjectColor) =
      while isDone (iteration cfg time h2)
        ( fromIntegral iterations
        , cameraPos
        , view
        , V4 0 0 0 0
        )

    skycolor = computeSkyColor (skyMode cfg) ctxVelocity

    result = blendcolors ctxObjectColor skycolor ^. _xyz
