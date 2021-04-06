{-# LANGUAGE TypeFamilies #-}
module LambdaCNC.Shaders.Common where

import           Control.Lens   ((^.))
import           Graphics.GPipe

--------------------------------------------------

toV4 :: R3 t => a -> t a -> V4 a
toV4 w v = V4 (v^._x) (v^._y) (v^._z) w

aspectRatio :: Fractional a => V2 a -> a
aspectRatio (V2 w h) = w / h

modelMat :: VFloat -> V4 (V4 VFloat)
modelMat time = rotMatrixZ (pi/2)
-- modelMat time = rotMatrixZ (time / 3)

nearPlane, farPlane :: Num a => a
nearPlane = 3000
farPlane = 350000

shadowMapSize :: Num a => V2 a
shadowMapSize = V2 1200 1200

lightMat :: Floating a => V4 a -> M44 a
lightMat lightPos = projMat !*! viewMat
  where
    viewMat = lookAt (lightPos^._xyz) (V3 0 0 0) (V3 0 0 1)
    projMat = ortho (-r) r (-t) t nearPlane farPlane
    t = 50000
    r = 80000

cameraMat :: Floating a => V2 a -> V3 a -> M44 a
cameraMat screenSize cameraPos = projMat !*! viewMat
  where
    viewMat = lookAt cameraPos (V3 0 0 10000) (V3 0 0 1)
    projMat = perspective (30 * pi / 180) (aspectRatio screenSize) nearPlane farPlane

-- getLightPos :: Floating a => a -> V4 a
-- getLightPos time = rotMatrixZ (time/2) !* V4 120000 0 30000 1
-- getLightPos _ = rotMatrixZ (pi/10*(-7)) !* V4 100000 1000 30000 1

--------------------------------------------------

type Shader3DInput = (B3 Float, B3 Float)
type Buffer3D os = Buffer os Shader3DInput
type Shader2DInput = (B2 Float)
type Buffer2D os = Buffer os Shader2DInput

type ShadowColorTex os = Texture2D os (Format RFloat)
type ShadowDepthTex os = Texture2D os (Format Depth)

type MonochromeTex os = Texture2D os (Format RFloat)
type ColorTex os = Texture2D os (Format RGBFloat)

--------------------------------------------------

data ShadowMap os = ShadowMap
    { shadowColorTex :: ShadowColorTex os
    , shadowDepthTex :: ShadowDepthTex os
    }

data FragLight = FragLight
    { fragLightPos     :: V3 FFloat
    , fragLightSampler :: V2 FFloat -> FFloat
    }
