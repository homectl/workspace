{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module LambdaCNC.Shaders
  ( ShadowShader, ShadowShaderEnv (..), compileShadowShader
  , SolidsShader, SolidsShaderEnv, compileSolidsShader
  , compileWireframeShader
  , QuadShader, QuadShaderEnv, compileQuadShader

  , ObjectShaderInput
  , QuadShaderInput

  , ShadowColorTex
  , ShadowDepthTex
  , MonochromeTex
  , ColorTex
  ) where

import           Control.Lens     ((^.))
import           Graphics.GPipe   hiding (normalize)
import           LambdaCNC.Config (RuntimeConfig (..), UniformBuffer)
import           Prelude          hiding ((<*))


toV4 :: R3 t => a -> t a -> V4 a
toV4 w v = V4 (v^._x) (v^._y) (v^._z) w

modelMat :: V4 (V4 VFloat)
modelMat = rotMatrixZ (pi/8 * 5) -- time

--------------------------------------------------

type ObjectShaderInput = (B3 Float, B3 Float)
type QuadShaderInput = (B2 Float)

type ShadowColorTex os = Texture2D os (Format RFloat)
type ShadowDepthTex os = Texture2D os (Format Depth)
type MonochromeTex os = Texture2D os (Format RFloat)
type ColorTex os = Texture2D os (Format RGBFloat)

--------------------------------------------------

type ShadowShader os = CompiledShader os ShadowShaderEnv
data ShadowShaderEnv = ShadowShaderEnv
    { envPrimitives  :: PrimitiveArray Triangles ObjectShaderInput
    , envShadowColor :: Image (Format RFloat)
    , envShadowDepth :: Image (Format Depth)
    }

vertLight :: RuntimeConfig VFloat -> (V3 VFloat, V3 VFloat) -> (VPos, VFloat)
vertLight RuntimeConfig{} (toV4 1 -> pos, normal) =
    (screenPos, screenPos^._z * 0.5 + 0.5)
  where
    viewMat = lookAt (V3 100000 50000 50000) (V3 0 0 0) (V3 0 0 1)
    projMat = ortho (-50000) 50000 (-50000) 50000 1000 350000
    screenPos = projMat !*! viewMat !*! modelMat !* pos


fragShadow :: RuntimeConfig FFloat -> FFloat -> (FFloat, FragDepth)
fragShadow RuntimeConfig{} c = (c, c)


compileShadowShader
    :: ContextHandler ctx
    => UniformBuffer os
    -> ContextT ctx os IO (ShadowShader os)
compileShadowShader uniformBuffer = compileShader $ do
    vertCfg <- getUniform (const (uniformBuffer, 0))
    fragCfg <- getUniform (const (uniformBuffer, 0))

    primitiveStream <- fmap (vertLight vertCfg) <$>
        toPrimitiveStream envPrimitives

    fragmentStream <- fmap (fragShadow fragCfg) <$>
        rasterize (const (Back, PolygonFill, ViewPort (V2 0 0) (V2 1000 1000), DepthRange 0 1)) primitiveStream

    drawDepth (\s -> (NoBlending, envShadowDepth s, DepthOption Less True)) fragmentStream $
        drawColor (\s -> (envShadowColor s, True, False))


--------------------------------------------------

type SolidsShader os = CompiledShader os SolidsShaderEnv
type SolidsShaderEnv = PrimitiveArray Triangles ObjectShaderInput
type SolidShaderAttachment x = (V2 (S x Float), V4 (S x Float), V4 (S x Float))


vertCamera :: RuntimeConfig VFloat -> (V3 VFloat, V3 VFloat) -> (VPos, SolidShaderAttachment V)
vertCamera RuntimeConfig{..} (toV4 1 -> pos, normal) = (screenPos, (uv, fragPos, toV4 1 normal))
  where
    viewMat = lookAt cameraPos (V3 0 0 0) (V3 0 0 1)
    projMat = perspective (pi/3) 1 1000 350000

    fragPos = modelMat !* pos
    screenPos = projMat !*! viewMat !* fragPos
    uv = pos^._xy


diffuseLight :: (Floating a, IfB a, OrdB a) => V4 a -> V4 a -> V4 a -> V3 a -> V3 a
diffuseLight fragPos normal lightPos lightColor =
    let
        lightDir = signorm (lightPos - fragPos)
        diff = maxB (dot normal lightDir) 0
        diffuse = lightColor ^* diff
    in
    diffuse


fragSolid :: RuntimeConfig FFloat -> (V2 FFloat -> FFloat) -> (V2 FFloat -> FFloat) -> SolidShaderAttachment F -> V3 FFloat
fragSolid RuntimeConfig{..} shadowSamp texSamp (uv, fragPos, normal) = c
  where
    objectColor = V3 1 1 1 ^* texSamp (uv ^/ 80000)

    lightPos = rotMatrixZ (time/2) !* V4 60000 0 30000 1
    diffuse = diffuseLight fragPos normal lightPos (V3 0.8 0.8 0.8)

    c = objectColor * diffuse


compileSolidsShader
    :: ContextHandler ctx
    => Window os RGBFloat Depth
    -> V2 Int
    -> UniformBuffer os
    -> ShadowColorTex os
    -> MonochromeTex os
    -> ContextT ctx os IO (SolidsShader os)
compileSolidsShader win screenSize uniformBuffer shadowTex tex = compileShader $ do
    vertCfg <- getUniform (const (uniformBuffer, 0))
    fragCfg <- getUniform (const (uniformBuffer, 0))

    primitiveStream <- fmap (vertCamera vertCfg) <$>
        toPrimitiveStream id

    shadowSampler <- newSampler2D (const (shadowTex, SamplerNearest, (pure Repeat, undefined)))
    texSampler <- newSampler2D (const (tex, SamplerNearest, (pure Repeat, undefined)))

    let shadowSamp = sample2D shadowSampler SampleAuto Nothing Nothing
    let texSamp = sample2D texSampler SampleAuto Nothing Nothing

    fragmentStream <- withRasterizedInfo (\a r -> (fragSolid fragCfg shadowSamp texSamp a, rasterizedFragCoord r ^. _z)) <$>
        rasterize (const (Front, PolygonFill, ViewPort (V2 0 0) screenSize, DepthRange 0 1)) primitiveStream

    drawWindowColorDepth (const (win, ContextColorOption NoBlending (pure True), DepthOption Less True)) fragmentStream


--------------------------------------------------

compileWireframeShader
    :: ContextHandler ctx
    => Window os RGBFloat ds
    -> V2 Int
    -> UniformBuffer os
    -> ContextT ctx os IO (SolidsShader os)
compileWireframeShader win screenSize uniformBuffer = compileShader $ do
    vertCfg <- getUniform (const (uniformBuffer, 0))

    primitiveStream <- fmap (vertCamera vertCfg) <$>
        toPrimitiveStream id

    fragmentStream <- fmap (const 0) <$>
        rasterize (const (Front, PolygonLine 1, ViewPort (V2 0 0) screenSize, DepthRange 0 1)) primitiveStream

    drawWindowColor (const (win, ContextColorOption NoBlending (pure True))) fragmentStream


--------------------------------------------------

type QuadShader os = CompiledShader os QuadShaderEnv
type QuadShaderEnv = PrimitiveArray Triangles QuadShaderInput

vertQuad :: V2 VFloat -> (VPos, V2 VFloat)
vertQuad pos = (V4 x y 0 1, (pos + 1) / 2)
  where
    V2 x y = pos/4 - 0.75

compileQuadShader
    :: ContextHandler ctx
    => Window os RGBFloat ds
    -> V2 Int
    -> ShadowColorTex os
    -> ContextT ctx os IO (QuadShader os)
compileQuadShader win screenSize tex = compileShader $ do
    primitiveStream <- fmap vertQuad <$> toPrimitiveStream id

    texSampler <- newSampler2D (const (tex, SamplerNearest, (pure Repeat, undefined)))
    let texSamp = sample2D texSampler SampleAuto Nothing Nothing

    fragmentStream <- fmap (\uv -> let c = texSamp uv in V3 c c c) <$>
        rasterize (const (FrontAndBack, PolygonFill, ViewPort (V2 0 0) screenSize, DepthRange 0 1)) primitiveStream

    drawWindowColor (const (win, ContextColorOption NoBlending (pure True))) fragmentStream
