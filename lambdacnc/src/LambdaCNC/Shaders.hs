{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE TypeFamilies #-}
module LambdaCNC.Shaders
  ( ShadowShader, ShadowShaderEnv (..), compileShadowShader
  , SolidsShader, SolidsShaderEnv, compileSolidsShader
  , compileWireframeShader
  , QuadShader, QuadShaderEnv, compileQuadShader
  , compileBulbShader, compileBulbWireframeShader

  , ObjectShaderInput, Buffer3D
  , QuadShaderInput, Buffer2D

  , ShadowColorTex
  , ShadowDepthTex
  , MonochromeTex
  , ColorTex

  , shadowMapSize
  ) where

import           Control.Lens     ((^.))
import           Graphics.GPipe   hiding (normalize)
import           LambdaCNC.Config (GlobalUniforms (..), GlobalUniformBuffer, ObjectUniforms(..), ObjectUniformBuffer)
import           Prelude          hiding ((<*))


toV4 :: R3 t => a -> t a -> V4 a
toV4 w v = V4 (v^._x) (v^._y) (v^._z) w

aspectRatio :: Fractional a => V2 a -> a
aspectRatio (V2 w h) = w / h

modelMat :: VFloat -> V4 (V4 VFloat)
modelMat time = rotMatrixZ (pi/2)
-- modelMat time = rotMatrixZ (time / 3)

lightMat :: Floating a => V4 a -> M44 a
lightMat lightPos = projMat !*! viewMat
  where
    viewMat = lookAt (lightPos^._xyz) (V3 0 0 0) (V3 0 0 1)
    projMat = ortho (-r) r (-t) t 1000 350000
    t = 50000
    r = 80000

getLightPos :: Floating a => a -> V4 a
-- getLightPos time = rotMatrixZ (time/2) !* V4 120000 0 30000 1
getLightPos _ = rotMatrixZ (pi/10*(-7)) !* V4 100000 1000 30000 1

--------------------------------------------------

type ObjectShaderInput = (B3 Float, B3 Float)
type Buffer3D os = Buffer os ObjectShaderInput
type QuadShaderInput = (B2 Float)
type Buffer2D os = Buffer os QuadShaderInput

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


vertLight :: GlobalUniforms VFloat -> ObjectUniforms VFloat -> (V3 VFloat, V3 VFloat) -> (VPos, VFloat)
vertLight GlobalUniforms{..} ObjectUniforms{..} (toV4 1 -> pos, normal) =
    (screenPos, screenPos^._z * 0.5 + 0.5)
  where
    screenPos = lightMat (getLightPos time) !*! modelMat time !* (pos + toV4 0 objectPos)


fragShadow :: GlobalUniforms FFloat -> FFloat -> (FFloat, FragDepth)
fragShadow GlobalUniforms{} c = (c, c)


compileShadowShader
    :: ContextHandler ctx
    => GlobalUniformBuffer os
    -> ObjectUniformBuffer os
    -> ContextT ctx os IO (ShadowShader os)
compileShadowShader globalUni objectUni = compileShader $ do
    vertGlobal <- getUniform (const (globalUni, 0))
    vertObject <- getUniform (const (objectUni, 0))
    fragGlobal <- getUniform (const (globalUni, 0))

    primitiveStream <- fmap (vertLight vertGlobal vertObject) <$>
        toPrimitiveStream envPrimitives

    fragmentStream <- fmap (fragShadow fragGlobal) <$>
        rasterize (const (Back, PolygonFill, ViewPort (V2 0 0) shadowMapSize, DepthRange 0 1)) primitiveStream

    drawDepth (\s -> (NoBlending, envShadowDepth s, DepthOption Less True)) fragmentStream $
        drawColor (\s -> (envShadowColor s, True, False))


--------------------------------------------------

type SolidsShader os = CompiledShader os SolidsShaderEnv
type SolidsShaderEnv = PrimitiveArray Triangles ObjectShaderInput
type SolidShaderAttachment x = (V2 (S x Float), V4 (S x Float), V4 (S x Float), V4 (S x Float))

cameraMat :: Floating a => V2 a -> V3 a -> M44 a
cameraMat screenSize cameraPos = projMat !*! viewMat
  where
    viewMat = lookAt cameraPos (V3 0 0 0) (V3 0 0 1)
    projMat = perspective (pi/3) (aspectRatio screenSize) 1000 350000


vertCamera :: GlobalUniforms VFloat -> ObjectUniforms VFloat -> (V3 VFloat, V3 VFloat) -> (VPos, SolidShaderAttachment V)
vertCamera GlobalUniforms{..} ObjectUniforms{..} (toV4 1 -> pos, normal) = (screenPos, (uv, fragPos, fragNormal, fragPosLightSpace))
  where
    fragPos = modelMat time !* (pos + toV4 0 objectPos)
    fragNormal = modelMat time !* toV4 1 normal
    fragPosLightSpace = lightMat (getLightPos time) !* fragPos
    screenPos = cameraMat screenSize cameraPos !* fragPos
    uv = pos^._xy


shadowMapSize :: Num a => V2 a
shadowMapSize = V2 700 700
texelSize :: Fractional a => V2 a
texelSize = fmap (1.0 /) shadowMapSize


shadowCoords :: (V2 FFloat -> FFloat) -> V4 FFloat -> V4 FFloat -> V4 FFloat -> V4 FFloat -> V2 FFloat -> FFloat
shadowCoords shadowSamp lightPos fp lsfp normal offset = shadow
  where
    lightDir = signorm (lightPos - fp)

    -- perform perspective divide
    projCoords = (lsfp^._xyz ^/ lsfp^._w) * 0.5 + 0.5
    -- get depth of current fragment from light's perspective
    currentDepth = projCoords^._z

    pcfCoords = projCoords^._xy + offset * texelSize
    closestDepth = shadowSamp pcfCoords

    -- check whether current frag pos is in shadow
    bias = 0.005 -- maxB (0.05 * (1.0 - dot normal lightDir)) 0.005

    shadow = ifThenElse' (currentDepth - bias >* closestDepth) 0.3 1.0


shadowCalculation :: (V2 FFloat -> FFloat) -> V4 FFloat -> V4 FFloat -> V4 FFloat -> V4 FFloat -> FFloat
shadowCalculation shadowSamp lightPos fragPos normal fragPosLightSpace =
    (/ fromIntegral (length pcfOffsets))
    . sum
    . map (shadowCoords shadowSamp lightPos fragPos fragPosLightSpace normal)
    $ pcfOffsets
  where
    pcfOffsets =
        [ V2 0 0
        -- , V2 (-1) (-1)
        -- , V2 (-1) 0
        -- , V2 (-1) 1
        -- , V2 0 (-1)
        -- , V2 0 1
        -- , V2 1 (-1)
        -- , V2 1 0
        -- , V2 1 1
        ]


diffuseLight :: (Floating a, IfB a, OrdB a) => V4 a -> V4 a -> V4 a -> V3 a -> V3 a
diffuseLight fragPos normal lightPos lightColor =
    let
        lightDir = signorm (lightPos - fragPos)
        diff = maxB (dot normal lightDir) 0
        diffuse = lightColor ^* diff
    in
    diffuse


fragSolid :: GlobalUniforms FFloat -> (V2 FFloat -> FFloat) -> (V2 FFloat -> FFloat) -> SolidShaderAttachment F -> V3 FFloat
fragSolid GlobalUniforms{..} shadowSamp texSamp (uv, fragPos, normal, fragPosLightSpace) = c
  where
    objectColor = V3 1 1 1 ^* texSamp (uv ^/ 80000)
    lightPos = getLightPos time

    diffuse = diffuseLight fragPos normal lightPos (V3 1 1 1)
            ^* shadowCalculation shadowSamp lightPos fragPos normal fragPosLightSpace

    c = objectColor * diffuse


compileSolidsShader
    :: ContextHandler ctx
    => Window os RGBFloat Depth
    -> V2 Int
    -> GlobalUniformBuffer os
    -> ObjectUniformBuffer os
    -> ShadowColorTex os
    -> MonochromeTex os
    -> ContextT ctx os IO (SolidsShader os)
compileSolidsShader win screenSize globalUni objectUni shadowTex tex = compileShader $ do
    vertGlobal <- getUniform (const (globalUni, 0))
    vertObject <- getUniform (const (objectUni, 0))
    fragGlobal <- getUniform (const (globalUni, 0))

    primitiveStream <- fmap (vertCamera vertGlobal vertObject) <$>
        toPrimitiveStream id

    shadowSampler <- newSampler2D (const (shadowTex, SamplerFilter Linear Linear Linear Nothing, (pure Repeat, undefined)))
    texSampler <- newSampler2D (const (tex, SamplerNearest, (pure Repeat, undefined)))

    let shadowSamp = sample2D shadowSampler SampleAuto Nothing Nothing
    let texSamp = sample2D texSampler SampleAuto Nothing Nothing

    fragmentStream <- withRasterizedInfo (\a r -> (fragSolid fragGlobal shadowSamp texSamp a, rasterizedFragCoord r ^. _z)) <$>
        rasterize (const (Front, PolygonFill, ViewPort (V2 0 0) screenSize, DepthRange 0 1)) primitiveStream

    drawWindowColorDepth (const (win, ContextColorOption NoBlending (pure True), DepthOption Less True)) fragmentStream


--------------------------------------------------

compileWireframeShader
    :: ContextHandler ctx
    => Window os RGBFloat Depth
    -> V2 Int
    -> GlobalUniformBuffer os
    -> ObjectUniformBuffer os
    -> ContextT ctx os IO (SolidsShader os)
compileWireframeShader win screenSize globalUni objectUni = compileShader $ do
    vertGlobal <- getUniform (const (globalUni, 0))
    vertObject <- getUniform (const (objectUni, 0))

    primitiveStream <- fmap (vertCamera vertGlobal vertObject) <$>
        toPrimitiveStream id

    fragmentStream <- withRasterizedInfo (\_ r -> (0, rasterizedFragCoord r ^. _z)) <$>
        rasterize (const (Front, PolygonLine 1, ViewPort (V2 0 0) screenSize, DepthRange 0 1)) primitiveStream

    drawWindowColorDepth (const (win, ContextColorOption NoBlending (pure True), DepthOption Less True)) fragmentStream


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

--------------------------------------------------

-- Roughly put the light emitter in the center of the bulb by moving the bulb's
-- sphere up (in Z direction).
bulbOffset :: V4 VFloat
bulbOffset = V4 0 0 4200 0

vertBulb :: GlobalUniforms VFloat -> (V3 VFloat, V3 VFloat) -> (V4 VFloat, V3 VFloat)
vertBulb GlobalUniforms{..} (vertPos, n) = (pos, n)
  where
    objPos = rotMatrixX (-pi/2) !*! scaled 200 !* toV4 0 vertPos + getLightPos time + bulbOffset
    pos = cameraMat screenSize cameraPos !* objPos


compileBulbShader
    :: ContextHandler ctx
    => Window os RGBFloat Depth
    -> V2 Int
    -> GlobalUniformBuffer os
    -> ContextT ctx os IO (SolidsShader os)
compileBulbShader win screenSize globalUni = compileShader $ do
    vertGlobal <- getUniform (const (globalUni, 0))

    primitiveStream <- fmap (vertBulb vertGlobal) <$> toPrimitiveStream id

    fragmentStream <- withRasterizedInfo (\_ r -> (1, rasterizedFragCoord r ^. _z)) <$>
        rasterize (const (Front, PolygonFill, ViewPort (V2 0 0) screenSize, DepthRange 0 1)) primitiveStream

    drawWindowColorDepth (const (win, ContextColorOption NoBlending (pure True), DepthOption Less True)) fragmentStream


compileBulbWireframeShader
    :: ContextHandler ctx
    => Window os RGBFloat Depth
    -> V2 Int
    -> GlobalUniformBuffer os
    -> ContextT ctx os IO (SolidsShader os)
compileBulbWireframeShader win screenSize globalUni = compileShader $ do
    vertGlobal <- getUniform (const (globalUni, 0))

    primitiveStream <- fmap (vertBulb vertGlobal) <$> toPrimitiveStream id

    fragmentStream <- withRasterizedInfo (\_ r -> (0, rasterizedFragCoord r ^. _z)) <$>
        rasterize (const (FrontAndBack, PolygonLine 1, ViewPort (V2 0 0) screenSize, DepthRange 0 1)) primitiveStream

    drawWindowColorDepth (const (win, ContextColorOption NoBlending (pure True), DepthOption Less True)) fragmentStream
