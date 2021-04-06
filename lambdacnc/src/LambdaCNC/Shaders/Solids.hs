{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE TypeFamilies #-}
module LambdaCNC.Shaders.Solids where

import           Control.Lens     ((^.))
import           Graphics.GPipe   hiding (normalize)
import           LambdaCNC.Config (GlobalUniforms (..), GlobalUniformBuffer, ObjectUniforms(..), ObjectUniformBuffer)
import           Prelude          hiding ((<*))
import LambdaCNC.Shaders.Common
    ( MonochromeTex,
      ShadowColorTex,
      Shader3DInput,
      toV4,
      modelMat,
      shadowMapSize,
      lightMat,
      getLightPos,
      cameraMat )

--------------------------------------------------

type Compiled os = CompiledShader os Env
data Env = Env
    { envScreenSize :: V2 Int
    , envPrimitives :: PrimitiveArray Triangles Shader3DInput
    }
type SolidShaderAttachment x = (V2 (S x Float), V4 (S x Float), V4 (S x Float), V4 (S x Float))

--------------------------------------------------

vert :: GlobalUniforms VFloat -> ObjectUniforms VFloat -> (V3 VFloat, V3 VFloat) -> (VPos, SolidShaderAttachment V)
vert GlobalUniforms{..} ObjectUniforms{..} (toV4 1 -> pos, normal) = (screenPos, (uv, fragPos, fragNormal, fragPosLightSpace))
  where
    fragPos = modelMat time !* (pos + toV4 0 objectPos)
    fragNormal = modelMat time !* toV4 1 normal
    fragPosLightSpace = lightMat (getLightPos time) !* fragPos
    screenPos = cameraMat screenSize cameraPos !* fragPos
    uv = pos^._xy

--------------------------------------------------

shadowCoords :: (V2 FFloat -> FFloat) -> V4 FFloat -> V4 FFloat -> V4 FFloat -> V4 FFloat -> V2 FFloat -> FFloat
shadowCoords shadowSamp lightPos fp lsfp normal offset = shadow
  where
    lightDir = signorm (lightPos - fp)

    -- perform perspective divide
    projCoords = (lsfp^._xyz ^/ lsfp^._w) * 0.5 + 0.5
    -- get depth of current fragment from light's perspective
    currentDepth = projCoords^._z

    texelSize = fmap (1.0 /) shadowMapSize
    pcfCoords = projCoords^._xy + offset * texelSize
    closestDepth = shadowSamp pcfCoords

    -- check whether current frag pos is in shadow
    bias = maxB (0.0005 * (1.0 - dot normal lightDir)) 0.00005

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
        , V2 (-1) (-1)
        , V2 (-1) 0
        , V2 (-1) 1
        , V2 0 (-1)
        , V2 0 1
        , V2 1 (-1)
        , V2 1 0
        , V2 1 1
        ]


diffuseLight :: (Floating a, IfB a, OrdB a) => V4 a -> V4 a -> V4 a -> V3 a -> V3 a
diffuseLight fragPos normal lightPos lightColor =
    let
        lightDir = signorm (lightPos - fragPos)
        diff = maxB (dot normal lightDir) 0
        diffuse = lightColor ^* diff
    in
    diffuse


frag :: GlobalUniforms FFloat -> (V2 FFloat -> FFloat) -> (V2 FFloat -> FFloat) -> SolidShaderAttachment F -> V3 FFloat
frag GlobalUniforms{..} shadowSamp texSamp (uv, fragPos, normal, fragPosLightSpace) = c
  where
    objectColor = V3 1 1 1 ^* texSamp (uv ^/ 80000)
    lightPos = getLightPos time

    diffuse = diffuseLight fragPos normal lightPos (V3 1 1 1)
            ^* shadowCalculation shadowSamp lightPos fragPos normal fragPosLightSpace

    c = diffuse / 1.2 * objectColor

--------------------------------------------------

solidShader
    :: ContextHandler ctx
    => Window os RGBFloat Depth
    -> GlobalUniformBuffer os
    -> ObjectUniformBuffer os
    -> ShadowColorTex os
    -> MonochromeTex os
    -> ContextT ctx os IO (Compiled os)
solidShader win globalUni objectUni shadowTex tex = compileShader $ do
    vertGlobal <- getUniform (const (globalUni, 0))
    vertObject <- getUniform (const (objectUni, 0))
    fragGlobal <- getUniform (const (globalUni, 0))

    primitiveStream <- fmap (vert vertGlobal vertObject) <$>
        toPrimitiveStream envPrimitives

    shadowSampler <- newSampler2D (const (shadowTex, SamplerFilter Linear Linear Linear Nothing, (pure Repeat, undefined)))
    texSampler <- newSampler2D (const (tex, SamplerNearest, (pure Repeat, undefined)))

    let shadowSamp = sample2D shadowSampler SampleAuto Nothing Nothing
    let texSamp = sample2D texSampler SampleAuto Nothing Nothing

    fragmentStream <- withRasterizedInfo (\a r -> (frag fragGlobal shadowSamp texSamp a, rasterizedFragCoord r ^. _z)) <$>
        rasterize (\env -> (Front, PolygonFill, ViewPort (V2 0 0) (envScreenSize env), DepthRange 0 1)) primitiveStream

    drawWindowColorDepth (const (win, ContextColorOption NoBlending (pure True), DepthOption Less True)) fragmentStream

--------------------------------------------------

wireframeShader
    :: ContextHandler ctx
    => Window os RGBFloat Depth
    -> GlobalUniformBuffer os
    -> ObjectUniformBuffer os
    -> ContextT ctx os IO (Compiled os)
wireframeShader win globalUni objectUni = compileShader $ do
    vertGlobal <- getUniform (const (globalUni, 0))
    vertObject <- getUniform (const (objectUni, 0))

    primitiveStream <- fmap (vert vertGlobal vertObject) <$>
        toPrimitiveStream envPrimitives

    fragmentStream <- withRasterizedInfo (\_ r -> (0, rasterizedFragCoord r ^. _z)) <$>
        rasterize (\env -> (Front, PolygonLine 1, ViewPort (V2 0 0) (envScreenSize env), DepthRange 0 1)) primitiveStream

    drawWindowColorDepth (const (win, ContextColorOption NoBlending (pure True), DepthOption Less True)) fragmentStream
