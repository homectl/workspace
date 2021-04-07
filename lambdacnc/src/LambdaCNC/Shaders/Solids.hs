{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE TypeFamilies        #-}
module LambdaCNC.Shaders.Solids where

import           Control.Applicative         (liftA2)
import           Control.Lens                ((^.))
import           Graphics.GPipe              hiding (normalize)
import           LambdaCNC.Config            (GlobalUniformBuffer,
                                              GlobalUniforms (..),
                                              LightUniformBuffer,
                                              LightUniforms (..),
                                              ObjectUniformBuffer,
                                              ObjectUniforms (..))
import           LambdaCNC.Shaders.Common    (FragLight (..), MonochromeTex,
                                              Shader3DInput, cameraMat,
                                              lightMat, lightTransform,
                                              modelMat, shadowMapSize, toV4)
import           LambdaCNC.Shaders.LightInfo (LightInfo (..))
import qualified LambdaCNC.Shaders.LightInfo as LightInfo
import           Prelude                     hiding ((<*))

--------------------------------------------------

type Compiled os = CompiledShader os (Env os)
data Env os = Env
    { envScreenSize :: V2 Int
    , envPrimitives :: PrimitiveArray Triangles Shader3DInput
    , envColor      :: Image (Format RGBFloat)
    , envDepth      :: Image (Format Depth)
    }
type SolidShaderAttachment x = (V2 (S x Float), V4 (S x Float), V4 (S x Float), LightInfo (V4 (S x Float)))

--------------------------------------------------

vert :: GlobalUniforms VFloat -> ObjectUniforms VFloat -> LightInfo (LightUniforms VFloat) -> (V3 VFloat, V3 VFloat) -> (VPos, SolidShaderAttachment V)
vert GlobalUniforms{..} ObjectUniforms{..} lightUnis (toV4 1 -> pos, normal) = (screenPos, (uv, fragPos, fragNormal, fragPosLightSPace))
  where
    fragPos = modelMat time !* (pos + toV4 0 objectPos)
    fragNormal = modelMat time !* toV4 1 normal
    fragPosLightSPace = fmap (\LightUniforms{..} -> lightMat (lightTransform time !* toV4 1 lightPos) !* fragPos) lightUnis
    screenPos = cameraMat screenSize cameraPos !* fragPos
    uv = pos^._xy

vertWireframe :: GlobalUniforms VFloat -> ObjectUniforms VFloat -> (V3 VFloat, V3 VFloat) -> (VPos, VFloat)
vertWireframe GlobalUniforms{..} ObjectUniforms{..} (toV4 1 -> pos, normal) = (screenPos, screenPos^._x)
  where
    fragPos = modelMat time !* (pos + toV4 0 objectPos)
    screenPos = cameraMat screenSize cameraPos !* fragPos

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

    shadow = ifThenElse' (currentDepth - bias >* closestDepth) 0.1 1.0


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


frag :: GlobalUniforms FFloat -> LightInfo FragLight -> (V2 FFloat -> FFloat) -> SolidShaderAttachment F -> V3 FFloat
frag GlobalUniforms{..} lights texSamp (uv, fragPos, normal, fragPosLightSpace) = c
  where
    objectColor = V3 1 1 1 ^* texSamp (uv ^/ 100000)

    diffuse =
        sum $ liftA2
            (\FragLight{..} fpls ->
                let lightPos = lightTransform time !* toV4 1 fragLightPos in
                diffuseLight fragPos normal lightPos fragLightColor
                    ^* shadowCalculation fragLightSampler lightPos fragPos normal fpls)
            lights
            fragPosLightSpace

    c = diffuse * objectColor

--------------------------------------------------

solidShader
    :: ContextHandler ctx
    => GlobalUniformBuffer os
    -> ObjectUniformBuffer os
    -> LightUniformBuffer os
    -> LightInfo (MonochromeTex os)
    -> MonochromeTex os
    -> ContextT ctx os IO (Compiled os)
solidShader globalUni objectUni lightUni shadowTextures tex = compileShader $ do
    vertGlobal <- getUniform (const (globalUni, 0))
    vertObject <- getUniform (const (objectUni, 0))
    vertLight <- sequence $ liftA2 (\_ i -> getUniform (const (lightUni, i))) shadowTextures $ LightInfo.fromList [0..]

    fragGlobal <- getUniform (const (globalUni, 0))
    fragLight <- sequence $ liftA2 (\_ i -> getUniform (const (lightUni, i))) shadowTextures $ LightInfo.fromList [0..]

    primitiveStream <- fmap (vert vertGlobal vertObject vertLight) <$>
        toPrimitiveStream envPrimitives

    shadowSamplers <- mapM (\shadowTex -> newSampler2D (const (shadowTex, SamplerFilter Linear Linear Linear Nothing, (pure ClampToBorder, 1)))) shadowTextures
    texSampler <- newSampler2D (const (tex, SamplerNearest, (pure Repeat, undefined)))

    let shadowSamp = liftA2 (\LightUniforms{..} sampler -> FragLight lightPos lightColor sampler)
            fragLight
            (fmap (\sampler -> sample2D sampler SampleAuto Nothing Nothing) shadowSamplers)
    let texSamp = sample2D texSampler SampleAuto Nothing Nothing

    fragmentStream <- withRasterizedInfo (\a r -> (frag fragGlobal shadowSamp texSamp a, rasterizedFragCoord r ^. _z)) <$>
        rasterize (\env -> (Front, PolygonFill, ViewPort (V2 0 0) (envScreenSize env), DepthRange 0 1)) primitiveStream

    drawDepth (\s -> (NoBlending, envDepth s, DepthOption Less True)) fragmentStream $
        drawColor (\s -> (envColor s, pure True, False))

--------------------------------------------------

wireframeShader
    :: ContextHandler ctx
    => GlobalUniformBuffer os
    -> ObjectUniformBuffer os
    -> ContextT ctx os IO (Compiled os)
wireframeShader globalUni objectUni = compileShader $ do
    vertGlobal <- getUniform (const (globalUni, 0))
    vertObject <- getUniform (const (objectUni, 0))

    primitiveStream <- fmap (vertWireframe vertGlobal vertObject) <$>
        toPrimitiveStream envPrimitives

    fragmentStream <- withRasterizedInfo (const $ \r -> (0, rasterizedFragCoord r ^. _z)) <$>
        rasterize (\env -> (Front, PolygonLine 1, ViewPort (V2 0 0) (envScreenSize env), DepthRange 0 1)) primitiveStream

    drawDepth (\s -> (NoBlending, envDepth s, DepthOption Less True)) fragmentStream $
        drawColor (\s -> (envColor s, pure True, False))
