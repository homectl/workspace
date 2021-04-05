{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module LambdaCnc.Shaders
  ( ShadowShader, ShadowShaderEnv (..), compileShadowShader
  , SolidsShader, SolidsShaderEnv, compileSolidsShader
  , compileWireframeShader

  , ShaderInput

  , ShadowColorTex
  , ShadowDepthTex
  , ColorTex
  ) where

import           Control.Lens     ((^.))
import           Graphics.GPipe   hiding (normalize)
import           LambdaCnc.Config (RuntimeConfig (..), UniformBuffer)
import           Prelude          hiding ((<*))


toV4 :: R3 t => a -> t a -> V4 a
toV4 w v = V4 (v^._x) (v^._y) (v^._z) w

modelMat :: V4 (V4 VFloat)
modelMat = rotMatrixZ (pi/8 * 5) -- time

--------------------------------------------------

type ShaderInput = (B3 Float, B3 Float)

type ShadowColorTex os = Texture2D os (Format RFloat)
type ShadowDepthTex os = Texture2D os (Format Depth)
type ColorTex os = Texture2D os (Format RFloat)

--------------------------------------------------

type ShadowShader os = CompiledShader os ShadowShaderEnv
data ShadowShaderEnv = ShadowShaderEnv
    { envPrimitives  :: PrimitiveArray Triangles ShaderInput
    , envShadowColor :: Image (Format RFloat)
    , envShadowDepth :: Image (Format Depth)
    }

vertLight :: RuntimeConfig VFloat -> (V3 VFloat, V3 VFloat) -> (VPos, VFloat)
vertLight RuntimeConfig{..} (toV4 1 -> pos, col) =
    (screenPos, screenPos^._z * 0.5 + 0.5)
  where
    viewMat = lookAt (V3 100000 50000 50000) (V3 0 0 0) (V3 0 0 1)
    projMat = ortho (-50000) 50000 (-50000) 50000 1000 350000
    screenPos = projMat !*! viewMat !*! modelMat !* pos


fragShadow :: RuntimeConfig FFloat -> FFloat -> (FFloat, FragDepth)
fragShadow RuntimeConfig{..} c = (0.5, c)


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
    -- draw (const NoBlending) (fst <$> fragmentStream) $
        drawColor (\s -> (envShadowColor s, True, False))


--------------------------------------------------

type SolidsShader os = CompiledShader os SolidsShaderEnv
type SolidsShaderEnv = PrimitiveArray Triangles ShaderInput


vertCamera :: RuntimeConfig VFloat -> (V3 VFloat, V3 VFloat) -> (VPos, (V2 VFloat, V2 VFloat))
vertCamera RuntimeConfig{..} (toV4 1 -> pos, col) = (screenPos, (uv, col^._xy))
  where
    viewMat = lookAt (V3 40000 50000 60000) (V3 0 0 0) (V3 0 0 1)
    projMat = perspective (pi/3) 1 1000 350000
    screenPos = projMat !*! viewMat !*! modelMat !* pos
    uv = pos^._xy


fragSolid :: RuntimeConfig FFloat -> (V2 FFloat -> FFloat) -> (V2 FFloat -> FFloat) -> (V2 FFloat, V2 FFloat) -> V3 FFloat
fragSolid RuntimeConfig{..} shadowSamp texSamp (V2 u v, _) = V3 c c c
  where c = texSamp (V2 u v / 80000)


compileSolidsShader
    :: ContextHandler ctx
    => Window os RGBFloat ()
    -> V2 Int
    -> UniformBuffer os
    -> ShadowColorTex os
    -> ColorTex os
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

    fragmentStream <- fmap (fragSolid fragCfg shadowSamp texSamp) <$>
        rasterize (const (Front, PolygonFill, ViewPort (V2 0 0) screenSize, DepthRange 0 1)) primitiveStream

    drawWindowColor (const (win, ContextColorOption NoBlending (pure True))) fragmentStream


--------------------------------------------------

compileWireframeShader
    :: ContextHandler ctx
    => Window os RGBFloat ()
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
