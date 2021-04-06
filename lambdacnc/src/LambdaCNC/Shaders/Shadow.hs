{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE TypeFamilies        #-}
module LambdaCNC.Shaders.Shadow where

import           Control.Lens             ((^.))
import           Graphics.GPipe           hiding (normalize)
import           LambdaCNC.Config         (GlobalUniformBuffer,
                                           GlobalUniforms (..),
                                           ObjectUniformBuffer,
                                           ObjectUniforms (..), LightUniforms(..), LightUniformBuffer)
import           LambdaCNC.Shaders.Common (Shader3DInput, lightMat,
                                           modelMat, shadowMapSize, toV4)
import           Prelude                  hiding ((<*))

--------------------------------------------------

type Compiled os = CompiledShader os Env
data Env = Env
    { envPrimitives  :: PrimitiveArray Triangles Shader3DInput
    , envShadowColor :: Image (Format RFloat)
    , envShadowDepth :: Image (Format Depth)
    }

--------------------------------------------------

vert :: GlobalUniforms VFloat -> ObjectUniforms VFloat -> [LightUniforms VFloat] -> (V3 VFloat, V3 VFloat) -> (VPos, VFloat)
vert GlobalUniforms{..} ObjectUniforms{..} [LightUniforms{..}] (toV4 1 -> pos, normal) =
    (screenPos, screenPos^._z * 0.5 + 0.5)
  where
    screenPos = lightMat (toV4 1 lightPos) !*! modelMat time !* (pos + toV4 0 objectPos)

--------------------------------------------------

frag :: GlobalUniforms FFloat -> FFloat -> (FFloat, FragDepth)
frag GlobalUniforms{} c = (c, c)

--------------------------------------------------

solidShader
    :: ContextHandler ctx
    => GlobalUniformBuffer os
    -> ObjectUniformBuffer os
    -> LightUniformBuffer os
    -> ContextT ctx os IO (Compiled os)
solidShader globalUni objectUni lightUni = compileShader $ do
    vertGlobal <- getUniform (const (globalUni, 0))
    vertObject <- getUniform (const (objectUni, 0))
    vertLight <- getUniform (const (lightUni, 0))
    
    fragGlobal <- getUniform (const (globalUni, 0))

    primitiveStream <- fmap (vert vertGlobal vertObject [vertLight]) <$>
        toPrimitiveStream envPrimitives

    fragmentStream <- fmap (frag fragGlobal) <$>
        rasterize (const (Back, PolygonFill, ViewPort (V2 0 0) shadowMapSize, DepthRange 0 1)) primitiveStream

    drawDepth (\s -> (NoBlending, envShadowDepth s, DepthOption Less True)) fragmentStream $
        drawColor (\s -> (envShadowColor s, True, False))

