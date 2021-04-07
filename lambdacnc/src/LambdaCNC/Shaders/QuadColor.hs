{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module LambdaCNC.Shaders.QuadColor where

import           Control.Lens             ((^.))
import           Graphics.GPipe           hiding (normalize)
import           LambdaCNC.Config         (ObjectUniformBuffer,
                                           ObjectUniforms (..))
import           LambdaCNC.Shaders.Common (ColorTex, Shader2DInput)
import           Prelude                  hiding ((<*))

--------------------------------------------------

type Compiled os = CompiledShader os (Env os)
data Env os = Env
    { envScreenSize :: V2 Int
    , envPrimitives :: PrimitiveArray Triangles Shader2DInput
    , envTexture    :: ColorTex os
    }

--------------------------------------------------

vert :: ObjectUniforms VFloat -> V2 VFloat -> (VPos, V2 VFloat)
vert ObjectUniforms{..} pos = (V4 x y 0 1, (pos + 1) / 2)
  where
    V2 x y = pos ^* objectScale - (1 - pure objectScale) + objectPos^._xy

--------------------------------------------------

solidShader
    :: ContextHandler ctx
    => ObjectUniformBuffer os
    -> Window os RGBFloat ds
    -> ContextT ctx os IO (Compiled os)
solidShader objectUni win = compileShader $ do
    vertObject <- getUniform (const (objectUni, 0))

    primitiveStream <- fmap (vert vertObject) <$> toPrimitiveStream envPrimitives

    texSampler <- newSampler2D (\Env{..} -> (envTexture, SamplerNearest, (pure Repeat, undefined)))
    let texSamp = sample2D texSampler SampleAuto Nothing Nothing

    fragmentStream <- fmap texSamp <$>
        rasterize (\env -> (FrontAndBack, PolygonFill, ViewPort (V2 0 0) (envScreenSize env), DepthRange 0 1)) primitiveStream

    drawWindowColor (const (win, ContextColorOption NoBlending (pure True))) fragmentStream
