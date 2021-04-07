{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module LambdaCNC.Shaders.Quad where

import           Control.Lens             ((^.))
import           Graphics.GPipe           hiding (normalize)
import           LambdaCNC.Config         (ObjectUniformBuffer,
                                           ObjectUniforms (..))
import           LambdaCNC.Shaders.Common (Shader2DInput)
import           Prelude                  hiding ((<*))

--------------------------------------------------

type Compiled os f = CompiledShader os (Env os f)
data Env os f = Env
    { envScreenSize :: V2 Int
    , envPrimitives :: PrimitiveArray Triangles Shader2DInput
    , envTexture    :: Texture2D os (Format f)
    }

--------------------------------------------------

vert :: ObjectUniforms VFloat -> V2 VFloat -> (VPos, V2 VFloat)
vert ObjectUniforms{..} pos = (V4 x y 0 1, (pos + 1) / 2)
  where
    V2 x y = pos ^* objectScale - (1 - pure objectScale) + objectPos^._xy

--------------------------------------------------

solidShader
    :: (ContextHandler ctx, ColorSampleable f)
    => ObjectUniformBuffer os
    -> (Color f (S F (ColorElement f)) -> V3 FFloat)
    -> Window os RGBFloat ds
    -> ContextT ctx os IO (Compiled os f)
solidShader objectUni toColor win = compileShader $ do
    vertObject <- getUniform (const (objectUni, 0))

    primitiveStream <- fmap (vert vertObject) <$> toPrimitiveStream envPrimitives

    texSampler <- newSampler2D (\Env{..} -> (envTexture, SamplerNearest, (pure Repeat, undefined)))
    let texSamp = sample2D texSampler SampleAuto Nothing Nothing

    fragmentStream <- fmap (toColor . texSamp) <$>
        rasterize (\env -> (FrontAndBack, PolygonFill, ViewPort (V2 0 0) (envScreenSize env), DepthRange 0 1)) primitiveStream

    drawWindowColor (const (win, ContextColorOption NoBlending (pure True))) fragmentStream
