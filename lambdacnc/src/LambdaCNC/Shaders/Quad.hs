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
import           LambdaCNC.Shaders.Common (Shader2DInput, ShadowColorTex)
import           Prelude                  hiding ((<*))

--------------------------------------------------

type Compiled os = CompiledShader os (Env os)
data Env os = Env
    { envScreenSize :: V2 Int
    , envPrimitives :: PrimitiveArray Triangles Shader2DInput
    , envTexture    :: ShadowColorTex os
    , envIndex :: Int
    }

--------------------------------------------------

vert :: ObjectUniforms VFloat -> V2 VFloat -> (VPos, V2 VFloat)
vert ObjectUniforms{..} pos = (V4 x y 0 1, (pos + 1) / 2)
  where
    V2 x y = pos ^* objectScale - (1 - pure objectScale) + objectPos^._xy

--------------------------------------------------

frag :: (V2 FFloat -> FFloat) -> V2 FFloat -> V3 FFloat
frag texSamp uv = let c = texSamp uv in V3 c c c

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

    fragmentStream <- fmap (frag texSamp) <$>
        rasterize (\env -> (FrontAndBack, PolygonFill, ViewPort (V2 0 0) (envScreenSize env), DepthRange 0 1)) primitiveStream

    drawWindowColor (const (win, ContextColorOption NoBlending (pure True))) fragmentStream
