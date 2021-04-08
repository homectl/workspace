{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module LambdaCNC.Shaders.Blend where

import           Graphics.GPipe           hiding (normalize)
import           LambdaCNC.Config         (GlobalUniformBuffer,
                                           GlobalUniforms (..))
import           LambdaCNC.Shaders.Common (ColorTex, Shader2DInput)
import           Prelude                  hiding ((<*))

--------------------------------------------------

type Compiled os = CompiledShader os (Env os)
data Env os = Env
    { envScreenSize :: V2 Int
    , envPrimitives :: PrimitiveArray Triangles Shader2DInput
    , envTexture1   :: ColorTex os
    , envTexture2   :: ColorTex os
    , envColorFb    :: Image (Format RGBFloat)
    }

--------------------------------------------------

vert :: V2 VFloat -> (VPos, V2 VFloat)
vert pos = (V4 x y 0 1, (pos + 1) / 2)
  where
    V2 x y = pos

--------------------------------------------------

frag :: GlobalUniforms FFloat -> Sampler2D (Format RGBFloat) -> Sampler2D (Format RGBFloat) -> V2 FFloat -> V3 FFloat
frag GlobalUniforms{..} sampler1 sampler2 uv = result
  where
    sample1, sample2 :: V2 FFloat -> V3 FFloat
    sample1 = sample2D sampler1 SampleAuto Nothing Nothing
    sample2 = sample2D sampler2 SampleAuto Nothing Nothing

    gamma = 0.8

    result = (1 - exp(-(sample1 uv + sample2 uv) ^* exposure)) ** (1.0 / gamma)

--------------------------------------------------

solidShader
    :: ContextHandler ctx
    => GlobalUniformBuffer os
    -> ContextT ctx os IO (Compiled os)
solidShader globalUni = compileShader $ do
    fragGlobal <- getUniform (const (globalUni, 0))

    primitiveStream <- fmap vert <$> toPrimitiveStream envPrimitives

    sampler1 <- newSampler2D (\Env{..} -> (envTexture1, SamplerNearest, (pure ClampToEdge, undefined)))
    sampler2 <- newSampler2D (\Env{..} -> (envTexture2, SamplerNearest, (pure ClampToEdge, undefined)))

    fragmentStream <- fmap (frag fragGlobal sampler1 sampler2) <$>
        rasterize (\env -> (FrontAndBack, PolygonFill, ViewPort (V2 0 0) (envScreenSize env), DepthRange 0 1)) primitiveStream

    draw (const NoBlending) fragmentStream $
        drawColor (\s -> (envColorFb s, pure True, False))
