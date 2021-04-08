{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module LambdaCNC.Shaders.GaussianBlur where

import           Data.Int                 (Int32)
import           Graphics.GPipe           hiding (normalize)
import           LambdaCNC.Shaders.Common (ColorTex, Shader2DInput)
import           Prelude                  hiding ((<*))

--------------------------------------------------

type Compiled os = CompiledShader os (Env os)
data Env os = Env
    { envScreenSize :: V2 Int
    , envPrimitives :: PrimitiveArray Triangles Shader2DInput
    , envTexture    :: ColorTex os
    , envColorFb    :: Image (Format RGBFloat)
    }

--------------------------------------------------

vert :: V2 VFloat -> (VPos, V2 VFloat)
vert pos = (V4 x y 0 1, (pos + 1) / 2)
  where
    V2 x y = pos

--------------------------------------------------

frag :: FInt -> Sampler2D (Format RGBFloat) -> V2 FFloat -> V3 FFloat
frag horizontal sampler uv = result
  where
    sample = sample2D sampler SampleAuto Nothing Nothing
    weight = (0.227027, [0.1945946, 0.1216216, 0.054054, 0.016216])

    V2 xOff yOff = 1.0 / (toFloat <$> sampler2DSize sampler 0)  -- gets size of single texel

    sampleX i w = sample (uv + V2 (xOff * i) 0) * w
                + sample (uv - V2 (xOff * i) 0) * w
    sampleY i w = sample (uv + V2 0 (yOff * i)) * w
                + sample (uv - V2 0 (yOff * i)) * w

    -- current fragment's contribution + weighted pixel values
    result = sample uv * fst weight +
        ifThenElse' (horizontal ==* 1)
            (blur sampleX)
            (blur sampleY)

    blur sampleWeighted =
        sum . zipWith sampleWeighted (map fromIntegral ([1..] :: [Int])) . snd $ weight

--------------------------------------------------

solidShader
    :: ContextHandler ctx
    => Buffer os (Uniform (B Int32))
    -> ContextT ctx os IO (Compiled os)
solidShader gaussUni = compileShader $ do
    fragGauss <- getUniform (const (gaussUni, 0))

    primitiveStream <- fmap vert <$> toPrimitiveStream envPrimitives

    sampler <- newSampler2D (\Env{..} -> (envTexture, SamplerNearest, (pure ClampToEdge, undefined)))

    fragmentStream <- fmap (frag fragGauss sampler) <$>
        rasterize (\env -> (FrontAndBack, PolygonFill, ViewPort (V2 0 0) (envScreenSize env), DepthRange 0 1)) primitiveStream

    draw (const NoBlending) fragmentStream $
        drawColor (\s -> (envColorFb s, pure True, False))
