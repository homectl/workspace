{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE TypeFamilies #-}
module LambdaCNC.Shaders.Quad where

import           Graphics.GPipe   hiding (normalize)
import           Prelude          hiding ((<*))
import LambdaCNC.Shaders.Common ( ShadowColorTex, Shader2DInput )

--------------------------------------------------

type Compiled os = CompiledShader os Env
data Env = Env
    { envScreenSize :: V2 Int
    , envPrimitives :: PrimitiveArray Triangles Shader2DInput
    }

--------------------------------------------------

vert :: V2 VFloat -> (VPos, V2 VFloat)
vert pos = (V4 x y 0 1, (pos + 1) / 2)
  where
    V2 x y = pos/8 - (1 - 1.0 / 8)

--------------------------------------------------

frag :: (V2 FFloat -> FFloat) -> V2 FFloat -> V3 FFloat
frag texSamp uv = let c = texSamp uv in V3 c c c

--------------------------------------------------

solidShader
    :: ContextHandler ctx
    => [ShadowColorTex os]
    -> Window os RGBFloat ds
    -> ContextT ctx os IO (Compiled os)
solidShader [tex] win = compileShader $ do
    primitiveStream <- fmap vert <$> toPrimitiveStream envPrimitives

    texSampler <- newSampler2D (const (tex, SamplerNearest, (pure Repeat, undefined)))
    let texSamp = sample2D texSampler SampleAuto Nothing Nothing

    fragmentStream <- fmap (frag texSamp) <$>
        rasterize (\env -> (FrontAndBack, PolygonFill, ViewPort (V2 0 0) (envScreenSize env), DepthRange 0 1)) primitiveStream

    drawWindowColor (const (win, ContextColorOption NoBlending (pure True))) fragmentStream
