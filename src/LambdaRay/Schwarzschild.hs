{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module LambdaRay.Schwarzschild where

import           Graphics.GPipe

compile :: (ContextHandler ctx, VertexInput a1, FragmentInput a2, VertexFormat a1 ~ (VPos, a2), Color RGBFloat Bool ~ V3 Bool, FragmentFormat a2 ~ Color RGBFloat (S F (ColorElement RGBFloat))) => Window os RGBFloat () -> ContextT ctx os IO (CompiledShader os (PrimitiveArray p a1))
compile win = compileShader $ do
    primitiveStream <- toPrimitiveStream id
    fragmentStream <- rasterize (const (FrontAndBack, ViewPort (V2 0 0) (V2 500 500), DepthRange 0 1)) primitiveStream
    drawWindowColor (const (win, ContextColorOption NoBlending (V3 True True True))) fragmentStream
