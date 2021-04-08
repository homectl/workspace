{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module LambdaCNC.Shaders.Bulb where

import           Control.Lens             ((^.))
import           Graphics.GPipe           hiding (normalize)
import           LambdaCNC.Config         (GlobalUniformBuffer,
                                           GlobalUniforms (..),
                                           LightUniformBuffer,
                                           LightUniforms (..))
import           LambdaCNC.Shaders.Common (Shader3DInput, cameraMat,
                                           lightTransform, toV4)
import           Prelude                  hiding ((<*))

--------------------------------------------------

type Compiled os = CompiledShader os Env
data Env = Env
    { envScreenSize :: V2 Int
    , envPrimitives :: PrimitiveArray Triangles Shader3DInput
    , envColorFb      :: Image (Format RGBAFloat)
    , envBrightFb      :: Image (Format RGBAFloat)
    , envDepthFb      :: Image (Format Depth)
    , envIndex      :: Int
    }

--------------------------------------------------

-- Roughly put the light emitter in the center of the bulb by moving the bulb's
-- sphere up (in Z direction).
bulbOffset :: V4 VFloat
bulbOffset = V4 0 0 4200 0


vert :: GlobalUniforms VFloat -> LightUniforms VFloat -> (V3 VFloat, V3 VFloat) -> (V4 VFloat, V3 VFloat)
vert GlobalUniforms{..} LightUniforms{..} (vertPos, n) = (pos, n)
  where
    objPos = rotMatrixX (-pi/2) !*! scaled 200 !* toV4 0 vertPos + bulbOffset + (lightTransform time !* toV4 1 lightPos)
    pos = cameraMat screenSize cameraPos !* objPos

--------------------------------------------------

frag :: GlobalUniforms FFloat -> LightUniforms FFloat -> V3 FFloat -> (V4 FFloat, V4 FFloat)
frag GlobalUniforms{..} LightUniforms{..} _ = (toV4 1 fragColor, toV4 1 brightColor)
  where
    fragColor = exposure *^ lightColor
    brightness = dot fragColor (V3 0.2126 0.7152 0.0722)
    brightColor = ifThenElse' (brightness >* 1.0) fragColor 0

--------------------------------------------------

solidShader
    :: ContextHandler ctx
    => GlobalUniformBuffer os
    -> LightUniformBuffer os
    -> ContextT ctx os IO (Compiled os)
solidShader globalUni lightUni = compileShader $ do
    vertGlobal <- getUniform (const (globalUni, 0))
    vertLight <- getUniform (\Env{..} -> (lightUni, envIndex))
    fragGlobal <- getUniform (const (globalUni, 0))
    fragLight <- getUniform (\Env{..} -> (lightUni, envIndex))

    primitiveStream <- fmap (vert vertGlobal vertLight) <$> toPrimitiveStream envPrimitives

    fragmentStream <- withRasterizedInfo (\a r -> (frag fragGlobal fragLight a, rasterizedFragCoord r ^. _z)) <$>
        rasterize (\env -> (Front, PolygonFill, ViewPort (V2 0 0) (envScreenSize env), DepthRange 0 1)) primitiveStream

    drawDepth (\s -> (NoBlending, envDepthFb s, DepthOption Less True)) fragmentStream $
        \(fragColor, brightColor) -> do
            drawColor (\s -> (envColorFb s, pure True, False)) fragColor
            drawColor (\s -> (envBrightFb s, pure True, False)) brightColor

--------------------------------------------------

wireframeShader
    :: ContextHandler ctx
    => GlobalUniformBuffer os
    -> LightUniformBuffer os
    -> ContextT ctx os IO (Compiled os)
wireframeShader globalUni lightUni = compileShader $ do
    vertGlobal <- getUniform (const (globalUni, 0))
    vertLight <- getUniform (\Env{..} -> (lightUni, envIndex))

    primitiveStream <- fmap (vert vertGlobal vertLight) <$> toPrimitiveStream envPrimitives

    fragmentStream <- withRasterizedInfo (\_ r -> (0, rasterizedFragCoord r ^. _z)) <$>
        rasterize (\env -> (FrontAndBack, PolygonLine 1, ViewPort (V2 0 0) (envScreenSize env), DepthRange 0 1)) primitiveStream

    drawDepth (\s -> (NoBlending, envDepthFb s, DepthOption Less True)) fragmentStream $
        drawColor (\s -> (envColorFb s, pure True, False))
