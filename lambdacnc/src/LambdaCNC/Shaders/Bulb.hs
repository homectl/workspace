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
                                           GlobalUniforms (..), LightUniformBuffer, LightUniforms(..))
import           LambdaCNC.Shaders.Common (Shader3DInput, cameraMat,
                                            toV4)
import           Prelude                  hiding ((<*))

--------------------------------------------------

type Compiled os = CompiledShader os Env
data Env = Env
    { envScreenSize :: V2 Int
    , envPrimitives :: PrimitiveArray Triangles Shader3DInput
    , envIndex :: Int
    }

--------------------------------------------------

-- Roughly put the light emitter in the center of the bulb by moving the bulb's
-- sphere up (in Z direction).
bulbOffset :: V4 VFloat
bulbOffset = V4 0 0 4200 0


vert :: GlobalUniforms VFloat -> LightUniforms VFloat -> (V3 VFloat, V3 VFloat) -> (V4 VFloat, V3 VFloat)
vert GlobalUniforms{..} LightUniforms{..} (vertPos, n) = (pos, n)
  where
    objPos = rotMatrixX (-pi/2) !*! scaled 200 !* toV4 0 vertPos + bulbOffset + toV4 1 lightPos
    pos = cameraMat screenSize cameraPos !* objPos

--------------------------------------------------

frag :: V3 FFloat -> V3 FFloat
frag _ = 1

--------------------------------------------------

solidShader
    :: ContextHandler ctx
    => GlobalUniformBuffer os
    -> LightUniformBuffer os
    -> Window os RGBFloat Depth
    -> ContextT ctx os IO (Compiled os)
solidShader globalUni lightUni win = compileShader $ do
    vertGlobal <- getUniform (const (globalUni, 0))
    vertLight <- getUniform (\Env{..} -> (lightUni, envIndex))

    primitiveStream <- fmap (vert vertGlobal vertLight) <$> toPrimitiveStream envPrimitives

    fragmentStream <- withRasterizedInfo (\a r -> (frag a, rasterizedFragCoord r ^. _z)) <$>
        rasterize (\env -> (Front, PolygonFill, ViewPort (V2 0 0) (envScreenSize env), DepthRange 0 1)) primitiveStream

    drawWindowColorDepth (const (win, ContextColorOption NoBlending (pure True), DepthOption Less True)) fragmentStream

--------------------------------------------------

wireframeShader
    :: ContextHandler ctx
    => GlobalUniformBuffer os
    -> LightUniformBuffer os
    -> Window os RGBFloat Depth
    -> ContextT ctx os IO (Compiled os)
wireframeShader globalUni lightUni win = compileShader $ do
    vertGlobal <- getUniform (const (globalUni, 0))
    vertLight <- getUniform (\Env{..} -> (lightUni, envIndex))

    primitiveStream <- fmap (vert vertGlobal vertLight) <$> toPrimitiveStream envPrimitives

    fragmentStream <- withRasterizedInfo (\_ r -> (0, rasterizedFragCoord r ^. _z)) <$>
        rasterize (\env -> (FrontAndBack, PolygonLine 1, ViewPort (V2 0 0) (envScreenSize env), DepthRange 0 1)) primitiveStream

    drawWindowColorDepth (const (win, ContextColorOption NoBlending (pure True), DepthOption Less True)) fragmentStream
