{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE DeriveFunctor #-}
module LambdaCNC.GPU
  ( main
  ) where

import           Control.Applicative          (liftA2)
import           Control.Concurrent           (threadDelay)
import           Control.Monad                (forM, forM_, unless)
import           Control.Monad.IO.Class       (liftIO)
import qualified Data.Time.Clock              as Time
import           Data.Word                    (Word32)
import           Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW  as GLFW
import qualified Graphics.GPipe.Engine.STL    as STL
import           Graphics.GPipe.Engine.TimeIt (timeIt, timeItInPlace)
import           LambdaCNC.Config             (GlobalUniformBuffer,
                                               GlobalUniforms (..),
                                               ObjectUniformBuffer, Solids (..),
                                               defaultGlobalUniforms,
                                               defaultObjectUniforms, ObjectUniforms(..))
import qualified LambdaCNC.Shaders            as Shaders
import           Prelude                      hiding ((<*))
import qualified System.Environment           as Env


fps :: Double
fps = 24

viewPort :: V2 Int
viewPort = V2 2000 1500


data Shaders os = Shaders
    { shadowShader    :: Shaders.ShadowShader os
    , solidsShader    :: Shaders.SolidsShader os
    , wireframeShader :: Shaders.SolidsShader os
    , quadShader      :: Shaders.QuadShader os
    }

data MachinePosition a = MachinePosition
    { xPos :: a
    , yPos :: a
    , zPos :: a
    }
    deriving (Functor)

machMax :: MachinePosition Int
machMax = MachinePosition 40000 61500 5600

objectPositions :: MachinePosition Int -> Solids (V3 Float)
objectPositions MachinePosition{..} =
    (pure 0)
        { objXAxis = V3 x (y + 4000)      24500
        , objYAxis = V3 0  y               5000
        , objZAxis = V3 x (y - 4000) (z + 24500)
        }
  where
    x = toFloat (xPos - (xMax `div` 2))
    y = toFloat (yPos - (yMax `div` 2))
    z = toFloat (zPos - (zMax `div` 2))

    MachinePosition xMax yMax zMax = machMax


main :: IO ()
main = do
    Env.setEnv "GPIPE_DEBUG" "1"
    runContextT GLFW.defaultHandleConfig{GLFW.configEventPolicy = Just $ GLFW.WaitTimeout $ 1 / fps} $ do
        let V2 w h = viewPort
        win <- newWindow (WindowFormatColorDepth RGB8 Depth16) $ (GLFW.defaultWindowConfig "LambdaCNC")
            { GLFW.configWidth = w
            , GLFW.configHeight = h
            , GLFW.configHints = [GLFW.WindowHint'Samples (Just 4)]
            }

        meshes <- timeIt "Loading meshes" $ do
            meshes <- liftIO $ Solids
                <$> STL.mustLoadSTL "data/models/Bed.stl"
                <*> STL.mustLoadSTL "data/models/Ground.stl"
                <*> STL.mustLoadSTL "data/models/XAxis.stl"
                <*> STL.mustLoadSTL "data/models/YAxis.stl"
                <*> STL.mustLoadSTL "data/models/ZAxis.stl"
            forM meshes $ \mesh -> do
                buf <- newBuffer $ length mesh
                writeBuffer buf 0 mesh
                return buf
        let solids = liftA2 (,) meshes (objectPositions $ MachinePosition 0 0 0)

        quadVertexBuffer <- timeIt "Generating quad" $ do
            buf <- newBuffer 6
            writeBuffer buf 0 [ V2 (-1) (-1), V2 (-1)   1 , V2 1 1
                              , V2 (-1) (-1), V2   1  (-1), V2 1 1
                              ]
            return buf

        globalUni <- newBuffer 1
        writeBuffer globalUni 0 [defaultGlobalUniforms]

        objectUni <- newBuffer 1
        writeBuffer objectUni 0 [defaultObjectUniforms]

        tex <- newTexture2D R8 (V2 8 8) 1
        let whiteBlack = cycle [minBound + maxBound `div` 4,maxBound - maxBound `div` 4] :: [Word32]
            blackWhite = tail whiteBlack
        writeTexture2D tex 0 0 (V2 8 8) (cycle (take 8 whiteBlack ++ take 8 blackWhite))

        shadowColorTex <- newTexture2D R8 Shaders.shadowMapSize 1
        shadowDepthTex <- newTexture2D Depth16 Shaders.shadowMapSize 1

        shaders <- Shaders
            <$> timeIt "Compiling shadow shader..." (Shaders.compileShadowShader globalUni objectUni)
            <*> timeIt "Compiling solids shader..." (Shaders.compileSolidsShader win viewPort globalUni objectUni shadowColorTex tex)
            <*> timeIt "Compiling wireframe shader..." (Shaders.compileWireframeShader win viewPort globalUni objectUni)
            <*> timeIt "Compiling shadow map view shader..." (Shaders.compileQuadShader win viewPort shadowColorTex)

        startTime <- liftIO Time.getCurrentTime
        loop win startTime solids quadVertexBuffer globalUni objectUni shadowColorTex shadowDepthTex shaders


updateUniforms :: Floating a => Time.UTCTime -> IO (GlobalUniforms a)
updateUniforms startTime = do
    now <- Time.getCurrentTime
    return defaultGlobalUniforms{time = fromRational $ toRational $ Time.diffUTCTime now startTime}


loop
    :: Window os RGBFloat Depth
    -> Time.UTCTime
    -> Solids (Buffer os Shaders.ObjectShaderInput, V3 Float)
    -> Buffer os Shaders.QuadShaderInput
    -> GlobalUniformBuffer os
    -> ObjectUniformBuffer os
    -> Shaders.ShadowColorTex os
    -> Shaders.ShadowDepthTex os
    -> Shaders os
    -> ContextT GLFW.Handle os IO ()
loop win startTime solids quadVertexBuffer globalUni objectUni shadowColorTex shadowDepthTex shaders@Shaders{..} = do
    closeRequested <- timeItInPlace "Rendering..." $ do
        cfg <- liftIO $ updateUniforms startTime
        writeBuffer globalUni 0 [cfg]

        -- Clear color and depth of the shadow map.
        render $ do
            shadowColor <- getTexture2DImage shadowColorTex 0
            shadowDepth <- getTexture2DImage shadowDepthTex 0

            clearImageColor shadowColor 0
            clearImageDepth shadowDepth 1

        -- Render each object on the shadow map.
        forM_ solids $ \(solid, pos) -> do
            writeBuffer objectUni 0 [ObjectUniforms pos]
            render $ do
                shadowColor <- getTexture2DImage shadowColorTex 0
                shadowDepth <- getTexture2DImage shadowDepthTex 0

                prim <- fmap (toPrimitiveArray TriangleList) . newVertexArray $ solid
                shadowShader Shaders.ShadowShaderEnv
                    { Shaders.envPrimitives = prim
                    , Shaders.envShadowColor = shadowColor
                    , Shaders.envShadowDepth = shadowDepth
                    }

        -- Clear the window frame buffer.
        render $ do
            clearWindowColor win (V3 0 0 0.8)
            clearWindowDepth win 1

        -- Render each object on the window frame buffer.
        forM_ solids $ \(solid, pos) -> do
            writeBuffer objectUni 0 [ObjectUniforms pos]
            render $ do
                prim <- fmap (toPrimitiveArray TriangleList) . newVertexArray $ solid
                solidsShader prim
                -- wireframeShader prim

                quadVertexArray <- toPrimitiveArray TriangleList <$> newVertexArray quadVertexBuffer
                quadShader quadVertexArray

        swapWindowBuffers win

        GLFW.windowShouldClose win

    liftIO $ threadDelay 10000
    unless (closeRequested == Just True) $
        loop win startTime solids quadVertexBuffer globalUni objectUni shadowColorTex shadowDepthTex shaders
