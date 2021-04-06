{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module LambdaCNC.GPU
  ( main
  ) where

import           Control.Applicative          (liftA2)
import           Control.Concurrent           (threadDelay)
import           Control.Lens                 ((^.))
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
                                               ObjectUniformBuffer,
                                               ObjectUniforms (..), Solids (..),
                                               defaultGlobalUniforms,
                                               defaultObjectUniforms)
import qualified LambdaCNC.Shaders.Bulb       as BulbShader
import qualified LambdaCNC.Shaders.Common     as Shaders
import qualified LambdaCNC.Shaders.Quad       as QuadShader
import qualified LambdaCNC.Shaders.Shadow     as ShadowShader
import qualified LambdaCNC.Shaders.Solids     as SolidsShader
import           Prelude                      hiding ((<*))
import qualified System.Environment           as Env


fps :: Double
fps = 24

windowSize :: V2 Int
windowSize = V2 3000 1500


data Shaders os = Shaders
    { shadowShader        :: ShadowShader.Compiled os
    , solidsShader        :: SolidsShader.Compiled os
    , wireframeShader     :: SolidsShader.Compiled os
    , quadShader          :: QuadShader.Compiled os
    , bulbShader          :: BulbShader.Compiled os
    , bulbWireframeShader :: BulbShader.Compiled os
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
        win <- newWindow (WindowFormatColorDepth RGB8 Depth16) $ (GLFW.defaultWindowConfig "LambdaCNC (GPipe)")
            { GLFW.configWidth = windowSize^._x
            , GLFW.configHeight = windowSize^._y
            , GLFW.configHints =
                [
                -- , GLFW.WindowHint'Samples (Just 1)
                ]
            }

        meshes <- timeIt "Loading object meshes" $ do
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

        lightbulb <- timeIt "Loading object meshes" $ do
            mesh <- liftIO $ STL.mustLoadSTL "data/models/lightbulb.stl"
            buf <- newBuffer $ length mesh
            writeBuffer buf 0 mesh
            return buf

        quad <- timeIt "Generating quad" $ do
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

        shadowColorTex <- newTexture2D R16F Shaders.shadowMapSize 1
        shadowDepthTex <- newTexture2D Depth16 Shaders.shadowMapSize 1

        shaders <- Shaders
            <$> timeIt "Compiling shadow shader..." (ShadowShader.solidShader globalUni objectUni)
            <*> timeIt "Compiling solids shader..." (SolidsShader.solidShader win globalUni objectUni shadowColorTex tex)
            <*> timeIt "Compiling wireframe shader..." (SolidsShader.wireframeShader win globalUni objectUni)
            <*> timeIt "Compiling shadow map view shader..." (QuadShader.solidShader win shadowColorTex)
            <*> timeIt "Compiling lightbulb shader..." (BulbShader.solidShader win globalUni)
            <*> timeIt "Compiling lightbulb wireframe shader..." (BulbShader.wireframeShader win globalUni)

        startTime <- liftIO Time.getCurrentTime
        loop win startTime solids lightbulb quad globalUni objectUni shadowColorTex shadowDepthTex shaders


updateUniforms :: Floating a => Time.UTCTime -> IO (GlobalUniforms a)
updateUniforms startTime = do
    now <- Time.getCurrentTime
    return defaultGlobalUniforms{time = fromRational $ toRational $ Time.diffUTCTime now startTime}


loop
    :: Window os RGBFloat Depth
    -> Time.UTCTime
    -> Solids (Shaders.Buffer3D os, V3 Float)
    -> Shaders.Buffer3D os
    -> Shaders.Buffer2D os
    -> GlobalUniformBuffer os
    -> ObjectUniformBuffer os
    -> Shaders.ShadowColorTex os
    -> Shaders.ShadowDepthTex os
    -> Shaders os
    -> ContextT GLFW.Handle os IO ()
loop win startTime solids lightbulb quad globalUni objectUni shadowColorTex shadowDepthTex shaders@Shaders{..} = do
    closeRequested <- timeItInPlace "Rendering..." $ do
        Just (w, h) <- GLFW.getWindowSize win

        cfg <- liftIO $ updateUniforms startTime
        writeBuffer globalUni 0 [cfg]

        -- Clear color and depth of the shadow map.
        render $ do
            shadowColor <- getTexture2DImage shadowColorTex 0
            shadowDepth <- getTexture2DImage shadowDepthTex 0

            clearImageColor shadowColor 0
            clearImageDepth shadowDepth 1

        -- Render each object on the shadow map.
        forM_ [objBed solids, objGround solids] $ \(solid, pos) -> do
            writeBuffer objectUni 0 [ObjectUniforms pos]
            render $ do
                shadowColor <- getTexture2DImage shadowColorTex 0
                shadowDepth <- getTexture2DImage shadowDepthTex 0

                prim <- fmap (toPrimitiveArray TriangleList) . newVertexArray $ solid
                shadowShader ShadowShader.Env
                    { ShadowShader.envPrimitives = prim
                    , ShadowShader.envShadowColor = shadowColor
                    , ShadowShader.envShadowDepth = shadowDepth
                    }

        -- Clear the window frame buffer.
        render $ do
            clearWindowColor win (V3 0.7 0.7 0.7)
            clearWindowDepth win 1

        -- Render each object on the window frame buffer.
        forM_ [objBed solids, objGround solids] $ \(solid, pos) -> do
            writeBuffer objectUni 0 [ObjectUniforms pos]
            render $ do
                solidsPrim <- fmap (toPrimitiveArray TriangleList) . newVertexArray $ solid
                solidsShader SolidsShader.Env
                    { SolidsShader.envScreenSize = V2 w h
                    , SolidsShader.envPrimitives = solidsPrim
                    }
                -- wireframeShader prim

                quadPrim <- toPrimitiveArray TriangleList <$> newVertexArray quad
                quadShader QuadShader.Env
                    { QuadShader.envScreenSize = V2 w h
                    , QuadShader.envPrimitives = quadPrim
                    }

        render $ do
            prim <- fmap (toPrimitiveArray TriangleList) . newVertexArray $ lightbulb
            let env = BulbShader.Env
                    { BulbShader.envScreenSize = V2 w h
                    , BulbShader.envPrimitives = prim
                    }
            bulbShader env
            bulbWireframeShader env

        swapWindowBuffers win

        GLFW.windowShouldClose win

    liftIO $ threadDelay 10000
    unless (closeRequested == Just True) $
        loop win startTime solids lightbulb quad globalUni objectUni shadowColorTex shadowDepthTex shaders
