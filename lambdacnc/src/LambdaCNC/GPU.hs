{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE DeriveTraversable #-}
module LambdaCNC.GPU
  ( main
  ) where

import           Control.Concurrent          (threadDelay)
import           Control.Monad               (unless, forM_, forM)
import           Control.Monad.IO.Class      (liftIO)
import qualified Data.Time.Clock             as Time
import           Data.Word                   (Word32)
import           Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import           LambdaCNC.Config            (RuntimeConfig (..), UniformBuffer,
                                              defaultRuntimeConfig)
import qualified Graphics.GPipe.Engine.STL               as STL
import qualified LambdaCNC.Shaders           as Shaders
import           Graphics.GPipe.Engine.TimeIt (timeIt)
import qualified Graphics.GPipe.Engine.TimeIt as TimeIt
import           Prelude                     hiding ((<*))
import qualified System.Environment          as Env


fps :: Double
fps = 5

viewPort :: V2 Int
viewPort = V2 1500 1000


data Solids a = Solids
    { objBed :: a
    , objGround :: a
    , objXAxis :: a
    , objYAxis :: a
    , objZAxis :: a
    }
    deriving (Functor, Foldable, Traversable)

instance TimeIt.Info (Solids a) where
    getInfo r = (r, (TimeIt.Done, "(" ++ show (length r) ++ " solids)"))


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

        objVertexBuffer <- timeIt "Loading meshes" $ do
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

        quadVertexBuffer <- timeIt "Generating quad" $ do
            buf <- newBuffer 6
            writeBuffer buf 0 [ V2 (-1) (-1), V2 (-1)   1 , V2 1 1
                              , V2 (-1) (-1), V2   1  (-1), V2 1 1
                              ]
            return buf

        uniformBuffer <- newBuffer 1
        writeBuffer uniformBuffer 0 [defaultRuntimeConfig]

        tex <- newTexture2D R8 (V2 8 8) 1
        let whiteBlack = cycle [minBound + maxBound `div` 4,maxBound - maxBound `div` 4] :: [Word32]
            blackWhite = tail whiteBlack
        writeTexture2D tex 0 0 (V2 8 8) (cycle (take 8 whiteBlack ++ take 8 blackWhite))

        shadowColorTex <- newTexture2D R8 (V2 1000 1000) 1
        shadowDepthTex <- newTexture2D Depth16 (V2 1000 1000) 1

        shadowShader <- timeIt "Compiling shadow shader..." $ Shaders.compileShadowShader uniformBuffer
        solidsShader <- timeIt "Compiling solids shader..." $ Shaders.compileSolidsShader win viewPort uniformBuffer shadowColorTex tex
        wireframeShader <- timeIt "Compiling wireframe shader..." $ Shaders.compileWireframeShader win viewPort uniformBuffer
        shadowViewShader <- timeIt "Compiling shadow map view shader..." $ Shaders.compileQuadShader win viewPort shadowColorTex

        startTime <- liftIO Time.getCurrentTime
        loop win startTime objVertexBuffer quadVertexBuffer uniformBuffer shadowColorTex shadowDepthTex shadowShader solidsShader wireframeShader shadowViewShader


updateUniforms :: Floating a => Time.UTCTime -> IO (RuntimeConfig a)
updateUniforms startTime = do
    now <- Time.getCurrentTime
    return defaultRuntimeConfig{time = fromRational $ toRational $ Time.diffUTCTime now startTime}


loop
    :: Window os RGBFloat Depth
    -> Time.UTCTime
    -> Solids (Buffer os Shaders.ObjectShaderInput)
    -> Buffer os Shaders.QuadShaderInput
    -> UniformBuffer os
    -> Shaders.ShadowColorTex os
    -> Shaders.ShadowDepthTex os
    -> Shaders.ShadowShader os
    -> Shaders.SolidsShader os
    -> Shaders.SolidsShader os
    -> Shaders.QuadShader os
    -> ContextT GLFW.Handle os IO ()
loop win startTime objVertexBuffer quadVertexBuffer uniformBuffer shadowColorTex shadowDepthTex shadowShader solidsShader wireframeShader shadowViewShader = do
    closeRequested <- timeIt "Rendering..." $ do
        cfg <- liftIO $ updateUniforms startTime
        writeBuffer uniformBuffer 0 [cfg]

        render $ do
            objVertexArray <- mapM (fmap (toPrimitiveArray TriangleList) . newVertexArray) objVertexBuffer
            shadowColor <- getTexture2DImage shadowColorTex 0
            shadowDepth <- getTexture2DImage shadowDepthTex 0
            clearImageColor shadowColor 0
            clearImageDepth shadowDepth 1
            forM_ objVertexArray $ \prims -> shadowShader Shaders.ShadowShaderEnv
                { Shaders.envPrimitives = prims
                , Shaders.envShadowColor = shadowColor
                , Shaders.envShadowDepth = shadowDepth
                }

        render $ do
            clearWindowColor win (V3 0 0 0.8)
            clearWindowDepth win 1
            objVertexArray <- mapM (fmap (toPrimitiveArray TriangleList) . newVertexArray) objVertexBuffer
            quadVertexArray <- toPrimitiveArray TriangleList <$> newVertexArray quadVertexBuffer
            mapM_ solidsShader objVertexArray
            -- wireframeShader objVertexArray
            shadowViewShader quadVertexArray

        swapWindowBuffers win

        GLFW.windowShouldClose win

    liftIO $ threadDelay 10000
    unless (closeRequested == Just True) $
        loop win startTime objVertexBuffer quadVertexBuffer uniformBuffer shadowColorTex shadowDepthTex shadowShader solidsShader wireframeShader shadowViewShader
