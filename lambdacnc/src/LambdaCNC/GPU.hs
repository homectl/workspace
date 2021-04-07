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

import           Control.Applicative               (liftA2)
import           Control.Concurrent.MVar           (MVar)
import qualified Control.Concurrent.MVar           as MVar
import           Control.Lens                      ((^.))
import           Control.Lens.Indexed              (iforM_)
import           Control.Monad                     (forM, forM_)
import           Control.Monad.IO.Class            (liftIO)
import           Data.Foldable                     (toList)
import qualified Data.Time.Clock                   as Time
import           Data.Word                         (Word32)
import           Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW       as GLFW
import qualified Graphics.GPipe.Context.GLFW.Input as Input
import qualified Graphics.GPipe.Engine             as Engine
import qualified Graphics.GPipe.Engine.STL         as STL
import           Graphics.GPipe.Engine.TimeIt      (timeIt)
import           LambdaCNC.Config                  (GlobalUniformBuffer,
                                                    GlobalUniforms (..),
                                                    LightUniforms (..),
                                                    ObjectUniformBuffer,
                                                    ObjectUniforms (..),
                                                    Solids (..),
                                                    defaultGlobalUniforms,
                                                    defaultObjectUniforms)
import qualified LambdaCNC.Shaders.Bulb            as BulbShader
import qualified LambdaCNC.Shaders.Common          as Shaders
import           LambdaCNC.Shaders.LightInfo       (LightInfo (..))
import qualified LambdaCNC.Shaders.LightInfo       as LightInfo
import qualified LambdaCNC.Shaders.Quad            as QuadShader
import qualified LambdaCNC.Shaders.Shadow          as ShadowShader
import qualified LambdaCNC.Shaders.Solids          as SolidsShader
import           Prelude                           hiding ((<*))
import qualified System.Environment                as Env


fps :: Double
fps = 24

windowSize :: V2 Int
windowSize = V2 1500 800


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

startPos :: MachinePosition Int
startPos = fmap (`div` 2) machMax

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
                [ GLFW.WindowHint'Samples (Just 8)
                ]
            }

        solids <- timeIt "Loading object meshes" $ do
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

        lightbulb <- timeIt "Loading lightbulb mesh" $ do
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

        tex <- newTexture2D R8 (V2 8 8) 1
        let whiteBlack = cycle [minBound + maxBound `div` 4,maxBound - maxBound `div` 4] :: [Word32]
            blackWhite = tail whiteBlack
        writeTexture2D tex 0 0 (V2 8 8) (cycle (take 8 whiteBlack ++ take 8 blackWhite))

        let lights = LightInfo.fromList
                [ LightUniforms (V3 (-60000) (-60000) 30000) (V3 0.7 0.0 0.0)
                , LightUniforms (V3 (-60000)   60000  30000) (V3 0.0 0.7 0.0)
                , LightUniforms (V3   60000  (-60000) 30000) (V3 0.0 0.0 0.7)
                , LightUniforms (V3   60000    60000  30000) (V3 0.3 0.3 0.0)
                , LightUniforms (V3   60000        0  30000) (V3 0.0 0.3 0.3)
                , LightUniforms (V3       0    60000  30000) (V3 0.3 0.0 0.3)
                , LightUniforms (V3 (-60000)       0  30000) (V3 0.3 0.3 0.3)
                , LightUniforms (V3       0  (-60000) 30000) (V3 0.8 0.5 0.3)
                ]
        shadowMaps <- sequence $ (`fmap` lights) $ const $
            Shaders.ShadowMap
                <$> newTexture2D R16F Shaders.shadowMapSize 1
                <*> newTexture2D Depth16 Shaders.shadowMapSize 1

        globalUni <- newBuffer 1
        writeBuffer globalUni 0 [defaultGlobalUniforms]

        objectUni <- newBuffer 1
        writeBuffer objectUni 0 [defaultObjectUniforms]

        lightUni <- newBuffer $ length lights
        writeBuffer lightUni 0 $ toList lights

        let shadowTextures = fmap Shaders.shadowColorTex shadowMaps

        shaders <- Shaders
            <$> timeIt "Compiling shadow shader..." (ShadowShader.solidShader globalUni objectUni lightUni)
            <*> timeIt "Compiling solids shader..." (SolidsShader.solidShader globalUni objectUni lightUni shadowTextures tex win)
            <*> timeIt "Compiling wireframe shader..." (SolidsShader.wireframeShader globalUni objectUni win)
            <*> timeIt "Compiling shadow map view shader..." (QuadShader.solidShader objectUni win)
            <*> timeIt "Compiling lightbulb shader..." (BulbShader.solidShader globalUni lightUni win)
            <*> timeIt "Compiling lightbulb wireframe shader..." (BulbShader.wireframeShader globalUni lightUni win)

        mvState <- liftIO $ MVar.newMVar startPos
        setupInput win mvState

        startTime <- liftIO Time.getCurrentTime
        Engine.mainloop win renderings (startTime, mvState, solids, lightbulb, quad, globalUni, objectUni, shadowMaps, shaders)


setupInput :: Window os c ds -> MVar (MachinePosition Int) -> ContextT GLFW.Handle os IO ()
setupInput win mvState = do
    _ <- Input.setKeyCallback win $ Just keyCallback
    return ()
  where
    keyCallback k _ Input.KeyState'Pressed _   = keyPressed k
    keyCallback k _ Input.KeyState'Repeating _ = keyPressed k
    keyCallback _ _ Input.KeyState'Released _  = return ()

    MachinePosition xMax yMax zMax = machMax

    keyPressed k =
        let fb = moveMult (k == Input.Key'Down) (k == Input.Key'Up)
            lr = moveMult (k == Input.Key'Left) (k == Input.Key'Right)
            ud = moveMult (k == Input.Key'PageDown) (k == Input.Key'PageUp)
        in
        MVar.modifyMVar_ mvState $ \pos@MachinePosition{..} -> return pos
            { xPos = max 0 . min xMax $ (xPos + (xMax `div` 50) * lr)
            , yPos = max 0 . min yMax $ (yPos + (yMax `div` 50) * fb)
            , zPos = max 0 . min zMax $ (zPos + (zMax `div` 50) * ud)
            }

    moveMult True _      = -1
    moveMult _ True      = 1
    moveMult False False = 0


updateUniforms :: Floating a => Time.UTCTime -> V2 Int -> IO (GlobalUniforms a)
updateUniforms startTime screenSize = do
    now <- Time.getCurrentTime
    return defaultGlobalUniforms
        { time = fromRational $ toRational $ Time.diffUTCTime now startTime
        , screenSize = fmap fromIntegral screenSize
        }


renderings
    :: (ContextHandler ctx)
    => Window os RGBFloat Depth
    -> V2 Int
    -> ( Time.UTCTime
       , MVar (MachinePosition Int)
       , Solids (Shaders.Buffer3D os)
       , Shaders.Buffer3D os
       , Shaders.Buffer2D os
       , GlobalUniformBuffer os
       , ObjectUniformBuffer os
       , LightInfo (Shaders.ShadowMap os)
       , Shaders os)
    -> ContextT ctx os IO ()
renderings win envScreenSize (startTime, mvState, solids, lightbulb, quad, globalUni, objectUni, shadowMaps, Shaders{..}) = do
    state <- liftIO $ MVar.readMVar mvState

    let solidsWithPos = liftA2 (,) solids (objectPositions state)

    cfg <- liftIO $ updateUniforms startTime envScreenSize
    writeBuffer globalUni 0 [cfg]

    iforM_ shadowMaps $ \envIndex Shaders.ShadowMap{..} -> do
        -- Clear color and depth of the shadow map.
        render $ do
            shadowColor <- getTexture2DImage shadowColorTex 0
            shadowDepth <- getTexture2DImage shadowDepthTex 0

            clearImageColor shadowColor 0
            clearImageDepth shadowDepth 1

        -- Render each object on the shadow map.
        forM_ solidsWithPos $ \(solid, pos) -> do
            writeBuffer objectUni 0 [ObjectUniforms pos]
            render $ do
                envShadowColor <- getTexture2DImage shadowColorTex 0
                envShadowDepth <- getTexture2DImage shadowDepthTex 0

                envPrimitives <- fmap (toPrimitiveArray TriangleList) . newVertexArray $ solid
                shadowShader ShadowShader.Env{..}

    -- Clear the window frame buffer.
    render $ do
        clearWindowColor win (V3 0.7 0.7 0.7)
        clearWindowDepth win 1

    -- Render each object on the window frame buffer.
    forM_ solidsWithPos $ \(solid, pos) -> do
        writeBuffer objectUni 0 [ObjectUniforms pos]
        render $ do
            envPrimitives <- fmap (toPrimitiveArray TriangleList) . newVertexArray $ solid
            let env = SolidsShader.Env{..}
            solidsShader env
            -- wireframeShader env

    iforM_ shadowMaps $ \envIndex Shaders.ShadowMap{..} -> do
        writeBuffer objectUni 0 [ObjectUniforms (V3 (1/4 * fromIntegral envIndex) 0 0)]
        render $ do
            envPrimitives <- toPrimitiveArray TriangleList <$> newVertexArray quad
            let envTexture = shadowColorTex
            quadShader QuadShader.Env{..}

    -- Render the light bulbs.
    iforM_ shadowMaps $ \envIndex _ -> render $ do
        envPrimitives <- fmap (toPrimitiveArray TriangleList) . newVertexArray $ lightbulb
        let env = BulbShader.Env{..}
        bulbShader env
        bulbWireframeShader env
