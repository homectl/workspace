{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module LambdaCNC.Pipeline where

import           Control.Applicative               (liftA2)
import           Control.Lens.Indexed              (iforM_)
import           Control.Monad                     (forM, forM_)
import           Control.Monad.IO.Class            (liftIO)
import           Data.Foldable                     (toList)
import qualified Data.Time.Clock                   as Time
import           Data.Word                         (Word32)
import           Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW.Input as Input
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

--------------------------------------------------
--
-- Dynamic pipeline data, i.e. the mutable state
--
--------------------------------------------------

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


data FrameBuffer os = FrameBuffer
    { fbColor :: Shaders.ColorTex os
    , fbDepth :: Shaders.DepthTex os
    }

newtype FrameBuffers os = FrameBuffers
    { fbFinal :: FrameBuffer os
    }


initFrameBuffers
  :: ContextHandler ctx
  => V2 Int
  -> ContextT ctx os IO (FrameBuffers os)
initFrameBuffers windowSize = do
    FrameBuffers <$> makeFrameBuffer
  where
    makeFrameBuffer = FrameBuffer
        <$> newTexture2D RGB16F windowSize 1
        <*> newTexture2D Depth16 windowSize 1


data PipelineState os = PipelineState
    { stMachPos      :: MachinePosition Int
    , stWindowSize   :: V2 Int

    , stFrameBuffers :: Maybe (FrameBuffers os)
    }


initState
  :: ContextHandler ctx
  => V2 Int
  -> ContextT ctx os IO (PipelineState os)
initState stWindowSize = do
    updateState $ PipelineState
        { stMachPos = fmap (`div` 2) machMax
        , stFrameBuffers = Nothing
        , ..
        }


updateState
  :: ContextHandler ctx
  => PipelineState os
  -> ContextT ctx os IO (PipelineState os)
updateState state@PipelineState{stFrameBuffers=Nothing, stWindowSize} = do
    stFrameBuffers <- Just <$> initFrameBuffers stWindowSize
    return state{stFrameBuffers}
updateState state = return state

--------------------------------------------------
--
-- Static pipeline data: shaders, textures, etc.
--
--------------------------------------------------

data Shaders os = Shaders
    { shadowShader        :: ShadowShader.Compiled os
    , solidsShader        :: SolidsShader.Compiled os
    , wireframeShader     :: SolidsShader.Compiled os
    , quadShader          :: QuadShader.Compiled os RFloat
    , quadColorShader     :: QuadShader.Compiled os RGBFloat
    , bulbShader          :: BulbShader.Compiled os
    , bulbWireframeShader :: BulbShader.Compiled os
    }


data PipelineData os = PipelineData
    { startTime  :: Time.UTCTime
    , solids     :: Solids (Shaders.Buffer3D os)
    , lightbulb  :: Shaders.Buffer3D os
    , quad       :: Shaders.Buffer2D os
    , globalUni  :: GlobalUniformBuffer os
    , objectUni  :: ObjectUniformBuffer os
    , shadowMaps :: LightInfo (Shaders.ShadowMap os)
    , shaders    :: Shaders os
    }


initData
  :: ContextHandler ctx
  => Window os RGBFloat Depth
  -> ContextT ctx os IO (PipelineData os)
initData win = do
    startTime <- liftIO Time.getCurrentTime

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
        writeBuffer buf 0
            [ V2 (-1) (-1), V2 (-1)   1 , V2 1 1
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
            , LightUniforms (V3   80000        0  30000) (V3 0.0 0.3 0.3)
            , LightUniforms (V3       0    80000  30000) (V3 0.3 0.0 0.3)
            , LightUniforms (V3 (-80000)       0  30000) (V3 0.3 0.3 0.3)
            , LightUniforms (V3       0  (-80000) 30000) (V3 0.8 0.5 0.3)
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
        <*> timeIt "Compiling solids shader..." (SolidsShader.solidShader globalUni objectUni lightUni shadowTextures tex)
        <*> timeIt "Compiling wireframe shader..." (SolidsShader.wireframeShader globalUni objectUni)
        <*> timeIt "Compiling shadow map view shader..." (QuadShader.solidShader objectUni pure win)
        <*> timeIt "Compiling final frame shader..." (QuadShader.solidShader objectUni id win)
        <*> timeIt "Compiling lightbulb shader..." (BulbShader.solidShader globalUni lightUni)
        <*> timeIt "Compiling lightbulb wireframe shader..." (BulbShader.wireframeShader globalUni lightUni)

    return PipelineData{..}

--------------------------------------------------
--
-- Rendering pipeline
--
--------------------------------------------------

renderings
    :: ContextHandler ctx
    => Window os RGBFloat Depth
    -> PipelineData os
    -> PipelineState os
    -> ContextT ctx os IO ()
renderings _ _ PipelineState{stFrameBuffers=Nothing} =
    liftIO $ putStrLn "WARNING renderings: frame buffers not initialised"
renderings win PipelineData{shaders=Shaders{..}, ..} PipelineState{stFrameBuffers=Just FrameBuffers{..}, ..} = do
    let solidsWithPos = liftA2 (,) solids (objectPositions stMachPos)
    let envScreenSize = stWindowSize

    cfg <- liftIO $ updateUniforms startTime stWindowSize
    writeBuffer globalUni 0 [cfg]

    ------------------------ PRODUCE SHADOW MAPS ------------------------

    iforM_ shadowMaps $ \envIndex Shaders.ShadowMap{..} -> do
        -- Clear color and depth of the shadow map.
        render $ do
            shadowColor <- getTexture2DImage shadowColorTex 0
            shadowDepth <- getTexture2DImage shadowDepthTex 0

            clearImageColor shadowColor 0
            clearImageDepth shadowDepth 1

        -- Render each object on the shadow map.
        forM_ solidsWithPos $ \(solid, objectPos) -> do
            writeBuffer objectUni 0 [defaultObjectUniforms{objectPos}]
            render $ do
                envShadowColor <- getTexture2DImage shadowColorTex 0
                envShadowDepth <- getTexture2DImage shadowDepthTex 0

                envPrimitives <- fmap (toPrimitiveArray TriangleList) . newVertexArray $ solid
                shadowShader ShadowShader.Env{..}

    ------------------------ DRAW TO FRAME BUFFERS ------------------------

    -- Clear the final rendering texture.
    render $ do
        shadowColor <- getTexture2DImage (fbColor fbFinal) 0
        shadowDepth <- getTexture2DImage (fbDepth fbFinal) 0

        clearImageColor shadowColor 0.7
        clearImageDepth shadowDepth 1

    -- Render each object on the window frame buffer.
    forM_ solidsWithPos $ \(solid, objectPos) -> do
        writeBuffer objectUni 0 [defaultObjectUniforms{objectPos}]
        render $ do
            envColor <- getTexture2DImage (fbColor fbFinal) 0
            envDepth <- getTexture2DImage (fbDepth fbFinal) 0
            envPrimitives <- fmap (toPrimitiveArray TriangleList) . newVertexArray $ solid

            let env = SolidsShader.Env{..}
            solidsShader env
            -- wireframeShader env

    -- Render the light bulbs.
    iforM_ shadowMaps $ \envIndex _ -> render $ do
        envColor <- getTexture2DImage (fbColor fbFinal) 0
        envDepth <- getTexture2DImage (fbDepth fbFinal) 0
        envPrimitives <- fmap (toPrimitiveArray TriangleList) . newVertexArray $ lightbulb
        let env = BulbShader.Env{..}
        bulbShader env
        bulbWireframeShader env

    ------------------------ DRAW ON SCREEN ------------------------

    -- Clear the window frame buffer.
    render $ do
        clearWindowColor win 0
        clearWindowDepth win 1

    -- Paint the final image onto a full screen quad.
    writeBuffer objectUni 0 [ObjectUniforms{objectPos=V3 0 0 0, objectScale=1}]
    render $ do
        envPrimitives <- toPrimitiveArray TriangleList <$> newVertexArray quad
        let envTexture = fbColor fbFinal
        quadColorShader QuadShader.Env{..}

    -- Render the shadow maps as small pictures on quads at the bottom of the screen.
    iforM_ shadowMaps $ \i Shaders.ShadowMap{..} -> do
        writeBuffer objectUni 0 [ObjectUniforms{objectPos=V3 (1/5 * fromIntegral i) 0 0, objectScale=1/10}]
        render $ do
            envPrimitives <- toPrimitiveArray TriangleList <$> newVertexArray quad
            let envTexture = shadowColorTex
            quadShader QuadShader.Env{..}


updateUniforms :: Floating a => Time.UTCTime -> V2 Int -> IO (GlobalUniforms a)
updateUniforms startTime windowSize = do
    now <- Time.getCurrentTime
    return defaultGlobalUniforms
        { time = fromRational $ toRational $ Time.diffUTCTime now startTime
        , screenSize = fmap fromIntegral windowSize
        }

--------------------------------------------------
--
-- Event processing
--
--------------------------------------------------

keyCallback
  :: PipelineState os
  -> Input.Key
  -> Int
  -> Input.KeyState
  -> Input.ModifierKeys
  -> PipelineState os
keyCallback state@PipelineState{stMachPos=pos@MachinePosition{..}} = process
  where
    process k _ Input.KeyState'Pressed _   = keyPressed k
    process k _ Input.KeyState'Repeating _ = keyPressed k
    process _ _ Input.KeyState'Released _  = state

    MachinePosition xMax yMax zMax = machMax

    keyPressed k =
        let fb = moveMult (k == Input.Key'Down) (k == Input.Key'Up)
            lr = moveMult (k == Input.Key'Left) (k == Input.Key'Right)
            ud = moveMult (k == Input.Key'PageDown) (k == Input.Key'PageUp)
        in
        state { stMachPos = pos
            { xPos = max 0 . min xMax $ (xPos + (xMax `div` 50) * lr)
            , yPos = max 0 . min yMax $ (yPos + (yMax `div` 50) * fb)
            , zPos = max 0 . min zMax $ (zPos + (zMax `div` 50) * ud)
            } }

    moveMult True _      = -1
    moveMult _ True      = 1
    moveMult False False = 0


windowSizeCallback
  :: PipelineState os
  -> Int -> Int
  -> PipelineState os
windowSizeCallback state w h = state
    { stWindowSize = V2 w h
    , stFrameBuffers = Nothing  -- Reinitialise frame buffers on next rendering.
    }
