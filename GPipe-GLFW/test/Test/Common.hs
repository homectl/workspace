{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TypeFamilies     #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Test.Common where

import           Control.Monad.IO.Class      (liftIO)
import           Data.Traversable            (forM)
import           GHC.Float                   (double2Float)
import qualified System.Environment          as Env

import           Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW (getTime)
import           Test.Control                (Controller (..))

xAxis :: [(V3 Float, V3 Float)]
xAxis = zip (repeat $ V3 1 0 0) -- red
    [V3 (-1) 0 0, V3 1 0 0, V3 0.8 (-0.1) 0, V3 0.8 0 0, V3 1 (-0.1) 0]

yAxis :: [(V3 Float, V3 Float)]
yAxis = zip (repeat $ V3 0 1 0) -- green
    [V3 0 (-1) 0, V3 0 1 0, V3 0 0.8 (-0.1), V3 0 0.9 (-0.05), V3 0 1 (-0.1)]

zAxis :: [(V3 Float, V3 Float)]
zAxis = zip (repeat $ V3 0 0 1) -- blue
    [V3 0 0 (-1), V3 0 0 1, V3 (-0.1) 0 0.8, V3 (-0.1) 0 1]

plane :: [(V3 Float, V3 Float)]
plane = zip (repeat $ V3 1 1 1) -- white
    [V3 (-2) (-0.1) (-2), V3 (-2) (-0.1) 2, V3 2 (-0.1) 2, V3 2 (-0.1) (-2), V3 (-2) (-0.1) (-2)]

data ShaderEnv os = ShaderEnv
    { extractProjU    :: (Buffer os (Uniform (V4 (B4 Float))), Int)
    , extractLinePA   :: PrimitiveArray Lines (B3 Float, B3 Float)
    , extractRastOpts :: (Side, PolygonMode, ViewPort, DepthRange)
    }

initBuffers win rawMeshes = do
    -- make mesh buffers
    meshesB <- forM rawMeshes $ \pts -> do
        buf <- newBuffer $ length pts
        writeBuffer buf 0 pts
        return buf
    -- make projection matrix buffer
    projMatB <- newBuffer 1
    return (meshesB, projMatB)

projectLines :: Shader os (ShaderEnv os) (FragmentStream (V3 FFloat, FFloat))
projectLines = do
    projMat <- getUniform extractProjU
    linePS <- toPrimitiveStream extractLinePA
    -- project points
    let projectedLinePS = (\(c, p) -> (projMat !* point p, c)) <$> linePS
    lineFS <- rasterize extractRastOpts projectedLinePS
    -- write fragment depths and return frags
    return $ withRasterizedInfo (\fr inf -> (fr, depth inf)) lineFS
    where
        depth RasterizedInfo {rasterizedFragCoord = (V4 _ _ z _)} = z

checkEnv :: IO ()
checkEnv = do
    val <- Env.lookupEnv "LIBGL_ALWAYS_SOFTWARE"
    let warn = case val of
            Nothing -> " !! If you don't have hardware support, expect a crash."
            _       -> ""
    putStrLn $ "LIBGL_ALWAYS_SOFTWARE: " ++ show val ++ warn

-- XXX: just adds the draw call to the end of the shader & glues some init functions together
initRenderContext win rawMeshes  = do
    liftIO checkEnv
    projShader <- compileShader (projectLines >>= drawWindowColorDepth (const (win, ContextColorOption NoBlending $ pure True, DepthOption Less True)))
    (meshesB, projMatB) <- initBuffers win rawMeshes
    return (meshesB, projMatB, projShader)

computeProjMat :: Float -> V2 Int -> M44 Float
computeProjMat frac (V2 w h) = camera2clip !*! world2camera !*! model2world
    where
        t = frac * 2 * pi
        model2world = identity
        world2camera = lookAt
            -- eye: camera position in world coordinates
            (V3 (3 * sin t) (0.5 + cos (t * 2)) (3 * cos t))
            (V3 0 0.1 0) -- center: camera look-at target position in world coords
            (V3 0 1 0) -- up: camera up-vector
        camera2clip = perspective
            (pi / 3) -- 60deg field of view "in y direction"
            (fromIntegral w / fromIntegral h) -- aspect ratio
            1 100 -- near and far clipping plane

renderStep
    :: Window os RGBFloat Depth
    -> V2 Int
    -> ( [Buffer os (B3 Float, B3 Float)]
       , Buffer os (Uniform (V4 (B4 Float)))
       , CompiledShader os (ShaderEnv os)
       )
    -> Render os ()
renderStep win size (meshesB, projMatB, projShader) = do
    clearWindowColor win 0.2 -- grey
    clearWindowDepth win 1 -- far plane
    meshPAs <- forM meshesB $ \mesh -> do
        meshVA <- newVertexArray mesh
        return $ toPrimitiveArray LineStrip meshVA
    projShader $ ShaderEnv (projMatB, 0) (mconcat meshPAs) (Front, PolygonFill, ViewPort 0 size, DepthRange 0 1)

runContext :: (ContextHandler ctx) => ContextHandlerParameters ctx -> (forall os. ContextT ctx os IO a) -> IO a
runContext = runContextT

continue :: Monad m => a -> m Bool
continue _ = return False

mainloop win controller resources@(_, projMatB, _) hook = do
    stop <- hook controller
    Just now <- liftIO GLFW.getTime
    if done now controller || stop
    then return ()
    else do
        -- compute a projection matrix & write it to the buffer
        size <- getFrameBufferSize win
        writeBuffer projMatB 0 [computeProjMat (double2Float $ frac now controller) size]
        -- render the scene and then loop
        render $ renderStep win size resources
        swapWindowBuffers win
        mainloop win (next now controller) resources hook
