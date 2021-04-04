{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module LambdaRay.GPU (main) where

import qualified Codec.Picture                     as Pic
import qualified Codec.Picture.Types               as Pic
import           Control.Concurrent                (threadDelay)
import           Control.Concurrent.MVar
import           Control.Monad                     (unless)
import           Control.Monad.IO.Class            (MonadIO, liftIO)
import qualified Data.Time.Clock                   as Time
import           Graphics.GPipe
import qualified System.Environment as Env
import qualified Graphics.GPipe.Context.GLFW       as GLFW
import qualified Graphics.GPipe.Context.GLFW.Input as Input
import           LambdaRay.Config                  (Config (..), DiskMode (..),
                                                    RuntimeConfig (..),
                                                    SkyMode (..), defaultConfig,
                                                    defaultRuntimeConfig)
import qualified LambdaRay.Schwarzschild           as Schwarzschild
import           Prelude                           hiding ((<*))
import           System.IO                         (hFlush, stdout)

viewPort :: V2 Int
viewPort = V2 1500 1000


renderSchwarzschild win (uniformBuffer :: Buffer os (Uniform (B Float, B Float, B Float))) diskTex skyTex = compileShader $ do
    (time, stepfactor, stepsize) <- getUniform (const (uniformBuffer, 0))
    primitiveStream <- toPrimitiveStream id
    fragmentStream <- rasterize (const (FrontAndBack, ViewPort (V2 0 0) viewPort, DepthRange 0 1)) primitiveStream

    let filterMode = SamplerFilter Linear Linear Linear (Just 4)
    diskSamp <- newSampler2D (const (diskTex, filterMode, (pure Repeat, undefined)))
    skySamp <- newSampler2D (const (skyTex, filterMode, (pure Repeat, undefined)))

    let cfg = defaultConfig
                { diskMode = DiskTexture (sample2D diskSamp SampleAuto Nothing Nothing)
                , skyMode = SkyTexture (sample2D skySamp SampleAuto Nothing Nothing)
                }
        rt = defaultRuntimeConfig{stepsize, stepfactor, time}
        result = Schwarzschild.frag cfg viewPort rt <$> fragmentStream

    drawWindowColor (const (win, ContextColorOption NoBlending (V3 True True True))) result


main :: IO ()
main = do
    Env.setEnv "GPIPE_DEBUG" "1"
    runContextT GLFW.defaultHandleConfig $ do
      let V2 w h = viewPort
      win <- newWindow (WindowFormatColor RGB8) $ (GLFW.defaultWindowConfig "LambdaRay")
          { GLFW.configWidth = w
          , GLFW.configHeight = h
          , GLFW.configHints = [GLFW.WindowHint'Samples (Just 4)]
          }

      vertexBuffer :: Buffer os (B4 Float, B2 Float) <- newBuffer 6
      writeBuffer vertexBuffer 0 [ (V4 (-1) (-1) 0 1, V2 0 0)
                                 , (V4 (-1) 1 0 1, V2 0 1)
                                 , (V4 1 1 0 1, V2 1 1)
                                 , (V4 (-1) (-1) 0 1, V2 0 0)
                                 , (V4 1 (-1) 0 1, V2 1 0)
                                 , (V4 1 1 0 1, V2 1 1)
                                 ]

      uniformBuffer <- newBuffer 1
      writeBuffer uniformBuffer 0 [(0, 0.9884, 0.58)]

      mvStepfactor <- liftIO $ newMVar 1
      mvStepsize <- liftIO $ newMVar 0.4

      _ <- Input.setKeyCallback win $ Just $ \k i s m -> do
        putStrLn $ "Key: " ++ show k ++ " " ++ show i ++ " " ++ show s
        case (k, s) of
          (Input.Key'A, Input.KeyState'Pressed) ->
            modifyMVar_ mvStepfactor $ \sf -> do
              let newSf = sf + 0.0001
              putStrLn $ "stepfactor: " ++ show newSf
              return newSf
          (Input.Key'Z, Input.KeyState'Pressed) ->
            modifyMVar_ mvStepfactor $ \sf -> do
              let newSf = sf - 0.0001
              putStrLn $ "stepfactor: " ++ show newSf
              return newSf
          (Input.Key'S, Input.KeyState'Pressed) ->
            modifyMVar_ mvStepsize $ \ss -> do
              let newSs = ss + 0.01
              putStrLn $ "stepsize: " ++ show newSs
              return newSs
          (Input.Key'X, Input.KeyState'Pressed) ->
            modifyMVar_ mvStepsize $ \ss -> do
              let newSs = ss - 0.01
              putStrLn $ "stepsize: " ++ show newSs
              return newSs
          _ -> return ()

      -- Load image into texture
      diskTex <- loadTexture "disk"
      skyTex <- loadTexture "sky"

      shader <- timeIt "Compiling..." $ renderSchwarzschild win uniformBuffer diskTex skyTex

      startTime <- liftIO Time.getCurrentTime
      loop startTime vertexBuffer uniformBuffer mvStepfactor mvStepsize shader win
  where
    loadTexture name =
      let file = "data/textures/" ++ name ++ ".jpg" in do
        image <- timeIt ("Loading texture: " ++ file) (liftIO $ Pic.readImage file) >>= \case
          Left err                    -> error err
          Right (Pic.ImageYCbCr8 img) -> return img
          Right _                     -> error "Wrong pixel format"
        let w = Pic.imageWidth image
            h = Pic.imageHeight image
            size = V2 w h
        pixels <- timeIt "=> Converting pixel format" $
          pure $ Pic.pixelFold getJuicyPixel [] image
        timeIt "=> Uploading to GPU" $ do
          tex <- newTexture2D SRGB8 size maxBound -- JPG converts to SRGB
          writeTexture2D tex 0 0 size pixels
          return tex


getJuicyPixel :: [V3 Pic.Pixel8] -> Int -> Int -> Pic.PixelYCbCr8 -> [V3 Pic.Pixel8]
getJuicyPixel xs _x _y pix =
  let Pic.PixelRGB8 r g b = Pic.convertPixel pix in V3 r g b : xs


loop startTime vertexBuffer (uniformBuffer :: Buffer os (Uniform (B Float, B Float, B Float))) mvStepfactor mvStepsize shader win = do
    closeRequested <- timeIt "Rendering..." $ do
      now <- liftIO Time.getCurrentTime
      stepfactor <- liftIO $ readMVar mvStepfactor
      stepsize <- liftIO $ readMVar mvStepsize
      writeBuffer uniformBuffer 0 [(fromRational $ toRational $ Time.diffUTCTime now startTime, stepfactor, stepsize)]

      render $ do
        clearWindowColor win (V3 0 0 0.8)
        vertexArray <- newVertexArray vertexBuffer
        let primitiveArray = toPrimitiveArray TriangleList vertexArray
        shader primitiveArray
      swapWindowBuffers win

      GLFW.windowShouldClose win

    liftIO $ threadDelay 10000
    unless (closeRequested == Just True) $
      loop startTime vertexBuffer uniformBuffer mvStepfactor mvStepsize shader win


timeIt :: (Info a, MonadIO m) => String -> m a -> m a
timeIt text m = do
    liftIO $ putStr ("[" ++ show Running ++ "] " ++ text) >> hFlush stdout
    s <- liftIO Time.getCurrentTime
    (r, (status, info)) <- getInfo <$> m
    liftIO $ putStr (" " ++ info ++ "\t") >> hFlush stdout
    e <- liftIO Time.getCurrentTime
    liftIO $ putStr $ show $ Time.nominalDiffTimeToSeconds $ Time.diffUTCTime e s
    liftIO $ putStrLn ("\r[" ++ show status)
    return r


data Status
    = Fail
    | Done
    | Running

instance Show Status where
    show Fail    = "\027[1;31mFAIL\027[0m"
    show Done    = "\027[1;32mDONE\027[0m"
    show Running = "\027[1;33m....\027[0m"

class Info a where
    getInfo :: a -> (a, (Status, String))

instance Info (Maybe a) where
    getInfo r@Nothing = (r, (Done, ""))
    getInfo r@Just{}  = (r, (Done, "OK"))

instance Info [a] where
    getInfo r = (r, (Done, "(" ++ show (length r) ++ ")"))

instance Info b => Info (Either a b) where
    getInfo r@Left{}     = (r, (Fail, ""))
    getInfo r@(Right ok) = (r, snd $ getInfo ok)

instance Info Pic.DynamicImage where
    getInfo r@(Pic.ImageYCbCr8 img) = (r, (Done, "(" ++ show w ++ "x" ++ show h ++ ")"))
      where
        w = Pic.imageWidth img
        h = Pic.imageHeight img
    getInfo r = (r, (Done, ""))

instance Info (Texture2D a b) where
    getInfo r = (r, (Done, ""))

instance Info (a -> b) where
    getInfo r = (r, (Done, ""))
