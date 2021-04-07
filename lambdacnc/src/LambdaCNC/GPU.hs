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

import           Control.Concurrent.MVar           (MVar)
import qualified Control.Concurrent.MVar           as MVar
import           Control.Lens                      ((^.))
import           Control.Monad.IO.Class            (liftIO)
import qualified Data.Time.Clock                   as Time
import           Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW       as GLFW
import qualified Graphics.GPipe.Context.GLFW.Input as Input
import qualified Graphics.GPipe.Engine             as Engine
import qualified LambdaCNC.Pipeline                as Pipeline
import           Prelude                           hiding ((<*))
import qualified System.Directory                  as Dir
import qualified System.Environment                as Env
import           System.FilePath                   ((</>))


fps :: Double
fps = 24

windowSize :: V2 Int
windowSize = V2 1500 800


cleanupShaders :: IO ()
cleanupShaders = do
    let prefix = "generated-shaders"
    files <- map (prefix </>) . filter (/= "README.md") <$> Dir.listDirectory prefix
    mapM_ Dir.removeFile files


main :: IO ()
main = do
    Env.setEnv "GPIPE_DEBUG" "1"
    cleanupShaders

    runContextT GLFW.defaultHandleConfig{GLFW.configEventPolicy = Just $ GLFW.WaitTimeout $ 1 / fps } $ do
        win <- newWindow (WindowFormatColorDepth RGB8 Depth16) $ (GLFW.defaultWindowConfig "LambdaCNC (GPipe)")
            { GLFW.configWidth = windowSize^._x
            , GLFW.configHeight = windowSize^._y
            , GLFW.configHints =
                [ GLFW.WindowHint'Samples (Just 8)
                ]
            }

        pipelineData <- Pipeline.initialise win

        mvState <- liftIO $ MVar.newMVar Pipeline.startPos
        setupInput win mvState

        startTime <- liftIO Time.getCurrentTime
        Engine.mainloop win False Pipeline.renderings (startTime, mvState, pipelineData)


setupInput :: Window os c ds -> MVar (Pipeline.MachinePosition Int) -> ContextT GLFW.Handle os IO ()
setupInput win mvState = do
    _ <- Input.setKeyCallback win $ Just keyCallback
    return ()
  where
    keyCallback k _ Input.KeyState'Pressed _   = keyPressed k
    keyCallback k _ Input.KeyState'Repeating _ = keyPressed k
    keyCallback _ _ Input.KeyState'Released _  = return ()

    Pipeline.MachinePosition xMax yMax zMax = Pipeline.machMax

    keyPressed k =
        let fb = moveMult (k == Input.Key'Down) (k == Input.Key'Up)
            lr = moveMult (k == Input.Key'Left) (k == Input.Key'Right)
            ud = moveMult (k == Input.Key'PageDown) (k == Input.Key'PageUp)
        in
        MVar.modifyMVar_ mvState $ \pos@Pipeline.MachinePosition{..} -> return pos
            { Pipeline.xPos = max 0 . min xMax $ (xPos + (xMax `div` 50) * lr)
            , Pipeline.yPos = max 0 . min yMax $ (yPos + (yMax `div` 50) * fb)
            , Pipeline.zPos = max 0 . min zMax $ (zPos + (zMax `div` 50) * ud)
            }

    moveMult True _      = -1
    moveMult _ True      = 1
    moveMult False False = 0
