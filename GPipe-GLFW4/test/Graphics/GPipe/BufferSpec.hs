{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Graphics.GPipe.BufferSpec where

import           Test.Hspec                  (Spec, describe, it, shouldBe)

import           Graphics.GPipe.Buffer

import           Graphics.GPipe.Context      (runContextT)
import qualified Graphics.GPipe.Context.GLFW as GLFW


spec :: Spec
spec = do
  describe "bufferLength" $ do
    it "should return the initial size of the buffer" $ do
      l <- runContextT GLFW.defaultHandleConfig $ do
        buf :: Buffer os (B Float) <- newBuffer 100000
        writeBuffer buf 0 [0..50000]
        return $ bufferLength buf
      l `shouldBe` 100000
