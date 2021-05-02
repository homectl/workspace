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

  describe "resizeBuffer" $ do
    it "from 0 to 100k, should return the new size of the buffer" $ do
      l <- runContextT GLFW.defaultHandleConfig $ do
        buf :: Buffer os (B Float) <- newBuffer 0
        resized <- resizeBuffer buf 100000
        writeBuffer resized 0 [1..100000]
        return $ bufferLength resized
      l `shouldBe` 100000

    it "from 10 to 1, shouldn't crash on OOB write" $ do
      l <- runContextT GLFW.defaultHandleConfig $ do
        buf :: Buffer os (B Float) <- newBuffer 10
        resized <- resizeBuffer buf 1
        writeBuffer resized 0 [0..1]
        return $ bufferLength resized
      l `shouldBe` 1
