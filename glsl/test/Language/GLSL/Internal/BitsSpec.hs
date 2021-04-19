{-# LANGUAGE OverloadedStrings #-}
module Language.GLSL.Internal.BitsSpec where

import           Test.Hspec                  (Spec, describe, it, shouldBe)

import           Language.GLSL.Internal.Bits (B (I, O), flat, unflat)

spec :: Spec
spec = do
  describe "flat" $ do
    it "should preserve the order of the input" $
      flat ((O,I),I,(I,O,O,I))
      `shouldBe`
      [O,I,I,I,O,O,I]

  describe "unflat" $ do
    it "should preserve the order of the input and consume all input" $
      unflat [O,I,I,I,O,O,I]
      `shouldBe`
      Just (((O,I),I,(I,O,O,I)),[])
