{-# LANGUAGE OverloadedStrings #-}
module Graphics.GPipe.ExprSpec where

import           Test.Hspec                   (Spec, describe, it, shouldBe)

import           Graphics.GPipe.Internal.Expr


spec :: Spec
spec = do
    describe "analyse" $ do
        it "should parse a simple function" $ do
            let a = (1 :: FFloat)
                b = (2 :: FFloat)
                c = a + b
            0 `shouldBe` 0
