{-# LANGUAGE OverloadedStrings #-}
module Graphics.GPipe.ExprSpec where

import           Test.Hspec                   (Spec, describe, it, shouldBe)

import           Data.Text                    as T (unlines)
import           Graphics.GPipe.Internal.Expr


wrap :: FFloat -> IO ExprResult
wrap expr = runExprM (tellGlobal "") $ unS expr >>= tellAssignment' "result"


spec :: Spec
spec = do
    describe "expressions" $ do
        it "should be able to add two numbers" $ do
            let a = 1
                b = 2
                c = a + b
            res <- wrap c
            source res `shouldBe` T.unlines
                [ "#version 330"
                , "void main() {"
                , "float t0 = (1+2);"
                , "result = t0;"
                , "}"
                ]
