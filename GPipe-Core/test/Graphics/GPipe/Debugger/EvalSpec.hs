{-# LANGUAGE OverloadedStrings #-}
module Graphics.GPipe.Debugger.EvalSpec where

import           Test.Hspec                   (Spec, describe, it, shouldBe)

import qualified Data.Text.Lazy               as LT
import           Graphics.GPipe.Debugger.Eval (Value (..))
import qualified Graphics.GPipe.Debugger.Eval as Eval
import           Graphics.GPipe.Expr          (FFloat)

compile :: FFloat -> IO LT.Text
compile = Eval.compile

eval :: LT.Text -> Either String Value
eval = Eval.eval


spec :: Spec
spec = do
    describe "(+)" $ do
        it "should be able to add two numbers" $ do
            let a = 1
                b = 2
                c = a + b
            code <- compile c
            let res = eval code
            res `shouldBe` Right (FloatValue 3)
