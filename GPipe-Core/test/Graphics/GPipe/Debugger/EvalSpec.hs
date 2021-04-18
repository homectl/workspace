{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.GPipe.Debugger.EvalSpec where

import           Test.Hspec                      (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck           (prop)

import qualified Data.Text.Lazy                  as LT
import qualified Graphics.GPipe.Debugger.Compile as Compile
import qualified Graphics.GPipe.Debugger.Eval    as Eval
import           Graphics.GPipe.Debugger.Value   (Value (..))
import           Graphics.GPipe.Expr             (FFloat)
import           Graphics.GPipe.Linear           (V3 (..), V4 (..), identity,
                                                  norm, signorm, (!*))

compile :: FFloat -> IO LT.Text
compile = Compile.compile

compileV4 :: V4 FFloat -> IO LT.Text
compileV4 = Compile.compile

eval :: LT.Text -> Either String Value
eval = Eval.eval

ffloat :: Float -> FFloat
ffloat = fromRational . toRational


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

    describe "norm" $ do
        it "should return the cartesian length of a vector" $ do
            let a = V4 2 4 8 4
                r = norm a
            code <- compile r
            let res = eval code
            res `shouldBe` Right (FloatValue 10)

    describe "signorm" $ do
        prop "should return a vector of length 1" $ \case
            (0, 0, 0) -> return ()
            (x, y, z) -> do
                let r = norm . signorm $ V3 (ffloat x) (ffloat y) (ffloat z)
                code <- compile r
                let res = eval code
                res `shouldBe` Right (FloatValue 1)

    describe "!*" $ do
        prop "should work on the identity matrix" $ \case
            (0, 0, 0) -> return ()
            (x, y, z) -> do
                let m = identity
                    v = V4 (ffloat x) (ffloat y) (ffloat z) 1
                    r = m !* v
                code <- compileV4 r
                let res = eval code
                res `shouldBe` Right (Vec4Value (V4 x y z 1))
