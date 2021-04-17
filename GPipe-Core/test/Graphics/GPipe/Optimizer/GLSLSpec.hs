{-# LANGUAGE OverloadedStrings #-}
module Graphics.GPipe.Optimizer.GLSLSpec where

import           Test.Hspec                       (Spec, describe, it)

import           Control.Applicative              (liftA2)
import           Data.Attoparsec.ByteString.Char8 (decimal)
import qualified Graphics.GPipe.Optimizer.GLSL    as GLSL
import           Test.GLSL                        (Transform, shouldTransformTo,
                                                   with)


newtype TestAnnot = TestAnnot Int

instance GLSL.Annot TestAnnot where
  parseAnnot = "// " >> TestAnnot <$> decimal >>= ("\n" >>) . pure
  ppAnnot (TestAnnot n) = Just $ GLSL.ppInt n


testAddAnnot :: [Int] -> Transform () TestAnnot
testAddAnnot = zipWith (fmap . setAnnot)
  where
    setAnnot i () = TestAnnot i


testJoinAnnot :: Transform () (TestAnnot, TestAnnot)
testJoinAnnot ss =
    let ss1 = testAddAnnot [0..3] ss
        ss2 = testAddAnnot (reverse [0..3]) ss
    in (zipWith . liftA2) (,) ss1 ss2


spec :: Spec
spec = do
    describe "fmap" $ do
        it "should add annotations" $
            [ "float t1 = 0;"
            , "float t2 = 0.0;"
            , "float t3 = (t2+1.1);"
            , "float t4 = (t3+vf1);"
            ] `with` testAddAnnot [0..3]
            `shouldTransformTo`
            [ "// 0"
            , "float t1 = 0;"
            , "// 1"
            , "float t2 = 0.0;"
            , "// 2"
            , "float t3 = (t2+1.1);"
            , "// 3"
            , "float t4 = (t3+vf1);"
            ]

    describe "liftA2" $ do
        it "should join annotations" $
            [ "float t1 = 0;"
            , "float t2 = 0.0;"
            , "float t3 = (t2+1.1);"
            , "float t4 = (t3+vf1);"
            ] `with` testJoinAnnot
            `shouldTransformTo`
            [ "// (0, 3)"
            , "float t1 = 0;"
            , "// (1, 2)"
            , "float t2 = 0.0;"
            , "// (2, 1)"
            , "float t3 = (t2+1.1);"
            , "// (3, 0)"
            , "float t4 = (t3+vf1);"
            ]
