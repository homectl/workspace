{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Graphics.SceneGraph.BoundingBoxSpec where


import           Test.Hspec                      (Spec, describe, it, shouldBe,
                                                  shouldSatisfy)
import           Test.Hspec.QuickCheck           (prop)
import           Test.QuickCheck                 (Arbitrary (arbitrary))

import           Graphics.SceneGraph.Basic
import           Graphics.SceneGraph.BoundingBox
import           Linear                          (V3 (..))


instance Arbitrary a => Arbitrary (V3 a) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary


spec :: Spec
spec = do
  describe "union" $ do
    prop "should be idempotent" $ \(box :: Box Float) ->
      union box box `shouldBe` box

    it "should yield a superset of the two input boxes" $ do
      let a = (V3 (-2) (-2) (-2), V3 2 2 2)
          b = (V3 (-1) (-1) (-1), V3 3 3 3)
      union a b `shouldBe` (V3 (-2) (-2) (-2), V3 3 3 3)


  describe "bounds" $ do
    it "should yield the a bounding box that's at least as large as the items within" $ do
      testScene <- osg $ do
            group =<< sequence
              [ translate (V3 2 0 0) camera
              , translate (V3 1 0 3) camera
              , translate (V3 0 (-4) 0) camera
              ]

      let sz = boxSize (bounds testScene)
      sz `shouldSatisfy` (> V3 2 4 3)
      sz `shouldSatisfy` (< V3 3 5 4)
