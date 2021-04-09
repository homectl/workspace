{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Graphics.SceneGraph.BasicSpec where

import           Test.Hspec                (Expectation, Spec, describe, it,
                                            shouldBe, shouldNotBe,
                                            shouldSatisfy)
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck

import           Control.Lens              ((^.))
import           Graphics.SceneGraph.Basic
import           Linear                    (R1 (..), R2 (..), R3 (..), V3 (..),
                                            V4 (..), (!*))
import qualified Linear                    as L


instance Arbitrary a => Arbitrary (V3 a) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

infix 1 `shouldBeApproximately`

shouldBeApproximately :: Float -> Float -> Expectation
shouldBeApproximately a b = round a `shouldBe` round b


origin :: V4 Float
origin = V4 0 0 0 1


spec :: Spec
spec = do
  describe "visualise" $ do
    it "should produce a non-empty string" $ do
      let testScene = do
            translate camera (V3 1 0 0)
      dotText <- visualise <$> osg testScene
      dotText `shouldNotBe` ""


  describe "union" $ do
    prop "should be idempotent" $ \(box :: Box Float) ->
      union box box `shouldBe` box

    it "should yield a superset of the two input boxes" $ do
      let a = (V3 (-2) (-2) (-2), V3 2 2 2)
          b = (V3 (-1) (-1) (-1), V3 3 3 3)
      union a b `shouldBe` (V3 (-2) (-2) (-2), V3 3 3 3)


  describe "translate" $ do
    prop "should move a point linearly" $ \p -> do
      testScene <- osg $ do
            translate camera p
      let camNode = findCamera testScene 0
      getTransformTo testScene camNode !* origin `shouldBe` L.point p


  describe "rotateX" $ do
    prop "should not modify the X coordinate" $ \p theta -> do
      testScene <- osg $ do
            rotateX (translate camera p) theta
      let camNode = findCamera testScene 0
      (getTransformTo testScene camNode !* origin) ^. _x
        `shouldBeApproximately` p ^. _x

    it "should turn a Y unit vector into a Z unit vector" $ do
      testScene <- osg $ do
            rotateX (translate camera $ V3 0 1 0) 90
      let camNode = findCamera testScene 0
      -- TODO: the rotation matrix is super imprecise, this rounding shouldn't
      -- be necessary.
      fmap round ((getTransformTo testScene camNode !* origin) ^. _xyz)
        `shouldBe` V3 0 0 1


  describe "rotateY" $ do
    prop "should not modify the Y coordinate" $ \p theta -> do
      testScene <- osg $ do
            rotateY (translate camera p) theta
      let camNode = findCamera testScene 0
      (getTransformTo testScene camNode !* origin) ^. _y
        `shouldBeApproximately` p ^. _y

    it "should turn an X unit vector into a Z unit vector" $ do
      testScene <- osg $ do
            rotateY (translate camera $ V3 1 0 0) 90
      let camNode = findCamera testScene 0
      -- TODO: the rotation matrix is super imprecise, this rounding shouldn't
      -- be necessary.
      fmap round ((getTransformTo testScene camNode !* origin) ^. _xyz)
        `shouldBe` V3 0 0 (-1)


  describe "rotateZ" $ do
    prop "should not modify the Z coordinate" $ \p theta -> do
      testScene <- osg $ do
            rotateZ (translate camera p) theta
      let camNode = findCamera testScene 0
      (getTransformTo testScene camNode !* origin) ^. _z
        `shouldBeApproximately` p ^. _z

    it "should turn an X unit vector into a Y unit vector" $ do
      testScene <- osg $ do
            rotateZ (translate camera $ V3 1 0 0) 90
      let camNode = findCamera testScene 0
      -- TODO: the rotation matrix is super imprecise, this rounding shouldn't
      -- be necessary.
      fmap round ((getTransformTo testScene camNode !* origin) ^. _xyz)
        `shouldBe` V3 0 1 0


  describe "bounds" $ do
    it "should yield the a bounding box that's at least as large as the items within" $ do
      testScene <- osg $ do
            makeGroup =<< sequence
              [ translate camera (V3 2 0 0)
              , translate camera (V3 1 0 3)
              , translate camera (V3 0 (-4) 0)
              ]

      let sz = boxSize (bounds testScene)
      sz `shouldSatisfy` (> V3 2 4 3)
      sz `shouldSatisfy` (< V3 3 5 4)
