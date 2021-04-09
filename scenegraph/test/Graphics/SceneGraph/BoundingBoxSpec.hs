{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Graphics.SceneGraph.BoundingBoxSpec where

import           Test.Hspec                      (Expectation, Spec, describe,
                                                  it, shouldBe, shouldSatisfy)
import           Test.Hspec.QuickCheck           (prop)
import           Test.QuickCheck                 (Arbitrary (arbitrary))

import           Control.Lens                    ((^.))
import           Graphics.SceneGraph.Basic
import           Graphics.SceneGraph.BoundingBox
import           Linear                          (R1 (..), R2 (..), R3 (..),
                                                  V3 (..), V4 (..), (!*))
import qualified Linear                          as L
import           Linear.Vector                   ((^*))


instance Arbitrary a => Arbitrary (V3 a) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

infix 1 `shouldBeApproximately`

shouldBeApproximately :: V3 Float -> V3 Float -> Expectation
shouldBeApproximately a b = roundV a `shouldBe` roundV b
  where
    roundV :: V3 Float -> V3 Float
    roundV v = fmap ((/ 100.0) . fromIntegral . round) (v ^* 100)


origin :: V4 Float
origin = L.point $ pure 0


spec :: Spec
spec = do
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
            translate p camera
      let camNode = findCamera testScene 0
      getTransformTo testScene camNode !* origin `shouldBe` L.point p


  describe "rotateX" $ do
    prop "should not modify the X coordinate" $ \p theta -> do
      testScene <- osg $ do
            rotateX theta (translate p camera)
      let camNode = findCamera testScene 0
      (getTransformTo testScene camNode !* origin) ^. _x
        `shouldBe` p ^. _x

    it "should turn a Y unit vector into a Z unit vector" $ do
      testScene <- osg $ do
            rotateX 90 (translate (V3 0 1 0) camera)
      let camNode = findCamera testScene 0
      (getTransformTo testScene camNode !* origin) ^. _xyz
        `shouldBeApproximately` V3 0 0 1


  describe "rotateY" $ do
    prop "should not modify the Y coordinate" $ \p theta -> do
      testScene <- osg $ do
            rotateY theta (translate p camera)
      let camNode = findCamera testScene 0
      (getTransformTo testScene camNode !* origin) ^. _y
        `shouldBe` p ^. _y

    it "should turn an X unit vector into a Z unit vector" $ do
      testScene <- osg $ do
            rotateY 90 (translate (V3 1 0 0) camera)
      let camNode = findCamera testScene 0
      (getTransformTo testScene camNode !* origin) ^. _xyz
        `shouldBeApproximately` V3 0 0 (-1)


  describe "rotateZ" $ do
    prop "should not modify the Z coordinate" $ \p theta -> do
      testScene <- osg $ do
            rotateZ theta (translate p camera)
      let camNode = findCamera testScene 0
      (getTransformTo testScene camNode !* origin) ^. _z
        `shouldBe` p ^. _z

    it "should turn an X unit vector into a Y unit vector" $ do
      testScene <- osg $ do
            rotateZ 90 (translate (V3 1 0 0) camera)
      let camNode = findCamera testScene 0
      (getTransformTo testScene camNode !* origin) ^. _xyz
        `shouldBeApproximately` V3 0 1 0


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
