{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.SceneGraph.VisualiseSpec where

import           Test.Hspec                    (Spec, describe, it, shouldBe)

import           Graphics.SceneGraph.Basic
import           Graphics.SceneGraph.Types
import qualified Graphics.SceneGraph.Visualise as GV
import           Linear                        (V3 (..))


spec :: Spec
spec = do
  describe "toSvg" $ do
    it "should match the golden test file" $ do
      testScene <- osg $ do
            let cam = translate (V3 4 0 (-2)) camera
            let lamp = rotateX 90 $ color Yellow $ translate (V3 0 5 3) light
            cam <+> lamp
      svgFile <- GV.toSvg testScene "test.svg"
      svgFile `shouldBe` "test.svg"
      dotFile <- GV.toDot testScene "test.dot"
      dotFile `shouldBe` "test.dot"

    it "should support cyclic graphs" $ do
      testScene <- osg $ do
            let cam = translate (V3 4 0 (-2)) camera
            let lamp = rotateX 90 $ color Yellow $ translate (V3 0 5 3) light
            cam <+> lamp
      svgFile <- GV.toSvg testScene "test.svg"
      svgFile `shouldBe` "test.svg"
      dotFile <- GV.toDot testScene "test.dot"
      dotFile `shouldBe` "test.dot"
