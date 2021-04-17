{-# LANGUAGE OverloadedStrings #-}
module Graphics.GPipe.Optimizer.DeinlineSpec where

import           Test.Hspec                        (Spec, describe, it)

import           Graphics.GPipe.Optimizer.Deinline (Config (..), defaultConfig,
                                                    diStmts)
import           Test.GLSL                         (Transform,
                                                    shouldNotTransform,
                                                    shouldTransformTo, with)


-- | Test config has smaller window size so we don't need to write as many
--   lines in our tests.
testConfig :: Config
testConfig = defaultConfig{windowSize=3}

testDI :: Transform () ()
testDI = diStmts testConfig


spec :: Spec
spec = do
    describe "diStmts" $ do
        it "should leave non-repetitive code alone" $
            shouldNotTransform $
            [ "float t1 = 0;"
            , "float t2 = 0.0;"
            , "float t3 = (t2+1.1);"
            , "float t4 = (t3+vf1);"
            ] `with` testDI

        it "should remove repeated code" $
            [ "float t1 = 0;"
            , "float t2 = 0.0;"
            , "float t3 = (t2+1.1);" -- start of same code
            , "float t4 = (t3+vf1);"
            , "float t5 = (t4*vf1);"
            , "float t6 = (t5/vf1);"
            , "float t7 = (t5-vf1);" -- end of same code
            , "float t8 = (t2+1.1);" -- different code
            , "float t9 = sin(t3);" -- also different
            , "float t10 = (t9+1.1);" -- start of same code
            , "float t11 = (t10+vf1);"
            , "float t12 = (t11*vf1);"
            , "float t13 = (t12/vf1);"
            , "float t14 = (t13-vf1);" -- end of same code
            , "float t15 = cos(t14);" -- different code
            , "float t16 = tan(t15);" -- also different
            ] `with` testDI
            `shouldTransformTo`
            [ "float t1 = 0;"
            , "float t2 = 0.0;"
            , "float t8 = (t2+1.1);" -- different code
            , "float t9 = sin(t3);" -- also different
            , "float t15 = cos(t14);" -- different code
            , "float t16 = tan(t15);" -- also different
            ]
