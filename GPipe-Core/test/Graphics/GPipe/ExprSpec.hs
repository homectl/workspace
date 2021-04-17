{-# LANGUAGE OverloadedStrings #-}
module Graphics.GPipe.ExprSpec where

import           Test.Hspec                   (Spec, describe, it, shouldBe)

import qualified Data.Text.Lazy               as LT
import           Graphics.GPipe               (V4 (..), identity, ifB, norm,
                                               (==*))
import qualified Graphics.GPipe.Debugger.Eval as Eval
import           Graphics.GPipe.Expr          (FFloat, ifThenElse')
import           Graphics.GPipe.Linear        ((!*))

wrap :: FFloat -> IO LT.Text
wrap = Eval.compile

golden :: LT.Text -> IO ()
golden = mapM_ (\l -> putStrLn $ "                , " ++ show l) . LT.lines


spec :: Spec
spec = do
    describe "(+)" $ do
        it "should be able to add two numbers" $ do
            let a = 1
                b = 2
                c = a + b
            res <- wrap c
            res `shouldBe` LT.unlines
                [ "#version 450"
                , "out float out0;"
                , "void main() {"
                , "float t0 = (1+2);"
                , "out0 = t0;"
                , "}"
                ]

        it "should be able to add many numbers" $ do
            let (a, b, c, d) = (1, 2, 3, 4)
                out0 = a + b + c + d
            res <- wrap out0
            res `shouldBe` LT.unlines
                [ "#version 450"
                , "out float out0;"
                , "void main() {"
                , "float t0 = (1+2);"
                , "float t1 = (t0+3);"
                , "float t2 = (t1+4);"
                , "out0 = t2;"
                , "}"
                ]

        it "should reuse already computed values" $ do
            let (a, b, c, d) = (1, 2, 3, 4)
                e = a + b + c + d
                out0 = e + e
            res <- wrap out0
            res `shouldBe` LT.unlines
                [ "#version 450"
                , "out float out0;"
                , "void main() {"
                , "float t0 = (1+2);"
                , "float t1 = (t0+3);"
                , "float t2 = (t1+4);"
                , "float t3 = (t2+t2);"
                , "out0 = t3;"
                , "}"
                ]


    describe "ifB" $ do
        it "should produce 4 separate 'if's for V4 branch values" $ do
            let a = V4 1 2 3 4
                b = V4 5 6 7 8
                c = a ==* b
                out0 = norm (ifB c a b)
            res <- wrap out0
            -- golden res
            res `shouldBe` LT.unlines
                [ "#version 450"
                , "out float out0;"
                , "void main() {"
                , "bool t0 = (1==5);"
                , "bool t1 = (2==6);"
                , "bool t2 = (3==7);"
                , "bool t3 = (4==8);"
                , "bool t4 = (t2&&t3);"
                , "bool t5 = (t1&&t4);"
                , "bool t6 = (t0&&t5);"
                , "float t7;"
                , "if(t6){"
                , "t7 = 1;"
                , "} else {"
                , "t7 = 5;"
                , "}"
                , "float t8;"
                , "if(t6){"
                , "t8 = 2;"
                , "} else {"
                , "t8 = 6;"
                , "}"
                , "float t9;"
                , "if(t6){"
                , "t9 = 3;"
                , "} else {"
                , "t9 = 7;"
                , "}"
                , "float t10;"
                , "if(t6){"
                , "t10 = 4;"
                , "} else {"
                , "t10 = 8;"
                , "}"
                , "vec4 t11 = vec4(t7,t8,t9,t10);"
                , "float t12 = length(t11);"
                , "out0 = t12;"
                , "}"
                ]


    describe "ifThenElse'" $ do
        it "should produce only 2 'if's for V4 branch values" $ do
            let a = V4 1 2 3 4
                b = V4 5 6 7 8
                c = a ==* b
                out0 = norm $ ifThenElse' c a b
            res <- wrap out0
            -- golden res
            res `shouldBe` LT.unlines
                [ "#version 450"
                , "out float out0;"
                , "void main() {"
                , "bool t0 = (1==5);"
                , "bool t1 = (2==6);"
                , "bool t2 = (3==7);"
                , "bool t3 = (4==8);"
                , "bool t4 = (t2&&t3);"
                , "bool t5 = (t1&&t4);"
                , "bool t6 = (t0&&t5);"
                , "float t7;"
                , "float t8;"
                , "float t9;"
                , "float t10;"
                , "if(t6){"
                , "t7 = 1;"
                , "t8 = 2;"
                , "t9 = 3;"
                , "t10 = 4;"
                , "} else {"
                , "t7 = 5;"
                , "t8 = 6;"
                , "t9 = 7;"
                , "t10 = 8;"
                , "}"
                , "vec4 t11 = vec4(t7,t8,t9,t10);"
                , "float t12 = length(t11);"
                , "out0 = t12;"
                , "}"
                ]


    describe "!*" $ do
        it "should compile to GLSL multiplication operator '*'" $ do
            let m = identity
                v = V4 5 6 7 8
                out0 = norm $ m !* v
            res <- wrap out0
            -- golden res
            res `shouldBe` LT.unlines
                [ "#version 450"
                , "out float out0;"
                , "void main() {"
                , "vec4 t0 = vec4(5,6,7,8);"
                , "vec4 t1 = vec4(1,0,0,0);"
                , "vec4 t2 = vec4(0,1,0,0);"
                , "vec4 t3 = vec4(0,0,1,0);"
                , "vec4 t4 = vec4(0,0,0,1);"
                , "mat4x4 t5 = mat4x4(t1,t2,t3,t4);"
                , "vec4 t6 = (t0*t5);"
                , "vec4 t7 = vec4(t6[0],t6[1],t6[2],t6[3]);"
                , "float t8 = length(t7);"
                , "out0 = t8;"
                , "}"
                ]
