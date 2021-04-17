{-# LANGUAGE OverloadedStrings #-}
module Test.GLSL where

import           Test.Hspec                        (Expectation, shouldBe)

import           Data.Attoparsec.ByteString.Char8  (many1, parseOnly)
import qualified Data.ByteString.Char8             as BS
import           Data.String                       (fromString)
import qualified Data.Text.Lazy                    as LT
import qualified Data.Text.Lazy.Builder            as LTB
import qualified Graphics.GPipe.Optimizer.GLSL     as GLSL


type Transform a b = [GLSL.StmtAnnot a] -> [GLSL.StmtAnnot b]
type TransformExpectation a b
    = (Transform a b, [BS.ByteString])
    -> [LT.Text]
    -> Expectation

with :: [s] -> Transform a b -> (Transform a b, [s])
with = flip (,)

shouldTransformTo :: (GLSL.Annot a, GLSL.Annot b) => TransformExpectation a b
shouldTransformTo (f, code) expected =
    let Right glsl = parseOnly (many1 GLSL.parseStmtAnnot) $ BS.unlines code
        res = f glsl
    in
    concatMap (LT.lines . LTB.toLazyText . GLSL.ppStmtAnnot) res
    `shouldBe`
    expected

shouldNotTransform
    :: (GLSL.Annot a, GLSL.Annot b)
    => (Transform a b, [String])
    -> Expectation
shouldNotTransform (f, a) =
    shouldTransformTo (f, map fromString a) (map fromString a)
