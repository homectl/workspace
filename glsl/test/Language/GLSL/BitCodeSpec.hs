{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module Language.GLSL.BitCodeSpec where

import           Test.GLSL.Arbitrary              ()
import           Test.Hspec                       (Spec, describe, it, runIO,
                                                   shouldBe, shouldNotBe)
import           Test.Hspec.QuickCheck            (prop)
import           Test.QuickCheck                  (forAllShrink, shrink)
import qualified Test.QuickCheck.Gen              as Gen

import           Control.Monad                    (forM_, unless, when)
import qualified Data.Text.Lazy.IO                as IO
import qualified Data.Vector                      as V
import           Language.GLSL.BitCode            (encodeStmt)
import qualified Language.GLSL.ConstExpr          as ConstExpr
import           Language.GLSL.StructuralEquality (eqStmt)
import           Language.GLSL.Types
import qualified System.Directory as Dir

-- Run an exhaustive n^2 test comparing every line to every other line in the
-- test file. This takes around 13 seconds on my laptop.
exhaustive :: Bool
exhaustive = False

getStatements :: FilePath -> IO (V.Vector (Stmt ()))
getStatements path = do
  inTopDir <- Dir.doesDirectoryExist "glsl"
  when inTopDir $ Dir.setCurrentDirectory "glsl"
  glsl <- parseShader <$> IO.readFile path
  case glsl of
    Left err ->
      fail err
    Right (GLSL _ (reverse -> (ProcDecl _ _ ss):_)) ->
      return . V.fromList . map unAnnot $ ss
    Right _ ->
      fail "test file does not end with a main function"


encodeStmtSpec :: Maybe ConstExpr.ConstExprs -> Spec
encodeStmtSpec ce = do
  prop "should produce the same encoding for structurally equal statements" $
    \(s1 :: Stmt ()) (s2 :: Stmt ()) ->
      let enc1 = encodeStmt ce s1
          enc2 = encodeStmt ce s2
      in
      when (eqStmt ce s1 s2) $
        enc1 `shouldBe` enc2

  prop "should produce different encoding for structurally different statements" $
    \(s1 :: Stmt ()) (s2 :: Stmt ()) ->
      let enc1 = encodeStmt ce s1
          enc2 = encodeStmt ce s2
      in
      unless (eqStmt ce s1 s2) $
        enc1 `shouldNotBe` enc2


testFileSpec :: FilePath -> Spec
testFileSpec filename = do
  stmts <- runIO $ getStatements $ "test/data/large-shaders/" <> filename
  let forAllStmt p = forAllShrink (
          (stmts V.!) <$> Gen.chooseBoundedIntegral (0, V.length stmts - 1))
        shrink p

  it ("should be able to encode all statements from " <> filename) $ do
    forM_ stmts $ \s ->
      encodeStmt Nothing s
      `shouldBe`
      encodeStmt Nothing s

  prop ("should obey (a = b) => enc a = enc b for all pairs in " <> filename) $
    forAllStmt $ \s1 -> forAllStmt $ \s2 ->
        let enc1 = encodeStmt Nothing s1
            enc2 = encodeStmt Nothing s2
        in
        if eqStmt Nothing s1 s2
          then enc1 `shouldBe` enc2
          else enc1 `shouldNotBe` enc2

  when exhaustive $
    it ("should obey (a = b) => enc a = enc b for every single pair in " <> filename) $
      forM_ [0..V.length stmts - 1] $ \i -> forM_ [i..V.length stmts - 1] $ \j ->
          let s1 = stmts V.! i
              s2 = stmts V.! j
              enc1 = encodeStmt Nothing s1
              enc2 = encodeStmt Nothing s2
          in
          if eqStmt Nothing s1 s2
            then enc1 `shouldBe` enc2
            else enc1 `shouldNotBe` enc2



spec :: Spec
spec = do
  describe "encodeStmt" $ do
    encodeStmtSpec Nothing

  describe "encodeStmt with ConstExpr" $ do
    encodeStmtSpec $ Just ConstExpr.empty

  -- testFileSpec "lambdacnc.frag"
  -- testFileSpec "lambdacnc.vert"
  testFileSpec "xax.frag"
  -- testFileSpec "xax.vert"
