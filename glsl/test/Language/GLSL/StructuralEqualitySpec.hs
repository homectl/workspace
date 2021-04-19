{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.GLSL.StructuralEqualitySpec where

import           Test.GLSL.Arbitrary              ()
import           Test.Hspec                       (Spec, describe, shouldBe, it)
import           Test.Hspec.QuickCheck            (prop)

import           Control.Monad                    (when)
import qualified Language.GLSL.ConstExpr          as ConstExpr
import           Language.GLSL.StructuralEquality (eqStmt)
import           Language.GLSL.Types              

ce :: Maybe ConstExpr.ConstExprs
ce = Just ConstExpr.empty

spec :: Spec
spec = do
  describe "eqStmt" $ do
    prop "should obey (a = b) & (b = c) => (a = c)" $
      \(a :: Stmt ()) (b :: Stmt ()) (c :: Stmt ()) ->
        when (eqStmt ce a b && eqStmt ce b c) $
          eqStmt ce a c `shouldBe` True

    prop "should obey (a = b) & (b != c) => (a != c)" $
      \(a :: Stmt ()) (b :: Stmt ()) (c :: Stmt ()) ->
        when (eqStmt ce a b && not (eqStmt ce b c)) $
          eqStmt ce a c `shouldBe` False

    prop "should be True for (a == b)" $
      \(a :: Stmt ()) ->
        eqStmt ce a a `shouldBe` True

    it "should differentiate constant expressions if we pass no ConstExpr" $
      let n = Name NsT (NameId 0)
          a = AssignStmt n (UnaryExpr UOpMinus (LitIntExpr NoCast 0))
          b = AssignStmt n (FunCallExpr PrimAsin [])
      in
      eqStmt Nothing a b `shouldBe` False

    it "should treat constant expressions as equal if we pass ConstExpr" $
      let n = Name NsT (NameId 0)
          a = AssignStmt n (UnaryExpr UOpMinus (LitIntExpr NoCast 0))
          b = AssignStmt n (FunCallExpr PrimAsin [])
      in
      eqStmt ce a b `shouldBe` True
