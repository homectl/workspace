{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.GPipe.Debugger.Compile where

import qualified Data.Text.Lazy               as LT
import           Graphics.GPipe.Internal.Expr
import           Graphics.GPipe.Linear        (V2, V3, V4)


class ResultValue a where
  resultExpr :: a -> ExprM LT.Text
  resultType :: a -> SType

instance ResultValue (S x Float) where
  resultExpr = unS
  resultType _ = STypeFloat

instance ResultValue (V2 (S x Float)) where
  resultExpr = unS . fromVec2
  resultType _ = STypeVec 2

instance ResultValue (V3 (S x Float)) where
  resultExpr = unS . fromVec3
  resultType _ = STypeVec 3

instance ResultValue (V4 (S x Float)) where
  resultExpr = unS . fromVec4
  resultType _ = STypeVec 4


compile :: ResultValue a => a -> IO LT.Text
compile expr =
  fmap finalSource
  $ runExprM (tellGlobalLn $ "out " <> stypeName (resultType expr) <> " out0")
  $ resultExpr expr >>= tellAssignment' "out0"
