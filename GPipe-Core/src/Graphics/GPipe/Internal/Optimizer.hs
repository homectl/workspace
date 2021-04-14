{-# LANGUAGE OverloadedStrings #-}
module Graphics.GPipe.Internal.Optimizer where

import           Data.Attoparsec.Text.Lazy
import           Data.Text                 (Text)
import qualified Data.Text.Lazy            as LT

optimizeShader :: a -> a
optimizeShader = id
-- optimizeShader :: LT.Text -> Either String LT.Text
-- optimizeShader = pprintGLSL . parseOnly parseGLSL

data GLSL
  = Version Int
  | LayoutDecl Expr GDeclKind Type Name
  | GlobalDecl GDeclKind Type Name
  | FunDecl Type Name [Decl] [Stmt]

data GDeclKind
  = In
  | Out
  | Uniform

data Type
  = TyFloat
  | TyVec Int
  | TyStruct [(Type, Name)]

newtype Name
  = Name Text

data Decl
  = LocalDecl Type Name (Maybe Expr)

data Expr
  = LitIntExpr Float
  | LitFloatExpr Float
  | MemberAccessExpr Name Name
  | AssignmentExpr Name Expr
  | BinaryExpr Expr BinaryOp Expr
  | FunCallExpr Name [Expr]

data BinaryOp
  = BOpPlus
  | BOpMinus
  | BOpMul
  | BOpDiv

data Stmt
  = AssignStmt Name Expr
  | DeclStmt Decl
  | IfStmt Expr [Stmt] [Stmt]

-- parseGLSL :: Text -> Parser GLSL
-- parseGLSL = return GLSL
