{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Language.GLSL.AST where

import           Control.Applicative              (Applicative (..))
import           Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Text.Lazy.Builder           as LTB
import           GHC.Generics                     (Generic)


data GLSL a = GLSL Version [TopDecl a]
  deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

newtype Version = Version Int
  deriving (Generic, Show, Eq)

data TopDecl a
  = LayoutDecl LayoutSpec GlobalDecl
  | GlobalDecl GlobalDecl
  | ProcDecl ProcName [ParamDecl] [StmtAnnot a]
  deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

data ProcName
  = ProcMain
  | ProcName NameId
  deriving (Generic, Show, Eq)

data LayoutSpec
  = LayoutStd140
  | LayoutLocation Int
  deriving (Generic, Show, Eq)

data ParamDecl
  = Param ParamKind LocalDecl
  deriving (Generic, Show, Eq)

data ParamKind
  = PkIn
  | PkOut
  | PkInout
  deriving (Generic, Show, Eq)

data LocalDecl
  = LDecl Type NameId (Maybe Expr)
  deriving (Generic, Show, Eq)

data GlobalDecl
  = GDecl GDeclKind Type Name
  deriving (Generic, Show, Eq)

data GDeclKind
  = GkIn
  | GkOut
  | GkUniform
  deriving (Generic, Show, Eq)

data Type
  = TyBool
  | TyFloat
  | TySampler2D
  | TyVec Int
  | TyMat Int Int
  | TyStruct NameId [(Type, NameId)]
  deriving (Generic, Show, Eq)

newtype NameId = NameId Int
  deriving (Generic, Show, Eq)

data Name
  = Name Namespace NameId
  deriving (Generic, Show, Eq)

data Namespace
  = NsT
  | NsS
  | NsU
  | NsVF
  | NsIn
  | NsOut
  deriving (Generic, Show, Eq)

data FunName
  = PrimAbs
  | PrimAsin
  | PrimAtan
  | PrimCos
  | PrimCross
  | PrimDot
  | PrimFloor
  | PrimFract
  | PrimLength
  | PrimMat3x3
  | PrimMat4x4
  | PrimMod
  | PrimNormalize
  | PrimPow
  | PrimSin
  | PrimSmoothstep
  | PrimSqrt
  | PrimStep
  | PrimTan
  | PrimVec2
  | PrimVec3
  | PrimVec4
  deriving (Generic, Show, Eq)

data Swizzle
  = X | Y | Z | W
  deriving (Generic, Show, Eq)

data NameExpr
  = NameExpr Name
  | UniformExpr NameId NameId
  deriving (Generic, Show, Eq)

data Cast
  = Cast
  | NoCast
  deriving (Generic, Show, Eq)

data ExprAtom
  = LitIntExpr Cast Int
  | LitFloatExpr Cast Float
  | IdentifierExpr NameExpr
  | SwizzleExpr NameId Swizzle
  | VecIndexExpr NameExpr Swizzle
  | MatIndexExpr NameExpr Swizzle Swizzle
  deriving (Generic, Show, Eq)

data Expr
  = UnaryExpr UnaryOp ExprAtom
  | BinaryExpr ExprAtom BinaryOp ExprAtom
  | FunCallExpr FunName [ExprAtom]
  | TextureExpr ExprAtom ExprAtom ExprAtom
  | AtomExpr ExprAtom
  deriving (Generic, Show, Eq)

data BinaryOp
  = BOpPlus
  | BOpMinus
  | BOpMul
  | BOpDiv
  | BOpGE
  | BOpGT
  | BOpLE
  | BOpLT
  | BOpAnd
  | BOpOr
  deriving (Generic, Show, Eq)

data UnaryOp
  = UOpMinus
  | UOpNot
  deriving (Generic, Show, Eq)

data StmtAnnot a = SA
  { annot   :: a
  , unAnnot :: Stmt a
  }
  deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

instance Applicative StmtAnnot where
  pure a = SA a (pure a)
  liftA2 f a b = SA (f (annot a) (annot b)) $ liftA2 f (unAnnot a) (unAnnot b)

data Stmt a
  = AssignStmt Name Expr
  | DeclStmt LocalDecl
  | EmitStmt Emit
  | IfStmt NameId [StmtAnnot a] [StmtAnnot a]
  deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

instance Applicative Stmt where
  -- Arbitrary decision because "pure" doesn't really make sense.
  pure _ = EmitStmt EmitFragDepth

  liftA2 f (IfStmt n t1 e1) (IfStmt _ t2 e2) = IfStmt n
    ((zipWith . liftA2) f t1 t2)
    ((zipWith . liftA2) f e1 e2)
  liftA2 _ (AssignStmt n e) _ = AssignStmt n e
  liftA2 _ (DeclStmt d) _ = DeclStmt d
  liftA2 _ (EmitStmt e) _ = EmitStmt e
  liftA2 _ (IfStmt n _ _) _ = IfStmt n [] []


data Emit
  = EmitPosition Expr
  | EmitFragDepth
  deriving (Generic, Show, Eq)


class Annot a where
  parseAnnot :: Parser a
  ppAnnot :: a -> Maybe LTB.Builder

instance Annot () where
  parseAnnot = pure ()
  ppAnnot = const Nothing

instance (Annot a, Annot b) => Annot (a, b) where
  parseAnnot = error "not implemented"
  ppAnnot (a, b) = do
    ppA <- ppAnnot a
    ppB <- ppAnnot b
    return $ "(" <> ppA <> ", " <> ppB <> ")"

----------------------------------

argCountForFunName :: FunName -> Int
argCountForFunName PrimAbs        = 1
argCountForFunName PrimAsin       = 1
argCountForFunName PrimAtan       = 2
argCountForFunName PrimCos        = 1
argCountForFunName PrimCross      = 2
argCountForFunName PrimDot        = 2
argCountForFunName PrimFloor      = 1
argCountForFunName PrimFract      = 1
argCountForFunName PrimLength     = 1
argCountForFunName PrimMat3x3     = 3
argCountForFunName PrimMat4x4     = 4
argCountForFunName PrimMod        = 2
argCountForFunName PrimNormalize  = 1
argCountForFunName PrimPow        = 2
argCountForFunName PrimSin        = 1
argCountForFunName PrimSmoothstep = 3
argCountForFunName PrimSqrt       = 1
argCountForFunName PrimStep       = 2
argCountForFunName PrimTan        = 1
argCountForFunName PrimVec2       = 2
argCountForFunName PrimVec3       = 3
argCountForFunName PrimVec4       = 4

isLitExpr :: ExprAtom -> Bool
isLitExpr LitFloatExpr{} = True
isLitExpr LitIntExpr{}   = True
isLitExpr _              = False

isIdentifierExpr :: ExprAtom -> Bool
isIdentifierExpr IdentifierExpr{} = True
isIdentifierExpr _                = False
