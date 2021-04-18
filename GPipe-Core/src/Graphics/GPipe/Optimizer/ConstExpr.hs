module Graphics.GPipe.Optimizer.ConstExpr
  ( ConstExprs
  , collectConstExprs
  , isConstExpr
  ) where

import qualified Data.IntSet                   as S
import           Graphics.GPipe.Optimizer.GLSL


newtype ConstExprs = ConstExprs S.IntSet

collectConstExprs :: [StmtAnnot a] -> ConstExprs
collectConstExprs = ConstExprs . foldr (add . unAnnot) S.empty
  where
    add :: Stmt a -> S.IntSet -> S.IntSet
    add (AssignStmt (Name NsT (NameId n)) e) s
      | isConstExpr (ConstExprs s) e = S.insert n s
    add _ s = s


isConstExpr :: ConstExprs -> Expr -> Bool
isConstExpr ce (BinaryExpr l BOpMul r) =
  any isZero [l, r] || all (isConstExprAtom ce) [l, r]

isConstExpr ce (AtomExpr e)         = isConstExprAtom ce e
isConstExpr ce (UnaryExpr _ e)      = isConstExprAtom ce e
isConstExpr ce (BinaryExpr l _ r)   = all (isConstExprAtom ce) [l, r]
isConstExpr ce (FunCallExpr _ args) = all (isConstExprAtom ce) args
isConstExpr _ TextureExpr{}         = False


isConstExprAtom :: ConstExprs -> ExprAtom -> Bool
isConstExprAtom (ConstExprs ce) (IdentifierExpr (NameExpr (Name NsT (NameId n)))) =
  S.member n ce

isConstExprAtom _ LitIntExpr{}   = True
isConstExprAtom _ LitFloatExpr{} = True
isConstExprAtom _ _              = False


isZero :: ExprAtom -> Bool
isZero (LitIntExpr _ 0)   = True
isZero (LitFloatExpr _ 0) = True
isZero _                  = False
