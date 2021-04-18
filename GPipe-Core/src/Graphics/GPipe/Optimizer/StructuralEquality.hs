-- | Structural equality, ignoring the variable names.
module Graphics.GPipe.Optimizer.StructuralEquality where

import           Graphics.GPipe.Optimizer.ConstExpr (ConstExprs, isConstExpr)
import           Graphics.GPipe.Optimizer.GLSL


eqStmtAnnots :: ConstExprs -> [(StmtAnnot a, StmtAnnot a)] -> Bool
eqStmtAnnots ce = all (uncurry (eqStmtAnnot ce))

eqStmtAnnot :: ConstExprs -> StmtAnnot a -> StmtAnnot a -> Bool
eqStmtAnnot ce (SA _ a) (SA _ b) = eqStmt ce a b


eqStmt :: ConstExprs -> Stmt a -> Stmt a -> Bool
eqStmt ce (AssignStmt _ ea) (AssignStmt _ eb) =
  -- We consider constant expressions to be equal, since we can just pass that
  -- constant into the function as an argument. Most of the time, it's small
  -- things like 1.0 or (-1.0) (possibly in a t-var, hence the ConstExprs set).
  isConstExpr ce ea && isConstExpr ce eb ||
  eqExpr ea eb
eqStmt _ (DeclStmt da) (DeclStmt db) =
  eqLocalDecl da db
eqStmt _ (EmitStmt ea) (EmitStmt eb) =
  eqEmit ea eb
eqStmt ce (IfStmt _ ta ea) (IfStmt _ tb eb) =
  eqStmtAnnots ce (zip ta tb) &&
  eqStmtAnnots ce (zip ea eb)
eqStmt _ _ _ = False


eqExpr :: Expr -> Expr -> Bool
eqExpr (AtomExpr ea) (AtomExpr eb) =
  eqExprAtom ea eb
eqExpr (UnaryExpr ua ea) (UnaryExpr ub eb) =
  ua == ub && eqExprAtom ea eb
eqExpr (FunCallExpr fa aa) (FunCallExpr fb ab) =
  fa == fb && all (uncurry eqExprAtom) (zip aa ab)
eqExpr (TextureExpr ta xa ya) (TextureExpr tb xb yb) =
  all (uncurry eqExprAtom) [(ta, tb), (xa, xb), (ya, yb)]
eqExpr (BinaryExpr la oa ra) (BinaryExpr lb ob rb) =
  oa == ob && eqExprAtom la lb && eqExprAtom ra rb
eqExpr _ _ =
  False


eqExprAtom :: ExprAtom -> ExprAtom -> Bool
eqExprAtom (LitIntExpr _ _) (LitIntExpr _ _)             = True
eqExprAtom (LitIntExpr _ _) (LitFloatExpr _ _)           = True
eqExprAtom (LitFloatExpr _ _) (LitFloatExpr _ _)         = True
eqExprAtom (LitFloatExpr _ _) (LitIntExpr _ _)           = True
eqExprAtom (IdentifierExpr a) (IdentifierExpr b)         = eqNameExpr a b
eqExprAtom (SwizzleExpr _ a) (SwizzleExpr _ b)           = a == b
eqExprAtom (VecIndexExpr _ ia) (VecIndexExpr _ ib)       = ia == ib
eqExprAtom (MatIndexExpr _ ia ja) (MatIndexExpr _ ib jb) = ia == ib && ja == jb
eqExprAtom _ _                                           = False

-- | All variable names are equal.
--
--   We used to ignore temporary names only, considering all other names as
--   globals and a fixed part of the code. This check is quite expensive and it
--   turns out that most of the time the global variables *are* the same. If
--   they are not, we'll need to pass them as arguments to our new function, but
--   this is rare enough so it won't increase the average function parameter
--   list length too much.
eqNameExpr :: NameExpr -> NameExpr -> Bool
eqNameExpr (UniformExpr na ma) (UniformExpr nb mb) = na == nb && ma == mb
-- eqName (Name NsT _) (Name NsT _)   = True
-- eqName (Name nsa na) (Name nsb nb) = nsa == nsb && na == nb
eqNameExpr _ _ = True

eqLocalDecl :: LocalDecl -> LocalDecl -> Bool
eqLocalDecl (LDecl tya _ ea) (LDecl tyb _ eb) =
  eqType tya tyb && eqMaybe eqExpr ea eb


eqType :: Type -> Type -> Bool
eqType = (==)


eqEmit :: Emit -> Emit -> Bool
eqEmit (EmitPosition a) (EmitPosition b) = eqExpr a b
eqEmit EmitFragDepth EmitFragDepth       = True
eqEmit _ _                               = False


eqMaybe :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Bool
eqMaybe f (Just a) (Just b) = f a b
eqMaybe _ Nothing Nothing   = True
eqMaybe _ _ _               = False
