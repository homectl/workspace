-- | Structural equality, ignoring the variable names.
module Language.GLSL.StructuralEquality where

import           Language.GLSL.ConstExpr (ConstExprs, isConstExpr)
import           Language.GLSL.Types


eqStmtAnnots :: Maybe ConstExprs -> [(StmtAnnot a, StmtAnnot a)] -> Bool
eqStmtAnnots ce = all (uncurry (eqStmtAnnot ce))

eqStmtAnnot :: Maybe ConstExprs -> StmtAnnot a -> StmtAnnot a -> Bool
eqStmtAnnot ce (SA _ a) (SA _ b) = eqStmt ce a b


eqStmt :: Maybe ConstExprs -> Stmt a -> Stmt a -> Bool
eqStmt ce (AssignStmt _ ea) (AssignStmt _ eb) =
  eqExpr ce ea eb
eqStmt ce (DeclStmt da) (DeclStmt db) =
  eqLocalDecl ce da db
eqStmt ce (EmitStmt ea) (EmitStmt eb) =
  eqEmit ce ea eb
eqStmt ce (IfStmt _ ta ea) (IfStmt _ tb eb) =
  length ta == length tb &&
  length ea == length eb &&
  eqStmtAnnots ce (zip ta tb) &&
  eqStmtAnnots ce (zip ea eb)
eqStmt _ _ _ = False


eqExpr :: Maybe ConstExprs -> Expr -> Expr -> Bool
-- We consider constant expressions to be equal, since we can just pass that
-- constant into the function as an argument. Most of the time, it's small
-- things like 1.0 or (-1.0) (possibly in a t-var, hence the ConstExprs set).
eqExpr (Just ce) a b | isConstExpr ce a && isConstExpr ce b = True
eqExpr _ (AtomExpr ea) (AtomExpr eb) =
  eqExprAtom ea eb
eqExpr _ (UnaryExpr ua ea) (UnaryExpr ub eb) =
  ua == ub &&
  eqExprAtom ea eb
eqExpr _ (FunCallExpr fa aa) (FunCallExpr fb ab) =
  fa == fb &&
  length aa == length ab &&
  all (uncurry eqExprAtom) (zip aa ab)
eqExpr _ (TextureExpr ta xa ya) (TextureExpr tb xb yb) =
  all (uncurry eqExprAtom) [(ta, tb), (xa, xb), (ya, yb)]
eqExpr _ (BinaryExpr la oa ra) (BinaryExpr lb ob rb) =
  oa == ob &&
  eqExprAtom la lb &&
  eqExprAtom ra rb
eqExpr _ _ _ =
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
-- eqNameExpr (UniformExpr na ma) (UniformExpr nb mb) = na == nb && ma == mb
-- eqName (Name NsT _) (Name NsT _)   = True
-- eqName (Name nsa na) (Name nsb nb) = nsa == nsb && na == nb
eqNameExpr _ _                                     = True

eqLocalDecl :: Maybe ConstExprs -> LocalDecl -> LocalDecl -> Bool
eqLocalDecl ce (LDecl tya _ ea) (LDecl tyb _ eb) =
  eqType tya tyb && eqMaybe (eqExpr ce) ea eb


eqType :: Type -> Type -> Bool
eqType = (==)


eqEmit :: Maybe ConstExprs -> Emit -> Emit -> Bool
eqEmit ce (EmitPosition a) (EmitPosition b) = eqExpr ce a b
eqEmit _ EmitFragDepth EmitFragDepth       = True
eqEmit _ _ _                               = False


eqMaybe :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Bool
eqMaybe f (Just a) (Just b) = f a b
eqMaybe _ Nothing Nothing   = True
eqMaybe _ _ _               = False
