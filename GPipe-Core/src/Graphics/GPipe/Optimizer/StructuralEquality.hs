-- | Structural equality, ignoring the variable names.
module Graphics.GPipe.Optimizer.StructuralEquality where

import           Graphics.GPipe.Optimizer.GLSL


eqStmts :: [(Stmt, Stmt)] -> Bool
eqStmts = all (uncurry eqStmt)


eqStmt :: Stmt -> Stmt -> Bool
eqStmt (AssignStmt _ ea) (AssignStmt _ eb) = eqExpr ea eb
eqStmt (DeclStmt da) (DeclStmt db) = eqLocalDecl da db
eqStmt (EmitStmt ea) (EmitStmt eb) = eqEmit ea eb
eqStmt (IfStmt _ ta ea) (IfStmt _ tb eb) =
  eqStmts (zip ta tb) &&
  eqStmts (zip ea eb)
eqStmt _ _ = False


eqExpr :: Expr -> Expr -> Bool
eqExpr (LitIntExpr _) (LitIntExpr _)           = True
eqExpr (LitIntExpr _) (LitFloatExpr _)         = True
eqExpr (LitFloatExpr _) (LitFloatExpr _)       = True
eqExpr (LitFloatExpr _) (LitIntExpr _)         = True
eqExpr (FunCallExpr PrimFloat [a]) b           = eqExpr a b
eqExpr a (FunCallExpr PrimFloat [b])           = eqExpr a b
eqExpr (IdentifierExpr a) (IdentifierExpr b)   = eqName a b
eqExpr (UniformExpr na ma) (UniformExpr nb mb) = na == nb && ma == mb
eqExpr (SwizzleExpr _ a) (SwizzleExpr _ b)     = a == b
eqExpr (IndexExpr ea ia) (IndexExpr eb ib)     = ia == ib && eqExpr ea eb
eqExpr (ParenExpr a) (ParenExpr b)             = eqExpr a b
eqExpr (UnaryExpr ua ea) (UnaryExpr ub eb)     = ua == ub && eqExpr ea eb
eqExpr (FunCallExpr fa aa) (FunCallExpr fb ab) = fa == fb && all (uncurry eqExpr) (zip aa ab)
eqExpr (BinaryExpr la oa ra) (BinaryExpr lb ob rb) = oa == ob && eqExpr la lb && eqExpr ra rb
eqExpr _ _                                         = False


-- | Ignore temporary names only. All other names are globals and we consider
--   them a fixed part of the code.
eqName :: Name -> Name -> Bool
eqName (Name NsT _) (Name NsT _)   = True
eqName (Name nsa na) (Name nsb nb) = nsa == nsb && na == nb


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
eqMaybe _ _ _               = False
