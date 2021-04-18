{-# LANGUAGE OverloadedStrings #-}
module Graphics.GPipe.Optimizer.Liveness where

import qualified Data.IntSet                   as S
import qualified Data.Text.Lazy.Builder        as LTB
import           Graphics.GPipe.Optimizer.GLSL hiding (t)


newtype Liveness = Liveness { unLiveness :: S.IntSet }

empty :: Liveness
empty = Liveness S.empty

instance Annot Liveness where
  parseAnnot = pure empty
  ppAnnot = Just . LTB.fromString . show . S.toList . unLiveness


computeLiveness :: GLSL a -> GLSL Liveness
computeLiveness (GLSL v d) = GLSL v (map clTopDecl d)

clTopDecl :: TopDecl a -> TopDecl Liveness
clTopDecl (LayoutDecl s d) = LayoutDecl s d
clTopDecl (GlobalDecl d) = GlobalDecl d
clTopDecl (ProcDecl name params ss) =
  ProcDecl name params . fst . clStmtAnnots empty $ ss

clStmtAnnots :: Liveness -> [StmtAnnot a] -> ([StmtAnnot Liveness], Liveness)
clStmtAnnots ls = foldr clStmtAnnot ([], ls)

clStmtAnnot
  :: StmtAnnot a
  -> ([StmtAnnot Liveness], Liveness)
  -> ([StmtAnnot Liveness], Liveness)
clStmtAnnot (SA _ s) (ss, ls) =
  let (s', ls') = clStmt s ls in
  (SA ls' s':ss, ls')

clStmt :: Stmt a -> Liveness -> (Stmt Liveness, Liveness)
clStmt (AssignStmt n e) ls =
  (AssignStmt n e, clExpr e ls)
clStmt (DeclStmt d@(LDecl _ (NameId n) e)) ls =
  (DeclStmt d, delete n . maybe id clExpr e $ ls)
clStmt (EmitStmt e) ls =
  (EmitStmt e, clEmit e ls)
clStmt (IfStmt (NameId c) t e) ls =
  let (t', lsT) = clStmtAnnots ls t
      (e', lsE) = clStmtAnnots ls e
  in
  (IfStmt (NameId c) t' e', insert c $ lsT `union` lsE)


clEmit :: Emit -> Liveness -> Liveness
clEmit EmitFragDepth    = id
clEmit (EmitPosition e) = clExpr e

clExpr :: Expr -> Liveness -> Liveness
clExpr (UnaryExpr _ e) ls      = clExprAtom e ls
clExpr (BinaryExpr l _ r) ls   = clExprAtom l . clExprAtom r $ ls
clExpr (FunCallExpr _ args) ls = foldr clExprAtom ls args
clExpr (TextureExpr t x y) ls  = foldr clExprAtom ls [t, x, y]
clExpr (AtomExpr e) ls         = clExprAtom e ls

clExprAtom :: ExprAtom -> Liveness -> Liveness
clExprAtom (IdentifierExpr n) ls         = clNameExpr n ls
clExprAtom (SwizzleExpr (NameId n) _) ls = insert n ls
clExprAtom _ ls                          = ls

clNameExpr :: NameExpr -> Liveness -> Liveness
clNameExpr (NameExpr (Name NsT (NameId n))) ls = insert n ls
clNameExpr _ ls                                = ls

insert :: Int -> Liveness -> Liveness
insert n = Liveness . S.insert n . unLiveness

delete :: Int -> Liveness -> Liveness
delete n = Liveness . S.delete n . unLiveness

union :: Liveness -> Liveness -> Liveness
union a b = Liveness $ S.union (unLiveness a) (unLiveness b)
