{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData      #-}
module Graphics.GPipe.Optimizer.DFG where

import           Control.Monad.Trans.State      (StateT, execState, get,
                                                 modify', put)
import           Data.Foldable                  (forM_)
import           Data.Functor                   (void)
import           Data.Functor.Identity          (Identity)
import           Data.Graph.Inductive           (Node)
import qualified Data.Graph.Inductive           as G
import qualified Data.GraphViz                  as GV
import qualified Data.GraphViz.Printing         as GV
import qualified Data.Text.Lazy.IO              as IO
import           Graphics.GPipe.Optimizer.Decls
import           Graphics.GPipe.Optimizer.GLSL  hiding (t)

--------------------------------------------------------------------------------

data DFG = DFG
  { gr         :: G.Gr DFGNode DFGEdge
  , decls      :: Decls Node
  , nextNodeId :: Node
  , ifCond     :: Maybe Node
  }

emptyDFG :: DFG
emptyDFG = DFG
  { gr = G.empty
  , decls = emptyDecls
  , nextNodeId = 0
  , ifCond = Nothing
  }

data DFGEdge = DFGEdge
  deriving (Show)

data DFGNode
  = DFGNode Namespace NameId
  deriving (Show)

type DFGState = StateT DFG Identity

genDFG :: GLSL a -> DFG
genDFG prog = execState (dfgGLSL prog) emptyDFG

dfgGLSL :: GLSL a -> DFGState ()
dfgGLSL (GLSL _ decls) = mapM_ dfgTopDecl decls

dfgTopDecl :: TopDecl a -> DFGState ()
dfgTopDecl (LayoutDecl _ d)     = dfgGlobalDecl d
dfgTopDecl (GlobalDecl d)       = dfgGlobalDecl d
dfgTopDecl (ProcDecl _ _ stmts) = mapM_ dfgStmtAnnot stmts

dfgGlobalDecl :: GlobalDecl -> DFGState ()
dfgGlobalDecl (GDecl _ (TyStruct _ ms) (Name NsU n)) =
  mapM_ (dfgStructMember n) ms
dfgGlobalDecl (GDecl _ _ (Name ns n)) =
  void $ addNode ns n

dfgStructMember :: NameId -> (Type, NameId) -> DFGState ()
dfgStructMember n (_, m) = void $ addNode NsU (toUniformId (n, m))

dfgStmt :: Stmt a -> DFGState ()
dfgStmt (DeclStmt d)     = dfgLocalDecl d
dfgStmt (AssignStmt n e) = do
  targetId <- nodeForName n
  DFG{ifCond} <- get
  forM_ ifCond (addEdge targetId)
  dfgExpr e targetId
dfgStmt (IfStmt c t e) = do
  ifCond <- Just <$> nodeFor NsT c
  modify' $ \dfg -> dfg{ifCond}
  mapM_ dfgStmtAnnot (t ++ e)
  modify' $ \dfg -> dfg{ifCond=Nothing}
dfgStmt _                = return ()

dfgStmtAnnot :: StmtAnnot a -> DFGState ()
dfgStmtAnnot (SA _ s) = dfgStmt s

dfgLocalDecl :: LocalDecl -> DFGState ()
dfgLocalDecl (LDecl _ n e) = do
  nodeId <- addNode NsT n
  maybe (return ()) (`dfgExpr` nodeId) e

dfgExpr :: Expr -> Node -> DFGState ()
dfgExpr (FunCallExpr _ args) declNode = mapM_ (`dfgExprAtom` declNode) args
dfgExpr (TextureExpr t x y) declNode  = mapM_ (`dfgExprAtom` declNode) [t, x, y]
dfgExpr (UnaryExpr _ e) declNode      = dfgExprAtom e declNode
dfgExpr (AtomExpr e) declNode         = dfgExprAtom e declNode
dfgExpr (BinaryExpr l _ r) declNode   = mapM_ (`dfgExprAtom` declNode) [l, r]

dfgExprAtom :: ExprAtom -> Node -> DFGState ()
dfgExprAtom LitIntExpr{} _                = return ()
dfgExprAtom LitFloatExpr{} _              = return ()
dfgExprAtom (IdentifierExpr n) declNode   = nodeForName n >>= addEdge declNode
dfgExprAtom (UniformExpr n m)  declNode   = nodeForUniform n m >>= addEdge declNode
dfgExprAtom (SwizzleExpr n _)  declNode   = nodeFor NsT n >>= addEdge declNode
dfgExprAtom (VecIndexExpr n _) declNode   = nodeForName n >>= addEdge declNode
dfgExprAtom (MatIndexExpr n _ _) declNode = nodeForName n >>= addEdge declNode


nodeForUniform :: NameId -> NameId -> DFGState Node
nodeForUniform n m = do
  DFG{..} <- get
  let i = toUniformId (n, m)
  case getDecl NsU i decls of
    Nothing -> error $ "no node for " <> showUniformId i
    Just ok -> return ok

nodeForName :: Name -> DFGState Node
nodeForName (Name ns n) = nodeFor ns n

nodeFor :: Namespace -> NameId -> DFGState Node
nodeFor ns n = do
  DFG{..} <- get
  case getDecl ns n decls of
    Nothing -> error $ "no node for " <> pp ppName (Name ns n)
    Just ok -> return ok

addEdge :: Node -> Node -> DFGState ()
addEdge declNode idNode =
  modify' $ \g@DFG{gr} -> g { gr = G.insEdge (declNode, idNode, DFGEdge) gr }

addNode :: Namespace -> NameId -> DFGState Node
addNode ns n = do
  g@DFG{..} <- get
  let nodeId = nextNodeId
  put g { gr = G.insNode (nodeId, DFGNode ns n) gr
        , decls = addDecl ns n nodeId decls
        , nextNodeId = nextNodeId + 1
        }
  return nodeId

--------------------------------------------------------------------------------
-- Visualisation
--------------------------------------------------------------------------------

instance GV.Labellable DFGNode where
  toLabelValue (DFGNode NsU i) = GV.toLabelValue $ showUniformId i
  toLabelValue (DFGNode ns n) = GV.toLabelValue $
    pp ppNamespace ns <> show n

instance GV.Labellable DFGEdge where
  toLabelValue DFGEdge = GV.toLabelValue ""

toDot :: FilePath -> DFG -> IO ()
toDot path =
  IO.writeFile path . GV.printIt . GV.graphToDot GV.quickParams . gr

toSvg :: DFG -> FilePath -> IO FilePath
toSvg DFG{gr} = GV.runGraphviz (GV.graphToDot GV.quickParams gr) GV.Svg
