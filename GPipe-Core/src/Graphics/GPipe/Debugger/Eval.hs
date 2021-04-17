{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Graphics.GPipe.Debugger.Eval where

import           Control.Monad                    (foldM, foldM_)
import           Control.Monad.Trans.State.Strict (StateT, evalStateT, get,
                                                   modify')
import qualified Data.IntMap                      as M
import qualified Data.Text.Lazy                   as LT
import           Graphics.GPipe.Internal.Expr
import           Graphics.GPipe.Linear            (V2, V3, V4)
import           Graphics.GPipe.Optimizer.GLSL


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


eval :: LT.Text -> Either String Value
eval code =
  let Right glsl = parseShader code in
  fromResult $ evalStateT (evalGLSL glsl) startState


data Proc
  = Proc [ParamDecl] [StmtAnnot ()]

data Value
  = FloatValue Float
  | IntValue Int
  | BoolValue Bool
  deriving (Show, Eq)

defaultValue :: Type -> Value
defaultValue TyFloat = FloatValue 0
defaultValue ty      = error $ "not implemented: " <> pp ppType ty

data EvalState = EvalState
  { procs    :: M.IntMap Proc
  , mainProc :: Maybe Proc
  , outs     :: M.IntMap Value
  }

newtype EvalResult a = EvalResult { fromResult :: Either String a }
  deriving (Functor, Applicative, Monad)

instance MonadFail EvalResult where
  fail = EvalResult . Left

type Eval = StateT EvalState EvalResult

startState :: EvalState
startState = EvalState
  { procs = M.empty
  , mainProc = Nothing
  , outs = M.empty
  }


evalGLSL :: GLSL () -> Eval Value
evalGLSL (GLSL _ d) = do
  mapM_ discoverTopDecl d
  evalMain
  getValue emptyLocals (Name NsOut (NameId 0))

discoverTopDecl :: TopDecl () -> Eval ()
discoverTopDecl (LayoutDecl _ d) = discoverGlobalDecl d
discoverTopDecl (GlobalDecl d) = discoverGlobalDecl d
discoverTopDecl (ProcDecl ProcMain params body) =
  modify' $ \st -> st{mainProc = Just $ Proc params body}
discoverTopDecl (ProcDecl (ProcName (NameId n)) params body) =
  modify' $ \st@EvalState{..} -> st{procs = M.insert n (Proc params body) procs}

discoverGlobalDecl :: GlobalDecl -> Eval ()
discoverGlobalDecl (GDecl GkOut ty (Name NsOut (NameId n))) =
  modify' $ \st@EvalState{..} -> st{outs = M.insert n (defaultValue ty) outs}
discoverGlobalDecl d =
  error $ "not implemented: " <> pp ppGlobalDecl d


evalMain :: Eval ()
evalMain = do
  Just main <- mainProc <$> get
  evalProc main []
  return ()

newtype LocalState = LocalState
  { temps :: M.IntMap Value
  }

emptyLocals :: LocalState
emptyLocals = LocalState
  { temps = M.empty
  }

evalProc :: Proc -> [Value] -> Eval ()
evalProc (Proc _ ss) _args =
  foldM_ evalStmtAnnot emptyLocals ss

evalStmtAnnot :: LocalState -> StmtAnnot () -> Eval LocalState
evalStmtAnnot lst (SA () s) = evalStmt lst s

evalStmt :: LocalState -> Stmt () -> Eval LocalState
evalStmt lst (AssignStmt n e) = do
  v <- evalExpr lst e
  setValue lst n v
evalStmt lst (DeclStmt d) = evalLocalDecl lst d
evalStmt lst (EmitStmt e) = evalEmit lst e
evalStmt lst (IfStmt cond thens elses) = do
  BoolValue v <- getValue lst (Name NsT cond)
  if v
    then foldM evalStmtAnnot lst thens
    else foldM evalStmtAnnot lst elses

evalEmit :: LocalState -> Emit -> Eval LocalState
evalEmit = error "not implemented"

evalLocalDecl :: LocalState -> LocalDecl -> Eval LocalState
evalLocalDecl lst (LDecl ty n Nothing) =
  setValue lst (Name NsT n) (defaultValue ty)
evalLocalDecl lst (LDecl ty n (Just e)) = do
  v <- evalExpr lst e >>= evalCoerce ty
  setValue lst (Name NsT n) v

evalCoerce :: Type -> Value -> Eval Value
evalCoerce TyFloat v@FloatValue{} = return v
evalCoerce TyFloat (IntValue i)   = return $ FloatValue (fromIntegral i)
evalCoerce ty v                   = fail $ "coerce failed: " <> show (ty, v)

evalExpr :: LocalState -> Expr -> Eval Value
evalExpr lst = \case
  BinaryExpr l op r -> do
    lv <- evalExprAtom lst l
    rv <- evalExprAtom lst r
    return $ evalBinaryOp lv op rv
  AtomExpr e -> evalExprAtom lst e
  e -> fail $ "evalExpr not implemented: " <> pp ppExpr e

evalBinaryOp :: Value -> BinaryOp -> Value -> Value
evalBinaryOp (FloatValue l) BOpPlus (FloatValue r) = FloatValue (l + r)
evalBinaryOp (IntValue l) BOpPlus (IntValue r) = IntValue (l + r)
evalBinaryOp l o r =
  error $ "not implemented: " <> show (l, o, r)

evalExprAtom :: LocalState -> ExprAtom -> Eval Value
evalExprAtom lst = \case
  LitFloatExpr _ f -> return $ FloatValue f
  LitIntExpr _ i   -> return $ IntValue i
  IdentifierExpr n -> getValue lst n
  e                -> fail $ "not implemented: " <> pp ppExprAtom e


setValue :: LocalState -> Name -> Value -> Eval LocalState
setValue lst@LocalState{..} (Name NsT (NameId n)) v =
  return lst{temps = M.insert n v temps}
setValue lst (Name NsOut (NameId n)) v = do
  modify' $ \st@EvalState{..} -> st{outs = M.insert n v outs}
  return lst
setValue _ _ _ =
  error "not implemented: setValue"

getValue :: LocalState -> Name -> Eval Value
getValue LocalState{..} (Name NsT (NameId n)) = do
  let Just v = M.lookup n temps
  return v
getValue _ (Name NsOut (NameId n)) = do
  Just v <- M.lookup n . outs <$> get
  return v
getValue _ _ =
  error "not implemented: setValue"
