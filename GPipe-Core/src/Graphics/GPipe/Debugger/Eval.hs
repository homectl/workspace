{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE ViewPatterns               #-}
module Graphics.GPipe.Debugger.Eval where

import           Control.Lens                     ((^.))
import           Control.Monad                    (foldM, foldM_)
import           Control.Monad.Trans.State.Strict (evalStateT, get, modify')
import qualified Data.IntMap                      as M
import qualified Data.Text.Lazy                   as LT
import qualified Data.Text.Lazy.IO                as IO
import qualified Debug.Trace                      as Trace
import qualified Graphics.GPipe.Debugger.PrimFuns as PrimFuns
import           Graphics.GPipe.Debugger.Value    (Eval, EvalResult (..),
                                                   EvalState (..), Proc (..),
                                                   Value (..), defaultValue,
                                                   evalBinaryOp, evalCoerce,
                                                   evalUnaryOp, isNaNValue,
                                                   roundValue)
import           Graphics.GPipe.Linear            (R1 (..), R2 (..), R3 (..),
                                                   R4 (..))
import           Graphics.GPipe.Optimizer.Decls   (addDecl, addDeclN,
                                                   emptyDecls, getDeclN,
                                                   toUniformId)
import           Graphics.GPipe.Optimizer.GLSL


traceAssignments :: Bool
traceAssignments = False

trace :: String -> a -> a
trace = if traceAssignments then Trace.trace else const id


eval :: LT.Text -> Either String Value
eval code = do
  glsl <- parseShader code
  fromResult $ evalStateT (evalGLSL glsl) startState


startState :: EvalState
startState = EvalState
  { stProcs = M.empty
  , stMainProc = Nothing

  , globals = emptyDecls
  , gl_Position = Nothing
  }


evalGLSL :: GLSL () -> Eval Value
evalGLSL (GLSL _ d) = do
  mapM_ discoverTopDecl d
  evalMain
  -- maybe (FloatValue 0) id . gl_Position <$> get
  roundValue <$> getValue emptyLocals (Name NsOut (NameId 0))

discoverTopDecl :: TopDecl () -> Eval ()
discoverTopDecl (LayoutDecl _ d) = discoverGlobalDecl d
discoverTopDecl (GlobalDecl d) = discoverGlobalDecl d
discoverTopDecl (ProcDecl ProcMain params body) =
  modify' $ \st -> st{stMainProc = Just $ Proc params body}
discoverTopDecl (ProcDecl (ProcName (NameId n)) params body) =
  modify' $ \st@EvalState{..} -> st{stProcs = M.insert n (Proc params body) stProcs}

discoverGlobalDecl :: GlobalDecl -> Eval ()
discoverGlobalDecl (GDecl GkUniform (TyStruct _ fields) (Name NsU n)) =
  modify' $ \st@EvalState{..} ->
    st{globals = foldr (\(ty, m) -> addDecl NsU (toUniformId (n, m)) (defaultValue ty)) globals fields}
discoverGlobalDecl (GDecl GkOut ty n) =
  modify' $ \st@EvalState{..} -> st{globals = addDeclN n (defaultValue ty) globals}
discoverGlobalDecl (GDecl GkIn ty n) =
  modify' $ \st@EvalState{..} -> st{globals = addDeclN n (defaultValue ty) globals}
discoverGlobalDecl d =
  error $ "discoverGlobalDecl not implemented: " <> pp ppGlobalDecl d


evalMain :: Eval ()
evalMain = do
  Just mainProc <- stMainProc <$> get
  evalProc mainProc []
  return ()

newtype LocalState = LocalState
  { temps :: M.IntMap Value
  }

emptyLocals :: LocalState
emptyLocals = LocalState
  { temps = M.empty
  }

evalProc :: Proc -> [Value] -> Eval ()
evalProc (Proc _params ss) _args =
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
evalEmit lst EmitFragDepth = return lst
evalEmit lst (EmitPosition e) = do
  v <- evalExpr lst e
  modify' $ \st -> st{gl_Position = Just v}
  return lst

evalLocalDecl :: LocalState -> LocalDecl -> Eval LocalState
evalLocalDecl lst (LDecl ty (Name NsT -> n) Nothing) =
  let v = defaultValue ty in
  setValue lst n v
evalLocalDecl lst (LDecl ty (Name NsT -> n) (Just e)) = do
  v <- evalExpr lst e >>= evalCoerce ty
  setValue lst n v

evalExpr :: LocalState -> Expr -> Eval Value
evalExpr lst = \case
  BinaryExpr l op r -> do
    lv <- evalExprAtom lst l
    rv <- evalExprAtom lst r
    return $ evalBinaryOp lv op rv
  UnaryExpr op e -> do
    v <- evalExprAtom lst e
    return $ evalUnaryOp op v
  AtomExpr e -> evalExprAtom lst e
  FunCallExpr fun args -> do
    vals <- mapM (evalExprAtom lst) args
    PrimFuns.eval fun vals
  e -> fail $ "evalExpr not implemented: " <> pp ppExpr e

evalExprAtom :: LocalState -> ExprAtom -> Eval Value
evalExprAtom lst = \case
  LitFloatExpr _ f -> return $ FloatValue f
  LitIntExpr _ i   -> return $ IntValue i
  IdentifierExpr n -> getValue lst n
  SwizzleExpr n s  -> getValue lst (Name NsT n) >>= evalSwizzle s
  VecIndexExpr n s -> getValue lst n >>= evalSwizzle s
  UniformExpr n m  -> getValue lst (Name NsU $ toUniformId (n, m))
  e                -> fail $ "evalExpr not implemented: " <> pp ppExprAtom e

evalSwizzle :: Swizzle -> Value -> Eval Value
evalSwizzle X (Vec2Value v) = return $ FloatValue $ v ^. _x
evalSwizzle Y (Vec2Value v) = return $ FloatValue $ v ^. _y
evalSwizzle X (Vec3Value v) = return $ FloatValue $ v ^. _x
evalSwizzle Y (Vec3Value v) = return $ FloatValue $ v ^. _y
evalSwizzle Z (Vec3Value v) = return $ FloatValue $ v ^. _z
evalSwizzle X (Vec4Value v) = return $ FloatValue $ v ^. _x
evalSwizzle Y (Vec4Value v) = return $ FloatValue $ v ^. _y
evalSwizzle Z (Vec4Value v) = return $ FloatValue $ v ^. _z
evalSwizzle W (Vec4Value v) = return $ FloatValue $ v ^. _w
evalSwizzle s v = fail $ "cannot access " <> pp ppSwizzle s <> " on " <> show v

setValue :: LocalState -> Name -> Value -> Eval LocalState
setValue lst@LocalState{..} n@(Name NsT (NameId nId)) v =
  trace (pp ppName n <> " = " <> show v) $
  if isNaNValue v
    then fail $ pp ppName n <> " = " <> show v
    else return lst{temps = M.insert nId v temps}
setValue lst n v = do
  modify' $ \st@EvalState{..} -> st{globals = addDeclN n v globals}
  trace (pp ppName n <> " = " <> show v) $
    if isNaNValue v
      then fail $ pp ppName n <> " = " <> show v
      else return lst


getValue :: LocalState -> Name -> Eval Value
getValue LocalState{..} (Name NsT (NameId n)) = do
  let Just v = M.lookup n temps
  return v
getValue _ n = do
  v <- getDeclN n . globals <$> get
  case v of
    Nothing -> fail $ "undefined global: " <> pp ppName n
    Just ok -> return ok


main :: IO ()
main = do
  txt <- IO.readFile "../large-shaders/xax.frag"
  print $ eval txt
