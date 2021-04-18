{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData                 #-}
module Graphics.GPipe.Debugger.Value where

import           Control.Monad.Trans.State.Strict (StateT)
import qualified Data.IntMap                      as M
import           Graphics.GPipe.Linear            (M44, V2, V3, V4, (!*), (!*!))
import           Language.GLSL.Decls   (Decls)
import           Language.GLSL.Types

data Proc
  = Proc [ParamDecl] [StmtAnnot ()]

data EvalState = EvalState
  { stProcs     :: M.IntMap Proc
  , stMainProc  :: Maybe Proc

  , globals     :: Decls Value
  , gl_Position :: Maybe Value
  }

newtype EvalResult a = EvalResult { fromResult :: Either String a }
  deriving (Functor, Applicative, Monad)

instance MonadFail EvalResult where
  fail = EvalResult . Left

type Eval = StateT EvalState EvalResult

data Value
  = FloatValue Float
  | IntValue Int
  | BoolValue Bool
  | Vec2Value (V2 Float)
  | Vec3Value (V3 Float)
  | Vec4Value (V4 Float)
  | Mat4x4Value (M44 Float)
  deriving (Show, Eq)


defaultValue :: Type -> Value
defaultValue TyFloat   = FloatValue 0
defaultValue (TyVec 2) = Vec2Value 0
defaultValue (TyVec 3) = Vec3Value 0
defaultValue (TyVec 4) = Vec4Value 0
defaultValue ty        = error $ "defaultValue not implemented: " <> pp ppType ty


isNaNValue :: Value -> Bool
isNaNValue (FloatValue v) = isNaN v
isNaNValue _              = False


roundValue :: Value -> Value
roundValue (FloatValue v) =
  FloatValue $ fromIntegral (round (v * 100000) :: Integer) / 100000
roundValue v = v


evalCoerce :: Type -> Value -> Eval Value
evalCoerce TyFloat v@FloatValue{} = return v
evalCoerce TyBool v@BoolValue{} = return v
evalCoerce TyFloat (IntValue i)   = return $ FloatValue (fromIntegral i)
evalCoerce (TyVec 2) v@Vec2Value{}     = return v
evalCoerce (TyVec 3) v@Vec3Value{}     = return v
evalCoerce (TyVec 4) v@Vec4Value{}     = return v
evalCoerce (TyMat 4 4) v@Mat4x4Value{} = return v
evalCoerce ty v                        = fail $ "coerce failed: " <> show (ty, v)


evalBinaryOp :: Value -> BinaryOp -> Value -> Value
evalBinaryOp (FloatValue l) BOpPlus (FloatValue r)  = FloatValue (l + r)
evalBinaryOp (FloatValue l) BOpMinus (FloatValue r) = FloatValue (l - r)
evalBinaryOp (FloatValue l) BOpMul (FloatValue r)   = FloatValue (l + r)
evalBinaryOp (FloatValue l) BOpDiv (FloatValue r)   = FloatValue (l / r)
evalBinaryOp (FloatValue l) BOpLE (FloatValue r)    = BoolValue (l <= r)
evalBinaryOp (FloatValue l) BOpGE (FloatValue r)    = BoolValue (l >= r)
evalBinaryOp (FloatValue l) BOpLT (FloatValue r)    = BoolValue (l < r)
evalBinaryOp (FloatValue l) BOpGT (FloatValue r)    = BoolValue (l > r)

evalBinaryOp (IntValue l) BOpPlus (IntValue r)      = IntValue (l + r)
evalBinaryOp (IntValue l) BOpMinus (IntValue r)     = IntValue (l - r)
evalBinaryOp (IntValue l) BOpMul (IntValue r)       = IntValue (l * r)

evalBinaryOp (Vec4Value l) BOpMul (Mat4x4Value r)   = Vec4Value (r !* l)
evalBinaryOp (Mat4x4Value l) BOpMul (Mat4x4Value r) = Mat4x4Value (r !*! l)

evalBinaryOp l@FloatValue{} o (IntValue r) = evalBinaryOp l o (FloatValue $ fromIntegral r)
evalBinaryOp (IntValue l) o r@FloatValue{} = evalBinaryOp (FloatValue $ fromIntegral l) o r

evalBinaryOp l o r =
  error $ "not implemented: " <> show (l, o, r)


evalUnaryOp :: UnaryOp -> Value -> Value
evalUnaryOp UOpMinus (FloatValue v) = FloatValue (-v)
evalUnaryOp UOpMinus (IntValue v) = IntValue (-v)

evalUnaryOp o e =
  error $ "not implemented: " <> show (o, e)
