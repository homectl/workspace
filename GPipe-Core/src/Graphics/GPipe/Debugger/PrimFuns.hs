module Graphics.GPipe.Debugger.PrimFuns where

import Data.Fixed(mod')
import           Control.Monad                 ((>=>))
import           Graphics.GPipe.Debugger.Value
import           Graphics.GPipe.Linear
import           Graphics.GPipe.Expr(smoothstep, step, fract')
import           Graphics.GPipe.Optimizer.GLSL

getFloat :: Value -> Eval Float
getFloat = evalCoerce TyFloat >=> fromFloatValue
  where
    fromFloatValue (FloatValue v) = return v
    fromFloatValue v              = fail $ "not a float value: " <> show v

getV4 :: Value -> Eval (V4 Float)
getV4 = evalCoerce (TyVec 4) >=> fromVec4Value
  where
    fromVec4Value (Vec4Value v) = return v
    fromVec4Value v             = fail $ "not a V4 value: " <> show v


eval :: FunName -> [Value] -> Eval Value
eval PrimVec2 [x, y] =
  fmap Vec2Value $ V2
    <$> getFloat x
    <*> getFloat y
eval PrimVec3 [x, y, z] =
  fmap Vec3Value $ V3
    <$> getFloat x
    <*> getFloat y
    <*> getFloat z
eval PrimVec4 [x, y, z, w] =
  fmap Vec4Value $ V4
    <$> getFloat x
    <*> getFloat y
    <*> getFloat z
    <*> getFloat w

eval PrimMat4x4 [x, y, z, w] =
  fmap Mat4x4Value $ V4
    <$> getV4 x
    <*> getV4 y
    <*> getV4 z
    <*> getV4 w

eval PrimLength [Vec2Value a] = return $ FloatValue $ norm a
eval PrimLength [Vec3Value a] = return $ FloatValue $ norm a
eval PrimLength [Vec4Value a] = return $ FloatValue $ norm a

eval PrimNormalize [Vec2Value a] = return $ Vec2Value $ signorm a
eval PrimNormalize [Vec3Value a] = return $ Vec3Value $ signorm a
eval PrimNormalize [Vec4Value a] = return $ Vec4Value $ signorm a

eval PrimSqrt [a]             = FloatValue . sqrt <$> getFloat a
eval PrimSin [a]    = FloatValue . sin <$> getFloat a
eval PrimAsin [a]   = FloatValue . asin <$> getFloat a
eval PrimCos [a]    = FloatValue . cos <$> getFloat a
eval PrimAbs [a]    = FloatValue . abs <$> getFloat a
eval PrimFloor [a]  = IntValue . floor <$> getFloat a
eval PrimMod [a,b]  = fmap FloatValue $ mod' <$> getFloat a <*> getFloat b
eval PrimAtan [a,b] = fmap FloatValue $ atan2 <$> getFloat a <*> getFloat b

eval PrimFract [a]          = FloatValue . fract' <$> getFloat a
eval PrimSmoothstep [a,b,c] = fmap FloatValue $ smoothstep <$> getFloat a <*> getFloat b <*> getFloat c
eval PrimStep [a,b]         = fmap FloatValue $ step <$> getFloat a <*> getFloat b

-- eval _ (a:_) = return a
eval fun _ = fail $ "primfun not implemented: " <> pp ppFunName fun
