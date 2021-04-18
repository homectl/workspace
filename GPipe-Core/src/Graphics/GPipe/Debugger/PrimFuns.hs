module Graphics.GPipe.Debugger.PrimFuns where

import           Control.Monad                 ((>=>))
import           Data.Fixed                    (mod')
import           Graphics.GPipe.Debugger.Value
import           Graphics.GPipe.Expr           (fract', smoothstep, step)
import           Graphics.GPipe.Linear
import           Graphics.GPipe.Optimizer.GLSL

flt :: Value -> Eval Float
flt = evalCoerce TyFloat >=> convert
  where
    convert (FloatValue v) = pure v
    convert v              = fail $ "not a Float value: " <> show v

v4 :: Value -> Eval (V4 Float)
v4 = evalCoerce (TyVec 4) >=> convert
  where
    convert (Vec4Value v) = pure v
    convert v             = fail $ "not a V4 value: " <> show v


eval :: FunName -> [Value] -> Eval Value
eval PrimVec2 [x, y] =
  fmap Vec2Value $ V2
    <$> flt x
    <*> flt y
eval PrimVec3 [x, y, z] =
  fmap Vec3Value $ V3
    <$> flt x
    <*> flt y
    <*> flt z
eval PrimVec4 [x, y, z, w] =
  fmap Vec4Value $ V4
    <$> flt x
    <*> flt y
    <*> flt z
    <*> flt w

eval PrimMat4x4 [x, y, z, w] =
  fmap Mat4x4Value $ V4
    <$> v4 x
    <*> v4 y
    <*> v4 z
    <*> v4 w

eval PrimLength [Vec2Value a] = pure $ FloatValue $ norm a
eval PrimLength [Vec3Value a] = pure $ FloatValue $ norm a
eval PrimLength [Vec4Value a] = pure $ FloatValue $ norm a

eval PrimNormalize [Vec2Value a] = pure $ Vec2Value $ signorm a
eval PrimNormalize [Vec3Value a] = pure $ Vec3Value $ signorm a
eval PrimNormalize [Vec4Value a] = pure $ Vec4Value $ signorm a

eval PrimSqrt   [a] = FloatValue      . sqrt   <$> flt a
eval PrimSin    [a] = FloatValue      . sin    <$> flt a
eval PrimAsin   [a] = FloatValue      . asin   <$> flt a
eval PrimCos    [a] = FloatValue      . cos    <$> flt a
eval PrimAbs    [a] = FloatValue      . abs    <$> flt a
eval PrimFloor  [a] = IntValue        . floor  <$> flt a
eval PrimFract  [a] = FloatValue      . fract' <$> flt a
eval PrimMod  [a,b] = fmap FloatValue $ mod'   <$> flt a <*> flt b
eval PrimAtan [a,b] = fmap FloatValue $ atan2  <$> flt a <*> flt b

eval PrimSmoothstep [a,b,c] = fmap FloatValue $ smoothstep <$> flt a <*> flt b <*> flt c
eval PrimStep         [a,b] = fmap FloatValue $ step <$> flt a <*> flt b

-- eval _ (a:_) = pure a
eval fun _ = fail $ "primfun not implemented: " <> pp ppFunName fun
