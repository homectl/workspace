{-# LANGUAGE BinaryLiterals        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Language.GLSL.BitCode where

import           Data.Bits                   (shiftL, (.|.))
import qualified Data.Text.Lazy              as LT
import qualified Data.Text.Lazy.IO           as IO
import           Data.Word                   (Word64)
import           Debug.Trace                 (trace)
import           Language.GLSL.ConstExpr     (ConstExprs)
import qualified Language.GLSL.ConstExpr     as ConstExpr
import           Language.GLSL.Internal.Bits (B (..), expand, flat, zero)
import           Language.GLSL.Types         hiding (t)


assemble :: BitsStmt -> Word64
assemble bits = trace (show bits) $
  foldr (\(toInt -> a) b -> (a `shiftL` 1) .|. b) 0 $ flat bits

toInt :: B -> Word64
toInt O = 0
toInt I = 1

type BitsStmt = ((B,B), (BitsType, B, BitsExpr))
encodeStmt :: Maybe ConstExprs -> Stmt a -> [BitsStmt]
encodeStmt ce = \case
  AssignStmt _ e ->
    [(bitsAssignStmt, (zero, zero, encodeExpr ce e))]
  DeclStmt d ->
    [(bitsDeclStmt, encodeLocalDecl ce d)]
  EmitStmt e ->
    [(bitsEmitStmt, uncurry (zero,,) $ encodeEmit ce e)]
  IfStmt _ t e -> do
    [(bitsIfStmt, ((bitsThenStmt, zero), zero, zero))]
    ++ concatMap (encodeStmt ce . unAnnot) t
    ++ [(bitsIfStmt, ((bitsElseStmt, zero), zero, zero))]
    ++ concatMap (encodeStmt ce . unAnnot) e
    ++ [(bitsIfStmt, ((bitsEndifStmt, zero), zero, zero))]
  where
    -- 2 bits encode which statement this is.
    bitsAssignStmt     = (O,O)
    bitsDeclStmt       = (O,I)
    bitsEmitStmt       = (I,O)
    bitsIfStmt         = (I,I)

    -- For IfStmt, we need to know when "else" branch starts and when it's over.
    -- We encode that in the next 2 bits for the IfStmt encoding.
    bitsThenStmt       = (O,O)
    bitsElseStmt       = (O,I)
    bitsEndifStmt      = (I,O)

encodeLocalDecl :: Maybe ConstExprs -> LocalDecl -> (BitsType, B, BitsExpr)
encodeLocalDecl _ (LDecl ty _ Nothing) =
  -- 1 bit encodes whether the declaration has an initialiser.
  (encodeType ty, O, zero)
encodeLocalDecl ce (LDecl ty _ (Just e)) =
  (encodeType ty, I, encodeExpr ce e)

-- | 2 bits encode the type, then 2 bits encode the vec/mat size.
type BitsType = ((B,B), (B,B))
encodeType :: Type -> BitsType
encodeType = \case
  TyBool      -> (bitsTyBool, (O,O))
  TyFloat     -> (bitsTyFloat, (O,O))
  TyVec i     -> (bitsTyVec, encodeVecSize i i)
  TyMat i j   -> (bitsTyMat, encodeVecSize i j)
  TySampler2D -> error "no encoding for local sampler declarations"
  TyStruct{}  -> error "no encoding for local struct declarations"
  where
    -- 2 bits encode the type of a LocalDecl.
    bitsTyBool         = (O,O)
    bitsTyFloat        = (O,I)
    bitsTyVec          = (I,O)
    bitsTyMat          = (I,I)

encodeVecSize :: Int -> Int -> (B,B)
encodeVecSize i j | i /= j = error "no encoding for non-square matrices"
encodeVecSize i _ = case i of
  2 -> bitsVecSize2
  3 -> bitsVecSize3
  4 -> bitsVecSize4
  _ -> error $ "no encoding for vec/mat size: " <> show i
  where
    -- 2 bits encode the size of TyVec/TyMat. We only allow square matrices, so
    -- the size of mat2x2 has the same encoding as the size of vec2.
    bitsVecSize2       = (O,O)
    bitsVecSize3       = (O,I)
    bitsVecSize4       = (I,O)

encodeEmit :: Maybe ConstExprs -> Emit -> (B, BitsExpr)
encodeEmit _ EmitFragDepth     = (O,zero)
encodeEmit ce (EmitPosition e) = (I,encodeExpr ce e)

type BitsExpr = ((B,B), BitsFunName, BitsExprAtom, BitsExprAtom)
encodeExpr :: Maybe ConstExprs -> Expr -> BitsExpr
encodeExpr (Just ce) e | ConstExpr.isConstExpr ce e =
  -- Constant expressions are encoded as atom expressions with the "constant
  -- expression" operator 0b11.
  ((O,O), expand (I,I), zero, zero)
encodeExpr _ expr = case expr of
  UnaryExpr o e -> (bitsUnaryExpr, expand $ encodeUnaryOp o, encodeExprAtom e, zero)
  BinaryExpr l o r -> (bitsBinaryExpr, expand $ encodeBinaryOp o, encodeExprAtom l, encodeExprAtom r)
  TextureExpr t x y -> (bitsTextureExpr, zero, zero, zero)
  FunCallExpr f args -> uncurry (bitsFunCallExpr, encodeFunName f,,) $ encodeArgs f args
  AtomExpr e -> (bitsAtomExpr, expand bitsUOpIdentity, encodeExprAtom e, zero)
  where
    -- 2 bits encode the expression type.
    bitsUnaryExpr      = (O,O)
    bitsBinaryExpr     = (O,I)
    bitsFunCallExpr    = (I,O)
    bitsTextureExpr    = (I,I)
    -- AtomExpr is a encoded as UnaryExpr with a zero operator.
    bitsAtomExpr       = bitsUnaryExpr
    bitsUOpIdentity    = (O,O)

-- | Implements special encodings for functions with more than 2 args.

-- Because we can't encode all the args individually, we take shortcuts where we
-- know something about how a function tends to be called.
encodeArgs :: FunName -> [ExprAtom] -> (BitsExprAtom, BitsExprAtom)
encodeArgs PrimSmoothstep [a,b,c] | isLitExpr a && isLitExpr b =
  (encodeExprAtom c, zero)
encodeArgs PrimVec4 args@[a,_,_,d] | all isLitExpr args =
  (encodeExprAtom a, encodeExprAtom d)
encodeArgs PrimVec4 [a,b,c,d] | all isIdentifierExpr [a,b,c] && isLitExpr d =
  (encodeExprAtom a, encodeExprAtom d)
encodeArgs PrimVec4 args@[a,_,_,d] | all isIdentifierExpr args =
  (encodeExprAtom a, encodeExprAtom d)
encodeArgs PrimMat4x4 args@[a,_,_,d] | all isIdentifierExpr args =
  (encodeExprAtom a, encodeExprAtom d)

encodeArgs _ [] = (zero, zero)
encodeArgs _ [a] = (encodeExprAtom a, zero)
encodeArgs _ [a,b] = (encodeExprAtom a, encodeExprAtom b)
encodeArgs f args =
  error $ "unsupported argument list for '" <> pp ppFunName f <> "': "
        <> pps ", " ppExprAtom args

encodeUnaryOp :: UnaryOp -> (B,B)
encodeUnaryOp = \case
  UOpMinus -> bitsUOpMinus
  UOpNot   -> bitsUOpNot
  where
    -- 2 bits encode encode the unary operator. 0 is AtomExpr (no operator or
    -- unary "+", i.e. the identity operator).
    bitsUOpMinus       = (O,I)
    bitsUOpNot         = (I,O)

-- | 4 bits encode the binary operator.
type BitsBinaryOp = (B,B,B,B)
encodeBinaryOp :: BinaryOp -> BitsBinaryOp
encodeBinaryOp = \case
  BOpPlus  -> bitsBOpPlus
  BOpMinus -> bitsBOpMinus
  BOpMul   -> bitsBOpMul
  BOpDiv   -> bitsBOpDiv
  BOpGE    -> bitsBOpGE
  BOpGT    -> bitsBOpGT
  BOpLE    -> bitsBOpLE
  BOpLT    -> bitsBOpLT
  BOpAnd   -> bitsBOpAnd
  BOpOr    -> bitsBOpOr
  where
    bitsBOpPlus        = (O,O,O,O)
    bitsBOpMinus       = (O,O,O,I)
    bitsBOpMul         = (O,O,I,O)
    bitsBOpDiv         = (O,O,I,I)
    bitsBOpGE          = (O,I,O,O)
    bitsBOpGT          = (O,I,O,I)
    bitsBOpLE          = (O,I,I,O)
    bitsBOpLT          = (O,I,I,I)
    bitsBOpAnd         = (I,O,O,O)
    bitsBOpOr          = (I,O,O,I)

-- | 5 bits encode the function name for FunCallExpr.
type BitsFunName = (B,B,B,B,B)
encodeFunName :: FunName -> BitsFunName
encodeFunName = \case
  PrimAbs        -> bitsPrimAbs
  PrimAsin       -> bitsPrimAsin
  PrimAtan       -> bitsPrimAtan
  PrimCos        -> bitsPrimCos
  PrimCross      -> bitsPrimCross
  PrimDot        -> bitsPrimDot
  PrimFloor      -> bitsPrimFloor
  PrimFract      -> bitsPrimFract
  PrimLength     -> bitsPrimLength
  PrimMat3x3     -> bitsPrimMat3x3
  PrimMat4x4     -> bitsPrimMat4x4
  PrimMod        -> bitsPrimMod
  PrimNormalize  -> bitsPrimNormalize
  PrimPow        -> bitsPrimPow
  PrimSin        -> bitsPrimSin
  PrimSmoothstep -> bitsPrimSmoothstep
  PrimSqrt       -> bitsPrimSqrt
  PrimStep       -> bitsPrimStep
  PrimTan        -> bitsPrimTan
  PrimVec2       -> bitsPrimVec2
  PrimVec3       -> bitsPrimVec3
  PrimVec4       -> bitsPrimVec4
  where
    bitsPrimAbs        = (O,O,O,O,O)
    bitsPrimAsin       = (O,O,O,O,I)
    bitsPrimAtan       = (O,O,O,I,O)
    bitsPrimCos        = (O,O,O,I,I)
    bitsPrimCross      = (O,O,I,O,O)
    bitsPrimDot        = (O,O,I,O,I)
    bitsPrimFloor      = (O,O,I,I,O)
    bitsPrimFract      = (O,O,I,I,I)
    bitsPrimLength     = (O,I,O,O,O)
    bitsPrimMat3x3     = (O,I,O,O,I)
    bitsPrimMat4x4     = (O,I,O,I,O)
    bitsPrimMod        = (O,I,O,I,I)
    bitsPrimNormalize  = (O,I,I,O,O)
    bitsPrimPow        = (O,I,I,O,I)
    bitsPrimSin        = (O,I,I,I,O)
    bitsPrimSmoothstep = (O,I,I,I,I)
    bitsPrimSqrt       = (I,O,O,O,O)
    bitsPrimStep       = (I,O,O,O,I)
    bitsPrimTan        = (I,O,O,I,O)
    bitsPrimVec2       = (I,O,O,I,I)
    bitsPrimVec3       = (I,O,I,O,O)
    bitsPrimVec4       = (I,O,I,O,I)

-- | ExprAtom type is encoded in 3 bits.
encodeExprAtomType :: ExprAtom -> (B,B,B)
encodeExprAtomType = \case
  LitIntExpr{}     -> bitsLitExpr
  LitFloatExpr{}   -> bitsLitExpr
  IdentifierExpr{} -> bitsIdentifierExpr
  SwizzleExpr{}    -> bitsSwizzleExpr
  VecIndexExpr{}   -> bitsVecIndexExpr
  MatIndexExpr{}   -> bitsMatIndexExpr
  where
    -- Literals are encoded the same.
    bitsLitExpr        = (O,O,O)
    bitsIdentifierExpr = (O,O,I)
    bitsSwizzleExpr    = (O,I,O)
    bitsVecIndexExpr   = (O,I,I)
    bitsMatIndexExpr   = (I,O,O)

-- ExprAtom is encoded in 7 bits: 3 bits for type and 4 bits for arguments.
--
-- We only need the full 4 bits for MatIndexExpr arguments which can be any of
-- the 16 positions in the matrix indexing operation.
type BitsExprAtom = ((B,B,B), ((B,B), (B,B)))
encodeExprAtom :: ExprAtom -> BitsExprAtom
encodeExprAtom e = (encodeExprAtomType e, encodeOperand e)
  where
    encodeOperand (SwizzleExpr _ i)    = (encodeSwizzle i, (O,O))
    encodeOperand (VecIndexExpr _ i)   = (encodeSwizzle i, (O,O))
    encodeOperand (MatIndexExpr _ i j) = (encodeSwizzle i, encodeSwizzle j)
    encodeOperand _                    = ((O,O),(O,O))

-- Swizzle/VecIndex is encoded in 2 bits.
encodeSwizzle :: Swizzle -> (B,B)
encodeSwizzle = \case
  X -> bitsSwizzleX
  Y -> bitsSwizzleY
  Z -> bitsSwizzleZ
  W -> bitsSwizzleW
  where
    bitsSwizzleX       = (O,O)
    bitsSwizzleY       = (O,I)
    bitsSwizzleZ       = (I,O)
    bitsSwizzleW       = (I,I)


parse :: LT.Text -> Either String (GLSL ())
parse = parseShader

main :: IO ()
main = do
  putStrLn "Loading shader source..."
  -- inText <- IO.readFile "../large-shaders/lambdacnc.frag"
  -- inText <- IO.readFile "../large-shaders/lambdacnc.vert"
  -- inText <- IO.readFile "../large-shaders/lambdaray.frag"
  -- inText <- IO.readFile "../large-shaders/xax.frag"
  -- inText <- IO.readFile "../large-shaders/xax.vert"
  inText <- IO.readFile "../large-shaders/small.vert"
  putStrLn "Parsing shader source..."
  case parse inText of
    Left err -> writeFile "../opt.glsl" $ "// Error\n" <> err
    Right (GLSL _ (reverse -> (ProcDecl _ _ ss):_)) ->
      mapM_ (print . encodeStmt Nothing . unAnnot) ss
    Right _ ->
      return ()
