{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Language.GLSL.PrettyPrint where

import           Data.List                        (intersperse)
import qualified Data.Text.Lazy                   as LT
import qualified Data.Text.Lazy.Builder           as LTB
import qualified Data.Text.Lazy.Builder.Int       as LTB
import qualified Data.Text.Lazy.Builder.RealFloat as LTB
import           Language.GLSL.AST


printShader :: Annot a => GLSL a -> LT.Text
printShader = LTB.toLazyText . ppGLSL

-- | Pretty-print GLSL

ppGLSL :: Annot a => GLSL a -> LTB.Builder
ppGLSL (GLSL v decls) =
  ppVersion v
  <> "\n" <> ppL ppTopDecl decls

-- | Pretty-print Version

ppVersion :: Version -> LTB.Builder
ppVersion (Version v) = "#version " <> ppInt v

-- | Pretty-print TopDecl

ppTopDecl :: Annot a => TopDecl a -> LTB.Builder
ppTopDecl (LayoutDecl e d) = "layout(" <> ppLayoutSpec e <> ") " <> ppGlobalDecl d
ppTopDecl (GlobalDecl d) = ppGlobalDecl d
ppTopDecl (ProcDecl n a b) =
  "void " <> ppProcName n
  <> "(" <> ppS "," ppParamDecl a <> ") {\n"
  <> ppL ppStmtAnnot b
  <> "}\n"

-- | Pretty-print ProcName

ppProcName :: ProcName -> LTB.Builder
ppProcName ProcMain     = "main"
ppProcName (ProcName n) = "p" <> ppNameId n

-- | Pretty-print LayoutSpec

ppLayoutSpec :: LayoutSpec -> LTB.Builder
ppLayoutSpec LayoutStd140       = "std140"
ppLayoutSpec (LayoutLocation l) = "location = " <> ppInt l

-- | Pretty-print ParamDecl

ppParamDecl :: ParamDecl -> LTB.Builder
ppParamDecl (Param k d) =
  ppParamKind k <> " " <> ppLocalDecl d

-- | Pretty-print ParamKind

ppParamKind :: ParamKind -> LTB.Builder
ppParamKind PkIn    = "in"
ppParamKind PkOut   = "out"
ppParamKind PkInout = "inout"

-- | Pretty-print LocalDecl

ppLocalDecl :: LocalDecl -> LTB.Builder
ppLocalDecl (LDecl t n Nothing) =
  ppType t
  <> " t" <> ppNameId n <> ";\n"
ppLocalDecl (LDecl t n (Just e)) =
  ppType t
  <> " t" <> ppNameId n
  <> " = " <> ppExpr e <> ";\n"

-- | Pretty-print GlobalDecl

ppGlobalDecl :: GlobalDecl -> LTB.Builder
ppGlobalDecl (GDecl k t n) =
  ppGDeclKind k
  <> " " <> ppType t
  <> " " <> ppName n <> ";\n"

-- | Pretty-print GDeclKind

ppGDeclKind :: GDeclKind -> LTB.Builder
ppGDeclKind GkIn      = "in"
ppGDeclKind GkOut     = "out"
ppGDeclKind GkUniform = "uniform"

-- | Pretty-print Type

ppType :: Type -> LTB.Builder
ppType TyBool = "bool"
ppType TyFloat = "float"
ppType TySampler2D = "sampler2D"
ppType (TyVec n) = "vec" <> ppInt n
ppType (TyMat n m) = "mat" <> ppInt n <> "x" <> ppInt m
ppType (TyStruct n ms) =
  "uBlock" <> ppNameId n
  <> " {\n" <> ppL ppStructMember ms <> "}"
  where ppStructMember (t, m) = ppType t <> " u" <> ppNameId m <> ";\n"

-- | Pretty-print NameId

ppNameId :: NameId -> LTB.Builder
ppNameId (NameId n) = ppInt n

-- | Pretty-print Name

ppName :: Name -> LTB.Builder
ppName (Name ns n) = ppNamespace ns <> ppNameId n

-- | Pretty-print Namespace

ppNamespace :: Namespace -> LTB.Builder
ppNamespace NsT   = "t"
ppNamespace NsS   = "s"
ppNamespace NsU   = "u"
ppNamespace NsVF  = "vf"
ppNamespace NsIn  = "in"
ppNamespace NsOut = "out"

-- | Pretty-print FunName

ppFunName :: FunName -> LTB.Builder
ppFunName PrimAbs        = "abs"
ppFunName PrimAsin       = "asin"
ppFunName PrimAtan       = "atan"
ppFunName PrimCos        = "cos"
ppFunName PrimCross      = "cross"
ppFunName PrimDot        = "dot"
ppFunName PrimFloor      = "floor"
ppFunName PrimFract      = "fract"
ppFunName PrimLength     = "length"
ppFunName PrimMat3x3     = "mat3x3"
ppFunName PrimMat4x4     = "mat4x4"
ppFunName PrimMod        = "mod"
ppFunName PrimNormalize  = "normalize"
ppFunName PrimPow        = "pow"
ppFunName PrimSin        = "sin"
ppFunName PrimSmoothstep = "smoothstep"
ppFunName PrimSqrt       = "sqrt"
ppFunName PrimStep       = "step"
ppFunName PrimTan        = "tan"
ppFunName PrimVec2       = "vec2"
ppFunName PrimVec3       = "vec3"
ppFunName PrimVec4       = "vec4"

-- | Pretty-print Swizzle

ppSwizzle :: Swizzle -> LTB.Builder
ppSwizzle X = "x"
ppSwizzle Y = "y"
ppSwizzle Z = "z"
ppSwizzle W = "w"

-- | Pretty-print VecIndex (Swizzle)

ppVecIndex :: Swizzle -> LTB.Builder
ppVecIndex X = "0"
ppVecIndex Y = "1"
ppVecIndex Z = "2"
ppVecIndex W = "3"

-- | Pretty-print NameExpr

ppNameExpr :: NameExpr -> LTB.Builder
ppNameExpr (NameExpr n)      = ppName n
ppNameExpr (UniformExpr n m) = "u" <> ppNameId n <> ".u" <> ppNameId m

-- | Pretty-print ExprAtom

ppExprAtom :: ExprAtom -> LTB.Builder
ppExprAtom (LitIntExpr Cast i)     = "int(" <> ppInt i <> ")"
ppExprAtom (LitIntExpr NoCast i)   = ppInt i
ppExprAtom (LitFloatExpr Cast n)   = "float(" <> ppFloat n <> ")"
ppExprAtom (LitFloatExpr NoCast r) = ppFloat r
ppExprAtom (IdentifierExpr n)      = ppNameExpr n
ppExprAtom (SwizzleExpr n m)       = "t" <> ppNameId n <> "." <> ppSwizzle m
ppExprAtom (VecIndexExpr n i)      = ppNameExpr n <> "[" <> ppVecIndex i <> "]"
ppExprAtom (MatIndexExpr n i j)    = ppNameExpr n <> "[" <> ppVecIndex i <> "]" <> "[" <> ppVecIndex j <> "]"

-- | Pretty-print Expr

ppExpr :: Expr -> LTB.Builder
ppExpr (AtomExpr e) = ppExprAtom e
ppExpr (UnaryExpr o e) = "(" <> ppUnaryOp o <> ppExprAtom e <> ")"
ppExpr (BinaryExpr l o r) = "(" <> ppExprAtom l <> ppBinaryOp o <> ppExprAtom r <> ")"
ppExpr (FunCallExpr n args) = ppFunName n <> "(" <> ppS "," ppExprAtom args <> ")"
ppExpr (TextureExpr t x y) = "texture(" <> ppExprAtom t <> ",vec2(" <> ppExprAtom x <> "," <> ppExprAtom y <> "))"

-- | Pretty-print BinaryOp

ppBinaryOp :: BinaryOp -> LTB.Builder
ppBinaryOp BOpPlus  = "+"
ppBinaryOp BOpMinus = "-"
ppBinaryOp BOpMul   = "*"
ppBinaryOp BOpDiv   = "/"
ppBinaryOp BOpGE    = ">="
ppBinaryOp BOpGT    = ">"
ppBinaryOp BOpLE    = "<="
ppBinaryOp BOpLT    = "<"
ppBinaryOp BOpAnd   = "&&"
ppBinaryOp BOpOr    = "||"

-- | Pretty-print UnaryOp

ppUnaryOp :: UnaryOp -> LTB.Builder
ppUnaryOp UOpMinus = "-"
ppUnaryOp UOpNot   = "!"

-- | Pretty-print StmtAnnot

ppStmtAnnot :: Annot a => StmtAnnot a -> LTB.Builder
ppStmtAnnot (SA a s) =
  maybe "" (\ltb -> "// " <> ltb <> "\n") (ppAnnot a) <> ppStmt s

-- | Pretty-print Stmt

ppStmt :: Annot a => Stmt a -> LTB.Builder
ppStmt (AssignStmt n e) = ppName n <> " = " <> ppExpr e <> ";\n"
ppStmt (DeclStmt d) = ppLocalDecl d
ppStmt (EmitStmt e) = ppEmit e
ppStmt (IfStmt c t e) =
  "if(t" <> ppNameId c <> "){\n"
  <> ppL ppStmtAnnot t
  <> "} else {\n"
  <> ppL ppStmtAnnot e
  <> "}\n"

-- | Pretty-print Emit

ppEmit :: Emit -> LTB.Builder
ppEmit (EmitPosition e) = "gl_Position = " <> ppExpr e <> ";\n"
ppEmit EmitFragDepth    = "gl_FragDepth = gl_FragCoord[2];\n"

-- | Pretty-printing utility

ppInt :: Int -> LTB.Builder
ppInt = LTB.decimal

ppFloat :: Float -> LTB.Builder
ppFloat = LTB.realFloat

ppL :: (a -> LTB.Builder) -> [a] -> LTB.Builder
ppL printer = mconcat . map printer

ppS :: LTB.Builder -> (a -> LTB.Builder) -> [a] -> LTB.Builder
ppS sep printer = mconcat . intersperse sep . map printer

----------------------------------
-- Pretty-printing to String

pp :: (a -> LTB.Builder) -> a -> String
pp printer = LT.unpack . LTB.toLazyText . printer

ppl :: (a -> LTB.Builder) -> [a] -> String
ppl printer = LT.unpack . LTB.toLazyText . ppL printer

pps :: LTB.Builder -> (a -> LTB.Builder) -> [a] -> String
pps sep printer = LT.unpack . LTB.toLazyText . ppS sep printer
