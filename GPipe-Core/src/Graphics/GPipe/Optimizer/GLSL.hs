{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Graphics.GPipe.Optimizer.GLSL where

import           Control.Applicative              (Applicative (..), (<|>))
import           Data.Attoparsec.ByteString.Char8 (IResult (Partial), Parser,
                                                   char, decimal, endOfInput,
                                                   many1, option, parse,
                                                   parseOnly, rational,
                                                   scientific, sepBy1)
import           Data.List                        (intersperse)
import qualified Data.Scientific                  as Sci
import qualified Data.Text.Encoding               as T
import qualified Data.Text.Lazy                   as LT
import qualified Data.Text.Lazy.Builder           as LTB
import qualified Data.Text.Lazy.Builder.Int       as LTB
import qualified Data.Text.Lazy.Builder.RealFloat as LTB


parseShader :: Annot a => LT.Text -> Either String (GLSL a)
parseShader = parseOnly parseGLSL . T.encodeUtf8 . LT.toStrict

printShader :: Annot a => GLSL a -> LT.Text
printShader = LTB.toLazyText . ppGLSL


data GLSL a = GLSL Version [TopDecl a]
  deriving (Show, Functor, Foldable, Traversable)

parseGLSL :: Annot a => Parser (GLSL a)
parseGLSL = GLSL
  <$> parseVersion
  <*> ("\n" >> many1 parseTopDecl >>= (endOfInput >>) . pure)

ppGLSL :: Annot a => GLSL a -> LTB.Builder
ppGLSL (GLSL v decls) =
  ppVersion v
  <> "\n" <> ppL ppTopDecl decls

newtype Version = Version Int
  deriving (Show)

parseVersion :: Parser Version
parseVersion = Version <$> ("#version " >> decimal)

ppVersion :: Version -> LTB.Builder
ppVersion (Version v) = "#version " <> ppInt v

data TopDecl a
  = LayoutDecl LayoutSpec GlobalDecl
  | GlobalDecl GlobalDecl
  | ProcDecl ProcName [ParamDecl] [StmtAnnot a]
  deriving (Show, Functor, Foldable, Traversable)

parseTopDecl :: Annot a => Parser (TopDecl a)
parseTopDecl = layoutDecl <|> globalDecl <|> procDecl
  where
    layoutDecl = LayoutDecl
      <$> ("layout(" >> parseLayoutSpec)
      <*> (") " >> parseGlobalDecl)

    globalDecl = GlobalDecl
      <$> parseGlobalDecl

    procDecl = ProcDecl
      <$> ("void " >> parseProcName)
      <*> ("() " >> pure [])
      -- <*> ("{\n" >> many1 parseStmtAnnot)
      <*> ("{\n" >> many1 parseStmtAnnot >>= ("}\n" >>) . pure)

ppTopDecl :: Annot a => TopDecl a -> LTB.Builder
ppTopDecl (LayoutDecl e d) = "layout(" <> ppLayoutSpec e <> ") " <> ppGlobalDecl d
ppTopDecl (GlobalDecl d) = ppGlobalDecl d
ppTopDecl (ProcDecl n a b) =
  "void " <> ppProcName n
  <> "(" <> ppS "," ppParamDecl a <> ") {\n"
  <> ppL ppStmtAnnot b
  <> "}\n"

data ProcName
  = ProcMain
  | ProcName NameId
  deriving (Show)

parseProcName :: Parser ProcName
parseProcName =
  ("main" >> pure ProcMain)
  <|> ("p" >> ProcName <$> parseNameId)

ppProcName :: ProcName -> LTB.Builder
ppProcName ProcMain     = "main"
ppProcName (ProcName n) = "p" <> ppNameId n

data LayoutSpec
  = LayoutStd140
  | LayoutLocation Int
  deriving (Show)

parseLayoutSpec :: Parser LayoutSpec
parseLayoutSpec =
  ("std140" >> pure LayoutStd140)
  <|> ("location = " >> LayoutLocation <$> decimal)

ppLayoutSpec :: LayoutSpec -> LTB.Builder
ppLayoutSpec LayoutStd140       = "std140"
ppLayoutSpec (LayoutLocation l) = "location = " <> ppInt l

data ParamDecl
  = Param ParamKind LocalDecl
  deriving (Show)

parseParamDecl :: Parser ParamDecl
parseParamDecl = Param
  <$> parseParamKind
  <*> (" " >> parseLocalDecl)

ppParamDecl :: ParamDecl -> LTB.Builder
ppParamDecl (Param k d) =
  ppParamKind k <> " " <> ppLocalDecl d

data ParamKind
  = PkIn
  | PkOut
  | PkInout
  deriving (Show)

parseParamKind :: Parser ParamKind
parseParamKind = (char ' ' <|> pure ' ') >>
  ("in" >> return PkIn) <|>
  ("out" >> return PkOut) <|>
  ("inout" >> return PkInout)

ppParamKind :: ParamKind -> LTB.Builder
ppParamKind PkIn    = "in"
ppParamKind PkOut   = "out"
ppParamKind PkInout = "inout"

data LocalDecl
  = LDecl Type NameId (Maybe Expr)
  deriving (Show)

parseLocalDecl :: Parser LocalDecl
parseLocalDecl = LDecl
  <$> parseType
  <*> (" t" >> parseNameId)
  <*> (option Nothing (" = " >> Just <$> parseExpr) >>= (";\n" >>) . pure)

ppLocalDecl :: LocalDecl -> LTB.Builder
ppLocalDecl (LDecl t n Nothing) =
  ppType t
  <> " t" <> ppNameId n <> ";\n"
ppLocalDecl (LDecl t n (Just e)) =
  ppType t
  <> " t" <> ppNameId n
  <> " = " <> ppExpr e <> ";\n"

data GlobalDecl
  = GDecl GDeclKind Type Name
  deriving (Show)

parseGlobalDecl :: Parser GlobalDecl
parseGlobalDecl = GDecl
  <$> parseGDeclKind
  <*> (" " >> parseType)
  <*> (" " >> parseName >>= (";\n" >>) . pure)

ppGlobalDecl :: GlobalDecl -> LTB.Builder
ppGlobalDecl (GDecl k t n) =
  ppGDeclKind k
  <> " " <> ppType t
  <> " " <> ppName n <> ";\n"

data GDeclKind
  = GkIn
  | GkOut
  | GkUniform
  deriving (Show)

parseGDeclKind :: Parser GDeclKind
parseGDeclKind =
  ("in" >> return GkIn) <|>
  ("out" >> return GkOut) <|>
  ("uniform" >> return GkUniform)

ppGDeclKind :: GDeclKind -> LTB.Builder
ppGDeclKind GkIn      = "in"
ppGDeclKind GkOut     = "out"
ppGDeclKind GkUniform = "uniform"

data Type
  = TyBool
  | TyFloat
  | TySampler2D
  | TyVec Int
  | TyMat Int Int
  | TyStruct NameId [(Type, NameId)]
  deriving (Show, Eq)

parseType :: Parser Type
parseType =
  ("bool" >> return TyBool)
  <|> ("float" >> return TyFloat)
  <|> ("sampler2D" >> return TySampler2D)
  <|> ("vec" >> TyVec <$> decimal)
  <|> ("mat" >> TyMat <$> decimal <*> ("x" >> decimal))
  <|> tyStruct
  where
    tyStruct = TyStruct
      <$> ("uBlock" >> parseNameId)
      <*> (" {\n" >> many1 parseStructMember >>= ("}" >>) . pure)

    parseStructMember :: Parser (Type, NameId)
    parseStructMember = (,)
      <$> parseType
      <*> (" u" >> parseNameId >>= (";\n" >>) . pure)

ppType :: Type -> LTB.Builder
ppType TyBool = "bool"
ppType TyFloat = "float"
ppType TySampler2D = "sampler2D"
ppType (TyVec n) = "vec" <> ppInt n
ppType (TyMat n m) = "mat" <> ppInt n <> "x" <> ppInt m
ppType (TyStruct n ms) =
  "uBlock" <> ppNameId n
  <> " {\n" <> ppL ppStructMember ms <> "}"
  where ppStructMember (t, n) = ppType t <> " u" <> ppNameId n <> ";\n"

newtype NameId = NameId Int
  deriving (Show, Eq)

parseNameId :: Parser NameId
parseNameId = NameId
  <$> decimal

ppNameId :: NameId -> LTB.Builder
ppNameId (NameId n) = ppInt n

data Name
  = Name Namespace NameId
  deriving (Show)

parseName :: Parser Name
parseName = Name
  <$> parseNamespace
  <*> parseNameId

ppName :: Name -> LTB.Builder
ppName (Name ns n) = ppNamespace ns <> ppNameId n

data Namespace
  = NsT
  | NsS
  | NsU
  | NsVF
  | NsIn
  | NsOut
  deriving (Show, Eq)

parseNamespace :: Parser Namespace
parseNamespace =
  ("in" >> pure NsIn)
  <|> ("out" >> pure NsOut)
  <|> ("vf" >> pure NsVF)
  <|> (char 't' >> pure NsT)
  <|> (char 'u' >> pure NsU)
  <|> (char 's' >> pure NsS)

ppNamespace :: Namespace -> LTB.Builder
ppNamespace NsT   = "t"
ppNamespace NsS   = "s"
ppNamespace NsU   = "u"
ppNamespace NsVF  = "vf"
ppNamespace NsIn  = "in"
ppNamespace NsOut = "out"

data FunName
  = PrimMain
  | PrimMat3x3
  | PrimMat4x4
  | PrimVec2
  | PrimVec3
  | PrimVec4
  | PrimPow
  | PrimDot
  | PrimCos
  | PrimAtan
  | PrimMod
  | PrimAbs
  | PrimCross
  | PrimLength
  | PrimAsin
  | PrimSmoothstep
  | PrimStep
  | PrimFract
  | PrimFloor
  | PrimSin
  | PrimTan
  | PrimSqrt
  | PrimNormalize
  deriving (Show, Eq)

parseFunName :: Parser FunName
parseFunName =
  ("main" >> pure PrimMain)
  <|> ("mat3x3" >> pure PrimMat3x3)
  <|> ("mat4x4" >> pure PrimMat4x4)
  <|> ("vec2" >> pure PrimVec2)
  <|> ("vec3" >> pure PrimVec3)
  <|> ("vec4" >> pure PrimVec4)
  <|> ("pow" >> pure PrimPow)
  <|> ("dot" >> pure PrimDot)
  <|> ("cos" >> pure PrimCos)
  <|> ("atan" >> pure PrimAtan)
  <|> ("mod" >> pure PrimMod)
  <|> ("abs" >> pure PrimAbs)
  <|> ("cross" >> pure PrimCross)
  <|> ("length" >> pure PrimLength)
  <|> ("asin" >> pure PrimAsin)
  <|> ("smoothstep" >> pure PrimSmoothstep)
  <|> ("step" >> pure PrimStep)
  <|> ("fract" >> pure PrimFract)
  <|> ("floor" >> pure PrimFloor)
  <|> ("sin" >> pure PrimSin)
  <|> ("tan" >> pure PrimTan)
  <|> ("sqrt" >> pure PrimSqrt)
  <|> ("normalize" >> pure PrimNormalize)

ppFunName :: FunName -> LTB.Builder
ppFunName PrimMain       = "main"
ppFunName PrimMat3x3     = "mat3x3"
ppFunName PrimMat4x4     = "mat4x4"
ppFunName PrimVec2       = "vec2"
ppFunName PrimVec3       = "vec3"
ppFunName PrimVec4       = "vec4"
ppFunName PrimPow        = "pow"
ppFunName PrimDot        = "dot"
ppFunName PrimCos        = "cos"
ppFunName PrimAtan       = "atan"
ppFunName PrimMod        = "mod"
ppFunName PrimAbs        = "abs"
ppFunName PrimCross      = "cross"
ppFunName PrimLength     = "length"
ppFunName PrimAsin       = "asin"
ppFunName PrimSmoothstep = "smoothstep"
ppFunName PrimStep       = "step"
ppFunName PrimFract      = "fract"
ppFunName PrimFloor      = "floor"
ppFunName PrimSin        = "sin"
ppFunName PrimTan        = "tan"
ppFunName PrimSqrt       = "sqrt"
ppFunName PrimNormalize  = "normalize"

data Swizzle
  = X | Y | Z | W
  deriving (Show, Eq)

parseSwizzle :: Parser Swizzle
parseSwizzle =
  (char 'x' >> pure X)
  <|> (char 'y' >> pure Y)
  <|> (char 'z' >> pure Z)
  <|> (char 'w' >> pure W)

ppSwizzle :: Swizzle -> LTB.Builder
ppSwizzle X = "x"
ppSwizzle Y = "y"
ppSwizzle Z = "z"
ppSwizzle W = "w"

parseVecIndex :: Parser Swizzle
parseVecIndex =
  (char '0' >> pure X)
  <|> (char '1' >> pure Y)
  <|> (char '2' >> pure Z)
  <|> (char '3' >> pure W)

ppVecIndex :: Swizzle -> LTB.Builder
ppVecIndex X = "0"
ppVecIndex Y = "1"
ppVecIndex Z = "2"
ppVecIndex W = "3"

data Cast
  = Cast
  | NoCast
  deriving (Show)

data ExprAtom
  = LitIntExpr Cast Int
  | LitFloatExpr Cast Float
  | IdentifierExpr Name
  | UniformExpr NameId NameId
  | SwizzleExpr NameId Swizzle
  | VecIndexExpr Name Swizzle
  | MatIndexExpr Name Swizzle Swizzle
  deriving (Show)

parseExprAtom :: Parser ExprAtom
parseExprAtom =
  litNumber <$> scientific
  <|> LitIntExpr Cast <$> ("int(" >> decimal >>= (")" >>) . pure)
  <|> LitFloatExpr Cast <$> ("float(" >> rational >>= (")" >>) . pure)
  <|> UniformExpr <$> (char 'u' >> parseNameId) <*> (".u" >> parseNameId)
  <|> SwizzleExpr <$> (char 't' >> parseNameId) <*> (char '.' >> parseSwizzle)
  <|> MatIndexExpr <$> parseName <*> ("[" >> parseVecIndex) <*> ("][" >> parseVecIndex >>= ("]" >>) . pure)
  <|> VecIndexExpr <$> parseName <*> ("[" >> parseVecIndex >>= ("]" >>) . pure)
  <|> IdentifierExpr <$> parseName
  where
    litNumber s =
      let e = Sci.base10Exponent s
          c = Sci.coefficient s
      in if e >= 0
          then LitIntExpr NoCast (fromInteger (c * 10 ^ e))
          else LitFloatExpr NoCast (Sci.toRealFloat s)

ppExprAtom :: ExprAtom -> LTB.Builder
ppExprAtom (LitIntExpr Cast i)     = "int(" <> ppInt i <> ")"
ppExprAtom (LitIntExpr NoCast i)   = ppInt i
ppExprAtom (LitFloatExpr Cast n)   = "float(" <> ppFloat n <> ")"
ppExprAtom (LitFloatExpr NoCast r) = ppFloat r
ppExprAtom (IdentifierExpr n)      = ppName n
ppExprAtom (UniformExpr n m)       = "u" <> ppNameId n <> ".u" <> ppNameId m
ppExprAtom (SwizzleExpr n m)       = "t" <> ppNameId n <> "." <> ppSwizzle m
ppExprAtom (VecIndexExpr n i)      = ppName n <> "[" <> ppVecIndex i <> "]"
ppExprAtom (MatIndexExpr n i j)    = ppName n <> "[" <> ppVecIndex i <> "]" <> "[" <> ppVecIndex j <> "]"

data Expr
  = UnaryExpr UnaryOp ExprAtom
  | BinaryExpr ExprAtom BinaryOp ExprAtom
  | FunCallExpr FunName [ExprAtom]
  | TextureExpr ExprAtom ExprAtom ExprAtom
  | AtomExpr ExprAtom
  deriving (Show)

parseExpr :: Parser Expr
parseExpr =
  (char '(' >> operatorExpr >>= (char ')' >>) . pure)
  <|> textureExpr
  <|> funCallExpr
  <|> AtomExpr <$> parseExprAtom

  where
    operatorExpr =
      BinaryExpr <$> parseExprAtom <*> parseBinaryOp <*> parseExprAtom
      <|> UnaryExpr <$> parseUnaryOp <*> parseExprAtom

    textureExpr = TextureExpr
      <$> ("texture(" >> parseExprAtom)
      <*> (",vec2(" >> parseExprAtom)
      <*> ("," >> parseExprAtom >>= ("))" >>) . pure)

    funCallExpr = FunCallExpr
      <$> parseFunName
      <*> (char '(' >> sepBy1 parseExprAtom (char ',') >>= (char ')' >>) . pure)

ppExpr :: Expr -> LTB.Builder
ppExpr (AtomExpr e) = ppExprAtom e
ppExpr (UnaryExpr o e) = "(" <> ppUnaryOp o <> ppExprAtom e <> ")"
ppExpr (BinaryExpr l o r) = "(" <> ppExprAtom l <> ppBinaryOp o <> ppExprAtom r <> ")"
ppExpr (FunCallExpr n args) = ppFunName n <> "(" <> ppS "," ppExprAtom args <> ")"
ppExpr (TextureExpr t x y) = "texture(" <> ppExprAtom t <> ",vec2(" <> ppExprAtom x <> "," <> ppExprAtom y <> "))"

data BinaryOp
  = BOpPlus
  | BOpMinus
  | BOpMul
  | BOpDiv
  | BOpGE
  | BOpGT
  | BOpLE
  | BOpLT
  | BOpAnd
  | BOpOr
  deriving (Show, Eq)

parseBinaryOp :: Parser BinaryOp
parseBinaryOp =
  (char '+' >> pure BOpPlus)
  <|> (char '-' >> pure BOpMinus)
  <|> (char '*' >> pure BOpMul)
  <|> (char '/' >> pure BOpDiv)
  <|> (">=" >> pure BOpGE)
  <|> (">" >> pure BOpGT)
  <|> ("<=" >> pure BOpLE)
  <|> ("<" >> pure BOpLT)
  <|> ("&&" >> pure BOpAnd)
  <|> ("||" >> pure BOpOr)

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

data UnaryOp
  = UOpMinus
  | UOpNot
  deriving (Show, Eq)

parseUnaryOp :: Parser UnaryOp
parseUnaryOp =
  (char '-' >> pure UOpMinus)
  <|> (char '!' >> pure UOpMinus)

ppUnaryOp :: UnaryOp -> LTB.Builder
ppUnaryOp UOpMinus = "-"
ppUnaryOp UOpNot   = "!"

data StmtAnnot a = SA
  { annot   :: a
  , unAnnot :: Stmt a
  }
  deriving (Show, Functor, Foldable, Traversable)

instance Applicative StmtAnnot where
  pure a = SA a (pure a)
  liftA2 f a b = SA (f (annot a) (annot b)) $ liftA2 f (unAnnot a) (unAnnot b)

parseStmtAnnot :: Annot a => Parser (StmtAnnot a)
parseStmtAnnot = SA <$> parseAnnot <*> parseStmt

ppStmtAnnot :: Annot a => StmtAnnot a -> LTB.Builder
ppStmtAnnot (SA a s) =
  maybe "" (\ltb -> "// " <> ltb <> "\n") (ppAnnot a) <> ppStmt s

data Stmt a
  = AssignStmt Name Expr
  | DeclStmt LocalDecl
  | EmitStmt Emit
  | IfStmt NameId [StmtAnnot a] [StmtAnnot a]
  deriving (Show, Functor, Foldable, Traversable)

instance Applicative Stmt where
  -- Arbitrary decision because "pure" doesn't really make sense.
  pure _ = EmitStmt EmitFragDepth
  
  liftA2 f (IfStmt n t1 e1) (IfStmt _ t2 e2) = IfStmt n
    ((zipWith . liftA2) f t1 t2)
    ((zipWith . liftA2) f e1 e2)
  liftA2 _ (AssignStmt n e) _ = AssignStmt n e
  liftA2 _ (DeclStmt d) _ = DeclStmt d
  liftA2 _ (EmitStmt e) _ = EmitStmt e
  liftA2 _ (IfStmt n _ _) _ = IfStmt n [] []


parseStmt :: Annot a => Parser (Stmt a)
parseStmt =
  IfStmt <$> ("if(t" >> parseNameId >>= ("){\n" >>) . pure)
         <*> many1 parseStmtAnnot
         <*> ("} else {\n" >> many1 parseStmtAnnot >>= ("}\n" >>) . pure)
  <|> AssignStmt <$> parseName <*> (" = " >> parseExpr >>= (";\n" >>) . pure)
  <|> DeclStmt <$> parseLocalDecl
  <|> EmitStmt <$> parseEmit

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

data Emit
  = EmitPosition Expr
  | EmitFragDepth
  deriving (Show)

parseEmit :: Parser Emit
parseEmit =
  EmitPosition <$> ("gl_Position = " >> parseExpr >>= (";\n" >>) . pure)
  <|> ("gl_FragDepth = gl_FragCoord[2];\n" >> pure EmitFragDepth)

ppEmit :: Emit -> LTB.Builder
ppEmit (EmitPosition e) = "gl_Position = " <> ppExpr e <> ";\n"
ppEmit EmitFragDepth    = "gl_FragDepth = gl_FragCoord[2];\n"

ppInt :: Int -> LTB.Builder
ppInt = LTB.decimal

ppFloat :: Float -> LTB.Builder
ppFloat = LTB.realFloat

ppL :: (a -> LTB.Builder) -> [a] -> LTB.Builder
ppL printer = mconcat . map printer

ppS :: LTB.Builder -> (a -> LTB.Builder) -> [a] -> LTB.Builder
ppS sep printer = mconcat . intersperse sep . map printer

class Annot a where
  parseAnnot :: Parser a
  ppAnnot :: a -> Maybe LTB.Builder

instance Annot () where
  parseAnnot = pure ()
  ppAnnot = const Nothing

instance (Annot a, Annot b) => Annot (a, b) where
  parseAnnot = error "not implemented"
  ppAnnot (a, b) = do
    ppA <- ppAnnot a
    ppB <- ppAnnot b
    return $ "(" <> ppA <> ", " <> ppB <> ")"

----------------------------------

parseTest :: Show a => Parser a -> LT.Text -> IO ()
parseTest p input =
  let r = show . fromPartial . parse p . T.encodeUtf8 . LT.toStrict $ input in
  if length r > 600
    then
      let start = take 500 r
          end = reverse $ take 100 $ reverse r
      in
      putStrLn $ start <> " ... " <> end
    else putStrLn r
  where
    fromPartial (Partial cont) = cont mempty
    fromPartial r              = r

t :: Show a => Parser a -> String -> IO ()
t p = parseTest p . LT.pack

pp :: (a -> LTB.Builder) -> a -> String
pp printer = LT.unpack . LTB.toLazyText . printer

ppl :: (a -> LTB.Builder) -> [a] -> String
ppl printer = LT.unpack . LTB.toLazyText . ppL printer
