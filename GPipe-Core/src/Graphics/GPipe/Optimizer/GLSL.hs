{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Graphics.GPipe.Optimizer.GLSL where

import           Control.Applicative              ((<|>))
import           Data.Attoparsec.ByteString.Char8 (IResult (Partial), Parser,
                                                   char, decimal, endOfInput,
                                                   many1, option, parse,
                                                   parseOnly, rational, sepBy1)
import           Data.List                        (intersperse)
import qualified Data.Text.Encoding               as T
import qualified Data.Text.Lazy                   as LT
import qualified Data.Text.Lazy.Builder           as LTB
import qualified Data.Text.Lazy.Builder.Int       as LTB
import qualified Data.Text.Lazy.Builder.RealFloat as LTB


parseShader :: LT.Text -> Either String GLSL
parseShader = parseOnly parseGLSL . T.encodeUtf8 . LT.toStrict

printShader :: GLSL -> LT.Text
printShader = LTB.toLazyText . ppGLSL


data GLSL = GLSL Version [TopDecl]
  deriving (Show)

parseGLSL :: Parser GLSL
parseGLSL = GLSL
  <$> parseVersion
  <*> ("\n" >> many1 parseTopDecl >>= (endOfInput >>) . pure)

ppGLSL :: GLSL -> LTB.Builder
ppGLSL (GLSL v decls) =
  ppVersion v
  <> "\n" <> (mconcat . map ppTopDecl $ decls)

newtype Version = Version Int
  deriving (Show)

parseVersion :: Parser Version
parseVersion = Version <$> ("#version " >> decimal)

ppVersion :: Version -> LTB.Builder
ppVersion (Version v) = "#version " <> LTB.decimal v

data TopDecl
  = LayoutDecl LayoutSpec GlobalDecl
  | GlobalDecl GlobalDecl
  | FunDecl Type FunName [ParamDecl] [Stmt]
  deriving (Show)

parseTopDecl :: Parser TopDecl
parseTopDecl = layoutDecl <|> globalDecl <|> funDecl
  where
    layoutDecl = LayoutDecl
      <$> ("layout(" >> parseLayoutSpec)
      <*> (") " >> parseGlobalDecl)

    globalDecl = GlobalDecl
      <$> parseGlobalDecl

    funDecl = FunDecl
      <$> ("void " >> return TyVoid)
      <*> parseFunName
      <*> ("() " >> pure [])
      -- <*> ("{\n" >> many1 parseStmt)
      <*> ("{\n" >> many1 parseStmt >>= ("}\n" >>) . pure)

ppTopDecl :: TopDecl -> LTB.Builder
ppTopDecl (LayoutDecl e d) = "layout(" <> ppLayoutSpec e <> ") " <> ppGlobalDecl d
ppTopDecl (GlobalDecl d) = ppGlobalDecl d
ppTopDecl (FunDecl t n a b) =
  ppType t
  <> " " <> ppFunName n
  <> "(" <> (mconcat . intersperse "," . map ppParamDecl $ a) <> ") {\n"
  <> (mconcat . map ppStmt $ b)
  <> "}\n"

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
ppLayoutSpec (LayoutLocation l) = "location = " <> LTB.decimal l

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
  = LDecl Type Name (Maybe Expr)
  deriving (Show)

parseLocalDecl :: Parser LocalDecl
parseLocalDecl = LDecl
  <$> parseType
  <*> (" " >> parseName)
  <*> (option Nothing (" = " >> Just <$> parseExpr) >>= (";\n" >>) . pure)

ppLocalDecl :: LocalDecl -> LTB.Builder
ppLocalDecl (LDecl t n Nothing) =
  ppType t
  <> " " <> ppName n <> ";\n"
ppLocalDecl (LDecl t n (Just e)) =
  ppType t
  <> " " <> ppName n
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
parseGDeclKind = (char ' ' <|> pure ' ') >>
  ("in" >> return GkIn) <|>
  ("out" >> return GkOut) <|>
  ("uniform" >> return GkUniform)

ppGDeclKind :: GDeclKind -> LTB.Builder
ppGDeclKind GkIn      = "in"
ppGDeclKind GkOut     = "out"
ppGDeclKind GkUniform = "uniform"

data Type
  = TyVoid
  | TyBool
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
ppType TyVoid = "void"
ppType TyBool = "bool"
ppType TyFloat = "float"
ppType TySampler2D = "sampler2D"
ppType (TyVec n) = "vec" <> LTB.decimal n
ppType (TyMat n m) = "mat" <> LTB.decimal n <> "x" <> LTB.decimal m
ppType (TyStruct n ms) =
  "uBlock" <> ppNameId n
  <> " {\n" <> mconcat (map ppStructMember ms) <> "}"
  where ppStructMember (t, n) = ppType t <> " u" <> ppNameId n <> ";\n"

newtype NameId = NameId Int
  deriving (Show, Eq)

parseNameId :: Parser NameId
parseNameId = NameId
  <$> decimal

ppNameId :: NameId -> LTB.Builder
ppNameId (NameId n) = LTB.decimal n

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
  | NsUBlock
  deriving (Show, Eq)

parseNamespace :: Parser Namespace
parseNamespace =
  ("uBlock" >> pure NsUBlock)
  <|> ("in" >> pure NsIn)
  <|> ("out" >> pure NsOut)
  <|> ("vf" >> pure NsVF)
  <|> (char 't' >> pure NsT)
  <|> (char 'u' >> pure NsU)
  <|> (char 's' >> pure NsS)

ppNamespace :: Namespace -> LTB.Builder
ppNamespace NsT      = "t"
ppNamespace NsS      = "s"
ppNamespace NsU      = "u"
ppNamespace NsVF     = "vf"
ppNamespace NsIn     = "in"
ppNamespace NsOut    = "out"
ppNamespace NsUBlock = "uBlock"

data FunName
  = PrimMain
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
  | PrimAsin
  | PrimSmoothstep
  | PrimStep
  | PrimFract
  | PrimFloor
  | PrimSin
  | PrimTan
  | PrimSqrt
  | PrimFloat
  | PrimTexture
  | PrimNormalize
  deriving (Show, Eq)

parseFunName :: Parser FunName
parseFunName =
  ("main" >> pure PrimMain)
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
  <|> ("asin" >> pure PrimAsin)
  <|> ("smoothstep" >> pure PrimSmoothstep)
  <|> ("step" >> pure PrimStep)
  <|> ("fract" >> pure PrimFract)
  <|> ("floor" >> pure PrimFloor)
  <|> ("sin" >> pure PrimSin)
  <|> ("tan" >> pure PrimTan)
  <|> ("sqrt" >> pure PrimSqrt)
  <|> ("float" >> pure PrimFloat)
  <|> ("texture" >> pure PrimTexture)
  <|> ("normalize" >> pure PrimNormalize)

ppFunName :: FunName -> LTB.Builder
ppFunName PrimMain       = "main"
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
ppFunName PrimAsin       = "asin"
ppFunName PrimSmoothstep = "smoothstep"
ppFunName PrimStep       = "step"
ppFunName PrimFract      = "fract"
ppFunName PrimFloor      = "floor"
ppFunName PrimSin        = "sin"
ppFunName PrimTan        = "tan"
ppFunName PrimSqrt       = "sqrt"
ppFunName PrimFloat      = "float"
ppFunName PrimTexture    = "texture"
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

data Expr
  = LitIntExpr Int
  | LitFloatExpr Float
  | IdentifierExpr Name
  | UniformExpr NameId NameId
  | SwizzleExpr NameId Swizzle
  | IndexExpr Expr [Int]
  | ParenExpr Expr
  | UnaryExpr UnaryOp Expr
  | BinaryExpr Expr BinaryOp Expr
  | FunCallExpr FunName [Expr]
  deriving (Show)

parseExpr :: Parser Expr
parseExpr =
  BinaryExpr <$> indexExpr <*> parseBinaryOp <*> parseExpr
  <|> UnaryExpr <$> parseUnaryOp <*> parseExpr
  <|> indexExpr

  where
    indexExpr =
      IndexExpr <$> atomicExpr <*> many1 (char '[' >> decimal >>= (char ']' >>) . pure)
      <|> atomicExpr

    atomicExpr =
      LitFloatExpr <$> rational
      <|> LitIntExpr <$> decimal
      <|> FunCallExpr <$> parseFunName <*> args
      <|> UniformExpr <$> (char 'u' >> parseNameId) <*> (".u" >> parseNameId)
      <|> SwizzleExpr <$> (char 't' >> parseNameId) <*> (char '.' >> parseSwizzle)
      <|> IdentifierExpr <$> parseName
      <|> ParenExpr <$> (char '(' >> parseExpr >>= (char ')' >>) . pure)

    args = char '(' >> sepBy1 parseExpr (char ',') >>= (char ')' >>) . pure

ppExpr :: Expr -> LTB.Builder
ppExpr (LitIntExpr i) = LTB.decimal i
ppExpr (LitFloatExpr r) = LTB.realFloat r
ppExpr (IdentifierExpr n) = ppName n
ppExpr (UniformExpr n m) = "u" <> ppNameId n <> ".u" <> ppNameId m
ppExpr (SwizzleExpr n m) = "t" <> ppNameId n <> "." <> ppSwizzle m
ppExpr (IndexExpr n is) = ppExpr n <> (mconcat . map (\i -> "[" <> LTB.decimal i <> "]") $ is)
ppExpr (ParenExpr e) = "(" <> ppExpr e <> ")"
ppExpr (UnaryExpr o e) = ppUnaryOp o <> ppExpr e
ppExpr (BinaryExpr l o r) = ppExpr l <> ppBinaryOp o <> ppExpr r
ppExpr (FunCallExpr n args) = ppFunName n <> "(" <> (mconcat . intersperse "," . map ppExpr $ args) <> ")"

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

data Stmt
  = AssignStmt Name Expr
  | DeclStmt LocalDecl
  | EmitStmt Emit
  | IfStmt NameId [Stmt] [Stmt]
  deriving (Show)

parseStmt :: Parser Stmt
parseStmt =
  IfStmt <$> ("if(t" >> parseNameId >>= ("){\n" >>) . pure)
         <*> many1 parseStmt
         <*> ("} else {\n" >> many1 parseStmt >>= ("}\n" >>) . pure)
  <|> AssignStmt <$> parseName <*> (" = " >> parseExpr >>= (";\n" >>) . pure)
  <|> DeclStmt <$> parseLocalDecl
  <|> EmitStmt <$> parseEmit

ppStmt :: Stmt -> LTB.Builder
ppStmt (AssignStmt n e) = ppName n <> " = " <> ppExpr e <> ";\n"
ppStmt (DeclStmt d) = ppLocalDecl d
ppStmt (EmitStmt e) = ppEmit e
ppStmt (IfStmt c t e) =
  "if(t" <> ppNameId c <> "){\n"
  <> (mconcat . map ppStmt $ t)
  <> "} else {\n"
  <> (mconcat . map ppStmt $ e)
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
ppl printer = LT.unpack . LTB.toLazyText . mconcat . map printer
