{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE DeriveGeneric     #-}
module Language.GLSL.Parser where

import           Control.Applicative              ((<|>))
import           Data.Attoparsec.ByteString.Char8 (IResult (..), Parser, char,
                                                   decimal, endOfInput, many1,
                                                   option, parse, parseOnly,
                                                   rational, scientific, sepBy1)
import qualified Data.Scientific                  as Sci
import qualified Data.Text.Encoding               as T
import qualified Data.Text.Lazy                   as LT
import           Language.GLSL.AST


parseShader :: Annot a => LT.Text -> Either String (GLSL a)
parseShader = parseOnly parseGLSL . T.encodeUtf8 . LT.toStrict

-- | Parse GLSL

parseGLSL :: Annot a => Parser (GLSL a)
parseGLSL = GLSL
  <$> parseVersion
  <*> ("\n" >> many1 parseTopDecl >>= (endOfInput >>) . pure)

-- | Parse Version

parseVersion :: Parser Version
parseVersion = Version <$> ("#version " >> decimal)

-- | Parse TopDecl

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

-- | Parse ProcName

parseProcName :: Parser ProcName
parseProcName =
  ("main" >> pure ProcMain)
  <|> ("p" >> ProcName <$> parseNameId)

-- | Parse LayoutSpec

parseLayoutSpec :: Parser LayoutSpec
parseLayoutSpec =
  ("std140" >> pure LayoutStd140)
  <|> ("location = " >> LayoutLocation <$> decimal)

-- | Parse ParamDecl

parseParamDecl :: Parser ParamDecl
parseParamDecl = Param
  <$> parseParamKind
  <*> (" " >> parseLocalDecl)

-- | Parse ParamKind

parseParamKind :: Parser ParamKind
parseParamKind = (char ' ' <|> pure ' ') >>
  ("in" >> return PkIn) <|>
  ("out" >> return PkOut) <|>
  ("inout" >> return PkInout)

-- | Parse LocalDecl

parseLocalDecl :: Parser LocalDecl
parseLocalDecl = LDecl
  <$> parseType
  <*> (" t" >> parseNameId)
  <*> (option Nothing (" = " >> Just <$> parseExpr) >>= (";\n" >>) . pure)

-- | Parse GlobalDecl

parseGlobalDecl :: Parser GlobalDecl
parseGlobalDecl = GDecl
  <$> parseGDeclKind
  <*> (" " >> parseType)
  <*> (" " >> parseName >>= (";\n" >>) . pure)

-- | Parse GDeclKind

parseGDeclKind :: Parser GDeclKind
parseGDeclKind =
  ("in" >> return GkIn) <|>
  ("out" >> return GkOut) <|>
  ("uniform" >> return GkUniform)

-- | Parse Type

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

-- | Parse NameId

parseNameId :: Parser NameId
parseNameId = NameId
  <$> decimal

-- | Parse Name

parseName :: Parser Name
parseName = Name
  <$> parseNamespace
  <*> parseNameId

-- | Parse Namespace

parseNamespace :: Parser Namespace
parseNamespace =
  ("in" >> pure NsIn)
  <|> ("out" >> pure NsOut)
  <|> ("vf" >> pure NsVF)
  <|> (char 't' >> pure NsT)
  <|> (char 'u' >> pure NsU)
  <|> (char 's' >> pure NsS)

-- | Parse FunName

parseFunName :: Parser FunName
parseFunName =
  ("abs" >> pure PrimAbs)
  <|> ("asin" >> pure PrimAsin)
  <|> ("atan" >> pure PrimAtan)
  <|> ("cos" >> pure PrimCos)
  <|> ("cross" >> pure PrimCross)
  <|> ("dot" >> pure PrimDot)
  <|> ("floor" >> pure PrimFloor)
  <|> ("fract" >> pure PrimFract)
  <|> ("length" >> pure PrimLength)
  <|> ("mat3x3" >> pure PrimMat3x3)
  <|> ("mat4x4" >> pure PrimMat4x4)
  <|> ("mod" >> pure PrimMod)
  <|> ("normalize" >> pure PrimNormalize)
  <|> ("pow" >> pure PrimPow)
  <|> ("sin" >> pure PrimSin)
  <|> ("smoothstep" >> pure PrimSmoothstep)
  <|> ("sqrt" >> pure PrimSqrt)
  <|> ("step" >> pure PrimStep)
  <|> ("tan" >> pure PrimTan)
  <|> ("vec2" >> pure PrimVec2)
  <|> ("vec3" >> pure PrimVec3)
  <|> ("vec4" >> pure PrimVec4)

-- | Parse Swizzle

parseSwizzle :: Parser Swizzle
parseSwizzle =
  (char 'x' >> pure X)
  <|> (char 'y' >> pure Y)
  <|> (char 'z' >> pure Z)
  <|> (char 'w' >> pure W)

-- | Parse VecIndex (Swizzle)

parseVecIndex :: Parser Swizzle
parseVecIndex =
  (char '0' >> pure X)
  <|> (char '1' >> pure Y)
  <|> (char '2' >> pure Z)
  <|> (char '3' >> pure W)

-- | Parse NameExpr

parseNameExpr :: Parser NameExpr
parseNameExpr =
  UniformExpr <$> (char 'u' >> parseNameId) <*> (".u" >> parseNameId)
  <|> NameExpr <$> parseName

-- | Parse ExprAtom

parseExprAtom :: Parser ExprAtom
parseExprAtom =
  litNumber <$> scientific
  <|> LitIntExpr Cast <$> ("int(" >> decimal >>= (")" >>) . pure)
  <|> LitFloatExpr Cast <$> ("float(" >> rational >>= (")" >>) . pure)
  <|> SwizzleExpr <$> (char 't' >> parseNameId) <*> (char '.' >> parseSwizzle)
  <|> MatIndexExpr <$> parseNameExpr <*> ("[" >> parseVecIndex) <*> ("][" >> parseVecIndex >>= ("]" >>) . pure)
  <|> VecIndexExpr <$> parseNameExpr <*> ("[" >> parseVecIndex >>= ("]" >>) . pure)
  <|> IdentifierExpr <$> parseNameExpr
  where
    litNumber s =
      let e = Sci.base10Exponent s
          c = Sci.coefficient s
      in if e >= 0
          then LitIntExpr NoCast (fromInteger (c * 10 ^ e))
          else LitFloatExpr NoCast (Sci.toRealFloat s)

-- | Parse Expr

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

-- | Parse BinaryOp

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

-- | Parse UnaryOp

parseUnaryOp :: Parser UnaryOp
parseUnaryOp =
  (char '-' >> pure UOpMinus)
  <|> (char '!' >> pure UOpMinus)

-- | Parse StmtAnnot

parseStmtAnnot :: Annot a => Parser (StmtAnnot a)
parseStmtAnnot = SA <$> parseAnnot <*> parseStmt

-- | Parse Stmt

parseStmt :: Annot a => Parser (Stmt a)
parseStmt =
  IfStmt <$> ("if(t" >> parseNameId >>= ("){\n" >>) . pure)
         <*> many1 parseStmtAnnot
         <*> ("} else {\n" >> many1 parseStmtAnnot >>= ("}\n" >>) . pure)
  <|> AssignStmt <$> parseName <*> (" = " >> parseExpr >>= (";\n" >>) . pure)
  <|> DeclStmt <$> parseLocalDecl
  <|> EmitStmt <$> parseEmit

-- | Parse Emit

parseEmit :: Parser Emit
parseEmit =
  EmitPosition <$> ("gl_Position = " >> parseExpr >>= (";\n" >>) . pure)
  <|> ("gl_FragDepth = gl_FragCoord[2];\n" >> pure EmitFragDepth)

------------------

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
