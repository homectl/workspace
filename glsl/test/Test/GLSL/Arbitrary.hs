{-# OPTIONS_GHC -Wno-orphans #-}
module Test.GLSL.Arbitrary where

import           Language.GLSL.Types
import           Test.QuickCheck.Arbitrary (Arbitrary (..), genericShrink)
import qualified Test.QuickCheck.Gen       as Gen


instance Arbitrary a => Arbitrary (Stmt a) where
  shrink = genericShrink
  arbitrary = Gen.sized $ \sz -> Gen.oneof
    [ AssignStmt <$> arbitrary <*> arbitrary
    , DeclStmt <$> arbitrary
    , EmitStmt <$> arbitrary
    , IfStmt <$> arbitrary
        <*> Gen.resize (sz`div`4) arbitrary
        <*> Gen.resize (sz`div`4) arbitrary
    ]

instance Arbitrary ExprAtom where
  shrink = genericShrink
  arbitrary = Gen.oneof
    [ LitIntExpr NoCast <$> arbitrary
    , IdentifierExpr <$> arbitrary
    , SwizzleExpr <$> arbitrary <*> arbitrary
    , VecIndexExpr <$> arbitrary <*> arbitrary
    , MatIndexExpr <$> arbitrary <*> arbitrary <*> arbitrary
    ]

instance Arbitrary Expr where
  shrink = filter isValidExpr . genericShrink
  arbitrary = Gen.oneof
    [ UnaryExpr <$> arbitrary <*> arbitrary
    , BinaryExpr <$> arbitrary <*> arbitrary <*> arbitrary
    , FunCallExpr <$> arbitrary <*> arbArgs
    -- , TextureExpr <$> arbitrary <*> arbitrary <*> arbitrary
    , AtomExpr <$> arbitrary
    ]
    where
      arbArgs :: Gen.Gen [ExprAtom]
      arbArgs = pure []

isValidExpr :: Expr -> Bool
isValidExpr (FunCallExpr f args) = length args == argCountForFunName f
isValidExpr _ = True

instance Arbitrary Emit where
  shrink = genericShrink
  arbitrary = Gen.oneof
    [ pure EmitFragDepth
    , EmitPosition <$> arbitrary
    ]



instance Arbitrary a => Arbitrary (StmtAnnot a) where
  shrink = genericShrink
  arbitrary = SA <$> arbitrary <*> arbitrary

instance Arbitrary LocalDecl where
  shrink = genericShrink
  arbitrary = LDecl <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Type where
  arbitrary = Gen.elements
    [ TyBool
    , TyFloat
    , TyVec 4
    , TyMat 4 4
    ]

instance Arbitrary NameId where
  shrink = genericShrink
  arbitrary = NameId <$> arbitrary

instance Arbitrary Namespace where
  arbitrary = pure NsT

instance Arbitrary Name where
  shrink = genericShrink
  arbitrary = Name <$> arbitrary <*> arbitrary

instance Arbitrary FunName where
  arbitrary = Gen.elements
    [ PrimAbs
    , PrimAsin
    ]

instance Arbitrary UnaryOp where
  arbitrary = Gen.elements
    [ UOpMinus
    , UOpNot
    ]

instance Arbitrary BinaryOp where
  arbitrary = Gen.elements
    [ BOpPlus
    , BOpMinus
    ]

instance Arbitrary Cast where
  shrink = genericShrink
  arbitrary = Gen.elements [NoCast, Cast]

instance Arbitrary NameExpr where
  shrink = genericShrink
  arbitrary = NameExpr <$> arbitrary

instance Arbitrary Swizzle where
  shrink = genericShrink
  arbitrary = Gen.elements [X, Y, Z, W]
