{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData      #-}
module Graphics.GPipe.Optimizer.Decls where

import qualified Data.IntMap.Strict            as M
import           Graphics.GPipe.Optimizer.GLSL (Name (..), NameExpr (..),
                                                NameId (..), Namespace (..))


data Decls a = Decls
  { declsS   :: M.IntMap a
  , declsT   :: M.IntMap a
  , declsU   :: M.IntMap a
  , declsVF  :: M.IntMap a
  , declsIn  :: M.IntMap a
  , declsOut :: M.IntMap a
  }

emptyDecls :: Decls a
emptyDecls = Decls M.empty M.empty M.empty M.empty M.empty M.empty

addDecl :: Namespace -> NameId -> a -> Decls a -> Decls a
addDecl NsT (NameId n) v decls@Decls{..} = decls{declsT = M.insert n v declsT}
addDecl NsS (NameId n) v decls@Decls{..} = decls{declsS = M.insert n v declsS}
addDecl NsU (NameId n) v decls@Decls{..} = decls{declsU = M.insert n v declsU}
addDecl NsVF (NameId n) v decls@Decls{..} = decls{declsVF = M.insert n v declsVF}
addDecl NsIn (NameId n) v decls@Decls{..} = decls{declsIn = M.insert n v declsIn}
addDecl NsOut (NameId n) v decls@Decls{..} = decls{declsOut = M.insert n v declsOut}

addDeclN :: Name -> a -> Decls a -> Decls a
addDeclN (Name ns n) = addDecl ns n

addDeclNE :: NameExpr -> a -> Decls a -> Decls a
addDeclNE (NameExpr n)      = addDeclN n
addDeclNE (UniformExpr n m) = addDecl NsU (toUniformId (n, m))

getDecls :: Namespace -> Decls a -> M.IntMap a
getDecls NsT Decls{..}   = declsT
getDecls NsS Decls{..}   = declsS
getDecls NsU Decls{..}   = declsU
getDecls NsVF Decls{..}  = declsVF
getDecls NsIn Decls{..}  = declsIn
getDecls NsOut Decls{..} = declsOut

getDecl :: Namespace -> NameId -> Decls a -> Maybe a
getDecl ns (NameId n) decls = M.lookup n (getDecls ns decls)

getDeclN :: Name -> Decls a -> Maybe a
getDeclN (Name ns n) = getDecl ns n

getDeclNE :: NameExpr -> Decls a -> Maybe a
getDeclNE (NameExpr n)      = getDeclN n
getDeclNE (UniformExpr n m) = getDecl NsU (toUniformId (n, m))

toUniformId :: (NameId, NameId) -> NameId
toUniformId (NameId n, NameId m) = NameId $ n * 1000 + m

fromUniformId :: NameId -> (NameId, NameId)
fromUniformId (NameId i) = let (n, m) = i `divMod` 1000 in (NameId n, NameId m)

showUniformId :: NameId -> String
showUniformId i =
  let (n, m) = fromUniformId i in
  "u" <> show n <> ".u" <> show m
