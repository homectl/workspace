{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.IntMap.Polymorphic.Type where

import qualified Data.IntMap.Strict as M

newtype IntMap k v = IntMap { rep :: M.IntMap v }
  deriving (Semigroup, Monoid)
