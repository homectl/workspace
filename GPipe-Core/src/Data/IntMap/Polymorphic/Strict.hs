module Data.IntMap.Polymorphic.Strict
  ( module Data.IntMap.Polymorphic.Strict
  , IntMap
  ) where

import           Control.Arrow                (first, second)
import qualified Data.IntMap.Lazy             as M
import           Data.IntMap.Polymorphic.Type (IntMap (..))

empty :: IntMap k v
empty = IntMap M.empty

singleton :: Integral k => k -> v -> IntMap k v
singleton k = IntMap . M.singleton (fromIntegral k)

insert :: Integral k => k -> v -> IntMap k v -> IntMap k v
insert k v m = IntMap $ M.insert (fromIntegral k) v (rep m)

delete :: Integral k => k -> IntMap k v -> IntMap k v
delete k m = IntMap $ M.delete (fromIntegral k) (rep m)

updateLookupWithKey :: Integral k => (k -> v -> Maybe v) -> k -> IntMap k v -> (Maybe v, IntMap k v)
updateLookupWithKey f k m = second IntMap $ M.updateLookupWithKey (f . fromIntegral) (fromIntegral k) (rep m)

toList :: Integral k => IntMap k v -> [(k, v)]
toList = Prelude.map (first fromIntegral) . M.toList . rep

toAscList :: Integral k => IntMap k v -> [(k, v)]
toAscList = Prelude.map (first fromIntegral) . M.toAscList . rep

lookup :: Integral k => k -> IntMap k v -> Maybe v
lookup k m = M.lookup (fromIntegral k) (rep m)

(!) :: Integral k => IntMap k v -> k -> v
(!) m k = (M.!) (rep m) (fromIntegral k)

null :: IntMap k v -> Bool
null = M.null . rep

size :: IntMap k v -> Int
size = M.size . rep

union :: IntMap k v -> IntMap k v -> IntMap k v
union m1 m2 = IntMap $ M.union (rep m1) (rep m2)

map :: (a -> b) -> IntMap k a -> IntMap k b
map f = IntMap . M.map f . rep
