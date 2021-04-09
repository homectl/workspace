{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.GPipe.Internal.IDs where

newtype WinId = WinId { unWinId :: Int }
    deriving (Num, Enum, Real, Eq, Ord, Integral)
