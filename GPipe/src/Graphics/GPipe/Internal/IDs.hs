{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.GPipe.Internal.IDs where

class Identifier a

newtype WinId = WinId { unWinId :: Int }
    deriving (Num, Enum, Real, Eq, Ord, Integral)

newtype UniformId = UniformId { unUniformId :: Int }
    deriving (Num, Enum, Real, Eq, Ord, Integral)
instance Show UniformId where show = show . unUniformId
instance Identifier UniformId
