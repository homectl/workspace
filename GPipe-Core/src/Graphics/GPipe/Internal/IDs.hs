{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.GPipe.Internal.IDs where

newtype WinId = WinId { unWinId :: Int }
    deriving (Num, Enum, Real, Eq, Ord, Integral)

newtype UniformId = UniformId { unUniformId :: Int }
    deriving (Num, Enum, Real, Eq, Ord, Integral)
instance Show UniformId where show = show . unUniformId

newtype SamplerId = SamplerId { unSamplerId :: Int }
    deriving (Num, Enum, Real, Eq, Ord, Integral)
instance Show SamplerId where show = show . unSamplerId
