module LambdaRay.Config where

data HorizonMode
    = HorizonGrid
    | HorizonBlack

horizonMode :: HorizonMode
horizonMode = HorizonBlack


data DiskMode
    = DiskGrid
    | DiskSolid

diskMode :: DiskMode
diskMode = DiskSolid


iterations :: Int
iterations = 150

stepsize :: Fractional a => a
stepsize = 0.2

