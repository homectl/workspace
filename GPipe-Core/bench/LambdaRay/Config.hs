module LambdaRay.Config where

import           Graphics.GPipe (V2, V3 (..))

data HorizonMode
    = HorizonGrid
    | HorizonBlack

data DiskMode a
    = DiskGrid
    | DiskSolid
    | DiskTexture (V2 a -> V3 a)

data SkyMode a
    = SkyBlack
    | SkyTexture (V2 a -> V3 a)

data DistortionMethod
    = MethodNone
    | MethodLeapFrog

data Config a = Config
    { horizonMode      :: HorizonMode
    , diskMode         :: DiskMode a
    , skyMode          :: SkyMode a
    , distortionMethod :: DistortionMethod
    , iterations       :: Int
    }

defaultConfig :: Fractional a => Config a
defaultConfig = Config
    { distortionMethod = MethodLeapFrog
    , horizonMode = HorizonBlack
    , diskMode = DiskSolid
    , skyMode = SkyBlack
    , iterations = 70
    }

data RuntimeConfig a = RuntimeConfig
    { stepsize   :: a
    , stepfactor :: a
    , time       :: a
    , cameraPos  :: V3 a
    }

defaultRuntimeConfig :: Fractional a => RuntimeConfig a
defaultRuntimeConfig = RuntimeConfig
    -- 0.4 is the maximum before you end up with lots of additional Einstein rings.
    { stepsize = 0.4
    , stepfactor = 1
    , time = 0
    , cameraPos = V3 0 1 (-20)
    }
