{-# LANGUAGE Arrows       #-}
{-# LANGUAGE TypeFamilies #-}
module LambdaCnc.Config where

import           Control.Arrow  (returnA)
import           Graphics.GPipe (B, Buffer, BufferFormat (..), Uniform,
                                 UniformInput (..), V3 (..))


data RuntimeConfig a = RuntimeConfig
    { time      :: a
    , cameraPos :: V3 a
    }

type UniformBuffer os = Buffer os (Uniform (RuntimeConfig (B Float)))

instance UniformInput a => UniformInput (RuntimeConfig a) where
    type UniformFormat (RuntimeConfig a) x = (RuntimeConfig (UniformFormat a x))
    toUniform = proc ~(RuntimeConfig a b) -> do
        (a', b') <- toUniform -< (a, b)
        returnA -< RuntimeConfig a' b'

instance BufferFormat a => BufferFormat (RuntimeConfig a) where
    type HostFormat (RuntimeConfig a) = RuntimeConfig (HostFormat a)
    toBuffer = proc ~(RuntimeConfig a b) -> do
        (a', b') <- toBuffer -< (a, b)
        returnA -< RuntimeConfig a' b'

defaultRuntimeConfig :: Fractional a => RuntimeConfig a
defaultRuntimeConfig = RuntimeConfig
    { time = 0
    , cameraPos = V3 0 1 (-20)
    }
