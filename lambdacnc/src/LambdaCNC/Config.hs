{-# LANGUAGE Arrows            #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies      #-}
module LambdaCNC.Config where

import           Control.Applicative          (Applicative (..))
import           Control.Arrow                (returnA)
import           Graphics.GPipe               (B, Buffer, BufferFormat (..),
                                               Uniform, UniformInput (..),
                                               V2 (..), V3 (..), V4 (..))
import qualified Graphics.GPipe.Engine.TimeIt as TimeIt


data Solids a = Solids
    { objBed    :: a
    , objGround :: a
    , objXAxis  :: a
    , objYAxis  :: a
    , objZAxis  :: a
    }
    deriving (Functor, Foldable, Traversable)

instance Applicative Solids where
    pure a = Solids a a a a a
    liftA2 f (Solids a1 a2 a3 a4 a5) (Solids b1 b2 b3 b4 b5) = Solids
        (f a1 b1)
        (f a2 b2)
        (f a3 b3)
        (f a4 b4)
        (f a5 b5)

instance TimeIt.Info (Solids a) where
    getInfo r = (r, (TimeIt.Done, "(" ++ show (length r) ++ " solids)"))

-------------------------------------------------

data GlobalUniforms a = GlobalUniforms
    { time       :: a
    , screenSize :: V2 a
    , cameraPos  :: V3 a
    , exposure :: a
    }

type GlobalUniformBuffer os = Buffer os (Uniform (GlobalUniforms (B Float)))

instance UniformInput a => UniformInput (GlobalUniforms a) where
    type UniformFormat (GlobalUniforms a) x = (GlobalUniforms (UniformFormat a x))
    toUniform = proc ~(GlobalUniforms a b c d) -> do
        (a', b', c', d') <- toUniform -< (a, b, c, d)
        returnA -< GlobalUniforms a' b' c' d'

instance BufferFormat a => BufferFormat (GlobalUniforms a) where
    type HostFormat (GlobalUniforms a) = GlobalUniforms (HostFormat a)
    toBuffer = proc ~(GlobalUniforms a b c d) -> do
        (a', b', c', d') <- toBuffer -< (a, b, c, d)
        returnA -< GlobalUniforms a' b' c' d'

defaultGlobalUniforms :: Fractional a => GlobalUniforms a
defaultGlobalUniforms = GlobalUniforms
    { time = 0
    , screenSize = V2 960 540
    , cameraPos = V3 90000 40000 20000 * 2
    , exposure = 1.0
    }


-------------------------------------------------

data ObjectUniforms a = ObjectUniforms
    { objectPos :: V3 a
    , objectColor :: V4 a
    , objectScale :: a
    }

type ObjectUniformBuffer os = Buffer os (Uniform (ObjectUniforms (B Float)))

instance UniformInput a => UniformInput (ObjectUniforms a) where
    type UniformFormat (ObjectUniforms a) x = (ObjectUniforms (UniformFormat a x))
    toUniform = proc ~(ObjectUniforms a b c) -> do
        (a', b', c') <- toUniform -< (a, b, c)
        returnA -< ObjectUniforms a' b' c'

instance BufferFormat a => BufferFormat (ObjectUniforms a) where
    type HostFormat (ObjectUniforms a) = ObjectUniforms (HostFormat a)
    toBuffer = proc ~(ObjectUniforms a b c) -> do
        (a', b', c') <- toBuffer -< (a, b, c)
        returnA -< ObjectUniforms a' b' c'

defaultObjectUniforms :: Num a => ObjectUniforms a
defaultObjectUniforms = ObjectUniforms
    { objectPos = pure 0
    , objectColor = pure 1
    , objectScale = 1
    }

-------------------------------------------------

data LightUniforms a = LightUniforms
    { lightPos   :: V3 a
    , lightColor :: V3 a
    }
    deriving (Show)

type LightUniformBuffer os = Buffer os (Uniform (LightUniforms (B Float)))

instance UniformInput a => UniformInput (LightUniforms a) where
    type UniformFormat (LightUniforms a) x = (LightUniforms (UniformFormat a x))
    toUniform = proc ~(LightUniforms a b) -> do
        a' <- toUniform -< a
        b' <- toUniform -< b
        returnA -< LightUniforms a' b'

instance BufferFormat a => BufferFormat (LightUniforms a) where
    type HostFormat (LightUniforms a) = LightUniforms (HostFormat a)
    toBuffer = proc ~(LightUniforms a b) -> do
        a' <- toBuffer -< a
        b' <- toBuffer -< b
        returnA -< LightUniforms a' b'
