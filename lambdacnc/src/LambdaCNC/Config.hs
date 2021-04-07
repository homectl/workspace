{-# LANGUAGE Arrows            #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies      #-}
module LambdaCNC.Config where

import           Control.Applicative          (Applicative (..))
import           Control.Arrow                (returnA)
import           Data.Default                 (Default (..))
import           Graphics.GPipe               (B, Buffer, BufferFormat (..),
                                               Uniform, UniformInput (..),
                                               V2 (..), V3 (..))
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
    }

type GlobalUniformBuffer os = Buffer os (Uniform (GlobalUniforms (B Float)))

instance UniformInput a => UniformInput (GlobalUniforms a) where
    type UniformFormat (GlobalUniforms a) x = (GlobalUniforms (UniformFormat a x))
    toUniform = proc ~(GlobalUniforms a b c) -> do
        (a', b', c') <- toUniform -< (a, b, c)
        returnA -< GlobalUniforms a' b' c'

instance BufferFormat a => BufferFormat (GlobalUniforms a) where
    type HostFormat (GlobalUniforms a) = GlobalUniforms (HostFormat a)
    toBuffer = proc ~(GlobalUniforms a b c) -> do
        (a', b', c') <- toBuffer -< (a, b, c)
        returnA -< GlobalUniforms a' b' c'

defaultGlobalUniforms :: Fractional a => GlobalUniforms a
defaultGlobalUniforms = GlobalUniforms
    { time = 0
    , screenSize = V2 960 540
    , cameraPos = V3 90000 40000 20000 * 1.4
    }


-------------------------------------------------

newtype ObjectUniforms a = ObjectUniforms
    { objectPos :: V3 a
    }

type ObjectUniformBuffer os = Buffer os (Uniform (ObjectUniforms (B Float)))

instance UniformInput a => UniformInput (ObjectUniforms a) where
    type UniformFormat (ObjectUniforms a) x = (ObjectUniforms (UniformFormat a x))
    toUniform = proc ~(ObjectUniforms a) -> do
        a' <- toUniform -< a
        returnA -< ObjectUniforms a'

instance BufferFormat a => BufferFormat (ObjectUniforms a) where
    type HostFormat (ObjectUniforms a) = ObjectUniforms (HostFormat a)
    toBuffer = proc ~(ObjectUniforms a) -> do
        a' <- toBuffer -< a
        returnA -< ObjectUniforms a'

defaultObjectUniforms :: Default a => ObjectUniforms a
defaultObjectUniforms = ObjectUniforms
    { objectPos = pure def
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
