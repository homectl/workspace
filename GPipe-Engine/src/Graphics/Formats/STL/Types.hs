{-# LANGUAGE OverloadedStrings #-}

module Graphics.Formats.STL.Types
       (
           STL(..),
           Triangle(..),
           Vector,
           triple,
       ) where

import qualified Data.ByteString as BS

-- | A representation of an STL file, consisting of a (possibly empty)
-- object name, and a list of triangles.
data STL = STL
    { name      :: BS.ByteString
    , triangles :: [Triangle]
    }

-- | A single triangle in STL is represented by a normal vector and
-- three vertices.
data Triangle = Triangle
    { normal   :: Maybe Vector
    , vertices :: (Vector, Vector, Vector)
    }

type Vector = (Float, Float, Float)

triple :: a -> a -> a -> (a, a, a)
triple a b c = (a, b, c)
