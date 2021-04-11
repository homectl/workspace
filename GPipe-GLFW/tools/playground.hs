{-# LANGUAGE Arrows          #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
module Main (main) where

import           Control.Arrow   (returnA)
import           Control.Lens    ((^.))
import           Data.Function   ((&))
import           Graphics.GPipe
import           Test.ShaderTest (Shaders (Shaders))
import qualified Test.ShaderTest as S

data ShaderAttachment a = ShaderAttachment
  { color  :: V3 a
  , normal :: V3 a
  }

instance FragmentInput a => FragmentInput (ShaderAttachment a) where
  type FragmentFormat (ShaderAttachment a) = ShaderAttachment (FragmentFormat a)
  toFragment = proc ~(ShaderAttachment a b) -> do
    a' <- toFragment -< a
    b' <- toFragment -< b
    returnA -< ShaderAttachment a' b'

instance FragmentCreator a => FragmentCreator (ShaderAttachment a) where
  createFragment = ShaderAttachment
    <$> createFragment
    <*> createFragment

instance AnotherVertexInput a => AnotherVertexInput (ShaderAttachment a) where
  toAnotherVertex = proc ~(ShaderAttachment a b) -> do
    a' <- toAnotherVertex -< a
    b' <- toAnotherVertex -< b
    returnA -< ShaderAttachment a' b'

instance AnotherFragmentInput a => AnotherFragmentInput (ShaderAttachment a) where
  toFragment2 = proc ~(ShaderAttachment a b) -> do
    a' <- toFragment2 -< a
    b' <- toFragment2 -< b
    returnA -< ShaderAttachment a' b'

instance GeometryExplosive a => GeometryExplosive (ShaderAttachment a) where
    exploseGeometry (ShaderAttachment a b) n = exploseGeometry (a, b) n
    declareGeometry ~(ShaderAttachment a b) = declareGeometry (a, b)
    enumerateVaryings ~(ShaderAttachment a b) = enumerateVaryings (a, b)

--------------------------------------------------

topology :: Points
topology = PointList

vertices :: [(V3 Float, V3 Float, V3 Float)]
vertices =
  [ (V3 (x / 5) (y / 5) 0, V3 0 0 1, V3 0.5 1 0.5)
  | x <- [-5..5], y <- [-5..5]
  ]

--------------------------------------------------

viewMat, projMat, projection :: M44 VFloat
viewMat = lookAt (V3 2 2 1) (V3 0 0 0) (V3 0 0 1)
projMat = perspective (pi/3) 1 1 100
projection = projMat !*! viewMat

vert
  :: (V3 VFloat, V3 VFloat, V3 VFloat)
  -> (VPos, ShaderAttachment VFloat)
vert (pos, normal, clr) = (fragPos, sa)
  where
    fragPos = projection !* point pos

    normalMat = transpose (inv44 viewMat)
    sa = ShaderAttachment{color=clr / 10, normal=(normalMat !* point normal) ^. _xyz}

--------------------------------------------------

maxVertices :: Int
maxVertices = 5

geom
  :: GGenerativeGeometry Triangles (VPos, ShaderAttachment VFloat)
  -> Geometry Points (VPos, ShaderAttachment VFloat)
  -> GGenerativeGeometry Triangles (VPos, ShaderAttachment VFloat)
geom g (Point (pos, sa@ShaderAttachment{..})) = g
  & emit (V4 (-0.05) (-0.1) 0 0) color
  & emit (V4   0.05  (-0.1) 0 0) color
  & emit (V4 (-0.05)   0.1  0 0) (color * 2)
  & emit (V4   0.05    0.1  0 0) (color * 2)
  & emit (V4   0.00    0.2  0 0) (color * 2.5)

  & endPrimitive
  where
    emit p c = emitVertexPosition ((pos + p), sa{color=c})

--------------------------------------------------

frag
  :: ShaderAttachment FFloat
  -> V3 FFloat
frag ShaderAttachment{..} = color + normal*0

--------------------------------------------------

main :: IO ()
main = S.main Shaders{topology, vertices, vert, maxVertices, geom, frag}
