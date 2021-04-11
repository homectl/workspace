{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module Graphics.GPipe.GeometryStream.Shader2DSpec (spec) where

import           Test.Hspec      (Spec, it)

import           Control.Lens    ((^.))
import           Data.Function   ((&))
import           Graphics.GPipe
import           Test.ShaderTest (Shaders (Shaders))
import qualified Test.ShaderTest as S

type ShaderAttachment a = V3 a

--------------------------------------------------

topology :: Triangles
topology = TriangleList

vertices :: [(V3 Float, V3 Float, V3 Float)]
vertices =
  [ (V3 (-1) 1 0, 0, V3 1 0 0)
  , (V3 0 (-1) 0, 0, V3 0 1 0)
  , (V3 1 1 0, 0, V3 0 0 1)
  ]

--------------------------------------------------

vert
  :: (V3 VFloat, V3 VFloat, V3 VFloat)
  -> (VPos, ShaderAttachment VFloat)
vert (pos, _n, clr) = (point $ pos^._xyz / 2 - 0.25, clr / 10)

--------------------------------------------------

maxVertices :: Int
maxVertices = 6

geom
  :: GGenerativeGeometry Triangles (VPos, ShaderAttachment VFloat)
  -> Geometry Triangles (VPos, ShaderAttachment VFloat)
  -> GGenerativeGeometry Triangles (VPos, ShaderAttachment VFloat)
geom g (Triangle p1 p2 p3) = g
  & emitVertexPosition p1
  & emitVertexPosition p2
  & emitVertexPosition p3

  & emitVertexPosition (fst p1 + V4 1.0 0.5 0 0, snd p1 * 10)
  & emitVertexPosition (fst p1 + V4 0.5 0.5 0 0, snd p2 * 10)
  & emitVertexPosition (fst p1 + V4 0.5 0.5 0 0, snd p3 * 10)

  & endPrimitive

--------------------------------------------------

frag
  :: ShaderAttachment FFloat
  -> V3 FFloat
frag clr = clr

--------------------------------------------------

spec :: Spec
spec = do
  it "should show multiple colourful triangles" $ do
    S.main Shaders{topology, vertices, vert, maxVertices, geom, frag}
