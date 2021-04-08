{-# LANGUAGE ParallelListComp #-}
module Graphics.GPipe.Fonts.SimpleDistanceField where

import           Graphics.GPipe
import           Graphics.GPipe.Fonts.Common

type OutlineVertices = [([V2 Float], [V2 Float], [V2 Float])]

type FillTriangles = [V2 Float]

type OutlineTriangles = [(V2 Float, Float)]

letterOutlineVertices :: Float -> OutlineCurves -> OutlineVertices
letterOutlineVertices outlineThickness = map (offsetEdges . removeCollapsedEdges . duplicateLast)
  where
    miterLimit = 4

    duplicateLast xs = last xs : xs

    removeCollapsedEdges (v1:v2:vs) =
        if quadrance (v1 - v2) > outlineThickness * 0.001
        then v1 : removeCollapsedEdges (v2:vs)
        else removeCollapsedEdges (v1:vs)
    removeCollapsedEdges vs = vs

    offsetEdges vertices = (innerVertices, vertices, outerVertices)
      where
        outerVertices = [v - o ^* outlineThickness | v <- vertices | o <- cycle offsets]
        innerVertices = [v + o ^* outlineThickness | v <- vertices | o <- cycle offsets]
        edges = [v2 - v1 | v1 <- vertices | v2 <- tail vertices]
        edgeNormals = map (normalize . turn) edges
        vertexNormals = [normalize (n1 + n2) | n1 <- last edgeNormals : edgeNormals | n2 <- edgeNormals]
        offsets = [nv ^* min miterLimit (recip (dot nv ne)) | ne <- edgeNormals | nv <- vertexNormals]

letterFillTriangles :: OutlineVertices -> FillTriangles
letterFillTriangles = concatMap makePoly
  where
    makePoly (_, v:vs, _) = concat (zipWith makeTri (init vs) (tail vs))
      where
        makeTri v1 v2 = [v, v1, v2]

letterOutlineOuterTriangles :: OutlineVertices -> OutlineTriangles
letterOutlineOuterTriangles = concatMap makeQuads
  where
    makeQuads (i1:i2:is, m1:m2:ms, o1:o2:os) = (m1, m) : (o1, o) : (o2, o) : (o2, o) : (m2, m) : (m1, m) : makeQuads (i2:is, m2:ms, o2:os)
    makeQuads _ = []
    m = 0.5
    o = 0

letterOutlineInnerTriangles :: OutlineVertices -> OutlineTriangles
letterOutlineInnerTriangles = concatMap makeQuads
  where
    makeQuads (i1:i2:is, m1:m2:ms, o1:o2:os) = (i1, i) : (m1, m) : (m2, m) : (m2, m) : (i2, i) : (i1, i) : makeQuads (i2:is, m2:ms, o2:os)
    makeQuads _ = []
    i = 1
    m = 0.5

makeFillMesh :: FillTriangles -> Mesh
makeFillMesh fillTriangles = Mesh
    { meshPosition = fillTriangles
    , meshUV = []
    , meshDistance = []
    }

makeOutlineMesh :: OutlineTriangles -> Mesh
makeOutlineMesh outlineTriangles = Mesh
    { meshPosition = map fst outlineTriangles
    , meshUV = []
    , meshDistance = [d | (_, d) <- outlineTriangles]
    }
