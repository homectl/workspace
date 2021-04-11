{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
module Graphics.GPipe.Engine.STL (mustLoadSTL) where

import qualified Graphics.Formats.STL       as STL
import           Graphics.Formats.STL.Types (STL (..))
import           Graphics.GPipe             (V3 (..), cross, normalize)


mustLoadSTL :: FilePath -> IO [(V3 Float, V3 Float)]
mustLoadSTL = fmap stlToMesh . STL.mustLoadSTL


stlToMesh :: STL -> [(V3 Float, V3 Float)]
stlToMesh STL{triangles} = zip positions normals
  where
    toV3 :: STL.Vector -> V3 Float
    toV3 (x, y, z) = V3 x y z

    positions =
        concatMap (\STL.Triangle{STL.vertices=(v1, v2, v3)} ->
            [toV3 v1, toV3 v2, toV3 v3]) triangles

    normal (a, b, c) =
        normalize (cross edge1 edge2)
      where
        edge1 = b - a
        edge2 = c - b

    normals = (`concatMap` triangles) $ \case
        STL.Triangle{STL.normal=Just n}         -> replicate 3 $ toV3 n
        STL.Triangle{STL.vertices=(v1, v2, v3)} -> replicate 3 $ normal (toV3 v1, toV3 v2, toV3 v3)
