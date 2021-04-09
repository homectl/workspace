{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Graphics.SceneGraph.Render where

import qualified Data.Graph.Inductive      as G
import           Graphics.SceneGraph.Types (Scene, SceneData (..),
                                            SceneNode (..), llab)
import           Linear                    (M44, (!*!))
import qualified Linear                    as L


-- | Draw a scene graph (or a scenegraph fragment)
drawScene :: Monad m => Scene g -> (M44 Float -> g -> m ()) -> m ()
drawScene = drawScene' L.identity

drawScene' :: Monad m => M44 Float -> Scene g -> (M44 Float -> g -> m ()) -> m ()
drawScene' curMat (gr, n) drawGeode = do
  case llab gr n of
    SceneNode _ (MatrixTransform m) -> do
      recurse (m !*! curMat)
    SceneNode _ (Geode _ g) -> do
      drawGeode curMat g
      recurse curMat
    _ -> do
      -- TODO: implement
      recurse curMat

  where
    recurse nextMat =
      mapM_ (\next -> drawScene' nextMat (gr, next) drawGeode) (G.suc gr n)
