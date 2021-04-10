{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Graphics.SceneGraph.Render where

import qualified Data.Graph.Inductive      as G
import           Graphics.SceneGraph.Types (Color (White), Phong (..),
                                            Scene (..), SceneData (..),
                                            SceneGraph, SceneNode (..),
                                            colorToPhong, llab)
import           Linear                    (M44, (!*!))
import qualified Linear                    as L


-- | Draw a scene graph (or a scenegraph fragment)
drawScene :: Monad m => Scene g -> (M44 Float -> Phong -> g -> m ()) -> m ()
drawScene = drawScene' L.identity (colorToPhong White)

drawScene' :: Monad m => M44 Float -> Phong -> Scene g -> (M44 Float -> Phong -> g -> m ()) -> m ()
drawScene' curMat curPhong (Scene gr n) drawGeode = do
  case llab gr n of
    SceneNode _ _ (MatrixTransform m) -> do
      recurse (m !*! curMat) curPhong
    SceneNode _ _ (Material phong) -> do
      recurse curMat phong
    SceneNode _ _ (Geode _ g) -> do
      drawGeode curMat curPhong g
      recurse curMat curPhong
    _ -> do
      -- TODO: implement
      recurse curMat curPhong

  where
    recurse nextMat nextPhong =
      mapM_ (\next -> drawScene' nextMat nextPhong (Scene gr next) drawGeode) (G.suc gr n)


mapSceneData :: (SceneData g1 -> SceneData g2) -> SceneGraph g1 -> SceneGraph g2
mapSceneData f =
  G.nmap (\(SceneNode nde lbl sd) -> SceneNode nde lbl (f sd))

foldSceneData :: (SceneData g -> a -> a) -> a -> SceneGraph g -> a
foldSceneData f =
  G.ufold (\(_, _, SceneNode _ _ sd, _) acc -> f sd acc)
