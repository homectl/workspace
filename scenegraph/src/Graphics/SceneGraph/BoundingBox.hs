module Graphics.SceneGraph.BoundingBox where

import           Control.Applicative       (liftA2)
import           Control.Lens              ((^.))
import           Data.Graph.Inductive      (Node)
import qualified Data.Graph.Inductive      as G
import           Data.Maybe                (fromMaybe)
import           Graphics.SceneGraph.Types
import           Linear                    (R3 (..), V3 (..), (!*))
import qualified Linear                    as L


-- | A box. Used for calculating bounds
type Box a = (V3 a, V3 a)

-- | Return the diagonal vector across the box corners.
boxSize :: Box Float -> V3 Float
boxSize (a, b) = b - a

-- | Bounds suitable for starting off with
smallBox :: Box Float
smallBox = (V3 (-0.1) (-0.1) (-0.1), V3 0.1 0.1 0.1)

-- | Create union of two boxes
union :: Box Float -> Box Float -> Box Float
union (v1,v2) (w1,w2) = (liftA2 min v1 w1, liftA2 max v2 w2)

bounds :: Scene g -> Box Float
bounds (Scene gr nde) =
  let sn = llab gr nde in
  boundsSceneNode gr sn

-- | Determine bounds of a @SceneNode@
boundsSceneNode :: SceneGraph g -> SceneNode g -> Box Float
boundsSceneNode gr (SceneNode nde _ (MatrixTransform mt)) =
  let (v1, v2) = boundsOfChildren gr nde in
  ((mt !* L.point v1) ^. _xyz, (mt !* L.point v2) ^. _xyz)

boundsSceneNode gr (SceneNode nde _ (Switch i)) =
  let nde' = G.suc gr nde !! i in
  bounds (Scene gr nde')

boundsSceneNode _ (SceneNode _ _ (Geode _ _)) = smallBox

boundsSceneNode gr (SceneNode nde _ _) = boundsOfChildren gr nde

boundsOfChildren :: SceneGraph g -> Node -> Box Float
boundsOfChildren gr =
  fromMaybe smallBox . foldr f Nothing . G.suc gr
  where
    f nde Nothing  = Just $ bounds (Scene gr nde)
    f nde (Just b) = Just $ b `union` bounds (Scene gr nde)
