{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Graphics.SceneGraph.Types where

import           Data.Default         (Default (..))
import           Data.Graph.Inductive (Node, (&))
import qualified Data.Graph.Inductive as G
import qualified Data.Text            as T
import           Linear               (M44, V2 (..), V3 (..), V4 (..))


-- | Scene Graph based on a Graph
type SceneGraph g = G.Gr (SceneNode g) SceneEdge

-- | Empty edge label for scene graphs.
data SceneEdge = DefaultEdge
  deriving (Eq, Ord)

instance Show SceneEdge where
  show = const "()"

-- | Scene Node. Made up of data and maybe a widget
data SceneNode g = SceneNode
  { nodeId    :: Node
  , nodeLabel :: String
  , nodeData  :: SceneData g
  }
  deriving (Show)

-- | Creates an empty scene graph
nullNode :: Node -> SceneNode g
nullNode n = SceneNode n (show n) Group

-- | Creates a scene graph containing the supplied node
trivialGr :: SceneNode g -> SceneGraph g
trivialGr n = ([], 1, n, []) & G.empty

-- | Scene Graph with indicate root node
data Scene g = Scene
  { sceneGraph :: SceneGraph g
  , sceneRoot  :: Node
  }

-- | View port refers to a camera node and has its own Scene which is drawn flattened
data Viewport g = Viewport
  { viewCamera :: Node
  , viewScene  :: Scene g
  }

-- | A scene with a number of view ports looking onto it.
data World g = World
  { worldScene     :: Scene g
  , worldViewports :: [Viewport g]
  }

instance Eq (SceneNode g) where
  (SceneNode id1 lbl1 _) == (SceneNode id2 lbl2 _) = id1 == id2 && lbl1 == lbl2

data KeyState
  = Up
  | Down
  deriving (Eq, Show)

type ClickHandler g = Scene g -> KeyState -> IO (SceneGraph g)
type DragHandler g = Scene g -> V3 Float -> IO (SceneGraph g, Float)

instance Show (ClickHandler g) where
  show _ = "<a ClickHandler>"

instance Show (DragHandler g) where
  show _ = "<a DragHandler>"

type Sink a = a -> IO ()

-- | Scene Node Data.
data SceneData g
  = Group
  | Geode T.Text g
  | LOD
  | MatrixTransform (M44 Float)
  | Switch Int
  | Material Phong
  | Handler (Maybe (ClickHandler g, Sink ())) (Maybe (DragHandler g, Sink Float))
  | Light
  | Camera
  | Texture FilePath
  | Text T.Text

instance Show (SceneData g) where
  show Group               = "Group"
  show (Geode n _)         = "Geode " ++ show n
  show LOD                 = "LOD"
  show (MatrixTransform _) = "MatrixTransform"
  show (Switch i)          = "Switch " ++ show i
  show (Material _)        = "Material"
  show (Handler _ _)       = "Handler"
  show Light               = "Light"
  show Camera              = "Camera"
  show (Texture _)         = "Texture"
  show (Text t)            = "Text " ++ T.unpack t

-- | Geometry. Either a basic GL object or a mesh.
--
data Geometry
  = Mesh2D [V2 Float]
  | Mesh3D [(V3 Float, V3 Float)]
  deriving (Eq, Show)

-- | Simple colors
data Color
  = Grey
  | JustWhite
  | Red
  | Green
  | Blue
  | Black
  | LightBlue
  | White
  | Yellow
  deriving (Show, Eq)

mapColor :: Color -> V4 Float
mapColor Red       = V4 1 0 0 1
mapColor Green     = V4 0 1 0 1
mapColor Blue      = V4 0 0 1 1
mapColor Grey      = V4 0.4 0.4 0.4 1
mapColor LightBlue = V4 0.3 0.3 1.0 1
mapColor Black     = V4 0 0 0 1
mapColor White     = V4 1 1 1 1
mapColor Yellow    = V4 1 1 0 1
mapColor JustWhite = V4 0.9 0.9 0.9 1

-- | Phong lighting
data Phong = Phong
  { phEmission     :: Maybe (V4 Float)
  , phAmbient      :: Maybe (V4 Float)
  , phDiffuse      :: Maybe (V4 Float)
  , phSpecular     :: Maybe (V4 Float)
  , phShine        :: Maybe Float
  , phReflective   :: Maybe (V4 Float)
  , phReflectivity :: Maybe Float
  , phTransparent  :: Maybe (V4 Float)
  , phTransparency :: Maybe Float
  }
  deriving (Eq, Show)

instance Default Phong where
  def = Phong Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | Convert from simple color to Phong
colorToPhong :: Color -> Phong
colorToPhong c = def
  { phDiffuse = Just $ mapColor c
  , phAmbient = Just $ mapColor c
  , phSpecular = Just $ V4 0.4 0.4 0.4 1.0
  , phShine = Just 5.0
  }


llab :: SceneGraph g -> Node -> SceneNode g
llab gr n =
  case G.lab gr n of
    Nothing -> error $ "Should not happen gr=" ++ show gr ++ "n = " ++ show n
    Just n' -> n'
