{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Graphics.SceneGraph.Types where

import           Data.Default         (Default (..))
import           Data.Graph.Inductive (Node, (&))
import qualified Data.Graph.Inductive as G
import qualified Data.Text            as T
import           Linear               (M44, V2 (..), V3 (..), V4 (..))


-- | Scene Graph based on a Graph
type SceneGraph = G.Gr SceneNode SceneLabel

-- | Empty label for scene graphs.
data SceneLabel = EmptyLabel
  deriving (Eq, Ord)

instance Show SceneLabel where
  show = const "()"

-- | Scene Node. Made up of data and maybe a widget
data SceneNode = SceneNode (Node, String) SceneData deriving Show

-- | Creates an empty scene graph
nullNode :: Node -> SceneNode
nullNode n = SceneNode (n, show n) Group

-- | Creates a scene graph containing the supplied node
trivialGr :: SceneNode -> SceneGraph
trivialGr n = ([], 1, n, []) & G.empty

-- | Scene Graph with indicate root node
type Scene = (SceneGraph, Node)

-- | View port refers to a camera node and has its own Scene which is drawn flattened
data Viewport = Viewport Node Scene

-- | A scene with a number of view ports looking onto it.
type World = (Scene, [Viewport])

instance Eq SceneNode where
  (SceneNode n _) == (SceneNode m _) = m == n

data KeyState
  = Up
  | Down
  deriving (Eq, Show)

type ClickHandler = Scene -> KeyState -> IO SceneGraph
type DragHandler = Scene -> V3 Float -> IO (SceneGraph, Float)

instance Show ClickHandler where
  show _ = "<a ClickHandler>"

instance Show DragHandler where
  show _ = "<a DragHandler>"

type Sink a = a -> IO ()

-- | Scene Node Data.
data SceneData
  = Group
  | Geode Geometry
  | LOD
  | MatrixTransform (M44 Float)
  | Switch Int
  | Material Phong
  | Handler (Maybe (ClickHandler, Sink ())) (Maybe (DragHandler, Sink Float))
  | Light
  | Camera
  | Texture FilePath
  | Text T.Text

instance Show SceneData where
  show Group               = "Group"
  show (Geode _)           = "Geode"
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

-- | Simple colours
data Colour
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

mapColour :: Colour -> V4 Float
mapColour Red       = V4 1 0 0 1
mapColour Green     = V4 0 1 0 1
mapColour Blue      = V4 0 0 1 1
mapColour Grey      = V4 0.4 0.4 0.4 1
mapColour LightBlue = V4 0.3 0.3 1.0 1
mapColour Black     = V4 0 0 0 1
mapColour White     = V4 1 1 1 1
mapColour Yellow    = V4 1 1 0 1
mapColour JustWhite = V4 0.9 0.9 0.9 1

-- | Phong colouring
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

-- | Convert from simple colour to Phong
colour2Phong :: Colour -> Phong
colour2Phong c = def
  { phDiffuse = Just $ mapColour c
  , phAmbient = Just $ mapColour c
  , phSpecular = Just $ V4 0.4 0.4 0.4 1.0
  , phShine = Just 5.0
  }
