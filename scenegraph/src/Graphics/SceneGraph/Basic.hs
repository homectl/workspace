module Graphics.SceneGraph.Basic where

import           Control.Applicative        (liftA2)
import           Control.Lens               ((^.))
import           Control.Monad.Identity     (Identity)
import           Control.Monad.State        (lift)
import qualified Control.Monad.State        as ST
import           Data.Default               (Default (..))
import           Data.Graph.Inductive       (Node, (&))
import qualified Data.Graph.Inductive       as G
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text                  as T
import           Graphics.SceneGraph.Matrix (rotateM, rotatePostM, scaleM,
                                             translateM, translatePostM)
import           Graphics.SceneGraph.Types  (ClickHandler, Colour, DragHandler,
                                             KeyState (Down), Phong, Scene,
                                             SceneData (..), SceneGraph,
                                             SceneLabel (..), SceneNode (..),
                                             colour2Phong, nullNode)
import           Linear                     (M44, R1 (..), R3 (..), V3 (..),
                                             (!*!), (!*))
import qualified Linear                     as L


-- | Holds state of graph as it is built.
data OSGState = OSGState
  { graph     :: SceneGraph
  , context   :: [SceneNode]
  , startNode :: Int
  , root      :: Int
  }
  deriving (Eq, Show)

instance Default OSGState where
  def = OSGState emptyOSG [] 0 0

emptyState :: OSGState
emptyState = def

-- | The OSG monad within which construction of scene graphs occur.
-- was 'type OSGT m = ErrorT Throwable (ST.StateT OSGState m)'
type OSGT m = ST.StateT OSGState m

type OSG = OSGT Identity

-- | Create and run a OSG monad to return a scene graph and root node.
runOSG :: Monad m => OSGState -> OSGT m SceneNode -> m (SceneNode, OSGState, Node)
runOSG state f = do
  (ret, state') <- ST.runStateT f state
  return (ret, state', root state')


runOSGShow :: OSGT IO SceneNode -> IO ()
runOSGShow f = do
  (ret, state, i) <- runOSG emptyState f
  print (ret, state, i)


-- | Wrapper for running the OSG monad to return a scene graph and root node.
osg :: Monad m => OSGT m SceneNode -> m Scene
osg f = do
  (n, state, _) <- runOSG emptyState f
  return (graph state, idd n)


idd :: SceneNode -> Node
idd (SceneNode (i, _)  _) = i


-- | Basic add node
addNodeBasic :: Monad m => SceneNode -> OSGT m SceneNode
addNodeBasic nde = addNode nde []

-- | Add node with scene data
addBasicNode :: Monad m => SceneData -> OSGT m SceneNode
addBasicNode g = addNode (SceneNode (0, "") g) []

-- | Add node with scene data
addBasicNamedNode :: Monad m => String -> SceneData -> OSGT m SceneNode
addBasicNamedNode name g = addNode (SceneNode (0, name) g) []

-- | Add empty node
addNullNode :: Monad m => OSGT m SceneNode
addNullNode = addNodeBasic $ nullNode 0

-- | Add a node to a scene graph with supplied children
addNode :: Monad m => SceneNode -> [(SceneLabel, Node)] -> OSGT m SceneNode
addNode nde children = do
  s <- ST.get
  let (sn, s') = addNode' s nde children
  ST.put s'
  return sn

-- | Non-monadic form of addNode
addNode' :: OSGState -> SceneNode -> [(SceneLabel, Node)] -> (SceneNode, OSGState)
addNode' s (SceneNode (m, l) d) children =
  let n = if m == 0 then startNode s + 2 else m
      sn = SceneNode (n, l) d
      g' = ([], n, sn, children) & graph s
      s' = s { graph = g', startNode = n, root = n }
  in (sn, s')

-- | Replace a Scene Node
replaceNode :: Monad m => SceneNode -> OSGT m SceneNode
replaceNode n = do
  s <- ST.get
  g' <- lift $ replaceNode' (graph s) n
  ST.put (s { graph = g' })
  return n

-- | Inner monad version of replace node
replaceNode' :: Monad md => SceneGraph -> SceneNode -> md SceneGraph
replaceNode' gr nn = return $ replaceNode'' gr nn

-- | Actually does the job of replacing node in a scene graph
replaceNode'' :: SceneGraph -> SceneNode -> SceneGraph
replaceNode'' gr nn =
  let (m, gr') = G.match (idd nn) gr in
  case m of
    Nothing           -> gr
    Just (i, n, _, o) -> (i, n, nn, o) & gr'

-- | Run the monad but keep it in the family.
runOSGL :: Monad m => OSGState -> OSGT m SceneNode -> OSGT m (SceneNode, OSGState, Node)
runOSGL s n = lift $ runOSG s n

-- | Run the monad but keep it in the family.
runOSGL' :: Monad m => OSGT m SceneNode -> OSGT m (SceneNode, Node)
runOSGL' n = do
  s <- ST.get
  (n1, s', i) <- runOSGL s n
  ST.put s'
  return (n1, i)

-- | Perform a function on a scene node
doOnNode :: Monad m => OSGT m SceneNode -> (SceneNode -> SceneNode) -> OSGT m SceneNode
doOnNode n f = do
  s <- ST.get
  (anode, s', _) <- runOSGL s n
  ST.put s'
  replaceNode (f anode)

-- | Create a light
light :: Monad m => OSGT m SceneNode
light = addBasicNode Light

-- | Create a camera
camera :: Monad m => OSGT m SceneNode
camera = addBasicNode Camera

fi :: (Integral a, Integral b) => a -> b
fi = fromIntegral


-- plane' :: Int -> ([(PrimitiveMode, Int, Int)],[VectorD],[VectorD])
-- plane' w = foldr (\ (a1,a2,a3) (b1,b2,b3) -> (a1:b1,a2++b2,a3++b3))  ([],[],[])    [ up w xs | xs <- [0..(w-1)]]

-- up :: Int -> Int -> ( (PrimitiveMode,Int,Int), [VectorD],[VectorD])
-- up w xs = ( (TriangleStrip, (xs*(w*2+2))+1, w*2+2),  [ fromList [(fi x),(fi y),0] | y <- [0..w],x <-[xs..(xs+1)]], [ fromList [0,0,1] |  x <-[1..2], y <- [0..w]])

-- -- | Create a plane
-- planeT ::  Monad m => Int -> OSGT m SceneNode
-- planeT w = addBasicNode (Geode $ Mesh1 a b c) where (a,b,c) = plane' w

-- -- | Create a quad mesh
-- quad :: (Int,Int) ->  ([(PrimitiveMode, Int, Int)],[VectorD],[VectorD])
-- quad (x,y) = ( [(Quads,1,100) ], [ fromList[x',y',0], fromList[(x'+1),y',0], fromList[(x'+1),(y'+1),0],
--                  fromList [x',(y'+1),0]], [fromList [0,0,1] | i <- [0..3]])
--              where x' = fi x
--                    y' = fi y

-- planeq' w = foldr (\ (a1,a2,a3) (b1,b2,b3) -> (a1,a2++b2,a3++b3))  ([],[],[])  [ quad (x,y) | x <- [0..(w-1)], y <- [0..(w-1)]]

-- plane ::  Monad m => Int -> OSGT m SceneNode
-- plane w = addBasicNode (Geode $ Mesh1 a b c) where (a,b,c) = planeq' w

-- planeQ ::  Monad m => Int -> OSGT m SceneNode
-- planeQ = plane


-- -- | Create a node containing a torus.
-- torus :: Monad m => Float -> OSGT m SceneNode
-- torus i =  addBasicNode  (Geode $ GLObj $ GL.Torus (realToFrac i) (realToFrac (r*2)) 50 50)

-- -- | Create a node containing a sphere
-- sphere :: Monad m => Float -> OSGT m SceneNode
-- sphere r =  addBasicNode  (Geode $ GLObj $ GL.Sphere' (realToFrac r) 50 50)

-- -- | Create a node containing a tetrahedron
-- tetra :: Monad m => OSGT m SceneNode
-- tetra =  addBasicNode  (Geode $ GLObj $ GL.Tetrahedron)

-- -- | Create a node containing a line
-- line :: Monad m => VectorD -> VectorD -> OSGT m SceneNode
-- line p q = addBasicNode (Geode $ Mesh1 [(Lines,1,2)] [p,q] [v1,v1] )

-- -- | Create a node containing a cube.
-- -- Fixme: Faces are not orientated same way.
-- cube :: Monad m => GLdouble -> OSGT m SceneNode
-- cube i = addBasicNode (Geode $ Mesh1 [ (Quads,1,6) ] (map fromList [
-- 	   [md,md,md], [d,md,md],  [d,d,md], [md,d,md], -- Z
-- 	   [md,d,d], [d,d,d],  [d,md,d], [md,md,d],
-- 	   [d,md,md], [d,d,md], [d,d,d],[d,md,d],       -- X
-- 	   [md,md,d], [md,d,d], [md,d,md],[md,md,md],
-- 	   [md,d,md], [d,d,md], [d,d,d], [md,d,d],      -- Y
-- 	   [md,md,md], [d,md,md], [d,md,d], [md,md,d]
-- 	])
-- 	(map fromList [
-- 	   [0,0,1],  [0,0,1],  [0,0,1],  [0,0,1],
-- 	   [0,0,mu],  [0,0,mu],  [0,0,mu],  [0,0,mu],
-- 	   [mu,0,0],  [mu,0,0],  [mu,0,0],  [mu,0,0],
-- 	   [1,0,0],  [1,0,0],  [1,0,0],  [1,0,0],
-- 	   [0,mu,0], [0,mu,0],[0,mu,0],[0,mu,0],
-- 	   [0,1,0],[0,1,0],[0,1,0],[0,1,0]
-- 	]))  where (d,md,mu) = (i/2,(-i/2),(-1))



-- -- | Create cylinder as a BezierMesh
-- cylinder :: Monad m => GLfloat -> GLfloat -> OSGT m SceneNode
-- cylinder r h = addBasicNode $ Geode $ BezierMesh $ [
--   [ (let z=z'*h in [Vertex3 0 (-r) z, Vertex3 (-d) (-r)  z, Vertex3 (-r) (-d) z, Vertex3 (-r) 0 z ]) | z' <- [0..1]],
--   [ (let z=z'*h in[Vertex3 (-r) 0 z, Vertex3 (-r) d z    , Vertex3 (-d) r z   , Vertex3 0 r z    ]) | z' <- [0..1]] ,
--   [ (let z=z'*h in[Vertex3 0 r z   , Vertex3 d r z       , Vertex3 r d z      , Vertex3 r 0 z    ]) | z' <- [0..1]],
--   [ (let z=z'*h in[Vertex3 r 0 z   , Vertex3 r (-d) z    , Vertex3 d (-r) z   , Vertex3 0 (-r) z ]) | z' <- [0..1]]]
--  where d = 0.66 * r

-- | Scale a node by equal amounts in all directions
scaleS :: Monad m => Float -> OSGT m SceneNode -> OSGT m SceneNode
scaleS f = scale (pure f)

-- | Scale a node
scale :: Monad m => V3 Float -> OSGT m SceneNode -> OSGT m SceneNode
scale v = transformSG (scaleM v) (scale v)

-- | Translate a node
translate :: Monad m => V3 Float -> OSGT m SceneNode -> OSGT m SceneNode
translate v = transformSG (translateM v) (translate v)

-- | Rotate a node by an angle around a vector.
rotate :: Monad m => (Float, V3 Float) -> OSGT m SceneNode -> OSGT m SceneNode
rotate a@(theta, v) = transformSG (rotateM theta v) (rotate a)

rad :: Float -> Float
rad x = x * pi / 180

-- | Rotate a node around X axis
rotateX :: Monad m => Float -> OSGT m SceneNode -> OSGT m SceneNode
rotateX theta = rotate (rad theta, V3 1 0 0 )

-- | Rotate a node around Y axis
rotateY :: Monad m => Float -> OSGT m SceneNode -> OSGT m SceneNode
rotateY theta = rotate (rad theta, V3 0 1 0)

-- | Rotate a node around Z axis
rotateZ :: Monad m => Float -> OSGT m SceneNode -> OSGT m SceneNode
rotateZ theta = rotate (rad theta, V3 0 0 1)

-- | Apply colour to the node
colourSG :: Monad m => OSGT m SceneNode -> (Phong -> Phong) -> (OSGT m SceneNode -> OSGT m SceneNode) -> OSGT m SceneNode
colourSG sn action self = do
  (n1, i) <- runOSGL' sn
  case n1 of
    (SceneNode n (Material p)) -> do
      let p' = action p
      replaceNode (SceneNode n (Material p'))
    _ -> do
      let n'' = addNode (SceneNode (0, "") (Material def)) [(EmptyLabel, i)]
      self n''

-- | Transform the node of a scene graph within the Monad with the supplied matrix transform
transformSG :: Monad m => (M44 Float -> M44 Float) -> (OSGT m SceneNode -> OSGT m SceneNode) -> OSGT m SceneNode -> OSGT m SceneNode
transformSG action self n = do
  (n1, i) <- runOSGL' n
  case n1 of
    (SceneNode num (MatrixTransform m)) -> do
      let m' = action m
      replaceNode (SceneNode num (MatrixTransform m'))
    _ -> do
      let n'' = addNode (SceneNode (0, "") (MatrixTransform L.identity)) [(EmptyLabel, i)]
      self n''

-- | Transform the node of a scene graph with the supplied matrix transform
transformSG' :: SceneGraph -> Node -> (M44 Float -> M44 Float) -> SceneGraph
transformSG' sg nde mf = case llab sg nde of
  (SceneNode _ (MatrixTransform m)) -> replaceNode'' sg (SceneNode (nde,show nde) (MatrixTransform (mf m)))
  _ -> error "FIXME: Not a transform node"

translateSG' :: SceneGraph -> Node -> V3 Float -> SceneGraph
translateSG' sg nde v = transformSG' sg nde (translateM v)

translatePostSG' :: SceneGraph -> Node -> V3 Float -> SceneGraph
translatePostSG' sg nde v = transformSG' sg nde (translatePostM v)

rotatePostSG' :: SceneGraph -> Node -> V3 Float -> Float -> SceneGraph
rotatePostSG' sg nde v theta = transformSG' sg nde (rotatePostM theta v)

-- | Add colour to a node
colour ::  Monad m => Colour -> OSGT m SceneNode -> OSGT m SceneNode
colour c n = colourSG n (const $ colour2Phong c) (colour c)

-- | Label a node
label :: Monad m => OSGT m SceneNode -> String -> OSGT m SceneNode
label anode lbl = do
  (SceneNode (nde,_) dte, _) <- runOSGL' anode
  replaceNode (SceneNode (nde, lbl) dte)


-- | Add texture
texture :: Monad m => OSGT m SceneNode -> String -> OSGT m SceneNode
texture n texName = do
  i <- snd <$> runOSGL' n
  addNode (SceneNode (0, "") (Texture texName ))  [(EmptyLabel, i)]

-- -- | Add Text
text :: Monad m => T.Text -> OSGT m SceneNode
text str =  addBasicNode (Text str)



infixr 5 <+>
infixl 9 <->
infixl 9 </>


-- | Join two graphs together
(<+>) ::  Monad m => OSGT m SceneNode -> OSGT m SceneNode -> OSGT m SceneNode
(<+>) a b = do
  s <- ST.get
  (_, s', i) <- runOSGL s a
  (_, s'', j) <- runOSGL s' b
  ST.put s''
  addNode (SceneNode (0, "") Group) [(EmptyLabel, i), (EmptyLabel, j)]


-- | Translate a node
(<->) :: Monad m => OSGT m SceneNode -> V3 Float -> OSGT m SceneNode
(<->) = flip translate

-- | Scale a node
(</>) :: Monad m => OSGT m SceneNode -> V3 Float -> OSGT m SceneNode
(</>) = flip scale


doNothing :: Monad m => p -> m ()
doNothing _ = return ()

-- | Add an handler node
handler :: Monad m => OSGT m SceneNode -> ClickHandler -> OSGT m SceneNode
handler n f = do
  (_, i) <- runOSGL' n
  addNode (SceneNode (0, "") (Handler (Just (f, doNothing)) Nothing)) [(EmptyLabel, i)]

handler2 :: Monad m =>  OSGT m SceneNode -> (ClickHandler, DragHandler) -> OSGT m SceneNode
handler2 n (f,g) = do
  (_, i) <- runOSGL' n
  addNode (SceneNode (0, "") (Handler (Just (f, doNothing)) (Just (g, doNothing)))) [(EmptyLabel, i)]

-- | Create a DragHandler
dragHandler :: DragHandler
dragHandler (sg,nde) vec = do
  let tnde = head $ G.pre sg nde
      sg' = translateSG' sg tnde vec
      SceneNode _ (MatrixTransform m) = llab sg' tnde
      posx = m^.(_x._z)
  return (if abs posx < 1 then sg' else sg,posx)

-- | Create a ClickHandler
switchHandler :: ClickHandler
switchHandler (sg, nde) ev = do
  let sn = head $ G.suc sg nde
      sn' = llab sg sn
  let sg' = switchNode sn' (if ev == Down then 1 else 0) sg
  return sg'

switchNode' :: Node -> Int -> SceneGraph -> SceneGraph
switchNode' nde n gr = replaceNode'' gr (SceneNode (nde,show nde) (Switch n))


switchNode ::  SceneNode -> Int -> SceneGraph -> SceneGraph
switchNode (SceneNode nde (Switch _)) n gr =
  replaceNode'' gr newNode
  where newNode = SceneNode nde (Switch n)
switchNode _ _ _ = error "no Switch"

-- | Create a switch node
switch ::  Monad m => OSGT m SceneNode -> OSGT m SceneNode -> OSGT m SceneNode
switch = switch' 0


switch'::  Monad m => Int -> OSGT m SceneNode -> OSGT m SceneNode -> OSGT m SceneNode
switch' nde a b = do
  s <- ST.get
  (_, s', i) <- runOSGL s a
  (_, s'', j) <- runOSGL s' b
  ST.put s''
  addNode (SceneNode (nde, show nde) (Switch 0)) [(EmptyLabel, i), (EmptyLabel, j)]


-- -- | Get a strip mesh
-- strip :: Monad m => OSGT m SceneNode
-- strip = do
--            let n = SceneNode (0,"") (Geode $ Mesh1 [(TriangleStrip,0,3)] [
--                                     vector3 (-2) 0 (-2),
--                                     vector3 (2)  0 (-2),
--                                     vector3 0 0 0 ] [
--                                     vector3 0 (-1) 0,
--                                     vector3 0 (-1) 0,
--                                     vector3 0 (-1) 0 ] )
--            addNode n []

-- | Make a group node from list of nodes
makeGroup :: Monad m => [SceneNode] -> OSGT m SceneNode
makeGroup [] = error "empty"
makeGroup [n] = addNode n []
makeGroup (n:ns) =
  let n' = makeGroup ns in
  addNode n [] <+> n'

emptyScene :: Scene
emptyScene = (G.empty, 0)


getHitAction :: Scene -> (Int -> IO ())
getHitAction = const $ const $ return ()

-- | Work up the tree from indicated no to find the first handler scene node.
findHandler :: SceneGraph -> Int -> Maybe SceneNode
findHandler gr num =
  let start = fromEnum num
      findUp num' =
        case llab gr num' of
          SceneNode (n, _) (Handler _ _) -> [llab gr n]
          _                              -> concatMap findUp (G.pre gr num')
  in
  case findUp start of
    []    -> Nothing
    (a:_) -> Just a


-- | Work down the tree from indicated no to find the first handler scene node.
findHandlerDown :: SceneGraph -> Int -> Int
findHandlerDown gr num =
  let findDown num' =
        case llab gr num' of
          SceneNode (n, _) (Handler _ _) -> [n]
          _                              -> concatMap findDown (G.suc gr num')
  in
  case findDown num of
    []    -> error "findHandlerDown failed"
    (a:_) -> a


findTextDown :: SceneGraph -> Int -> Int
findTextDown gr num =
  let findDown num' =
        case llab gr num' of
          SceneNode (n, _) (Text _ ) -> [n]
          _                          -> concatMap findDown (G.suc gr num')
  in
  case findDown num of
    []    -> error "findHandlerDown failed"
    (a:_) -> a

-- {--
-- -- Buttons are always switch nodes but selected geometry will not be so we need to search
-- -- up to find the owning widget.
-- -- FIXME use switchNode?
-- --}

-- | Handle some event
handleClickEvent :: Scene -> Int -> KeyState -> IO (Scene, Maybe Scene, Maybe (SceneGraph -> SceneGraph))
handleClickEvent (gr, start) n ks = do
  -- putStrLn $ "handle event" ++ show ks
  case findHandler gr n of
    Just (SceneNode (nodeId, _) (Handler (Just (fn, snk)) _ )) -> do
      sg <-  fn (gr, nodeId) ks
      case ks of
        Down -> snk ()
        _    -> return ()
      return ((sg, start), Just (sg, nodeId), Nothing)
    _ -> return ((gr, start), Nothing, Nothing)

llab :: SceneGraph -> Node -> SceneNode
llab gr n =
  case G.lab gr n of
    Nothing -> error $ "Should not happen gr=" ++ show gr ++ "n = " ++ show n
    Just n' -> n'

emptyOSG :: SceneGraph
emptyOSG = G.empty

findCamera :: Scene -> Int -> Node
findCamera (gr, _) _ = head . filter (\x ->
    case llab gr x of
      SceneNode _ Camera -> True
      _                  -> False) . G.nodes $ gr

findCameraPath :: Scene -> Int -> G.Path
findCameraPath (gr, nde) i =
  let nde2 = findCamera (gr, nde) i in
  G.esp nde nde2 gr


-- | Return the matrix got by traversing down the Node
getTransformTo :: Scene -> Node -> M44 Float
getTransformTo (gr, start) nde =
  foldr trans L.identity $ G.esp start nde gr
  where
    trans n mat1 =
      case llab gr n of
        SceneNode _ (MatrixTransform mat2) -> mat1 !*! mat2
        _                                  -> mat1


getByLabel :: SceneGraph -> String -> Node
getByLabel gr lbl =
  head
  . filter (\n -> let (SceneNode (_,lbl') _) = llab gr n in lbl == lbl')
  . G.nodes
  $ gr


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

-- | Determine bounds of the scene
bounds :: Scene -> Box Float
bounds (gr, nde) =
  let sn = llab gr nde in
  boundsSceneNode gr sn

-- | Determine bounds of a @SceneNode@
boundsSceneNode :: SceneGraph -> SceneNode -> Box Float
boundsSceneNode gr (SceneNode (nde, _) (MatrixTransform mt)) =
  let (v1, v2) = boundsOfChildren gr nde in
  ((mt !* L.point v1) ^. _xyz, (mt !* L.point v2) ^. _xyz)

boundsSceneNode gr (SceneNode (nde, _) (Switch i)) =
  let nde' = G.suc gr nde !! i in
  bounds (gr, nde')

boundsSceneNode _ (SceneNode _  (Geode _)) = smallBox

boundsSceneNode gr (SceneNode (nde, _) _) = boundsOfChildren gr nde

boundsOfChildren :: SceneGraph -> Node -> Box Float
boundsOfChildren gr =
  fromMaybe smallBox . foldr f Nothing . G.suc gr
  where
    f nde Nothing  = Just $ bounds (gr, nde)
    f nde (Just b) = Just $ b `union` bounds (gr, nde)
