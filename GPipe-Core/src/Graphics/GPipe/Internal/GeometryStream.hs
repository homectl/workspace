{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, Arrows, GeneralizedNewtypeDeriving, GADTs, MultiParamTypeClasses #-}

module Graphics.GPipe.Internal.GeometryStream where

import Control.Arrow
import Control.Category
import Control.Monad.Trans.State.Lazy
import qualified Control.Monad.Trans.Class as T (lift)
import Control.Monad.Trans.Writer (tell)
#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup (Semigroup(..))
#endif
import Prelude hiding (length, id, (.))

import Graphics.GPipe.Internal.Expr
import Graphics.GPipe.Internal.Shader
import Graphics.GPipe.Internal.Compiler
import Graphics.GPipe.Internal.PrimitiveArray
import Graphics.GPipe.Internal.PrimitiveStream
import Graphics.GPipe.Internal.FragmentStream

import Graphics.GL.Core45

import Data.Boolean
import Data.IntMap.Lazy (insert)
import Linear.V4
import Linear.V3
import Linear.V2
import Linear.V1
import Linear.V0
import Linear.Plucker (Plucker(..))
import Linear.Quaternion (Quaternion(..))
import Linear.Affine (Point(..))

------------------------------------------------------------------------------------------------------------------------------------

type GeometrizationName = Int

type LayoutName = String

data GeometryStreamData = GeometryStreamData GeometrizationName LayoutName PrimitiveStreamData

newtype GeometryStream a = GeometryStream [(a, GeometryStreamData)] deriving (Semigroup, Monoid)

instance Functor GeometryStream where
    fmap f (GeometryStream xs) = GeometryStream $ map (first f) xs

newtype ToGeometry a b = ToGeometry (Kleisli (State (Int, Int)) a b) deriving (Category, Arrow)

-- TODO Merge PrimitiveTopology and GeometryInput?
class (AnotherVertexInput a, PrimitiveTopology p) => GeometryInput p a where
    toGeometry :: ToGeometry a (Geometry p a)

newtype ToAnotherVertex a b = ToAnotherVertex (Kleisli (State (Int, Int)) (Int, a) b)

instance Category ToAnotherVertex where
    {-# INLINE id #-}
    id = ToAnotherVertex $ proc ~(i, x) -> do
        returnA -< x
    {-# INLINE (.) #-}
    ToAnotherVertex g . ToAnotherVertex f = ToAnotherVertex $ proc ~(i, x) -> do
        y <- f -< (i, x)
        z <- g -< (i, y)
        returnA -< z

instance Arrow ToAnotherVertex where
    {-# INLINE arr #-}
    arr f = ToAnotherVertex (arr (f . (\(_, c) -> c)))
    {-# INLINE first #-}
    first (ToAnotherVertex f) = ToAnotherVertex $ proc ~(i, (x, z)) -> do
        y <- f -< (i, x)
        returnA -< (y, z)

class AnotherVertexInput a where
    toAnotherVertex :: ToAnotherVertex a a

instance AnotherVertexInput a => GeometryInput Points a where
    toGeometry = ToGeometry $ Kleisli $ \x -> do
        let ToAnotherVertex (Kleisli m) = toAnotherVertex :: ToAnotherVertex a a
        x0 <- m (0, x)
        return $ Point x0

instance AnotherVertexInput a => GeometryInput Lines a where
    toGeometry = ToGeometry $ Kleisli $ \x -> do
        let ToAnotherVertex (Kleisli m) = toAnotherVertex :: ToAnotherVertex a a
        x0 <- m (0, x)
        x1 <- m (1, x)
        return $ Line x0 x1

instance AnotherVertexInput a => GeometryInput LinesWithAdjacency a where
    toGeometry = ToGeometry $ Kleisli $ \x -> do
        let ToAnotherVertex (Kleisli m) = toAnotherVertex :: ToAnotherVertex a a
        x0 <- m (0, x)
        x1 <- m (1, x)
        x2 <- m (2, x)
        x3 <- m (3, x)
        return $ LineWithAdjacency x0 x1 x2 x3

instance AnotherVertexInput a => GeometryInput Triangles a where
    toGeometry = ToGeometry $ Kleisli $ \x -> do
        let ToAnotherVertex (Kleisli m) = toAnotherVertex :: ToAnotherVertex a a
        x0 <- m (0, x)
        x1 <- m (1, x)
        x2 <- m (2, x)
        return $ Triangle x0 x1 x2

instance AnotherVertexInput a => GeometryInput TrianglesWithAdjacency a where
    toGeometry = ToGeometry $ Kleisli $ \x -> do
        let ToAnotherVertex (Kleisli m) = toAnotherVertex :: ToAnotherVertex a a
        x0 <- m (0, x)
        x1 <- m (1, x)
        x2 <- m (2, x)
        x3 <- m (3, x)
        x4 <- m (4, x)
        x5 <- m (5, x)
        return $ TriangleWithAdjacency x0 x1 x2 x3 x4 x5

------------------------------------------------------------------------------------------------------------------------------------

-- makeAnotherVertex :: String -> SType -> ((S c a) -> ExprM String) -> ToAnotherVertex (S c a) (S c a)
makeAnotherVertex :: String -> SType -> (b -> ExprM String) -> (S c a -> b) -> ToAnotherVertex b b
makeAnotherVertex qual styp f f' = ToAnotherVertex $ Kleisli $ \ (i, x) -> do
    (j, n) <- get
    let n' = if i == j then n else 0 -- reset when index change
    put (i, n'+1)
    return $ f' $ S $ useGInput qual styp i n' $ f x

instance AnotherVertexInput () where
    toAnotherVertex = arr (const ())

instance AnotherVertexInput VFloat where
    toAnotherVertex = makeAnotherVertex "" STypeFloat unS id

instance AnotherVertexInput FlatVFloat where
    toAnotherVertex = makeAnotherVertex "flat" STypeFloat (unS . unFlat) Flat

instance AnotherVertexInput NoPerspectiveVFloat where
    toAnotherVertex = makeAnotherVertex "noperspective" STypeFloat (unS . unNPersp) NoPerspective

instance AnotherVertexInput VInt where
    toAnotherVertex = makeAnotherVertex "flat" STypeInt unS id

instance AnotherVertexInput VWord where
    toAnotherVertex = makeAnotherVertex "flat" STypeUInt unS id

instance AnotherVertexInput VBool where
    toAnotherVertex = proc b -> do
        i <- toAnotherVertex -< ifB b 1 0 :: VInt
        returnA -< i ==* 1

instance (AnotherVertexInput a) => AnotherVertexInput (V0 a) where
    toAnotherVertex = arr (const V0)

instance (AnotherVertexInput a) => AnotherVertexInput (V1 a) where
    toAnotherVertex = proc ~(V1 a) -> do
        a' <- toAnotherVertex -< a
        returnA -< V1 a'

instance (AnotherVertexInput a) => AnotherVertexInput (V2 a) where
    toAnotherVertex = proc ~(V2 a b) -> do
        a' <- toAnotherVertex -< a
        b' <- toAnotherVertex -< b
        returnA -< V2 a' b'

instance (AnotherVertexInput a) => AnotherVertexInput (V3 a) where
    toAnotherVertex = proc ~(V3 a b c) -> do
        a' <- toAnotherVertex -< a
        b' <- toAnotherVertex -< b
        c' <- toAnotherVertex -< c
        returnA -< V3 a' b' c'

instance (AnotherVertexInput a) => AnotherVertexInput (V4 a) where
    toAnotherVertex = proc ~(V4 a b c d) -> do
        a' <- toAnotherVertex -< a
        b' <- toAnotherVertex -< b
        c' <- toAnotherVertex -< c
        d' <- toAnotherVertex -< d
        returnA -< V4 a' b' c' d'

instance (AnotherVertexInput a, AnotherVertexInput b) => AnotherVertexInput (a,b) where
    toAnotherVertex = proc ~(a,b) -> do
        a' <- toAnotherVertex -< a
        b' <- toAnotherVertex -< b
        returnA -< (a', b')

instance (AnotherVertexInput a, AnotherVertexInput b, AnotherVertexInput c) => AnotherVertexInput (a,b,c) where
    toAnotherVertex = proc ~(a,b,c) -> do
        a' <- toAnotherVertex -< a
        b' <- toAnotherVertex -< b
        c' <- toAnotherVertex -< c
        returnA -< (a', b', c')

instance (AnotherVertexInput a, AnotherVertexInput b, AnotherVertexInput c, AnotherVertexInput d) => AnotherVertexInput (a,b,c,d) where
    toAnotherVertex = proc ~(a,b,c,d) -> do
        a' <- toAnotherVertex -< a
        b' <- toAnotherVertex -< b
        c' <- toAnotherVertex -< c
        d' <- toAnotherVertex -< d
        returnA -< (a', b', c', d')

instance (AnotherVertexInput a, AnotherVertexInput b, AnotherVertexInput c, AnotherVertexInput d, AnotherVertexInput e) => AnotherVertexInput (a,b,c,d,e) where
    toAnotherVertex = proc ~(a,b,c,d,e) -> do
        a' <- toAnotherVertex -< a
        b' <- toAnotherVertex -< b
        c' <- toAnotherVertex -< c
        d' <- toAnotherVertex -< d
        e' <- toAnotherVertex -< e
        returnA -< (a', b', c', d', e')

instance (AnotherVertexInput a, AnotherVertexInput b, AnotherVertexInput c, AnotherVertexInput d, AnotherVertexInput e, AnotherVertexInput f) => AnotherVertexInput (a,b,c,d,e,f) where
    toAnotherVertex = proc ~(a,b,c,d,e,f) -> do
        a' <- toAnotherVertex -< a
        b' <- toAnotherVertex -< b
        c' <- toAnotherVertex -< c
        d' <- toAnotherVertex -< d
        e' <- toAnotherVertex -< e
        f' <- toAnotherVertex -< f
        returnA -< (a', b', c', d', e', f')

instance (AnotherVertexInput a, AnotherVertexInput b, AnotherVertexInput c, AnotherVertexInput d, AnotherVertexInput e, AnotherVertexInput f, AnotherVertexInput g) => AnotherVertexInput (a,b,c,d,e,f,g) where
    toAnotherVertex = proc ~(a,b,c,d,e,f,g) -> do
        a' <- toAnotherVertex -< a
        b' <- toAnotherVertex -< b
        c' <- toAnotherVertex -< c
        d' <- toAnotherVertex -< d
        e' <- toAnotherVertex -< e
        f' <- toAnotherVertex -< f
        g' <- toAnotherVertex -< g
        returnA -< (a', b', c', d', e', f', g')

instance AnotherVertexInput a => AnotherVertexInput (Quaternion a) where
    toAnotherVertex = proc ~(Quaternion a v) -> do
        a' <- toAnotherVertex -< a
        v' <- toAnotherVertex -< v
        returnA -< Quaternion a' v'

instance AnotherVertexInput a => AnotherVertexInput (Plucker a) where
    toAnotherVertex = proc ~(Plucker a b c d e f) -> do
        a' <- toAnotherVertex -< a
        b' <- toAnotherVertex -< b
        c' <- toAnotherVertex -< c
        d' <- toAnotherVertex -< d
        e' <- toAnotherVertex -< e
        f' <- toAnotherVertex -< f
        returnA -< Plucker a' b' c' d' e' f'

------------------------------------------------------------------------------------------------------------------------------------

geometrize :: forall p a s os f. GeometryInput p a => PrimitiveStream p a -> Shader os s (GeometryStream (Geometry p a))
geometrize (PrimitiveStream xs) = Shader $ do
        n <- getNewName
        modifyRenderIO (\s -> s { transformFeedbackToRenderIO = insert n io (transformFeedbackToRenderIO s) } )
        return (GeometryStream $ map (f n) xs)
    where
        ToGeometry (Kleisli m) = toGeometry :: ToGeometry a (Geometry p a)
        f :: GeometrizationName -> (a, (Maybe PointSize, PrimitiveStreamData)) -> (Geometry p a, GeometryStreamData)
        f n (x, (_, s)) = (evalState (m x) (0, 0), GeometryStreamData n (toLayoutIn (undefined :: p)) s)
        io _ _ = return ()

------------------------------------------------------------------------------------------------------------------------------------

notMeantToBeRead = "false" -- error "a generative geometry is inherently a write-only value"

generativePoints :: FragmentInput a => GGenerativeGeometry Points a
generativePoints = S $ return notMeantToBeRead

generativeLineStrip :: FragmentInput a => GGenerativeGeometry Lines a
generativeLineStrip = S $ return notMeantToBeRead

generativeTriangleStrip :: FragmentInput a => GGenerativeGeometry Triangles a
generativeTriangleStrip = S $ return notMeantToBeRead

emitVertex :: GeometryExplosive a => a -> GGenerativeGeometry p a -> GGenerativeGeometry p a
emitVertex a g = S $ do
    g' <- unS g
    exploseGeometry a 0
    T.lift $ T.lift $ tell "EmitVertex();\n"
    return notMeantToBeRead

emitVertexPosition :: GeometryExplosive a => (VPos, a) -> GGenerativeGeometry p (VPos, a) -> GGenerativeGeometry p (VPos, a)
emitVertexPosition (V4 x y z w, a) g = S $ do
    g' <- unS g
    x' <- unS x
    y' <- unS y
    z' <- unS z
    w' <- unS w
    tellAssignment' "gl_Position" $ "vec4("++x'++',':y'++',':z'++',':w'++")"
    exploseGeometry a 0
    T.lift $ T.lift $ tell "EmitVertex();\n"
    return notMeantToBeRead

emitVertexLayer :: GeometryExplosive a => (VInt, a) -> GGenerativeGeometry p (VInt, a) -> GGenerativeGeometry p (VInt, a)
emitVertexLayer (i, a) g = S $ do
    g' <- unS g
    i' <- unS i
    tellAssignment' "gl_Layer" $ i'
    T.lift $ T.lift $ tell "EmitVertex();\n"
    return notMeantToBeRead

emitVertexPositionAndLayer :: GeometryExplosive a => ((VPos, VInt), a) -> GGenerativeGeometry p ((VPos, VInt), a) -> GGenerativeGeometry p ((VPos, VInt), a)
emitVertexPositionAndLayer ((V4 x y z w, i), a) g = S $ do
    g' <- unS g
    x' <- unS x
    y' <- unS y
    z' <- unS z
    w' <- unS w
    tellAssignment' "gl_Position" $ "vec4("++x'++',':y'++',':z'++',':w'++")"
    exploseGeometry a 0
    i' <- unS i
    tellAssignment' "gl_Layer" $ i'
    T.lift $ T.lift $ tell "EmitVertex();\n"
    return notMeantToBeRead

endPrimitive :: GGenerativeGeometry p a -> GGenerativeGeometry p a
endPrimitive g = S $ do
    g' <- unS g
    T.lift $ T.lift $ tell "EndPrimitive();\n"
    return notMeantToBeRead

------------------------------------------------------------------------------------------------------------------------------------

class FragmentInput a => GeometryExplosive a where
    exploseGeometry :: a -> Int -> ExprM Int
    declareGeometry :: a -> State Int (GlobDeclM ())
    enumerateVaryings :: a -> State Int [String]

defaultExploseGeometry f x n = do
    let name = "vgf" ++ show n
    x' <- unS (f x)
    tellAssignment' name x'
    return (n + 1)

defaultDeclareGeometry t x = do
    n <- get
    put (n + 1)
    let name = "vgf" ++ show n
    return $ do
        tellGlobal $ "out "
        tellGlobal $ stypeName t
        tellGlobalLn $ ' ':name

defaultEnumerateVaryings x = do
    n <- get
    put (n + 1)
    return ["vgf" ++ show n]

instance GeometryExplosive () where
    exploseGeometry _ n = return n
    declareGeometry _ = return (return ())
    enumerateVaryings _ = return []

instance GeometryExplosive VFloat where
    exploseGeometry = defaultExploseGeometry id
    declareGeometry = defaultDeclareGeometry STypeFloat
    enumerateVaryings = defaultEnumerateVaryings

instance GeometryExplosive FlatVFloat where
    exploseGeometry = defaultExploseGeometry unFlat
    declareGeometry = defaultDeclareGeometry STypeFloat
    enumerateVaryings = defaultEnumerateVaryings

instance GeometryExplosive NoPerspectiveVFloat where
    exploseGeometry = defaultExploseGeometry unNPersp
    declareGeometry = defaultDeclareGeometry STypeFloat
    enumerateVaryings = defaultEnumerateVaryings

instance GeometryExplosive VInt where
    exploseGeometry = defaultExploseGeometry id
    declareGeometry = defaultDeclareGeometry STypeInt
    enumerateVaryings = defaultEnumerateVaryings

instance GeometryExplosive VWord where
    exploseGeometry = defaultExploseGeometry id
    declareGeometry = defaultDeclareGeometry STypeUInt
    enumerateVaryings = defaultEnumerateVaryings

instance GeometryExplosive VBool where
    exploseGeometry = defaultExploseGeometry id
    declareGeometry = defaultDeclareGeometry STypeBool
    enumerateVaryings = defaultEnumerateVaryings

instance (GeometryExplosive a) => GeometryExplosive (V0 a) where
    exploseGeometry V0 = return
    declareGeometry V0 = return (return ())
    enumerateVaryings V0 = return []

instance (GeometryExplosive a) => GeometryExplosive (V1 a) where
    exploseGeometry (V1 x) n = do
        exploseGeometry x n
    declareGeometry ~(V1 x) = do
        declareGeometry x
    enumerateVaryings ~(V1 x) =
        enumerateVaryings x

instance (GeometryExplosive a) => GeometryExplosive (V2 a) where
    exploseGeometry (V2 x y) n = do
        exploseGeometry x n >>= exploseGeometry y
    declareGeometry ~(V2 x y) = do
        ws <- sequence [declareGeometry x, declareGeometry y]
        return $ sequence_ ws
    enumerateVaryings ~(V2 x y) =
        concat <$> sequence [enumerateVaryings x, enumerateVaryings y]

instance (GeometryExplosive a) => GeometryExplosive (V3 a) where
    exploseGeometry (V3 x y z) n = do
        exploseGeometry x n >>= exploseGeometry y >>= exploseGeometry z
    declareGeometry ~(V3 x y z) = do
        ws <- sequence [declareGeometry x, declareGeometry y, declareGeometry z]
        return $ sequence_ ws
    enumerateVaryings ~(V3 x y z) =
        concat <$> sequence [enumerateVaryings x, enumerateVaryings y, enumerateVaryings z]

instance (GeometryExplosive a) => GeometryExplosive (V4 a) where
    exploseGeometry (V4 x y z w) n =
        exploseGeometry x n >>= exploseGeometry y >>= exploseGeometry z >>= exploseGeometry w
    declareGeometry ~(V4 x y z w) = do
        ws <- sequence [declareGeometry x, declareGeometry y, declareGeometry z, declareGeometry w]
        return $ sequence_ ws
    enumerateVaryings ~(V4 x y z w) =
        concat <$> sequence [enumerateVaryings x, enumerateVaryings y, enumerateVaryings z, enumerateVaryings w]

instance (GeometryExplosive a, GeometryExplosive b) => GeometryExplosive (a,b) where
    exploseGeometry (x, y) n =
        exploseGeometry x n >>= exploseGeometry y
    declareGeometry ~(x, y) = do
        ws <- sequence [declareGeometry x, declareGeometry y]
        return $ sequence_ ws
    enumerateVaryings ~(x, y) =
        concat <$> sequence [enumerateVaryings x, enumerateVaryings y]

instance (GeometryExplosive a, GeometryExplosive b, GeometryExplosive c) => GeometryExplosive (a,b,c) where
    exploseGeometry (x, y, z) n =
        exploseGeometry x n >>= exploseGeometry y >>= exploseGeometry z
    declareGeometry ~(x, y, z) = do
        ws <- sequence [declareGeometry x, declareGeometry y, declareGeometry z]
        return $ sequence_ ws
    enumerateVaryings ~(x, y, z) =
        concat <$> sequence [enumerateVaryings x, enumerateVaryings y, enumerateVaryings z]

instance (GeometryExplosive a, GeometryExplosive b, GeometryExplosive c, GeometryExplosive d) => GeometryExplosive (a,b,c,d) where
    exploseGeometry (x, y, z, w) n =
        exploseGeometry x n >>= exploseGeometry y >>= exploseGeometry z >>= exploseGeometry w
    declareGeometry ~(x, y, z, w) = do
        ws <- sequence [declareGeometry x, declareGeometry y, declareGeometry z, declareGeometry w]
        return $ sequence_ ws
    enumerateVaryings ~(x, y, z, w) =
        concat <$> sequence [enumerateVaryings x, enumerateVaryings y, enumerateVaryings z, enumerateVaryings w]

instance (GeometryExplosive a, GeometryExplosive b, GeometryExplosive c, GeometryExplosive d, GeometryExplosive e) => GeometryExplosive (a,b,c,d,e) where
    exploseGeometry (x, y, z, w, r) n =
        exploseGeometry x n >>= exploseGeometry y >>= exploseGeometry z >>= exploseGeometry w >>= exploseGeometry r
    declareGeometry ~(x, y, z, w, r) = do
        ws <- sequence [declareGeometry x, declareGeometry y, declareGeometry z, declareGeometry w, declareGeometry r]
        return $ sequence_ ws
    enumerateVaryings ~(x, y, z, w, r) =
        concat <$> sequence [enumerateVaryings x, enumerateVaryings y, enumerateVaryings z, enumerateVaryings w, enumerateVaryings r]

instance (GeometryExplosive a, GeometryExplosive b, GeometryExplosive c, GeometryExplosive d, GeometryExplosive e, GeometryExplosive f) => GeometryExplosive (a,b,c,d,e,f) where
    exploseGeometry (x, y, z, w, r, s) n =
        exploseGeometry x n >>= exploseGeometry y >>= exploseGeometry z >>= exploseGeometry w >>= exploseGeometry r >>= exploseGeometry s
    declareGeometry ~(x, y, z, w, r, s) = do
        ws <- sequence [declareGeometry x, declareGeometry y, declareGeometry z, declareGeometry w, declareGeometry r, declareGeometry s]
        return $ sequence_ ws
    enumerateVaryings ~(x, y, z, w, r, s) =
        concat <$> sequence [enumerateVaryings x, enumerateVaryings y, enumerateVaryings z, enumerateVaryings w, enumerateVaryings r, enumerateVaryings s]

instance (GeometryExplosive a, GeometryExplosive b, GeometryExplosive c, GeometryExplosive d, GeometryExplosive e, GeometryExplosive f, GeometryExplosive g) => GeometryExplosive (a,b,c,d,e,f,g) where
    exploseGeometry (x, y, z, w, r, s, t) n =
        exploseGeometry x n >>= exploseGeometry y >>= exploseGeometry z >>= exploseGeometry w >>= exploseGeometry r >>= exploseGeometry s >>= exploseGeometry t
    declareGeometry ~(x, y, z, w, r, s, t) = do
        ws <- sequence [declareGeometry x, declareGeometry y, declareGeometry z, declareGeometry w, declareGeometry r, declareGeometry s, declareGeometry t]
        return $ sequence_ ws
    enumerateVaryings ~(x, y, z, w, r, s, t) =
        concat <$> sequence [enumerateVaryings x, enumerateVaryings y, enumerateVaryings z, enumerateVaryings w, enumerateVaryings r, enumerateVaryings s, enumerateVaryings t]

------------------------------------------------------------------------------------------------------------------------------------

newtype ToFragmentFromGeometry a b = ToFragmentFromGeometry (Kleisli (State Int) a b) deriving (Category, Arrow)

class FragmentInputFromGeometry p a where
    toFragmentFromGeometry :: ToFragmentFromGeometry (GGenerativeGeometry p (b, a)) (FragmentFormat a)

instance FragmentCreator a => FragmentInputFromGeometry Triangles a where
    toFragmentFromGeometry = ToFragmentFromGeometry $ Kleisli $ \x -> do
        let ToAnotherFragment (Kleisli m) = toFragment2 :: ToAnotherFragment a (FragmentFormat a)
        m (evalState (createFragment :: State Int a) 0)

-- Note: from other constraint, b happens to be VPos or (VPos, VInt).
generateAndRasterize :: forall p b a s os f. (FragmentInputFromGeometry p a, PrimitiveTopology p)
        => (s -> (Side, PolygonMode, ViewPort, DepthRange))
        -> Int
        -> GeometryStream (GGenerativeGeometry p (b, a))
        -> Shader os s (FragmentStream (FragmentFormat a))
generateAndRasterize sf maxVertices (GeometryStream xs) = Shader $ do
        n <- getNewName
        modifyRenderIO (\s -> s { rasterizationNameToRenderIO = insert n io (rasterizationNameToRenderIO s) } )
        return (FragmentStream $ map (f n) xs)
    where
        ToFragmentFromGeometry (Kleisli m) = toFragmentFromGeometry :: ToFragmentFromGeometry (GGenerativeGeometry p (b, a)) (FragmentFormat a)
        f :: Int -> (GGenerativeGeometry p (b, a), GeometryStreamData) -> (FragmentFormat a, FragmentStreamData)
        f n (x, GeometryStreamData name layout psd) = (evalState (m x) 0, FragmentStreamData n True (makePrims layout x) psd true)

        makePrims a x = do
            declareGeometryLayout a (toLayoutOut (undefined :: p)) maxVertices
            x' <- unS x
            return ()

        io s =
            let (side, polygonMode, ViewPort (V2 x y) (V2 w h), DepthRange dmin dmax) = sf s
            in  if w < 0 || h < 0
                    then error "ViewPort, negative size"
                    else do setGlCullFace side
                            setGlPolygonMode polygonMode
                            glScissor (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
                            glViewport (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
                            glDepthRange (realToFrac dmin) (realToFrac dmax)
                            setGLPointSize

        setGlCullFace Front = glEnable GL_CULL_FACE >> glCullFace GL_BACK -- Back is culled when front is rasterized
        setGlCullFace Back = glEnable GL_CULL_FACE >> glCullFace GL_FRONT
        setGlCullFace _ = glDisable GL_CULL_FACE

        setGlPolygonMode PolygonFill      = glPolygonMode GL_FRONT_AND_BACK GL_FILL
        setGlPolygonMode PolygonPoint     = do
            glEnable GL_PROGRAM_POINT_SIZE
            glPolygonMode GL_FRONT_AND_BACK GL_POINT
        setGlPolygonMode (PolygonLine lw) = do
            glLineWidth (realToFrac lw)
            glPolygonMode GL_FRONT_AND_BACK GL_LINE

        setGLPointSize = glDisable GL_PROGRAM_POINT_SIZE

------------------------------------------------------------------------------------------------------------------------------------

newtype ToAnotherFragment a b = ToAnotherFragment (Kleisli (State Int) a b) deriving (Category, Arrow)

class FragmentInput a => AnotherFragmentInput a where
    toFragment2 :: ToAnotherFragment a (FragmentFormat a)

makeAnotherFragment :: String -> SType -> (a -> ExprM String) -> ToAnotherFragment a (S c a1)
makeAnotherFragment qual styp f = ToAnotherFragment $ Kleisli $ \ x -> do
        n <- get
        put (n + 1)
        return $ S $ useFInputFromG qual styp n $ f x

instance AnotherFragmentInput () where
    toFragment2 = arr (const ())

instance AnotherFragmentInput VFloat where
    toFragment2 = makeAnotherFragment "" STypeFloat unS

instance AnotherFragmentInput FlatVFloat where
    toFragment2 = makeAnotherFragment "flat" STypeFloat (unS . unFlat)

instance AnotherFragmentInput NoPerspectiveVFloat where
    toFragment2 = makeAnotherFragment "noperspective" STypeFloat (unS . unNPersp)

instance AnotherFragmentInput VInt where
    toFragment2 = makeAnotherFragment "flat" STypeInt unS

instance AnotherFragmentInput VWord where
    toFragment2 = makeAnotherFragment "flat" STypeUInt unS

instance AnotherFragmentInput VBool where
    toFragment2 = proc b -> do
        i <- toFragment2 -< ifB b 1 0 :: VInt
        returnA -< i ==* 1

instance (AnotherFragmentInput a) => AnotherFragmentInput (V0 a) where
    toFragment2 = arr (const V0)

instance (AnotherFragmentInput a) => AnotherFragmentInput (V1 a) where
    toFragment2 = proc ~(V1 a) -> do
        a' <- toFragment2 -< a
        returnA -< V1 a'

instance (AnotherFragmentInput a) => AnotherFragmentInput (V2 a) where
    toFragment2 = proc ~(V2 a b) -> do
        a' <- toFragment2 -< a
        b' <- toFragment2 -< b
        returnA -< V2 a' b'

instance (AnotherFragmentInput a) => AnotherFragmentInput (V3 a) where
    toFragment2 = proc ~(V3 a b c) -> do
        a' <- toFragment2 -< a
        b' <- toFragment2 -< b
        c' <- toFragment2 -< c
        returnA -< V3 a' b' c'

instance (AnotherFragmentInput a) => AnotherFragmentInput (V4 a) where
    toFragment2 = proc ~(V4 a b c d) -> do
        a' <- toFragment2 -< a
        b' <- toFragment2 -< b
        c' <- toFragment2 -< c
        d' <- toFragment2 -< d
        returnA -< V4 a' b' c' d'

instance (AnotherFragmentInput a, AnotherFragmentInput b) => AnotherFragmentInput (a,b) where
    toFragment2 = proc ~(a,b) -> do
        a' <- toFragment2 -< a
        b' <- toFragment2 -< b
        returnA -< (a', b')

instance (AnotherFragmentInput a, AnotherFragmentInput b, AnotherFragmentInput c) => AnotherFragmentInput (a,b,c) where
    toFragment2 = proc ~(a,b,c) -> do
        a' <- toFragment2 -< a
        b' <- toFragment2 -< b
        c' <- toFragment2 -< c
        returnA -< (a', b', c')

instance (AnotherFragmentInput a, AnotherFragmentInput b, AnotherFragmentInput c, AnotherFragmentInput d) => AnotherFragmentInput (a,b,c,d) where
    toFragment2 = proc ~(a,b,c,d) -> do
        a' <- toFragment2 -< a
        b' <- toFragment2 -< b
        c' <- toFragment2 -< c
        d' <- toFragment2 -< d
        returnA -< (a', b', c', d')

instance (AnotherFragmentInput a, AnotherFragmentInput b, AnotherFragmentInput c, AnotherFragmentInput d, AnotherFragmentInput e) => AnotherFragmentInput (a,b,c,d,e) where
    toFragment2 = proc ~(a,b,c,d,e) -> do
        a' <- toFragment2 -< a
        b' <- toFragment2 -< b
        c' <- toFragment2 -< c
        d' <- toFragment2 -< d
        e' <- toFragment2 -< e
        returnA -< (a', b', c', d', e')

instance (AnotherFragmentInput a, AnotherFragmentInput b, AnotherFragmentInput c, AnotherFragmentInput d, AnotherFragmentInput e, AnotherFragmentInput f) => AnotherFragmentInput (a,b,c,d,e,f) where
    toFragment2 = proc ~(a,b,c,d,e,f) -> do
        a' <- toFragment2 -< a
        b' <- toFragment2 -< b
        c' <- toFragment2 -< c
        d' <- toFragment2 -< d
        e' <- toFragment2 -< e
        f' <- toFragment2 -< f
        returnA -< (a', b', c', d', e', f')

instance (AnotherFragmentInput a, AnotherFragmentInput b, AnotherFragmentInput c, AnotherFragmentInput d, AnotherFragmentInput e, AnotherFragmentInput f, AnotherFragmentInput g) => AnotherFragmentInput (a,b,c,d,e,f,g) where
    toFragment2 = proc ~(a,b,c,d,e,f,g) -> do
        a' <- toFragment2 -< a
        b' <- toFragment2 -< b
        c' <- toFragment2 -< c
        d' <- toFragment2 -< d
        e' <- toFragment2 -< e
        f' <- toFragment2 -< f
        g' <- toFragment2 -< g
        returnA -< (a', b', c', d', e', f', g')

instance AnotherFragmentInput a => AnotherFragmentInput (Quaternion a) where
    toFragment2 = proc ~(Quaternion a v) -> do
        a' <- toFragment2 -< a
        v' <- toFragment2 -< v
        returnA -< Quaternion a' v'

instance (AnotherFragmentInput (f a), AnotherFragmentInput a, FragmentFormat (f a) ~ f (FragmentFormat a)) => AnotherFragmentInput (Point f a) where
    toFragment2 = proc ~(P a) -> do
        a' <- toFragment2 -< a
        returnA -< P a'

instance AnotherFragmentInput a => AnotherFragmentInput (Plucker a) where
    toFragment2 = proc ~(Plucker a b c d e f) -> do
        a' <- toFragment2 -< a
        b' <- toFragment2 -< b
        c' <- toFragment2 -< c
        d' <- toFragment2 -< d
        e' <- toFragment2 -< e
        f' <- toFragment2 -< f
        returnA -< Plucker a' b' c' d' e' f'

------------------------------------------------------------------------------------------------------------------------------------

class AnotherFragmentInput a => FragmentCreator a where
    createFragment :: State Int a

instance FragmentCreator () where
    createFragment = return ()

instance FragmentCreator VFloat where
    createFragment = do
        n <- get
        put (n + 1)
        return $ S (return $ show n)

instance FragmentCreator FlatVFloat where
    createFragment = do
        n <- get
        put (n + 1)
        return $ Flat $ S (return $ show n)

instance FragmentCreator NoPerspectiveVFloat where
    createFragment = do
        n <- get
        put (n + 1)
        return $ NoPerspective $ S (return $ show n)

instance FragmentCreator VInt where
    createFragment = do
        n <- get
        put (n + 1)
        return $ S (return $ show n)

instance FragmentCreator VWord where
    createFragment = do
        n <- get
        put (n + 1)
        return $ S (return $ show n)

instance FragmentCreator VBool where
    createFragment = do
        n <- get
        put (n + 1)
        return $ S (return $ show n)

instance (FragmentCreator a) => FragmentCreator (V0 a) where
    createFragment = return V0

instance (FragmentCreator a) => FragmentCreator (V1 a) where
    createFragment = createFragment >>= return . V1

instance (FragmentCreator a) => FragmentCreator (V2 a) where
    createFragment = do
        x <- createFragment
        y <- createFragment
        return (V2 x y)

instance (FragmentCreator a) => FragmentCreator (V3 a) where
    createFragment = do
        x <- createFragment
        y <- createFragment
        z <- createFragment
        return (V3 x y z)

instance (FragmentCreator a) => FragmentCreator (V4 a) where
    createFragment = do
        x <- createFragment
        y <- createFragment
        z <- createFragment
        w <- createFragment
        return (V4 x y z w)

instance (FragmentCreator a, FragmentCreator b) => FragmentCreator (a,b) where
    createFragment = do
        x <- createFragment
        y <- createFragment
        return (x, y)

instance (FragmentCreator a, FragmentCreator b, FragmentCreator c) => FragmentCreator (a,b,c) where
    createFragment = do
        x <- createFragment
        y <- createFragment
        z <- createFragment
        return (x, y, z)

instance (FragmentCreator a, FragmentCreator b, FragmentCreator c, FragmentCreator d) => FragmentCreator (a,b,c,d) where
    createFragment = do
        x <- createFragment
        y <- createFragment
        z <- createFragment
        w <- createFragment
        return (x, y, z, w)

instance (FragmentCreator a, FragmentCreator b, FragmentCreator c, FragmentCreator d, FragmentCreator e) => FragmentCreator (a,b,c,d,e) where
    createFragment = do
        x <- createFragment
        y <- createFragment
        z <- createFragment
        w <- createFragment
        r <- createFragment
        return (x, y, z, w, r)

instance (FragmentCreator a, FragmentCreator b, FragmentCreator c, FragmentCreator d, FragmentCreator e, FragmentCreator f) => FragmentCreator (a,b,c,d,e,f) where
    createFragment = do
        x <- createFragment
        y <- createFragment
        z <- createFragment
        w <- createFragment
        r <- createFragment
        s <- createFragment
        return (x, y, z, w, r, s)

instance (FragmentCreator a, FragmentCreator b, FragmentCreator c, FragmentCreator d, FragmentCreator e, FragmentCreator f, FragmentCreator g) => FragmentCreator (a,b,c,d,e,f,g) where
    createFragment = do
        x <- createFragment
        y <- createFragment
        z <- createFragment
        w <- createFragment
        r <- createFragment
        s <- createFragment
        t <- createFragment
        return (x, y, z, w, r, s, t)

instance FragmentCreator a => FragmentCreator (Quaternion a) where
    createFragment = do
        a <- createFragment
        v <- createFragment
        return (Quaternion a v)

instance (FragmentCreator (f a), FragmentCreator a, FragmentFormat (f a) ~ f (FragmentFormat a)) => FragmentCreator (Point f a) where
    createFragment = do
        a <- createFragment
        return (P a)

instance FragmentCreator a => FragmentCreator (Plucker a) where
    createFragment = do
        a <- createFragment
        b <- createFragment
        c <- createFragment
        d <- createFragment
        e <- createFragment
        f <- createFragment
        return (Plucker a b c d e f)
