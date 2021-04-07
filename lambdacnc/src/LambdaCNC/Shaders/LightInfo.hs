{-# LANGUAGE Arrows            #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module LambdaCNC.Shaders.LightInfo where

import           Control.Applicative (Applicative (..))
import           Control.Arrow       (returnA)
import           Data.Default        (Default (..))
import           Data.Maybe          (fromMaybe)
import           Graphics.GPipe      (FragmentInput (..), S, V4)

--------------------------------------------------

instance Default a => Default (V4 a) where
    def = pure def

instance Default (S x Float) where
    def = fromRational def

--------------------------------------------------

data LightInfo a = LightInfo
    (Maybe a)
    (Maybe a)
    (Maybe a)
    (Maybe a)
    (Maybe a)
    (Maybe a)
    (Maybe a)
    (Maybe a)
    (Maybe a)
    (Maybe a)
    deriving (Functor, Foldable, Traversable, Show)

instance Default (LightInfo a) where
    def = LightInfo
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing

instance Applicative LightInfo where
    pure a = LightInfo
        (Just a)
        (Just a)
        (Just a)
        (Just a)
        (Just a)
        (Just a)
        (Just a)
        (Just a)
        (Just a)
        (Just a)
    liftA2 f (LightInfo
                a1
                a2
                a3
                a4
                a5
                a6
                a7
                a8
                a9
                a10
              )
             (LightInfo
                b1
                b2
                b3
                b4
                b5
                b6
                b7
                b8
                b9
                b10
              ) = LightInfo
        (liftA2 f a1 b1)
        (liftA2 f a2 b2)
        (liftA2 f a3 b3)
        (liftA2 f a4 b4)
        (liftA2 f a5 b5)
        (liftA2 f a6 b6)
        (liftA2 f a7 b7)
        (liftA2 f a8 b8)
        (liftA2 f a9 b9)
        (liftA2 f a10 b10)

instance (Default a, FragmentInput a) => FragmentInput (LightInfo a) where
    type FragmentFormat (LightInfo a) = LightInfo (FragmentFormat a)
    toFragment =
        proc ~(LightInfo
            a
            b
            c
            d
            e
            f
            g
            h
            i
            j
        ) -> do
            a' <- toFragment -< fromMaybe def a
            b' <- toFragment -< fromMaybe def b
            c' <- toFragment -< fromMaybe def c
            d' <- toFragment -< fromMaybe def d
            e' <- toFragment -< fromMaybe def e
            f' <- toFragment -< fromMaybe def f
            g' <- toFragment -< fromMaybe def g
            h' <- toFragment -< fromMaybe def h
            i' <- toFragment -< fromMaybe def i
            j' <- toFragment -< fromMaybe def j
            returnA -< LightInfo
                (a' <$ a)
                (b' <$ b)
                (c' <$ c)
                (d' <$ d)
                (e' <$ e)
                (f' <$ f)
                (g' <$ g)
                (h' <$ h)
                (i' <$ i)
                (j' <$ j)

fromList :: [a] -> LightInfo a
fromList l =
    case expanded of
        (
          a1:
          a2:
          a3:
          a4:
          a5:
          a6:
          a7:
          a8:
          a9:
          a10:
          _) ->
            LightInfo
              a1
              a2
              a3 
              a4 
              a5 
              a6 
              a7 
              a8 
              a9 
              a10
        _ -> def
  where
    expanded = map Just l ++ repeat Nothing
