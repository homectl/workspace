{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.GLSL.Internal.Bits where

data B = O | I
  deriving (Show, Eq)

class Bits a where
  fill :: B -> a
  flat :: a -> [B]
  unflat :: [B] -> Maybe (a, [B])

instance Bits B where
  fill b = b
  flat a = [a]
  unflat (x:xs) = Just (x, xs)
  unflat _      = Nothing
instance (Bits a,Bits b) => Bits (a,b) where
  fill b = (fill b,fill b)
  flat (a,b) = flat a ++ flat b
  unflat xs = do
    (a,xsa) <- unflat xs
    (b,xsb) <- unflat xsa
    return ((a,b),xsb)
instance (Bits a,Bits b,Bits c) => Bits (a,b,c) where
  fill b = (fill b,fill b,fill b)
  flat (a,b,c) = flat a ++ flat b ++ flat c
  unflat xs = do
    (a,xsa) <- unflat xs
    (b,xsb) <- unflat xsa
    (c,xsc) <- unflat xsb
    return ((a,b,c),xsc)
instance (Bits a,Bits b,Bits c,Bits d) => Bits (a,b,c,d) where
  fill b = (fill b,fill b,fill b,fill b)
  flat (a,b,c,d) = flat a ++ flat b ++ flat c ++ flat d
  unflat xs = do
    (a,xsa) <- unflat xs
    (b,xsb) <- unflat xsa
    (c,xsc) <- unflat xsb
    (d,xsd) <- unflat xsc
    return ((a,b,c,d),xsd)
instance (Bits a,Bits b,Bits c,Bits d, Bits e) => Bits (a,b,c,d,e) where
  fill b = (fill b,fill b,fill b,fill b,fill b)
  flat (a,b,c,d,e) = flat a ++ flat b ++ flat c ++ flat d ++ flat e
  unflat xs = do
    (a,xsa) <- unflat xs
    (b,xsb) <- unflat xsa
    (c,xsc) <- unflat xsb
    (d,xsd) <- unflat xsc
    (e,xse) <- unflat xsd
    return ((a,b,c,d,e),xse)

zero, one :: Bits a => a
zero = fill O
one = fill I

class Expandable a b where
  expand :: a -> b

instance Expandable a a where
  expand x = x
-- TODO: can this be more generic?
instance Expandable (B,B) (B,B,B,B,B) where
  expand (a,b) = expand (a,b,O)
instance Expandable (B,B,B) (B,B,B,B,B) where
  expand (a,b,c) = expand (a,b,c,O)
instance Expandable (B,B,B,B) (B,B,B,B,B) where
  expand (a,b,c,d) = expand (a,b,c,d,O)
