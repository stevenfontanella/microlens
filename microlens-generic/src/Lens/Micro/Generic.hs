{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Lenses for the generic types and for types that have a Generic instance.
module Lens.Micro.Generic
  ( rep
  , m1
  , k1

  , g1
  , g2
  ) where

import Lens.Micro (Lens, _1, _2, lens)
import Lens.Micro.Internal (Field1, Field2)
import GHC.Generics (Generic, M1(unM1,M1), S1, K1(unK1,K1), Rep, (:*:)((:*:)), Rec0)
import GHC.Generics qualified as G
import Data.Kind (Type, Constraint)

-- * Optics for generic data

-- | Gives generic access to the first field.
--
-- Examples
-- @
-- data AB = AB Int Double deriving (Show, Generic)
--
-- getFirstABC :: Int
-- getFirstABC = AB 1 2 ^. g1
--
-- setFirstABC :: AB
-- setFirstABC = AB 1 2 & g1 %~ (* 2)
--
-- getTheFirstGenerically :: Int
-- getTheFirstGenerically = (1 :: Int, 2 :: Double) ^. g1
--
-- setTheFirstGenerically :: (Char, Double)
-- setTheFirstGenerically = (1 :: Int, 2 :: Double) & g1 .~ 'a'
-- @
g1 :: (G1 (Rep s) (Rep t) b, Generic s, Generic t) => Lens s t (G1A (Rep s)) b
g1 = rep . generic1
type G1 :: (Type -> Type) -> (Type -> Type) -> Type -> Constraint
class G1 s t b where
  type G1A s :: Type
  generic1 :: Lens (s p) (t p) (G1A s) b
instance G1 s t b => G1 (M1 i c s) (M1 i c t) b where
  type G1A (M1 i c s) = G1A s
  generic1 = m1 . generic1
instance G1 s t b => G1 (s :*: g) (t :*: g) b where
  type G1A (s :*: g) = G1A s
  generic1 = _1 . generic1
instance G1 (Rec0 a) (Rec0 b) b where
  type G1A (Rec0 a) = a
  generic1 = k1

-- | Gives generic access to the second field.
--
-- Examples
-- @
-- getSecond :: Double
-- getSecond = (1 :: Int, 2 :: Double) ^. g2
--
-- setSecond :: (Int, Char)
-- setSecond = (1 :: Int, 2 :: Double) & (g2 :: Lens (Int,Double) (Int,Char) Double Char) .~ 'a'
-- @
--
-- Not implemented for types with more than one product like
-- @
-- notImplemented :: Double
-- notImplemented = (1 :: Int, 2 :: Double, 3 :: Integer) ^. g2
-- @
g2 :: (G2 (Rep s) (Rep t) b, Generic s, Generic t) => Lens s t (G2A (Rep s)) b
g2 = rep . g2'
type G2 :: (Type -> Type) -> (Type -> Type) -> Type -> Constraint
class G2 s t b where
  type G2A s :: Type
  g2' :: Lens (s p) (t p) (G2A s) b
instance G2 s t b => G2 (M1 i c s) (M1 i c t) b where
  type G2A (M1 i c s) = G2A s
  g2' = m1 . g2'
--instance G2 (f :*: g) (f' :*: g') a b => G2 ((f :*: g) :*: h) ((f' :*: g') :*: h) a b where g2' = _1 . g2'
--instance G2 (g :*: h) (g' :*: h') a b => G2 (f :*: (g :*: h)) (f :*: (g' :*: h')) a b where g2' = _2 . g2'
instance G2 s t b => G2 (f :*: S1 m s) (f :*: S1 m t) b where
  type G2A (f :*: S1 m s) = G2A s
  g2' = _2 . g2'
instance G2 (Rec0 a) (Rec0 b) b where
  type G2A (Rec0 a) = a
  g2' = k1

-- * Optics for the generic representitive types
--
-- ** Some examples working using representitive type optics
--
-- | Example of getting a value out of a tuple
-- > getTheFirst :: Int
-- > getTheFirst = (1 :: Int, 2 :: Double) ^. rep . m1 . m1 . _1 . m1 . k1
--
-- Example of setting a value in a tuple
-- > setTheFirst :: (Char, Double)
-- > setTheFirst = (1 :: Int, 2 :: Double) & rep . m1 . m1 . _1 . m1 . k1 .~ 'd'

rep :: (Generic s, Generic t) => Lens s t (Rep s p) (Rep t p)
rep = lens G.from $ const G.to

m1 :: Lens (M1 i c f p) (M1 i c g p) (f p) (g p)
m1 = lens unM1 $ const M1

k1 :: Lens (K1 i a p) (K1 i b p) a b
k1 = lens unK1 $ const K1

instance Field1 ((s :*: s') p) ((s'' :*: s') p) (s p) (s'' p) where
  _1 = lens (\(x :*: _) -> x) (\(_ :*: y) x -> x :*: y)

instance Field2 ((s :*: s') p) ((s :*: s'') p) (s' p) (s'' p) where
  _2 = lens (\(_ :*: y) -> y) (\(x :*: _) y -> x :*: y)

