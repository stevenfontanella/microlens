{-# LANGUAGE
CPP,
DefaultSignatures,
MultiParamTypeClasses,
FunctionalDependencies,
GADTs,
FlexibleInstances,
UndecidableInstances
  #-}


module Lens.Micro.Each
(
  Each(..),
)
where


import Data.Complex
import Lens.Micro

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
import Data.Traversable
#endif


{- |
A class to support 'each'. If you're writing a library, don't write instances of this class which would be exported – other users won't be able to use them if they use lens.
-}
class Each s t a b | s -> a, t -> b, s b -> t, t a -> s where
  {- |
'each' tries to be a universal 'Traversal' – it behaves like 'traverse' in most situations, but also adds support for e.g. tuples with same-typed values:

>>> (1,2) & each %~ succ
(2,3)

>>> ["x", "y", "z"] ^. each
"xyz"

However, note that 'each' doesn't work on /every/ instance of 'Traversable' – the full list of instances can be found below.
  -}
  each :: Traversal s t a b
  default each :: (Traversable g, s ~ g a, t ~ g b) => Traversal s t a b
  each = traverse

-- | @'each' :: 'Traversal' (a,a) (b,b) a b@
instance (a~a', b~b') => Each (a,a') (b,b') a b where
  each f ~(a,b) = (,) <$> f a <*> f b
  {-# INLINE each #-}

-- | @'each' :: 'Traversal' (a,a,a) (b,b,b) a b@
instance (a~a2, a~a3, b~b2, b~b3) => Each (a,a2,a3) (b,b2,b3) a b where
  each f ~(a,b,c) = (,,) <$> f a <*> f b <*> f c
  {-# INLINE each #-}

-- | @'each' :: 'Traversal' (a,a,a,a) (b,b,b,b) a b@
instance (a~a2, a~a3, a~a4, b~b2, b~b3, b~b4) => Each (a,a2,a3,a4) (b,b2,b3,b4) a b where
  each f ~(a,b,c,d) = (,,,) <$> f a <*> f b <*> f c <*> f d
  {-# INLINE each #-}

-- | @'each' :: 'Traversal' (a,a,a,a,a) (b,b,b,b,b) a b@
instance (a~a2, a~a3, a~a4, a~a5, b~b2, b~b3, b~b4, b~b5) => Each (a,a2,a3,a4,a5) (b,b2,b3,b4,b5) a b where
  each f ~(a,b,c,d,e) = (,,,,) <$> f a <*> f b <*> f c <*> f d <*> f e
  {-# INLINE each #-}

-- | @'each' :: 'Traversal' (a,a,a,a,a,a) (b,b,b,b,b,b) a b@
instance (a~a2, a~a3, a~a4, a~a5, a~a6, b~b2, b~b3, b~b4, b~b5, b~b6) => Each (a,a2,a3,a4,a5,a6) (b,b2,b3,b4,b5,b6) a b where
  each f ~(a,b,c,d,e,g) = (,,,,,) <$> f a <*> f b <*> f c <*> f d <*> f e <*> f g
  {-# INLINE each #-}

-- | @'each' :: ('RealFloat' a, 'RealFloat' b) => 'Traversal' ('Complex' a) ('Complex' b) a b@
instance Each (Complex a) (Complex b) a b where
  each f (a :+ b) = (:+) <$> f a <*> f b
  {-# INLINE each #-}

-- | @'each' :: 'Traversal' [a] [b] a b@
instance Each [a] [b] a b

-- | @'each' :: 'Traversal' ('Maybe' a) ('Maybe' b) a b@
instance Each (Maybe a) (Maybe b) a b
