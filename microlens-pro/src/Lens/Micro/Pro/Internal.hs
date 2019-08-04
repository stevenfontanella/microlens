{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TypeInType #-}
#endif

{- |
Module      :  Lens.Micro.Pro.Internal
Copyright   :  (C) 2012-2016 Edward Kmett; 2018 Monadfix
License     :  BSD-style (see the file LICENSE)
-}
module Lens.Micro.Pro.Internal
(
  -- * Iso
  Iso, Iso',
  iso,
  Exchange(..),
  AnIso, AnIso',
  withIso,

  -- * Prism
  Prism, Prism',
  prism,
  Market(..),
  APrism, APrism',
  withPrism,
  clonePrism,

  -- * Review
  SimpleReview,
  unto,
  AReview,
)
where

import Data.Profunctor
import Data.Profunctor.Unsafe
import Data.Bifunctor
import Data.Functor.Identity
import Data.Void
import Data.Tagged
import Lens.Micro.Internal (coerce, coerce')

#if __GLASGOW_HASKELL__ >= 800
import GHC.Exts (TYPE)
#endif

----------------------------------------------------------------------------
-- Iso
----------------------------------------------------------------------------

type Iso s t a b =
  forall p f. (Profunctor p, Functor f)
  => p a (f b) -> p s (f t)

type Iso' s a = Iso s s a a

type AnIso s t a b
    = Exchange a b a (Identity b) -> Exchange a b s (Identity t)

type AnIso' s a = AnIso s s a a

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (fmap bt)
{-# INLINE iso #-}

data Exchange a b s t = Exchange (s -> a) (b -> t)

instance Functor (Exchange a b s) where
  fmap f (Exchange sa bt) = Exchange sa (f . bt)
  {-# INLINE fmap #-}

instance Profunctor (Exchange a b) where
  dimap f g (Exchange sa bt) = Exchange (sa . f) (g . bt)
  {-# INLINE dimap #-}
  lmap f (Exchange sa bt) = Exchange (sa . f) bt
  {-# INLINE lmap #-}
  rmap f (Exchange sa bt) = Exchange sa (f . bt)
  {-# INLINE rmap #-}
  ( #. ) _ = coerce'
  {-# INLINE ( #. ) #-}
  ( .# ) p _ = coerce p
  {-# INLINE ( .# ) #-}

-- | Extract the two functions, one from @s -> a@ and
-- one from @b -> t@ that characterize an 'Iso'.
#if __GLASGOW_HASKELL__ >= 800
withIso
  :: forall s t a b rep (r :: TYPE rep)
   . AnIso s t a b -> ((s -> a) -> (b -> t) -> r) -> r
#else
withIso
  :: AnIso s t a b -> ((s -> a) -> (b -> t) -> r) -> r
#endif
withIso ai k = case ai (Exchange id Identity) of
  Exchange sa bt -> k sa (runIdentity #. bt)
{-# INLINE withIso #-}

----------------------------------------------------------------------------
-- Prism
----------------------------------------------------------------------------

type Prism s t a b =
  forall p f. (Choice p, Applicative f)
  => p a (f b) -> p s (f t)

type Prism' s a = Prism s s a a

type APrism s t a b =
    Market a b a (Identity b) -> Market a b s (Identity t)

type APrism' s a = APrism s s a a

data Market a b s t = Market (b -> t) (s -> Either t a)

instance Functor (Market a b s) where
  fmap f (Market bt seta) = Market (f . bt) (either (Left . f) Right . seta)
  {-# INLINE fmap #-}

instance Profunctor (Market a b) where
  dimap f g (Market bt seta) = Market (g . bt) (either (Left . g) Right . seta . f)
  {-# INLINE dimap #-}
  lmap f (Market bt seta) = Market bt (seta . f)
  {-# INLINE lmap #-}
  rmap f (Market bt seta) = Market (f . bt) (either (Left . f) Right . seta)
  {-# INLINE rmap #-}
  ( #. ) _ = coerce'
  {-# INLINE ( #. ) #-}
  ( .# ) p _ = coerce p
  {-# INLINE ( .# ) #-}

instance Choice (Market a b) where
  left' (Market bt seta) = Market (Left . bt) $ \sc -> case sc of
    Left s -> case seta s of
      Left t -> Left (Left t)
      Right a -> Right a
    Right c -> Left (Right c)
  {-# INLINE left' #-}
  right' (Market bt seta) = Market (Right . bt) $ \cs -> case cs of
    Left c -> Left (Left c)
    Right s -> case seta s of
      Left t -> Left (Right t)
      Right a -> Right a
  {-# INLINE right' #-}

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap seta (either pure (fmap bt)) . right'
{-# INLINE prism #-}

-- | Convert 'APrism' to the pair of functions that characterize it.
withPrism :: APrism s t a b -> ((b -> t) -> (s -> Either t a) -> r) -> r
withPrism k f = case coerce (k (Market Identity Right)) of
  Market bt seta -> f bt seta
{-# INLINE withPrism #-}

-- | Clone a 'Prism' so that you can reuse the same monomorphically typed
-- 'Prism' for different purposes.
clonePrism :: APrism s t a b -> Prism s t a b
clonePrism k = withPrism k prism
{-# INLINE clonePrism #-}

----------------------------------------------------------------------------
-- Review
----------------------------------------------------------------------------

type SimpleReview t b =
  forall p. (Choice p, Bifunctor p)
  => p b (Identity b) -> p t (Identity t)

type AReview t b
    = Tagged b (Identity b) -> Tagged t (Identity t)

unto :: (Profunctor p, Bifunctor p, Functor f)
     => (b -> t) -> p a (f b) -> p s (f t)
unto f = first absurd . lmap absurd . rmap (fmap f)
{-# INLINE unto #-}
