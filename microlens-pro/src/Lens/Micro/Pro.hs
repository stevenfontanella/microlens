{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}

#ifndef MIN_VERSION_profunctors
#define MIN_VERSION_profunctors(x,y,z) 0
#endif

{- |
Module      :  Lens.Micro.Pro
Copyright   :  (C) 2013-2016 Edward Kmett, 2018 Monadfix
License     :  BSD-style (see the file LICENSE)

This module provides types and functions that require 'Profunctor'; they aren't included in the main microlens package because <http://hackage.haskell.org/package/profunctors profunctors> has a lot of dependencies.
-}
module Lens.Micro.Pro
(
  -- * Reexports
  -- $reexports-note
  module Lens.Micro,

  -- * Isomorphism: losslessly converts between types
  Iso, Iso',
  iso,
  withIso,
  from,
  non,
  non',
  enum,
  mapping,
#if __GLASGOW_HASKELL__ >= 708
  coerced,
#endif

  -- * Prism: inspects sum types (half-traversal, half-isomorphism)
  Prism, Prism',
  prism, prism',
  -- ** Common prisms
  _Left, _Right,
  _Just, _Nothing,
  only,

  -- * Review: constructs a branch of a sum type
  AReview,
  review,
  unto,

  -- * Generating prisms and isomorphisms
  makePrisms,
  makeClassyPrisms,
)
where


import Control.Monad.Reader
import Data.Profunctor
import Data.Profunctor.Unsafe
import Data.Tagged
import Data.Functor.Identity
import Data.Maybe
import Lens.Micro hiding (non, _Left, _Right, _Just, _Nothing)
import Lens.Micro.Pro.TH
import Lens.Micro.Pro.Internal
import Lens.Micro.Internal (coerce, coerce')

#if __GLASGOW_HASKELL__ >= 708
import Data.Coerce (Coercible)
#endif

{- $reexports-note

We reexport stuff. But if you want to combine microlens-platform and microlens-pro, well, that's a bit trickier.

-}

----------------------------------------------------------------------------
-- Isomorphisms
----------------------------------------------------------------------------

from :: AnIso s t a b -> Iso b a t s
from l = withIso l $ \ sa bt -> iso bt sa
{-# INLINE from #-}

enum :: Enum a => Iso' Int a
enum = iso toEnum fromEnum
{-# INLINE enum #-}

-- | This can be used to lift any 'Iso' into an arbitrary 'Functor'.
mapping
  :: (Functor f, Functor g)
  => AnIso s t a b -> Iso (f s) (g t) (f a) (g b)
mapping k = withIso k $ \ sa bt -> iso (fmap sa) (fmap bt)
{-# INLINE mapping #-}

non :: Eq a => a -> Iso' (Maybe a) a
non = non' . only
{-# INLINE non #-}

non' :: APrism' a () -> Iso' (Maybe a) a
non' p = iso (fromMaybe def) go where
  def                           = review (clonePrism p) ()
  go b | has (clonePrism p) b   = Nothing
       | otherwise              = Just b
{-# INLINE non' #-}

#if __GLASGOW_HASKELL__ >= 708
-- | Data types that are representationally equal are isomorphic.
--
-- This is only available on GHC 7.8+
coerced :: forall s t a b. (Coercible s a, Coercible t b) => Iso s t a b
# if __GLASGOW_HASKELL__ >= 710
coerced l = rmap (fmap coerce') l .# coerce
# else
coerced l = case sym Coercion :: Coercion a s of
              Coercion -> rmap (fmap coerce') l .# coerce
# endif
{-# INLINE coerced #-}
#endif

----------------------------------------------------------------------------
-- Prisms
----------------------------------------------------------------------------

prism' :: (b -> s) -> (s -> Maybe a) -> Prism s s a b
prism' bs sma = prism bs (\s -> maybe (Left s) Right (sma s))
{-# INLINE prism' #-}

_Left :: Prism (Either a c) (Either b c) a b
_Left = prism Left $ either Right (Left . Right)
{-# INLINE _Left #-}

_Right :: Prism (Either c a) (Either c b) a b
_Right = prism Right $ either (Left . Left) Right
{-# INLINE _Right #-}

_Just :: Prism (Maybe a) (Maybe b) a b
_Just = prism Just $ maybe (Left Nothing) Right
{-# INLINE _Just #-}

_Nothing :: Prism' (Maybe a) ()
_Nothing = prism' (const Nothing) $ maybe (Just ()) (const Nothing)
{-# INLINE _Nothing #-}

only :: Eq a => a -> Prism' a ()
only a = prism' (\() -> a) $ guard . (a ==)
{-# INLINE only #-}

----------------------------------------------------------------------------
-- Review
----------------------------------------------------------------------------

review :: MonadReader b m => AReview t b -> m t
review p = asks (runIdentity #. unTagged #. p .# Tagged .# Identity)
{-# INLINE review #-}
