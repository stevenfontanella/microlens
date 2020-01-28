{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}


{- |
Module      :  Lens.Micro.Contra
Copyright   :  (C) 2013-2016 Edward Kmett, 2015-2016 Artyom Kazak, 2018 Monadfix
License     :  BSD-style (see the file LICENSE)

This module provides types and functions that require 'Contravariant'; they aren't included in the main microlens package because <http://hackage.haskell.org/package/contravariant contravariant> has a lot of dependencies.

Starting from GHC 8.6, 'Contravariant' is included in base, the same functions are conditionally provided by microlens, and this package simply reexports the microlens variants (depending on the microlens version).
-}
module Lens.Micro.Contra
(
  -- * Getter
  Getter,
  fromSimpleGetter,

  -- * Fold
  Fold,
  fromSimpleFold,
)
where


#if MIN_VERSION_microlens(0,4,12) && (__GLASGOW_HASKELL__ >= 706)

import Lens.Micro

#else

import Lens.Micro
import Lens.Micro.Extras (view)

import Data.Foldable (traverse_)
import Data.Functor.Contravariant (phantom, Contravariant)

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

{- |
This is the same thing as 'SimpleGetter' but more generalised (so that it would fully match the type used in lens).
-}
type Getter s a =
  forall f. (Contravariant f, Functor f) => (a -> f a) -> s -> f s

{- |
Turn a 'SimpleGetter' into a true 'Getter'.
-}
fromSimpleGetter :: SimpleGetter s a -> Getter s a
fromSimpleGetter g f = phantom . f . view g
{-# INLINE fromSimpleGetter #-}

{- |
This is the same thing as 'SimpleFold' but more generalised (so that it would fully match the type used in lens). See documentation of 'SimpleFold' for the list of functions that work on 'Fold' but don't work on 'SimpleFold'.
-}
type Fold s a =
  forall f. (Contravariant f, Applicative f) => (a -> f a) -> s -> f s

{- |
Turn a 'SimpleFold' into a true 'Fold'.
-}
fromSimpleFold :: SimpleFold s a -> Fold s a
fromSimpleFold g f = phantom . traverse_ f . toListOf g
{-# INLINE fromSimpleFold #-}

#endif
