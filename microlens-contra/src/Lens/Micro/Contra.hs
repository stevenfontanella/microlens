{-# LANGUAGE
TemplateHaskell,
RankNTypes,
FlexibleContexts,
Safe
  #-}


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


import Lens.Micro
import Lens.Micro.Extras (view)

import Data.Foldable (traverse_)
import Data.Functor.Contravariant (phantom, Contravariant)


type Getter s a =
  forall f. (Contravariant f, Functor f) => (a -> f a) -> s -> f s

fromSimpleGetter :: SimpleGetter s a -> Getter s a
fromSimpleGetter g f = phantom . f . view g
{-# INLINE fromSimpleGetter #-}

type Fold s a =
  forall f. (Contravariant f, Applicative f) => (a -> f a) -> s -> f s

fromSimpleFold :: SimpleFold s a -> Fold s a
fromSimpleFold g f = phantom . traverse_ f . toListOf g
{-# INLINE fromSimpleFold #-}
