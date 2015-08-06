{-# LANGUAGE
CPP,
FlexibleContexts,
FlexibleInstances
  #-}

{- |
This module is needed to give other packages from the microlens family (like <http://hackage.haskell.org/package/microlens-ghc microlens-ghc>) access to functions that don't need to be exported from "Lens.Micro" (because they just clutter the namespace). Also, okay, uh, e.g. 'traversed' is here because otherwise there'd be a dependency cycle.
-}
module Lens.Micro.Internal
(
  traversed,
  folded,
  foldring,
  foldrOf,
  foldMapOf,
  sets,
)
where


import Lens.Micro.Type

import Data.Functor.Identity
import Data.Monoid
import Control.Applicative
import Data.Foldable as F

#if __GLASGOW_HASKELL__ < 710
import Data.Traversable
#endif
  

{- |
'traversed' traverses any 'Traversable' container (list, vector, @Map@, 'Maybe', you name it):

>>> Just 1 ^.. traversed
[1]

'traversed' is the same as 'traverse', but can be faster thanks to magic rewrite rules.
-}
traversed :: Traversable f => Traversal (f a) (f b) a b
traversed = traverse
{-# INLINE [0] traversed #-}

{-# RULES
"traversed -> mapped"
  traversed = sets fmap :: Functor f => ASetter (f a) (f b) a b;
"traversed -> folded"
  traversed = folded :: Foldable f => Getting (Endo r) (f a) a;
  #-}

{- |
'folded' is a fold for anything 'Foldable'. In a way, it's an opposite of
'mapped' – the most powerful getter, but can't be used as a setter.
-}
folded :: (Foldable f, Applicative (Const r)) => Getting r (f a) a
folded = foldring F.foldr
{-# INLINE folded #-}

foldring :: (Applicative (Const r)) => ((a -> Const r a -> Const r a) -> Const r a -> s -> Const r a) -> (a -> Const r b) -> s -> Const r t
foldring fr f = phantom . fr (\a fa -> f a *> fa) noEffect
{-# INLINE foldring #-}

foldrOf :: Getting (Endo r) s a -> (a -> r -> r) -> r -> s -> r
foldrOf l f z = flip appEndo z . foldMapOf l (Endo . f)
{-# INLINE foldrOf #-}

foldMapOf :: Getting r s a -> (a -> r) -> s -> r
foldMapOf l f = getConst . l (Const . f)
{-# INLINE foldMapOf #-}

{- |
'sets' creates an 'ASetter' from an ordinary function. (The only thing it does is wrapping and unwrapping 'Identity'.)
-}
sets :: ((a -> b) -> s -> t) -> ASetter s t a b
sets f g = Identity . f (runIdentity . g)
{-# INLINE sets #-}

------------------------------------------------------------------------------
-- Control.Lens.Internal.Getter
------------------------------------------------------------------------------

-- was renamed from “coerce”
phantom :: Const r a -> Const r b
phantom = Const . getConst
{-# INLINE phantom #-}

noEffect :: Applicative (Const r) => Const r a
noEffect = phantom (pure ())
{-# INLINE noEffect #-}
