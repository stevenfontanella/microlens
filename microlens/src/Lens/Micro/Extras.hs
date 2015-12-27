{-# LANGUAGE
RankNTypes,
FlexibleContexts,
Trustworthy
  #-}


{- |
This module provides functions and types that are inferior to their lens counterparts.
-}
module Lens.Micro.Extras
(
  view,
  preview,
  Getter,
  Fold,
)
where


import Lens.Micro
import Lens.Micro.Internal

import Control.Applicative
import Data.Monoid


{- |
'view' is a synonym for ('^.'):

>>> view _1 (1, 2)
1

The reason it's in this module is that @view@ in lens has a more general signature:

@
view :: MonadReader s m => Getting a s a -> m a
@

So, you would be able to use this 'view' with functions, but not in various reader monads. For most people this shouldn't be an issue.
-}
view :: Getting a s a -> s -> a
view l = getConst #. l Const
{-# INLINE view #-}

{- |
'preview' is a synonym for ('^?'):

>>> preview _head [1,2,3]
Just 1

The reason it's in this module is that @preview@ in lens has a more general signature:

@
preview :: MonadReader s m => Getting (First a) s a -> m (Maybe a)
@

So, just like with 'view', you would be able to use this 'preview' with functions, but not in reader monads.
-}
preview :: Getting (First a) s a -> s -> Maybe a
preview l = getFirst #. foldMapOf l (First #. Just)
{-# INLINE preview #-}

{- |
A @Getter s a@ extracts @a@ from @s@; so, it's the same thing as @(s -> a)@, but you can use it in lens chains. For details, see 'Getting'.

The reason it's in this module is that the actual @Getter@ from lens is more general:

@
type Getter s a =
  forall f. (Contravariant f, Functor f) => (a -> f a) -> s -> f s
@

I'm not currently aware of any functions that take lens's @Getter@ but won't accept this @Getter@, but you should try to avoid exporting 'Getter's anyway to minimise confusion.

Lens users: you can convert fake getters to real getters by applying @to . view@ to them.
-}
type Getter s a = forall r. Getting r s a

{- |
A @Fold s a@ extracts several @a@s from @s@; so, it's pretty much the same thing as @(s -> [a])@, but you can use it with lens operators.

The reason it's in this module is that the actual @Fold@ from lens is more general:

@
type Fold s a =
  forall f. (Contravariant f, Applicative f) => (a -> f a) -> s -> f s
@

I'm only aware of 2 functions that accept lens's @Fold@ but won't accept this @Fold@ – they are @<http://hackage.haskell.org/package/lens/docs/Control-Lens-Fold.html#v:foldByOf foldByOf>@ and @<http://hackage.haskell.org/package/lens-4.13/docs/Control-Lens-Fold.html#v:foldMapByOf foldMapByOf>@. They aren't used often, but you should try to avoid exporting 'Fold's anyway to minimise confusion (and to prevent people cursing you when they want to use @foldByOf@ with your fold).

Lens users: you can convert fake folds to real folds by applying @folded . toListOf@ to them.
-}
type Fold s a = forall r. Applicative (Const r) => Getting r s a
