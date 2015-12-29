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
  SimpleGetter,
  SimpleFold,
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
A @SimpleGetter s a@ extracts @a@ from @s@; so, it's the same thing as @(s -> a)@, but you can use it in lens chains. For details, see 'Getting'.

The reason it's in this module is that the actual @Getter@ from lens is more general:

@
type Getter s a =
  forall f. (Contravariant f, Functor f) => (a -> f a) -> s -> f s
@

I'm not currently aware of any functions that take lens's @Getter@ but won't accept 'SimpleGetter', but you should try to avoid exporting 'SimpleGetter's anyway to minimise confusion.

Lens users: you can convert a 'SimpleGetter' to @Getter@ by applying @to . view@ to it.
-}
type SimpleGetter s a = forall r. Getting r s a

{- |
A @SimpleFold s a@ extracts several @a@s from @s@; so, it's pretty much the same thing as @(s -> [a])@, but you can use it with lens operators.

The reason it's in this module is that the actual @Fold@ from lens is more general:

@
type Fold s a =
  forall f. (Contravariant f, Applicative f) => (a -> f a) -> s -> f s
@

There are several functions in lens that accept lens's @Fold@ but won't accept 'SimpleFold'; I'm aware of
@<http://hackage.haskell.org/package/lens/docs/Control-Lens-Fold.html#v:takingWhile takingWhile>@,
@<http://hackage.haskell.org/package/lens/docs/Control-Lens-Fold.html#v:droppingWhile droppingWhile>@,
@<http://hackage.haskell.org/package/lens/docs/Control-Lens-Fold.html#v:backwards backwards>@,
@<http://hackage.haskell.org/package/lens/docs/Control-Lens-Fold.html#v:foldByOf foldByOf>@,
@<http://hackage.haskell.org/package/lens/docs/Control-Lens-Fold.html#v:foldMapByOf foldMapByOf>@.
For this reason, try not to use 'SimpleFold' if at all possible.

Lens users: you can convert a 'SimpleFold' to @Fold@ by applying @folded . toListOf@ to it.
-}
type SimpleFold s a = forall r. Applicative (Const r) => Getting r s a
