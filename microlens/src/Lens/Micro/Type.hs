{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}


{- |
Module      :  Lens.Micro.Type
Copyright   :  (C) 2013-2016 Edward Kmett, 2015-2016 Artyom Kazak, 2018 Monadfix
License     :  BSD-style (see the file LICENSE)

This module provides just the types ('Lens', 'Traversal', etc). It's needed to break the dependency cycle – "Lens.Micro" depends on "Lens.Micro.Internal", but "Lens.Micro.Internal" needs types like 'Lens', so 'Lens' can't be defined in "Lens.Micro".
-}
module Lens.Micro.Type
(
  ASetter, ASetter',
  SimpleGetter, Getting,
#if MIN_VERSION_base(4,12,0)
  Getter,
#endif
  SimpleFold,
#if MIN_VERSION_base(4,12,0)
  Fold,
#endif
  Lens, Lens',
  Traversal, Traversal',
  LensLike, LensLike',
)
where


import Control.Applicative
import Data.Functor.Identity

#if MIN_VERSION_base(4,12,0)
import Data.Functor.Contravariant
#endif

#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif


{- |
@ASetter s t a b@ is something that turns a function modifying a value into a function modifying a /structure/. If you ignore 'Identity' (as @Identity a@ is the same thing as @a@), the type is:

@
type ASetter s t a b = (a -> b) -> s -> t
@

The reason 'Identity' is used here is for 'ASetter' to be composable with other types, such as 'Lens'.

Technically, if you're writing a library, you shouldn't use this type for setters you are exporting from your library; the right type to use is @<http://hackage.haskell.org/package/lens/docs/Control-Lens-Setter.html#t:Setter Setter>@, but it is not provided by this package (because then it'd have to depend on <http://hackage.haskell.org/package/distributive distributive>). It's completely alright, however, to export functions which take an 'ASetter' as an argument.
-}
type ASetter s t a b = (a -> Identity b) -> s -> Identity t

{- |
This is a type alias for monomorphic setters which don't change the type of the container (or of the value inside). It's useful more often than the same type in lens, because we can't provide real setters and so it does the job of both @<http://hackage.haskell.org/package/lens/docs/Control-Lens-Setter.html#t:ASetter-39- ASetter'>@ and @<http://hackage.haskell.org/package/lens/docs/Control-Lens-Setter.html#t:Setter-39- Setter'>@.
-}
type ASetter' s a = ASetter s s a a

{- |
__Note: starting from GHC 8.6, 'Getter' is available and you should use it instead of 'SimpleGetter'. For the time being, microlens-th still generates 'SimpleGetter's instead of 'Getter's.__

A @SimpleGetter s a@ extracts @a@ from @s@; so, it's the same thing as @(s -> a)@, but you can use it in lens chains because its type looks like this:

@
type SimpleGetter s a =
  forall r. (a -> Const r a) -> s -> Const r s
@

Since @Const r@ is a functor, 'SimpleGetter' has the same shape as other lens types and can be composed with them. To get @(s -> a)@ out of a 'SimpleGetter', choose @r ~ a@ and feed @Const :: a -> Const a a@ to the getter:

@
-- the actual signature is more permissive:
-- 'Lens.Micro.Extras.view' :: 'Getting' a s a -> s -> a
'Lens.Micro.Extras.view' :: 'SimpleGetter' s a -> s -> a
'Lens.Micro.Extras.view' getter = 'getConst' . getter 'Const'
@

The actual @<http://hackage.haskell.org/package/lens/docs/Control-Lens-Getter.html#t:Getter Getter>@ from lens is more general:

@
type Getter s a =
  forall f. (Contravariant f, Functor f) => (a -> f a) -> s -> f s
@

I am not currently aware of any functions that take lens's @Getter@ but won't accept 'SimpleGetter', but you should try to avoid exporting 'SimpleGetter's anyway to minimise confusion. Starting from GHC 8.6, microlens provides a proper 'Getter'. If you need to support older GHC versions as well, use <http://hackage.haskell.org/package/microlens-contra microlens-contra>.

Lens users: you can convert a 'SimpleGetter' into a @Getter@ by applying @to . view@ to it.
-}
type SimpleGetter s a = forall r. Getting r s a

#if MIN_VERSION_base(4,12,0)
{- |
Like 'SimpleGetter', but fully generalized – an exact copy of @<http://hackage.haskell.org/package/lens/docs/Control-Lens-Getter.html#t:Getter Getter>@ from lens.

Only available starting from GHC 8.6 because it needs 'Contravariant' in base.

@since 0.4.12
-}
type Getter s a = forall f. (Contravariant f, Functor f) => (a -> f a) -> s -> f s
#endif

{- |
Functions that operate on getters and folds – such as ('Lens.Micro.^.'), ('Lens.Micro.^..'), ('Lens.Micro.^?') – use @Getter r s a@ (with different values of @r@) to describe what kind of result they need. For instance, ('Lens.Micro.^.') needs the getter to be able to return a single value, and so it accepts a getter of type @Getting a s a@. ('Lens.Micro.^..') wants the getter to gather values together, so it uses @Getting (Endo [a]) s a@ (it could've used @Getting [a] s a@ instead, but it's faster with 'Data.Monoid.Endo'). The choice of @r@ depends on what you want to do with elements you're extracting from @s@.
-}
type Getting r s a = (a -> Const r a) -> s -> Const r s

{- |
__Note: starting from GHC 8.6, 'Fold' is available and you should use it instead of 'SimpleFold'. For the time being, microlens-th still generates 'SimpleFold's instead of 'Fold's.__

A @SimpleFold s a@ extracts several @a@s from @s@; so, it's pretty much the same thing as @(s -> [a])@, but you can use it with lens operators.

The actual @Fold@ from lens is more general:

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
For this reason, try not to export 'SimpleFold's if at all possible. Starting from GHC 8.6, microlens provides a proper 'Fold'. If you need to support older GHC versions as well, use <http://hackage.haskell.org/package/microlens-contra microlens-contra>.

Lens users: you can convert a 'SimpleFold' into a @Fold@ by applying @folded . toListOf@ to it.
-}
type SimpleFold s a = forall r. Monoid r => Getting r s a

#if MIN_VERSION_base(4,12,0)
{- |
Like 'SimpleFold', but fully generalized – an exact copy of @<http://hackage.haskell.org/package/lens/docs/Control-Lens-Fold.html#t:Fold Fold>@ from lens.

Only available starting from GHC 8.6 because it needs 'Contravariant' in base.

@since 0.4.12
-}
type Fold s a = forall f. (Contravariant f, Applicative f) => (a -> f a) -> s -> f s
#endif

{- |
@Lens s t a b@ is the lowest common denominator of a setter and a getter, something that has the power of both; it has a 'Functor' constraint, and since both 'Const' and 'Identity' are functors, it can be used whenever a getter or a setter is needed.

  * @a@ is the type of the value inside of structure
  * @b@ is the type of the replaced value
  * @s@ is the type of the whole structure
  * @t@ is the type of the structure after replacing @a@ in it with @b@
-}
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

{- |
This is a type alias for monomorphic lenses which don't change the type of the container (or of the value inside).
-}
type Lens' s a = Lens s s a a

{- |
@Traversal s t a b@ is a generalisation of 'Lens' which allows many targets (possibly 0). It's achieved by changing the constraint to 'Applicative' instead of 'Functor' – indeed, the point of 'Applicative' is that you can combine effects, which is just what we need to have many targets.

Ultimately, traversals should follow 2 laws:

@
t pure ≡ pure
fmap (t f) . t g ≡ getCompose . t (Compose . fmap f . g)
@

The 1st law states that you can't change the shape of the structure or do anything funny with elements (traverse elements which aren't in the structure, create new elements out of thin air, etc.). The 2nd law states that you should be able to fuse 2 identical traversals into one. For a more detailed explanation of the laws, see <http://artyom.me/lens-over-tea-2#traversal-laws this blog post> (if you prefer rambling blog posts), or <https://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf The Essence Of The Iterator Pattern> (if you prefer papers).

Traversing any value twice is a violation of traversal laws. You can, however, traverse values in any order.
-}
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

{- |
This is a type alias for monomorphic traversals which don't change the type of the container (or of the values inside).
-}
type Traversal' s a = Traversal s s a a

{- |
'LensLike' is a type that is often used to make combinators as general as possible. For instance, take ('Lens.Micro.<<%~'), which only requires the passed lens to be able to work with the @(,) a@ functor (lenses and traversals can do that). The fully expanded type is as follows:

@
('Lens.Micro.<<%~') :: ((a -> (a, b)) -> s -> (a, t)) -> (a -> b) -> s -> (a, t)
@

With 'LensLike', the intent to use the @(,) a@ functor can be made a bit clearer:

@
('Lens.Micro.<<%~') :: LensLike ((,) a) s t a b -> (a -> b) -> s -> (a, t)
@
-}
type LensLike f s t a b = (a -> f b) -> s -> f t

{- |
A type alias for monomorphic 'LensLike's.
-}
type LensLike' f s a = LensLike f s s a a
