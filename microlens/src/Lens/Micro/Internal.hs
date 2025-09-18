{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}

-- Note: this module is marked 'Unsafe' because it exports 'coerce', and Data.Coerce is marked 'Unsafe' in base. As per <https://github.com/ekmett/lens/issues/661>, this is an issue for 'lens' as well but they have opted for 'Trustworthy' instead.
{-# LANGUAGE Unsafe #-}

{- |
Module      :  Lens.Micro.Internal
Copyright   :  (C) 2013-2016 Edward Kmett, 2015-2016 Artyom Kazak, 2018 Monadfix
License     :  BSD-style (see the file LICENSE)

This module is needed to give other packages from the microlens family (like <http://hackage.haskell.org/package/microlens-ghc microlens-ghc>) access to functions and classes that don't need to be exported from "Lens.Micro" (because they just clutter the namespace). Also:

  * 'traversed' is here because otherwise there'd be a dependency cycle
  * 'sets' is here because it's used in RULEs

Classes like 'Each', 'Ixed', etc are provided for convenience – you're not supposed to export functions that work on all members of 'Ixed', for instance. Only microlens can do that. You mustn't declare instances of those classes for other types, either; these classes are incompatible with lens's classes, and by doing so you would divide the ecosystem.

If you absolutely need to define an instance (e.g. for internal use), only do it for your own types, because otherwise I might add an instance to one of the microlens packages later and if our instances are different it might lead to subtle bugs.
-}
module Lens.Micro.Internal
(
  traversed,
  folded,
  foldring,
  foldrOf,
  foldMapOf,
  sets,
  phantom,
  Each(..),
  Index,
  IxValue,
  Ixed(..),
  At(..),
  ixAt,
  Cons(..),
  Snoc(..),
  Strict(..),

  -- * CallStack
  HasCallStack,

  -- * Coerce compatibility shim
  coerce,

  -- * Coerce-like composition
  ( #. ),
  ( .# ),
)
where


import Lens.Micro.Type

import Control.Applicative
import Data.Monoid
import Data.Foldable as F
import Data.Functor.Identity
import Data.Complex

#if __GLASGOW_HASKELL__ >= 800
import Data.List.NonEmpty (NonEmpty(..))
#endif

#if __GLASGOW_HASKELL__ < 710
import Data.Traversable
#endif

#if __GLASGOW_HASKELL__ >= 708
import Data.Coerce
#else
import Unsafe.Coerce
#endif

-- We don't depend on the call-stack package because building an extra
-- package is likely slower than adding several lines of code here.
#if MIN_VERSION_base(4,9,0)
import Data.Kind (Type)
import GHC.Stack (HasCallStack)
#elif MIN_VERSION_base(4,8,1)
import qualified GHC.Stack as GHC
type HasCallStack = (?callStack :: GHC.CallStack)
#else
import GHC.Exts (Constraint)
type HasCallStack = (() :: Constraint)
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
folded :: Foldable f => SimpleFold (f a) a
folded = foldring F.foldr
{-# INLINE folded #-}

foldring :: Monoid r => ((a -> Const r a -> Const r a) -> Const r a -> s -> Const r a) -> (a -> Const r b) -> s -> Const r t
foldring fr f = phantom . fr (\a fa -> f a *> fa) noEffect
{-# INLINE foldring #-}

foldrOf :: Getting (Endo r) s a -> (a -> r -> r) -> r -> s -> r
foldrOf l f z = flip appEndo z . foldMapOf l (Endo #. f)
{-# INLINE foldrOf #-}

foldMapOf :: Getting r s a -> (a -> r) -> s -> r
foldMapOf l f = getConst #. l (Const #. f)
{-# INLINE foldMapOf #-}

{- |
'sets' creates an 'ASetter' from an ordinary function. (The only thing it does is wrapping and unwrapping 'Identity'.)
-}
sets :: ((a -> b) -> s -> t) -> ASetter s t a b
sets f g = Identity #. f (runIdentity #. g)
{-# INLINE sets #-}

------------------------------------------------------------------------------
-- Control.Lens.Internal.Getter
------------------------------------------------------------------------------

-- was renamed from “coerce”
phantom :: Const r a -> Const r b
phantom = Const #. getConst
{-# INLINE phantom #-}

noEffect :: Monoid r => Const r a
noEffect = phantom (pure ())
{-# INLINE noEffect #-}

------------------------------------------------------------------------------
-- classes
------------------------------------------------------------------------------

class Each s t a b | s -> a, t -> b, s b -> t, t a -> s where
  {- |
'each' tries to be a universal 'Traversal' – it behaves like 'traversed' in most situations, but also adds support for e.g. tuples with same-typed values:

>>> (1,2) & each %~ succ
(2,3)

>>> ["x", "y", "z"] ^. each
"xyz"

However, note that 'each' doesn't work on /every/ instance of 'Traversable'. If you have a 'Traversable' which isn't supported by 'each', you can use 'traversed' instead. Personally, I like using 'each' instead of 'traversed' whenever possible – it's shorter and more descriptive.

You can use 'each' with these things:

@
'each' :: 'Traversal' [a] [b] a b

'each' :: 'Traversal' ('Maybe' a) ('Maybe' b) a b
'each' :: 'Traversal' ('Either' a a) ('Either' b b) a b  -- since 0.4.11

'each' :: 'Traversal' (a,a) (b,b) a b
'each' :: 'Traversal' (a,a,a) (b,b,b) a b
'each' :: 'Traversal' (a,a,a,a) (b,b,b,b) a b
'each' :: 'Traversal' (a,a,a,a,a) (b,b,b,b,b) a b

'each' :: ('RealFloat' a, 'RealFloat' b) => 'Traversal' ('Complex' a) ('Complex' b) a b
@

You can also use 'each' with types from <http://hackage.haskell.org/package/array array>, <http://hackage.haskell.org/package/bytestring bytestring>, and <http://hackage.haskell.org/package/containers containers> by using <http://hackage.haskell.org/package/microlens-ghc microlens-ghc>, or additionally with types from <http://hackage.haskell.org/package/vector vector>, <http://hackage.haskell.org/package/text text>, and <http://hackage.haskell.org/package/unordered-containers unordered-containers> by using <http://hackage.haskell.org/package/microlens-platform microlens-platform>.
  -}
  each :: Traversal s t a b

instance (a~b, q~r) => Each (a,b) (q,r) a q where
  each f ~(a,b) = (,) <$> f a <*> f b
  {-# INLINE each #-}

instance (a~b, a~c, q~r, q~s) => Each (a,b,c) (q,r,s) a q where
  each f ~(a,b,c) = (,,) <$> f a <*> f b <*> f c
  {-# INLINE each #-}

instance (a~b, a~c, a~d, q~r, q~s, q~t) => Each (a,b,c,d) (q,r,s,t) a q where
  each f ~(a,b,c,d) = (,,,) <$> f a <*> f b <*> f c <*> f d
  {-# INLINE each #-}

instance (a~b, a~c, a~d, a~e, q~r, q~s, q~t, q~u) => Each (a,b,c,d,e) (q,r,s,t,u) a q where
  each f ~(a,b,c,d,e) = (,,,,) <$> f a <*> f b <*> f c <*> f d <*> f e
  {-# INLINE each #-}

instance Each (Complex a) (Complex b) a b where
  each f (a :+ b) = (:+) <$> f a <*> f b
  {-# INLINE each #-}

instance Each [a] [b] a b where
  each = traversed
  {-# INLINE each #-}

instance Each (Maybe a) (Maybe b) a b where
  each = traverse
  {-# INLINE each #-}

{- |
@since 0.4.11
-}
instance (a~a', b~b') => Each (Either a a') (Either b b') a b where
  each f (Left a)   = Left <$> f a
  each f (Right a ) = Right <$> f a
  {-# INLINE each #-}

#if __GLASGOW_HASKELL__ >= 800
instance Each (NonEmpty a) (NonEmpty b) a b where
  each = traversed
  {-# INLINE each #-}
#endif

-- NOTE: when adding new instances of 'Each', update the docs for 'each'.

#if MIN_VERSION_base(4,9,0)
type family Index (s :: Type) :: Type
type family IxValue (m :: Type) :: Type
#else
type family Index (s :: *) :: *
type family IxValue (m :: *) :: *
#endif

type instance Index   (e -> a) = e
type instance IxValue (e -> a) = a
type instance Index   [a] = Int
type instance IxValue [a] = a

#if __GLASGOW_HASKELL__ >= 800
type instance Index   (NonEmpty a) = Int
type instance IxValue (NonEmpty a) = a
#endif

class Ixed m where
  {- |
This traversal lets you access (and update) an arbitrary element in a list, array, @Map@, etc. (If you want to insert or delete elements as well, look at 'at'.)

An example for lists:

>>> [0..5] & ix 3 .~ 10
[0,1,2,10,4,5]

You can use it for getting, too:

>>> [0..5] ^? ix 3
Just 3

Of course, the element may not be present (which means that you can use 'ix' as a safe variant of ('!!')):

>>> [0..5] ^? ix 10
Nothing

Another useful instance is the one for functions – it lets you modify their outputs for specific inputs. For instance, here's 'maximum' that returns 0 when the list is empty (instead of throwing an exception):

@
maximum0 = 'maximum' 'Lens.Micro.&' 'ix' [] 'Lens.Micro..~' 0
@

The following instances are provided in this package:

#if __GLASGOW_HASKELL__ >= 800
@
'ix' :: 'Int' -> 'Traversal'' [a] a

'ix' :: 'Int' -> 'Traversal'' (NonEmpty a) a

'ix' :: ('Eq' e) => e -> 'Traversal'' (e -> a) a
@
#else
@
'ix' :: 'Int' -> 'Traversal'' [a] a

'ix' :: ('Eq' e) => e -> 'Traversal'' (e -> a) a
@
#endif

You can also use 'ix' with types from <http://hackage.haskell.org/package/array array>, <http://hackage.haskell.org/package/bytestring bytestring>, and <http://hackage.haskell.org/package/containers containers> by using <http://hackage.haskell.org/package/microlens-ghc microlens-ghc>, or additionally with types from <http://hackage.haskell.org/package/vector vector>, <http://hackage.haskell.org/package/text text>, and <http://hackage.haskell.org/package/unordered-containers unordered-containers> by using <http://hackage.haskell.org/package/microlens-platform microlens-platform>.
  -}
  ix :: Index m -> Traversal' m (IxValue m)

class Ixed m => At m where
  {- |
This lens lets you read, write, or delete elements in @Map@-like structures. It returns 'Nothing' when the value isn't found, just like @lookup@:

@
Data.Map.lookup k m = m 'Lens.Micro.^.' at k
@

However, it also lets you insert and delete values by setting the value to @'Just' value@ or 'Nothing':

@
Data.Map.insert k a m = m 'Lens.Micro.&' at k 'Lens.Micro..~' Just a

Data.Map.delete k m = m 'Lens.Micro.&' at k 'Lens.Micro..~' Nothing
@

Or you could use ('Lens.Micro.?~') instead of ('Lens.Micro..~'):

@
Data.Map.insert k a m = m 'Lens.Micro.&' at k 'Lens.Micro.?~' a
@

Note that 'at' doesn't work for arrays or lists. You can't delete an arbitrary element from an array (what would be left in its place?), and you can't set an arbitrary element in a list because if the index is out of list's bounds, you'd have to somehow fill the stretch between the last element and the element you just inserted (i.e. @[1,2,3] & at 10 .~ 5@ is undefined). If you want to modify an already existing value in an array or list, you should use 'ix' instead.

'at' is often used with 'Lens.Micro.non'. See the documentation of 'Lens.Micro.non' for examples.

Note that 'at' isn't strict for @Map@, even if you're using @Data.Map.Strict@:

>>> Data.Map.Strict.size (Data.Map.Strict.empty & at 1 .~ Just undefined)
1

The reason for such behavior is that there's actually no “strict @Map@” type; @Data.Map.Strict@ just provides some strict functions for ordinary @Map@s.

This package doesn't actually provide any instances for 'at', but there are instances for @Map@ and @IntMap@ in <http://hackage.haskell.org/package/microlens-ghc microlens-ghc> and an instance for @HashMap@ in <http://hackage.haskell.org/package/microlens-platform microlens-platform>.
  -}
  at :: Index m -> Lens' m (Maybe (IxValue m))

ixAt :: At m => Index m -> Traversal' m (IxValue m)
ixAt i = at i . traverse
{-# INLINE ixAt #-}

instance Eq e => Ixed (e -> a) where
  ix e p f = (\a e' -> if e == e' then a else f e') <$> p (f e)
  {-# INLINE ix #-}

instance Ixed [a] where
  ix k f xs0 | k < 0     = pure xs0
             | otherwise = go xs0 k where
    go [] _ = pure []
    go (a:as) 0 = (:as) <$> f a
    go (a:as) i = (a:) <$> (go as $! i - 1)
  {-# INLINE ix #-}

#if __GLASGOW_HASKELL__ >= 800
instance Ixed (NonEmpty a) where
  ix k f xs0 | k < 0 = pure xs0
             | otherwise = go xs0 k where
    go (a:|as) 0 = (:|as) <$> f a
    go (a:|as) i = (a:|) <$> ix (i - 1) f as
  {-# INLINE ix #-}
#endif

class Cons s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _Cons :: Traversal s t (a,s) (b,t)

instance Cons [a] [b] a b where
  _Cons f (a:as) = uncurry (:) <$> f (a, as)
  _Cons _ []     = pure []
  {-# INLINE _Cons #-}

class Snoc s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _Snoc :: Traversal s t (s,a) (t,b)

instance Snoc [a] [b] a b where
  _Snoc _ [] = pure []
  _Snoc f xs = (\(as,a) -> as ++ [a]) <$> f (init xs, last xs)
  {-# INLINE _Snoc #-}

class Strict lazy strict | lazy -> strict, strict -> lazy where
  {- |
'strict' lets you convert between strict and lazy versions of a datatype:

>>> let someText = "hello" :: Lazy.Text
>>> someText ^. strict
"hello" :: Strict.Text

It can also be useful if you have a function that works on a strict type but your type is lazy:

@
stripDiacritics :: Strict.Text -> Strict.Text
stripDiacritics = ...
@

>>> let someText = "Paul Erdős" :: Lazy.Text
>>> someText & strict %~ stripDiacritics
"Paul Erdos" :: Lazy.Text

'strict' works on @ByteString@ and @StateT@\/@WriterT@\/@RWST@ if you use <http://hackage.haskell.org/package/microlens-ghc microlens-ghc>, and additionally on @Text@ if you use <http://hackage.haskell.org/package/microlens-platform microlens-platform>.
  -}
  strict :: Lens' lazy strict

  {- |
'lazy' is like 'strict' but works in opposite direction:

>>> let someText = "hello" :: Strict.Text
>>> someText ^. lazy
"hello" :: Lazy.Text
  -}
  lazy   :: Lens' strict lazy

----------------------------------------------------------------------------
-- Coerce compatibility shim
----------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ < 708
coerce :: a -> b
coerce = unsafeCoerce
{-# INLINE coerce #-}
#endif

----------------------------------------------------------------------------
-- Coerce-like composition
----------------------------------------------------------------------------

-- Note: 'lens' defines a type-restricted version of (#.) to work around a
-- bug, but our version is restricted enough that we don't need it. See
-- <https://github.com/ekmett/lens/commit/cde2fc39c0dba413d1a6f814b47bd14431a5e339>

#if __GLASGOW_HASKELL__ >= 708
( #. ) :: Coercible c b => (b -> c) -> (a -> b) -> (a -> c)
( #. ) _ = coerce (\x -> x :: b) :: forall a b. Coercible b a => a -> b

( .# ) :: Coercible b a => (b -> c) -> (a -> b) -> (a -> c)
( .# ) pbc _ = coerce pbc
#else
( #. ) :: (b -> c) -> (a -> b) -> (a -> c)
( #. ) _ = unsafeCoerce

( .# ) :: (b -> c) -> (a -> b) -> (a -> c)
( .# ) pbc _ = unsafeCoerce pbc
#endif

{-# INLINE ( #. ) #-}
{-# INLINE ( .# ) #-}

infixr 9 #.
infixl 8 .#
