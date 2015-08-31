{-# LANGUAGE
CPP,
FlexibleContexts,
FlexibleInstances,
UndecidableInstances,
RankNTypes,
ScopedTypeVariables,
DefaultSignatures,
KindSignatures,
TypeFamilies,
MultiParamTypeClasses,
FunctionalDependencies,
Unsafe
  #-}


{- |
This module is needed to give other packages from the microlens family (like <http://hackage.haskell.org/package/microlens-ghc microlens-ghc>) access to functions and classes that don't need to be exported from "Lens.Micro" (because they just clutter the namespace). Also, okay, uh, e.g. 'traversed' is here because otherwise there'd be a dependency cycle.

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
  ( #. ),
  ( .# ),
  phantom,
  Each(..),
  Index,
  IxValue,
  Ixed(..),
  At(..),
  Field1(..),
  Field2(..),
  Field3(..),
  Field4(..),
  Field5(..),
  Cons(..),
  Snoc(..),
)
where


import Lens.Micro.Type

import Control.Applicative
import Data.Monoid
import Data.Foldable as F
import Data.Functor.Identity
import Data.Complex

#if __GLASGOW_HASKELL__ < 710
import Data.Traversable
#endif

#if __GLASGOW_HASKELL__ >= 708
import Data.Coerce
#else
import Unsafe.Coerce
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

noEffect :: Applicative (Const r) => Const r a
noEffect = phantom (pure ())
{-# INLINE noEffect #-}

------------------------------------------------------------------------------
-- Data.Profunctor.Unsafe
------------------------------------------------------------------------------

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

'each' :: 'Traversal' (a,a) (b,b) a b
'each' :: 'Traversal' (a,a,a) (b,b,b) a b
'each' :: 'Traversal' (a,a,a,a) (b,b,b,b) a b
'each' :: 'Traversal' (a,a,a,a,a) (b,b,b,b,b) a b

'each' :: ('RealFloat' a, 'RealFloat' b) => 'Traversal' ('Complex' a) ('Complex' b) a b
@

Additionally, you can use 'each' with types from <http://hackage.haskell.org/package/array array>, <http://hackage.haskell.org/package/bytestring bytestring>, and <http://hackage.haskell.org/package/containers containers> by using @Lens.Micro.GHC@ from <http://hackage.haskell.org/package/microlens-ghc microlens-ghc>, or with types from <http://hackage.haskell.org/package/vector vector>, <http://hackage.haskell.org/package/text text>, and <http://hackage.haskell.org/package/unordered-containers unordered-containers> by using @Lens.Micro.Platform@ from <http://hackage.haskell.org/package/microlens-platform microlens-platform>.
  -}
  each :: Traversal s t a b
  default each :: (Traversable g, s ~ g a, t ~ g b) => Traversal s t a b
  each = traverse

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

instance Each (Maybe a) (Maybe b) a b

type family Index (s :: *) :: *

type family IxValue (m :: *) :: *

type instance Index   (e -> a) = e
type instance IxValue (e -> a) = a
type instance Index   [a] = Int
type instance IxValue [a] = a

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

@
'ix' :: 'Int' -> 'Traversal'' [a] a

'ix' :: ('Eq' e) => e -> 'Traversal'' (e -> a) a
@

Additionally, you can use 'ix' with types from <http://hackage.haskell.org/package/array array>, <http://hackage.haskell.org/package/bytestring bytestring>, and <http://hackage.haskell.org/package/containers containers> by using @Lens.Micro.GHC@ from <http://hackage.haskell.org/package/microlens-ghc microlens-ghc>, or with types from <http://hackage.haskell.org/package/vector vector>, <http://hackage.haskell.org/package/text text>, and <http://hackage.haskell.org/package/unordered-containers unordered-containers> by using @Lens.Micro.Platform@ from <http://hackage.haskell.org/package/microlens-platform microlens-platform>.
  -}
  ix :: Index m -> Traversal' m (IxValue m)
  default ix :: (At m) => Index m -> Traversal' m (IxValue m)
  ix = ixAt
  {-# INLINE ix #-}

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

'at' doesn't work for arrays, because you can't delete an arbitrary element from an array.

If you want to modify an already existing value, you should use 'ix' instead because then you won't have to deal with 'Maybe' ('ix' is available for all types that have 'at').

'at' is often used with 'Lens.Micro.non'.

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

class Field1 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  {- |
Gives access to the 1st field of a tuple (up to 5-tuples).

Getting the 1st component:

>>> (1,2,3,4,5) ^. _1
1

Setting the 1st component:

>>> (1,2,3) & _1 .~ 10
(10,2,3)

Note that this lens is lazy, and can set fields even of 'undefined':

>>> set _1 10 undefined :: (Int, Int)
(10,*** Exception: Prelude.undefined

This is done to avoid violating a lens law stating that you can get back what you put:

>>> view _1 . set _1 10 $ (undefined :: (Int, Int))
10

The implementation (for 2-tuples) is:

@
'_1' f t = (,) '<$>' f    ('fst' t)
             '<*>' 'pure' ('snd' t)
@

or, alternatively,

@
'_1' f ~(a,b) = (\\a' -> (a',b)) '<$>' f a
@

(where @~@ means a <https://wiki.haskell.org/Lazy_pattern_match lazy pattern>).

'_2', '_3', '_4', and '_5' are also available (see below).
  -}
  _1 :: Lens s t a b

instance Field1 (a,b) (a',b) a a' where
  _1 k ~(a,b) = (\a' -> (a',b)) <$> k a
  {-# INLINE _1 #-}

instance Field1 (a,b,c) (a',b,c) a a' where
  _1 k ~(a,b,c) = (\a' -> (a',b,c)) <$> k a
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d) (a',b,c,d) a a' where
  _1 k ~(a,b,c,d) = (\a' -> (a',b,c,d)) <$> k a
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d,e) (a',b,c,d,e) a a' where
  _1 k ~(a,b,c,d,e) = (\a' -> (a',b,c,d,e)) <$> k a
  {-# INLINE _1 #-}

{-

instance Field1 (a,b,c,d,e,f) (a',b,c,d,e,f) a a' where
  _1 k ~(a,b,c,d,e,f) = (\a' -> (a',b,c,d,e,f)) <$> k a
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d,e,f,g) (a',b,c,d,e,f,g) a a' where
  _1 k ~(a,b,c,d,e,f,g) = (\a' -> (a',b,c,d,e,f,g)) <$> k a
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d,e,f,g,h) (a',b,c,d,e,f,g,h) a a' where
  _1 k ~(a,b,c,d,e,f,g,h) = (\a' -> (a',b,c,d,e,f,g,h)) <$> k a
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d,e,f,g,h,i) (a',b,c,d,e,f,g,h,i) a a' where
  _1 k ~(a,b,c,d,e,f,g,h,i) = (\a' -> (a',b,c,d,e,f,g,h,i)) <$> k a
  {-# INLINE _1 #-}

-}

class Field2 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _2 :: Lens s t a b

instance Field2 (a,b) (a,b') b b' where
  _2 k ~(a,b) = (\b' -> (a,b')) <$> k b
  {-# INLINE _2 #-}

instance Field2 (a,b,c) (a,b',c) b b' where
  _2 k ~(a,b,c) = (\b' -> (a,b',c)) <$> k b
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d) (a,b',c,d) b b' where
  _2 k ~(a,b,c,d) = (\b' -> (a,b',c,d)) <$> k b
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d,e) (a,b',c,d,e) b b' where
  _2 k ~(a,b,c,d,e) = (\b' -> (a,b',c,d,e)) <$> k b
  {-# INLINE _2 #-}

{-

instance Field2 (a,b,c,d,e,f) (a,b',c,d,e,f) b b' where
  _2 k ~(a,b,c,d,e,f) = (\b' -> (a,b',c,d,e,f)) <$> k b
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d,e,f,g) (a,b',c,d,e,f,g) b b' where
  _2 k ~(a,b,c,d,e,f,g) = (\b' -> (a,b',c,d,e,f,g)) <$> k b
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d,e,f,g,h) (a,b',c,d,e,f,g,h) b b' where
  _2 k ~(a,b,c,d,e,f,g,h) = (\b' -> (a,b',c,d,e,f,g,h)) <$> k b
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d,e,f,g,h,i) (a,b',c,d,e,f,g,h,i) b b' where
  _2 k ~(a,b,c,d,e,f,g,h,i) = (\b' -> (a,b',c,d,e,f,g,h,i)) <$> k b
  {-# INLINE _2 #-}

-}

class Field3 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _3 :: Lens s t a b

instance Field3 (a,b,c) (a,b,c') c c' where
  _3 k ~(a,b,c) = (\c' -> (a,b,c')) <$> k c
  {-# INLINE _3 #-}

instance Field3 (a,b,c,d) (a,b,c',d) c c' where
  _3 k ~(a,b,c,d) = (\c' -> (a,b,c',d)) <$> k c
  {-# INLINE _3 #-}

instance Field3 (a,b,c,d,e) (a,b,c',d,e) c c' where
  _3 k ~(a,b,c,d,e) = (\c' -> (a,b,c',d,e)) <$> k c
  {-# INLINE _3 #-}

{-

instance Field3 (a,b,c,d,e,f) (a,b,c',d,e,f) c c' where
  _3 k ~(a,b,c,d,e,f) = (\c' -> (a,b,c',d,e,f)) <$> k c
  {-# INLINE _3 #-}

instance Field3 (a,b,c,d,e,f,g) (a,b,c',d,e,f,g) c c' where
  _3 k ~(a,b,c,d,e,f,g) = (\c' -> (a,b,c',d,e,f,g)) <$> k c
  {-# INLINE _3 #-}

instance Field3 (a,b,c,d,e,f,g,h) (a,b,c',d,e,f,g,h) c c' where
  _3 k ~(a,b,c,d,e,f,g,h) = (\c' -> (a,b,c',d,e,f,g,h)) <$> k c
  {-# INLINE _3 #-}

instance Field3 (a,b,c,d,e,f,g,h,i) (a,b,c',d,e,f,g,h,i) c c' where
  _3 k ~(a,b,c,d,e,f,g,h,i) = (\c' -> (a,b,c',d,e,f,g,h,i)) <$> k c
  {-# INLINE _3 #-}

-}

class Field4 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _4 :: Lens s t a b

instance Field4 (a,b,c,d) (a,b,c,d') d d' where
  _4 k ~(a,b,c,d) = (\d' -> (a,b,c,d')) <$> k d
  {-# INLINE _4 #-}

instance Field4 (a,b,c,d,e) (a,b,c,d',e) d d' where
  _4 k ~(a,b,c,d,e) = (\d' -> (a,b,c,d',e)) <$> k d
  {-# INLINE _4 #-}

{-

instance Field4 (a,b,c,d,e,f) (a,b,c,d',e,f) d d' where
  _4 k ~(a,b,c,d,e,f) = (\d' -> (a,b,c,d',e,f)) <$> k d
  {-# INLINE _4 #-}

instance Field4 (a,b,c,d,e,f,g) (a,b,c,d',e,f,g) d d' where
  _4 k ~(a,b,c,d,e,f,g) = (\d' -> (a,b,c,d',e,f,g)) <$> k d
  {-# INLINE _4 #-}

instance Field4 (a,b,c,d,e,f,g,h) (a,b,c,d',e,f,g,h) d d' where
  _4 k ~(a,b,c,d,e,f,g,h) = (\d' -> (a,b,c,d',e,f,g,h)) <$> k d
  {-# INLINE _4 #-}

instance Field4 (a,b,c,d,e,f,g,h,i) (a,b,c,d',e,f,g,h,i) d d' where
  _4 k ~(a,b,c,d,e,f,g,h,i) = (\d' -> (a,b,c,d',e,f,g,h,i)) <$> k d
  {-# INLINE _4 #-}

-}

class Field5 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _5 :: Lens s t a b

instance Field5 (a,b,c,d,e) (a,b,c,d,e') e e' where
  _5 k ~(a,b,c,d,e) = (\e' -> (a,b,c,d,e')) <$> k e
  {-# INLINE _5 #-}

{-

instance Field5 (a,b,c,d,e,f) (a,b,c,d,e',f) e e' where
  _5 k ~(a,b,c,d,e,f) = (\e' -> (a,b,c,d,e',f)) <$> k e
  {-# INLINE _5 #-}

instance Field5 (a,b,c,d,e,f,g) (a,b,c,d,e',f,g) e e' where
  _5 k ~(a,b,c,d,e,f,g) = (\e' -> (a,b,c,d,e',f,g)) <$> k e
  {-# INLINE _5 #-}

instance Field5 (a,b,c,d,e,f,g,h) (a,b,c,d,e',f,g,h) e e' where
  _5 k ~(a,b,c,d,e,f,g,h) = (\e' -> (a,b,c,d,e',f,g,h)) <$> k e
  {-# INLINE _5 #-}

instance Field5 (a,b,c,d,e,f,g,h,i) (a,b,c,d,e',f,g,h,i) e e' where
  _5 k ~(a,b,c,d,e,f,g,h,i) = (\e' -> (a,b,c,d,e',f,g,h,i)) <$> k e
  {-# INLINE _5 #-}

-}

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
