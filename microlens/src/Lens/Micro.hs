{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Trustworthy #-}


{- |
Module      :  Lens.Micro
Copyright   :  (C) 2013-2016 Edward Kmett, 2015-2016 Artyom Kazak, 2018 Monadfix
License     :  BSD-style (see the file LICENSE)

This module provides the essential functionality. There are other packages in the microlens family – mix and match them at will. If you're writing an app, you want <http://hackage.haskell.org/package/microlens-platform microlens-platform> – it provides the most functionality.

* <http://hackage.haskell.org/package/microlens-mtl microlens-mtl> – (@+=@) and friends, @use@, @zoom@\/@magnify@
* <http://hackage.haskell.org/package/microlens-th microlens-th> – @makeLenses@ and @makeFields@
* <http://hackage.haskell.org/package/microlens-ghc microlens-ghc> – everything in microlens + instances to make @each@\/@at@\/@ix@ usable with arrays, @ByteString@, and containers
* <http://hackage.haskell.org/package/microlens-platform microlens-platform> – microlens-ghc + microlens-mtl + microlens-th + instances for @Text@, @Vector@, and @HashMap@
* <http://hackage.haskell.org/package/microlens-contra microlens-contra> – @Fold@ and @Getter@ that are exact copies of types in lens

Unofficial:

* <http://hackage.haskell.org/package/microlens-aeson microlens-aeson> – a port of <http://hackage.haskell.org/package/lens-aeson lens-aeson>

-}
module Lens.Micro
(
  (&),
  -- $ampersand-note
  (<&>),
  -- $reverse-fmap-note

  -- * Setter: modifies something in a structure
  -- $setters-note
  ASetter, ASetter',
  sets,
  (%~), over, (+~), (-~),
  (<>~),
  (.~), set,
  (?~),
  (<%~), (<<%~), (<<.~),
  mapped,

  -- * Getter: extracts a value from a structure
  -- $getters-note
  SimpleGetter,
  Getting,
  (^.),
  to,

  -- * Fold: extracts multiple elements
  -- $folds-note
  SimpleFold,
  (^..), toListOf,
  (^?),
  (^?!),
  traverseOf_,
  forOf_,
  has,
  folded,
  folding,

  -- * Lens: a combined getter-and-setter
  -- $lenses-note
  Lens, Lens',
  lens,
  at,
  _1, _2, _3, _4, _5,

  -- * Iso: a lens that only changes the representation
  -- $isos-note
  strict, lazy,
  non,

  -- * Traversal: a lens iterating over several elements
  -- $traversals-note
  Traversal, Traversal',
  traverseOf,
  forOf,
  singular,
  failing,
  filtered,
  both,
  traversed,
  each,
  ix,
  _head, _tail, _init, _last,
  mapAccumLOf,

  -- * Prism: a traversal iterating over at most 1 element
  -- $prisms-note
  _Left, _Right,
  _Just, _Nothing,

  -- * Other types
  LensLike, LensLike',
)
where


import Lens.Micro.Type
import Lens.Micro.Internal

import Control.Applicative
import Control.Monad
import Data.Functor.Identity
import Data.Monoid
import Data.Maybe
import Data.Tuple
import qualified Data.Foldable as F
import Unsafe.Coerce

#if MIN_VERSION_base(4,8,0)
import Data.Function ((&))
#endif

#if MIN_VERSION_base(4,11,0)
import Data.Functor ((<&>))
#endif

-- This is for the reimplementation of State
#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail as Fail
#endif

{- $setup
-- >>> import Data.Char (toUpper)
-- >>> import Control.Arrow (first, second, left, right)
-}


#if !(MIN_VERSION_base(4,8,0))
{- |
'&' is a reverse application operator. This provides notational convenience. Its precedence is one higher than that of the forward application operator '$', which allows '&' to be nested in '$'.
-}
(&) :: a -> (a -> b) -> b
a & f = f a
{-# INLINE (&) #-}
infixl 1 &
#endif

{- $ampersand-note

This operator is useful when you want to modify something several times. For instance, if you want to change 1st and 3rd elements of a tuple, you can write this:

@
(1,2,3) '&' '_1' '.~' 0
        '&' '_3' '%~' 'negate'
@

instead of e.g. this:

@
('_1' '.~' 0) '.' ('_3' '%~' 'negate') '$' (1,2,3)
@

or this:

@
'set' '_1' 0 '.'
'over' '_3' 'negate'
  '$' (1,2,3)
@
-}

#if !(MIN_VERSION_base(4,11,0))
{- |
Flipped version of '<$>'.
-}
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) x f = f <$> x
{-# INLINE (<&>) #-}

infixl 1 <&>
#endif

{- $reverse-fmap-note

('<&>') is flipped ('<$>'):

@
x '<&>' f = f '<$>' x
@

It's often useful when writing lenses. For instance, let's say you're writing 'ix' for @Map@; if the key is found in the map, you have to apply a function to it and then change the map based on the new value – which requires a lambda, like this:

@
'ix' key f map = case Map.lookup key map of
     Just val -> (\\val' -> Map.insert key val' map) '<$>' f val
     Nothing  -> 'pure' map
@

With ('<&>') you can get rid of parentheses and move the long lambda expression to the right of the value (like when you use '>>='):

@
'ix' key f map = case Map.lookup key map of
     Just val -> f val '<&>' \\val' -> Map.insert key val' map
     Nothing  -> 'pure' map
@
-}

-- Setting -----------------------------------------------------------------

{- $setters-note

A setter is, broadly speaking, something that lets you modify a part of some value. Most likely you already know some setters:

  * @'Control.Arrow.first' :: (a -> b) -> (a, x) -> (b, x)@

      (modifies 1st element of a pair; corresponds to 'Lens.Micro._1')

  * @'Control.Arrow.left' :: (a -> b) -> 'Either' a x -> 'Either' b x@

      (modifies left branch of 'Either'; corresponds to 'Lens.Micro._Left')

  * @'map' :: (a -> b) -> [a] -> [b]@

      (modifies every element in a list; corresponds to 'Lens.Micro.mapped')

As you see, a setter takes a function, a value, and applies the function to some part (or several parts) of the value. Moreover, setters can be pretty specific – for instance, a function that modifies the 3rd element of a list is a setter too:

@
-- Modify 3rd element in a list, if present.
modify3rd :: (a -> a) -> [a] -> [a]
modify3rd f (a:b:c:xs) = a : b : f c : xs
modify3rd _ xs         = xs
@

A nice thing about setters is that they compose easily – you can write @'map' '.' 'Control.Arrow.left'@ and it would be a function that takes a list of 'Either's and modifies all of them that are 'Left's.

This library provides its own type for setters – 'ASetter'; it's needed so that some functions in this library (like '_1') would be usable both as setters and as getters. You can turn an ordinary function like 'map' to a “lensy” setter with 'sets'.

To apply a setter to a value, use ('%~') or 'over':

>>> [1,2,3] & mapped %~ succ
[2,3,4]
>>> over _head toUpper "jane"
"Jane"

To modify a value deeper inside the structure, use ('.'):

>>> ["abc","def","ghi"] & ix 1 . ix 2 %~ toUpper
["abc","deF","ghi"]

To set a value instead of modifying it, use 'set' or ('.~'):

>>> "abc" & mapped .~ 'x'
"xxx"
>>> set _2 'X' ('a','b','c')
('a','X','c')

It's also possible to get both the old and the new value back – see ('<%~') and ('<<%~').
-}

{- |
('%~') applies a function to the target; an alternative explanation is that it is an inverse of 'sets', which turns a setter into an ordinary function. @'mapped' '%~' 'reverse'@ is the same thing as @'fmap' 'reverse'@.

See 'over' if you want a non-operator synonym.

Negating the 1st element of a pair:

>>> (1,2) & _1 %~ negate
(-1,2)

Turning all @Left@s in a list to upper case:

>>> (mapped._Left.mapped %~ toUpper) [Left "foo", Right "bar"]
[Left "FOO",Right "bar"]
-}
(%~) :: ASetter s t a b -> (a -> b) -> s -> t
(%~) = over
{-# INLINE (%~) #-}

infixr 4 %~

{- |
'over' is a synonym for ('%~').

Getting 'fmap' in a roundabout way:

@
'over' 'mapped' :: 'Functor' f => (a -> b) -> f a -> f b
'over' 'mapped' = 'fmap'
@

Applying a function to both components of a pair:

@
'over' 'both' :: (a -> b) -> (a, a) -> (b, b)
'over' 'both' = \\f t -> (f (fst t), f (snd t))
@

Using @'over' '_2'@ as a replacement for 'Control.Arrow.second':

>>> over _2 show (10,20)
(10,"20")
-}
over :: ASetter s t a b -> (a -> b) -> s -> t
over l f = runIdentity #. l (Identity #. f)
{-# INLINE over #-}


-- | Increment the target(s) of a numerically valued 'Lens' or 'Traversal'.
--
-- >>> (a,b) & _1 +~ c
-- (a + c,b)
--
-- >>> (a,b) & both +~ c
-- (a + c,b + c)
--
-- >>> (1,2) & _2 +~ 1
-- (1,3)
--
-- >>> [(a,b),(c,d)] & traverse.both +~ e
-- [(a + e,b + e),(c + e,d + e)]
--
-- @
-- ('+~') :: 'Num' a => 'Lens'' s a      -> a -> s -> s
-- ('+~') :: 'Num' a => 'Traversal'' s a -> a -> s -> s
-- @
(+~) :: Num a => ASetter s t a a -> a -> s -> t
l +~ n = over l (+ n)
{-# INLINE (+~) #-}

infixr 4 +~

-- | Decrement the target(s) of a numerically valued 'Lens', or 'Traversal'.
--
-- >>> (a,b) & _1 -~ c
-- (a - c,b)
--
-- >>> (a,b) & both -~ c
-- (a - c,b - c)
--
-- >>> _1 -~ 2 $ (1,2)
-- (-1,2)
--
-- >>> mapped.mapped -~ 1 $ [[4,5],[6,7]]
-- [[3,4],[5,6]]
--
-- @
-- ('-~') :: 'Num' a => 'Lens'' s a      -> a -> s -> s
-- ('-~') :: 'Num' a => 'Traversal'' s a -> a -> s -> s
-- @
(-~) :: Num a => ASetter s t a a -> a -> s -> t
l -~ n = over l (subtract n)
{-# INLINE (-~) #-}

infixr 4 -~

{- |
('<>~') appends a value monoidally to the target.

>>> ("hello", "goodbye") & both <>~ " world!"
("hello world!", "goodbye world!")
-}
(<>~) :: (Monoid a) => ASetter s t a a -> a -> s -> t
(<>~) l a = over l (`mappend` a)
{-# INLINE (<>~) #-}

infixr 4 <>~

{- |
('.~') assigns a value to the target. It's the same thing as using ('%~') with 'const':

@
l '.~' x = l '%~' 'const' x
@

See 'set' if you want a non-operator synonym.

Here it is used to change 2 fields of a 3-tuple:

>>> (0,0,0) & _1 .~ 1 & _3 .~ 3
(1,0,3)
-}
(.~) :: ASetter s t a b -> b -> s -> t
(.~) = set
{-# INLINE (.~) #-}

infixr 4 .~

{- |
'set' is a synonym for ('.~').

Setting the 1st component of a pair:

@
'set' '_1' :: x -> (a, b) -> (x, b)
'set' '_1' = \\x t -> (x, snd t)
@

Using it to rewrite ('Data.Functor.<$'):

@
'set' 'mapped' :: 'Functor' f => a -> f b -> f a
'set' 'mapped' = ('Data.Functor.<$')
@
-}
set :: ASetter s t a b -> b -> s -> t
set l b = runIdentity #. l (\_ -> Identity b)
{-# INLINE set #-}

{- |
('?~') is a version of ('.~') that wraps the value into 'Just' before setting.

@
l ?~ b = l .~ Just b
@

It can be useful in combination with 'at':

>>> Map.empty & at 3 ?~ x
fromList [(3,x)]
-}
(?~) :: ASetter s t a (Maybe b) -> b -> s -> t
l ?~ b = set l (Just b)
{-# INLINE (?~) #-}

infixr 4 ?~

{- |
'mapped' is a setter for everything contained in a functor. You can use it to map over lists, @Maybe@, or even @IO@ (which is something you can't do with 'traversed' or 'each').

Here 'mapped' is used to turn a value to all non-'Nothing' values in a list:

>>> [Just 3,Nothing,Just 5] & mapped.mapped .~ 0
[Just 0,Nothing,Just 0]

Keep in mind that while 'mapped' is a more powerful setter than 'each', it can't be used as a getter! This won't work (and will fail with a type error):

@
[(1,2),(3,4),(5,6)] '^..' 'mapped' . 'both'
@
-}
mapped :: Functor f => ASetter (f a) (f b) a b
mapped = sets fmap
{-# INLINE mapped #-}

{- |
This is a version of ('%~') which modifies the structure and returns it along with the new value:

>>> (1, 2) & _1 <%~ negate
(-1, (-1, 2))

Simpler type signatures:

@
('<%~') ::             'Lens' s t a b      -> (a -> b) -> s -> (b, t)
('<%~') :: 'Monoid' b => 'Traversal' s t a b -> (a -> b) -> s -> (b, t)
@

Since it does getting in addition to setting, you can't use it with 'ASetter' (but you can use it with lens and traversals).
-}
(<%~) :: LensLike ((,) b) s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f = l (join (,) . f)
{-# INLINE (<%~) #-}

infixr 4 <%~

{- |
This is a version of ('%~') which modifies the structure and returns it along with the old value:

>>> (1, 2) & _1 <<%~ negate
(1, (-1, 2))

Simpler type signatures:

@
('<<%~') ::             'Lens' s t a b      -> (a -> b) -> s -> (a, t)
('<<%~') :: 'Monoid' a => 'Traversal' s t a b -> (a -> b) -> s -> (a, t)
@
-}
(<<%~) :: LensLike ((,) a) s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f = l (\a -> (a, f a))
{-# INLINE (<<%~) #-}

infixr 4 <<%~

{- |
This is a version of ('.~') which modifies the structure and returns it along with the old value:

>>> (1, 2) & _1 <<.~ 0
(1, (0, 2))

Simpler type signatures:

@
('<<.~') ::             'Lens' s t a b      -> b -> s -> (a, t)
('<<.~') :: 'Monoid' a => 'Traversal' s t a b -> b -> s -> (a, t)
@
-}
(<<.~) :: LensLike ((,) a) s t a b -> b -> s -> (a, t)
(<<.~) l x = l (\a -> (a, x))
{-# INLINE (<<.~) #-}

infixr 4 <<.~

-- Getting -----------------------------------------------------------------

{- $getters-note

A getter extracts something from a value; in fact, any function is a getter. However, same as with setters, this library uses a special type for getters so that functions like '_1' would be usable both as a setter and a getter. An ordinary function can be turned into a getter with 'to'.

Using a getter is done with ('^.') or 'Lens.Micro.Extras.view' from "Lens.Micro.Extras":

>>> ('x','y') ^. _1
'x'
>>> view (ix 2) [0..5]
2

Getters can be composed with ('.'):

>>> [(1,2),(3,4),(5,6)] ^. ix 1 . _2
4

A getter always returns exactly 1 element (getters that can return more than one element are called folds and are present in this library as well).
-}

{- |
('^.') applies a getter to a value; in other words, it gets a value out of a structure using a getter (which can be a lens, traversal, fold, etc.).

Getting 1st field of a tuple:

@
('^.' '_1') :: (a, b) -> a
('^.' '_1') = 'fst'
@

When ('^.') is used with a traversal, it combines all results using the 'Monoid' instance for the resulting type. For instance, for lists it would be simple concatenation:

>>> ("str","ing") ^. each
"string"

The reason for this is that traversals use 'Applicative', and the 'Applicative' instance for 'Const' uses monoid concatenation to combine “effects” of 'Const'.

A non-operator version of ('^.') is called @view@, and it's a bit more general than ('^.') (it works in @MonadReader@). If you need the general version, you can get it from <http://hackage.haskell.org/package/microlens-mtl microlens-mtl>; otherwise there's 'Lens.Micro.Extras.view' available in "Lens.Micro.Extras".
-}
(^.) :: s -> Getting a s a -> a
s ^. l = getConst (l Const s)
{-# INLINE (^.) #-}

infixl 8 ^.

{- |
'to' creates a getter from any function:

@
a '^.' 'to' f = f a
@

It's most useful in chains, because it lets you mix lenses and ordinary functions. Suppose you have a record which comes from some third-party library and doesn't have any lens accessors. You want to do something like this:

@
value ^. _1 . field . at 2
@

However, @field@ isn't a getter, and you have to do this instead:

@
field (value ^. _1) ^. at 2
@

but now @value@ is in the middle and it's hard to read the resulting code. A variant with 'to' is prettier and more readable:

@
value ^. _1 . to field . at 2
@
-}
to :: (s -> a) -> SimpleGetter s a
to k f = phantom . f . k
{-# INLINE to #-}

-- Folds -------------------------------------------------------------------

{- $folds-note

Folds are getters that can return more than one element (or no elements at all). <http://comonad.com/reader/2015/free-monoids-in-haskell/ Except for some rare cases>, a fold is the same thing as @(s -> [a])@; you can use 'folding' to turn any function of type @(s -> f a)@ (where @f@ is 'F.Foldable') into a fold.

Folds can be applied to values by using operators like ('^..'), ('^?'), etc:

>>> (1,2) ^.. both
[1,2]

A nice thing about folds is that you can combine them with ('Data.Monoid.<>') to concatenate their outputs:

>>> (1,2,3) ^.. (_2 <> _1)
[2,1]

When you need to get all elements of the same type in a complicated structure, ('Data.Monoid.<>') can be more helpful than 'each':

>>> ([1,2], 3, [Nothing, Just 4]) ^.. (_1.each <> _2 <> _3.each._Just)
[1,2,3,4]

(Just like setters and getters before, folds can be composed with ('.').)

The ('Data.Monoid.<>') trick works nicely with ('^?'), too. For instance, if you want to get the 9th element of the list, but would be fine with 5th too if the list is too short, you could combine @ix 9@ and @ix 5@:

>>> [0..9] ^? (ix 9 <> ix 5)
Just 9
>>> [0..8] ^? (ix 9 <> ix 5)
Just 5
>>> [0..3] ^? (ix 9 <> ix 5)
Nothing

(Unfortunately, this trick won't help you with setting or modifying.)
-}

{- |
@s ^.. t@ returns the list of all values that @t@ gets from @s@.

A 'Maybe' contains either 0 or 1 values:

>>> Just 3 ^.. _Just
[3]

Gathering all values in a list of tuples:

>>> [(1,2),(3,4)] ^.. each.each
[1,2,3,4]
-}
(^..) :: s -> Getting (Endo [a]) s a -> [a]
s ^.. l = toListOf l s
{-# INLINE (^..) #-}

infixl 8 ^..

{- |
'toListOf' is a synonym for ('^..').
-}
toListOf :: Getting (Endo [a]) s a -> s -> [a]
toListOf l = foldrOf l (:) []
{-# INLINE toListOf #-}

{- |
@s ^? t@ returns the 1st element @t@ returns, or 'Nothing' if @t@ doesn't return anything. It's trivially implemented by passing the 'First' monoid to the getter.

Safe 'head':

>>> [] ^? each
Nothing

>>> [1..3] ^? each
Just 1

Converting 'Either' to 'Maybe':

>>> Left 1 ^? _Right
Nothing

>>> Right 1 ^? _Right
Just 1

A non-operator version of ('^?') is called @preview@, and – like @view@ – it's a bit more general than ('^?') (it works in @MonadReader@). If you need the general version, you can get it from <http://hackage.haskell.org/package/microlens-mtl microlens-mtl>; otherwise there's 'Lens.Micro.Extras.preview' available in "Lens.Micro.Extras".
-}
(^?) :: s -> Getting (First a) s a -> Maybe a
s ^? l = getFirst (foldMapOf l (First #. Just) s)
{-# INLINE (^?) #-}

infixl 8 ^?

{- |
('^?!') is an unsafe variant of ('^?') – instead of using 'Nothing' to indicate that there were no elements returned, it throws an exception.
-}
(^?!) :: HasCallStack => s -> Getting (Endo a) s a -> a
s ^?! l = foldrOf l const (error "(^?!): empty Fold") s
{-# INLINE (^?!) #-}

infixl 8 ^?!

{- |
Apply an action to all targets and discard the result (like 'Control.Monad.mapM_' or 'Data.Foldable.traverse_'):

>>> traverseOf_ both putStrLn ("hello", "world")
hello
world

Works with anything that allows getting, including lenses and getters (so, anything except for 'ASetter'). Should be faster than 'traverseOf' when you don't need the result.
-}
traverseOf_
  :: Functor f
  => Getting (Traversed r f) s a -> (a -> f r) -> s -> f ()
traverseOf_ l f = void . getTraversed #. foldMapOf l (Traversed #. f)
{-# INLINE traverseOf_ #-}

{- |
'traverseOf_' with flipped arguments. Useful if the “loop body” is a lambda
or a @do@ block, or in some other cases – for instance, you can avoid
accidentally using 'Data.Foldable.for_' on a tuple or 'Either' by switching
to @'forOf_' 'each'@. Or you can write custom loops like these:

@
'forOf_' 'both' (a, b) $ \\x -\>
  ...
'forOf_' 'each' [1..10] $ \\x -\>
  ...
'forOf_' ('each' . '_Right') $ \\x -\>
  ...
@
-}
forOf_
  :: Functor f
  => Getting (Traversed r f) s a -> s -> (a -> f r) -> f ()
forOf_ = flip . traverseOf_
{-# INLINE forOf_ #-}

{- |
'has' checks whether a getter (any getter, including lenses, traversals, and folds) returns at least 1 value.

Checking whether a list is non-empty:

>>> has each []
False

You can also use it with e.g. '_Left' (and other 0-or-1 traversals) as a replacement for 'Data.Maybe.isNothing', 'Data.Maybe.isJust' and other @isConstructorName@ functions:

>>> has _Left (Left 1)
True
-}
has :: Getting Any s a -> s -> Bool
has l = getAny #. foldMapOf l (\_ -> Any True)
{-# INLINE has #-}

{- |
'folding' creates a fold out of any function that returns a 'F.Foldable' container (for instance, a list):

>>> [1..5] ^.. folding tail
[2,3,4,5]
-}
folding :: F.Foldable f => (s -> f a) -> SimpleFold s a
folding sfa agb = phantom . F.traverse_ agb . sfa
{-# INLINE folding #-}

-- Lenses ------------------------------------------------------------------

{- $lenses-note

Lenses are composable “pointers” at values inside some bigger structure (e.g. '_1' points at the first element of a tuple). You can use ('^.') to get, ('.~') to set, and ('%~') to modify:

>>> (1,2) ^. _1
1
>>> (1,2) & _1 .~ 3
(3,2)
>>> (1,2) & _1 %~ negate
(-1,2)

To apply a monadic action (or an 'Applicative' action, or even a 'Functor' action) to the pointed value, just apply the lens directly or use 'traverseOf' (or 'traverseOf_' if you don't need the result):

>>> traverseOf_ _1 print (1,2)
1

>>> _1 id (Just 1, 2)
Just (1, 2)
>>> _1 id (Nothing, 2)
Nothing

A 'Lens' can only point at a single value inside a structure (unlike a 'Traversal').

('.') composes lenses (i.e. if a @B@ is a part of @A@, and a @C@ is a part of @B@, then @b.c@ lets you operate on @C@ inside @A@). You can create lenses with 'lens', or you can write them by hand.

There are several ways to get lenses for some datatype:

* They can already be provided by the package, by @microlens@, or by some other package like <http://hackage.haskell.org/package/microlens-platform microlens-platform>.

* They can be provided by some unofficial package (like <http://hackage.haskell.org/package/microlens-aeson microlens-aeson>).

* You can get them by combining already existing lenses.

* You can derive them with Template Haskell (with <http://hackage.haskell.org/package/microlens-th microlens-th>).

* You can write them with 'lens' if you have a setter and a getter. It's a simple and good way.

* You can write them manually (sometimes it looks a bit better than the variant with 'lens', sometimes worse). The generic template is as follows:

@
somelens :: Lens s t a b

-- “f” is the “a -> f b” function, “s” is the structure.
somelens f s =
  let
    a = ...                 -- Extract the value from “s”.
    rebuildWith b = ...     -- Write a function which would
                            -- combine “s” and modified value
                            -- to produce new structure.
  in
    rebuildWith '<$>' f a     -- Apply the structure-producing
                            -- function to the modified value.
@

Here's the '_1' lens, for instance:

@
'_1' :: 'Lens' (a, x) (b, x) a b
'_1' f (a, x) = (\\b -> (b, x)) '<$>' f a
@

Here's a more complicated lens, which extracts /several/ values from a structure (in a tuple):

@
type Age     = Int
type City    = String
type Country = String

data Person = Person Age City Country

-- This lens lets you access all location-related information about a person.
location :: 'Lens'' Person (City, Country)
location f (Person age city country) =
  (\\(city', country') -> Person age city' country') '<$>' f (city, country)
@

You even can choose to use a lens to present /all/ information contained in the structure (in a different way). Such lenses are called @<http://hackage.haskell.org/package/lens/docs/Control-Lens-Iso.html#t:Iso Iso>@ in lens's terminology. For instance (assuming you don't mind functions that can error out), here's a lens which lets you act on the string representation of a value:

@
string :: (Read a, Show a) => 'Lens'' a String
string f s = read '<$>' f (show s)
@

Using it to reverse a number:

@
>>> 123 '&' string '%~' reverse
321
@
-}

{- |
'lens' creates a 'Lens' from a getter and a setter. The resulting lens isn't the most effective one (because of having to traverse the structure twice when modifying), but it shouldn't matter much.

A (partial) lens for list indexing:

@
ix :: Int -> 'Lens'' [a] a
ix i = 'lens' ('!!' i)                                   -- getter
            (\\s b -> take i s ++ b : drop (i+1) s)   -- setter
@

Usage:

@
>>> [1..9] '^.' ix 3
4

>>> [1..9] & ix 3 '%~' negate
[1,2,3,-4,5,6,7,8,9]
@

When getting, the setter is completely unused; when setting, the getter is unused. Both are used only when the value is being modified. For instance, here we define a lens for the 1st element of a list, but instead of a legitimate getter we use 'undefined'. Then we use the resulting lens for /setting/ and it works, which proves that the getter wasn't used:

>>> [1,2,3] & lens undefined (\s b -> b : tail s) .~ 10
[10,2,3]
-}
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s <$> afb (sa s)
{-# INLINE lens #-}

-- Isos --------------------------------------------------------------------

{- $isos-note

Isos (or isomorphisms) are lenses that convert a value instead of targeting a part of it; in other words, inside of every list lives a reversed list, inside of every strict @Text@ lives a lazy @Text@, and inside of every @(a, b)@ lives a @(b, a)@. Since an isomorphism doesn't lose any information, it's possible to /reverse/ it and use it in the opposite direction by using @from@ from the lens library:

@
from :: Iso' s a -> Iso' a s
@

However, it's not possible for microlens to export isomorphisms, because their type depends on @<http://hackage.haskell.org/package/profunctors/docs/Data-Profunctor.html#t:Profunctor Profunctor>@, which resides in the <http://hackage.haskell.org/package/profunctors profunctors> library, which is a somewhat huge dependency. So, all isomorphisms included here are lenses instead (and thus you can't use them in the opposite direction).
-}

{- |
'non' lets you “relabel” a 'Maybe' by equating 'Nothing' to an arbitrary value (which you can choose):

>>> Just 1 ^. non 0
1

>>> Nothing ^. non 0
0

The most useful thing about 'non' is that relabeling also works in other direction. If you try to 'set' the “forbidden” value, it'll be turned to 'Nothing':

>>> Just 1 & non 0 .~ 0
Nothing

Setting anything else works just fine:

>>> Just 1 & non 0 .~ 5
Just 5

Same happens if you try to modify a value:

>>> Just 1 & non 0 %~ subtract 1
Nothing

>>> Just 1 & non 0 %~ (+ 1)
Just 2

'non' is often useful when combined with 'at'. For instance, if you have a map of songs and their playcounts, it makes sense not to store songs with 0 plays in the map; 'non' can act as a filter that wouldn't pass such entries.

Decrease playcount of a song to 0, and it'll be gone:

>>> fromList [("Soon",1),("Yesterday",3)] & at "Soon" . non 0 %~ subtract 1
fromList [("Yesterday",3)]

Try to add a song with 0 plays, and it won't be added:

>>> fromList [("Yesterday",3)] & at "Soon" . non 0 .~ 0
fromList [("Yesterday",3)]

But it will be added if you set any other number:

>>> fromList [("Yesterday",3)] & at "Soon" . non 0 .~ 1
fromList [("Soon",1),("Yesterday",3)]

'non' is also useful when working with nested maps. Here a nested map is created when it's missing:

>>> Map.empty & at "Dez Mona" . non Map.empty . at "Soon" .~ Just 1
fromList [("Dez Mona",fromList [("Soon",1)])]

and here it is deleted when its last entry is deleted (notice that 'non' is used twice here):

>>> fromList [("Dez Mona",fromList [("Soon",1)])] & at "Dez Mona" . non Map.empty . at "Soon" . non 0 %~ subtract 1
fromList []

To understand the last example better, observe the flow of values in it:

* the map goes into @at \"Dez Mona\"@
* the nested map (wrapped into @Just@) goes into @non Map.empty@
* @Just@ is unwrapped and the nested map goes into @at \"Soon\"@
* @Just 1@ is unwrapped by @non 0@

Then the final value – i.e. 1 – is modified by @subtract 1@ and the result (which is 0) starts flowing backwards:

* @non 0@ sees the 0 and produces a @Nothing@
* @at \"Soon\"@ sees @Nothing@ and deletes the corresponding value from the map
* the resulting empty map is passed to @non Map.empty@, which sees that it's empty and thus produces @Nothing@
* @at \"Dez Mona\"@ sees @Nothing@ and removes the key from the map
-}
non :: Eq a => a -> Lens' (Maybe a) a
non x afb s = f <$> afb (fromMaybe x s)
  where f y = if x == y then Nothing else Just y
{-# INLINE non #-}

-- Traversals --------------------------------------------------------------

{- $traversals-note

Traversals are like lenses but they can point at multiple values. Use ('^..') to get all values, ('^?') to get the 1st value, ('.~') to set values, ('%~') to modify them. ('.') composes traversals just as it composes lenses. ('^.') can be used with traversals as well, but don't confuse it with ('^..') – ('^..') gets all traversed values, ('^.') combines traversed values using the ('Data.Monoid.<>') operation (if the values are instances of 'Monoid'; if they aren't, it won't compile). 'traverseOf' and 'traverseOf_' apply an action to all pointed values of a traversal.

Traversals don't differ from lenses when it comes to setting – you can use usual ('%~') and ('.~') to modify and set values. Getting is a bit different, because you have to decide what to do in the case of multiple values. In particular, you can use these combinators (as well as everything else in the “Folds” section):

  * ('^..') gets a list of values
  * ('^?') gets the 1st value (or 'Nothing' if there are no values)
  * ('^?!') gets the 1st value and throws an exception if there are no values

If you are sure that the traversal will traverse at least one value, you can convert it to a lens with 'singular'.

'traversed' is a universal traversal for anything that belongs to the 'Traversable' typeclass. However, in many cases 'each' works as well and is shorter and nicer-looking.
-}

{- |
Apply an action to all targets (like 'Control.Monad.mapM' or 'Data.Traversable.traverse'):

>>> traverseOf both readFile ("file1", "file2")
(<contents of file1>, <contents of file2>)

>>> traverseOf _1 id (Just 1, 2)
Just (1, 2)
>>> traverseOf _1 id (Nothing, 2)
Nothing

You can also just apply the lens\/traversal directly (but 'traverseOf' might be more readable):

>>> both readFile ("file1", "file2")
(<contents of file1>, <contents of file2>)

If you don't need the result, use 'traverseOf_'.
-}
traverseOf :: LensLike f s t a b -> (a -> f b) -> s -> f t
traverseOf = id
{-# INLINE traverseOf #-}

{- |
'traverseOf' with flipped arguments. Useful if the “loop body” is a lambda or
a @do@ block.
-}
forOf :: LensLike f s t a b -> s -> (a -> f b) -> f t
forOf = flip
{-# INLINE forOf #-}

{- |
'singular' turns a traversal into a lens that behaves like a single-element traversal:

>>> [1,2,3] ^. singular each
1

>>> [1,2,3] & singular each %~ negate
[-1,2,3]

If there is nothing to return, it'll throw an error:

>>> [] ^. singular each
*** Exception: Lens.Micro.singular: empty traversal

However, it won't fail if you are merely setting the value:

>>> [] & singular each %~ negate
-}
singular :: HasCallStack => Traversal s t a a -> Lens s t a a
singular l afb s = case ins b of
  (w:ws) -> unsafeOuts b . (:ws) <$> afb w
  []     -> unsafeOuts b . return <$>
              afb (error "Lens.Micro.singular: empty traversal")
  where
    Bazaar b = l sell s
    sell w = Bazaar ($ w)
    ins f = (unsafeCoerce :: [Identity a] -> [a])
              (getConst (f (\ra -> Const [Identity ra])))
    unsafeOuts f = evalState (f (\_ -> state (unconsWithDefault fakeVal)))
      where fakeVal = error "unsafeOuts: not enough elements were supplied"
    unconsWithDefault d []     = (d,[])
    unconsWithDefault _ (x:xs) = (x,xs)
{-# INLINE singular #-}

{- |
'failing' lets you chain traversals together; if the 1st traversal fails, the 2nd traversal will be used.

>>> ([1,2],[3]) & failing (_1.each) (_2.each) .~ 0
([0,0],[3])

>>> ([],[3]) & failing (_1.each) (_2.each) .~ 0
([],[0])

Note that the resulting traversal won't be valid unless either both traversals don't touch each others' elements, or both traversals return exactly the same results. To see an example of how 'failing' can generate invalid traversals, see <http://stackoverflow.com/questions/27138856/why-does-failing-from-lens-produce-invalid-traversals this Stackoverflow question>.
-}
failing :: Traversal s t a b -> Traversal s t a b -> Traversal s t a b
failing left right afb s = case pins b of
  [] -> right afb s
  _  -> b afb
  where
    Bazaar b = left sell s
    sell w = Bazaar ($ w)
    pins f = getConst (f (\ra -> Const [Identity ra]))

infixl 5 `failing`

{- |
'filtered' is a traversal that filters elements “passing” through it:

>>> (1,2,3,4) ^.. each
[1,2,3,4]

>>> (1,2,3,4) ^.. each . filtered even
[2,4]

It also can be used to modify elements selectively:

>>> (1,2,3,4) & each . filtered even %~ (*100)
(1,200,3,400)

The implementation of 'filtered' is very simple. Consider this traversal, which always “traverses” just the value it's given:

@
id :: 'Traversal'' a a
id f s = f s
@

And this traversal, which traverses nothing (in other words, /doesn't/ traverse the value it's given):

@
ignored :: 'Traversal'' a a
ignored f s = 'pure' s
@

And now combine them into a traversal that conditionally traverses the value it's given, and you get 'filtered':

@
filtered :: (a -> Bool) -> 'Traversal'' a a
filtered p f s = if p s then f s else 'pure' s
@

By the way, note that 'filtered' can generate illegal traversals – sometimes this can bite you. In particular, an optimisation that should be safe becomes unsafe. (To the best of my knowledge, this optimisation never happens automatically. If you just use 'filtered' to modify/view something, you're safe. If you don't define any traversals that use 'filtered', you're safe too.)

Let's use @evens@ as an example:

@
evens = 'filtered' 'even'
@

If @evens@ was a legal traversal, you'd be able to fuse several applications of @evens@ like this:

@
'over' evens f '.' 'over' evens g = 'over' evens (f '.' g)
@

Unfortunately, in case of @evens@ this isn't a correct optimisation:

  * the left-side variant applies @g@ to all even numbers, and then applies @f@ to all even numbers that are left after @f@ (because @f@ might've turned some even numbers into odd ones)

  * the right-side variant applies @f@ and @g@ to all even numbers

Of course, when you are careful and know what you're doing, you won't try to make such an optimisation. However, if you export an illegal traversal created with 'filtered' and someone tries to use it, they might mistakenly assume that it's legal, do the optimisation, and silently get an incorrect result.

If you are using 'filtered' with some another traversal that doesn't overlap with -whatever the predicate checks-, the resulting traversal will be legal. For instance, here the predicate looks at the 1st element of a tuple, but the resulting traversal only gives you access to the 2nd:

@
pairedWithEvens :: 'Traversal' [(Int, a)] [(Int, b)] a b
pairedWithEvens = 'each' '.' 'filtered' ('even' '.' 'fst') '.' '_2'
@

Since you can't do anything with the 1st components through this traversal, the following holds for any @f@ and @g@:

@
'over' pairedWithEvens f '.' 'over' pairedWithEvens g = 'over' pairedWithEvens (f '.' g)
@
-}
filtered :: (a -> Bool) -> Traversal' a a
filtered p f s = if p s then f s else pure s
{-# INLINE filtered #-}

{- |
'both' traverses both fields of a tuple. Unlike @<http://hackage.haskell.org/package/lens/docs/Control-Lens-Traversal.html#v:both both>@ from lens, it only works for pairs – not for triples or 'Either'.

>>> ("str","ing") ^. both
"string"

>>> ("str","ing") & both %~ reverse
("rts","gni")
-}
both :: Traversal (a, a) (b, b) a b
both f = \ ~(a, b) -> liftA2 (,) (f a) (f b)
{-# INLINE both #-}

{- |
'_head' traverses the 1st element of something (usually a list, but can also be a @Seq@, etc):

>>> [1..5] ^? _head
Just 1

It can be used to modify too, as in this example where the 1st letter of a sentence is capitalised:

>>> "mary had a little lamb." & _head %~ toTitle
"Mary had a little lamb."

The reason it's a traversal and not a lens is that there's nothing to traverse when the list is empty:

>>> [] ^? _head
Nothing

This package only lets you use '_head' on lists, but if you use <http://hackage.haskell.org/package/microlens-ghc microlens-ghc> you get instances for @ByteString@ and @Seq@, and if you use <http://hackage.haskell.org/package/microlens-platform microlens-platform> you additionally get instances for @Text@ and @Vector@.
-}
_head :: Cons s s a a => Traversal' s a
_head = _Cons._1
{-# INLINE _head #-}

{- |
'_tail' gives you access to the tail of a list (or @Seq@, etc):

>>> [1..5] ^? _tail
Just [2,3,4,5]

You can modify the tail as well:

>>> [4,1,2,3] & _tail %~ reverse
[4,3,2,1]

Since lists are monoids, you can use '_tail' with plain ('^.') (and then it'll return an empty list if you give it an empty list):

>>> [1..5] ^. _tail
[2,3,4,5]

>>> [] ^. _tail
[]

If you want to traverse each /element/ of the tail, use '_tail' with 'each':

>>> "I HATE CAPS." & _tail.each %~ toLower
"I hate caps."

This package only lets you use '_tail' on lists, but if you use <http://hackage.haskell.org/package/microlens-ghc microlens-ghc> you get instances for @ByteString@ and @Seq@, and if you use <http://hackage.haskell.org/package/microlens-platform microlens-platform> you additionally get instances for @Text@ and @Vector@.
-}
_tail :: Cons s s a a => Traversal' s s
_tail = _Cons._2
{-# INLINE _tail #-}

{- |
'_init' gives you access to all-but-the-last elements of the list:

>>> "Hello." ^. _init
"Hello"

See documentation for '_tail', as '_init' and '_tail' are pretty similar.
-}
_init :: Snoc s s a a => Traversal' s s
_init = _Snoc._1
{-# INLINE _init #-}

{- |
'_last' gives you access to the last element of the list:

>>> "Hello." ^? _last
'.'

See documentation for '_head', as '_last' and '_head' are pretty similar.
-}
_last :: Snoc s s a a => Traversal' s a
_last = _Snoc._2
{-# INLINE _last #-}


{- |
This generalizes 'Data.Traversable.mapAccumL' to an arbitrary 'Traversal'. (Note that it doesn't work on folds, only traversals.)

@
'mapAccumL' ≡ 'mapAccumLOf' 'traverse'
@
-}
mapAccumLOf :: LensLike (State acc) s t a b -> (acc -> a -> (acc, b)) -> acc -> s -> (acc, t)
mapAccumLOf l f acc0 s = swap (runState (l g s) acc0)
  where
    g a = state $ \acc -> swap (f acc a)
{-# INLINE mapAccumLOf #-}

-- Prisms ------------------------------------------------------------------

{- $prisms-note

Prisms are traversals that always target 0 or 1 values. Moreover, it's possible to /reverse/ a prism, using it to construct a structure instead of peeking into it. Here's an example from the lens library:

@
>>> over _Left (+1) (Left 2)
Left 3

>>> _Left # 5
Left 5
@

However, it's not possible for microlens to export prisms, because their type depends on @<http://hackage.haskell.org/package/profunctors/docs/Data-Profunctor.html#t:Choice Choice>@ from <http://hackage.haskell.org/package/profunctors profunctors>. So, all prisms included here are traversals instead (and you can't reverse them).
-}

{- |
'_Left' targets the value contained in an 'Either', provided it's a 'Left'.

Gathering all @Left@s in a structure (like the 'Data.Either.lefts' function, but not necessarily just for lists):

>>> [Left 1, Right 'c', Left 3] ^.. each._Left
[1,3]

Checking whether an 'Either' is a 'Left' (like 'Data.Either.isLeft'):

>>> has _Left (Left 1)
True

>>> has _Left (Right 1)
False

Extracting a value (if you're sure it's a 'Left'):

>>> Left 1 ^?! _Left
1

Mapping over all 'Left's:

>>> (each._Left %~ map toUpper) [Left "foo", Right "bar"]
[Left "FOO",Right "bar"]

Implementation:

@
'_Left' f (Left a)  = 'Left' '<$>' f a
'_Left' _ (Right b) = 'pure' ('Right' b)
@
-}
_Left :: Traversal (Either a b) (Either a' b) a a'
_Left f (Left a) = Left <$> f a
_Left _ (Right b) = pure (Right b)
{-# INLINE _Left #-}

{- |
'_Right' targets the value contained in an 'Either', provided it's a 'Right'.

See documentation for '_Left'.
-}
_Right :: Traversal (Either a b) (Either a b') b b'
_Right f (Right b) = Right <$> f b
_Right _ (Left a) = pure (Left a)
{-# INLINE _Right #-}

{- |
'_Just' targets the value contained in a 'Maybe', provided it's a 'Just'.

See documentation for '_Left' (as these 2 are pretty similar). In particular, it can be used to write these:

  * Unsafely extracting a value from a 'Just':

    @
    'Data.Maybe.fromJust' = ('^?!' '_Just')
    @

  * Checking whether a value is a 'Just':

    @
    'Data.Maybe.isJust' = 'has' '_Just'
    @

  * Converting a 'Maybe' to a list (empty or consisting of a single element):

    @
    'Data.Maybe.maybeToList' = ('^..' '_Just')
    @

  * Gathering all 'Just's in a list:

    @
    'Data.Maybe.catMaybes' = ('^..' 'each' '.' '_Just')
    @
-}
_Just :: Traversal (Maybe a) (Maybe a') a a'
_Just f (Just a) = Just <$> f a
_Just _ Nothing = pure Nothing
{-# INLINE _Just #-}

{- |
'_Nothing' targets a @()@ if the 'Maybe' is a 'Nothing', and doesn't target anything otherwise:

>>> Just 1 ^.. _Nothing
[]

>>> Nothing ^.. _Nothing
[()]

It's not particularly useful (unless you want to use @'has' '_Nothing'@ as a replacement for 'Data.Maybe.isNothing'), and provided mainly for consistency.

Implementation:

@
'_Nothing' f Nothing = 'const' 'Nothing' '<$>' f ()
'_Nothing' _ j       = 'pure' j
@
-}
_Nothing :: Traversal' (Maybe a) ()
_Nothing f Nothing = const Nothing <$> f ()
_Nothing _ j = pure j
{-# INLINE _Nothing #-}

-- Some of the guts of lens

newtype Traversed a f = Traversed { getTraversed :: f a }

instance Applicative f => Monoid (Traversed a f) where
  mempty = Traversed (pure (error "Lens.Micro.Traversed: value used"))
  {-# INLINE mempty #-}
#if !MIN_VERSION_base(4,11,0)
  Traversed ma `mappend` Traversed mb = Traversed (ma *> mb)
  {-# INLINE mappend #-}
#else
instance Applicative f => Semigroup (Traversed a f) where
  Traversed ma <> Traversed mb = Traversed (ma *> mb)
  {-# INLINE (<>) #-}
#endif

newtype Bazaar a b t = Bazaar (forall f. Applicative f => (a -> f b) -> f t)

instance Functor (Bazaar a b) where
  fmap f (Bazaar k) = Bazaar (fmap f . k)
  {-# INLINE fmap #-}

instance Applicative (Bazaar a b) where
  pure a = Bazaar $ \_ -> pure a
  {-# INLINE pure #-}
  Bazaar mf <*> Bazaar ma = Bazaar $ \afb -> mf afb <*> ma afb
  {-# INLINE (<*>) #-}

-- A reimplementation of State

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

type State s = StateT s Identity

state :: Monad m => (s -> (a, s)) -> StateT s m a
state f = StateT (return . f)
{-# INLINE state #-}

evalState :: State s a -> s -> a
evalState m s = fst (runState m s)
{-# INLINE evalState #-}

runState :: State s a -> s -> (a, s)
runState m = runIdentity . runStateT m
{-# INLINE runState #-}

instance (Functor m) => Functor (StateT s m) where
    fmap f m = StateT $ \ s ->
        fmap (\ ~(a, s') -> (f a, s')) $ runStateT m s
    {-# INLINE fmap #-}

instance (Functor m, Monad m) => Applicative (StateT s m) where
    pure a = StateT $ \ s -> return (a, s)
    {-# INLINE pure #-}
    StateT mf <*> StateT mx = StateT $ \ s -> do
        ~(f, s') <- mf s
        ~(x, s'') <- mx s'
        return (f x, s'')
    {-# INLINE (<*>) #-}

instance (Monad m) => Monad (StateT s m) where
#if !(MIN_VERSION_base(4,8,0))
    return a = StateT $ \ s -> return (a, s)
    {-# INLINE return #-}
#endif
    m >>= k  = StateT $ \ s -> do
        ~(a, s') <- runStateT m s
        runStateT (k a) s'
    {-# INLINE (>>=) #-}
    fail str = StateT $ \ _ -> fail str
    {-# INLINE fail #-}

#if MIN_VERSION_base(4,9,0)
instance (Fail.MonadFail m) => Fail.MonadFail (StateT s m) where
    fail str = StateT $ \ _ -> Fail.fail str
#endif
