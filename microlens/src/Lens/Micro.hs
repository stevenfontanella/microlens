{-# LANGUAGE
CPP,
FlexibleInstances,
FlexibleContexts,
UndecidableInstances,
RankNTypes,
ScopedTypeVariables
  #-}


module Lens.Micro
(
  (&),
  -- $ampersand-note

  -- * Setting (applying a function to values)
  ASetter, ASetter',
  sets,
  (%~), over,
  (.~), set,
  mapped,

  -- * Getting (retrieving a value)
  -- $getters-note
  Getting,
  (^.),

  -- * Folds (getters returning multiple elements)
  -- $folds-note
  (^..),
  (^?),
  (^?!),
  folded,
  has,

  -- * Lenses (setters and getters at once)
  Lens, Lens',
  lens,
  at,
  _1, _2, _3, _4, _5,

  -- * Traversals (lenses iterating over several elements)
  Traversal, Traversal',
  both,
  traversed,
  each,
  ix,

  -- * Prisms (traversals iterating over at most 1 element)
  -- $prisms-note
  _Left, _Right,
  _Just, _Nothing,
)
where


import Lens.Micro.Type
import Lens.Micro.Internal

import Control.Applicative
import Data.Functor.Identity
import Data.Monoid

#if __GLASGOW_HASKELL__ >= 710
import Data.Function ((&))
#endif


{- $setup
-- >>> import Data.Char (toUpper)
-- >>> import Control.Arrow (first, second, left, right)
-}


#if __GLASGOW_HASKELL__ < 710
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

-- Setting -----------------------------------------------------------------

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

{- |
('.~') assigns a value to the target. These are equivalent:

* @l '.~' x@
* @l '%~' 'const' x@

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

-- Getting -----------------------------------------------------------------

{- $getters-note

Getters are a not-entirely-obvious way to use lenses to /carry out/ information from a structure (instead of changing something inside the structure). Any lens or traversal is a getter.

For details, see the documentation for 'Getting'.

Including @<http://hackage.haskell.org/package/lens/docs/Control-Lens-Getter.html#t:Getter Getter>@ is impossible, as then this package would have to depend on <http://hackage.haskell.org/package/contravariant contravariant> and it's a big dependency.
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
-}
(^.) :: s -> Getting a s a -> a
s ^. l = getConst (l Const s)
{-# INLINE (^.) #-}

infixl 8 ^.

-- Folds -------------------------------------------------------------------

{- $folds-note

Folds are getters that can traverse more than one element (or no elements at all). The only fold here which isn't simultaneously a 'Traversal' is 'folded' (traversals are folds that also can modify elements they're traversing).

You can apply folds to values by using operators like ('^..'), ('^?'), etc:

>>> (1,2) ^.. both
[1,2]

A nice thing about folds is that you can combine them with ('Data.Monoid.<>') to concatenate their outputs:

>>> (1,2,3) ^.. (_2 <> _1)  -- in reversed order because why not
[2,1]

You can build more complicated getters with it when 'each' would be unhelpful:

>>> ([1,2], 3, [Nothing, Just 4]) ^.. (_1.each <> _2 <> _3.each._Just)
[1,2,3,4]

It plays nicely with ('^?'), too:

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
s ^.. l = foldrOf l (:) [] s
{-# INLINE (^..) #-}

infixl 8 ^..

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
-}
(^?) :: s -> Getting (First a) s a -> Maybe a
s ^? l = getFirst (foldMapOf l (First #. Just) s)
{-# INLINE (^?) #-}

infixl 8 ^?

{- |
('^?!') is an unsafe variant of ('^?') – instead of using 'Nothing' to indicate that there were no elements returned, it throws an exception.
-}
(^?!) :: s -> Getting (Endo a) s a -> a
s ^?! l = foldrOf l const (error "(^?!): empty Fold") s
{-# INLINE (^?!) #-}

infixl 8 ^?!

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

-- Lenses ------------------------------------------------------------------

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

-- Traversals --------------------------------------------------------------

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

-- Prisms ------------------------------------------------------------------

{- $prisms-note

Prisms are traversals that always target 0 or 1 values. Moreover, it's possible to /reverse/ a prism, using it to construct a structure instead of peeking into it. Here's an example from the lens library:

@
>>> over _Left (+1) (Left 2)
Left 3

>>> _Left # 5
Left 5
@

However, it's not possible for microlens to export prisms, because their type depends on @<http://hackage.haskell.org/package/profunctors/docs/Data-Profunctor.html#t:Choice Choice>@, which resides in the <http://hackage.haskell.org/package/profunctors profunctors> library, which is a somewhat huge dependency. So, all prisms included here are traversals instead.
-}

{- |
'_Left' targets the value contained in an 'Either', provided it's a 'Left'.

Gathering all @Left@s in a structure (like the 'Data.Either.lefts' function, but not necessarily just for lists):

>>> [Left 1, Right 'c', Left 3] ^.. each._Just
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
