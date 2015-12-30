{-# LANGUAGE
CPP,
FlexibleInstances,
FlexibleContexts,
UndecidableInstances,
RankNTypes,
ScopedTypeVariables,
Trustworthy
  #-}


module Lens.Micro
(
  (&),
  -- $ampersand-note

  -- * Setting (applying a function to values)
  -- $setting-note
  ASetter, ASetter',
  sets,
  (%~), over,
  (.~), set,
  (<%~), (<<%~), (<<.~),
  mapped,

  -- * Getting (extracting a value)
  -- $getting-note
  SimpleGetter,
  Getting,
  (^.),
  to,

  -- * Folds (extracting multiple elements)
  -- $folds-note
  SimpleFold,
  (^..), toListOf,
  (^?),
  (^?!),
  has,
  folded,
  folding,

  -- * Lenses (combined getters-and-setters)
  Lens, Lens',
  lens,
  at,
  non,
  _1, _2, _3, _4, _5,

  -- * Traversals (lenses iterating over several elements)
  Traversal, Traversal',
  failing,
  filtered,
  both,
  traversed,
  each,
  ix,
  _head, _tail, _init, _last,

  -- * Prisms (traversals iterating over at most 1 element)
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
import qualified Data.Foldable as F

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

{- $setting-note

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

A nice thing about setters is that they compose easily – you can write @'map' . 'Control.Arrow.left'@ and it would be a function that takes a list of 'Either's and modifies all of them that are 'Left's.

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

-- Getting -----------------------------------------------------------------

{- $getting-note

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

folding :: Foldable f => (s -> f a) -> SimpleFold s a
folding sfa agb = phantom . F.traverse_ agb . sfa
{-# INLINE folding #-}

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

>>> Just 1 & non 0 .~ (+ 1)
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

{- |
'failing' lets you chain traversals together; if the 1st traversal fails, the 2nd traversal will be used.

>>> ([1,2],[3]) & failing (_1.each) (_2.each) .~ 0
([0,0],[3])

>>> ([],[3]) & failing (_1.each) (_2.each) .~ 0
([],[0])

Note that the resulting traversal won't be valid unless either both traversals don't touch each others' elements, or both traversals return exactly the same results. To see an example of how 'failing' can generate invalid traversals, see <http://stackoverflow.com/questions/27138856/why-does-failing-from-lens-produce-invalid-traversals this Stackoverflow question>.
-}
failing :: Traversal s t a b -> Traversal s t a b -> Traversal s t a b
failing left right afb s = case pins t of
  [] -> right afb s
  _  -> t afb
  where
    Bazaar t = left sell s
    sell w = Bazaar ($ w)
    pins f = getConst (f (\ra -> Const [Identity ra]))

infixl 5 `failing`

newtype Bazaar a b t = Bazaar (forall f. Applicative f => (a -> f b) -> f t)

instance Functor (Bazaar a b) where
  fmap f (Bazaar k) = Bazaar (fmap f . k)
  {-# INLINE fmap #-}

instance Applicative (Bazaar a b) where
  pure a = Bazaar $ \_ -> pure a
  {-# INLINE pure #-}
  Bazaar mf <*> Bazaar ma = Bazaar $ \afb -> mf afb <*> ma afb
  {-# INLINE (<*>) #-}

{- |
'filtered' is a traversal that filters elements “passing” thru it:

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
filtered p s = if p s then f s else 'pure' s
@

By the way, note that 'filtered' can generate illegal traversals – sometimes this can bite you. For instance, take @evens@:

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

Of course, when you are careful and know what you're doing, you won't try to make such an optimisation. However, if you export an illegal traversal created with 'filtered' and someone tries to use it, ne might mistakenly assume that it's legal, do the optimisation, and silently get an incorrect result.

If you are using 'filtered' with some another traversal that doesn't overlap with -whatever the predicate checks-, the resulting traversal will be legal. For instance, here the predicate looks at the 1st element of a tuple, but the resulting traversal only gives you access to the 2nd:

@
pairedWithEvens :: 'Traversal' [(Int, a)] [(Int, b)] a b
pairedWithEvens = 'each' '.' 'filtered' ('even' '.' 'fst') '.' '_2'
@

Since you can't do anything with the 1st components thru this traversal, the following holds for any @f@ and @g@:

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

This package only lets you use '_head' on lists, but you can use @Lens.Micro.GHC@ from <http://hackage.haskell.org/package/microlens-ghc microlens-ghc> and get instances for @ByteString@ and @Seq@, or use @Lens.Micro.Platform@ from <http://hackage.haskell.org/package/microlens-platform microlens-platform> and additionally get instances for @Text@ and @Vector@.
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

This package only lets you use '_tail' on lists, but you can use @Lens.Micro.GHC@ from <http://hackage.haskell.org/package/microlens-ghc microlens-ghc> and get instances for @ByteString@ and @Seq@, or use @Lens.Micro.Platform@ from <http://hackage.haskell.org/package/microlens-platform microlens-platform> and additionally get instances for @Text@ and @Vector@.
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
