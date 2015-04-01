{-# LANGUAGE
      CPP
    , MultiParamTypeClasses
    , FunctionalDependencies
    , FlexibleInstances
    , FlexibleContexts
    , RankNTypes
    , ScopedTypeVariables
  #-}


module Lens.Micro
(
  (&),

  -- * Setting (applying a function to values)
  ASetter,
  sets,
  (%~), over,
  (.~), set,
  mapped,

  -- * Getting (retrieving a value)
  -- $getters-note
  Getting,
  (^.), view,
  use,

  -- * Folds (getters which return multiple elements)
  (^..), toListOf,
  (^?),
  (^?!),
  folded,
  has,

  -- * Lenses (things which are both setters and getters)
  Lens, Lens',
  lens,

  -- * Traversals (lenses which have multiple targets)
  Traversal, Traversal',
  both,

  -- * Prisms
  -- $prisms-note
  _Left, _Right,
  _Just, _Nothing,

  -- * Tuples
  Field1(..),
  Field2(..),
  Field3(..),
  Field4(..),
  Field5(..),
)
where


import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Data.Foldable
import Data.Monoid

#if __GLASGOW_HASKELL__ >= 710
import Data.Function ((&))
#endif


{- $setup
-- >>> import Data.Char (toUpper)
-- >>> import Control.Arrow (first, second, left, right)
-}


#if __GLASGOW_HASKELL__ < 710
(&) :: a -> (a -> b) -> b
a & f = f a
{-# INLINE (&) #-}
infixl 1 &
#endif

-- Setting -----------------------------------------------------------------

{- |
@ASetter s t a b@ is something that turns a function modifying a value into a
function modifying a /structure/. If you ignore 'Identity' (as @Identity a@
is the same thing as @a@), the type is:

@
type ASetter s t a b = (a -> b) -> s -> t
@

This means that examples of setters you might've already seen are:

  * @'map' :: (a -> b) -> [a] -> [b]@

    (which corresponds to 'mapped')

  * @'fmap' :: 'Functor' f => (a -> b) -> f a -> f b@

    (which corresponds to 'mapped' as well)

  * @'Control.Arrow.first' :: (a -> b) -> (a, x) -> (b, x)@

    (which corresponds to '_1')

  * @'Control.Arrow.left' :: (a -> b) -> Either a x -> Either b x@

    (which corresponds to '_Left')

The reason 'Identity' is used here is for 'ASetter' to be composable with
other types, such as 'Lens'.

Technically, if you're writing a library, you shouldn't use this type for
setters you are exporting from your library; the right type to use is
@Setter@, but it is not provided by microlens. It's completely alright,
however, to export functions which take an 'ASetter' as an argument.
-}
type ASetter s t a b = (a -> Identity b) -> s -> Identity t

{- |
'sets' creates an 'ASetter' from an ordinary function. (The only thing it
does is wrapping and unwrapping 'Identity'.)
-}
sets :: ((a -> b) -> s -> t) -> ASetter s t a b
sets f g = Identity . f (runIdentity . g)
{-# INLINE sets #-}

{- |
'%~' applies a function to the target; an alternative explanation is that it
is an inverse of 'sets', which turns a setter into an ordinary
function. @'mapped' '%~' reverse@ is the same thing as @'fmap' reverse@.

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
'over' is a synonym for '%~'.

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
over l f = runIdentity . l (Identity . f)
{-# INLINE over #-}

{- |
'.~' assigns a value to the target. These are equivalent:

@
l '.~' x
l '%~' 'const' x
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
'set' is a synonym for '.~'.

Setting the 1st component of a pair:

@
'set' '_1' :: x -> (a, b) -> (x, b)
'set' '_1' = \\x t -> (x, snd t)
@

Using it to rewrite 'Data.Functor.<$':

@
'set' 'mapped' :: 'Functor' f => a -> f b -> f a
'set' 'mapped' = ('Data.Functor.<$')
@
-}
set :: ASetter s t a b -> b -> s -> t
set l b = runIdentity . l (\_ -> Identity b)
{-# INLINE set #-}

{- |
'mapped' is a setter for everything contained in a functor. You can use it
to map over lists, @Maybe@, or even @IO@ (which is something you can't do
with 'traversed' or 'each').

Here 'mapped' is used to turn a value to all non-@Nothing@ values in a list:

>>> [Just 3,Nothing,Just 5] & mapped.mapped .~ 0
[Just 0,Nothing,Just 0]

Keep in mind that while 'mapped' is a more powerful setter than 'each', it
can't be used as a getter! This won't work (and will fail with a type error):

@
[(1,2),(3,4),(5,6)] '^..' 'mapped' . 'both'
@
-}
mapped :: Functor f => ASetter (f a) (f b) a b
mapped = sets fmap
{-# INLINE mapped #-}

-- Getting -----------------------------------------------------------------

{- $getters-note

Getters are a not-entirely-obvious way to use (supposedly) /value-changing/
traversals to /carry out/ information from a structure. For details, see the
documentation for 'Getting'.

Exporting @Getter@ is impossible, as then microlens would have to depend on
contravariant.
-}

{- |
@Getting r s a@ is, in a way, equivalent to @s -> a@. Since @'Const' r a@ is
the same as @r@, 'Getting' is actually @(a -> r) -> s -> r@, which is just
CPS-transformed @s -> a@. The reason 'Const' and CPS are used is that we want
getters to have the same shape as lenses (which we achieve because 'Const' is
a functor).
-}
type Getting r s a = (a -> Const r a) -> s -> Const r s

{- |
'^.' applies a getter to a value; in other words, it gets a value out of a
structure using a getter (which can be a lens, traversal, fold, etc.).

Getting 1st field of a tuple:

@
('^.' '_1') :: (a, b) -> a
('^.' '_1') = 'fst'
@

When '^.' is used with a traversal, it combines all results using the
'Monoid' instance for the resulting type. For instance, for lists it would be
simple concatenation:

>>> ("str","ing") ^. each
"string"

The reason for this is that traversals use 'Applicative', and the
'Applicative' instance for 'Const' uses monoid concatenation to combine
“effects” of 'Const'.
-}
(^.) :: s -> Getting a s a -> a
s ^. l = getConst (l Const s)
{-# INLINE (^.) #-}

infixl 8 ^.

{- |
'view' is a synonym for '^.', generalised for 'MonadReader' (since functions
are instances of the 'MonadReader' class).
-}
view :: MonadReader s m => Getting a s a -> m a
view l = asks (getConst . l Const)
{-# INLINE view #-}

{- |
'use' is 'view' which implicitly operates on the state.
-}
use :: MonadState s m => Getting a s a -> m a
use l = gets (view l)
{-# INLINE use #-}

-- Folds -------------------------------------------------------------------

-- | A 'Monoid' for a 'Contravariant' 'Applicative'.
newtype Folding f a = Folding { getFolding :: f a }

instance (Applicative (Const r)) => Monoid (Folding (Const r) a) where
  mempty = Folding (Const . getConst $ pure ())
  {-# INLINE mempty #-}
  Folding fr `mappend` Folding fs = Folding (fr *> fs)
  {-# INLINE mappend #-}

{- |
@s ^.. t@ returns the list of all values that @t@ gets from @s@.

Turning a 'Maybe' into a list (either empty or having 1 element – that's what
'Data.Maybe.maybeToList' does):

>>> Just 3 ^.. _Just
[3]


-}
(^..) :: s -> Getting (Endo [a]) s a -> [a]
s ^.. l = toListOf l s
{-# INLINE (^..) #-}

infixl 8 ^..

toListOf :: Getting (Endo [a]) s a -> s -> [a]
toListOf l = foldrOf l (:) []
{-# INLINE toListOf #-}

{- |
@s ^? t@ returns the 1st element @t@ returns, or 'Nothing' if @t@ doesn't
return anything.

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

It's trivially implemented by passing 'First' to the getter.
-}
(^?) :: s -> Getting (First a) s a -> Maybe a
s ^? l = getFirst (foldMapOf l (First . Just) s)
{-# INLINE (^?) #-}

infixl 8 ^?

{- |
'^?!' is an unsafe variant of '^?' – instead of using 'Nothing' to indicate
that there were no elements returned, it throws an exception.
-}
(^?!) :: s -> Getting (Endo a) s a -> a
s ^?! l = foldrOf l const (error "(^?!): empty Fold") s
{-# INLINE (^?!) #-}

infixl 8 ^?!

foldrOf :: Getting (Endo r) s a -> (a -> r -> r) -> r -> s -> r
foldrOf l f z = flip appEndo z . foldMapOf l (Endo . f)
{-# INLINE foldrOf #-}

foldMapOf :: Getting r s a -> (a -> r) -> s -> r
foldMapOf l f = getConst . l (Const . f)
{-# INLINE foldMapOf #-}

folded :: (Foldable f, Applicative (Const r)) => Getting r (f a) a
folded f = Const . getConst . getFolding . foldMap (Folding . f)
{-# INLINE folded #-}

{- |
'has' checks whether a getter (any getter, including lenses, traversals, and
folds) returns at least 1 value.

Checking whether a list is non-empty:

>>> has each []
False

You can also use it with e.g. '_Left' (and other 0-or-1 traversals) as a
replacement for 'Data.Maybe.isNothing', 'Data.Maybe.isJust' and other
@isConstructorName@:

>>> has _Left (Left 1)
True
-}
has :: Getting Any s a -> s -> Bool
has l = getAny . foldMapOf l (\_ -> Any True)
{-# INLINE has #-}

-- Lenses ------------------------------------------------------------------

{- |
Lenses in a nutshell: use '^.' to get, '.~' to set, '%~' to modify. '.'
composes lenses (i.e. if a @B@ is a part of @A@, and a @C@ is a part of in
@B@, then @b.c@ lets you operate on @C@ inside @A@). You can create lenses
with 'lens', or you can write them by hand (see below).

@Lens s t a b@ is the lowest common denominator of a setter and a getter,
something that has the power of both; it has a 'Functor' constraint, and
since both 'Const' and 'Identity' are functors, it can be used whenever a
getter or a setter is needed.

  * @a@ is the type of the value inside of structure
  * @b@ is the type of the replaced value
  * @s@ is the type of the whole structure
  * @t@ is the type of the structure after replacing @a@ in it with @b@

A 'Lens' can only point at a single value inside a structure (unlike a
'Traversal').

It is easy to write lenses manually. The generic template is:

@
somelens :: Lens s t a b

-- "f" is the "a -> f b" function, "s" is the structure.
somelens f s =
  let
    a = ...                 -- Extract the value from "s".
    rebuildWith b = ...     -- Write a function which would
                            -- combine "s" and modified value
                            -- to produce new structure.
  in
    rebuildWith '<$>' f a     -- Apply the structure-producing
                            -- function to the modified value.
@

Here's the '_1' lens:

@
_1 :: Lens (a, x) (b, x) a b
_1 f (a, x) = (\\b -> (b, x)) <$> f a
@

Here's a more complicated lens, which extracts /several/ values from a
structure (in a tuple):

@
type Age     = Int
type City    = String
type Country = String

data Person = Person Age City Country

-- This lens lets you access all location-related information about a person.
location :: 'Lens'' Person (City, Country)
location f (Person age city country) =
  (\\(city', country') -> Person age city' country') <$> f (city, country)
@

You even can choose to use a lens to present /all/ information contained in
the structure (in a different way). Such lenses are called @Iso@ in lens's
terminology. For instance (assuming you don't mind functions that can error
out), here's a lens which lets you act on the string representation of a
value:

@
string :: (Read a, Show a) => 'Lens'' a String
string f s = read <$> f (show s)
@

Using it to reverse a number:

@
>>> 123 & string %~ reverse
321
@
-}
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

{- |
This is a type alias for monomorphic lenses which don't change the type of
the container (or of the value inside).
-}
type Lens' s a = Lens s s a a

{- |
'lens' creates a 'Lens' from a getter and a setter. The resulting lens isn't
the most effective one (because of having to traverse the structure twice
when modifying), but it shouldn't matter much.

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

When getting, the setter is completely unused. When setting, the getter is
unused. Both are used only when the value is being modified.

Here's an example of using a lens targeting the head of a list. The getter is
replaced with 'undefined' to make sure it's not used:

>>> [1,2,3] & lens undefined (\s b -> b : tail s) .~ 10
[10,2,3]
-}
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s <$> afb (sa s)
{-# INLINE lens #-}

-- Traversals --------------------------------------------------------------

{- |
Traversals in a nutshell: they're like lenses but they can point at multiple
values. Use '^..' (not '^.') to get all values, '^?' to get the 1st value,
'.~' to set values, '%~' to modify them. '.' composes traversals just as it
composes lenses.

@Traversal s t a b@ is a generalisation of 'Lens' which allows many targets
(possibly 0). It's achieved by changing the constraint to 'Applicative'
instead of 'Functor' – indeed, the point of 'Applicative' is that you can
combine effects, which is just what we need to have many targets.

Traversals don't differ from lenses when it comes to setting – you can use
usual '%~' and '.~' to modify and set values. Getting is a bit different,
because you have to decide what to do in the case of multiple values. In
particular, you can use these combinators (as well as everything else in the
“Folds” section):

  * '^..' gets a list of values
  * '^?' gets the 1st value (or 'Nothing' if there are no values)
  * '^?!' gets the 1st value and throws an exception if there are no values

In addition, '^.' works for traversals as well – it combines traversed values
using the '<>' operation (if the values are instances of 'Monoid').

Traversing any value twice is a violation of traversal laws. You can,
however, traverse values in any order.

Ultimately, traversals should follow 2 laws:

@
t pure ≡ pure
fmap (t f) . t g ≡ getCompose . t (Compose . fmap f . g)
@

The 1st law states that you can't change the shape of the structure or do
anything funny with elements (traverse elements which aren't in the
structure, create new elements out of thin air, etc.). The 2nd law states
that you should be able to fuse 2 identical traversals into one. For a more
detailed explanation of the laws, see
<http://artyom.me/lens-over-tea-2#traversal-laws this blog post> (if you
prefer rambling blog posts), or
<https://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf The Essence
Of The Iterator Pattern> (if you prefer papers).
-}
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

{- |
This is a type alias for monomorphic traversals which don't change the type
of the container (or of the values inside).
-}
type Traversal' s a = Traversal s s a a

{- |
'both' traverses both fields of a tuple. Unlike @both@ from lens, it only
works for pairs – not for triples or 'Either'.

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

Prisms are traversals which always target 0 or 1 values. Moreover, it's
possible to /reverse/ a prism, using it to construct a structure instead of
peeking into it. Here's an example from the lens library:

@
>>> over _Left (+1) (Left 2)
Left 3

>>> _Left # 5
Left 5
@

However, it's not possible for microlens to export prisms, because their type
depends on @Choice@, which resides in the profunctors library, which is a
somewhat huge dependency. So, all prisms included here are traversals
instead.
-}

{- |
'_Left' targets the value contained in an 'Either', provided it's a 'Left'.

Gathering all @Left@s in a structure (like the 'Data.Either.lefts' function):

@
'toListOf' ('each' . '_Left') :: ['Either' a b] -> [a]
'toListOf' ('each' . '_Left') = 'Data.Either.lefts'
@

Checking whether an 'Either' is a 'Left' (like 'Data.Either.isLeft'):

>>> has _Left (Left 1)
True

>>> has _Left (Right 1)
False

Extracting a value (if you're sure it's a 'Left'):

>>> Left 1 ^?! _Left
1

Mapping over all @Left@s:

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

See documentation for '_Left' (as these 2 are pretty similar). In particular,
it can be used to write these:

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

  * Gathering all @Just@s in a list:

    @
    'Data.Maybe.catMaybes' = ('^..' 'each' . '_Just')
    @
-}
_Just :: Traversal (Maybe a) (Maybe a') a a'
_Just f (Just a) = Just <$> f a
_Just _ Nothing = pure Nothing
{-# INLINE _Just #-}

{- |
'_Nothing' targets a @()@ if the 'Maybe' is a 'Nothing', and doesn't target
anything otherwise:

>>> Just 1 ^.. _Nothing
[]

>>> Nothing ^.. _Nothing
[()]

It's not particularly useful (unless you want to use @'has' '_Nothing'@ as a
replacement for 'Data.Maybe.isNothing'), and provided mainly for consistency.

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

-- Tuples ------------------------------------------------------------------

-- Commented instances amount to ~0.8s of building time.

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

This is done to avoid violating a lens law stating that you can get
back what you put:

>>> view _1 . set _1 10 $ (undefined :: (Int, Int))
10

The implementation (for 2-tuples) is:

@
'_1' f t = (,) '<$>' f    (fst t)
             '<*>' 'pure' (snd t)
@

or, alternatively,

@
'_1' f ~(a,b) = (\\a' -> (a',b)) '<$>' f a
@

(where @~@ means a lazy pattern).
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
  {- |
Gives access to the 2nd field of a tuple (up to 5-tuples).

See documentation for '_1'.
  -}
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
  {- |
Gives access to the 3rd field of a tuple (up to 5-tuples).

See documentation for '_1'.
  -}
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
  {- |
Gives access to the 4th field of a tuple (up to 5-tuples).

See documentation for '_1'.
  -}
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
  {- |
Gives access to the 5th field of a tuple (only for 5-tuples).

See documentation for '_1'.
  -}
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
