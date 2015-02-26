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
  -- * Setting (applying a function to values)
  ASetter,
  sets,
  (%~), over,
  (.~), set,
  (&),
  mapped,

  -- * Getting (retrieving a value)
  Getting,
  Getter,
  (^.), view,
  use,

  -- * Lenses (things which are both setters and getters)
  Lens, Lens',
  lens,

  -- * Traversals (lenses which have multiple targets)
  Traversal, Traversal',
  both,

  -- * Folds
  Fold,
  (^..), toListOf,
  (^?),
  (^?!),
  folded,

  -- * Prisms
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

infixr 4 %~, .~


{- $setup
-- >>> import Data.Char (toUpper)
-- >>> import Control.Arrow (first, second, left, right)
-}

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
'mapped' is a setter for everything contained in a functor. You can use it
to map over lists, @Maybe@, or even @IO@ (which is something you can't do
with 'traversed' or 'each').

Here 'mapped' is used to turn a value to all non-@Nothing@ values in a list:

>>> [Just 3,Nothing,Just 5] & mapped.mapped .~ 0
[Just 0,Nothing,Just 0]
-}
mapped :: Functor f => ASetter (f a) (f b) a b
mapped = sets fmap
{-# INLINE mapped #-}

{- |
'%~' applies a function to the target; an alternative explanation is that it
is an inverse of 'sets', which turns a setter into an ordinary
function. @'mapped' '%~' reverse@ is the same thing as @'fmap' reverse@.

See 'over' if you want a non-operator synonym.

In this example we negate the 1st element of a pair:

>>> (1,2) & _1 %~ negate
(-1,2)

In this example we upper-case all @Left@s in a list:

>>> (mapped._Left.mapped %~ toUpper) [Left "foo", Right "bar"]
[Left "FOO",Right "bar"]
-}
(%~) :: ASetter s t a b -> (a -> b) -> s -> t
(%~) = over
{-# INLINE (%~) #-}

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

In this example @'over' '_2'@ is used as a replacement for
'Control.Arrow.second':

>>> over _2 show (10,20)
(10,"20")
-}
over :: ASetter s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)
{-# INLINE over #-}

#if __GLASGOW_HASKELL__ >= 710
import Data.Function ((&))
#endif

#if __GLASGOW_HASKELL__ < 710
(&) :: a -> (a -> b) -> b
a & f = f a
{-# INLINE (&) #-}
infixl 1 &
#endif

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


-- Getter.hs

infixl 8 ^.

type Getting r s a = (a -> Const r a) -> s -> Const r s

type Getter s a = forall r. (a -> Const r a) -> s -> Const r s

view :: MonadReader s m => Getting a s a -> m a
view l = asks (getConst . l Const)
{-# INLINE view #-}

(^.) :: s -> Getting a s a -> a
s ^. l = getConst (l Const s)
{-# INLINE (^.) #-}

use :: MonadState s m => Getting a s a -> m a
use l = gets (view l)
{-# INLINE use #-}

-- Setter.hs

-- Lens.hs

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s <$> afb (sa s)
{-# INLINE lens #-}

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a

-- Traversal.hs

both :: Traversal (a, a) (b, b) a b
both f = \ ~(a, b) -> liftA2 (,) (f a) (f b)
{-# INLINE both #-}

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

type Traversal' s a = Traversal s s a a

-- Fold.hs

infixl 8 ^.., ^?, ^?!

-- type Fold s a = forall f. (Contravariant f, Applicative f)
--               => (a -> f a) -> s -> f s
--
-- We don't want to depend on contravariant, and the only instance of it
-- we're going to use is 'Const a' anyway.

type Fold s a = forall r. (Applicative (Const r))
                => (a -> Const r a) -> s -> Const r s

-- | A 'Monoid' for a 'Contravariant' 'Applicative'.
newtype Folding f a = Folding { getFolding :: f a }

instance (Applicative (Const r)) => Monoid (Folding (Const r) a) where
  mempty = Folding (Const . getConst $ pure ())
  {-# INLINE mempty #-}
  Folding fr `mappend` Folding fs = Folding (fr *> fs)
  {-# INLINE mappend #-}

toListOf :: Getting (Endo [a]) s a -> s -> [a]
toListOf l = foldrOf l (:) []
{-# INLINE toListOf #-}

(^..) :: s -> Getting (Endo [a]) s a -> [a]
s ^.. l = toListOf l s
{-# INLINE (^..) #-}

(^?) :: s -> Getting (First a) s a -> Maybe a
s ^? l = getFirst (foldMapOf l (First . Just) s)
{-# INLINE (^?) #-}

(^?!) :: s -> Getting (Endo a) s a -> a
s ^?! l = foldrOf l const (error "(^?!): empty Fold") s
{-# INLINE (^?!) #-}

foldrOf :: Getting (Endo r) s a -> (a -> r -> r) -> r -> s -> r
foldrOf l f z = flip appEndo z . foldMapOf l (Endo . f)
{-# INLINE foldrOf #-}

foldMapOf :: Getting r s a -> (a -> r) -> s -> r
foldMapOf l f = getConst . l (Const . f)
{-# INLINE foldMapOf #-}

folded :: Foldable f => Fold (f a) a
folded f = Const . getConst . getFolding . foldMap (Folding . f)
{-# INLINE folded #-}

-- Prism.hs

-- We can't have @Prism@s, because they depend on something profunctor-y.

_Left :: Traversal (Either a b) (Either a' b) a a'
_Left f (Left a) = Left <$> f a
_Left _ (Right b) = pure (Right b)

_Right :: Traversal (Either a b) (Either a b') b b'
_Right f (Right b) = Right <$> f b
_Right _ (Left a) = pure (Left a)

_Just :: Traversal (Maybe a) (Maybe a') a a'
_Just f (Just a) = Just <$> f a
_Just _ Nothing = pure Nothing

_Nothing :: Traversal' (Maybe a) ()
_Nothing f Nothing = const Nothing <$> f ()
_Nothing _ j = pure j

-- Tuple.hs

-- Commented instances amount to ~0.8s of building time.

-- | Provides access to 1st field of a tuple.
class Field1 s t a b | s -> a, t -> b, s b -> t, t a -> s where
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

-- | Provides access to the 2nd field of a tuple.
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

-- | Provides access to the 3rd field of a tuple.
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

-- | Provide access to the 4th field of a tuple.
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

-- | Provides access to the 5th field of a tuple.
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
