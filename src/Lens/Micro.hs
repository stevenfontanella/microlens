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
  -- $intro

  -- * Setting
  ASetter,
  sets,
  -- $toy-setters
  mapped,
  over,
  (%~),
  -- $reverse-application
  (&),
  set,
  (.~),
  -- $record-def
  -- $state-examples
  -- $record-examples

  -- * Getter
  Getting,
  Getter,
  (^.),
  view,
  use,
  -- * Lens
  lens,
  Lens, Lens',
  -- * Traversal
  both,
  Traversal, Traversal',
  -- * Fold
  Fold,
  toListOf,
  (^..),
  (^?),
  (^?!),
  folded,
  -- * Prism
  _Left, _Right,
  _Just, _Nothing,
  -- * Tuple
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

{- |
@'ASetter' s t a b@ is something that turns a function modifying a value into
a function modifying a /structure/. If you ignore 'Identity' (as @'Identity'
a@ is the same thing as @a@), the type is:

@
type ASetter s t a b = (a -> b) -> s -> t
@

This means that examples of setters you might've already seen are:

  * @'map' :: (a -> b) -> [a] -> [b]@

  * @'fmap' :: 'Functor' f => (a -> b) -> f a -> f b@, which corresponds to
    'mapped'

  * @'Control.Arrow.first' :: (a -> b) -> (a, x) -> (b, x)@, which
    corresponds to '_1'

  * @'Control.Arrow.left' :: (a -> b) -> 'Either' a x -> 'Either' b x@, which
    corresponds to '_Left'

The reason 'Identity' is used here is for 'ASetter' to be composable with
other types, such as 'Lens'.

Technically, if you're writing a library, you shouldn't use this type for
setters you are exporting from your library; the right type to use is
@Setter@, but it is not provided by microlens. It's completely alright,
however, to export functions which take an 'ASetter' as an argument.
-}
type ASetter s t a b = (a -> Identity b) -> s -> Identity t

-- |
-- To make producing 'ASetter's from ordinary functions easy, there's
-- 'sets'. It simply unwraps @'Identity' b@ (the result of the original
-- function) and rewraps @t@ (the result of the modified function):
--
-- @
-- sets f = \g -> Identity . f (runIdentity . g)
-- @
--
-- @g@ is the function given to 'ASetter', of type @a -> 'Identity' b@.
-- @'runIdentity' . g@ gets us @a -> b@, then we pass it to @f@ to get
-- @s -> t@, and then we compose 'Identity' with it to get
-- @s -> 'Identity' t@, which is what the result of 'ASetter' has to be.
--
sets :: ((a -> b) -> s -> t) -> ASetter s t a b
sets f g = Identity . f (runIdentity . g)
{-# INLINE sets #-}

-- $toy-setters
--
-- Let's make some setters to make it easier to illustrate things later:
--
-- >>> let _1 = sets first
--
-- >>> let _2 = sets second
--
-- >>> let both = sets (\f (a, b) -> (f a, f b))
--
-- >>> let mapped = sets fmap  -- because 'fmap' is more general than 'map'
--
-- And I'll spell out what they do in English, just in case:
--
--   * @_1@ makes a function work on the 1st element of a tuple.
--   * @_2@ makes a function work on the 2nd element of a tuple.
--   * @both@ makes a function work on /both/ elements of a tuple (if they
--     are of the same type, of course).
--   * 'mapped' makes a function work on any 'Functor' (that is, a list, or
--     'Maybe', or even 'IO').

-- |
-- In reality only 'mapped' is an 'ASetter' (the rest can be used as setters
-- too, but they're actually more general, but ignore this for now).
mapped :: Functor f => ASetter (f a) (f b) a b
mapped = sets fmap
{-# INLINE mapped #-}

-- |
-- 'over' is a dual of 'sets'; it takes an 'ASetter' and makes an ordinary
-- function back from it:
--
-- @
-- over _1 === over (sets first) === first
-- @
--
-- The implementation is symmetrical as well; just switch 'Identity' and
-- 'runIdentity' in the definition of 'sets'.
--
-- @
-- over l = \f -> runIdentity . l (Identity . f)
-- @
--
-- With 'over', we can apply compositions of 'ASetter's just as we were
-- applying compositions of "ordinary functions" before:
--
-- @
-- over (mapped._1._1) === map.first.first
-- @
--
over :: ASetter s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)
{-# INLINE over #-}

-- |
-- And this is an operator version of 'over'. Some examples:
--
-- >>> (_1 %~ (+1)) (1,2)
-- (2,2)
--
-- >>> (mapped %~ reverse) ["hello","world"]
-- ["olleh","dlrow"]
--
(%~) :: ASetter s t a b -> (a -> b) -> s -> t
(%~) = over
{-# INLINE (%~) #-}

infixr 4 %~

#if __GLASGOW_HASKELL__ >= 710
import Data.Function ((&))
#endif

#if __GLASGOW_HASKELL__ < 710
(&) :: a -> (a -> b) -> b
a & f = f a
{-# INLINE (&) #-}
infixl 1 &
#endif

-- $reverse-application
--
-- Since @(_1 '%~' (+1)) (1,2)@ isn't really pretty, you can use ('&') to
-- turn things backwards and put the modifying function /after/ the thing
-- it's supposed to modify:
--
-- >>> (1,2) & _1 %~ (+1)
-- (2,2)
--
-- >>> [(1,"hello"),(2,"world")] & mapped._2 %~ reverse
-- [(1,"olleh"),(2,"dlrow")]
--
-- Let's retrace the steps again, from setters and weird operators to
-- ordinary functions:
--
-- @
-- [(1,"hello"),(2,"world")] & mapped._2 %~ reverse
--
-- [(1,"hello"),(2,"world")] & (mapped._2 %~ reverse)
--
-- (mapped._2 %~ reverse) [(1,"hello"),(2,"world")]
--
-- over (mapped._2) reverse $ [(1,"hello"),(2,"world")]
--
-- (map.second) reverse $ [(1,"hello"),(2,"world")]
--
-- map (second reverse) [(1,"hello"),(2,"world")]
-- @

-- |
-- Another convenient function is 'set', which replaces values instead of
-- modifying them, which is equivalent to using 'const' as the modifying
-- function:
--
-- >>> map (const "foo") [1,2,3]
-- ["foo","foo","foo"]
--
-- Here's how to write 'set':
--
-- @
-- set l b = over l (const b)
-- @
--
set :: ASetter s t a b -> b -> s -> t
set l b = runIdentity . l (\_ -> Identity b)
{-# INLINE set #-}

-- |
-- 'set' has an operator version as well. With it, we can mimic "assignment"
-- from OOP languages:
--
-- >>> let variable = (1,("foo","bar"))
--
-- >>> variable & _2._1 .~ "tux"
-- (1,("tux","bar"))
--
-- If this was some OOP language (C++, perhaps), it would've looked like this:
--
-- @
-- std::pair <int,<char*,char*> >  variable;
--
-- variable.second.first = "tux";
-- @
--
-- (Everything flows backwards in this example: @"tux"@ is turned into
-- @'const' "tux"@ by '.~', then '_1' picks it up to make a function which
-- works on tuples from it, then '_2' makes a function which works on tuples
-- of tuples, and finally '&' applies this thing to @variable@.)
--
(.~) :: ASetter s t a b -> b -> s -> t
(.~) = set
{-# INLINE (.~) #-}

infixr 4 .~

-- $record-def
--
-- To make examples more interesting, I'll define some types and setters for
-- them. (Full disclosure: I stole them from
-- <http://www.haskellforall.com/2013/05/program-imperatively-using-haskell.html
-- Gabriel>.)
--
-- @
-- -- This is internal state of a simple game.
-- data Game = Game
--   { _score :: Int
--   , _units :: [Unit]
--   , _boss  :: Unit
--   }
--   deriving (Show)
--
-- -- This is a unit in this game.
-- data Unit = Unit
--   { _health   :: Double
--   , _position :: Point
--   }
--   deriving (Show)
--
-- -- This is a type used to store location.
-- data Point = Point
--   { _x :: Double
--   , _y :: Double
--   }
--   deriving (Show)
-- @
--
-- And here are setters, which are all pretty similar:
--
-- @
-- score :: 'ASetter' Game Game Int Int
-- score = 'sets' $ \f game -> game {_score = f (_score game)}
--
-- units :: 'ASetter' Game Game [Unit] [Unit]
-- units = 'sets' $ \f game -> game {_units = f (_units game)}
--
-- boss :: 'ASetter' Game Game Unit Unit
-- boss = 'sets' $ \f game -> game {_boss = f (_boss game)}
--
-- -- And so on...
-- @

-- $state-examples
--
-- The following examples showcase operators available from
-- "Lens.Micro.Extras". They aren't exported from the main module because
-- they aren't needed often and they clash with operators defined by other
-- libraries (@aeson@, @cassava@, @hxt@, @cmdargs@, and quite a few
-- others). If you find them useful (and they /are/ useful occasionally),
-- don't hesitate to import "Lenx.Micro.Extras".
--
-- 'Lens.Micro.Extras..=' is an assignment operator which works in 'State'
-- monad, thus resembling assignment from imperative languages even more. If
-- you are unfamiliar with 'State' monad, here's an example:
--
-- @
-- example :: State String ()
-- example = do
--   s <- 'get'                  -- 'get' returns current state
--   'put' (s ++ "!")            -- 'put' updates current state
--   'modify' (map toUpper)      -- 'modify' applies a function to current state
-- @
--
-- Here's what will happen once it's run:
--
-- @
-- >>> 'execState' example "hello"
-- "HELLO!"
-- @
--
-- Now, 'Lens.Micro.Extras..=' is '.~' lifted to work in 'State' -- that is,
-- instead of creating a function it creates a 'State' action which gets
-- applied to the current state. Like this:
--
-- @
-- example :: State (Int, String) ()
-- example = do
--   _1 .= 17
--   _2 .= "foo"
-- @
--
-- This code is equivalent to this:
--
-- @
-- example = do
--   s0 <- get
--   let s1 = s0  &  _1 .~ 17
--   let s2 = s1  &  _2 .~ "foo"
--   put s2
-- @
--
-- or, since you can use '&' several times in an expression (just like you
-- can use '$' several times), to this:
--
-- @
-- example = do
--   s <- get
--   let s' = s  &  _1 .~ 17
--               &  _2 .~ "foo"
--   put s'
-- @
--
-- or even to this:
--
-- @
-- example = modify $ (_1 .~ 17) . (_2 .~ "foo")
-- @
--
-- Don't let '&' fool you, there's no magic going on. It's just function
-- application.
--

-- $record-examples
--
-- Next examples use operators 'Lens.Micro.Extras.+=',
-- 'Lens.Micro.Extras.-=', 'Lens.Micro.Extras.*=', and
-- 'Lens.Micro.Extras.//='. They are all defined using
-- 'Lens.Micro.Extras.%=', which is to 'Lens.Micro.Extras..=' what '%~' is to
-- '.~'. You should be able to deduce what they do easily:
--
-- @
-- -- Boss's health.
-- bossHP :: 'ASetter' Game Game Double Double
-- bossHP = boss.health
--
-- -- Strike the boss.
-- strike :: State Game ()
-- strike = bossHP 'Lens.Micro.Extras.-=' 10
--
-- -- Breathe fire, damaging everyone.
-- breatheFire :: State Game ()
-- breatheFire = do
--   units.'mapped'.health 'Lens.Micro.Extras.*=' 0.7
--   bossHP 'Lens.Micro.Extras.*=' 0.9
--
-- -- Heal every unit by 20HP (with 100 being the cap).
-- healUnits :: State Game ()
-- healUnits =
--   units.'mapped'.health 'Lens.Micro.Extras.%=' (\h -> min 100 (h+20))
-- @

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
