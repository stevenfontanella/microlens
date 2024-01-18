{-|
Module      : Lens.Micro.Pro
Copyright   : (C) 2013-2016 Edward Kmett, 2018 Monadfix
License     : BSD-style (see the file LICENSE)

This module is home to lens definitions that require
[profunctors](https://hackage.haskell.org/package/profunctors), most notably
'Iso' and 'Prism'. Depending on 'profunctors' is quite the to bear — one
that includes all dependencies of @microlens-platform@. For this reason,
@microlens-pro@ ships with a compatiblity module "Lens.Micro.ProCompat" which
re-exports the entirety of "Lens.Micro.Platform", but with the profunctor-less
definitions hidden and overridden with profunctor'd definitions from this module.

-}
{-# LANGUAGE DefaultSignatures #-}
module Lens.Micro.Pro
    (
    -- * Iso: Losslessly convert between types
    -- $isos-note
      Iso, Iso'
    -- ** Constructing Isos
    , iso
    , from
    , non, non'
    -- ** Common Isos
    , _Show
    , strict, lazy
    , enum
    , coerced
    , mapping
    -- ** Miscellaneous
    , AnIso, AnIso'
    , cloneIso
    , withIso

    -- * Prism: A traversal with zero or one targets
    -- $prisms-note
    , Prism, Prism'
    -- ** Constructing Prisms
    , prism, prism'
    , nearly
    , only
    -- ** Common Prisms
    , _Left, _Right
    , _Just, _Nothing
    , _Empty
    -- ** Miscellaneous
    , APrism, APrism'
    , clonePrism
    , withPrism

    -- * Review
    , AReview
    , SimpleReview
    , re
    , review
    , (#)
    , unto
    )
    where
--------------------------------------------------------------------------------
import Lens.Micro                   (has)
import Lens.Micro.Contra
import Lens.Micro.Pro.Type
import Lens.Micro.Pro.Internal
import Control.Monad                (guard)
import Control.Monad.Reader.Class
import Data.Coerce
import Data.Maybe
import Data.Tagged
import Data.Functor.Contravariant
import Data.Functor.Identity
import Data.Bifunctor
import Data.Void
import Data.Profunctor
import Data.Profunctor.Unsafe
import GHC.Exts                     (TYPE)

-- implement instances
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.HashMap.Strict
import qualified Data.Map
import qualified Data.Vector
--------------------------------------------------------------------------------

-- | This type is used for effecient "deconstruction" of an 'Iso'. From the
-- user's perspective, a function with an 'AnIso' as an argument is simply
-- expecting a normal 'Iso'.

type AnIso s t a b = Exchange a b a (Identity b)
                  -> Exchange a b s (Identity t)

-- | Monomorphic 'AnIso'.

type AnIso' s a = AnIso s s a a

type APrism s t a b = Market a b a (Identity b) -> Market a b s (Identity t)

type APrism' s a = Market a a a (Identity a) -> Market a a s (Identity s)

-- | Extract the 'Iso' underlying every 'AnIso'

cloneIso :: AnIso s t a b -> Iso s t a b
cloneIso k = withIso k $ \sa bt -> iso sa bt

{-# INLINE cloneIso #-}

{- $isos-note

Isos (or isomorphisms) are lenses that convert a value instead of targeting a
part of it; in other words, inside of every list lives a reversed list, inside
of every strict @Text@ lives a lazy @Text@, and inside of every @(a, b)@ lives a
@(b, a)@. Since an isomorphism doesn't lose any information, it's possible to
/reverse/ it and use it in the opposite direction by using @from@:

@
from :: Iso' s a -> Iso' a s
from :: Iso s t a b -> Iso t s b a
@

The isomorphisms defined in this module are true lens-compatible isos. Many of
them share names with the lens-__incompatible__ definitions from
[Lens.Micro](https://hackage.haskell.org/package/microlens-0.4.13.1/docs/Lens-Micro.html#g:5)
and
[Lens.Micro.Platform](https://hackage.haskell.org/package/microlens-platform-0.4.3.4/docs/Lens-Micro-Platform.html).
For convenience, we provide a module "Lens.Micro.ProCompat" which emulates
Lens.Micro.Platform, but uses the lens-compatible isos.

-}

-- | Construct an 'Iso' from two inverse functions. See the documentation for
-- 'from' for a summary of the behaviour expected of the 'Iso' you create.

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (fmap bt)

{-# INLINE iso #-}

-- | Invert an 'Iso'. Should you define any 'Iso's, it's expected that they
-- abide by the following law, essentially saying that inverting an 'Iso' twice
-- yields the same 'Iso' you started with.
--
-- @
-- 'from' ('from' l) ≡ l
-- @

from :: AnIso s t a b -> Iso b a t s
from l = withIso l $ \sa bt -> iso bt sa

{-# INLINE from #-}

-- | Extract the two functions, @s -> a@ and one @b -> t@ that characterize an
--   'Iso'.

withIso :: forall s t a b rep (r :: TYPE rep).
             AnIso s t a b -> ((s -> a) -> (b -> t) -> r) -> r
withIso ai k = case ai (Exchange id Identity) of
    Exchange sa bt -> k sa (runIdentity #. bt)

{-# INLINE withIso #-}

{- |
Lawful instances of 'Show' and 'Read' give rise to this isomorphism.

@
>>> 123 & from _Show %~ reverse
321
>>> "123" & _Show %~ (*2)
"246"
@
-}
_Show :: (Read a, Show a) => Iso' String a
_Show = iso read show

{-# INLINE _Show #-}

{- |
'enum' is a questionable inclusion, as many (most) 'Enum' instances throw
errors for out-of-bounds integers, but it is occasionally useful when used with
that information in mind. Handle with care!

>>> 97 ^. enum :: Char
'a'
>>> (-1) ^. enum :: Char
*** Exception: Prelude.chr: bad argument: (-1)
>>> [True,False] ^. mapping (from enum)
[1,0]
-}

enum :: (Enum a) => Iso' Int a
enum = iso toEnum fromEnum

{-# INLINE enum #-}

{- |
'non' lets you “relabel” a 'Maybe' by equating 'Nothing' to an arbitrary value
(which you can choose):

>>> Just 1 ^. non 0 1

>>> Nothing ^. non 0 0

The most useful thing about 'non' is that relabeling also works in other
direction. If you try to 'set' the “forbidden” value, it'll be turned to
'Nothing':

>>> Just 1 & non 0 .~ 0 Nothing

Setting anything else works just fine:

>>> Just 1 & non 0 .~ 5 Just 5

Same happens if you try to modify a value:

>>> Just 1 & non 0 %~ subtract 1 Nothing

>>> Just 1 & non 0 %~ (+ 1) Just 2

'non' is often useful when combined with 'at'. For instance, if you have a map
of songs and their playcounts, it makes sense not to store songs with 0 plays in
the map; 'non' can act as a filter that wouldn't pass such entries.

Decrease playcount of a song to 0, and it'll be gone:

>>> fromList [("Soon",1),("Yesterday",3)] & at "Soon" . non 0 %~ subtract 1
fromList [("Yesterday",3)]

Try to add a song with 0 plays, and it won't be added:

>>> fromList [("Yesterday",3)] & at "Soon" . non 0 .~ 0
fromList [("Yesterday",3)]

But it will be added if you set any other number:

>>> fromList [("Yesterday",3)] & at "Soon" . non 0 .~ 1
fromList [("Soon",1),("Yesterday",3)]

'non' is also useful when working with nested maps. Here a nested map is created
when it's missing:

>>> Map.empty & at "Dez Mona" . non Map.empty . at "Soon" .~ Just 1
fromList [("Dez Mona",fromList [("Soon",1)])]

and here it is deleted when its last entry is deleted (notice that 'non' is used
twice here):

>>> fromList [("Dez Mona",fromList [("Soon",1)])] & at "Dez Mona" . non Map.empty . at "Soon" . non 0 %~ subtract 1
fromList []

To understand the last example better, observe the flow of values in it:

* the map goes into @at \"Dez Mona\"@ * the nested map (wrapped into @Just@)
goes into @non Map.empty@ * @Just@ is unwrapped and the nested map goes into @at
\"Soon\"@ * @Just 1@ is unwrapped by @non 0@

Then the final value – i.e. 1 – is modified by @subtract 1@ and the result
(which is 0) starts flowing backwards:

* @non 0@ sees the 0 and produces a @Nothing@

* @at \"Soon\"@ sees @Nothing@ and deletes the corresponding value from the map

* the resulting empty map is passed to @non Map.empty@, which sees that it's
empty and thus produces @Nothing@

* @at \"Dez Mona\"@ sees @Nothing@ and removes the key from the map
-}

non :: (Eq a) => a -> Iso' (Maybe a) a
non a = non' $ only a

{-# INLINE non #-}

non' :: APrism' a () -> Iso' (Maybe a) a
non' p = iso (fromMaybe def) go where
  def                           = review (clonePrism p) ()
  go b | has (clonePrism p) b   = Nothing
       | otherwise              = Just b

{-# INLINE non' #-}

{- |
Coercible types have the same runtime representation, i.e. they are isomorphic.

>>> (Sum 123 :: Sum Int) ^. coerced :: Int
123
-}

coerced :: forall s t a b. (Coercible s a, Coercible t b) => Iso s t a b
coerced l = rmap (fmap coerce) l .# coerce

{-# INLINE coerced #-}

{- |
An isomorphism holds when lifted into a functor. For example, if a list contains
a bunch of @a@'s which are each isomorphic to a @b@, the whole list of @a@'s is
isomorphic to a list of @b@'s.

>>> ["1","2","3"] ^. mapping _Show :: [Int]
[1,2,3]
>>> ([1,2,3] :: [Int]) ^. from (mapping _Show)
["1","2","3"]

This also hold across different functors:

>>> let l = mapping @[] @Maybe _Show
>>> :t l
l :: (Read b, Show b) => Iso [String] (Maybe String) [b] (Maybe b)
>>> ["1","2","3"] & l %~ Just . sum
Just "6"
-}

mapping :: (Functor f, Functor g) => AnIso s t a b -> Iso (f s) (g t) (f a) (g b)
mapping k = withIso k $ \ sa bt -> iso (fmap sa) (fmap bt)

{-# INLINE mapping #-}

--------------------------------------------------------------------------------


{- $prisms-note

If a 'Lens' views and updates individual components of /product/ types, a
'Prism' views and updates individual components of /sum/ types. For example, you
may want to update the 'Left' field of an 'Either':

>>> Left "salmon" & _Left .~ "orb"
Left "orb"
>>> Right "pudding" & _Left .~ "orb"
Right "pudding"

Also similarly to a 'Lens', you might want to view the 'Left' field. However, it
might not always be there, so we treat it as a traversal with either one or zero
results.

>>> Right "bass" ^? _Left
Nothing
>>> Left "bubbles" ^? _Left
Just "bubbles"

A unique feature of 'Prism's is that they may be flipped around using 're' to
construct the larger structure. Maintaining our example of 'Either', remember
that you can construct the entire 'Either' via the constructor 'Left'.

>>> :t re _Left
re _Left :: Getter b (Either b c)
>>> view (re _Left) "bungo"
Left "bungo"

This @'view' ('re' l)@ idiom isn't the prettiest, so we define @'review' =
'view' . 're'@ as shorthand. 'review' also has an infix synonym, '(#)'.

>>> :t _Just
_Just :: Prism (Maybe a) (Maybe b) a b
>>> review _Just "bilbo"
Just "bilbo"
>>> _Just # "bilbo"
Just "bilbo"

As is the whole point of optics, prisms may of course be composed with other
optics:

@
type Thing = Either (Maybe String) (Maybe (Either [Bool] Int))
thing :: Thing
thing = Right (Just (Left [True,False]))
@
>>> thing & _Right . _Just . _Left . each %~ not
Right (Just (Left [False,True]))

-}

{- |
Generate a 'Prism' out of a constructor and a selector. You may initially wonder
why the selector function returns an 'Either t a' rather than the more obvious
choice of 'Maybe a'. This is to allow @s@ and @t@ to differ — see 'prism''.

@
_Left = prism Left $ either Right (Left . Right)
@
-}

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap seta (either pure (fmap bt)) . right'

{-# INLINE prism #-}

{- |
Generate a 'Prism' out of a constructor and a selector.

@
_Nothing = prism Left $ either Right (Left . Right)
@
-}

prism' :: (b -> s) -> (s -> Maybe a) -> Prism s s a b
prism' bs sma = prism bs (\s -> maybe (Left s) Right (sma s))

{-# INLINE prism' #-}

{- |
Clone a Prism so that you can reuse the same monomorphically typed Prism for
different purposes.
Cloning a 'Prism' is one way to make sure you aren't given something weaker,
such as a 'Traversal' and can be used as a way to pass around lenses that have
to be monomorphic in f.
-}

clonePrism :: APrism s t a b -> Prism s t a b
clonePrism k = withPrism k $ \bt sta -> prism bt sta

{-# INLINE clonePrism #-}

{- |
Convert a 'Prism' into the constructor and selector that characterise it. See:
'prism'.
-}
withPrism :: APrism s t a b -> ((b -> t) -> (s -> Either t a) -> r) -> r
withPrism k f = case coerce (k (Market Identity Right)) of
    Market bt seta -> f bt seta

{-# INLINE withPrism #-}

{- |
Focus the 'Just' of a 'Maybe'. This might seem redundant, as:

>>> Just "pikyben" ^? _Just
Just "pikyben"

but '_Just' proves useful when composing with other optics.
-}

_Just :: Prism (Maybe a) (Maybe b) a b
_Just = prism Just $ maybe (Left Nothing) Right

{-# INLINE _Just #-}

{- |
'_Nothing' focuses the 'Nothing' in a 'Maybe'.

>>> Nothing ^? _Nothing 
Just ()
>>> Just "wassa" ^? _Nothing 
Nothing
>>> 'has' _Nothing (Just "something")
False
-}
_Nothing :: Prism' (Maybe a) ()
_Nothing = prism' (const Nothing) $ maybe (Just ()) (const Nothing)

{-# INLINE _Nothing #-}

{- |
Focus the 'Left' component of an 'Either'

>>> Left "doge" ^? _Left
Just "doge"
>>> Right "soge" ^? _Left
Nothing
>>> review _Left "quoge"
Left "quoge"
-}

_Left :: Prism (Either a c) (Either b c) a b
_Left = prism Left $ either Right (Left . Right)

{-# INLINE _Left #-}

{- |
Focus the 'Right' component of an 'Either'

>>> Left "doge" ^? _Right
Nothing
>>> Right "soge" ^? _Right
Just "soge"
>>> review _Right "quoge"
Right "quoge"
-}

_Right :: Prism (Either c a) (Either c b) a b
_Right = prism Right $ either (Left . Left) Right

{-# INLINE _Right #-}

class AsEmpty a where
    _Empty :: Prism' a ()
    default _Empty :: (Monoid a, Eq a) => Prism' a ()
    _Empty = only mempty
    {-# INLINE _Empty #-}

instance AsEmpty [a] where
    _Empty = nearly [] null
    {-# INLINE _Empty #-}

instance AsEmpty (Data.Map.Map k v) where
    _Empty = nearly Data.Map.empty Data.Map.null
    {-# INLINE _Empty #-}

instance AsEmpty (Maybe a) where
    _Empty = _Nothing
    {-# INLINE _Empty #-}

instance AsEmpty (Data.HashMap.Strict.HashMap k v) where
    _Empty = nearly Data.HashMap.Strict.empty Data.HashMap.Strict.null
    {-# INLINE _Empty #-}

instance AsEmpty (Data.Vector.Vector a) where
    _Empty = nearly Data.Vector.empty Data.Vector.null
    {-# INLINE _Empty #-}

instance AsEmpty Data.Text.Text where
    _Empty = nearly Data.Text.empty Data.Text.null
    {-# INLINE _Empty #-}

instance AsEmpty Data.Text.Lazy.Text where
    _Empty = nearly Data.Text.Lazy.empty Data.Text.Lazy.null
    {-# INLINE _Empty #-}

only :: Eq a => a -> Prism' a ()
only a = prism' (\() -> a) $ guard . (a ==)

{-# INLINE only #-}

{- |
>>> ["something"] ^? nearly [] null
Nothing
>>> [] ^? nearly [] null
Just ()

>>> ["one","two"] ^? nearly [] (even . length)
Just ()
>>> ["one","two","three"] ^? nearly [] (even . length)
Nothing
-}

nearly :: a -> (a -> Bool) -> Prism' a ()
nearly a p = prism' (\() -> a) $ guard . p

{-# INLINE nearly #-}

{- |
If you see this in a signature for a function, the function is expecting a
Review. This usually means a 'Prism' or an 'Iso'.
-}
type AReview t b = Tagged b (Identity b) -> Tagged t (Identity t)

{- |
[@Review@](https://hackage.haskell.org/package/lens-5.2.3/docs/Control-Lens-Type.html#t:Review),
from lens, is limited form of 'Prism' that can only be used for 're' operations.

Similarly to 'SimpleGetter' from microlens, microlens-pro does not define 'Review' and opts for
a less general 'SimplerReview' in order to avoid a
[distributive](https://hackage.haskell.org/package/distributive-0.6.2.1)
dependency.
-}

type SimpleReview t b = forall p. (Choice p, Bifunctor p)
                     => p b (Identity b) -> p t (Identity t)

{-|
Reverse a 'Prism' or 'Iso' and 'view' it

@
review ≡ view . re
@

@
>>> review _Just "sploink"
Just "sploink"
@

'review' is often used with the function monad, @((->)r)@:

@
review :: AReview t b -> b -> t
@
-}

review :: MonadReader b m => AReview t b -> m t
review p = asks (runIdentity #. unTagged #. p .# Tagged .# Identity)

{-# INLINE review #-}

{-|
Reverse a 'Prism' or 'Iso' turning it into a getter. 're' is a weaker version of
'from', in that you can't flip it back around after reversing it the first time.

>>> "hello worms" ^. re _Just
Just "hello worms"
-}

re :: AReview t b -> Getter b t
re p = to (runIdentity #. unTagged #. p .# Tagged .# Identity)

{-# INLINE re #-}

-- | An infix synonym of 'review'
(#) :: AReview t b -> b -> t
(#) p = runIdentity #. unTagged #. p .# Tagged .# Identity

infixr 8 #
{-# INLINE (#) #-}

-- TODO: `to` is temporarily defined here. This should be in microlens-contra,
-- or better yet, microlens as Contravariant has been in base since at least ghc
-- 8.6.5. This definition isn't perfect either -- the version from lens is:
--
-- to :: (Profunctor p, Contravariant f) => (s -> a) -> Optic' p f s a

to :: (s -> a) -> Getter s a
to k = dimap k (contramap k)

{-# INLINE to #-}

unto :: (Profunctor p, Bifunctor p, Functor f)
     => (b -> t)
     -> p a (f b) -> p s (f t)
unto f = first absurd . lmap absurd . rmap (fmap f)

{-# INLINE unto #-}

