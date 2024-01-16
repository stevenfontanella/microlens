{-# LANGUAGE DefaultSignatures #-}
module Lens.Micro.Pro
    (
    -- * Iso: Losslessly convert between types
      Iso, Iso'
    , iso
    , from
    , strict, lazy
    , _Show
    , enum
    , anon, non, non'
    , coerced
    , AnIso, AnIso'
    , cloneIso
    , withIso

    -- * Prism: A traversal targeting exactly one or zero values
    , Prism, Prism'
    , prism, prism'
    , _Left, _Right
    , _Just, _Nothing
    , _Empty
    , only
    , APrism, APrism'
    , clonePrism
    , withPrism

    -- * Review
    , AReview
    , review
    , (#)
    , re
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
import Data.Profunctor
import Data.Profunctor.Unsafe
import GHC.Exts                     (TYPE)
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

_Show :: (Read a, Show a) => Iso' String a
_Show = iso read show

enum :: (Enum a) => Iso' Int a
enum = iso toEnum fromEnum

anon :: a -> (a -> Bool) -> Iso' (Maybe a) a
anon a p = iso (fromMaybe a) go where
  go b | p b       = Nothing
       | otherwise = Just b

{-# INLINE anon #-}

non :: (Eq a) => a -> Iso' (Maybe a) a
non a = non' $ only a

{-# INLINE non #-}

non' :: APrism' a () -> Iso' (Maybe a) a
non' p = iso (fromMaybe def) go where
  def                           = review (clonePrism p) ()
  go b | has (clonePrism p) b   = Nothing
       | otherwise              = Just b

{-# INLINE non' #-}

coerced :: forall s t a b. (Coercible s a, Coercible t b) => Iso s t a b
coerced l = rmap (fmap coerce) l .# coerce

{-# INLINE coerced #-}

--------------------------------------------------------------------------------

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap seta (either pure (fmap bt)) . right'

{-# INLINE prism #-}

prism' :: (b -> s) -> (s -> Maybe a) -> Prism s s a b
prism' bs sma = prism bs (\s -> maybe (Left s) Right (sma s))

{-# INLINE prism' #-}

clonePrism :: APrism s t a b -> Prism s t a b
clonePrism k = withPrism k $ \bt sta -> prism bt sta

{-# INLINE clonePrism #-}

withPrism :: APrism s t a b -> ((b -> t) -> (s -> Either t a) -> r) -> r
withPrism k f = case coerce (k (Market Identity Right)) of
    Market bt seta -> f bt seta

{-# INLINE withPrism #-}

_Just :: Prism (Maybe a) (Maybe b) a b
_Just = prism Just $ maybe (Left Nothing) Right

{-# INLINE _Just #-}

_Nothing :: Prism' (Maybe a) ()
_Nothing = prism' (const Nothing) $ maybe (Just ()) (const Nothing)

{-# INLINE _Nothing #-}

_Left :: Prism (Either a c) (Either b c) a b
_Left = prism Left $ either Right (Left . Right)

{-# INLINE _Left #-}

_Right :: Prism (Either c a) (Either c b) a b
_Right = prism Right $ either (Left . Left) Right

{-# INLINE _Right #-}

class AsEmpty a where
    _Empty :: Prism' a ()
    default _Empty :: (Monoid a, Eq a) => Prism' a ()
    _Empty = only mempty
    {-# INLINE _Empty #-}

only :: Eq a => a -> Prism' a ()
only a = prism' (\() -> a) $ guard . (a ==)

{-# INLINE only #-}

type AReview t b = Tagged b (Identity b) -> Tagged t (Identity t)

review :: MonadReader b m => AReview t b -> m t
review p = asks (runIdentity #. unTagged #. p .# Tagged .# Identity)

{-# INLINE review #-}

re :: AReview t b -> Getter b t
re p = to (runIdentity #. unTagged #. p .# Tagged .# Identity)

{-# INLINE re #-}

-- | An infix synonym for 'review'
(#) :: AReview t b -> b -> t
(#) p = runIdentity #. unTagged #. p .# Tagged .# Identity
{-# INLINE (#) #-}

-- TODO: `to` is temporarily defined here. This should be in microlens-contra,
-- or better yet, microlens as Contravariant has been in base since at least ghc
-- 8.6.5

to :: (s -> a) -> Getter s a
to k = dimap k (contramap k)

{-# INLINE to #-}

