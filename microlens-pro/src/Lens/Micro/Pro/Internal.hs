{-|
Module      : Lens.Micro.Pro.Internal
Copyright   : (C) 2013-2016 Edward Kmett, 2018 Monadfix
License     : BSD-style (see the file LICENSE)

Definitions used internally by microlens. If you're going to use these, only do
so for your own types!
-}
{-# LANGUAGE FunctionalDependencies #-}
module Lens.Micro.Pro.Internal
    ( Strict(strict, lazy)

    , IsText(packed, unpacked)

    , Exchange(..), Exchange'
    , Market(..), Market'

    , Iso, Iso'
    , Prism, Prism'
    )
    where
--------------------------------------------------------------------------------
import Lens.Micro.Pro.Type
import Data.Coerce
import Data.Profunctor
import Data.Profunctor.Unsafe
--------------------------------------------------------------------------------

class Strict lazy strict | lazy -> strict, strict -> lazy where
    strict :: Iso' lazy   strict
    lazy   :: Iso' strict lazy

-- | This type is used internally by the 'Iso' code to provide efficient access
--   to the two parts of an Iso.
data Exchange a b s t = Exchange (s -> a) (b -> t)

type Exchange' a s = Exchange a a s s

instance Functor (Exchange a b s) where
    fmap f (Exchange sa bt) = Exchange sa (f . bt)
    {-# INLINE fmap #-}

instance Profunctor (Exchange a b) where
    dimap f g (Exchange sa bt) = Exchange (sa . f) (g . bt)
    lmap f (Exchange sa bt) = Exchange (sa . f) bt
    rmap f (Exchange sa bt) = Exchange sa (f . bt)

    {-# INLINE dimap #-}
    {-# INLINE lmap #-}
    {-# INLINE rmap #-}

    (#.) _ = coerce
    (.#) p _ = coerce p

    {-# INLINE (#.) #-}
    {-# INLINE (.#) #-}

-- | This type is used internally by the Prism code to provide efficient access
--   to the two parts of a Prism.
data Market a b s t = Market (b -> t) (s -> Either t a)

instance Functor (Market a b s) where
  fmap f (Market bt seta) = Market (f . bt) (either (Left . f) Right . seta)
  {-# INLINE fmap #-}

instance Profunctor (Market a b) where
    dimap f g (Market bt seta) =
        Market (g . bt) (either (Left . g) Right . seta . f)

    lmap f (Market bt seta) = Market bt (seta . f)
    rmap f (Market bt seta) = Market (f . bt) (either (Left . f) Right . seta)

    {-# INLINE rmap #-}
    {-# INLINE lmap #-}
    {-# INLINE dimap #-}

    (#.) _ = coerce
    (.#) p _ = coerce p

    {-# INLINE (#.) #-}
    {-# INLINE (.#) #-}

instance Choice (Market a b) where
    left' (Market bt seta) = Market (Left . bt) $ \sc -> case sc of
        Left s -> case seta s of
            Left t -> Left (Left t)
            Right a -> Right a
        Right c -> Left (Right c)

    right' (Market bt seta) = Market (Right . bt) $ \cs -> case cs of
        Left c -> Left (Left c)
        Right s -> case seta s of
            Left t -> Left (Right t)
            Right a -> Right a

    {-# INLINE right' #-}
    {-# INLINE left' #-}

type Market' a s = Market a a s s

-- TODO: IsText is up for discussion

class IsText t where
    packed   :: Iso' String t
    unpacked :: Iso' t String

