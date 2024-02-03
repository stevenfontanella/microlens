{-|
Module      : Lens.Micro.Pro.Type
Copyright   : (C) 2013-2016 Edward Kmett, 2018 Monadfix
License     : BSD-style (see the file LICENSE)

This module defines just the 'Iso' and 'Prism' types, in order to break a
dependency cycle. You'll find the interesting stuff in 'Lens.Micro.Pro' and
'Lens.Micro.Pro.Internal'.
-}
module Lens.Micro.Pro.Type
    ( Iso, Iso'
    , Prism, Prism'
    )
    where
--------------------------------------------------------------------------------
import Data.Profunctor
--------------------------------------------------------------------------------

{- |
The type signature of 'Lens.Micro.Pro.iso' provides a nice interpretation of
'Iso'. If you want to apply a function @a -> b@ to a type @s@, you'd have to
convert with @s -> a@, apply your function @a -> b@, and convert back with
@b -> t@.

@
'Lens.Micro.Pro.iso' :: (s -> a) -> (b -> t) -> Iso s t a b
-- or, put monomorphically
'Lens.Micro.Pro.iso' :: (s -> a) -> (a -> s) -> Iso' s a
@
-}

type Iso s t a b = forall p f. (Profunctor p, Functor f)
                => p a (f b) -> p s (f t)

{- |
The type of monomorphic isomorphisms, i.e. isos that change neither the outer type
@s@ nor the inner type @a@.
-}

type Iso' s a = Iso s s a a

{- |
* @s@ is the type of the whole structure
* @t@ is the type of the reconstructed structure
* @a@ is the type of the target
* @b@ is the type of the value used for reconstruction
-}
type Prism s t a b = forall p f. (Choice p, Applicative f)
                  => p a (f b) -> p s (f t) 

{- |
The type of monomorphic prisms, i.e. prisms that change neither the outer type
@s@ nor the inner type @a@.
-}

type Prism' s a = Prism s s a a

