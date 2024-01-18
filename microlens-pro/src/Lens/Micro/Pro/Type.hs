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

type Iso s t a b = forall p f. (Profunctor p, Functor f)
                => p a (f b) -> p s (f t)

-- | The type of isos that change neither the type of the container, or the
-- types of anything inside it.

type Iso' s a = Iso s s a a

type Prism s t a b = forall p f. (Choice p, Applicative f)
                  => p a (f b) -> p s (f t) 

-- | The type of prisms that change neither the type of the container, or the
-- types of anything inside it.

type Prism' s a = Prism s s a a

