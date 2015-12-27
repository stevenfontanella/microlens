{-# LANGUAGE
RankNTypes,
FlexibleContexts,
Trustworthy
  #-}


{- |
This module provides functions and types that are inferior to their lens counterparts.
-}
module Lens.Micro.Extras
(
  view,
  preview,
)
where


import Lens.Micro
import Lens.Micro.Internal

import Control.Applicative
import Data.Monoid


{- |
'view' is a synonym for ('^.'):

>>> view _1 (1, 2)
1

The reason it's in this module is that @view@ in lens has a more general signature:

@
view :: MonadReader s m => Getting a s a -> m a
@

So, you would be able to use this 'view' with functions, but not in various reader monads. For most people this shouldn't be an issue.
-}
view :: Getting a s a -> s -> a
view l = getConst #. l Const
{-# INLINE view #-}

{- |
'preview' is a synonym for ('^?'):

>>> preview _head [1,2,3]
Just 1

The reason it's in this module is that @preview@ in lens has a more general signature:

@
preview :: MonadReader s m => Getting (First a) s a -> m (Maybe a)
@

So, just like with 'view', you would be able to use this 'preview' with functions, but not in reader monads.
-}
preview :: Getting (First a) s a -> s -> Maybe a
preview l = getFirst #. foldMapOf l (First #. Just)
{-# INLINE preview #-}
