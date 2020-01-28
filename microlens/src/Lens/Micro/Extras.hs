{-# LANGUAGE Trustworthy #-}


{- |
Module      :  Lens.Micro.Extras
Copyright   :  (C) 2013-2016 Edward Kmett, 2015-2016 Artyom Kazak, 2018 Monadfix
License     :  BSD-style (see the file LICENSE)
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

The reason it's not in "Lens.Micro" is that @view@ in lens has a more general signature:

@
view :: MonadReader s m => Getting a s a -> m a
@

So, you would be able to use this 'view' with functions, but not in various reader monads. For most people this shouldn't be an issue; if it is for you, use @view@ from <https://hackage.haskell.org/package/microlens-mtl microlens-mtl>.
-}
view :: Getting a s a -> s -> a
view l = getConst #. l Const
{-# INLINE view #-}

{- |
'preview' is a synonym for ('^?'):

>>> preview _head [1,2,3]
Just 1

The reason it's not in "Lens.Micro" is that @preview@ in lens has a more general signature:

@
preview :: MonadReader s m => Getting (First a) s a -> m (Maybe a)
@

Just like with 'view', you would be able to use this 'preview' with functions, but not in reader monads; if this is an issue for you, use @preview@ from <https://hackage.haskell.org/package/microlens-mtl microlens-mtl>.
-}
preview :: Getting (First a) s a -> s -> Maybe a
preview l = getFirst #. foldMapOf l (First #. Just)
{-# INLINE preview #-}
