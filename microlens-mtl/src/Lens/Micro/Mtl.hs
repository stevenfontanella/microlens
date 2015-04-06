module Lens.Micro.Mtl
(
  view,
  use,
  (.=), (%=),
  (+=), (-=), (*=), (//=),
)
where


import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Lens.Micro
import Lens.Micro.Extras


{- |
'view' is a synonym for ('^.'), generalised for 'MonadReader' (since
functions are instances of the 'MonadReader' class).
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


infix  4 .=, %=
infix  4 +=, -=, *=, //=

-- |
-- Assign value to the target. This is '.~' which works in 'State'.
--
-- @
-- l '.=' b = 'modify' (l '.~' b)
-- @
(.=) :: MonadState s m => ASetter s s a b -> b -> m ()
l .= b = modify (l .~ b)
{-# INLINE (.=) #-}

-- |
-- Apply a function to the target. This is '%~' which works in 'State'.
--
-- >>> execState (do _1 %= (+1); _2 %= reverse) (1,"hello")
-- (2,"olleh")
--
(%=) :: (MonadState s m) => ASetter s s a b -> (a -> b) -> m ()
l %= f = modify (l %~ f)
{-# INLINE (%=) #-}

-- |
-- Add a number to the target.
--
-- @
-- l '+=' x = l '%=' (+x)
-- @
(+=) :: (MonadState s m, Num a) => ASetter s s a a -> a -> m ()
l += b = modify (l +~ b)
{-# INLINE (+=) #-}

-- |
-- Subtract a number from the target.
--
-- @
-- l '-=' x = l '%=' ('subtract' x)
-- @
(-=) :: (MonadState s m, Num a) => ASetter s s a a -> a -> m ()
l -= b = modify (l -~ b)
{-# INLINE (-=) #-}

-- |
-- Multiply the target by a number.
--
-- @
-- l '*=' x = l '%=' (*x)
-- @
(*=) :: (MonadState s m, Num a) => ASetter s s a a -> a -> m ()
l *= b = modify (l *~ b)
{-# INLINE (*=) #-}

-- |
-- Divide the target by a number.
--
-- @
-- l += x = l %= (+x)
-- @
(//=) :: (MonadState s m, Fractional a) => ASetter s s a a -> a -> m ()
l //= a = modify (l //~ a)
{-# INLINE (//=) #-}

