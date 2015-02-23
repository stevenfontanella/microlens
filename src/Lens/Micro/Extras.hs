module Lens.Micro.Extras
(
  (.=), (%=),
  (+=), (-=), (*=), (//=),
  (+~), (-~), (*~), (//~),
)
where

import Control.Monad.State
import Lens.Micro

infix  4 .=, %=
infix  4 +=, -=, *=, //=
infixr 4 +~, -~, *~, //~

(.=) :: MonadState s m => ASetter s s a b -> b -> m ()
l .= b = modify (l .~ b)
{-# INLINE (.=) #-}

-- |
-- This is a 'State' version of ('%~'), the same way ('.=') is a 'State'
-- version of ('.~'). It modifies state using the setter and function it's
-- given.
--
-- >>> 'execState' (do _1 %= (+1); _2 %= reverse) (1,"hello")
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


(+~) :: Num a => ASetter s t a a -> a -> s -> t
l +~ n = over l (+ n)
{-# INLINE (+~) #-}

(*~) :: Num a => ASetter s t a a -> a -> s -> t
l *~ n = over l (* n)
{-# INLINE (*~) #-}

(-~) :: Num a => ASetter s t a a -> a -> s -> t
l -~ n = over l (subtract n)
{-# INLINE (-~) #-}

(//~) :: Fractional a => ASetter s t a a -> a -> s -> t
l //~ n = over l (/ n)
{-# INLINE (//~) #-}
