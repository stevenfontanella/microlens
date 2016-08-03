{-# LANGUAGE
CPP,
MultiParamTypeClasses,
FunctionalDependencies,
FlexibleInstances,
UndecidableInstances,
TypeFamilies,
Trustworthy
  #-}

-- This is needed because ErrorT is deprecated.
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}


{- |
Module      :  Lens.Micro.Mtl
Copyright   :  (C) 2013-2016 Edward Kmett, 2015-2016 Artyom
License     :  BSD-style (see the file LICENSE)
-}
module Lens.Micro.Mtl
(
  -- * Getting
  view, preview,
  use, preuse,

  -- * Setting
  (%=), modifying,
  (.=), assign,
  (?=),
  (<~),

  -- * Specialised modifying operators
  -- $arith-note
  (+=), (-=), (*=), (//=),

  -- * Setting with passthrough
  (<%=), (<.=), (<?=),
  (<<%=), (<<.=),

  -- * Zooming
  zoom,
  magnify,
)
where


import Control.Applicative
import Data.Monoid

import Control.Monad.Reader as Reader
import Control.Monad.State as State
-- microlens
import Lens.Micro
import Lens.Micro.Internal
-- Internal modules
import Lens.Micro.Mtl.Internal


{- |
'view' is a synonym for ('^.'), generalised for 'MonadReader' (we are able to use it instead of ('^.') since functions are instances of the 'MonadReader' class):

>>> view _1 (1, 2)
1

When you're using 'Reader.Reader' for config and your config type has lenses generated for it, most of the time you'll be using 'view' instead of 'Reader.asks':

@
doSomething :: ('MonadReader' Config m) => m Int
doSomething = do
  thingy        <- 'view' setting1  -- same as “'Reader.asks' ('^.' setting1)”
  anotherThingy <- 'view' setting2
  ...
@
-}
view :: MonadReader s m => Getting a s a -> m a
view l = Reader.asks (getConst #. l Const)
{-# INLINE view #-}

{- |
'preview' is a synonym for ('^?'), generalised for 'MonadReader' (just like 'view', which is a synonym for ('^.')).

>>> preview each [1..5]
Just 1
-}
preview :: MonadReader s m => Getting (First a) s a -> m (Maybe a)
preview l = Reader.asks (getFirst #. foldMapOf l (First #. Just))
{-# INLINE preview #-}

{- |
'use' is ('^.') (or 'view') which implicitly operates on the state; for instance, if your state is a record containing a field @foo@, you can write

@
x \<- 'use' foo
@

to extract @foo@ from the state. In other words, 'use' is the same as 'State.gets', but for getters instead of functions.

The implementation of 'use' is straightforward:

@
'use' l = 'State.gets' ('view' l)
@

If you need to extract something with a fold or traversal, you need 'preuse'.
-}
use :: MonadState s m => Getting a s a -> m a
use l = State.gets (view l)
{-# INLINE use #-}

{- |
'preuse' is ('^?') (or 'preview') which implicitly operates on the state – it takes the state and applies a traversal (or fold) to it to extract the 1st element the traversal points at.

@
'preuse' l = 'State.gets' ('preview' l)
@
-}
preuse :: MonadState s m => Getting (First a) s a -> m (Maybe a)
preuse l = State.gets (preview l)
{-# INLINE preuse #-}


infix  4 .=, %=, ?=
infix  4 <<.=, <<%=, <%=, <.=, <?=
infix  4 +=, -=, *=, //=
infixr 2 <~

{- |
Modify state by “assigning” a value to a part of the state.

This is merely ('.~') which works in 'MonadState':

@
l '.=' x = 'State.modify' (l '.~' x)
@

If you also want to know the value that was replaced by ('.='), use ('<<.=').
-}
(.=) :: MonadState s m => ASetter s s a b -> b -> m ()
l .= x = State.modify (l .~ x)
{-# INLINE (.=) #-}

{- |
A synonym for ('.=').
-}
assign :: MonadState s m => ASetter s s a b -> b -> m ()
assign l x = l .= x
{-# INLINE assign #-}

{- |
('?=') is a version of ('.=') that wraps the value into 'Just' before setting.

@
l '?=' b = l '.=' Just b
@

It can be useful in combination with 'at'.
-}
(?=) :: MonadState s m => ASetter s s a (Maybe b) -> b -> m ()
l ?= b = l .= Just b
{-# INLINE (?=) #-}

{- |
('<~') is a version of ('.=') that takes a monadic value (and then executes it and assigns the result to the lens).

@
l '<~' mb = do
  b <- mb
  l '.=' b
@
-}
(<~) :: MonadState s m => ASetter s s a b -> m b -> m ()
l <~ mb = mb >>= (l .=)
{-# INLINE (<~) #-}

{- |
Modify state by applying a function to a part of the state. An example:

>>> execState (do _1 %= (+1); _2 %= reverse) (1,"hello")
(2,"olleh")

Implementation:

@
l '%=' f = 'State.modify' (l '%~' f)
@

If you also want to get the value before\/after the modification, use ('<<%=')\/('<%=').

There are a few specialised versions of ('%=') which mimic C operators:

* ('+=') for addition
* ('-=') for substraction
* ('*=') for multiplication
* ('//=') for division
-}
(%=) :: (MonadState s m) => ASetter s s a b -> (a -> b) -> m ()
l %= f = State.modify (l %~ f)
{-# INLINE (%=) #-}

{- |
A synonym for ('%=').
-}
modifying :: (MonadState s m) => ASetter s s a b -> (a -> b) -> m ()
modifying l f = l %= f
{-# INLINE modifying #-}

{- $arith-note

The following operators mimic well-known C operators ('+=', '-=', etc). ('//=') stands for division.

They're implemented like this:

@
l '+=' x = l '%=' (+x)
l '-=' x = l '%=' ('subtract' x)
...
@
-}

(+=) :: (MonadState s m, Num a) => ASetter s s a a -> a -> m ()
l += x = l %= (+x)
{-# INLINE (+=) #-}

(-=) :: (MonadState s m, Num a) => ASetter s s a a -> a -> m ()
l -= x = l %= (subtract x)
{-# INLINE (-=) #-}

(*=) :: (MonadState s m, Num a) => ASetter s s a a -> a -> m ()
l *= x = l %= (*x)
{-# INLINE (*=) #-}

(//=) :: (MonadState s m, Fractional a) => ASetter s s a a -> a -> m ()
l //= x = l %= (/x)
{-# INLINE (//=) #-}

{- |
Modify state and return the modified (new) value.

@
l '<%=' f = do
  l '%=' f
  'use' l
@
-}
(<%=) :: MonadState s m => LensLike ((,) b) s s a b -> (a -> b) -> m b
l <%= f = l %%= (\a -> (a, a)) . f
{-# INLINE (<%=) #-}

{- |
Modify state and return the old value (i.e. as it was before the modificaton).

@
l '<<%=' f = do
  old <- 'use' l
  l '%=' f
  return old
@
-}
(<<%=) :: MonadState s m => LensLike ((,) a) s s a b -> (a -> b) -> m a
l <<%= f = l %%= (\a -> (a, f a))
{-# INLINE (<<%=) #-}

{- |
Set state and return the old value.

@
l '<<.=' b = do
  old <- 'use' l
  l '.=' b
  return old
@
-}
(<<.=) :: MonadState s m => LensLike ((,) a) s s a b -> b -> m a
l <<.= b = l %%= (\a -> (a, b))
{-# INLINE (<<.=) #-}

{- |
Set state and return new value.

@
l '<.=' b = do
  l '.=' b
  return b
@
-}
(<.=) :: MonadState s m => LensLike ((,) b) s s a b -> b -> m b
l <.= b = l <%= const b
{-# INLINE (<.=) #-}

{- |
('<?=') is a version of ('<.=') that wraps the value into 'Just' before setting.

@
l '<?=' b = do
  l '.=' Just b
  'return' b
@

It can be useful in combination with 'at'.
-}
(<?=) :: MonadState s m => LensLike ((,) b) s s a (Maybe b) -> b -> m b
l <?= b = l %%= const (b, Just b)
{-# INLINE (<?=) #-}

(%%=) :: MonadState s m => LensLike ((,) r) s s a b -> (a -> (r, b)) -> m r
#if MIN_VERSION_mtl(2,1,1)
l %%= f = State.state (l f)
#else
l %%= f = do
  (r, s) <- State.gets (l f)
  State.put s
  return r
#endif
{-# INLINE (%%=) #-}

infix 4 %%=
