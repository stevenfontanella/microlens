{-# LANGUAGE
MultiParamTypeClasses,
FunctionalDependencies,
FlexibleInstances,
UndecidableInstances,
TypeFamilies
  #-}

-- This is needed because ErrorT is deprecated.
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}


module Lens.Micro.Mtl
(
  view,
  preview,
  use,
  zoom,
  magnify,
  (.=), (%=),
  (+=), (-=), (*=), (//=),
)
where


import Control.Applicative
import Data.Monoid

import Control.Monad.Reader as Reader
import Control.Monad.State as State
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.Trans.Error
import Control.Monad.Trans.Except
import Control.Monad.Trans.List
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
-- microlens
import Lens.Micro
import Lens.Micro.Internal
-- Internal modules
import Lens.Micro.Mtl.Zoom


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
'use' is 'view' which implicitly operates on the state; for instance, if your state is a record containing a field @foo@, you can write

@
x \<- 'use' foo
@

to extract @foo@ from the state. In other words, 'use' is the same as 'State.gets', but for getters instead of functions.

The implementation of 'use' is straightforward:

@
'use' l = 'State.gets' ('view' l)
@
-}
use :: MonadState s m => Getting a s a -> m a
use l = State.gets (view l)
{-# INLINE use #-}


infix  4 .=, %=
infix  4 +=, -=, *=, //=
infixr 2 `zoom`, `magnify`

{- |
Modify state by “assigning” a value to a part of the state.

This is merely ('.~') which works in 'MonadState':

@
l '.=' x = 'State.modify' (l '.~' x)
@
-}
(.=) :: MonadState s m => ASetter s s a b -> b -> m ()
l .= x = State.modify (l .~ x)
{-# INLINE (.=) #-}

{- |
Modify state by applying a function to a part of the state. An example:

>>> execState (do _1 %= (+1); _2 %= reverse) (1,"hello")
(2,"olleh")

Implementation:

@
l '%=' f = 'State.modify' (l '%~' f)
@

There are also a few specialised versions of ('%=') which mimic C operators:

* ('+=') for addition
* ('-=') for substraction
* ('*=') for multiplication
* ('//=') for division (since ('/=') is already taken)
-}
(%=) :: (MonadState s m) => ASetter s s a b -> (a -> b) -> m ()
l %= f = State.modify (l %~ f)
{-# INLINE (%=) #-}

{- |
Add a number to the target.

@
l '+=' x = l '%=' (+x)
@
-}
(+=) :: (MonadState s m, Num a) => ASetter s s a a -> a -> m ()
l += x = l %= (+x)
{-# INLINE (+=) #-}

{- |
Subtract a number from the target.

@
l '-=' x = l '%=' ('subtract' x)
@
-}
(-=) :: (MonadState s m, Num a) => ASetter s s a a -> a -> m ()
l -= x = l %= (subtract x)
{-# INLINE (-=) #-}

{- |
Multiply the target by a number.

@
l '*=' x = l '%=' (*x)
@
-}
(*=) :: (MonadState s m, Num a) => ASetter s s a a -> a -> m ()
l *= x = l %= (*x)
{-# INLINE (*=) #-}

{- |
Divide the target by a number.

@
l '//=' x = l '%=' (/x)
@
-}
(//=) :: (MonadState s m, Fractional a) => ASetter s s a a -> a -> m ()
l //= x = l %= (/x)
{-# INLINE (//=) #-}

class (Zoomed m ~ Zoomed n, MonadState s m, MonadState t n) => Zoom m n s t | m -> s, n -> t, m t -> n, n s -> m where
  {- |
When you're in a state monad, this function lets you operate on a part of your state. For instance, if your state was a record containing a @position@ field, after zooming @position@ would become your whole state (and when you modify it, the bigger structure would be modified as well).

(Your 'Lazy.State' \/ 'Lazy.StateT' or 'Lazy.RWS' \/ 'Lazy.RWST' can be anywhere in the stack, but you can't use 'zoom' with arbitrary 'MonadState' because it doesn't provide any methods to change the type of the state. See <https://github.com/ekmett/lens/issues/316 this issue> for details.)

For the sake of the example, let's define some types first:

@
data Position = Position {
  _x, _y :: Int }

data Player = Player {
  _position :: Position,
  ... }

data Game = Game {
  _player :: Player,
  _obstacles :: [Position],
  ... }

concat \<$\> mapM makeLenses [''Position, ''Player, ''Game]
@

Now, here's an action that moves the player north-east:

@
moveNE :: 'Lazy.State' Game ()
moveNE = do
  player.position.x '+=' 1
  player.position.y '+=' 1
@

With 'zoom', you can use @player.position@ to focus just on a part of the state:

@
moveNE :: 'Lazy.State' Game ()
moveNE = do
  'zoom' (player.position) $ do
    x '+=' 1
    y '+=' 1
@

You can just as well use it for retrieving things out of the state:

@
getCoords :: 'Lazy.State' Game (Int, Int)
getCoords = 'zoom' (player.position) ((,) '<$>' 'use' x '<*>' 'use' y)
@

Or more explicitly:

@
getCoords = 'zoom' (player.position) $ do
  x' <- 'use' x
  y' <- 'use' y
  return (x', y')
@

When you pass a traversal to 'zoom', it'll work as a loop. For instance, here we move all obstacles:

@
moveObstaclesNE :: 'Lazy.State' Game ()
moveObstaclesNE = do
  'zoom' (obstacles.each) $ do
    x '+=' 1
    y '+=' 1
@

If the action returns a result, all results would be combined with '<>' – the same way they're combined when '^.' is passed a traversal. In this example, @moveObstaclesNE@ returns a list of old coordinates of obstacles in addition to moving them:

@
moveObstaclesNE = do
  xys <- 'zoom' (obstacles.each) $ do
    -- Get old coordinates.
    x' <- 'use' x
    y' <- 'use' y
    -- Update them.
    x '.=' x' + 1
    y '.=' y' + 1
    -- Return a single-element list with old coordinates.
    return [(x', y')]
  ...
@
  -}
  zoom :: ((s -> Zoomed m c s) -> t -> Zoomed m c t) -> m c -> n c

instance Monad z => Zoom (Strict.StateT s z) (Strict.StateT t z) s t where
  zoom l (Strict.StateT m) = Strict.StateT $ unfocusing #. l (Focusing #. m)
  {-# INLINE zoom #-}

instance Monad z => Zoom (Lazy.StateT s z) (Lazy.StateT t z) s t where
  zoom l (Lazy.StateT m) = Lazy.StateT $ unfocusing #. l (Focusing #. m)
  {-# INLINE zoom #-}

instance Zoom m n s t => Zoom (ReaderT e m) (ReaderT e n) s t where
  zoom l (ReaderT m) = ReaderT (zoom l . m)
  {-# INLINE zoom #-}

instance Zoom m n s t => Zoom (IdentityT m) (IdentityT n) s t where
  zoom l (IdentityT m) = IdentityT (zoom l m)
  {-# INLINE zoom #-}

instance (Monoid w, Monad z) => Zoom (Strict.RWST r w s z) (Strict.RWST r w t z) s t where
  zoom l (Strict.RWST m) = Strict.RWST $ \r -> unfocusingWith #. l (FocusingWith #. m r)
  {-# INLINE zoom #-}

instance (Monoid w, Monad z) => Zoom (Lazy.RWST r w s z) (Lazy.RWST r w t z) s t where
  zoom l (Lazy.RWST m) = Lazy.RWST $ \r -> unfocusingWith #. l (FocusingWith #. m r)
  {-# INLINE zoom #-}

instance (Monoid w, Zoom m n s t) => Zoom (Strict.WriterT w m) (Strict.WriterT w n) s t where
  zoom l = Strict.WriterT . zoom (\afb -> unfocusingPlus #. l (FocusingPlus #. afb)) . Strict.runWriterT
  {-# INLINE zoom #-}

instance (Monoid w, Zoom m n s t) => Zoom (Lazy.WriterT w m) (Lazy.WriterT w n) s t where
  zoom l = Lazy.WriterT . zoom (\afb -> unfocusingPlus #. l (FocusingPlus #. afb)) . Lazy.runWriterT
  {-# INLINE zoom #-}

instance Zoom m n s t => Zoom (ListT m) (ListT n) s t where
  zoom l = ListT . zoom (\afb -> unfocusingOn . l (FocusingOn . afb)) . runListT
  {-# INLINE zoom #-}

instance Zoom m n s t => Zoom (MaybeT m) (MaybeT n) s t where
  zoom l = MaybeT . liftM getMay . zoom (\afb -> unfocusingMay #. l (FocusingMay #. afb)) . liftM May . runMaybeT
  {-# INLINE zoom #-}

instance (Error e, Zoom m n s t) => Zoom (ErrorT e m) (ErrorT e n) s t where
  zoom l = ErrorT . liftM getErr . zoom (\afb -> unfocusingErr #. l (FocusingErr #. afb)) . liftM Err . runErrorT
  {-# INLINE zoom #-}

instance Zoom m n s t => Zoom (ExceptT e m) (ExceptT e n) s t where
  zoom l = ExceptT . liftM getErr . zoom (\afb -> unfocusingErr #. l (FocusingErr #. afb)) . liftM Err . runExceptT
  {-# INLINE zoom #-}

-- TODO: instance Zoom m m a a => Zoom (ContT r m) (ContT r m) a a where

class (Magnified m ~ Magnified n, MonadReader b m, MonadReader a n) => Magnify m n b a | m -> b, n -> a, m a -> n, n b -> m where
  {- |
This is an equivalent of 'Reader.local' which lets you apply a getter to your environment instead of merely applying a function (and it also lets you change the type of the environment).

@
'Reader.local'   :: (r -> r)   -> 'Reader.Reader' r a -> 'Reader.Reader' r a
'magnify' :: Getter r x -> 'Reader.Reader' x a -> 'Reader.Reader' r a
@

'magnify' works with 'Reader.Reader' \/ 'Reader.ReaderT', 'Lazy.RWS' \/ 'Lazy.RWST', and @(->)@.

Here's an example of 'magnify' being used to work with a part of a bigger config. First, the types:

@
data URL = URL {
  _protocol :: Maybe String,
  _path :: String }

data Config = Config {
  _base :: URL,
  ... }

makeLenses ''URL
makeLenses ''Config
@

Now, let's define a function which returns the base url:

@
getBase :: 'Reader.Reader' Config String
getBase = do
  protocol \<- 'Data.Maybe.fromMaybe' \"https\" '<$>' 'view' (base.protocol)
  path     \<- 'view' (base.path)
  return (protocol ++ path)
@

With 'magnify', we can factor out @base@:

@
getBase = 'magnify' base $ do
  protocol \<- 'Data.Maybe.fromMaybe' \"https\" '<$>' 'view' protocol
  path     \<- 'view' path
  return (protocol ++ path)
@
  -}
  magnify :: ((b -> Magnified m c b) -> a -> Magnified m c a) -> m c -> n c

instance Monad m => Magnify (ReaderT b m) (ReaderT a m) b a where
  magnify l (ReaderT m) = ReaderT $ getEffect #. l (Effect #. m)
  {-# INLINE magnify #-}

instance Magnify ((->) b) ((->) a) b a where
  magnify l f = Reader.asks (getConst #. l (Const #. f))
  {-# INLINE magnify #-}

instance (Monad m, Monoid w) => Magnify (Strict.RWST b w s m) (Strict.RWST a w s m) b a where
  magnify l (Strict.RWST m) = Strict.RWST $ getEffectRWS #. l (EffectRWS #. m)
  {-# INLINE magnify #-}

instance (Monad m, Monoid w) => Magnify (Lazy.RWST b w s m) (Lazy.RWST a w s m) b a where
  magnify l (Lazy.RWST m) = Lazy.RWST $ getEffectRWS #. l (EffectRWS #. m)
  {-# INLINE magnify #-}

instance Magnify m n b a => Magnify (IdentityT m) (IdentityT n) b a where
  magnify l (IdentityT m) = IdentityT (magnify l m)
  {-# INLINE magnify #-}
