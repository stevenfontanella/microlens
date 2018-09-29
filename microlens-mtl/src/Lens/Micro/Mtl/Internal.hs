{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Trustworthy #-}

-- This is needed because ErrorT is deprecated.
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}


{- |
Module      :  Lens.Micro.Mtl.Internal
Copyright   :  (C) 2013-2016 Edward Kmett, 2015-2016 Artyom Kazak, 2018 Monadfix
License     :  BSD-style (see the file LICENSE)

This module lets you define your own instances of 'Zoom' and 'Magnify'.

The warning from "Lens.Micro.Internal" applies to this module as well. Don't export functions that have 'Zoom' or 'Magnify' in their type signatures. If you absolutely need to define an instance (e.g. for internal use), only do it for your own types, because otherwise I might add an instance to one of the microlens packages later and if our instances are different it might lead to subtle bugs.
-}
module Lens.Micro.Mtl.Internal
(
  -- * Classes
  Zoomed,
  Zoom(..),
  Magnified,
  Magnify(..),

  -- * Focusing (used for 'Zoom')
  Focusing(..),
  FocusingWith(..),
  FocusingPlus(..),
  FocusingOn(..),
  FocusingMay(..),
  FocusingErr(..),

  -- * Effect (used for 'Magnify')
  Effect(..),
  EffectRWS(..),

  -- * Utilities
  May(..),
  Err(..),
)
where


import Control.Applicative
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

#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif


------------------------------------------------------------------------------
-- Zoomed
------------------------------------------------------------------------------

-- | This type family is used by 'Zoom' to describe the common effect type.
type family Zoomed (m :: * -> *) :: * -> * -> *
type instance Zoomed (Strict.StateT s z) = Focusing z
type instance Zoomed (Lazy.StateT s z) = Focusing z
type instance Zoomed (ReaderT e m) = Zoomed m
type instance Zoomed (IdentityT m) = Zoomed m
type instance Zoomed (Strict.RWST r w s z) = FocusingWith w z
type instance Zoomed (Lazy.RWST r w s z) = FocusingWith w z
type instance Zoomed (Strict.WriterT w m) = FocusingPlus w (Zoomed m)
type instance Zoomed (Lazy.WriterT w m) = FocusingPlus w (Zoomed m)
type instance Zoomed (ListT m) = FocusingOn [] (Zoomed m)
type instance Zoomed (MaybeT m) = FocusingMay (Zoomed m)
type instance Zoomed (ErrorT e m) = FocusingErr e (Zoomed m)
type instance Zoomed (ExceptT e m) = FocusingErr e (Zoomed m)

------------------------------------------------------------------------------
-- Focusing
------------------------------------------------------------------------------

-- | Used by 'Zoom' to 'zoom' into 'Control.Monad.State.StateT'.
newtype Focusing m s a = Focusing { unfocusing :: m (s, a) }

instance Monad m => Functor (Focusing m s) where
  fmap f (Focusing m) = Focusing $ do
     (s, a) <- m
     return (s, f a)
  {-# INLINE fmap #-}

instance (Monad m, Monoid s) => Applicative (Focusing m s) where
  pure a = Focusing (return (mempty, a))
  {-# INLINE pure #-}
  Focusing mf <*> Focusing ma = Focusing $ do
    (s, f) <- mf
    (s', a) <- ma
    return (mappend s s', f a)
  {-# INLINE (<*>) #-}

------------------------------------------------------------------------------
-- FocusingWith
------------------------------------------------------------------------------

-- | Used by 'Zoom' to 'zoom' into 'Control.Monad.RWS.RWST'.
newtype FocusingWith w m s a = FocusingWith { unfocusingWith :: m (s, a, w) }

instance Monad m => Functor (FocusingWith w m s) where
  fmap f (FocusingWith m) = FocusingWith $ do
     (s, a, w) <- m
     return (s, f a, w)
  {-# INLINE fmap #-}

instance (Monad m, Monoid s, Monoid w) => Applicative (FocusingWith w m s) where
  pure a = FocusingWith (return (mempty, a, mempty))
  {-# INLINE pure #-}
  FocusingWith mf <*> FocusingWith ma = FocusingWith $ do
    (s, f, w) <- mf
    (s', a, w') <- ma
    return (mappend s s', f a, mappend w w')
  {-# INLINE (<*>) #-}

------------------------------------------------------------------------------
-- FocusingPlus
------------------------------------------------------------------------------

-- | Used by 'Zoom' to 'zoom' into 'Control.Monad.Writer.WriterT'.
newtype FocusingPlus w k s a = FocusingPlus { unfocusingPlus :: k (s, w) a }

instance Functor (k (s, w)) => Functor (FocusingPlus w k s) where
  fmap f (FocusingPlus as) = FocusingPlus (fmap f as)
  {-# INLINE fmap #-}

instance Applicative (k (s, w)) => Applicative (FocusingPlus w k s) where
  pure = FocusingPlus . pure
  {-# INLINE pure #-}
  FocusingPlus kf <*> FocusingPlus ka = FocusingPlus (kf <*> ka)
  {-# INLINE (<*>) #-}

------------------------------------------------------------------------------
-- FocusingOn
------------------------------------------------------------------------------

-- | Used by 'Zoom' to 'zoom' into 'Control.Monad.Trans.Maybe.MaybeT' or 'Control.Monad.Trans.List.ListT'.
newtype FocusingOn f k s a = FocusingOn { unfocusingOn :: k (f s) a }

instance Functor (k (f s)) => Functor (FocusingOn f k s) where
  fmap f (FocusingOn as) = FocusingOn (fmap f as)
  {-# INLINE fmap #-}

instance Applicative (k (f s)) => Applicative (FocusingOn f k s) where
  pure = FocusingOn . pure
  {-# INLINE pure #-}
  FocusingOn kf <*> FocusingOn ka = FocusingOn (kf <*> ka)
  {-# INLINE (<*>) #-}

------------------------------------------------------------------------------
-- May
------------------------------------------------------------------------------

-- | Make a 'Monoid' out of 'Maybe' for error handling.
newtype May a = May { getMay :: Maybe a }

instance Monoid a => Monoid (May a) where
  mempty = May (Just mempty)
  {-# INLINE mempty #-}
#if !MIN_VERSION_base(4,11,0)
  May Nothing `mappend` _ = May Nothing
  _ `mappend` May Nothing = May Nothing
  May (Just a) `mappend` May (Just b) = May (Just (mappend a b))
  {-# INLINE mappend #-}
#else
instance Semigroup a => Semigroup (May a) where
  May Nothing <> _ = May Nothing
  _ <> May Nothing = May Nothing
  May (Just a) <> May (Just b) = May (Just (a <> b))
  {-# INLINE (<>) #-}
#endif

------------------------------------------------------------------------------
-- FocusingMay
------------------------------------------------------------------------------

-- | Used by 'Zoom' to 'zoom' into 'Control.Monad.Error.ErrorT'.
newtype FocusingMay k s a = FocusingMay { unfocusingMay :: k (May s) a }

instance Functor (k (May s)) => Functor (FocusingMay k s) where
  fmap f (FocusingMay as) = FocusingMay (fmap f as)
  {-# INLINE fmap #-}

instance Applicative (k (May s)) => Applicative (FocusingMay k s) where
  pure = FocusingMay . pure
  {-# INLINE pure #-}
  FocusingMay kf <*> FocusingMay ka = FocusingMay (kf <*> ka)
  {-# INLINE (<*>) #-}

------------------------------------------------------------------------------
-- Err
------------------------------------------------------------------------------

-- | Make a 'Monoid' out of 'Either' for error handling.
newtype Err e a = Err { getErr :: Either e a }

instance Monoid a => Monoid (Err e a) where
  mempty = Err (Right mempty)
  {-# INLINE mempty #-}
#if !MIN_VERSION_base(4,11,0)
  Err (Left e) `mappend` _ = Err (Left e)
  _ `mappend` Err (Left e) = Err (Left e)
  Err (Right a) `mappend` Err (Right b) = Err (Right (mappend a b))
  {-# INLINE mappend #-}
#else
instance Semigroup a => Semigroup (Err e a) where
  Err (Left e) <> _ = Err (Left e)
  _ <> Err (Left e) = Err (Left e)
  Err (Right a) <> Err (Right b) = Err (Right (a <> b))
  {-# INLINE (<>) #-}
#endif

------------------------------------------------------------------------------
-- FocusingErr
------------------------------------------------------------------------------

-- | Used by 'Zoom' to 'zoom' into 'Control.Monad.Error.ErrorT'.
newtype FocusingErr e k s a = FocusingErr { unfocusingErr :: k (Err e s) a }

instance Functor (k (Err e s)) => Functor (FocusingErr e k s) where
  fmap f (FocusingErr as) = FocusingErr (fmap f as)
  {-# INLINE fmap #-}

instance Applicative (k (Err e s)) => Applicative (FocusingErr e k s) where
  pure = FocusingErr . pure
  {-# INLINE pure #-}
  FocusingErr kf <*> FocusingErr ka = FocusingErr (kf <*> ka)
  {-# INLINE (<*>) #-}

------------------------------------------------------------------------------
-- Zoom
------------------------------------------------------------------------------

infixr 2 `zoom`

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
  player.position.x 'Lens.Micro.Mtl.+=' 1
  player.position.y 'Lens.Micro.Mtl.+=' 1
@

With 'zoom', you can use @player.position@ to focus just on a part of the state:

@
moveNE :: 'Lazy.State' Game ()
moveNE = do
  'zoom' (player.position) $ do
    x 'Lens.Micro.Mtl.+=' 1
    y 'Lens.Micro.Mtl.+=' 1
@

You can just as well use it for retrieving things out of the state:

@
getCoords :: 'Lazy.State' Game (Int, Int)
getCoords = 'zoom' (player.position) ((,) '<$>' 'Lens.Micro.Mtl.use' x '<*>' 'Lens.Micro.Mtl.use' y)
@

Or more explicitly:

@
getCoords = 'zoom' (player.position) $ do
  x' <- 'Lens.Micro.Mtl.use' x
  y' <- 'Lens.Micro.Mtl.use' y
  return (x', y')
@

When you pass a traversal to 'zoom', it'll work as a loop. For instance, here we move all obstacles:

@
moveObstaclesNE :: 'Lazy.State' Game ()
moveObstaclesNE = do
  'zoom' (obstacles.'each') $ do
    x 'Lens.Micro.Mtl.+=' 1
    y 'Lens.Micro.Mtl.+=' 1
@

If the action returns a result, all results would be combined with '<>' – the same way they're combined when '^.' is passed a traversal. In this example, @moveObstaclesNE@ returns a list of old coordinates of obstacles in addition to moving them:

@
moveObstaclesNE = do
  xys <- 'zoom' (obstacles.'each') $ do
    -- Get old coordinates.
    x' <- 'Lens.Micro.Mtl.use' x
    y' <- 'Lens.Micro.Mtl.use' y
    -- Update them.
    x 'Lens.Micro.Mtl..=' x' + 1
    y 'Lens.Micro.Mtl..=' y' + 1
    -- Return a single-element list with old coordinates.
    return [(x', y')]
  ...
@

Finally, you might need to write your own instances of 'Zoom' if you use @newtype@d transformers in your monad stack. This can be done as follows:

@
import "Lens.Micro.Mtl.Internal"

type instance 'Zoomed' (MyStateT s m) = 'Zoomed' (StateT s m)

instance Monad m =\> 'Zoom' (MyStateT s m) (MyStateT t m) s t where
    'zoom' l (MyStateT m) = MyStateT ('zoom' l m)
@
  -}
  zoom :: LensLike' (Zoomed m c) t s -> m c -> n c

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

------------------------------------------------------------------------------
-- Magnified
------------------------------------------------------------------------------

-- | This type family is used by 'Magnify' to describe the common effect type.
type family Magnified (m :: * -> *) :: * -> * -> *
type instance Magnified (ReaderT b m) = Effect m
type instance Magnified ((->)b) = Const
type instance Magnified (Strict.RWST a w s m) = EffectRWS w s m
type instance Magnified (Lazy.RWST a w s m) = EffectRWS w s m
type instance Magnified (IdentityT m) = Magnified m

------------------------------------------------------------------------------
-- Magnify
------------------------------------------------------------------------------

infixr 2 `magnify`

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
  protocol \<- 'Data.Maybe.fromMaybe' \"https\" '<$>' 'Lens.Micro.Mtl.view' (base.protocol)
  path     \<- 'Lens.Micro.Mtl.view' (base.path)
  return (protocol ++ path)
@

With 'magnify', we can factor out @base@:

@
getBase = 'magnify' base $ do
  protocol \<- 'Data.Maybe.fromMaybe' \"https\" '<$>' 'Lens.Micro.Mtl.view' protocol
  path     \<- 'Lens.Micro.Mtl.view' path
  return (protocol ++ path)
@

This concludes the example.

Finally, you should know writing instances of 'Magnify' for your own types can be done as follows:

@
import "Lens.Micro.Mtl.Internal"

type instance 'Magnified' (MyReaderT r m) = 'Magnified' (ReaderT r m)

instance Monad m =\> 'Magnify' (MyReaderT r m) (MyReaderT t m) r t where
    'magnify' l (MyReaderT m) = MyReaderT ('magnify' l m)
@
  -}
  magnify :: LensLike' (Magnified m c) a b -> m c -> n c

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

-----------------------------------------------------------------------------
--- Effect
-------------------------------------------------------------------------------

-- | Wrap a monadic effect with a phantom type argument.
newtype Effect m r a = Effect { getEffect :: m r }
-- type role Effect representational nominal phantom

instance Functor (Effect m r) where
  fmap _ (Effect m) = Effect m
  {-# INLINE fmap #-}

instance (Monad m, Monoid r) => Monoid (Effect m r a) where
  mempty = Effect (return mempty)
  {-# INLINE mempty #-}
#if !MIN_VERSION_base(4,11,0)
  Effect ma `mappend` Effect mb = Effect (liftM2 mappend ma mb)
  {-# INLINE mappend #-}
#else
instance (Monad m, Semigroup r) => Semigroup (Effect m r a) where
  Effect ma <> Effect mb = Effect (liftM2 (<>) ma mb)
  {-# INLINE (<>) #-}
#endif

instance (Monad m, Monoid r) => Applicative (Effect m r) where
  pure _ = Effect (return mempty)
  {-# INLINE pure #-}
  Effect ma <*> Effect mb = Effect (liftM2 mappend ma mb)
  {-# INLINE (<*>) #-}

------------------------------------------------------------------------------
-- EffectRWS
------------------------------------------------------------------------------

-- | Wrap a monadic effect with a phantom type argument. Used when magnifying 'Control.Monad.RWS.RWST'.
newtype EffectRWS w st m s a = EffectRWS { getEffectRWS :: st -> m (s,st,w) }

instance Functor (EffectRWS w st m s) where
  fmap _ (EffectRWS m) = EffectRWS m
  {-# INLINE fmap #-}

instance (Monoid s, Monoid w, Monad m) => Applicative (EffectRWS w st m s) where
  pure _ = EffectRWS $ \st -> return (mempty, st, mempty)
  {-# INLINE pure #-}
  EffectRWS m <*> EffectRWS n = EffectRWS $ \st -> m st >>= \ (s,t,w) -> n t >>= \ (s',u,w') -> return (mappend s s', u, mappend w w')
  {-# INLINE (<*>) #-}
