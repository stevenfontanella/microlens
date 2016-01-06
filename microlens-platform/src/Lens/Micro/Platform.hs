{-# LANGUAGE
CPP,
TypeFamilies,
MultiParamTypeClasses,
FlexibleInstances,
UndecidableInstances,
Trustworthy
  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif


{- |
This module is an approximation for @<http://hackage.haskell.org/package/lens/docs/Control-Lens.html Control.Lens>@ from <http://hackage.haskell.org/package/lens lens>; by importing it you get all functions and instances from <http://hackage.haskell.org/package/microlens microlens>, <http://hackage.haskell.org/package/microlens-mtl microlens-mtl>, <http://hackage.haskell.org/package/microlens-ghc microlens-ghc>, as well as the following instances:

* 'at' for 'HashMap'

* 'each' and 'ix' for

    * 'HashMap'
    * 'Vector.Vector' and variants (unboxed vectors, etc)
    * strict 'T.Text' and lazy 'TL.Text'

* '_head', '_tail', '_init', '_last' for

    * 'Vector.Vector' and variants
    * strict and lazy @Text@

* 'strict' and 'lazy' for @Text@
-}
module Lens.Micro.Platform
(
  module Lens.Micro.GHC,
  module Lens.Micro.Mtl,
  module Lens.Micro.TH,
)
where


import Lens.Micro.Internal
import Lens.Micro.GHC
import Lens.Micro.Mtl
import Lens.Micro.TH

import Data.Hashable
import Data.Int
import Data.Monoid

import Data.HashMap.Lazy as HashMap
import Data.Vector as Vector
import Data.Vector.Primitive as Prim
import Data.Vector.Storable as Storable
import Data.Vector.Unboxed as Unboxed
import Data.Vector.Generic as Generic

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif


(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as
{-# INLINE (<&>) #-}

infixl 1 <&>


type instance Index   (HashMap k a) = k
type instance IxValue (HashMap k a) = a
type instance Index   (Vector.Vector a) = Int
type instance IxValue (Vector.Vector a) = a
type instance Index   (Prim.Vector a) = Int
type instance IxValue (Prim.Vector a) = a
type instance Index   (Storable.Vector a) = Int
type instance IxValue (Storable.Vector a) = a
type instance Index   (Unboxed.Vector a) = Int
type instance IxValue (Unboxed.Vector a) = a
type instance Index   T.Text = Int
type instance IxValue T.Text = Char
type instance Index   TL.Text = Int64
type instance IxValue TL.Text = Char

instance (Eq k, Hashable k) => Ixed (HashMap k a) where
  ix k f m = case HashMap.lookup k m of
     Just v  -> f v <&> \v' -> HashMap.insert k v' m
     Nothing -> pure m
  {-# INLINE ix #-}

instance (Eq k, Hashable k) => At (HashMap k a) where
  at k f m = f mv <&> \r -> case r of
    Nothing -> maybe m (const (HashMap.delete k m)) mv
    Just v' -> HashMap.insert k v' m
    where mv = HashMap.lookup k m
  {-# INLINE at #-}

instance Ixed (Vector.Vector a) where
  ix i f v
    | 0 <= i && i < Vector.length v = f (v Vector.! i) <&> \a -> v Vector.// [(i, a)]
    | otherwise                     = pure v
  {-# INLINE ix #-}

instance Prim a => Ixed (Prim.Vector a) where
  ix i f v
    | 0 <= i && i < Prim.length v = f (v Prim.! i) <&> \a -> v Prim.// [(i, a)]
    | otherwise                   = pure v
  {-# INLINE ix #-}

instance Storable a => Ixed (Storable.Vector a) where
  ix i f v
    | 0 <= i && i < Storable.length v = f (v Storable.! i) <&> \a -> v Storable.// [(i, a)]
    | otherwise                       = pure v
  {-# INLINE ix #-}

instance Unbox a => Ixed (Unboxed.Vector a) where
  ix i f v
    | 0 <= i && i < Unboxed.length v = f (v Unboxed.! i) <&> \a -> v Unboxed.// [(i, a)]
    | otherwise                      = pure v
  {-# INLINE ix #-}

instance Ixed T.Text where
  ix e f s = case T.splitAt e s of
     (l, mr) -> case T.uncons mr of
       Nothing      -> pure s
       Just (c, xs) -> f c <&> \d -> T.concat [l, T.singleton d, xs]
  {-# INLINE ix #-}

instance Ixed TL.Text where
  ix e f s = case TL.splitAt e s of
     (l, mr) -> case TL.uncons mr of
       Nothing      -> pure s
       Just (c, xs) -> f c <&> \d -> TL.append l (TL.cons d xs)
  {-# INLINE ix #-}

instance Cons T.Text T.Text Char Char where
  _Cons f s = case T.uncons s of
    Just x  -> uncurry T.cons <$> f x
    Nothing -> pure T.empty
  {-# INLINE _Cons #-}

instance Cons TL.Text TL.Text Char Char where
  _Cons f s = case TL.uncons s of
    Just x  -> uncurry TL.cons <$> f x
    Nothing -> pure TL.empty
  {-# INLINE _Cons #-}

instance Snoc T.Text T.Text Char Char where
  _Snoc f s = if T.null s
    then pure T.empty
    else uncurry T.snoc <$> f (T.init s, T.last s)
  {-# INLINE _Snoc #-}

instance Snoc TL.Text TL.Text Char Char where
  _Snoc f s = if TL.null s
    then pure TL.empty
    else uncurry TL.snoc <$> f (TL.init s, TL.last s)
  {-# INLINE _Snoc #-}

instance Cons (Vector.Vector a) (Vector.Vector b) a b where
  _Cons f s = if Vector.null s
    then pure Vector.empty
    else uncurry Vector.cons <$> f (Vector.unsafeHead s, Vector.unsafeTail s)
  {-# INLINE _Cons #-}

instance (Prim a, Prim b) => Cons (Prim.Vector a) (Prim.Vector b) a b where
  _Cons f s = if Prim.null s
    then pure Prim.empty
    else uncurry Prim.cons <$> f (Prim.unsafeHead s, Prim.unsafeTail s)
  {-# INLINE _Cons #-}

instance (Storable a, Storable b) => Cons (Storable.Vector a) (Storable.Vector b) a b where
  _Cons f s = if Storable.null s
    then pure Storable.empty
    else uncurry Storable.cons <$> f (Storable.unsafeHead s, Storable.unsafeTail s)
  {-# INLINE _Cons #-}

instance (Unbox a, Unbox b) => Cons (Unboxed.Vector a) (Unboxed.Vector b) a b where
  _Cons f s = if Unboxed.null s
    then pure Unboxed.empty
    else uncurry Unboxed.cons <$> f (Unboxed.unsafeHead s, Unboxed.unsafeTail s)
  {-# INLINE _Cons #-}

instance Snoc (Vector.Vector a) (Vector.Vector b) a b where
  _Snoc f s = if Vector.null s
    then pure Vector.empty
    else uncurry Vector.snoc <$> f (Vector.unsafeInit s, Vector.unsafeLast s)
  {-# INLINE _Snoc #-}

instance (Prim a, Prim b) => Snoc (Prim.Vector a) (Prim.Vector b) a b where
  _Snoc f s = if Prim.null s
    then pure Prim.empty
    else uncurry Prim.snoc <$> f (Prim.unsafeInit s, Prim.unsafeLast s)
  {-# INLINE _Snoc #-}

instance (Storable a, Storable b) => Snoc (Storable.Vector a) (Storable.Vector b) a b where
  _Snoc f s = if Storable.null s
    then pure Storable.empty
    else uncurry Storable.snoc <$> f (Storable.unsafeInit s, Storable.unsafeLast s)
  {-# INLINE _Snoc #-}

instance (Unbox a, Unbox b) => Snoc (Unboxed.Vector a) (Unboxed.Vector b) a b where
  _Snoc f s = if Unboxed.null s
    then pure Unboxed.empty
    else uncurry Unboxed.snoc <$> f (Unboxed.unsafeInit s, Unboxed.unsafeLast s)
  {-# INLINE _Snoc #-}

instance Each (Vector.Vector a) (Vector.Vector b) a b where
  each = vectorTraverse
  {-# INLINE each #-}

instance (Prim a, Prim b) => Each (Prim.Vector a) (Prim.Vector b) a b where
  each = vectorTraverse
  {-# INLINE each #-}

instance (Storable a, Storable b) => Each (Storable.Vector a) (Storable.Vector b) a b where
  each = vectorTraverse
  {-# INLINE each #-}

instance (Unbox a, Unbox b) => Each (Unboxed.Vector a) (Unboxed.Vector b) a b where
  each = vectorTraverse
  {-# INLINE each #-}

instance (c ~ d) => Each (HashMap c a) (HashMap d b) a b where
  each = traversed
  {-# INLINE each #-}  

instance (a ~ Char, b ~ Char) => Each T.Text T.Text a b where
  each = strictText
  {-# INLINE each #-}

instance (a ~ Char, b ~ Char) => Each TL.Text TL.Text a b where
  each = lazyText
  {-# INLINE each #-}

strictUnpacked :: Lens' T.Text String
strictUnpacked f t = T.pack <$> f (T.unpack t)
{-# INLINE strictUnpacked #-}

strictText :: Traversal' T.Text Char
strictText = strictUnpacked . traversed
{-# INLINE [0] strictText #-}

{-# RULES
"strict text -> map"    strictText = sets T.map        :: ASetter' T.Text Char;
"strict text -> foldr"  strictText = foldring T.foldr  :: Getting (Endo r) T.Text Char;
 #-}

lazyUnpacked :: Lens' TL.Text String
lazyUnpacked f t = TL.pack <$> f (TL.unpack t)
{-# INLINE lazyUnpacked #-}

lazyText :: Traversal' TL.Text Char
lazyText = lazyUnpacked . traversed
{-# INLINE [0] lazyText #-}

{-# RULES
"lazy text -> map"    lazyText = sets TL.map        :: ASetter' TL.Text Char;
"lazy text -> foldr"  lazyText = foldring TL.foldr  :: Getting (Endo r) TL.Text Char;
 #-}

vectorTraverse :: (Generic.Vector v a, Generic.Vector w b) => Traversal (v a) (w b) a b
vectorTraverse f v = Generic.fromListN (Generic.length v) <$> traversed f (Generic.toList v)
{-# INLINE [0] vectorTraverse #-}

{-# RULES
"vectorTraverse -> mapped" vectorTraverse  = sets Generic.map         :: (Generic.Vector v a, Generic.Vector v b) => ASetter (v a) (v b) a b;
"vectorTraverse -> foldr"  vectorTraverse  = foldring Generic.foldr   :: Generic.Vector v a => Getting (Endo r) (v a) a;
 #-}

instance Strict TL.Text T.Text where
  strict f s = TL.fromStrict <$> f (TL.toStrict s)
  {-# INLINE strict #-}
  lazy f s = TL.toStrict <$> f (TL.fromStrict s)
  {-# INLINE lazy #-}
