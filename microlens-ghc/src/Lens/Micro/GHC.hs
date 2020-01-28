{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Trustworthy #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}


{- |
Module      :  Lens.Micro.GHC
Copyright   :  (C) 2013-2016 Edward Kmett, 2015-2016 Artyom Kazak, 2018 Monadfix
License     :  BSD-style (see the file LICENSE)

By importing this module you get all functions and types from <http://hackage.haskell.org/package/microlens microlens>, as well as the following instances:

* 'at' for 'Map' and 'IntMap'

* 'each' and 'ix' for

    * 'Map' and 'IntMap'
    * 'Array' and 'UArray'
    * 'Seq'
    * strict 'B.ByteString' and lazy 'BL.ByteString'
    * 'Tree'

* '_head', '_tail', '_init', '_last' for

    * 'Seq'
    * strict and lazy bytestrings

* 'strict' and 'lazy' for

    * bytestrings
    * @StateT@, @WriterT@, @RWST@
-}
module Lens.Micro.GHC
(
  module Lens.Micro,
  packedBytes, unpackedBytes,
  packedChars, unpackedChars,
  chars,
)
where


import Lens.Micro
import Lens.Micro.Internal
import Lens.Micro.GHC.Internal

import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.IntMap as IntMap
import           Data.IntMap (IntMap)
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.IntSet as IntSet
import           Data.IntSet (IntSet)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.Trans.RWS.Strict as Strict

import Data.Tree
import Data.Array.IArray as Array
import Data.Array.Unboxed

import Data.Int
import Data.Word

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
import Data.Traversable
#endif


type instance Index   (Map k a) = k
type instance IxValue (Map k a) = a
type instance Index   (IntMap a) = Int
type instance IxValue (IntMap a) = a
type instance Index   (Seq a) = Int
type instance IxValue (Seq a) = a
type instance Index   (Set a) = a
type instance IxValue (Set a) = ()
type instance Index   IntSet = Int
type instance IxValue IntSet = ()
type instance Index   (Tree a) = [Int]
type instance IxValue (Tree a) = a
type instance Index   (Array.Array i e) = i
type instance IxValue (Array.Array i e) = e
type instance Index   (UArray i e) = i
type instance IxValue (UArray i e) = e
type instance Index   B.ByteString = Int
type instance IxValue B.ByteString = Word8
type instance Index   BL.ByteString = Int64
type instance IxValue BL.ByteString = Word8

instance Ord k => Ixed (Map k a) where
  ix k f m = case Map.lookup k m of
     Just v  -> f v <&> \v' -> Map.insert k v' m
     Nothing -> pure m
  {-# INLINE ix #-}

instance Ixed (IntMap a) where
  ix k f m = case IntMap.lookup k m of
     Just v -> f v <&> \v' -> IntMap.insert k v' m
     Nothing -> pure m
  {-# INLINE ix #-}

instance Ixed (Seq a) where
  ix i f m
    | 0 <= i && i < Seq.length m = f (Seq.index m i) <&> \a -> Seq.update i a m
    | otherwise                  = pure m
  {-# INLINE ix #-}

instance Ord k => Ixed (Set k) where
  ix k f m = if Set.member k m
     then f () <&> \() -> Set.insert k m
     else pure m
  {-# INLINE ix #-}

instance Ixed IntSet where
  ix k f m = if IntSet.member k m
     then f () <&> \() -> IntSet.insert k m
     else pure m
  {-# INLINE ix #-}

instance Ixed (Tree a) where
  ix xs0 f = go xs0 where
    go [] (Node a as) = f a <&> \a' -> Node a' as
    go (i:is) t@(Node a as)
      | i < 0     = pure t
      | otherwise = Node a <$> ix i (go is) as
  {-# INLINE ix #-}

instance Ix i => Ixed (Array.Array i e) where
  ix i f arr
    | inRange (bounds arr) i = f (arr Array.! i) <&> \e -> arr Array.// [(i,e)]
    | otherwise              = pure arr
  {-# INLINE ix #-}

instance (IArray UArray e, Ix i) => Ixed (UArray i e) where
  ix i f arr
    | inRange (bounds arr) i = f (arr Array.! i) <&> \e -> arr Array.// [(i,e)]
    | otherwise              = pure arr
  {-# INLINE ix #-}

instance Ixed B.ByteString where
  ix e f s = case B.splitAt e s of
     (l, mr) -> case B.uncons mr of
       Nothing      -> pure s
       Just (c, xs) -> f c <&> \d -> B.concat [l, B.singleton d, xs]
  {-# INLINE ix #-}

instance Ixed BL.ByteString where
  -- TODO: we could be lazier, returning each chunk as it is passed
  ix e f s = case BL.splitAt e s of
     (l, mr) -> case BL.uncons mr of
       Nothing      -> pure s
       Just (c, xs) -> f c <&> \d -> BL.append l (BL.cons d xs)
  {-# INLINE ix #-}

instance At (IntMap a) where
#if MIN_VERSION_containers(0,5,8)
  at k f = IntMap.alterF f k
#else
  at k f m = f mv <&> \r -> case r of
    Nothing -> maybe m (const (IntMap.delete k m)) mv
    Just v' -> IntMap.insert k v' m
    where mv = IntMap.lookup k m
#endif
  {-# INLINE at #-}

instance Ord k => At (Map k a) where
#if MIN_VERSION_containers(0,5,8)
  at k f = Map.alterF f k
#else
  at k f m = f mv <&> \r -> case r of
    Nothing -> maybe m (const (Map.delete k m)) mv
    Just v' -> Map.insert k v' m
    where mv = Map.lookup k m
#endif
  {-# INLINE at #-}

instance At IntSet where
  at k f m = f mv <&> \r -> case r of
    Nothing -> maybe m (const (IntSet.delete k m)) mv
    Just () -> IntSet.insert k m
    where mv = if IntSet.member k m then Just () else Nothing
  {-# INLINE at #-}

instance Ord k => At (Set k) where
  at k f m = f mv <&> \r -> case r of
    Nothing -> maybe m (const (Set.delete k m)) mv
    Just () -> Set.insert k m
    where mv = if Set.member k m then Just () else Nothing
  {-# INLINE at #-}

instance (c ~ d) => Each (Map c a) (Map d b) a b where
  each = traversed
  {-# INLINE each #-}

instance Each (IntMap a) (IntMap b) a b where
  each = traversed
  {-# INLINE each #-}

instance Each (Seq a) (Seq b) a b where
  each = traversed
  {-# INLINE each #-}

instance Each (Tree a) (Tree b) a b where
  each = traversed
  {-# INLINE each #-}

instance (Ix i, i ~ j) => Each (Array i a) (Array j b) a b where
  each f arr = array (bounds arr) <$> traverse (\(i,a) -> (,) i <$> f a) (Array.assocs arr)
  {-# INLINE each #-}

instance (Ix i, IArray UArray a, IArray UArray b, i ~ j) => Each (UArray i a) (UArray j b) a b where
  each f arr = array (bounds arr) <$> traverse (\(i,a) -> (,) i <$> f a) (Array.assocs arr)
  {-# INLINE each #-}

instance (a ~ Word8, b ~ Word8) => Each B.ByteString B.ByteString a b where
  each = traversedStrictTree
  {-# INLINE each #-}

instance (a ~ Word8, b ~ Word8) => Each BL.ByteString BL.ByteString a b where
  each = traversedLazy
  {-# INLINE each #-}

instance Cons (Seq a) (Seq b) a b where
  _Cons f s = case Seq.viewl s of
    x Seq.:< xs -> uncurry (Seq.<|) <$> f (x, xs)
    Seq.EmptyL  -> pure Seq.empty
  {-# INLINE _Cons #-}

instance Snoc (Seq a) (Seq b) a b where
  _Snoc f s = case Seq.viewr s of
    xs Seq.:> x -> uncurry (Seq.|>) <$> f (xs, x)
    Seq.EmptyR  -> pure Seq.empty
  {-# INLINE _Snoc #-}

instance Cons B.ByteString B.ByteString Word8 Word8 where
  _Cons f s = case B.uncons s of
    Just x  -> uncurry B.cons <$> f x
    Nothing -> pure B.empty
  {-# INLINE _Cons #-}

instance Cons BL.ByteString BL.ByteString Word8 Word8 where
  _Cons f s = case BL.uncons s of
    Just x  -> uncurry BL.cons <$> f x
    Nothing -> pure BL.empty
  {-# INLINE _Cons #-}

instance Snoc B.ByteString B.ByteString Word8 Word8 where
  _Snoc f s = if B.null s
    then pure B.empty
    else uncurry B.snoc <$> f (B.init s, B.last s)
  {-# INLINE _Snoc #-}

instance Snoc BL.ByteString BL.ByteString Word8 Word8 where
  _Snoc f s = if BL.null s
    then pure BL.empty
    else uncurry BL.snoc <$> f (BL.init s, BL.last s)
  {-# INLINE _Snoc #-}

instance Strict BL.ByteString B.ByteString where
  strict f s = fromStrict <$> f (toStrict s)
  {-# INLINE strict #-}
  lazy f s = toStrict <$> f (fromStrict s)
  {-# INLINE lazy #-}

instance Strict (Lazy.StateT s m a) (Strict.StateT s m a) where
  strict f s = Lazy.StateT . Strict.runStateT <$>
               f (Strict.StateT (Lazy.runStateT s))
  {-# INLINE strict #-}
  lazy f s = Strict.StateT . Lazy.runStateT <$>
             f (Lazy.StateT (Strict.runStateT s))
  {-# INLINE lazy #-}

instance Strict (Lazy.WriterT w m a) (Strict.WriterT w m a) where
  strict f s = Lazy.WriterT . Strict.runWriterT <$>
               f (Strict.WriterT (Lazy.runWriterT s))
  {-# INLINE strict #-}
  lazy f s = Strict.WriterT . Lazy.runWriterT <$>
             f (Lazy.WriterT (Strict.runWriterT s))
  {-# INLINE lazy #-}

instance Strict (Lazy.RWST r w s m a) (Strict.RWST r w s m a) where
  strict f s = Lazy.RWST . Strict.runRWST <$>
               f (Strict.RWST (Lazy.runRWST s))
  {-# INLINE strict #-}
  lazy f s = Strict.RWST . Lazy.runRWST <$>
             f (Lazy.RWST (Strict.runRWST s))
  {-# INLINE lazy #-}
