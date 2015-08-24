{-# LANGUAGE
CPP,
MultiParamTypeClasses,
TypeFamilies,
FlexibleContexts,
FlexibleInstances,
UndecidableInstances,
BangPatterns,
Trustworthy
  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif


{- |
This module contains only instances. Import this module to get:

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
-}
module Lens.Micro.GHC
(
)
where


import Lens.Micro
import Lens.Micro.Internal

import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.IntMap as IntMap
import           Data.IntMap (IntMap)
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU

import Data.Tree
import Data.Array.IArray as Array
import Data.Array.Unboxed

import Data.Int
import Data.Word
import Data.Monoid
import Foreign.Storable
import Foreign.Ptr
import Data.Bits
#if MIN_VERSION_base(4,8,0)
import Foreign.ForeignPtr
#else
import Foreign.ForeignPtr.Safe
#endif
import GHC.ForeignPtr (mallocPlainForeignPtrBytes)
import GHC.IO (unsafeDupablePerformIO)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
import Data.Traversable
#endif


(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as
{-# INLINE (<&>) #-}

infixl 1 <&>

type instance Index   (Map k a) = k
type instance IxValue (Map k a) = a
type instance Index   (IntMap a) = Int
type instance IxValue (IntMap a) = a
type instance Index   (Seq a) = Int
type instance IxValue (Seq a) = a
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
  at k f m = f mv <&> \r -> case r of
    Nothing -> maybe m (const (IntMap.delete k m)) mv
    Just v' -> IntMap.insert k v' m
    where mv = IntMap.lookup k m
  {-# INLINE at #-}

instance Ord k => At (Map k a) where
  at k f m = f mv <&> \r -> case r of
    Nothing -> maybe m (const (Map.delete k m)) mv
    Just v' -> Map.insert k v' m
    where mv = Map.lookup k m
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

instance Each (Tree a) (Tree b) a b

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

------------------------------------------------------------------------------
-- Control.Lens.Internal.ByteString
------------------------------------------------------------------------------

grain :: Int
grain = 32
{-# INLINE grain #-}

traversedStrictTree :: Traversal' B.ByteString Word8
traversedStrictTree afb bs = unsafeCreate len <$> go 0 len
 where
   len = B.length bs
   go !i !j
     | i + grain < j, k <- i + shiftR (j - i) 1 = (\l r q -> l q >> r q) <$> go i k <*> go k j
     | otherwise = run i j
   run !i !j
     | i == j    = pure (\_ -> return ())
     | otherwise = let !x = BU.unsafeIndex bs i
                   in (\y ys q -> pokeByteOff q i y >> ys q) <$> afb x <*> run (i + 1) j
{-# INLINE [0] traversedStrictTree #-}

{-# RULES
"bytes -> map"
  traversedStrictTree = sets B.map :: ASetter' B.ByteString Word8;
"bytes -> foldr"
  traversedStrictTree = foldring B.foldr :: Getting (Endo r) B.ByteString Word8;
  #-}

-- A way of creating ByteStrings outside the IO monad. The @Int@
-- argument gives the final size of the ByteString. Unlike
-- 'createAndTrim' the ByteString is not reallocated if the final size
-- is less than the estimated size.
unsafeCreate :: Int -> (Ptr Word8 -> IO ()) -> B.ByteString
unsafeCreate l f = unsafeDupablePerformIO (create l f)
{-# INLINE unsafeCreate #-}

-- Create ByteString of size @l@ and use action @f@ to fill it's contents.
create :: Int -> (Ptr Word8 -> IO ()) -> IO B.ByteString
create l f = do
    fp <- mallocPlainForeignPtrBytes l
    withForeignPtr fp $ \p -> f p
    return $! BI.PS fp 0 l
{-# INLINE create #-}

traversedLazy :: Traversal' BL.ByteString Word8
traversedLazy afb = \lbs -> foldrChunks go (pure BL.empty) lbs
  where
  go c fcs = BL.append . fromStrict
             <$> traversedStrictTree afb c
             <*> fcs
{-# INLINE [1] traversedLazy #-}

{-# RULES
"sets lazy bytestring"
  traversedLazy = sets BL.map :: ASetter' BL.ByteString Word8;
"gets lazy bytestring"
  traversedLazy = foldring BL.foldr :: Getting (Endo r) BL.ByteString Word8;
  #-}

fromStrict :: B.ByteString -> BL.ByteString
#if MIN_VERSION_bytestring(0,10,0)
fromStrict = BL.fromStrict
#else
fromStrict = \x -> BL.fromChunks [x]
#endif
{-# INLINE fromStrict #-}

foldrChunks :: (B.ByteString -> r -> r) -> r -> BL.ByteString -> r
#if MIN_VERSION_bytestring(0,10,0)
foldrChunks = BL.foldrChunks
#else
foldrChunks f z b = foldr f z (BL.toChunks b)
#endif
{-# INLINE foldrChunks #-}
