{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Unsafe #-}


{- |
Module      :  Lens.Micro.GHC.Internal
Copyright   :  (C) 2013-2016 Edward Kmett, 2015-2016 Artyom
License     :  BSD-style (see the file LICENSE)
-}
module Lens.Micro.GHC.Internal
(
  IsByteString(..),
  -- * Unpacking bytestrings
  unpackStrict,
  unpackStrict8,
  unpackLazy,
  unpackLazy8,
  -- * Converting bytestrings between strict and lazy
  fromStrict,
  toStrict,
  -- * Traversing bytestrings
  traversedStrictTree,
  traversedStrictTree8,
  traversedLazy,
  traversedLazy8,
)
where


import Lens.Micro
import Lens.Micro.Internal

import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Internal   as BI
import qualified Data.ByteString.Unsafe     as BU

import Data.Int
import Data.Word
import Data.Char
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
#if !MIN_VERSION_bytestring(0,10,4)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
#endif
import GHC.IO (unsafeDupablePerformIO)
import GHC.Base (unsafeChr)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif


class IsByteString t where
  {- |
Treat a list of bytes as a strict or lazy @ByteString@.
  -}
  packedBytes :: Lens' [Word8] t
  {- |
Treat a strict or lazy @ByteString@ as a list of bytes.
  -}
  unpackedBytes :: Lens' t [Word8]
  {- |
Treat a 'String' as a strict or lazy @ByteString@. (Note that it will garble characters above 0xFF, same as 'B8.pack' does.)
  -}
  packedChars :: Lens' String t
  {- |
Treat a strict or lazy @ByteString@ as a 'String'. (Just as 'packedChars', it will garble characters above 0xFF.)
  -}
  unpackedChars :: Lens' t String
  {- |
Traverse characters in a strict or lazy @ByteString@ (to traverse bytes instead of characters, use 'each').
  -}
  chars :: Traversal' t Char

-- When writing back to the 'ByteString' it is assumed that every 'Char'
-- lies between @'\x00'@ and @'\xff'@.

instance IsByteString B.ByteString where
  packedBytes f s = unpackStrict <$> f (B.pack s)
  {-# INLINE packedBytes #-}
  unpackedBytes f s = B.pack <$> f (unpackStrict s)
  {-# INLINE unpackedBytes #-}
  packedChars f s = unpackStrict8 <$> f (B8.pack s)
  {-# INLINE packedChars #-}
  unpackedChars f s = B8.pack <$> f (unpackStrict8 s)
  {-# INLINE unpackedChars #-}
  chars = traversedStrictTree8
  {-# INLINE chars #-}

instance IsByteString BL.ByteString where
  packedBytes f s = unpackLazy <$> f (BL.pack s)
  {-# INLINE packedBytes #-}
  unpackedBytes f s = BL.pack <$> f (unpackLazy s)
  {-# INLINE unpackedBytes #-}
  packedChars f s = unpackLazy8 <$> f (BL8.pack s)
  {-# INLINE packedChars #-}
  unpackedChars f s = BL8.pack <$> f (unpackLazy8 s)
  {-# INLINE unpackedChars #-}
  chars = traversedLazy8
  {-# INLINE chars #-}

-- unpacking

unpackStrict :: B.ByteString -> [Word8]
#if MIN_VERSION_bytestring(0,10,4)
unpackStrict = B.unpack
#else
unpackStrict (BI.PS fp off len) =
      let p = unsafeForeignPtrToPtr fp
       in go (p `plusPtr` off) (p `plusPtr` (off+len))
    where
      go !p !q | p == q    = []
               | otherwise = let !x = BI.inlinePerformIO $ do
                                        x' <- peek p
                                        touchForeignPtr fp
                                        return x'
                             in x : go (p `plusPtr` 1) q
#endif
{-# INLINE unpackStrict #-}

unpackStrict8 :: B.ByteString -> String
#if MIN_VERSION_bytestring(0,10,4)
unpackStrict8 = B8.unpack
#else
unpackStrict8 (BI.PS fp off len) =
      let p = unsafeForeignPtrToPtr fp
       in go (p `plusPtr` off) (p `plusPtr` (off+len))
    where
      go !p !q | p == q    = []
               | otherwise = let !x = BI.inlinePerformIO $ do
                                        x' <- peek p
                                        touchForeignPtr fp
                                        return x'
                             in w2c x : go (p `plusPtr` 1) q
#endif
{-# INLINE unpackStrict8 #-}

unpackLazy :: BL.ByteString -> [Word8]
unpackLazy = BL.unpack
{-# INLINE unpackLazy #-}

unpackLazy8 :: BL.ByteString -> String
unpackLazy8 = BL8.unpack
{-# INLINE unpackLazy8 #-}

-- converting between strict and lazy

fromStrict :: B.ByteString -> BL.ByteString
#if MIN_VERSION_bytestring(0,10,0)
fromStrict = BL.fromStrict
#else
fromStrict = \x -> BL.fromChunks [x]
#endif
{-# INLINE fromStrict #-}

toStrict :: BL.ByteString -> B.ByteString
#if MIN_VERSION_bytestring(0,10,0)
toStrict = BL.toStrict
#else
toStrict = B.concat . BL.toChunks
#endif
{-# INLINE toStrict #-}

-- traversing

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

traversedStrictTree8 :: Traversal' B.ByteString Char
traversedStrictTree8 pafb bs = unsafeCreate len <$> go 0 len
 where
   len = B.length bs
   go !i !j
     | i + grain < j    = let k = i + shiftR (j - i) 1
                          in (\l r q -> l q >> r q) <$> go i k <*> go k j
     | otherwise        = run i j
   run !i !j
     | i == j           = pure (\_ -> return ())
     | otherwise        = let !x = BU.unsafeIndex bs i
                          in (\y ys q -> pokeByteOff q i (c2w y) >> ys q)
                         <$> pafb (w2c x)
                         <*> run (i + 1) j
{-# INLINE [0] traversedStrictTree8 #-}

{-# RULES
"chars -> map"
  traversedStrictTree8 = sets B8.map :: ASetter' B.ByteString Char;
"chars -> foldr"
  traversedStrictTree8 = foldring B8.foldr :: Getting (Endo r) B.ByteString Char;
  #-}

traversedLazy :: Traversal' BL.ByteString Word8
traversedLazy pafb = \lbs -> foldrChunks go (\_ -> pure BL.empty) lbs 0
  where
  go c fcs acc = BL.append . fromStrict
             <$> traversedStrictTree pafb c
             <*> fcs acc'
    where
    acc' :: Int64
    !acc' = acc + fromIntegral (B.length c)
{-# INLINE [1] traversedLazy #-}

{-# RULES
"sets lazy bytestring"
  traversedLazy = sets BL.map :: ASetter' BL.ByteString Word8;
"gets lazy bytestring"
  traversedLazy = foldring BL.foldr :: Getting (Endo r) BL.ByteString Word8;
  #-}

traversedLazy8 :: Traversal' BL.ByteString Char
traversedLazy8 pafb = \lbs -> foldrChunks go (\_ -> pure BL.empty) lbs 0
  where
  go c fcs acc = BL.append . fromStrict
             <$> traversedStrictTree8 pafb c
             <*> fcs acc'
    where
    acc' :: Int64
    !acc' = acc + fromIntegral (B.length c)
{-# INLINE [1] traversedLazy8 #-}

{-# RULES
"sets lazy bytestring"
  traversedLazy8 = sets BL8.map :: ASetter' BL8.ByteString Char;
"gets lazy bytestring"
  traversedLazy8 = foldring BL8.foldr :: Getting (Endo r) BL8.ByteString Char;
  #-}

-- A way of creating ByteStrings outside the IO monad. The @Int@
-- argument gives the final size of the ByteString. Unlike
-- 'createAndTrim' the ByteString is not reallocated if the final size
-- is less than the estimated size.
unsafeCreate :: Int -> (Ptr Word8 -> IO ()) -> B.ByteString
unsafeCreate l f = unsafeDupablePerformIO (create l f)
{-# INLINE unsafeCreate #-}

-- Create ByteString of size @l@ and use action @f@ to fill its contents.
create :: Int -> (Ptr Word8 -> IO ()) -> IO B.ByteString
create l f = do
    fp <- mallocPlainForeignPtrBytes l
    withForeignPtr fp $ \p -> f p
    return $! BI.PS fp 0 l
{-# INLINE create #-}

foldrChunks :: (B.ByteString -> r -> r) -> r -> BL.ByteString -> r
#if MIN_VERSION_bytestring(0,10,0)
foldrChunks = BL.foldrChunks
#else
foldrChunks f z b = foldr f z (BL.toChunks b)
#endif
{-# INLINE foldrChunks #-}

w2c :: Word8 -> Char
w2c = unsafeChr . fromIntegral
{-# INLINE w2c #-}

c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}
