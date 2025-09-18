{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Lens.Micro.FieldN where

import Lens.Micro.Type

class Field1 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  {- |
Gives access to the 1st field of a tuple (up to 5-tuples).

Getting the 1st component:

>>> (1,2,3,4,5) ^. _1
1

Setting the 1st component:

>>> (1,2,3) & _1 .~ 10
(10,2,3)

Note that this lens is lazy, and can set fields even of 'undefined':

>>> set _1 10 undefined :: (Int, Int)
(10,*** Exception: Prelude.undefined

This is done to avoid violating a lens law stating that you can get back what you put:

>>> view _1 . set _1 10 $ (undefined :: (Int, Int))
10

The implementation (for 2-tuples) is:

@
'_1' f t = (,) '<$>' f    ('fst' t)
             '<*>' 'pure' ('snd' t)
@

or, alternatively,

@
'_1' f ~(a,b) = (\\a' -> (a',b)) '<$>' f a
@

(where @~@ means a <https://wiki.haskell.org/Lazy_pattern_match lazy pattern>).

'_2', '_3', '_4', and '_5' are also available (see below).
  -}
  _1 :: Lens s t a b

instance Field1 (a,b) (a',b) a a' where
  _1 k ~(a,b) = (\a' -> (a',b)) <$> k a
  {-# INLINE _1 #-}

instance Field1 (a,b,c) (a',b,c) a a' where
  _1 k ~(a,b,c) = (\a' -> (a',b,c)) <$> k a
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d) (a',b,c,d) a a' where
  _1 k ~(a,b,c,d) = (\a' -> (a',b,c,d)) <$> k a
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d,e) (a',b,c,d,e) a a' where
  _1 k ~(a,b,c,d,e) = (\a' -> (a',b,c,d,e)) <$> k a
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d,e,f) (a',b,c,d,e,f) a a' where
  _1 k ~(a,b,c,d,e,f) = (\a' -> (a',b,c,d,e,f)) <$> k a
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d,e,f,g) (a',b,c,d,e,f,g) a a' where
  _1 k ~(a,b,c,d,e,f,g) = (\a' -> (a',b,c,d,e,f,g)) <$> k a
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d,e,f,g,h) (a',b,c,d,e,f,g,h) a a' where
  _1 k ~(a,b,c,d,e,f,g,h) = (\a' -> (a',b,c,d,e,f,g,h)) <$> k a
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d,e,f,g,h,i) (a',b,c,d,e,f,g,h,i) a a' where
  _1 k ~(a,b,c,d,e,f,g,h,i) = (\a' -> (a',b,c,d,e,f,g,h,i)) <$> k a
  {-# INLINE _1 #-}

class Field2 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _2 :: Lens s t a b

instance Field2 (a,b) (a,b') b b' where
  _2 k ~(a,b) = (\b' -> (a,b')) <$> k b
  {-# INLINE _2 #-}

instance Field2 (a,b,c) (a,b',c) b b' where
  _2 k ~(a,b,c) = (\b' -> (a,b',c)) <$> k b
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d) (a,b',c,d) b b' where
  _2 k ~(a,b,c,d) = (\b' -> (a,b',c,d)) <$> k b
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d,e) (a,b',c,d,e) b b' where
  _2 k ~(a,b,c,d,e) = (\b' -> (a,b',c,d,e)) <$> k b
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d,e,f) (a,b',c,d,e,f) b b' where
  _2 k ~(a,b,c,d,e,f) = (\b' -> (a,b',c,d,e,f)) <$> k b
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d,e,f,g) (a,b',c,d,e,f,g) b b' where
  _2 k ~(a,b,c,d,e,f,g) = (\b' -> (a,b',c,d,e,f,g)) <$> k b
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d,e,f,g,h) (a,b',c,d,e,f,g,h) b b' where
  _2 k ~(a,b,c,d,e,f,g,h) = (\b' -> (a,b',c,d,e,f,g,h)) <$> k b
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d,e,f,g,h,i) (a,b',c,d,e,f,g,h,i) b b' where
  _2 k ~(a,b,c,d,e,f,g,h,i) = (\b' -> (a,b',c,d,e,f,g,h,i)) <$> k b
  {-# INLINE _2 #-}

class Field3 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _3 :: Lens s t a b

instance Field3 (a,b,c) (a,b,c') c c' where
  _3 k ~(a,b,c) = (\c' -> (a,b,c')) <$> k c
  {-# INLINE _3 #-}

instance Field3 (a,b,c,d) (a,b,c',d) c c' where
  _3 k ~(a,b,c,d) = (\c' -> (a,b,c',d)) <$> k c
  {-# INLINE _3 #-}

instance Field3 (a,b,c,d,e) (a,b,c',d,e) c c' where
  _3 k ~(a,b,c,d,e) = (\c' -> (a,b,c',d,e)) <$> k c
  {-# INLINE _3 #-}

instance Field3 (a,b,c,d,e,f) (a,b,c',d,e,f) c c' where
  _3 k ~(a,b,c,d,e,f) = (\c' -> (a,b,c',d,e,f)) <$> k c
  {-# INLINE _3 #-}

instance Field3 (a,b,c,d,e,f,g) (a,b,c',d,e,f,g) c c' where
  _3 k ~(a,b,c,d,e,f,g) = (\c' -> (a,b,c',d,e,f,g)) <$> k c
  {-# INLINE _3 #-}

instance Field3 (a,b,c,d,e,f,g,h) (a,b,c',d,e,f,g,h) c c' where
  _3 k ~(a,b,c,d,e,f,g,h) = (\c' -> (a,b,c',d,e,f,g,h)) <$> k c
  {-# INLINE _3 #-}

instance Field3 (a,b,c,d,e,f,g,h,i) (a,b,c',d,e,f,g,h,i) c c' where
  _3 k ~(a,b,c,d,e,f,g,h,i) = (\c' -> (a,b,c',d,e,f,g,h,i)) <$> k c
  {-# INLINE _3 #-}

class Field4 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _4 :: Lens s t a b

instance Field4 (a,b,c,d) (a,b,c,d') d d' where
  _4 k ~(a,b,c,d) = (\d' -> (a,b,c,d')) <$> k d
  {-# INLINE _4 #-}

instance Field4 (a,b,c,d,e) (a,b,c,d',e) d d' where
  _4 k ~(a,b,c,d,e) = (\d' -> (a,b,c,d',e)) <$> k d
  {-# INLINE _4 #-}

instance Field4 (a,b,c,d,e,f) (a,b,c,d',e,f) d d' where
  _4 k ~(a,b,c,d,e,f) = (\d' -> (a,b,c,d',e,f)) <$> k d
  {-# INLINE _4 #-}

instance Field4 (a,b,c,d,e,f,g) (a,b,c,d',e,f,g) d d' where
  _4 k ~(a,b,c,d,e,f,g) = (\d' -> (a,b,c,d',e,f,g)) <$> k d
  {-# INLINE _4 #-}

instance Field4 (a,b,c,d,e,f,g,h) (a,b,c,d',e,f,g,h) d d' where
  _4 k ~(a,b,c,d,e,f,g,h) = (\d' -> (a,b,c,d',e,f,g,h)) <$> k d
  {-# INLINE _4 #-}

instance Field4 (a,b,c,d,e,f,g,h,i) (a,b,c,d',e,f,g,h,i) d d' where
  _4 k ~(a,b,c,d,e,f,g,h,i) = (\d' -> (a,b,c,d',e,f,g,h,i)) <$> k d
  {-# INLINE _4 #-}

class Field5 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _5 :: Lens s t a b

instance Field5 (a,b,c,d,e) (a,b,c,d,e') e e' where
  _5 k ~(a,b,c,d,e) = (\e' -> (a,b,c,d,e')) <$> k e
  {-# INLINE _5 #-}

instance Field5 (a,b,c,d,e,f) (a,b,c,d,e',f) e e' where
  _5 k ~(a,b,c,d,e,f) = (\e' -> (a,b,c,d,e',f)) <$> k e
  {-# INLINE _5 #-}

instance Field5 (a,b,c,d,e,f,g) (a,b,c,d,e',f,g) e e' where
  _5 k ~(a,b,c,d,e,f,g) = (\e' -> (a,b,c,d,e',f,g)) <$> k e
  {-# INLINE _5 #-}

instance Field5 (a,b,c,d,e,f,g,h) (a,b,c,d,e',f,g,h) e e' where
  _5 k ~(a,b,c,d,e,f,g,h) = (\e' -> (a,b,c,d,e',f,g,h)) <$> k e
  {-# INLINE _5 #-}

instance Field5 (a,b,c,d,e,f,g,h,i) (a,b,c,d,e',f,g,h,i) e e' where
  _5 k ~(a,b,c,d,e,f,g,h,i) = (\e' -> (a,b,c,d,e',f,g,h,i)) <$> k e
  {-# INLINE _5 #-}

class Field6 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _6 :: Lens s t a b

instance Field6 (a,b,c,d,e,f) (a,b,c,d,e,f') f f' where
  _6 k ~(a,b,c,d,e,f) = (\f' -> (a,b,c,d,e,f')) <$> k f
  {-# INLINE _6 #-}

instance Field6 (a,b,c,d,e,f,g) (a,b,c,d,e,f',g) f f' where
  _6 k ~(a,b,c,d,e,f,g) = (\f' -> (a,b,c,d,e,f',g)) <$> k f
  {-# INLINE _6 #-}

instance Field6 (a,b,c,d,e,f,g,h) (a,b,c,d,e,f',g,h) f f' where
  _6 k ~(a,b,c,d,e,f,g,h) = (\f' -> (a,b,c,d,e,f',g,h)) <$> k f
  {-# INLINE _6 #-}

instance Field6 (a,b,c,d,e,f,g,h,i) (a,b,c,d,e,f',g,h,i) f f' where
  _6 k ~(a,b,c,d,e,f,g,h,i) = (\f' -> (a,b,c,d,e,f',g,h,i)) <$> k f
  {-# INLINE _6 #-}

class Field7 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _7 :: Lens s t a b

instance Field7 (a,b,c,d,e,f,g) (a,b,c,d,e,f,g') g g' where
  _7 k ~(a,b,c,d,e,f,g) = (\g' -> (a,b,c,d,e,f,g')) <$> k g
  {-# INLINE _7 #-}

instance Field7 (a,b,c,d,e,f,g,h) (a,b,c,d,e,f,g',h) g g' where
  _7 k ~(a,b,c,d,e,f,g,h) = (\g' -> (a,b,c,d,e,f,g',h)) <$> k g
  {-# INLINE _7 #-}

instance Field7 (a,b,c,d,e,f,g,h,i) (a,b,c,d,e,f,g',h,i) g g' where
  _7 k ~(a,b,c,d,e,f,g,h,i) = (\g' -> (a,b,c,d,e,f,g',h,i)) <$> k g
  {-# INLINE _7 #-}

class Field8 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _8 :: Lens s t a b

instance Field8 (a,b,c,d,e,f,g,h) (a,b,c,d,e,f,g,h') h h' where
  _8 k ~(a,b,c,d,e,f,g,h) = (\h' -> (a,b,c,d,e,f,g,h')) <$> k h
  {-# INLINE _8 #-}

instance Field8 (a,b,c,d,e,f,g,h,i) (a,b,c,d,e,f,g,h',i) h h' where
  _8 k ~(a,b,c,d,e,f,g,h,i) = (\h' -> (a,b,c,d,e,f,g,h',i)) <$> k h
  {-# INLINE _8 #-}

class Field9 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _9 :: Lens s t a b

instance Field9 (a,b,c,d,e,f,g,h,i) (a,b,c,d,e,f,g,h,i') i i' where
  _9 k ~(a,b,c,d,e,f,g,h,i) = (\i' -> (a,b,c,d,e,f,g,h,i')) <$> k i
