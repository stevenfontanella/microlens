{-# LANGUAGE
CPP,
FlexibleInstances,
Safe
  #-}


{- |
Module      :  Lens.Micro.Platform.Internal
Copyright   :  (C) 2013-2016 Edward Kmett, 2015-2016 Artyom
License     :  BSD-style (see the file LICENSE)
-}
module Lens.Micro.Platform.Internal
(
  IsText(..),
)
where


import Lens.Micro

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif


class IsText t where
  {- |
'packed' lets you convert between 'String' and @Text@ (strict or lazy). It can be used as a replacement for @pack@ or as a way to modify some 'String' if you have a function like @Text -> Text@.
  -}
  packed :: Lens' String t

  {- |
'unpacked' is like 'packed' but works in the opposite direction.
  -}
  unpacked :: Lens' t String

instance IsText String where
  packed = id
  {-# INLINE packed #-}
  unpacked = id
  {-# INLINE unpacked #-}

instance IsText T.Text where
  packed f s = T.unpack <$> f (T.pack s)
  {-# INLINE packed #-}
  unpacked f s = T.pack <$> f (T.unpack s)
  {-# INLINE unpacked #-}

instance IsText TL.Text where
  packed f s = TL.unpack <$> f (TL.pack s)
  {-# INLINE packed #-}
  unpacked f s = TL.pack <$> f (TL.unpack s)
  {-# INLINE unpacked #-}
