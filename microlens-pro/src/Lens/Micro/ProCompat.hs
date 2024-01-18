{-|
Module      : Lens.Micro.ProCompat
Copyright   : (C) 2013-2016 Edward Kmett, 2018 Monadfix
License     : BSD-style (see the file LICENSE)

This module re-exports 'Lens.Micro', overriding the Prisms.
-}
module Lens.Micro.ProCompat
    ( module Lens.Micro
    , module Lens.Micro.Pro
    )
    where
--------------------------------------------------------------------------------

-- everything hidden here is redefined in 'Lens.Micro.Pro'
import Lens.Micro hiding ( _Left, _Right, _Just, _Nothing, _Show
                         , strict, lazy, non
                         )
import Lens.Micro.Pro

