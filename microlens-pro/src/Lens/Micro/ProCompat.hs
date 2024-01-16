{-|
Module      : Lens.Micro.ProCompat
Copyright   : (C) 2013-2016 Edward Kmett, 2018 Monadfix
License     : BSD-style (see the file LICENSE)

This module re-exports 'Lens.Micro', overriding the Prisms.
-}
module Lens.Micro.ProCompat
(
    (&),
    -- $ampersand-note
    (<&>),
    -- $reverse-fmap-note

    -- * Setter: modifies something in a structure
    -- $setters-note
    ASetter, ASetter',
    sets,
    (%~), over, (+~), (-~),
    (<>~),
    (.~), set,
    (?~),
    (<%~), (<<%~), (<<.~),
    mapped,
    rewriteOf,
    transformOf,

    -- * Getter: extracts a value from a structure
    -- $getters-note
    SimpleGetter,
    Getting,
    (^.),
    to,

    -- * Fold: extracts multiple elements
    -- $folds-note
    SimpleFold,
    (^..), toListOf,
    (^?),
    (^?!),
    traverseOf_,
    forOf_,
    has,
    folded,
    folding,

    -- * Lens: a combined getter-and-setter
    -- $lenses-note
    Lens, Lens',
    lens,
    at,
    _1, _2, _3, _4, _5,

    -- * Iso: a lens that only changes the representation
    -- $isos-note
    Iso, Iso',
    strict, lazy,
    non,

    -- * Traversal: a lens iterating over several elements
    -- $traversals-note
    Traversal, Traversal',
    traverseOf,
    forOf,
    singular,
    failing,
    filtered,
    both,
    traversed,
    each,
    ix,
    _head, _tail, _init, _last,
    mapAccumLOf,
    worded, lined,

    -- * Prism: a traversal iterating over at most 1 element
    -- $prisms-note
    Prism, Prism',
    _Left, _Right,
    _Just, _Nothing,
    _Show,

    -- * Other types
    LensLike, LensLike',
)
where
--------------------------------------------------------------------------------

-- everything hidden here is redefined in 'Lens.Micro.Pro'
import Lens.Micro hiding ( _Left, _Right, _Just, _Nothing, _Show
                         , strict, lazy, non
                         )
import Lens.Micro.Pro

