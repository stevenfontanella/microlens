module Lens.Micro.Extras
(
  (+~), (-~), (*~), (//~),
)
where


import Lens.Micro


infixr 4 +~, -~, *~, //~

(+~) :: Num a => ASetter s t a a -> a -> s -> t
l +~ n = over l (+ n)
{-# INLINE (+~) #-}

(*~) :: Num a => ASetter s t a a -> a -> s -> t
l *~ n = over l (* n)
{-# INLINE (*~) #-}

(-~) :: Num a => ASetter s t a a -> a -> s -> t
l -~ n = over l (subtract n)
{-# INLINE (-~) #-}

(//~) :: Fractional a => ASetter s t a a -> a -> s -> t
l //~ n = over l (/ n)
{-# INLINE (//~) #-}
