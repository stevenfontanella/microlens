{-# LANGUAGE
RankNTypes,
Safe
  #-}


{- |
This module provides just the types ('Lens', 'Traversal', etc). It's needed to break the dependency cycle – "Lens.Micro" depends on "Lens.Micro.Classes", but "Lens.Micro.Classes" needs types like 'Lens', so 'Lens' can't be defined in "Lens.Micro".
-}
module Lens.Micro.Type
(
  ASetter, ASetter',
  Getting,
  Lens, Lens',
  Traversal, Traversal',
  LensLike, LensLike',
)
where


import Control.Applicative
import Data.Functor.Identity


{- |
@ASetter s t a b@ is something that turns a function modifying a value into a function modifying a /structure/. If you ignore 'Identity' (as @Identity a@ is the same thing as @a@), the type is:

@
type ASetter s t a b = (a -> b) -> s -> t
@

This means that examples of setters you might've already seen are:

  * @'map' :: (a -> b) -> [a] -> [b]@

    (which corresponds to 'Lens.Micro.mapped')

  * @'fmap' :: 'Functor' f => (a -> b) -> f a -> f b@

    (which corresponds to 'Lens.Micro.mapped' as well)

  * @'Control.Arrow.first' :: (a -> b) -> (a, x) -> (b, x)@

    (which corresponds to 'Lens.Micro._1')

  * @'Control.Arrow.left' :: (a -> b) -> 'Either' a x -> 'Either' b x@

    (which corresponds to 'Lens.Micro._Left')

The reason 'Identity' is used here is for 'ASetter' to be composable with other types, such as 'Lens'.

Technically, if you're writing a library, you shouldn't use this type for setters you are exporting from your library; the right type to use is @<http://hackage.haskell.org/package/lens/docs/Control-Lens-Setter.html#t:Setter Setter>@, but it is not provided by this package (because then we'd have to depend on <http://hackage.haskell.org/package/distributive distributive>). It's completely alright, however, to export functions which take an 'ASetter' as an argument.
-}
type ASetter s t a b = (a -> Identity b) -> s -> Identity t

{- |
This is a type alias for monomorphic setters which don't change the type of the container (or of the value inside). It's useful more often than the same type in lens, because we can't provide real setters and so it does the job of both @<http://hackage.haskell.org/package/lens/docs/Control-Lens-Setter.html#t:ASetter-39- ASetter'>@ and @<http://hackage.haskell.org/package/lens/docs/Control-Lens-Setter.html#t:Setter-39- Setter'>@.
-}
type ASetter' s a = ASetter s s a a

{- |
If you take a lens or a traversal and choose @'Const' r@ as your functor, you will get @Getting r s a@. This can be used to get something out of the structure instead of modifying it:

@
s 'Lens.Micro.^.' l = 'getConst' (l 'Const' s)
@

Functions that operate on getters – such as ('Lens.Micro.^.'), ('Lens.Micro.^..'), ('Lens.Micro.^?') – use @Getter r s a@ (with different values of @r@) to describe what kind of getter they need. For instance, ('Lens.Micro.^.') needs the getter to be able to return a single value, and so it accepts a getter of type @Getting a s a@. ('Lens.Micro.^..') wants the getter to gather values together, so it uses @Getting (Endo [a]) s a@ (it could've used @Getting [a] s a@ instead, but it's faster with 'Data.Monoid.Endo'). The choice of @r@ depends on what you want to do with elements you're extracting from @s@.
-}
type Getting r s a = (a -> Const r a) -> s -> Const r s

{- |
Lenses in a nutshell: use ('Lens.Micro.^.') to get, ('Lens.Micro..~') to set, ('Lens.Micro.%~') to modify. ('.') composes lenses (i.e. if a @B@ is a part of @A@, and a @C@ is a part of in @B@, then @b.c@ lets you operate on @C@ inside @A@). You can create lenses with 'Lens.Micro.lens', or you can write them by hand (see below).

@Lens s t a b@ is the lowest common denominator of a setter and a getter, something that has the power of both; it has a 'Functor' constraint, and since both 'Const' and 'Identity' are functors, it can be used whenever a getter or a setter is needed.

  * @a@ is the type of the value inside of structure
  * @b@ is the type of the replaced value
  * @s@ is the type of the whole structure
  * @t@ is the type of the structure after replacing @a@ in it with @b@

A 'Lens' can only point at a single value inside a structure (unlike a 'Traversal').

It is easy to write lenses manually. The generic template is:

@
somelens :: Lens s t a b

-- “f” is the “a -> f b” function, “s” is the structure.
somelens f s =
  let
    a = ...                 -- Extract the value from “s”.
    rebuildWith b = ...     -- Write a function which would
                            -- combine “s” and modified value
                            -- to produce new structure.
  in
    rebuildWith '<$>' f a     -- Apply the structure-producing
                            -- function to the modified value.
@

Here's the 'Lens.Micro._1' lens:

@
'Lens.Micro._1' :: 'Lens' (a, x) (b, x) a b
'Lens.Micro._1' f (a, x) = (\\b -> (b, x)) '<$>' f a
@

Here's a more complicated lens, which extracts /several/ values from a structure (in a tuple):

@
type Age     = Int
type City    = String
type Country = String

data Person = Person Age City Country

-- This lens lets you access all location-related information about a person.
location :: 'Lens'' Person (City, Country)
location f (Person age city country) =
  (\\(city', country') -> Person age city' country') '<$>' f (city, country)
@

You even can choose to use a lens to present /all/ information contained in the structure (in a different way). Such lenses are called @<http://hackage.haskell.org/package/lens/docs/Control-Lens-Iso.html#t:Iso Iso>@ in lens's terminology. For instance (assuming you don't mind functions that can error out), here's a lens which lets you act on the string representation of a value:

@
string :: (Read a, Show a) => 'Lens'' a String
string f s = read '<$>' f (show s)
@

Using it to reverse a number:

@
>>> 123 'Lens.Micro.&' string 'Lens.Micro.%~' reverse
321
@
-}
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

{- |
This is a type alias for monomorphic lenses which don't change the type of the container (or of the value inside).
-}
type Lens' s a = Lens s s a a

{- |
Traversals in a nutshell: they're like lenses but they can point at multiple values. Use ('Lens.Micro.^..') to get all values, ('Lens.Micro.^?') to get the 1st value, ('Lens.Micro..~') to set values, ('Lens.Micro.%~') to modify them. ('.') composes traversals just as it composes lenses. ('Lens.Micro.^.') can be used with traversals as well, but don't confuse it with ('Lens.Micro.^..').

@Traversal s t a b@ is a generalisation of 'Lens' which allows many targets (possibly 0). It's achieved by changing the constraint to 'Applicative' instead of 'Functor' – indeed, the point of 'Applicative' is that you can combine effects, which is just what we need to have many targets.

Traversals don't differ from lenses when it comes to setting – you can use usual ('Lens.Micro.%~') and ('Lens.Micro..~') to modify and set values. Getting is a bit different, because you have to decide what to do in the case of multiple values. In particular, you can use these combinators (as well as everything else in the “Folds” section):

  * ('Lens.Micro.^..') gets a list of values
  * ('Lens.Micro.^?') gets the 1st value (or 'Nothing' if there are no values)
  * ('Lens.Micro.^?!') gets the 1st value and throws an exception if there are no values

In addition, ('Lens.Micro.^.') works for traversals as well – it combines traversed values using the ('Data.Monoid.<>') operation (if the values are instances of 'Monoid').

Traversing any value twice is a violation of traversal laws. You can, however, traverse values in any order.

Ultimately, traversals should follow 2 laws:

@
t pure ≡ pure
fmap (t f) . t g ≡ getCompose . t (Compose . fmap f . g)
@

The 1st law states that you can't change the shape of the structure or do anything funny with elements (traverse elements which aren't in the structure, create new elements out of thin air, etc.). The 2nd law states that you should be able to fuse 2 identical traversals into one. For a more detailed explanation of the laws, see <http://artyom.me/lens-over-tea-2#traversal-laws this blog post> (if you prefer rambling blog posts), or <https://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf The Essence Of The Iterator Pattern> (if you prefer papers).
-}
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

{- |
This is a type alias for monomorphic traversals which don't change the type of the container (or of the values inside).
-}
type Traversal' s a = Traversal s s a a

{- |
'LensLike' is a type that is often used to make combinators as general as possible. For instance, take ('Lens.Micro.<<%~'), which only requires the passed lens to be able to work with the @(,) a@ functor (lenses and traversals can do that). The fully expanded type is as follows:

@
('Lens.Micro.<<%~') :: ((a -> (a, b)) -> s -> (a, t)) -> (a -> b) -> s -> (a, t)
@

With 'LensLike', the intent to use the @(,) a@ functor can be made a bit clearer:

@
('Lens.Micro.<<%~') :: LensLike ((,) a) s t a b -> (a -> b) -> s -> (a, t)
@
-}
type LensLike f s t a b = (a -> f b) -> s -> f t

{- |
A type alias for monomorphic 'LensLike's.
-}
type LensLike' f s a = LensLike f s s a a
