# microlens

[![Build status](https://secure.travis-ci.org/aelve/microlens.svg)](http://travis-ci.org/aelve/microlens)
![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)

*A tiny part of the lens library with no dependencies.*

## If you're completely new to this whole lenses thing

Read [this tutorial](http://hackage.haskell.org/package/lens-tutorial/docs/Control-Lens-Tutorial.html). It's for [lens][], but it applies to microlens just as well (except for module names).

## What is microlens?

microlens is a lens library, just like [lens][], but smaller. It provides essential lenses and traversals (like `_1` and `_Just`), as well as ones which are simply nice to have (like `each`, `at`, and `ix`), and some combinators (like `failing` and `singular`), but everything else is stripped. As the result, microlens has no dependencies. However, there are also separate packages ([microlens-ghc][] and [microlens-platform][]) which provide additional instances and let you use `each` and friends with various container types.

| Package            | Build time with dependencies | Pure build time |
| ------------------ | ----------------------------:| ---------------:|
| **lens**           |                    **4m10s** |       **1m12s** |
| microlens          |                           3s |              3s |
| microlens-th       |                         7.2s |            4.5s |
| microlens-ghc      |                         5.7s |            3.3s |
| microlens-mtl      |                         8.8s |            3.7s |
| microlens-platform |                        1m47s |            4.9s |
| microlens-contra   |                        1m12s |            2.1s |
| microlens-aeson    |                        3m47s |            9.2s |

Other features:

  * Nicer documentation.

  * Compatibility with lens. If you want to define a `Lens` or a `Traversal` in your package, you can depend on this package without fear.

  * No awkward renamed functions or any of such nonsense. You can at any moment replace `Lens.Micro` with `Control.Lens` and get the full power of lens. There are also no unique to microlens functions which you would have to rewrite when switching to lens (even tho I was tempted to add some).

  * No Template Haskell dependency. There is a separate package for generating (lens-compatible) record lenses, which is called [microlens-th][].

  * All `INLINE` pragmas sprinkled thru lens were preserved, as well as flags from the `.cabal` file. Performance shouldn't suffer; if it does, it's a bug.

[microlens]: http://hackage.haskell.org/package/microlens
[microlens-mtl]: http://hackage.haskell.org/package/microlens-mtl
[microlens-th]: http://hackage.haskell.org/package/microlens-th
[microlens-ghc]: http://hackage.haskell.org/package/microlens-ghc
[microlens-platform]: http://hackage.haskell.org/package/microlens-platform
[microlens-contra]: http://hackage.haskell.org/package/microlens-contra
[microlens-aeson]: http://hackage.haskell.org/package/microlens-aeson

## Migration guide

[ilist]: https://github.com/aelve/ilist

  * If you use `ALens`, indexed traversals, prisms, isomorphisms, or `Wrapped`, you won't be able to migrate. (Tho some indexed functions are available elsewhere – containers and vector provide them, and [ilist][] provides indexed functions for lists.)

  * If you have your own instances of `Each`, `At`, `Ix`, `Zoomed`, or `Field*`, and you don't export them, it's okay. Otherwise you should keep using lens, since those classes are incompatible with classes defined in lens. Similarly, if you export any functions with `At`/`Zoom`/etc constraints, don't migrate.

  * If you export `Getter`s or `Fold`s, you would have to use [microlens-contra][] for full compatibility, and it has more heavy dependencies (but still much less heavy than lens). “Full compatibility” here means that some lens functions (such as `takingWhile`) don't work with `SimpleGetter` and `SimpleFold` available from the main microlens package.

  * In the very rare case of using `makeLensesWith` and having `generateUpdateableOptics` disabled, you'd have to apply `fromSimpleFold` and `fromSimpleGetter` to folds/getters you export. Same with fields that have a `forall.` in them.

Otherwise, everything should work just fine without any code changes – the microlens API mirrors the lens API. The license is the same, too.

(The list might look big, but in reality it isn't and in the majority of cases you'll be able to migrate just fine. “If it compiles and you didn't have to change any type signatures, it works.”)

If you're unsure, just open an issue in your project, mention me (@neongreen), and I'll look at your code and tell you whether it'll work or not.

## All packages in the family

  * [microlens][] – all basic functionality, plus `each`/`at`/`ix`
  * [microlens-mtl][] – `+=` and friends, `use`, `zoom`/`magnify`
  * [microlens-th][] – `makeLenses` and `makeFields`
  * [microlens-ghc][] – everything in microlens + instances to make `each`/`at`/`ix` usable with arrays, `ByteString`, and containers
  * [microlens-platform][] – microlens-ghc + microlens-mtl + microlens-th + instances for `Text`, `Vector`, and `HashMap`
  * [microlens-contra][] – `Fold` and `Getter` that are copies of types in lens (the reason they're in a separate library is that those types depend on [contravariant][])

[contravariant]: http://hackage.haskell.org/package/contravariant

Unofficial:

  * [microlens-aeson][] – a port of [lens-aeson][]

[lens-aeson]: http://hackage.haskell.org/package/lens-aeson

If you're writing a library, use [microlens][] and other packages as needed; if you're writing an application, perhaps use [microlens-platform][].

Versions of microlens-ghc and microlens-platform are incremented whenever versions of their dependencies are incremented, so if you're using these packages it's always enough to specify just their versions and nothing else. In other words, there's no risk of the following happening:

  * a new version of microlens is released, with several functions removed
  * version of microlens-platform stays the same
  * your code silently stops compiling as the result

## Competitors

  * [basic-lens][] – the smallest library ever, containing only `Lens`, `view`, `set`, and `over` (and no lenses whatsoever). Uses only 1 extension – `RankNTypes` – and thus can be used with e.g. JHC and really old GHCs.

  * [reasonable-lens][] – a bigger library which has `Lens`, some utilities (like `view`, `use`, `+=`), `makeLenses` even, but little else – no lenses (except for `_1` ... `_4`), no `Traversal`, no documentation. Overall it looks like something slapped together in a hurry by someone who simply needed to get rid of a lens dependency in one of nir projects.

  * [lens-simple][] – a single module reexporting parts of [lens-family][]. It's the most feature-complete library on this list, with both `Lens` and `Traversal` available, as well as a number of lenses, traversals, and utilities. However, it has some annoyances – no `each`, `_1` and `_2` work only on pairs, `ix` doesn't work on lists or arrays and is thus useless, `at` only works on `Map`, etc. I don't think these will ever be fixed, as they require defining some ad-hoc typeclasses, and the current absence of any such typeclasses in lens-family seems to suggest that the authors consider it a bad idea.

  * [data-lens-light][] – a library which uses a different formulation of lenses and is thus incompatible with lens (it uses different names, too). Doesn't actually provide any lenses.

[basic-lens]: http://hackage.haskell.org/package/basic-lens
[reasonable-lens]: http://hackage.haskell.org/package/reasonable-lens
[lens-simple]: http://hackage.haskell.org/package/lens-simple
[lens-family]: http://hackage.haskell.org/package/lens-family
[data-lens-light]: http://hackage.haskell.org/package/data-lens-light

So, I recommend:

  * [lens-simple][] if you specifically want a library with a clean, understandable implementation, even if it's sometimes more cumbersome to use and can be a bit slower.

  * [lens-family][] if you like [lens-simple][] but don't want the Template Haskell dependency.

  * [lens][] if you use anything that's not included in [microlens][].

  * [microlens][] otherwise.

## What's bad about this package

I hate it when people advertise things without also describing their disadvantages, so I'll list the ones I can think of here.

  * No prisms, no isomorphisms, no indexed traversals, and probably never will be.

  * This package doesn't actually let you write everything full lens-style (for instance, there are few operators, no myriads of functions generalised for lenses by adding the `Of` suffix aren't included, etc). On the other hand, I guess some would actually consider it an advantage. Anyway, if you want to use lens as a *language* instead of as a tool, you probably can afford depending on the full package.

  * There are orphan instances, e.g. in the [microlens-ghc][] package. (However, the only way someone can actually break things is by using `Lens.Micro.Internal` and ignoring the warnings there, so I think it's not a huge danger.)

  * There are `set` and `over` in the basic module (i.e. `Lens.Micro`), but `view` lives in `Lens.Micro.Extras` and it doesn't work in `MonadReader` (and the version that does is in [microlens-mtl][]).

  * `makeLenses` can generate `SimpleFold` and `SimpleGetter` which are sli-ightly less general that `Fold` and `Getter` in [lens][]. (If you're a lens user, you still can convert from those versions to fully general versions, so you're not doomed or anything – it's just a minor nuisance / opportunity for confusion. Also, [microlens-contra][] provides true `Fold` and `Getter`.)

  * The implementation is as cryptic/complicated as [lens][]'s (performance has its costs).

## Design decisions

microlens doesn't include anything lens doesn't include, even tho sometimes I'm very tempted to improve something in microlens just because I have control over it.

-----------------------------------------------------------------------------

All the `*Of` functions aren't included. If you don't know, those are `sumOf`, `lengthOf`, `setOf`, etc., and they are roughly equivalent to following:

~~~ haskell
sumOf    l s = sum          (s ^.. l)
lengthOf l s = length       (s ^.. l)
setOf    l s = Set.fromList (s ^.. l)
~~~

(Where `^..` takes something which extracts several targets, and returns a list of those targets. E.g. `(1, 2) ^.. both` is `[1, 2]`).

I guess the reason for including them all into `lens` (and there's an awful lot of them) is somewhere between

  * “they are faster than going thru intermediate lists”
  * “there are some rare cases when you can use a SomeSpecialisedMonoid but can't use `Endo [a]`”
  * “it's nice to be able to say `sumOf (each._1) [(1,"x"),(2,"y")]` instead of clumsy `sum . (^.. each._1) $ [(1,"x"),(2,"y")]`”

I suspect that the last reason is the most important one. The last reason is also the one I dislike most.

There are lots of functions which work on lists; lists are something like “the basic collection/stream type” in Haskell. GHC tries a lot to optimise code which produces and consumes lists; admittedly, it doesn't always succeed. `lens` seems to be trying to sidestep this whole list machinery.

  * With lists: one function traverses something and extracts a list of results, another function does something to those results.

  * With lenses: one function traverses something and takes another function as a parameter (to know what to do with results). Note that here `each._1` is the traversing function; it seems like `sumOf` takes it as a parameter, but in reality `sumOf` merely gives “summation” as the parameter to the traversing function.

The latter way is theoretically nicer, but *not* when you've got the rest of huge ecosystem using lists as the preferred way of information flow, otherwise you're bound to keep rewriting all functions and adding `Of` to them. `lens` is good for creating functions which extract data, and for creating functions which update structures (nested records, etc.), but it's probably not good enough to make the whole world want to switch to writing lens-compatible *consumers* of data.

-----------------------------------------------------------------------------

All those `<>~` and `+~` operators aren't included. Operators like `+=` or `.=` are available from [microlens-mtl][]. The only operators available from `Lens.Micro` are `&`, `%~`, `.~`, and their `<<` variants (`<%~`, `<<%~`, `<<.~`) which I thought could be useful sometimes and yet they aren't trivial to write by yourself.

-----------------------------------------------------------------------------

`Prism` and `Iso` aren't included, as their definitions depend on `Profunctor` and I don't want to depend on [profunctors][]. For now prisms/isos which are included are actually just traversals.

[profunctors]: http://hackage.haskell.org/package/profunctors

For the same reason nothing indexed is included, since it's impossible to get `Conjoined` without adding a pile of dependencies:

~~~
class ( Choice p, Corepresentable p
      , Comonad (Corep p), Traversable (Corep p)
      , Strong p, Representable p
      , Monad (Rep p), MonadFix (Rep p)
      , Distributive (Rep p)
      , ArrowLoop p, ArrowApply p, ArrowChoice p
      ) 
      => Conjoined p

class Conjoined p => Indexable i p
~~~

There'd definitely be prisms if `Profunctor` and `Choice` were in base, but [it's complicated](https://www.reddit.com/r/haskell/comments/3kbj9r/edward_kmett_the_unreasonable_effectiveness_of/cuwucle). Another option is creating a package containing only classes (`Profunctor`, `Choice`, `Contravariant`, etc) and letting everyone else depend on it, but that leads to a proliferation of packages (I still think it'd be a good thing to do in this case, but, admittedly, I've also spent more time complaining about it than the issue actually deserves).

For now, if you need to export prisms, you can either depend on [lens][] or just depend on [profunctors][] and define `Prism` locally:

~~~ haskell
-- Shouldn't be exported by your library.
type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap seta (either pure (fmap bt)) . right'
{-# INLINE prism #-}
~~~

-----------------------------------------------------------------------------

Instances of `Ixed`, `Each`, `At`, etc are all split off into separate packages, which is understandable, because otherwise we'd have to have [vector][] as a dependency (the alternative is having orphan instances, which I'm not particularly afraid of). However, even instances for libraries shipped with GHC (such as [array][]) are in [their own package][microlens-ghc]. There are 2 reasons for this:

* I *really* want to be able to say “this library has no dependencies”.
* All those instances actually take quite some time to build (for the same reason not all instances for tuples are included in the main package).

[array]: http://hackage.haskell.org/package/array

## Motivation

[lens][] is awesome. It's also huge, and requires lots of dependencies; not only [vector][] and [text][] (which you probably have anyway), but:

  * bifunctors
  * comonad
  * contravariant
  * distributive
  * exceptions
  * free
  * hashable
  * primitive
  * profunctors
  * reflection
  * semigroupoids
  * semigroups
  * split
  * tagged
  * transformers-compat
  * unordered-containers
  * void

[lens]: http://hackage.haskell.org/package/lens
[vector]: http://hackage.haskell.org/package/vector
[text]: http://hackage.haskell.org/package/text

Some packages – like small libraries – can't afford having a huge list of dependencies, and this means that their authors don't get to enjoy the benefits of lenses (I'm not talking about exporting lenses, I'm talking about using lenses to write code). Of course, a lot of people don't care about build times, or might even argue that people who *do* care about them are “wrong”, but it doesn't change the fact that having a miniature lenses library would lead to more people being able to use lenses in their code.

So, microlens attempts to be a library that would be a nearly *unquestionable* win for some people. When there are no tradeoffs, the choice becomes much easier.

## What about lens-family?

[lens-family][] is another small lenses library which is mostly compatible with lens (unless I decide to nitpick and say that its `makeLensesBy` and `intAt` aren't present in lens at all), which has few dependencies, and which provides Template Haskell in a separate package as well.

[lens-family]: http://hackage.haskell.org/package/lens-family

It looks like lens-family values cleanness and simplicity, which unfortunately means that it might've been hard for me (if possible at all) to convince its maintainer to make changes which would bring it closer to lens (`INLINE` pragmas, using unsafe `#.` operator, adding `each`, etc). I actually like cleanness and dislike excessive optimisation (especially of the kind that is used in lens) too, but making a library *I* would like wasn't my goal. The goal was to push people who aren't using a lens library towards using one.

Yep, in a way it's [NIH](https://en.wikipedia.org/wiki/Not_invented_here). However, I think that in this case in this case NIH is justified, if only because most reasons NIH is bad (time spent rewriting the library could've been spent improving another library, different libraries are incompatible with each other and so the community is fractured, etc) don't really apply here.
