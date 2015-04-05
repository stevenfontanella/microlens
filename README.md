# microlens

[![Build status](https://secure.travis-ci.org/aelve/microlens.svg)](http://travis-ci.org/aelve/microlens)

*A tiny part of the `lens` library which you can depend upon.*

## What you get

  * Essential lenses and traversals.

  * Nice, comprehensive documentation; a tutorial for people who haven't ever seen lenses before (like the embedded tutorial in the pipes package); and a complementary set of exercises at School Of Haskell (still in progress).

  * Compatibility with `lens`. If you want to define a `Lens` or a `Traversal` in your package, you can depend on this package without fear. (Documentation for `Lens` and `Traversal` here refers the user to `lens` package as well, which is done so that you wouldn't have to explain that while you're depending on `microlens`, you're probably intending your lenses to be used with `lens`, which is more powerful.)

  * Only **1** dependency (`mtl`). And the whole package builds in ~4s on my laptop. (There are plans to get rid even of this single dependency, however.)

  * No awkward renamed functions or any of such nonsense. You can at any moment replace `Lens.Micro` with `Control.Lens` and get the full power of `lens`.

  * No Template Haskell dependency. There is a separate package for generating (`lens`-compatible) record lenses, which is called [`microlens-th`][]. It takes ~6s to build and it doesn't require any special dependencies either.

  * All `INLINE` pragmas sprinkled thru `lens` were preserved, as well as flags from the `.cabal` file. Performance shouldn't suffer; if it does, it's a bug.

  * `microlens` should also be compilable with JHC, but I haven't checked. Tell me if it works!

[`microlens-th`]: http://hackage.haskell.org/package/microlens-th

## Tutorial for newcomers to the whole lens thing

Just look at <http://hackage.haskell.org/package/microlens/docs/Lens-Micro-Tutorial.html>.

## Design decisions

(Or rather “why I don't think you need this” decisions.)

---

All the `*Of` functions aren't included. If you don't know, those are `sumOf`, `lengthOf`, `setOf`, etc., and they are roughly equivalent to following:

~~~ haskell
sumOf    l = sum          . toListOf l
lengthOf l = length       . toListOf l
setOf    l = Set.fromList . toListOf l
~~~

(Where `toListOf` takes something which extracts several targets, and returns a list of those targets. E.g. `toListOf both (1, 2)` is `[1, 2]`).

I guess the reason for including them all into `lens` (and there's an awful lot of them) is somewhere between

  * “they are faster than going thru intermediate lists”
  * “there are some rare cases when you can use a SomeSpecialisedMonoid but can't use `Endo [a]`”
  * “it's nice to be able to say `sumOf (each._1) [(1,"x"),(2,"y")]` instead of clumsy `sum . (^.. each._1) $ [(1,"x"),(2,"y")]`”

I suspect that the last reason is the most important one. The last reason is also the one I dislike most.

There are lots of functions which work on lists; lists are something like “the basic collection/stream type” in Haskell. GHC tries a lot to optimise code which produces and consumes lists; admittedly, it doesn't always succeed. `lens` seems to be trying to sidestep this whole list machinery.

  * With lists: one function traverses something and extracts a list of results, another function does something to those results.

  * With lenses: one function traverses something and takes another function as a parameter (to know what to do with results). Note that here `each._1` is the traversing function; it seems like `sumOf` takes it as a parameter, but in reality `sumOf` merely gives “summation” as the parameter to the traversing function.

The latter way is theoretically nicer, but *not* when you've got the rest of huge ecosystem using lists as the preferred way of information flow, otherwise you're bound to keep rewriting all functions and adding `Of` to them. `lens` is good for creating functions which extract data, and for creating functions which update structures (nested records, etc.), but it's not good enough to make the whole world want to switch to writing lens-compatible *consumers* of data.

So, don't write `Of` functions yet, it's not their time. Also, this suffix is uglyuglyugly.

---

All those `+=` and `<<&&=` operators aren't included. My opinion is that they shouldn't exist, and that all code for which it's “really nice” to have all those operators available should either be using normal operators instead (if it's an open-source project with code meant to be read by others), or should define them locally (if it's production code), or *at least* import them implicitly from some module called `Weird.Operators`. (It's a pity Haskell doesn't have something like "operator modifiers"; it would solve this problem nicely, as well as “let's have lifted versions of `==` and stuff because `liftA2 (==)` is too long” and other similar ones. You can use Hayoo to find out how many different versions of `==` there are on Hackage.)

Moreover, `+=` and friends steal lots of nice operator names. A search for `+=` over Github shows that pretty much the only package which uses it in the `lens` meaning is `lens` itself.

`.=` conflicts with pretty much everything (especially `aeson`).

For these reasons, operators are available from `Lens.Micro.Extras`, but not from the main module. (Not the full `lens` set, just the most useful ones.) The only operators available from `Lens.Micro` are `%~`, `.~` and `&`.

---

`Prism` and `Iso` aren't included. Long story short, I can't afford depending on `profunctors`. This is explained better in the [Motivation][] section. It'd be cool if I could include them, but for now isos are nonexistent and those prisms/isos which are included are actually just traversals.

---

Nothing indexed is included.

~~~
class ( Choice p
      , Corepresentable p
      , Comonad (Corep p)
      , Traversable (Corep p)
      , Strong p
      , Representable p
      , Monad (Rep p)
      , MonadFix (Rep p)
      , Distributive (Rep p)
      , ArrowLoop p
      , ArrowApply p
      , ArrowChoice p
      ) 
      => Conjoined p

class Conjoined p => Indexable i p
~~~

Perhaps it is for the best.

---

`ix` and `at` aren't included, because they are pretty useless without instances for `Map`, `Vector`, `Text`, etc., and this would require lots of dependencies. In an ideal world, we would have `Ixed` and `At` defined in `microlens`, and `vector`, `text`, `unordered-containers`, etc. would depend on `microlens` and provide instances. For this to happen:

  * `microlens` itself must remain dependency-free. It's not free now (due to the `mtl` dependency, which might be something `vector` maintainers won't agree to), but it will be once everything which doesn't depend on `mtl` is moved into something like `baselens` (better name suggestions are welcome), which should happen anyway because `mtl` isn't the only monad transformers library and I've been in the past hindered by not being able to use `view`, `use`, etc. with transformers libraries like `monad-classes`.

  * Someone has to convince conservative maintainers of various container libraries to agree to a `microlens` dependency. The best way to get them to agree is probably to start using the library a lot, and providing lenses and instances of classes like `Ixed` and `At` in all your packages. Pull requests to others' packages might work, too.

---

`zoom` isn't included yet, but should be.

---

The module with extra functions and operators is called `Lens.Micro.Extras` instead of `Lens.Micro.Extra`, as plurals seem to be more common on Hackage than singulars (e.g. look for `Type` vs. `Types`). Moreover, the corresponding module of `lens` is named `Control.Lens.Extras`.

## Motivation

[`lens`][] is awesome. It's also huge, and requires lots of dependencies; not only `vector` and `text` (which you probably have anyway), but:

  * `bifunctors`
  * `comonad`
  * `contravariant`
  * `distributive`
  * `exceptions`
  * `free`
  * `hashable`
  * `primitive`
  * `profunctors`
  * `reflection`
  * `semigroupoids`
  * `semigroups`
  * `split`
  * `tagged`
  * `transformers-compat`
  * `unordered-containers`
  * `void`

[`lens`]: http://hackage.haskell.com/package/lens

Each of these packages is a nice one, solving a well-defined task, or providing a useful abstraction. Most of category-theoretical ones (`bifunctors`, etc.) only take seconds to install. However, having to depend on them all is a death by a thousand papercuts. It's alright for an application, but it's absolutely not alright for a library... until `lens` becomes a part of the Haskell Prelude, or subsumes it entirely. (If this was Edward Kmett's plan all along: sorry for spoiling it!)

What Edward wants is a huge, huge, huge Swiss knife. And ne is smart and strong enough to carry such a knife with nem and benefit from it. I imagine that there are lots of applications in the world which depend on `lens` and use more than 20% of its possibilities; even if not, creating such a Swiss knife is an entirely legitimate goal in itself.

What I want is a small knife to carry in my pocket. I actually have one in mine right now; it's just as sharp as other knives, but it's 5cm long and it contains scissors, a toothpick, tweezers, and a nail file. It's not enough for all purposes, or for even *most* purposes; but the cool thing is that I never have to consider taking it out of the pocket. Choosing to carry it isn't ever a tradeoff.

If you don't like knives (I don't either, I actually only ever use small scissors), consider an alternative analogy: `lens` is Emacs or Microsoft Word, `microlens` is nano (or Notepad) (possibly with Emacs's bindings, tho Emacs's default bindings actually suck, so it's not such a good analogy after all).

### But hasn't Edward said splitting `lens` into several packages is impossible?

Yes, but it's not what I'm trying to do. What Edward said (...or I think ne said), in a nutshell, is:

> You can't make a nice library with prisms and isos and everything without tying yourself to GHC.

Yes, but it doesn't matter because lots of people don't mind being tied to GHC. It's like saying that you shouldn't play World Of Warcraft because then you'll be tied to Windows. So what? People play World Of Warcraft anyway.

Moreover, I can't include `Prism` and `Iso` anyway without depending on `profunctors`, which depends on all other Edward's small packages, because they all provide instances for each other's types, because there's a useful type called `Tambara`, and it's important that there are instances like `Profunctor p => Strong (Tambara p)`. Like a big, happy family, Edward's packages all hold to each other.

I don't know why can't Edward just make a separate package called `classes`, where `Profunctor` and `Contravariant` would reside along with their *only* instances people ever use, namely `Profunctor (->)` and `Contravariant (Const a)`, and all nir other packages would just depend on `classes` and provide useful `Tambara` and `Cotambara` instances, and then `microlens` could also depend on `classes` and provide prisms and isos. There's probably a good reason.

> Splitting `lens` would either lead to orphaned instances or loss of functionality.

Agreed. Splitting anything reasonably complex into several parts would lead to loss of functionality.

However, those who don't mind losing e.g. “useful instances of `Comonad` for `Bazaar`” are welcome to use `microlens`. Functionality isn't the only important thing.

> Template Haskell code can't be moved out either because it doesn't make sense to tie yourself to GHC and implement so many useful things but leave automatic lens generation out.

Agreed.

It definitely doesn't make much sense to create `lens` and leave TH bits out, just as it doesn't make sense to produce a 2000$ tv set where you can't switch between sound tracks when watching a file from a usb stick. Fuck you, Philips, so much.

It makes sense, however, to leave TH bits out when they account for a 200% increase in build time, and they do in my case. So I moved them into a separate package.

> I used to think I wanted a simple lens library. Then I threw the doors open to everyone to tug on it in different directions and I found that the simple library I wanted wasn't so simple any more and I let it reshape itself to meet everyone's contradictory demands.

Well, yep, if you've made an amazing knife for youself you might as well make it nicer for other people to use it, and let them add features you personally don't need but don't mind having in your knife either, because what does it matter when the knife weighs 10kg already?

### What about `lens-family`?

[`lens-family`][] is a nice package which is mostly compatible with `lens`, which has few dependencies, and which provides Template Haskell in a separate package as well.

[`lens-family`]: http://hackage.haskell.org/packages/lens-family

I'm afraid to depend on it, because it isn't *guaranteed* to be compatible and doesn't even try to be compatible.

It's also hard to sell.

### Sell?

I want people to use lenses and be happy. This requires “selling” some package to them. I can't sell `lens` because it's simply unsellable to people who can't depend on such a big package, and I can't sell `lens-family` because it's outside of my reach. I can't give promises of compatibility, I can't include a nice tutorial in the docs, I can't even file an issue on Github because the library isn't on Github.

It's really easier for me to just write a package from scratch (well, actually more like “by copypasting bits and pieces of `lens`”) than to either try to promote `lens-family` or convince its maintainer to let me do what I want to do to it.

### Wouldn't the development of `lens` be hindered if people switched to another, more basic package?

My guess is that at this point the majority of `lens` development is done by people who want `lens` for the sake of having `lens`. The development of Haskell wasn't hindered by people who used C++; in fact, one of the reasons Haskell is so nice now is that it didn't become popular and so developers didn't have to worry about maintaining compatibility so much.

(I wonder whether it shows that I'm afraid of Edward seeing all this, pointing finger at me, and saying “you spoiled my secret plans, insolent muggle, now Haskell won't ever be as nice as I envisioned it, and it's all because of you, you!”...)
