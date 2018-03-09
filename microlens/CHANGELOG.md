# 0.4.9

* Added `<>~`.
* Added fixities for `<%~`, `<<%~`, `<<.~`.

# 0.4.8.3

* Fixed compilation on GHC 8.4.

# 0.4.8.2

Skipped (the tarball got corrupted).

# 0.4.8.1

* Added `HasCallStack` for some partial functions.

# 0.4.8.0

* Added `forOf_` and `forOf`.
* Added an instance for `Each (NonEmpty a)` (available starting from GHC 8).

# 0.4.7.0

* Fixed the [Haddock crash on GHC 8](https://github.com/aelve/microlens/issues/72) by removing default method implementations (`each = traverse` and `ix = ixAt`). If you had custom instances of `Ixed` or `Each` which relied on default methods, they'd stop working.

# 0.4.6.0

* Added `traverseOf` and `traverseOf_`.
* Changed fixities of `#.` and `.#` to the ones in the profunctors package. Those operators are only available from `Lens.Micro.Internal`, so this shouldn't affect most users.

# 0.4.5.0

* Added `<&>` (which makes lens creation easier).

# 0.4.4.3

* Fixed markup in the .cabal file.
* Added descriptions of other packages to `Lens.Micro`.

# 0.4.4.2

* More changes to make microlens-platform more prominent.

# 0.4.4.1

* Pointed to microlens-platform in the synopsis.

# 0.4.4.0

* Added `mapAccumLOf`.

# 0.4.3.0

* Added `?~`.

# 0.4.2.1

* Added forgotten copyright/authorship information.

# 0.4.2.0

* Added `singular`.

# 0.4.1.0

* Added `strict` and `lazy`.

# 0.4.0.1

* Fixed a bug that wasn't letting the package compile with GHC 8.0 (see issue #63).

# 0.4.0.0

* Added `folding`.
* Renamed `Getter` and `Fold` to `SimpleGetter` and `SimpleFold` and put them into `Lens.Micro`. Genuine `Getter` and `Fold` are available in microlens-contra.
* Replaced `Applicative (Const r)` constraints with `Monoid r` because it's the same thing but easier to understand.

# 0.3.5.1

* Backported the fix for the bug that wasn't letting the package compile with GHC 8.0 (see issue #63).

# 0.3.5.0

* Added `Lens.Micro.Extras` with `view`, `preview`, `Getter`, and `Fold`. Now you no longer need microlens-mtl if the only thing you need from it is `view`.

# 0.3.4.1

* Changed the description of the package from “A tiny part of the lens library which you can depend upon” to “A tiny part of the lens library with no dependencies” because the previous one was ambiguous (I admit I kinda liked that ambiguity, tho).

# 0.3.4.0

* Added `non`.

# 0.3.3.0

* Added `filtered`.
* Added Safe Haskell pragmas.

# 0.3.2.0

* Added `toListOf` back.
* Added `to`.

# 0.3.1.0

* Added `LensLike` and `LensLike'`.
* Added `failing`.

# 0.3.0.0

* Moved `Lens.Micro.Classes` into `Lens.Micro.Internal`.
* Added `<%~`, `<<%~`, `<<.~`.
* Added `_head`, `_tail`, `_init`, `_last`.

# 0.2.0.0

* Removed `toListOf`.
* Removed `+~`, `-~`, `*~`, `//~` and the `Lens.Micro.Extras` module.

# 0.1.5.0

* Added `ix` and `at`.
* Added `traversed`.
* Moved some things into `Lens.Micro.Internal`.
* Bumped base version.

# 0.1.3.0

* Moved some things into `Lens.Micro.Type` and `Lens.Micro.Classes`.
* `Each` and `Field*` aren't exported by `Lens.Micro` now.

# 0.1.2.0

* Added `each`.

# 0.1.1.0

* Added `ASetter'`, which is useful because we can't provide real `Setter` and `Setter'`.

# 0.1.0.0

First release.
