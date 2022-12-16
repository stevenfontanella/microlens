# 0.2.0.3

* [#161](https://github.com/stevenfontanella/microlens/pull/161) Fix GHC 9.4 warning for using `~` without TypeOperators
* [#162](https://github.com/stevenfontanella/microlens/pull/162) Fix GHC warning for depending on StarIsType

# 0.2.0.2

* Added support for mtl 2.3 and transformers 0.6 per [#152](https://github.com/stevenfontanella/microlens/issues/152).

# 0.2.0.1

* No more conditional `Safe` (see [#122](https://github.com/monadfix/microlens/issues/122)).

# 0.2.0

* Removed of equality constraints on `Zoom` and `Magnify`, as was done in `lens` earlier. This allows instances of `Zoom` and `Magnify` for `FreeT`. (Thanks to @treeowl.)

# 0.1.11.1

* Fixed compilation on GHC 8.4.

# 0.1.11.0

* Exported `Focusing`, etc. from `Lens.Micro.Mtl.Internal`.
* Added `&~`.

# 0.1.10.0

* Added `<?=` and `<.=`.

# 0.1.9.0

* Added `?=` and `<~`.

# 0.1.8.0

* Added `assign` and `modifying` as synonyms for `.=` and `%=`.
* Added `<%=`, `<<%=`, and `<<.=`.

# 0.1.7.1

* Added forgotten copyright/authorship information.

# 0.1.7.0

* Added `preuse`.

# 0.1.6.1

* Bumped transformers version.
* Bumped microlens version.

# 0.1.6.0

* Added `Lens.Micro.Mtl.Internal` (which exports zooming type classes)

# 0.1.5.0

* Added Safe Haskell pragmas.

# 0.1.4.1

* Bumped microlens version.

# 0.1.4.0

* Added `preview` (a synonym for (`^?`)).
* Bumped microlens version.

# 0.1.3.1

* Bumped microlens version.

# 0.1.3.0

* Moved some internally used functions to `Lens.Micro.Internal` in microlens.
* Bumped base version.

# 0.1.2.0

* Package now compiles with `-O2` and other optimisations by default.

# 0.1.1.0

* Added `zoom` and `magnify`.

# 0.1.0.0

Initial release.
