# 0.2.2.0

* Moved `Getter` and `Fold` from this package to microlens (they're in `Lens.Micro.Extras`).

# 0.2.1.3

* Bumped template-haskell (so that the package would compile with GHC HEAD).

# 0.2.1.2

* Bumped microlens version to be able to use phantom.

# 0.2.1.1

* Bumped microlens version again.

# 0.2.1.0

* Bumped base version.
* Bumped microlens version.

# 0.2.0.0

* `createClass` was removed because it doesn't seem to be useful without `lensClass`.
* `defaultFieldRules` isn't exported anymore – use `camelCaseFields`.

# 0.1.2.0

* Package now compiles with `-O2` and other optimisations by default.

# 0.1.1.0

* Added `makeLensesFor` (and `lensRulesFor`).

# 0.1.0.1

* Wrote a bit of documentation.

# 0.1.0.0

Initial release.
