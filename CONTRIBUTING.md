# Contributing

## Current goals regarding dependencies

`microlens` should have no dependencies. `microlens-ghc` should only depend on packages shipped with GHC. `microlens-platform` should only depend on packages shipped with the Haskell Platform.

## Current goals regarding GHC versions

`microlens` tries to support GHC versions as far into the past as possible, unless it becomes too annoying.

## Current goals regarding `lens` feature parity

Any code using `microlens` should also compile and behave the same with `lens`. This implies that `microlens` should never implement any functions or instances, even if useful, unless they also appear in `lens`. This is so that `microlens` users are never locked into `microlens` and can "break out" and switch to `lens` if they feel that they don't care about dependencies anymore.

It is possible that sometimes code working with `lens` will not compile with `microlens`. It's okay, but should be documented in the haddocks.

There is no goal to have as many functions and operators from `lens` available in `microlens`. The build time of `microlens` packages should remain small. When people propose adding new functions/operators, use your best judgment.

## Hackage revisions vs patch versions

_So far the policy has been "prefer publishing a new version over making a Hackage revision", because revisions are a bit of an annoyance with Nix, but maybe it's not an issue anymore. — @neongreen, 11 May 2022_

## Adding a new package

  * Add the new package to [README.md](README.md)
  * Add build times to [README.md](README.md) (this is perhaps optional)
  * Mention the new package in [microlens.cabal](microlens/microlens.cabal)
  * Mention the new package in `Lens.Micro`
  * [Create a Stackage pull request](https://github.com/fpco/stackage/edit/master/build-constraints.yaml)
  * Update this file

## Releasing a new version of any package

The versioning is `0.<major>.<minor>.<patch>`.

Some packages, like `microlens-platform`, are supposed to pin versions of other `microlens` packages. So if a new version of `microlens` is released, a new version of `microlens-platform` also has to be released.

Specifically, the rules are as follows.

When a new **patch** version of any package is released, there is no need to bump anything.

When a new **minor** version of a specific package is released, bump the following:

  * New **`microlens`** → bump `microlens-ghc`, `microlens-platform`
  * New **`microlens-ghc`** → bump `microlens-platform`
  * New **`microlens-th`** → bump `microlens-platform`
  * New **`microlens-mtl`** → bump `microlens-platform`
  * New **`microlens-platform`** → \<nothing\>
  * New **`microlens-contra`** → \<nothing\>

When a new **major** version is released, bump the following:

  * **microlens** → microlens-ghc, microlens-th, microlens-mtl, microlens-platform, microlens-contra
  * **microlens-ghc** → microlens-platform
  * **microlens-th** → microlens-platform
  * **microlens-mtl** → microlens-platform
  * **microlens-platform** → \<nothing\>
  * **microlens-contra** → \<nothing\>

Lastly, add “New minor/major release” to the changelogs of:

  * **microlens** → microlens-ghc, microlens-platform (don't forget that microlens-platform will have microlens-ghc bumped as well)
  * **microlens-ghc** → microlens-platform
  * **microlens-th** → microlens-platform
  * **microlens-mtl** → microlens-platform
  * **microlens-platform** → \<nothing\>
  * **microlens-contra** → \<nothing\>
