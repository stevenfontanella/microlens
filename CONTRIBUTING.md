# Contributing

## Adding a new package

  * add it to [README.md](README.md)
  * add build times to [README.md](README.md)
  * mention it in [microlens.cabal](microlens/microlens.cabal)
  * mention it in `Lens.Micro`
  * [create a Stackage pull request](https://github.com/fpco/stackage/edit/master/build-constraints.yaml)
  * update this file

## Releasing a new version

When a new **minor** version is released, bump the following:

  * **microlens** → microlens-ghc, microlens-platform
  * **microlens-ghc** → microlens-platform
  * **microlens-th** → microlens-platform
  * **microlens-mtl** → microlens-platform
  * **microlens-platform** → \<nothing\>
  * **microlens-contra** → \<nothing\>

When a new **major** version is released, bump the following:

  * **microlens** → microlens-ghc, microlens-th, microlens-mtl, microlens-platform, microlens-contra
  * **microlens-ghc** → microlens-platform
  * **microlens-th** → microlens-platform
  * **microlens-mtl** → microlens-platform
  * **microlens-platform** → \<nothing\>
  * **microlens-contra** → \<nothing\>

Lastly, add “New minor/major release” to changelogs of:

  * **microlens** → microlens-ghc, microlens-platform (don't forget that microlens-platform will have microlens-ghc bumped as well)
  * **microlens-ghc** → microlens-platform
  * **microlens-th** → microlens-platform
  * **microlens-mtl** → microlens-platform
  * **microlens-platform** → \<nothing\>
  * **microlens-contra** → \<nothing\>
