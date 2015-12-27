# Contributing

If you're not me, just open an issue or a pull request and we'll sort everything out.

If you're me, here's a checklist:

  * If there's a new package, add it to [install-order.txt](install-order.txt), [README.md](README.md), and possibly [microlens.cabal](microlens/microlens.cabal).

  * When bumping minor/major version, bump the following too:

      * microlens → microlens-ghc, microlens-th, microlens-mtl, microlens-platform
      * microlens-ghc → microlens-platform
      * microlens-th → microlens-platform
      * microlens-mtl → microlens-platform
      * microlens-platform → \<nothing\>

  * When bumping minor/major version, add “New minor release” to changelogs of:

      * microlens → microlens-ghc, microlens-platform (don't forget that microlens-platform will have microlens-ghc bumped as well)
      * microlens-ghc → microlens-platform
      * microlens-th → microlens-platform
      * microlens-mtl → microlens-platform
      * microlens-platform → \<nothing\>

  * And generally don't forget to update the changelogs.
