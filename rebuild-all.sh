for lib in microlens*
do
  cabal sandbox add-source $lib
  cabal sandbox hc-pkg unregister $lib -- --force
done

cabal install microlens*
