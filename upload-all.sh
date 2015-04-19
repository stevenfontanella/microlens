for lib in microlens*
do
  cd $lib
  cabal clean
  cabal configure
  cabal sdist
  cabal upload dist/$lib*
  cd ..
done
