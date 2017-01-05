#!/bin/bash
set -ev

function buildlib {
  cd $1
  if [ "$GHCVER" == "head" ]
  then cabal install --allow-newer --only-dependencies --dry-run > plan.txt
  else cabal install --only-dependencies --dry-run > plan.txt
  fi
  if grep -Fq microlens plan.txt
  then
    cat plan.txt
    echo "installing $lib led to downloading some microlens package!"
    exit 1
  fi
  rm plan.txt
  if [ "$GHCVER" == "head" ]
  then cabal install --allow-newer --ghc-options "-Werror"
  else cabal install --ghc-options "-Werror"
  fi
  cabal haddock
  # cabal check
  cabal sdist
  cd ..
}

# We install packages in the order specified in install-order.txt;
# if installing any package leads to *downloading* some microlens package,
# it's a bug.
while read lib; do buildlib $lib; done < install-order.txt
