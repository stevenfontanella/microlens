branch=`git branch --show`

dry=`[ "$1" = "dry" ]`

short_hash() {
  git log -1 --pretty=format:%h 
}

version_of() {
  version=`command ack '^version:' $1/$1.cabal | awk '{print $2}'`
  if [ -z $version ]; then
    return
  fi
  s=`short_hash`
  echo $1 $s $version
}

make_table() {
  for rev in `git rev-list HEAD | head | tac`; do
    git checkout $rev 2>/dev/null
    version_of $1
  done
}

if [ "$1" = dry ]; then
  make_table microlens | uniq --skip-fields=2
  git checkout $branch
  exit 0
fi
  

git checkout $branch