#! /bin/sh

set -e

dh_testdir
dh_clean

SOURCE=$(dpkg-parsechangelog | awk '/^Source:/ { print $2 }')
FULLVERSION=$(dpkg-parsechangelog | awk '/^Version:/ { print $2 }')
UPSTREAM_VERSION=${FULLVERSION%-*}
DEBIAN_VERSION=${FULLVERSION##*-}
THIS=$(basename $0)
THISDIR=$(basename $PWD)

tar cvzfC ../${SOURCE}_$UPSTREAM_VERSION.orig.tar.gz ../ \
    --exclude=$THISDIR/debian --exclude=$SOURCE/$THIS --exclude=CVS \
    $THISDIR
