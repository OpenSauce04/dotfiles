#!/bin/bash -ex

if [ "$(uname -s)" = "OpenBSD" ]; then
  export AUTOCONF_VERSION=2.72
  export MAKEINFO=/usr/local/bin/gmakeinfo
  export EMACS_MAKE_JOBS=$(go-nproc)
else
  export EMACS_MAKE_JOBS=$(getconf _NPROCESSORS_ONLN)
fi

if [ "$(uname -s)" = "NetBSD" ]; then
  export MAKEINFO=/usr/local/bin/gmakeinfo
fi

git clean -dfX

./autogen.sh
./configure --without-all --without-x --with-gnutls --with-modules --with-threads --with-tree-sitter --enable-link-time-optimization
gmake -j$EMACS_MAKE_JOBS
gmake install
