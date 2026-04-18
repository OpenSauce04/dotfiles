#!/bin/bash -ex

if [ "$(uname -s)" = "OpenBSD" ]; then
  export AUTOCONF_VERSION=2.72
  export MAKEINFO=/usr/local/bin/gmakeinfo
  export EMACS_MAKE_JOBS=$(go-nproc)
else
  export EMACS_MAKE_JOBS=$(getconf _NPROCESSORS_ONLN)
fi

sudo git clean -dfX

./autogen.sh
./configure --without-all --with-gnutls --with-modules --with-threads --with-toolkit-scroll-bars --with-tree-sitter --enable-link-time-optimization
gmake -j$EMACS_MAKE_JOBS
gmake install
