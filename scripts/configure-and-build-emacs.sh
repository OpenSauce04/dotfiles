#!/bin/bash -ex

git reset --hard
sudo git clean -dfX
git apply ~/dotfiles/patches/emacs/*.patch

./autogen.sh
./configure --without-all --with-gnutls --with-modules --with-threads --with-toolkit-scroll-bars --with-tree-sitter --enable-link-time-optimization
make -j$(getconf _NPROCESSORS_ONLN)
