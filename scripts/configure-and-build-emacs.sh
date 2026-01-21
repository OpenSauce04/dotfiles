git clean -dfX
./autogen.sh && \
  ./configure --without-all --with-gnutls --with-modules --with-threads --with-toolkit-scroll-bars --with-tree-sitter --enable-link-time-optimization && \
  make -j$(getconf _NPROCESSORS_ONLN)
