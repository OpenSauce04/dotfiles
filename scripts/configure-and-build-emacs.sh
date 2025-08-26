git clean -dfX
./autogen.sh && \
  ./configure --without-all --without-ns --without-x --with-gnutls --with-modules --with-threads --with-tree-sitter --with-xml2 --enable-link-time-optimization && \
  make -j$(getconf _NPROCESSORS_ONLN)
