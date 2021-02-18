#!/bin/bash

CABAL_BIN=${CABAL_BIN:-cabal}

$CABAL_BIN -- build --enable-executable-static --ghc-options=-split-sections
BUILDS=$(find dist-newstyle \( -name 'lsp-network-server' -o -name 'lsp-network-client'  \) -type f)
cp $BUILDS ~/.local/bin/
