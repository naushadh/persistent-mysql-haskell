#!/usr/bin/env bash

set -euxo pipefail

cabal update
cabal build --enable-tests
cabal test
cabal sdist
