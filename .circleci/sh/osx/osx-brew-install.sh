#!/usr/bin/env bash
set -Eeuxo pipefail

brew link gcc@9 || echo "INFO: brew gcc not installed"
# Only install if the cache has not been restored
if ! command -v gfortran; then
  brew install gcc@9
else
  echo "Using CircleCI cache"
fi
