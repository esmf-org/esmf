#!/usr/bin/env bash
set -Eeuxo pipefail

brew link gcc || echo "INFO: brew gcc not installed"
# Only install if the cache has not been restored
if ! command -v gfortran; then
  brew install gcc
else
  echo "Using CircleCI cache"
fi
