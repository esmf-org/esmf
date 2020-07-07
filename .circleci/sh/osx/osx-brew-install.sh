#!/usr/bin/env bash
set -Eeuxo pipefail

# Only install if the cache has not been restored
if ! command -v gfortran; then
  brew install gcc
else
  echo "Using CircleCI cache"
fi
