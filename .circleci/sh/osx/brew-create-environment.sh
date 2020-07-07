#!/usr/bin/env bash
set -Eeuxo pipefail

# Only install if the cache has not been restored
if [ ! -d ~/miniconda ]; then
  exit 1
else
  echo "Using CircleCI cache"
fi
