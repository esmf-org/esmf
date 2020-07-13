#!/usr/bin/env bash
set -Eeuxo pipefail

brew link gcc || echo "INFO: brew gcc not installed"

# Only install if the cache has not been restored
if ! command -v gfortran; then
  echo "INFO: install brew gcc"
  brew install gcc
else
  echo "INFO: using CircleCI cache"
fi

# Confirm gfortran is installed
if ! command -v gfortran; then
  echo "ERROR: gfortran not installed or not linked"
  exit 1
fi
