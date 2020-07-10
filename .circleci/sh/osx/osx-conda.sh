#!/usr/bin/env bash
set -Eeuxo pipefail

CONDA_EXE=~/miniconda/condabin/conda

# Only install if the cache has not been restored
if [ ! -d ~/miniconda ]; then
  curl -o  miniconda.sh https://repo.anaconda.com/miniconda/Miniconda3-latest-MacOSX-x86_64.sh
  bash miniconda.sh -b -p ~/miniconda
  hash -r
  bash -c "${CONDA_EXE} config --set always_yes yes --set changeps1 no"
  bash -c "${CONDA_EXE} update -q conda"
  bash -c "${CONDA_EXE} install -c anaconda gfortran_osx-64"
else
  echo "INFO: using CircleCI cache"
fi
