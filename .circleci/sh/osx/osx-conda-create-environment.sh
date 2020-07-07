#!/usr/bin/env bash
set -Eeuxo pipefail

: "${CONDA_INSTALLDIR?Need to set}"
: "${CONDA_EXE?Need to set}"

# Only install if the cache has not been restored
if [ ! -d "${CONDA_INSTALLDIR}" ]; then
  curl -o  miniconda.sh https://repo.anaconda.com/miniconda/Miniconda3-latest-MacOSX-x86_64.sh
  bash miniconda.sh -b -p "${CONDA_INSTALLDIR}"
  hash -r
  bash -c "${CONDA_EXE} config --set always_yes yes --set changeps1 no"
  bash -c "${CONDA_EXE} update -q conda"
  bash -c "${CONDA_EXE} create -n esmf -c conda-forge esmf esmpy"
  bash -c "${CONDA_EXE} remove -n esmf --force esmf esmpy"
else
  echo "Using CircleCI cache"
fi
