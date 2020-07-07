#!/usr/bin/env bash
set -Eeuxo pipefail

: "${CONDA_INSTALLDIR?Need to set}"

# Only install if the cache has not been restored
if [ ! -d "/path/to/dir" ]; then
  curl https://repo.anaconda.com/miniconda/Miniconda3-latest-MacOSX-x86_64.sh -O miniconda.sh
  bash miniconda.sh -b -p "${CONDA_INSTALLDIR}"
  source "${CONDA_INSTALLDIR}/etc/profile.d/conda.sh"
  hash -r
  conda config --set always_yes yes --set changeps1 no
  conda update -q conda
else
  echo "Using CircleCI cache"
fi
