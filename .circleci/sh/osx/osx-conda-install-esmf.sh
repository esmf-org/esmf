#!/usr/bin/env bash
set -Eeuxo pipefail

: "${CONDA_INSTALLDIR?Need to set}"

source "${CONDA_INSTALLDIR}/etc/profile.d/conda.sh"
conda install -y -c conda-forge esmf esmpy
conda remove --force esmf
conda remove --force esmpy
