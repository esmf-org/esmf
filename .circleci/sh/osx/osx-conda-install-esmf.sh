#!/usr/bin/env bash
set -Eeuxo pipefail

: "${CONDA_INSTALLDIR?Need to set}"
: "${CONDA_EXE?Need to set}"

${CONDA_EXE} install -n esmf -c conda-forge esmf esmpy
${CONDA_EXE} remove --force esmf esmpy
