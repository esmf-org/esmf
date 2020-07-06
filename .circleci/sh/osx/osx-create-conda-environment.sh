#!/usr/bin/env bash
set -Eeuxo pipefail

wget https://repo.anaconda.com/miniconda/Miniconda3-latest-MacOSX-x86_64.sh -O miniconda.sh
bash miniconda.sh -b -p ${HOME}/miniconda
source "${HOME}/miniconda/etc/profile.d/conda.sh"
hash -r
conda config --set always_yes yes --set changeps1 no
conda update -q conda