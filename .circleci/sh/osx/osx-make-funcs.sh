#!/usr/bin/env bash

source ~/miniconda/etc/profile.d/conda.sh
conda activate root || exit 1

set -Eeuxo pipefail

: "${1?Need to set}"

TARGET=${1}
ARTIFACTS=/tmp/osx-artifacts

export ESMF_DIR=~/esmf
export ESMF_COMPILER="gfortranclang"
export ESMF_COMM="mpiuni"
export ESMF_BOPT="g"
#export ESMF_NETCDF="nc-config"
export ESMF_INSTALL_PREFIX="${HOME}/sandbox/esmf/install"
export ESMF_INSTALL_BINDIR="${ESMF_INSTALL_PREFIX}/bin"
export ESMF_INSTALL_DOCDIR="${ESMF_INSTALL_PREFIX}/doc"
export ESMF_INSTALL_HEADERDIR="${ESMF_INSTALL_PREFIX}/include"
export ESMF_INSTALL_LIBDIR="${ESMF_INSTALL_PREFIX}/lib"
export ESMF_INSTALL_MODDIR="${ESMF_INSTALL_PREFIX}/mod"
export ESMF_TESTEXHAUSTIVE="OFF"

mkdir -p ${ARTIFACTS}
cd "${ESMF_DIR}" || exit 1

function osx_esmf_make_info () {
  make info 2>&1 | tee ${ARTIFACTS}/esmf-make-info.out
}

function osx_esmf_make () {
  make -j $(nproc) 2>&1 | tee ${ARTIFACTS}/esmf-make.out
}

function osx_esmf_make_install () {
  make install 2>&1 | tee ${ARTIFACTS}/esmf-make-install.out
}

function osx_esmf_make_check () {
  make check 2>&1 | tee ${ARTIFACTS}/esmf-make-check.out
}

if [ "${TARGET}" == "info" ]; then
  osx_esmf_make_info
elif [ "${TARGET}" == "make" ]; then
  osx_esmf_make
elif [ "${TARGET}" == "install" ]; then
  osx_esmf_make_install
elif [ "${TARGET}" == "check" ]; then
  osx_esmf_make_check
elif [ "${TARGET}" == "collect-test-results" ]; then
  cd ${ARTIFACTS}
  cd ..
  cp -rf ~/esmf/test ${ARTIFACTS} || echo "WARNING: no test directory. did tests run?"
  cp -rf ~/esmf/examples ${ARTIFACTS} || echo "WARNING: no examples directory. did examples run?"
  zip -r osx-artifacts.zip osx-artifacts
else
  echo "ERROR: command not recognized"
  exit 1
fi
