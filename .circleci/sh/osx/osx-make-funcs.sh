#!/usr/bin/env bash
set -Eeuxo pipefail

: "${1?Need to set}"

TARGET=${1}
ARTIFACTS=/tmp/artifacts

export ESMF_DIR=~/esmf
export ESMF_COMM="mpiuni"
export ESMF_BOPT="g"
#export ESMF_NETCDF="nc-config"
export ESMF_INSTALL_PREFIX="${HOME}/sandbox/esmf/install"
export ESMF_INSTALL_BINDIR="${ESMF_INSTALL_PREFIX}/bin"
export ESMF_INSTALL_DOCDIR="${ESMF_INSTALL_PREFIX}/doc"
export ESMF_INSTALL_HEADERDIR="${ESMF_INSTALL_PREFIX}/include"
export ESMF_INSTALL_LIBDIR="${ESMF_INSTALL_PREFIX}/lib"
export ESMF_INSTALL_MODDIR="${ESMF_INSTALL_PREFIX}/mod"

mkdir -p ${ARTIFACTS}
cd "${ESMF_DIR}"

function osx_esmf_make_info () {
  make info 2>&1 | tee ${ARTIFACTS}/esmf-make-info.out
}

function osx_esmf_make () {
  make -j $(nproc) 2>&1 | tee ${ARTIFACTS}/esmf-make.out
}

function osx_esmf_make_install () {
  make install 2>&1 | tee ${ARTIFACTS}/esmf-make-install.out
}

if [ "${TARGET}" == "info" ]; then
  osx_esmf_make_info
elif [ "${TARGET}" == "make" ]; then
  osx_esmf_make
elif [ "${TARGET}" == "install" ]; then
  osx_esmf_make_install
else
  echo "ERROR: command not recognized"
  exit 1
fi
