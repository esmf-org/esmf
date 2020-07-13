#!/usr/bin/env bash
set -Eeuxo pipefail

: "${ESMF_ARTIFACTS?Need to set}"
: "${ESMF_DIR?Need to set}"
: "${1?Need to set}"

TARGET=${1}
TOCOLLECT=( "*.stdout" "*.Log" )
DSTDIR="${ESMF_ARTIFACTS}/test-artifacts/${TARGET}"

cd "${ESMF_DIR}"
mkdir -p "${DSTDIR}"
if [ "${TARGET}" == "examples" ]; then
  SRCDIR="examples"
else
  SRCDIR="test"
fi

for i in "${TOCOLLECT[@]}"; do
  find ${SRCDIR} -name "${i}" -exec cp {} "${DSTDIR}" \;
done

cd "${ESMF_ARTIFACTS}"
zip -r "test-artifacts.zip" "test-artifacts"
