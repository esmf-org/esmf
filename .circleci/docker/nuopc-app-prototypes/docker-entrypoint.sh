#!/usr/bin/env bash
#set -Eeuxo pipefail

ESMF_ARTIFACTS=/artifacts/nuopc-app-prototypes

mkdir -p ${ESMF_ARTIFACTS}
bash testProtos.sh 2>&1 | tee ${ESMF_ARTIFACTS}/testProtos.out
cp ./*.ESMF_LogFile ${ESMF_ARTIFACTS}
