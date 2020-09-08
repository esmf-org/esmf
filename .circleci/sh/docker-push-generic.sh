#!/usr/bin/env bash
set -Eeuxo pipefail

: "${1?Need name to push}"

echo "${ESMF_DOCKER_PASSWORD}" | docker login -u "${ESMF_DOCKER_USERNAME}" --password-stdin
docker push "${1}"
