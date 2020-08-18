#!/usr/bin/env bash
set -Eeuxo pipefail

: "${1?Need name to push}"

echo "${DOCKER_PASSWORD}" | docker login -u "${DOCKER_USERNAME}" --password-stdin
docker push "${1}"
