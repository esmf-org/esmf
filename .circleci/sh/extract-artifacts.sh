#!/usr/bin/env bash
set -Eeuxo pipefail

: "${ESMF_ARTIFACTS?Need to set}"

mkdir -p /tmp/artifacts
docker run -dit --name runner -v /tmp/artifacts:/dexc "${DOCKER_IMG}:${CIRCLE_BRANCH}"
docker exec -t runner bash -c "cp ${ESMF_ARTIFACTS}/doc-artifacts.zip /dexc"
docker stop runner
docker rm runner