#!/usr/bin/env bash
set -Eeuxo pipefail


echo "${DOCKER_PASSWORD}" | docker login -u "${DOCKER_USERNAME}" --password-stdin
docker push "${DOCKER_IMG}:${CIRCLE_BRANCH}"
