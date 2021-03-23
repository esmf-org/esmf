#!/usr/bin/env bash

NAME="${DOCKER_IMG}:${CIRCLE_BRANCH}-${1}"

docker commit "${TEST_RUNNER}" "${NAME}"
docker stop "${TEST_RUNNER}"
echo "${DOCKER_PASSWORD}" | docker login -u "${DOCKER_USERNAME}" --password-stdin
docker push "${NAME}"
