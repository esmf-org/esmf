#!/usr/bin/env bash

docker stop "${TEST_RUNNER}"
echo "${DOCKER_PASSWORD}" | docker login -u "${DOCKER_USERNAME}" --password-stdin
docker push "${DOCKER_IMG}:${CIRCLE_BRANCH}"
