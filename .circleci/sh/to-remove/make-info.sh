#!/usr/bin/env bash

docker run --cpus=$(nproc) -dit --name "${TEST_RUNNER}" "${DOCKER_IMG}:${CIRCLE_BRANCH}"
docker exec -t "${TEST_RUNNER}" bash -c "make info 2>&1 | tee /tmp/esmf-make-info.out"
docker commit "${TEST_RUNNER}" "${DOCKER_IMG}:${CIRCLE_BRANCH}"
