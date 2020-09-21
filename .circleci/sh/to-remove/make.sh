#!/usr/bin/env bash

docker exec -t "${TEST_RUNNER}" bash -c 'make -j "$(nproc)" 2>&1 | tee /tmp/esmf-make.out'
docker commit "${TEST_RUNNER}" "${DOCKER_IMG}:${CIRCLE_BRANCH}"
