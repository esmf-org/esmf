#!/usr/bin/env bash

docker exec -t "${TEST_RUNNER}" bash -c 'make install 2>&1 | tee /tmp/esmf-make-install.out'
docker commit "${TEST_RUNNER}" "${DOCKER_IMG}:${CIRCLE_BRANCH}"
