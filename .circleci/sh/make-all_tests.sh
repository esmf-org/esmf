#!/usr/bin/env bash

docker run --cpus=$(nproc) -dit --name "${TEST_RUNNER}" "${DOCKER_IMG}:${CIRCLE_BRANCH}"
docker exec -t "${TEST_RUNNER}" bash -c 'echo "nproc=$(nproc)"'
docker exec -t "${TEST_RUNNER}" bash -c "tree lib"
docker exec -t "${TEST_RUNNER}" bash -c "make all_tests 2>&1 | tee esmf-make-all_tests.out"
docker commit "${TEST_RUNNER}" "${DOCKER_IMG}:${CIRCLE_BRANCH}"
