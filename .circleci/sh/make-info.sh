#!/usr/bin/env bash

docker build -q -t "${DOCKER_IMG}:${CIRCLE_BRANCH}" --file "${ESMF_DIR}/.circleci/docker/esmf/Dockerfile" --build-arg ESMF_BRANCH="${CIRCLE_BRANCH}" .
docker run --cpus=$(nproc) -dit --name "${TEST_RUNNER}" "${DOCKER_IMG}:${CIRCLE_BRANCH}"
docker exec -t "${TEST_RUNNER}" bash -c "make info 2>&1 | tee esmf-make-info.out"
docker commit "${TEST_RUNNER}" "${DOCKER_IMG}:${CIRCLE_BRANCH}"
