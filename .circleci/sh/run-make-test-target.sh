#!/usr/bin/env bash

docker run --cpus=$(nproc) -v $(pwd):/esmf-travis-ci -dit --name "${TEST_RUNNER}" "${DOCKER_IMG}:${CIRCLE_BRANCH}"
docker exec -t "${TEST_RUNNER}" bash -c "source /esmf-travis-ci/sh/esmf-make-funcs.sh && run_esmf_make_target ${1}"

NAME="${DOCKER_IMG}:${CIRCLE_BRANCH}-${1}"
docker commit "${TEST_RUNNER}" "${NAME}"
echo "${DOCKER_PASSWORD}" | docker login -u "${DOCKER_USERNAME}" --password-stdin
docker push "${NAME}" > docker-push.out
docker stop "${TEST_RUNNER}"
