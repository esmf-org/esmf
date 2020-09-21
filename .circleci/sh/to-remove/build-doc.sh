#!/usr/bin/env bash

docker build -t "${DOCKER_IMG}:${CIRCLE_BRANCH}" --file "${ESMF_DIR}/.circleci/docker/esmf-doc/Dockerfile" --build-arg ESMF_BRANCH="${CIRCLE_BRANCH}" .
#echo "${DOCKER_PASSWORD}" | docker login -u "${DOCKER_USERNAME}" --password-stdin
#docker push "${DOCKER_IMG}:${CIRCLE_BRANCH}"
