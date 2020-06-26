#!/usr/bin/env bash


docker run -dit --name runner -v /tmp:/dexc "${DOCKER_IMG}:${CIRCLE_BRANCH}"
docker exec -t runner bash -c "cp $ESMF_ARTIFACTS/doc-artifacts.zip /dexc"
docker stop runner
docker rm runner