#!/usr/bin/env bash


docker run -dit -v /tmp:/dexc bash -c "cp /$ESMF_ARTIFACTS/doc-artifacts.zip /dexc" "${DOCKER_IMG}:${CIRCLE_BRANCH}"