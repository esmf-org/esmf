#!/usr/bin/env bash
set -Eeuxo pipefail


CMD='cd "${ESMPY_DIR}" && python setup.py build --ESMFMKFILE="${ESMFMKFILE}" 2>&1 | tee /tmp/esmpy-build.out'
docker exec -t "${TEST_RUNNER}" bash -c "${CMD}"

CMD='cd "${ESMPY_DIR}" && python setup.py install 2>&1 | tee /tmp/esmpy-install.out'
docker exec -t "${TEST_RUNNER}" bash -c "${CMD}"

docker commit "${TEST_RUNNER}" "${DOCKER_IMG}:${CIRCLE_BRANCH}"
