#!/usr/bin/env bash
set -Eeuxo pipefail

docker rm --force runner || echo "WARNING: container does not exist"
docker run -dit --name runner esmf/nuopc-app-prototypes
docker exec -t runner /opt/docker-entrypoint.sh testProtos
#docker exec -t runner /opt/docker-entrypoint.sh meta_test
#docker exec -t runner /opt/docker-entrypoint.sh prep_artifacts
#docker commit runner esmf/nuopc-app-prototypes
#docker stop runner
#docker rm runner
