#!/usr/bin/env bash
set -Eeuxo pipefail

: "${1?Need to set host destination directory}"
: "${2?Need to set file path to extract}"
: "${3?Need to set Docker image name}"

mkdir -p "${1}"
CID=$(docker run -dit --name runner "${3}")
docker cp ${CID}:${2} ${1}
docker stop ${CID}
docker rm ${CID}
