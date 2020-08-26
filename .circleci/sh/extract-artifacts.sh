#!/usr/bin/env bash
set -Eeuxo pipefail

: "${1?Need to set host destination directory}"
: "${2?Need to set file path to extract}"
: "${3?Need to set Docker image name}"

mkdir -p "${1}"
docker run -dit --name runner -v "${1}:/dexc" "${3}"
docker exec -t runner bash -c "cp -rv ${2} /dexc"
docker stop runner
docker rm runner
