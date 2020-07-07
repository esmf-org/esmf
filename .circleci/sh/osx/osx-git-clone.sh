#!/usr/bin/env bash
set -Eeuxo pipefail

mkdir -p ~/sandbox/esmf
cd ~/sandbox/esmf
git clone --branch $CIRCLE_BRANCH --depth 1 https://github.com/esmf-org/esmf.git src-git
