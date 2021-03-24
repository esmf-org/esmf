#!/usr/bin/env bash
set -Eeuxo pipefail

DOC_ARTIFACTS="/tmp/artifacts/artifacts"

cd 

git config --global user.email "dunlap@ucar.edu"
git config --global user.name "esmf-orgbot"


# Clone the docs repository
git clone --depth 1 git@github.com:esmf-org/esmf-test-artifacts.git

# Test coverage -------------------------------------------------------------------
echo "ESMF_BRANCH=${CIRCLE_BRANCH}"

cd esmf-test-artifacts/

mkdir -p ${CIRCLE_BRANCH}/platform_independent/test_coverage

cd ${DOC_ARTIFACTS}
cp -rf ${DOC_ARTIFACTS}/* ~/esmf-test-artifacts/${CIRCLE_BRANCH}/platform_independent/test_coverage/


cd ~/esmf-test-artifacts/
git add .
git commit -a -m " Test Coverage pushed in the artifacts `date` [ci skip] "

# Push the changes ------------------------------------------------------------

git remote prune origin

# helps to avoid failures due to commits than come in between
git pull --no-edit
git push origin main
