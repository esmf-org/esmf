#!/usr/bin/env bash
set -Eeuxo pipefail

DOC_ARTIFACTS="/tmp/artifacts/artifacts"

git config --global user.email "dunlap@ucar.edu"
git config --global user.name "esmf-orgbot"

cd

# Clone the docs repository
git clone --depth 1 git@github.com:esmf-org/esmf-test-artifacts.git

# API changes -------------------------------------------------------------------
echo "ESMF_BRANCH=${CIRCLE_BRANCH}"

cd esmf-test-artifacts/
mkdir -p ${CIRCLE_BRANCH}/platform_independent/api_change

cd ${DOC_ARTIFACTS}
cp -rf ${DOC_ARTIFACTS}/* ~/esmf-test-artifacts/${CIRCLE_BRANCH}/platform_independent/api_change/


cd ~/esmf-test-artifacts/
git add .
git commit -a -m " `echo ${CIRCLE_BRANCH}` API changes pushed in the artifacts `date` [ci skip] "

# Push the changes ------------------------------------------------------------

git remote prune origin
git push origin main
