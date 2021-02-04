#!/usr/bin/env bash
set -Eeuxo pipefail

DOC_ARTIFACTS="/tmp/artifacts/api"

git config --global user.email "himanshupillai@gmail.com"
git config --global user.name "him-28"

cd

# Clone the docs repository
git clone --depth 1 git@github.com:esmf-org/esmf-test-artifacts.git

# API changes -------------------------------------------------------------------

cd esmf-test-artifacts
mkdir api_change

cd ${DOC_ARTIFACTS}
cp -rf ${DOC_ARTIFACTS}/* ~/esmf-test-artifacts/api_change/


cd ~/esmf-test-artifacts/
git add .
git commit -a -m " API changes pushed in the artifacts `date` [ci skip] "

# Push the changes ------------------------------------------------------------

git remote prune origin
git push origin master
