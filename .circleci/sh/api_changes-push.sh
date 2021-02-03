#!/usr/bin/env bash
set -Eeuxo pipefail

DOC_ARTIFACTS=""

git config --global user.email "himanshupillai@gmail.com"
git config --global user.name "him-28"

cd

# Clone the docs repository
git clone --depth 1 git@github.com:esmf-org/esmf-test-artifacts.git

# API changes -------------------------------------------------------------------

cd esmf-test-artifacts
mkdir api_change
cd api_change
cp -r ${DOC_ARTIFACTS}/APIs*.out .
cp -r ${DOC_ARTIFACTS}/diff*.out .
cd ..

git add .
git commit -a -m " API changes pushed in the artifacts `date` "

# Push the changes ------------------------------------------------------------

git remote prune origin
git push origin master
