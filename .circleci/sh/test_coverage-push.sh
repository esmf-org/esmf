#!/usr/bin/env bash
set -Eeuxo pipefail

git config --global user.email "himanshupillai@gmail.com"
git config --global user.name "him-28"


# Clone the docs repository
git clone --depth 1 git@github.com:esmf-org/esmf-test-artifacts.git

# Test coverage -------------------------------------------------------------------

cd esmf-test-artifacts
mkdir test_coverage
cd test_coverage
cp $LOGDIR/ESMF_*  .
cp $LOGDIR/ESMC_*  .
cp $LOGDIR/Methods_Tests  .
cd ..

git add .
git commit -a -m " Test Coverage pushed in the artifacts `date` "

# Push the changes ------------------------------------------------------------

git remote prune origin
git push origin master
