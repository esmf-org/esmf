#!/usr/bin/env bash
set -Eeuxo pipefail

DOC_ARTIFACTS="/tmp/artifacts/artifacts"

git config --global user.email "dunlap@ucar.edu"
git config --global user.name "esmf-orgbot"

cd

# Clone the docs repository
git clone --depth 1 git@github.com:esmf-org/esmf-org.github.io.git
cd esmf-org.github.io/
mkdir -p docs/nightly/${CIRCLE_BRANCH}/dev_guide

# ESMF Docs -------------------------------------------------------------------

cd ${DOC_ARTIFACTS}/doc-esmf
cp -rf ./* ~/esmf-org.github.io/docs/nightly/${CIRCLE_BRANCH}/

cd ~/esmf-org.github.io/
git add .
git commit -m " `echo ${CIRCLE_BRANCH}` ESMF doc build by CircleCI"

# NUOPC Docs ------------------------------------------------------------------

cd ${DOC_ARTIFACTS}/doc-nuopc

for i in  NUOPC_refdoc NUOPC_howtodoc
    do
        cp -rf $i ~/esmf-org.github.io/docs/nightly/${CIRCLE_BRANCH}/

    done

for i in  NUOPC_refdoc.pdf NUOPC_howtodoc.pdf
    do
        cp -rf $i ~/esmf-org.github.io/docs/nightly/${CIRCLE_BRANCH}/
    done

cd ~/esmf-org.github.io/docs/nightly/${CIRCLE_BRANCH}
git add .
git commit -m "`echo ${CIRCLE_BRANCH}` NUOPC doc build by CircleCI"

# Developer's Guide -----------------------------------------------------------

cd ${DOC_ARTIFACTS}/doc-dev_guide
cp -rf ./dev_guide/dev_guide/* ~/esmf-org.github.io/docs/nightly/${CIRCLE_BRANCH}/dev_guide/
cd ~/esmf-org.github.io/
git add .
git commit -m " `echo ${CIRCLE_BRANCH}` ESMF dev_guide build by CircleCI"

# Push the changes ------------------------------------------------------------

# Attempt to help with the push by updating remote refs.
# https://stackoverflow.com/questions/6656619/git-and-nasty-error-cannot-lock-existing-info-refs-fatal
git remote prune origin
git push origin master
