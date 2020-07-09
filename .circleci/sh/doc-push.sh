#!/usr/bin/env bash
set -Eeuxo pipefail

: "${CIRCLE_BRANCH?Need to set}"

DOC_ARTIFACTS="/tmp/artifacts"

git config --global user.email "himanshu@ucar.edu"
git config --global user.name "esmf-orgbot"

cd

# Clone the docs repository
git clone git@github.com:esmf-org/esmf-org.github.io.git

# ESMF Docs -------------------------------------------------------------------

cd ${DOC_ARTIFACTS}/doc-esmf
cp -rf ./* ~/esmf-org.github.io/dev_docs/

cd ~/esmf-org.github.io/
git pull
git add .
git commit -m " pushing trunk ref doc build by CircleCI on `date` "
git push origin "${CIRCLE_BRANCH}"

# NUOPC Docs ------------------------------------------------------------------

cd ${DOC_ARTIFACTS}/doc-nuopc

for i in  NUOPC_refdoc NUOPC_howtodoc
    do
        cp -rf $i ~/esmf-org.github.io/dev_docs/

    done

for i in  NUOPC_refdoc.pdf NUOPC_howtodoc.pdf
    do
        cp -rf $i ~/esmf-org.github.io/dev_docs/
    done

cd ~/esmf-org.github.io/dev_docs
git pull
git add .
git commit -m " pushing nuopc doc build by CircleCI on `date` "
git push origin "${CIRCLE_BRANCH}"
