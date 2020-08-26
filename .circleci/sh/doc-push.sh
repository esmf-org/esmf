#!/usr/bin/env bash
set -Eeuxo pipefail

DOC_ARTIFACTS="/tmp/artifacts/artifacts"

git config --global user.email "himanshu@ucar.edu"
git config --global user.name "esmf-orgbot"

cd

# Clone the docs repository
git clone git@github.com:esmf-org/esmf-org.github.io.git

# ESMF Docs -------------------------------------------------------------------

cd ${DOC_ARTIFACTS}/doc-esmf
cp -rf ./* ~/esmf-org.github.io/dev_docs/

cd ~/esmf-org.github.io/
git add .
git commit -m "ESMF doc build by CircleCI"

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
git add .
git commit -m "NUOPC doc build by CircleCI"

# Developer's Guide -----------------------------------------------------------

cd ${DOC_ARTIFACTS}/doc-dev_guide
cp -rf ./* ~/esmf-org.github.io/dev_docs/
cd ~/esmf-org.github.io/
git add .
git commit -m " ESMF dev_guide build by CircleCI"

# Push the changes ------------------------------------------------------------

git push origin master
