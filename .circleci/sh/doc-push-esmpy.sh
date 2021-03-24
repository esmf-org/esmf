#!/usr/bin/env bash
set -Eeuxo pipefail

git config --global user.email "dunlap@ucar.edu"
git config --global user.name "esmf-orgbot"

cd
git clone --depth 1 git@github.com:esmf-org/esmpy_doc.git

cd esmpy_doc/
mkdir -p docs/nightly/${CIRCLE_BRANCH}

cd ..
cp -rf /tmp/artifacts/doc-esmpy/esmpy_doc/html ~/esmpy_doc/docs/nightly/${CIRCLE_BRANCH}/
cp -rf /tmp/artifacts/doc-esmpy/esmpy_doc/latex/ESMPy.pdf ~/esmpy_doc/docs/nightly/${CIRCLE_BRANCH}/

cd ~/esmpy_doc/
git add .
git commit -m " `echo ${CIRCLE_BRANCH}` ESMPy doc build by CircleCI"
git remote prune origin
git push origin master
