#!/usr/bin/env bash
set -Eeuxo pipefail

git config --global user.email "himanshu@ucar.edu"
git config --global user.name "esmf-orgbot"

cd
git clone --depth 1 git@github.com:esmf-org/esmpy_doc.git
cd esmpy_doc/
mkdir -p docs/${CIRCLE_BRANCH}
cd ..
cp -rf /tmp/artifacts/doc-esmpy/esmpy_doc/* ~/esmpy_doc/docs/${CIRCLE_BRANCH}/
cd ~/esmpy_doc/
git add .
git commit -m " `echo ${CIRCLE_BRANCH}` ESMPy doc build by CircleCI"
git remote prune origin
git push origin master
