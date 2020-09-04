#!/usr/bin/env bash
set -Eeuxo pipefail

git config --global user.email "himanshu@ucar.edu"
git config --global user.name "esmf-orgbot"

cd
git clone --depth 1 git@github.com:esmf-org/esmpy_doc.git
cp -rf /tmp/artifacts/doc-esmpy/esmpy_doc/* ~/esmpy_doc/docs/develop/
cd ~/esmpy_doc/
git add .
git commit -m "ESMPy doc build by CircleCI"
git push origin master
