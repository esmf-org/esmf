#!/usr/bin/env bash
set -Eeuxo pipefail

DOC_ARTIFACTS="/tmp/artifacts"

git config --global user.email "himanshu@ucar.edu"
git config --global user.name "esmf-orgbot"

# TODO: Extract build artifacts from container

git clone --depth 1 git@github.com:esmf-org/esmpy_doc.git
cp -rf ~/esmf/src/addon/ESMPy/doc/esmpy_doc/*   ~/esmpy_doc/docs/develop/
cd ~/esmpy_doc/
git pull
git add .
git commit -m " pushing esmpy doc build by CircleCI on `date` "
git push origin master
