#!/usr/bin/env bash
set -Eeuxo pipefail

cd
git clone git@github.com:esmf-org/esmf-org.github.io.git esmf-gio
cd esmf-gio
ssh -T git@github.com

#cd ~/esmf/doc
#cp -rf * ~/esmf-org.github.io/dev_docs/
#
#cd ~/esmf-org.github.io/
#git pull
#git add .
#git commit -m " pushing trunk ref doc build by CircleCI on `date` "
#git push origin master
#
#cd ~/esmf/src/addon/NUOPC/doc
#
#for i in  NUOPC_refdoc NUOPC_howtodoc
#    do
#        cp -rf $i ~/esmf-org.github.io/dev_docs/
#
#    done
#
#for i in  NUOPC_refdoc.pdf NUOPC_howtodoc.pdf
#    do
#        cp -rf $i ~/esmf-org.github.io/dev_docs/
#    done
#
#cd ~/esmf-org.github.io/dev_docs
#git pull
#git add .
#git commit -m " pushing nuopc doc build by CircleCI on `date` "
#git push origin master
