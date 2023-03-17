#!/bin/bash

# get arguments
while getopts r:s: flag
do
  case "${flag}" in
    r) run_dir=${OPTARG};;
    s) spack_install_dir=${OPTARG};;
  esac
done

# check for default values
if [[ -z "$run_dir" || ! -z `echo $run_dir | grep '^-'` ]]; then
  run_dir=`pwd`
fi

if [[ -z "$spack_install_dir" || ! -z `echo $spack_install_dir | grep '^-'` ]]; then
  spack_install_dir=`pwd`
fi

# print out arguments
echo "Run Directory: $run_dir";

# go to directory
cd $run_dir

# install spack environment
echo "::group::Build and Run NUOPC Application Prototypes"
export ESMFMKFILE=$spack_install_dir/view/lib/esmf.mk
chmod 755 testProtos.sh
./testProtos.sh
echo "::endgroup::"
