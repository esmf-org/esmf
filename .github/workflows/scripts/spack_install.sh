#!/bin/bash

# get arguments
while getopts r: flag
do
  case "${flag}" in
    r) run_dir=${OPTARG};;
  esac
done

# check for default values
if [[ -z "$run_dir" || ! -z `echo $run_dir | grep '^-'` ]]; then
  run_dir=`pwd`
fi

# print out arguments
echo "Run Directory: $run_dir";

# go to directory
cd $run_dir

# install spack environment
echo "::group::Install Spack Packages"
. spack/share/spack/setup-env.sh
spack --color always -e $run_dir/. install -j3 --deprecated --no-checksum
echo "::endgroup::"
