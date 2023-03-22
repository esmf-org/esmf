#!/bin/bash

# get arguments
while getopts c:r:s: flag
do
  case "${flag}" in
    c) comp=${OPTARG};;
    r) run_dir=${OPTARG};;
    s) spack_install_dir=${OPTARG};;
  esac
done

# check for default values
if [[ -z "$comp" || ! -z `echo $comp | grep '^-'` ]]; then
  comp="gcc@11.3.0"
fi

if [[ -z "$run_dir" || ! -z `echo $run_dir | grep '^-'` ]]; then
  run_dir=`pwd`
fi

if [[ -z "$spack_install_dir" || ! -z `echo $spack_install_dir | grep '^-'` ]]; then
  spack_install_dir=`pwd`
fi

# print out arguments
echo "Compiler               : $comp"
echo "Run Directory          : $run_dir";
echo "Spack Install Directory: $spack_install_dir";

# go to directory
cd $run_dir

# run app prototypes 
echo "::group::Build and Run NUOPC Application Prototypes"
export ESMFMKFILE=$spack_install_dir/view/lib/esmf.mk
if [[ "$comp" == *"oneapi"* ]]; then
  # this is not correct, need to be changed later
  . /opt/intel/oneapi/setvars.sh
else
  export PATH=$spack_install_dir/view/bin:$PATH
  # set it for OpenMPI to fix not enough slots issue
  export TOOLRUN="--oversubscribe"
fi
which mpirun
chmod 755 testProtos.sh
./testProtos.sh >& testProtos.log
cat testProtos.log
echo "::endgroup::"

# process output
echo "::group::Process Output of NUOPC Application Prototypes"
lineFrom=`cat -n testProtos.log | grep "== TEST SUMMARY START ==" | awk '{print $1}'`
lineTo=`cat -n testProtos.log | grep "== TEST SUMMARY STOP ==" | awk '{print $1}'`
result=`sed -n '${lineFrom},${lineTo}p' testProtos.log | grep "FAIL"`
echo $result
if [[ ! -z "$result" ]]; then
  echo "Failed NUOPC app prototypes ..."
  exit 1
fi
echo "::endgroup::"
