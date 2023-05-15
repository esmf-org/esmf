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

# output esmf.mk file for debugging
echo "::group::Content of esmf.mk"
cat $spack_install_dir/view/lib/esmf.mk
echo "::endgroup::"

# debug
echo "::group::Prepare Environment and Debug Output for NUOPC Application Prototypes"
export ESMFMKFILE=$spack_install_dir/view/lib/esmf.mk
if [[ "$comp" == *"oneapi"* ]]; then
  # this is not correct, need to be changed later
  . /opt/intel/oneapi/setvars.sh
  export TOOLRUN=""
  MPI_FC=mpiifort
else
  export PATH=$spack_install_dir/view/bin:$PATH
  # set it for OpenMPI to fix not enough slots issue
  export TOOLRUN="--oversubscribe --mca btl_tcp_if_include eth0"
  MPI_FC=mpif90
fi
which mpirun
cat testProtos.sh | grep "^Test"
echo "::endgroup::"

# test installation by building and running simple MPI job
echo "::group::Test Installation"
echo "program test_mpi" > test_mpi.F90
echo "  use mpi" >> test_mpi.F90
echo "  integer :: err, p, id" >> test_mpi.F90
echo "  call MPI_Init(err)" >> test_mpi.F90
echo "  call MPI_Comm_size(MPI_COMM_WORLD, p, err)" >> test_mpi.F90
echo "  call MPI_Comm_rank(MPI_COMM_WORLD, id, err)" >> test_mpi.F90
echo "  print*, 'Hello from ', id, ' of ', p" >> test_mpi.F90
echo "  call MPI_Finalize(err)" >> test_mpi.F90
echo "end program test_mpi" >> test_mpi.F90
$MPI_FC -o test_mpi.x test_mpi.F90
mpirun -np 2 ${TOOLRUN} ./test_mpi.x
echo "::endgroup::"

# run app prototypes 
echo "::group::Build and Run NUOPC Application Prototypes"
# make proto script executable
chmod 755 testProtos.sh
# run app prototypes in the background
./testProtos.sh 2>&1 | tee testProtos.log
echo "::endgroup::"

# process output
echo "::group::Process Output of NUOPC Application Prototypes"
sed -n '/== TEST SUMMARY START ==/, /== TEST SUMMARY STOP ==/p' testProtos.log >& testProtos_summary.log
result_fail=`cat testProtos_summary.log | grep "FAIL"`
echo "-------------"
echo "FAILED TESTS:"
echo "-------------"
cat testProtos_summary.log | grep "FAIL"
echo "-------------"
echo "PASSED TESTS:"
echo "-------------"
cat testProtos_summary.log | grep "PASS"
if [[ ! -z "$result_fail" ]]; then
  echo "Some of NUOPC app prototypes are failed! Exiting ..."
  exit 1
fi
echo "::endgroup::"
