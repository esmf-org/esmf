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
  MPI_FC=mpiifort
else
  export PATH=$spack_install_dir/view/bin:$PATH
  # set it for OpenMPI to fix not enough slots issue
  export TOOLRUN="--oversubscribe --mca orte_base_help_aggregate 0"
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
mpirun -np 2 ./test_mpi.x
echo "::endgroup::"

# increase stack size
echo "::group::Increase Stack Size"
ulimit -s unlimited
ulimit -a
echo "::endgroup::"

# run app prototypes 
echo "::group::Build and Run NUOPC Application Prototypes"
chmod 755 testProtos.sh
#./testProtos.sh 2>&1 | tee testProtos.log
#cat testProtos.log

./testProtos.sh >&testProtos.log&
sleep 120

# attach gdb to processes

echo "#!/bin/bash" > trace_cmd.sh
echo "pid_lst=`ps -ef | grep $2 | awk '{print $2}'`" >> trace_cmd.sh
echo "for i in $pid_lst" >> trace_cmd.sh
echo "do" >> trace_cmd.sh
echo "  prefix=\"${3}_${i}\"" >> trace_cmd.sh
echo "  CONFFILE=\"$1/bt-${prefix}.conf\"" >> trace_cmd.sh
echo "  echo \"set pagination off\" >\"$CONFFILE\"" >> trace_cmd.sh
echo "  echo \"set logging file $1/bt-${prefix}.txt\" >> \"$CONFFILE\"" >> trace_cmd.sh
echo "  echo \"set logging overwrite on\" >> \"$CONFFILE\"" >> trace_cmd.sh
echo "  echo \"set logging redirect on\" >> \"$CONFFILE\"" >> trace_cmd.sh
echo "  echo \"set logging on\" >> \"$CONFFILE\"" >> trace_cmd.sh
echo "  echo \"attach $i\" >> \"$CONFFILE\"" >> trace_cmd.sh
echo "  echo \"bt\" >> \"$CONFFILE\"" >> trace_cmd.sh
echo "  echo \"detach\" >> \"$CONFFILE\"" >> trace_cmd.sh
echo "  echo \"set logging off\" >> \"$CONFFILE\"" >> trace_cmd.sh
echo "  echo \"quit\" >> \"$CONFFILE\"" >> trace_cmd.sh
echo "  gdb --batch -x \"$CONFFILE\" 2>/dev/null" >> trace_cmd.sh
echo "done" >> trace_cmd.sh

chmod 755 trace_cmd.sh

nohup ./trace_cmd.sh `pwd` sleep "gh_runner" > /dev/null 2>&1 &

sleep 120

cat bt-*

exit

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
