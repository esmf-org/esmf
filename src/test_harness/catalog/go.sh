#!/bin/csh
#PBS -l select=16:ncpus=1
#PBS -l walltime=01:00:00
#PBS -o /work01/oba/gblock/TestApps/field_harness/output
#PBS -e /work01/oba/gblock/TestApps/field_harness/errlog
#PBS -q debug
#
# batch file to allocate processors for catalog generation
# 
# to generate catalog:
#  qsub -I go.sh
#  cd $ESMF_DIR/src/test_harness/catalog
#  gmake
#
setenv PBS_O_WORKDIR $ESMF_DIR/src/test_harness/catalog
cd $PBS_O_WORKDIR
setenv OMP_NUM_THREADS 1
limit stacksize unlimited
limit coredumpsize 0
#mpirun -np 16 gmake
