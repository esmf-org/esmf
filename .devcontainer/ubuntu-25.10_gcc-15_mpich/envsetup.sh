#!/bin/bash

# Spack setup
. ${SPACK_ROOT}/share/spack/setup-env.sh
spack load cmake
spack load mpich
spack load hdf5
spack load netcdf-c
spack load netcdf-fortran
spack load python

# Environment setup
export CMAKE_ROOT=$(spack location -i cmake)
export MPICH_ROOT=$(spack location -i mpich)
export NETCDF_C_ROOT=$(spack location -i netcdf-c)
export NETCDF_FORTRAN_ROOT=$(spack location -i netcdf-fortran)

# ESMF build options
export ESMF_BOPT="O"
export ESMF_COMPILER="gfortran"
export ESMF_COMM="mpich"
export ESMF_NETCDF="nc-config"
export ESMF_PIO="internal"
export ESMF_TESTEXHAUSTIVE="ON"

# Print Welcome Message
echo "Welcome to the ESMF Development Container!"
echo "*** ${ESMF_DEVCONTAINER} ***"
echo ""
echo "The following packages have been pre-loaded:"
spack find --loaded --format "{name}@{version}"
echo ""
echo "The following ESMF environment variables have been pre-set:"
printenv | grep "ESMF_"
echo ""
