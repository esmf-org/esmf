#!/bin/bash

module purge
module use -a /ford1/share/gmao_SIteam/modulefiles
#ml GEOSpyD/25.3.1-0/3.13 ifort/2021.13.0 intelmpi/2021.13
ml ifort/2021.13.0 intelmpi/2021.13

export BASE_DIR=/home/bmauer/build_baselibs/Baselibs-8.27.0/src
export INSTALL_DIR=/home/bmauer/build_baselibs/Baselibs-8.27.0/x86_64-pc-linux-gnu/ifort_2021.13.0-intelmpi_2021.13/Linux
export PATH=$PATH:$INSTALL_DIR/bin
export ESMF_DIR=$BASE_DIR/esmf
export ESMF_BOPT=g
export ESMF_COMM=intelmpi
export ESMF_COMPILER=intel
export ESMF_INSTALL_PREFIX=$INSTALL_DIR
export ESMF_INSTALL_HEADERDIR=$INSTALL_DIR/include/esmf
export ESMF_INSTALL_MODDIR=$INSTALL_DIR/include/esmf
export ESMF_INSTALL_LIBDIR=$INSTALL_DIR/lib
export ESMF_INSTALL_BINDIR=$INSTALL_DIR/bin
export ESMF_PIO=internal
export ESMF_NETCDF=nc-config
export ESMF_TESTEXHAUSTIVE=ON

#make info
#make distclean
#make -j7 lib
#make -j7 install
#make -j6  build_system_tests
#make -j6 all_tests
#make clean_all_tests
#make check
#make doc


# how to run python tests just for regridding:
# and don't for get to do this:
rm pytest*.log
source /home/bmauer/myvenvs/esmpy/bin/activate
export ESMFMKFILE=/home/bmauer/build_baselibs/Baselibs-8.27.0/x86_64-pc-linux-gnu/ifort_2021.13.0-intelmpi_2021.13/Linux/lib/esmf.mk
python3 -m pip install .
#mpirun -np 4 python3 -m pytest -vs src/esmpy/test/test_api/test_regrid.py |& tee pytest.log
#python3 -m pytest -vs src/esmpy/test/test_api/test_regrid.py |& tee pytest.log
#python3 -m pytest -vs src/esmpy/test/test_api/test_grid.py |& tee pytest.log
#mpirun -np 4 python3 -m pytest 
python3 -m pytest |& tee pytest_np1.log
mpirun -np 2 python3 -m pytest -vs src/esmpy/test/test_api/test_regrid.py |& tee pytest_np4.log
rm PET*LogFile
