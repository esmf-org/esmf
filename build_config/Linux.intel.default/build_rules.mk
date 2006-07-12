# $Id: build_rules.mk,v 1.48.2.2 2006/07/12 06:56:21 theurich Exp $
#
# Linux.intel.default
#

############################################################
# Default MPI setting.
#
ifeq ($(ESMF_COMM),default)
export ESMF_COMM := mpiuni
endif


############################################################
# MPI dependent settings.
#
ifeq ($(ESMF_COMM),mpiuni)
# MPI stub library -----------------------------------------
ESMF_F90DEFAULT         = ifort
ESMF_F90LINKLIBS       += -lmpiuni
ESMF_CXXDEFAULT         = icpc
ESMF_CXXCOMPILEOPTS    += -DESMF_MPIUNI
ESMF_CXXCOMPILEPATHS   += -I$(ESMF_DIR)/src/Infrastructure/stubs/mpiuni
ESMF_CXXLINKLIBS       += -lmpiuni
ESMF_MPIRUNDEFAULT      = $(ESMF_DIR)/src/Infrastructure/stubs/mpiuni/mpirun
else
ifeq ($(ESMF_COMM),mpi)
# Vendor MPI -----------------------------------------------
ESMF_F90DEFAULT         = ifort
ESMF_F90LINKLIBS       += -lmpi -lmpi++
ESMF_CXXDEFAULT         = icpc
ESMF_CXXLINKLIBS       += -lmpi -lmpi++
ESMF_MPIRUNDEFAULT      = mpirun
ESMF_MPIMPMDRUNDEFAULT  = mpirun
else
ifeq ($(ESMF_COMM),mpich)
# Mpich ----------------------------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpiCC
ESMF_CXXCOMPILEOPTS    += -DESMF_MPICH
ESMF_MPIRUNDEFAULT      = mpirun
else
ifeq ($(ESMF_COMM),mpich2)
# Mpich2 ---------------------------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpicxx
ESMF_MPIRUNDEFAULT      = mpirun
ESMF_MPIMPMDRUNDEFAULT  = mpiexec
else
ifeq ($(ESMF_COMM),intelmpi)
# IntelMPI -------------------------------------------------
ESMF_F90DEFAULT         = mpiifort
ESMF_CXXDEFAULT         = mpiicpc
ESMF_MPIRUNDEFAULT      = mpirun
ESMF_MPIMPMDRUNDEFAULT  = mpiexec
else
ifeq ($(ESMF_COMM),lam)
# LAM (assumed to be built with ifort) ---------------------
ESMF_F90DEFAULT         = mpif77
ESMF_CXXDEFAULT         = mpic++
ESMF_MPIRUNDEFAULT      = mpirun
ESMF_MPIMPMDRUNDEFAULT  = mpiexec
else
ifeq ($(ESMF_COMM),openmpi)
# OpenMPI --------------------------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpicxx
ESMF_MPIRUNDEFAULT      = mpirun
ESMF_MPIMPMDRUNDEFAULT  = mpiexec
else
$(error Invalid ESMF_COMM setting: $(ESMF_COMM))
endif
endif
endif
endif
endif
endif
endif

############################################################
#
# location of external libs.  if you want to use any of these,
# define ESMF_SITE to my_site so the build system can find it,
# copy this file into Linux.intel.my_site, and uncomment the
# libs you want included.  remove the rest of this file since
# both this file and the site file will be included.

# LAPACK_INCLUDE   = 
# LAPACK_LIB       = -L/usr/local/lib -llapack
# NETCDF_INCLUDE   = -I/usr/local/include/netcdf
# NETCDF_LIB       = -L/usr/local/lib -lnetcdf
# HDF_INCLUDE      = -I/usr/local/include/hdf
# HDF_LIB          = -L/usr/local/lib/ -lmfhdf -ldf -ljpeg -lz
# BLAS_INCLUDE     = 
# BLAS_LIB         = -latlas -lscs

############################################################
#
# On IA64 set long and pointer types to 64-bit
ifeq ($(ESMF_PREC),64)
ESMF_CXXCOMPILEOPTS       += -size_lp64
ESMF_CXXLINKOPTS          += -size_lp64
ESMF_F90COMPILEOPTS       += -size_lp64
ESMF_F90LINKOPTS          += -size_lp64
endif

############################################################
#
# To compile with intel icpc but link with GCC's stdc++ lib
# set ESMF_STDCXX_LIBRARY to gcc before building
ifeq ($(ESMF_STDCXX_LIBRARY),gcc)
ESMF_F90LINKPATHS   += -L$(dir $(shell gcc -print-file-name=libstdc++.so))
ESMF_F90LINKLIBS    += -lstdc++
ESMF_CXXCOMPILEOPTS += -cxxlib-gcc
ESMF_CXXLINKOPTS    += -cxxlib-gcc
else
ESMF_F90LINKLIBS    += -lcprts
ESMF_CXXCOMPILEOPTS += -cxxlib-icc
ESMF_CXXLINKOPTS    += -cxxlib-icc
endif

############################################################
#
# Conditionally add pthread compiler and linker flags
ifeq ($(ESMF_PTHREADS),ON)
ESMF_F90COMPILEOPTS +=  -threads
ESMF_CXXCOMPILEOPTS +=  -pthread
ESMF_F90LINKOPTS    += -threads
ESMF_CXXLINKOPTS    += -pthread
endif

############################################################
#
# How to print versions
ESMF_CXXVOPT        = -V -v
ESMF_F90VOPT        = -V -v

############################################################
#
# Need this until the file convention is fixed (then remove these two lines)
ESMF_F90COMPILEFREENOCPP = -fpp0 -FR
ESMF_F90COMPILEFIXCPP    = -fpp

############################################################
#
# Determine where ifort's libraries are located
ESMF_CXXLINKPATHS += -L$(dir $(shell $(ESMF_DIR)/scripts/ifort-libpath $(ESMF_F90COMPILER)))
ESMF_CXXLINKRPATHS += \
  -Wl,-rpath,$(dir $(shell $(ESMF_DIR)/scripts/ifort-libpath $(ESMF_F90COMPILER)))

############################################################
#
# Link against libesmf.a using the F90 linker front-end
ESMF_F90LINKLIBS += -limf -lm -lcxa -lunwind -lrt -ldl

############################################################
#
# Link against libesmf.a using the C++ linker front-end
ESMF_CXXLINKLIBS += -lifcoremt -lunwind -lrt -ldl

###############################################################################

SL_LIBS_TO_MAKE = 
C_SL_LIBOPTS  =

