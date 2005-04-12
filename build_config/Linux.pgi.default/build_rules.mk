# $Id: build_rules.mk,v 1.22 2005/04/12 15:12:09 nscollins Exp $
#
#  Linux.pgi.default
#


#
# Default MPI setting.
#
ifeq ($(ESMF_COMM),default)
export ESMF_COMM := mpiuni
endif


############################################################
#
# location of external libs.  if you want to use any of these,
# define ESMF_SITE to my_site so the build system can find it,
# copy this file into Linux.absoft.my_site, and uncomment the
# libs you want included.  remove the rest of this file since
# both this file and the site file will be included.

# LAPACK_INCLUDE   = 
# LAPACK_LIB       = -L/usr/local/lib -llapack
# NETCDF_INCLUDE   = -I/usr/local/include/netcdf
# NETCDF_LIB       = -L/usr/local/lib -lnetcdf
# HDF_INCLUDE      = -I/usr/local/include/hdf
# HDF_LIB          = -L/usr/local/lib/ -lmfhdf -ldf -ljpeg -lz
# BLAS_INCLUDE     = 
# BLAS_LIB         = -latlas

#
############################################################

# Location of MPI (Message Passing Interface) software

ifeq ($(ESMF_COMM),lam)
# this section is set up for LAM mpi
MPI_LIB        += -lmpi -llam
THREAD_LIB     = -lpthread
endif

# for mpich: if your system sets either MPI_HOME or MPICH to the location of
# the mpi include files, libraries, and binary executables, this makefile 
# should work unchanged.  (MPICH takes precedence over MPI_HOME if both are 
# set.)  if your system has mpich installed but not in either /usr or
# /usr/local, and neither of these environment variables are set, you must 
# set one of them to the location where the include, lib, and bin dirs will 
# be found.

# set up to use MPICH
ifeq ($(ESMF_COMM),mpich)
 ifdef MPICH
  export MPI_HOME := $(MPICH)
 endif
MPI_INCLUDE    += -DESMF_MPICH=1
MPI_LIB        += -lpmpich++ -lmpich
MPIRUN         += $(ESMF_NODES)
endif

# compilers
#

# the default is to use the pgi C and C++ compilers.
# if you want gcc and g++, set ESMF_C_COMPILER to gcc before building.

ifneq ($(ESMF_COMM),mpich)
ifeq ($(ESMF_C_COMPILER),gcc)
C_CC               = gcc
C_CXX              = g++
C_FC               = pgf90
else
C_CC		   = pgcc 
C_CXX		   = pgCC
C_FC		   = pgf90
endif
endif

# if you are using mpich, then however the mpich wrappers have been built
# will determine which compilers you are using.
ifeq ($(ESMF_COMM),mpich)
C_CC		   = mpicc
C_CXX		   = mpiCC
C_FC		   = mpif90
endif

# the default is to link with the pgi C and C++ libraries unless you have
# already set ESMF_C_COMPILER to gcc. 
ifeq ($(ESMF_C_COMPILER),gcc)
PGI_C_LIB_NEEDED = -lstdc++
else
PGI_C_LIB_NEEDED =
endif

# print version 
C_CCV              = ${C_CC} -V -v
C_CXXV             = ${C_CXX} -V -v
C_FCV              = ${C_FC} -V -v

# fortran flags
F_FREECPP          = -Mpreprocess -Mfreeform
F_FIXCPP           = -Mpreprocess -Mnofreeform
F_FREENOCPP        = -Mfreeform
F_FIXNOCPP         = -Mnofreeform

# use LD_LIBRARY_PATH to find settings; this is just here as a default
# if the variable is unset
ifneq ($(origin LD_LIBRARY_PATH), environment)
LIB_PATHS   = -L/opt/pgi-5.2/linux86/5.2/lib
LD_PATHS    = $(C_SLFLAG)/opt/pgi/pgi-5.2/linux86/5.2/lib
endif

C_F90CXXLIBS       = $(LD_PATHS) $(LIB_PATHS) $(PGI_C_LIB_NEEDED) -lrt -lC -lc
C_CXXF90LIBS       = $(LD_PATHS) $(LIB_PATHS) $(PGI_C_LIB_NEEDED) -lrt -lC \
                     -lpgf90 -lpgf90_rpm1 -lpgf902 -lpgf90rtl -lpgftnrtl
###############################################################################

PARCH		   = linux_pgi


SL_LIBS_TO_MAKE = 
C_SL_LIBOPTS  = -shared


