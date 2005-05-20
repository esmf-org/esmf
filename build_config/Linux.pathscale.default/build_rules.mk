#  $Id: build_rules.mk,v 1.5 2005/05/20 16:48:23 nscollins Exp $

#
#  Linux.pathscale.default makefile
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
# copy this file into Linux.pathscale.my_site, and uncomment the
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
# with lam-mpi installed:
MPI_LIB        += -lmpi -llam 
endif

ifeq ($(ESMF_COMM),mpich)
# with mpich installed:
MPI_LIB        += -lmpich
MPI_INCLUDE    += -DESMF_MPICH
MPIRUN         += $(ESMF_NODES)
endif

# name of the lib which includes the posix thread support.
THREAD_LIB     = -lpthread


############################################################
# compilers

ifneq ($(ESMF_COMM),mpich)
C_CC    = pathcc
C_CXX   = pathCC
C_FC    = pathf90
endif

ifeq ($(ESMF_COMM),mpich)
C_CC    = mpicc
C_CXX   = mpiCC 
C_FC    = mpif90 
endif

ifeq ($(ESMF_PREC),64)
C_CC   += -default64
C_CXX  += -default64
C_FC   += -default64
endif

# version
C_CCV	= ${C_CC} -v
C_CXXV	= ${C_CXX} -v
C_FCV   = ${C_F} -v 

# fortran flags

F_FREECPP       = -freeform -cpp
F_FIXCPP        = -fixedform -cpp
F_FREENOCPP     = -freeform
F_FIXNOCPP      = -fixedform

# cross compiling libs - these will need to be filled in.

C_F90CXXLIBS       = 
C_CXXF90LIBS       = 

###############################################################################

PARCH		   = linux

SL_LIBS_TO_MAKE = 
C_SL_LIBOPTS  = 


