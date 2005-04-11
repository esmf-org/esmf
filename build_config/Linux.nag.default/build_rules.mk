#  $Id: build_rules.mk,v 1.22 2005/04/11 15:53:38 nscollins Exp $
#
#  Linux.nag.default.mk
#


#
#  Make sure that ESMF_PREC is set to 32
#
ESMF_PREC = 32

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
# copy this file into Linux.nag.my_site, and uncomment the
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

# comment in one or the other, depending on whether you have
# installed the mpich or lam library. 

ifeq ($(ESMF_COMM),lam)
# with lam-mpi installed:
MPI_LIB        += -lmpi -llam 
THREAD_LIB     = -lpthread
endif

ifeq ($(ESMF_COMM),mpich)
MPI_LIB        += -lmpich
MPI_INCLUDE    += -DESMF_MPICH=1
MPIRUN         += $(ESMF_NODES)
endif


# compilers
ifeq ($(ESMF_COMM),mpich)
C_CC		   = mpicc
C_CXX		   = mpiCC -fPIC
C_FC		   = mpif90
C_CCV		   = ${C_CC} -v 2>&1 | head -3
C_CXXV		   = ${C_CXX} -V 2>&1 | head -3
C_FCV              = ${C_FC} -V 2>&1 | head -2
EXTRALIBS          =
else
C_CC		   = gcc
C_CXX		   = g++ -fPIC
C_FC		   = f95
C_CCV		   = ${C_CC} -v 
C_CXXV		   = ${C_CXX} -v 2>&1 | head -2
C_FCV              = ${C_FC} -V 
EXTRALIBS          = ${F90LIBBASE}/safefit.o
endif

# fortran flags
#FFLAGS          += -w=x77 -kind=byte -dusty -mismatch_all-gline
FFLAGS          += -kind=byte -dusty
F_FREECPP       = -free -fpp
F_FIXCPP        = -fixed -fpp
F_FREENOCPP     = -free
F_FIXNOCPP      = -fixed

# use LD_LIBRARY_PATH, but if not set, put in some plausible defaults
ifneq ($(origin LD_LIBRARY_PATH), environment)
LIB_PATHS      =
CXXLIB_PATHS   = -L/soft/com/packages/intel-8.1/lib
F90LIB_PATHS   = -L/soft/com/packages/nag-f95-5.0/lib
endif

# include the lib which defines a fast intel memcpy if compiling optimized.
ifeq ($(ESMF_BOPT),O)
EXTRALIBS         += -lifcoremt
endif

C_F90CXXLIBS       = ${LIB_PATHS} \
                     ${F90LIB_PATHS} -lrt -lf96 \
                     ${CXXLIB_PATHS} -lcxa -lunwind -lstdc++ ${EXTRALIBS}
C_CXXF90LIBS       = ${LIB_PATHS} ${F90LIB_PATHS} -lrt -lf96 ${EXTRALIBS}


###########

PARCH		   = linux

SL_LIBS_TO_MAKE = 
C_SL_LIBOPTS  = 


