# $Id: build_rules.mk,v 1.8 2006/09/22 23:55:40 theurich Exp $
# 
#  NEC SX build, cross compiler on a Linux front end
#

export ESMF_ABI := 64


AR = sxar
LD = sxld


#
# Default MPI setting.
#
ifndef ESMF_COMM
export ESMF_COMM := sxmpi
endif
ifeq ($(ESMF_COMM),default)
export ESMF_COMM := sxmpi
endif

############################################################
#
# location of external libs.  if you want to use any of these,
# define ESMF_SITE to my_site so the build system can find it,
# copy this file into IRIX64.default.my_site, and uncomment the
# libs you want included.  remove the rest of this file since
# both this file and the site file will be included.

# LAPACK_INCLUDE   =
# LAPACK_LIB       = -L/usr/local/lib -llapacko
# NETCDF_INCLUDE   = -I/usr/local/include/netcdf
# NETCDF_LIB       = -L/usr/local/lib -lnetcdf
# HDF_INCLUDE      = -I/usr/local/include/hdf
# HDF_LIB          = -L/usr/local/lib -lmfhdf -ldf -ljpeg -lz
# BLAS_INCLUDE     =
# BLAS_LIB         = -latlas

#
###########################################################

#
# Location of MPI (Message Passing Interface) software  
#
# If you are using the MPICH implementation of MPI with version BELOW 1.1,
# you should remove the -DESMC_HAVE_INT_MPI_COMM. If you are using MPICH 
# Version 1.1 or NEC's version of MPI you MUST retain it.
#
ifeq ($(ESMF_COMM),sxmpi)
MPI_INCLUDE     = -DESMC_HAVE_INT_MPI_COMM
MPIRUN          = ${ESMC_MPIRUN}
endif


############################################################

ifeq ($(ESMF_ABI),32)

echo "NEC system is 64 bit only"
exit

endif


# the SX does not have support for 1 byte integers, and
# i believe 2 byte ints were also problematic.  
# these disable their creation in the code.

FPPDEFS += -DESMF_NO_INTEGER_1_BYTE -DESMF_NO_INTEGER_2_BYTE -DESMF_NEC_KIND_I8

# the SX does not support POSIX IPC (memory mappped files)
CFLAGS     += -DESMF_NOPOSIXIPC

############################################################
#
ifeq ($(ESMF_ABI), 64)

LD		   = sxmpic++ 

# compilers
#
C_CC		   = sxmpic++ -Xa 
C_CXX		   = sxmpic++ -K exceptions
C_FC		   = sxmpif90 -EP -dW

C_CLINKER	   = sxmpic++ 
C_FLINKER	   = sxmpif90

#
C_CCV              = sxmpic++ -Xa -V
C_CXXV		   = sxmpic++ -K exceptions -V
C_FCV		   = sxmpif90 -EP -dW -V

C_CXXF90LIBS       = -lpthread -lf90sxe -lm90sxe -li90sx -lC++_eh -lm
C_F90CXXLIBS       = -lpthread -lC++_eh -lcpp

endif

############################################################

PARCH		   = sxcross

# do not fool with shared libraries until we build here

# when we are ready, set the first line to = libesmf.so, and set the
# second to the proper link/load flags to create a shared lib.
SL_LIBS_TO_MAKE =
C_SL_LIBOPTS  =
