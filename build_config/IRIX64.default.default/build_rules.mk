# $Id: build_rules.mk,v 1.23 2006/01/12 23:05:25 nscollins Exp $
# 
# IRIX64.default.default
#

#
# Default MPI setting.
#
ifeq ($(ESMF_COMM),default)
export ESMF_COMM := mpi
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
############################################################

#
# Location of MPI (Message Passing Interface) software  
#
# We recommend using SGI's MPI implementation over MPICH on the Origin and 
# Powerchallenge.
#
# If you are using the MPICH implementation of MPI with version BELOW 1.1,
# you should remove the -DESMC_HAVE_INT_MPI_COMM. If you are using MPICH 
# Version 1.1 or SGI's version of MPI you MUST retain it.
#
ifeq ($(ESMF_COMM),mpi)
MPI_INCLUDE     += -DESMC_HAVE_INT_MPI_COMM
MPIMPMDRUN	 = mpirun
endif


#
# The following lines can be used with MPICH
#
ifeq ($(ESMF_COMM),mpich)
MPI_LIB        += -lmpi
MPI_INCLUDE    += -DESMC_HAVE_INT_MPI_COMM 
endif

# For IRIX64 the default is MPI_Comm_c2f not supported
# nsc - this seems to be causing problems - try without it.
#CPPFLAGS       += -DESMF_DONT_HAVE_MPI_COMM_C2F

############################################################

# cryptic flags used below
SGI_FLAGS1          = -Wl,-woff,84,-woff,85,-woff,134
SGI_FLAGS2          = -woff 1164

ifeq ($(ESMF_PREC),32)
SIZEFLAG = -n32
SL_ABIOPTS         = -check_registry /usr/lib32/so_locations
CXXINITFILE        = /usr/lib32/c++init_mp.o
endif

ifeq ($(ESMF_PREC),64)
SIZEFLAG = -64
SL_ABIOPTS         = -check_registry /usr/lib64/so_locations
CXXINITFILE        = /usr/lib64/c++init_mp.o
endif

C_CC		   = CC $(SIZEFLAG) -mp $(SGI_FLAGS2) 
C_CXX		   = CC $(SIZEFLAG) -mp $(SGI_FLAGS2) -LANG:std
C_FC		   = f90 $(SIZEFLAG) -mp -macro_expand

C_CLINKER	   = CC $(SIZEFLAG) -mp $(SGI_FLAGS1) -MP:open_mp=ON
C_FLINKER	   = f90 $(SIZEFLAG) -mp $(SGI_FLAGS1) -MP:open_mp=ON

C_SLFLAG           = -rpath

C_CXXF90LD         = CC $(SIZEFLAG)
C_F90CXXLD         = f90 $(SIZEFLAG)

RANLIB             = true

C_CCV		   = cc -version
C_CXXV		   = CC -version
C_FCV              = f90 -version

C_CXXF90LIBS       = -rpath . -lftn -lfortran -lCio -lmpi++ -lmpi -lpthread 
C_F90CXXLIBS       = $(CXXINITFILE) -rpath . -lC -lCio -lc -lmpi++ -lmpi -lpthread 

# fortran flags
F_FREECPP       = -freeform -cpp
F_FIXCPP        = -fixedform -cpp -extend_source
F_FREENOCPP     = -freeform -nocpp
F_FIXNOCPP      = -fixedform -nocpp -extend_source

# additional optimize flags
O_CFLAGS	   += -OPT:Olimit=6500

###############################################################################

PARCH		   = IRIX

# this platform does make a shared lib

C_SL_LIBLINKER = $(C_CXX) $(SIZEFLAG) -shared -rpath .
C_SL_LIBOPTS   = $(SL_ABIOPTS) -rpath $(ESMF_LIBDIR) -shared


