#  $Id: build_rules.mk,v 1.16 2005/04/20 22:12:33 nscollins Exp $
#
#  Darwin.nag.default
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
# copy this file into Darwin.nag.my_site, and uncomment the
# libs you want included.  remove the rest of this file since
# both this file and the site file will be included.

# LAPACK_INCLUDE   = 
# LAPACK_LIB       = -L/usr/local/lib -llapack -lessl
# NETCDF_INCLUDE   = -I/usr/local/include/netcdf
# NETCDF_LIB       = -L/usr/local/lib -lnetcdf
# HDF_INCLUDE      = -I/usr/local/include/hdf
# HDF_LIB          = -L/usr/local/lib -lmfhdf
# BLAS_INCLUDE     = 
# BLAS_LIB         = -lblas

#
############################################################

# Location of MPI (Message Passing Interface) software

# Set ESMF_COMM depending on whether you have installed the mpich
# or lam library.  The default location is to have the include files
# and libs installed in /usr/local - if they are someplace else,
# set MPI_HOME first.  (the mpiuni case is handled in the common.mk file.)

ifeq ($(ESMF_COMM),lam)
MPI_LIB        += -llamf77mpi -lmpi -llam
endif

ifeq ($(ESMF_COMM),mpich)
# with mpich installed in $MPI_HOME or /usr/local:
MPI_INCLUDE    += -DESMF_MPICH=1
MPI_LIB        += -lmpich -lpmpich
endif



############################################################

# commands which do not match the defaults exactly
RANLIB		   = ranlib -s
SED		   = /usr/bin/sed

# compilers
C_CC		   = cc
C_CXX		   = g++ -fPIC
C_FC		   = f95 

C_CCV		   = ${C_CC} --version
C_CXXV		   = ${C_CXX} --version
C_FCV              = ${C_FC} -V 

#
# Fortran flags
#
FFLAGS          += -kind=byte -w=x77 -mismatch_all -gline
F_FREECPP       = -free -fpp
F_FIXCPP        = -fixed -fpp
F_FREENOCPP     = -free
F_FIXNOCPP      = -fixed

C_F90CXXLIBS       = -L/usr/local/lib/NAGware -lf96 -lstdc++
C_CXXF90LIBS       = -L/usr/local/lib/NAGware -lf96 -lstdc++ \
                     /usr/local/lib/NAGWare/libf96.a  \
                     /usr/local/lib/NAGWare/quickfit.o

###############################################################################

PARCH		   = mac_osx

# set this to libesmf.so to build shared
SL_LIBS_TO_MAKE = 
C_SL_LIBOPTS  = 


