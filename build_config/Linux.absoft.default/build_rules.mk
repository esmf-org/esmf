#  $Id: build_rules.mk,v 1.21 2005/07/06 20:43:30 nscollins Exp $
#
#  Linux.absoft.default makefile fragment
#


#
#  Make sure that ESMF_PREC is set to 32
#  When a 64-bit compiler comes out, then just make the default 32, but
#  allow it to be set to 64 ahead of time.  (We will also need to add
#  sections for 32 vs 64 bit if the flags are different.)
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

# Set ESMF_COMM depending on whether you have installed the mpich
# or lam library.  The default location is to have the include files
# and libs installed in /usr/local - if they are someplace else,
# set MPI_HOME first.  (the mpiuni case is handled in the common.mk file.)

ifeq ($(ESMF_COMM),lam)
# with lam-mpi installed:
MPI_LIB        += -lmpi -llam 
endif

ifeq ($(ESMF_COMM),mpich)
# with mpich installed:
MPI_INCLUDE    += -DESMF_MPICH
MPI_LIB        += -lmpich
MPIRUN         += $(ESMF_NODES)
endif

# name of the lib which includes the posix thread support.
THREAD_LIB     = -lpthread


############################################################
#


# compilers and flags

ifneq ($(ESMF_COMM),mpich)
C_CC            = cc
C_CXX           = g++ -fPIC
C_FC            = f95
FFLAGS          += -YEXT_NAMES=LCS -s  -YEXT_SFX=_
C_FCV           = (x="`f95 -V 2>/dev/null`"; \
                  if [ -n "$$x" ] ; then echo $$x ; else f90fe -V ; fi)
# on absoft 8 and before, docs say f95 -V should work, but it causes an error.
# f90fe -V prints good version info.   absoft 9 and later, however, fixes this
# and now f90 -V prints good info, and f90fe gives license errors.  so try
# doing both, and take the one which is not null.
endif

ifeq ($(ESMF_COMM),mpich)
C_CC            = mpicc
C_CXX           = mpiCC -fPIC
C_FC            = mpif90 
FFLAGS          += -YEXT_NAMES=LCS -s 
C_FCV           = ${C_FC} -v 2>/dev/null
endif

# common settings

C_FC_MOD        = -p

C_CCV		= ${C_CC} --version
C_CXXV		= ${C_CXX} --version


# fortran flags

F_FREECPP       = -ffree
F_FIXCPP        = -ffixed
F_FREENOCPP     = -ffree
F_FIXNOCPP      = -ffixed

# by default append each directory which is in LD_LIBRARY_PATH to
# the -L flag and also to the run-time load flag.  (on systems which
# support the 'module' command, that is how it works - by adding dirs
# to LD_LIBRARY_PATH.)  if it is not set, default to where the absoft
# compilers try to install themselves.  if your compiler is someplace else
# either set LD_LIBRARY_PATH first, or make a site specific file and
# edit the paths explicitly.
ifneq ($(origin LD_LIBRARY_PATH), environment)
CXXLIB_PATHS   = -L/usr/lib
F90LIB_PATHS   = -L/soft/com/packages/absoft-9.0/opt/absoft/lib
C_LIB_PATHS      = ${CXXLIB_PATHS} ${F90LIB_PATHS}
C_LD_PATHS       = ${CXXLIB_PATHS} ${F90LIB_PATHS}
else
C_LIB_PATHS      = $(ENV_LIB_PATHS)
C_LD_PATHS       = $(ENV_LD_PATHS)
endif

C_F90CXXLIBS       = -lf90math -lfio -lf77math -lrt -lstdc++ 
C_CXXF90LIBS       = -lstdc++ -lf90math -lrt -lfio -lf77math

###############################################################################

PARCH		   = linux


SL_LIBS_TO_MAKE = 
C_SL_LIBOPTS  = 

