#  $Id: build_rules.mk,v 1.17 2006/03/20 20:13:19 tjcnrl Exp $
#
#  Darwin.xlf.default
#
# if you have xlf and gcc/g++, this should work as-is.
# if you also have xlc/xlC and want to use them, then:
#   setenv ESMF_C_COMPILER xlc
# before building.
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

#
# default C compiler setting.
#
ifndef ESMF_C_COMPILER
export ESMF_C_COMPILER := gcc
endif

############################################################
#
# location of external libs.  if you want to use any of these,
# define ESMF_SITE to my_site so the build system can find it,
# copy this file into Darwin.xlf.my_site, and uncomment the
# libs you want included.  remove the rest of this file since
# both this file and the site file will be included.

# LAPACK_INCLUDE   = 
# LAPACK_LIB       = -L/usr/local/lib -llapack
# NETCDF_INCLUDE   = -I/usr/local/include/netcdf
# NETCDF_LIB       = -L/usr/local/lib -lnetcdf
# HDF_INCLUDE      = -I/usr/local/include/hdf
# HDF_LIB          = -L/usr/local/lib -lmfhdf -ldf -ljpeg -lz
# BLAS_INCLUDE     = 
# BLAS_LIB         = -L/usr/local/lib -latlas

#
############################################################


# Location of MPI (Message Passing Interface) software

# Set ESMF_COMM depending on whether you have installed the mpich
# or lam library.  The default location is to have the include files
# and libs installed in /usr/local - if they are someplace else,
# set MPI_HOME first.  (the mpiuni case is handled in the common.mk file.)

ifeq ($(ESMF_COMM),lam)
# with lam-mpi installed in $MPI_HOME or /usr/local:
MPI_LIB        +=  -lmpi -llam -llamf77mpi
endif

ifeq ($(ESMF_COMM),mpich)
# with mpich installed in $MPI_HOME or /usr/local:
CPPFLAGS       += -DESMF_MPICH=1
MPI_LIB        += -lmpich -lpmpich
endif


# common commands which differ from the defaults somehow
SED			= /usr/bin/sed

#
# C compiler flags
#
RESTRICTED_POINTERS	= -qkeyword=restrict
NO_AUTO_PARALLEL	= -qsmp=noauto
NO_INLINING		= -Q
NO_LINE_DIRECTIVES	= -P

#
# Fortran compiler flags
#
REAL8			= -qrealsize=8
STRICT			= -qstrict
FPP_PREFIX		= -WF,
F_FREECPP               = -qfree=f90 -qsuffix=cpp=F90
F_FIXCPP                = -qfixed=132 -qsuffix=cpp=F
F_FREENOCPP             = -qfree=f90 -qsuffix=f=f90
F_FIXNOCPP              = -qfixed=132 -qsuffix=f=f        

#
# compiler section
#
ifeq ($(ESMF_COMM),mpich)
# with mpich, have to call wrappers instead of compilers directly
# for now, assume they are built with gcc settings and not around xlc
C_CC		= mpicc
C_CCV		= $(C_CC) --version
C_CXX		= mpicxx
C_CXXV		= $(C_CXX) --version
C_FC		= mpif90 
C_FCV		= $(C_FC) --version

CXXLIB_PATHS    =
F90LIB_PATHS    = -L/opt/ibmcmp/xlf/8.1/lib
C_CXXF90LIBS	= -L. $(CXXLIB_PATHS) -lm -lstdc++ \
		      $(F90LIB_PATHS) -lxlf90_r -lxlfmath -lxl
C_F90CXXLIBS	= -L. $(CXXLIB_PATHS) -lstdc++ \
		      $(F90LIB_PATHS) -lxlf90_r

else
# non-mpich section: lam or mpiuni

ifeq ($(ESMF_C_COMPILER),gcc)
# default is to use gcc/g++ for the compiling C files
C_CC		= gcc
C_CCV		= $(C_CC) -v
C_CXX		= g++
C_CXXV		= $(C_CXX) -v
CXXLIB_PATHS    =
F90LIB_PATHS    = -L/opt/ibmcmp/xlf/8.1/lib
C_CXXF90LIBS	= ${MPI_LIB} -lstdc++ -L. \
		  $(F90LIB_PATHS) -lxlf90_r -lxlfmath -lxl
C_F90CXXLIBS	= ${MPI_LIB} -lstdc++ -L. \
		  $(F90LIB_PATHS) -lxlf90_r

else
# if you have the ibm xlc/xlC product, setenv ESMF_C_COMPILER xlc first
C_CC		= xlc_r 
C_CCV		= which $(C_CC)
C_CXX		= xlC_r 
C_CXXV		= which $(C_CXX)
CXXLIB_PATHS    = -L/opt/ibmcmp/vacpp/6.0/lib/
F90LIB_PATHS    = -L/opt/ibmcmp/xlf/8.1/lib
C_CXXF90LIBS	= -L. $(F90LIB_PATHS) -lxlf90_r -lxlfmath -lxl \
		      $(CXXLIB_PATHS) -lm -libmc++ -lstdc++
C_F90CXXLIBS	= -L. $(F90LIB_PATHS) -lxlf90_r \
		      $(CXXLIB_PATHS) -libmc++ -lstdc++

# end of xlc section
endif


#
# xlf Fortran compiler from ibm for mac os x
#
C_FC		= xlf95_r 
C_FCV		= which $(C_FC)

# end of non-mpich section
endif

# settings independent of which compiler is selected

C_SLFLAG                  = -L

###########################################

PARCH			= mac_osx

SL_LIBS_TO_MAKE =

C_SL_LIBOPTS  = -G -qmkshrobj $(C_F90CXXLIBS) $(MPI_LIB)


