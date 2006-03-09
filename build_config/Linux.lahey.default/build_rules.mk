# $Id: build_rules.mk,v 1.20 2006/03/09 18:28:49 nscollins Exp $
#
# Linux.lahey.default
#
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
# copy this file into Linux.lahey.my_site, and uncomment the
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
# with lam-mpi installed in MPI_HOME or /usr/local:
MPI_LIB        += -lmpi -llam
endif

ifeq ($(ESMF_COMM),mpich)
# with mpich installed in MPI_HOME or /usr/local:
MPI_INCLUDE    += -DESMF_MPICH=1
MPI_LIB        += -lmpich
MPIRUN         +=  $(ESMF_NODES)
endif

# name of the lib which includes the posix thread support.
THREAD_LIB     = -lpthread


############################################################

#
# compilers and flags
#
C_CC		   = gcc
C_CXX		   = g++ -fPIC
C_FC		   = lf95

C_CCV		   = ${C_CC} --version
C_CXXV		   = ${C_CXX} --version
C_FCV              = lf95 --version

#
# Fortran compiler flags
#
F_FREECPP          = --nfix -Cpp
F_FIXCPP           = --fix -Cpp
F_FREENOCPP        = --nfix 
F_FIXNOCPP         = --fix

# use dir values in LD_LIBRARY_PATH, others are defaults if no other choice.
ifeq ($(origin ESMF_CXX_LIBRARY_PATH), environment)
PATHSET=true
endif

ifeq ($(origin ESMF_F90_LIBRARY_PATH), environment)
PATHSET=true
endif

ifeq ($(origin LD_LIBRARY_PATH), environment)
C_LIB_PATHS  += $(ENV_LIB_PATHS)
C_LD_PATHS   += $(ENV_LD_PATHS)
PATHSET=true
endif

ifeq ($PATHSET,)
# the user has given us no input, use hardcoded values which are 
# just a best-guess.
C_LIB_PATHS  += /usr/lib/gcc-lib/i386-redhat-linux/3.2.2 \
                /usr/local/lf9560/lib 

C_LD_PATHS   += /usr/lib/gcc-lib/i386-redhat-linux/3.2.2 \
                /usr/local/lf9560/lib 
endif

# by now there are vlaues for lib and ld paths; either from the
# environment or from the lines above.  add them plus the libs to
# the variable which gets used as a link line.
C_F90CXXLIBS       = $(LIB_PATHS) $(LD_PATHS) \
                     -Wl,-rpath $(ESMF_LIBDIR) \
                     -lstdc++ -lgcc -lg2c -lrt

C_CXXF90LIBS       = $(LIB_PATHS) $(LD_PATHS) \
                     -Wl,-rpath $(ESMF_LIBDIR) \
                     -lfj9i6 -lfj9f6 -lfj9e6 -lfccx86_6a -lrt


##############################################################################

PARCH		   = linux_lf95

# this platform makes a shared lib
C_SL_LIBOPTS       = -shared


