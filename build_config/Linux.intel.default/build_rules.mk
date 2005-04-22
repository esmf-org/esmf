# $Id: build_rules.mk,v 1.37 2005/04/22 20:14:25 nscollins Exp $
#
# Linux.intel.default
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

#
############################################################

#
# Location of MPI (Message Passing Interface) software
#

# This section is set up for LAM mpi.   If the lam include files, libs, and
# mpirun executable are installed in the dirs normally searched by the
# compiler/loader you do not have to set anything.  If installed in a 
# nonstandard place, set MPI_HOME to the location where the include, lib, 
# and bin subdirs will be found.   plus, lam required pthreads here.
ifeq ($(ESMF_COMM),lam)
MPI_LIB        += -llamf77mpi -lmpi -llam 
# lam requires pthreads
THREAD_LIB     = -lpthread
endif

# This section is set up for vendor supplied MPI (e.g. SGI Altix).
# It is assumed to be in a system-standard location, but can be overridden
# by setting MPI_HOME to another location.
ifeq ($(ESMF_COMM),mpi)
MPI_LIB   +=  -lmpi -lmpi++
endif

ifeq ($(ESMF_COMM),mpi_pbs)
MPI_LIB    += -lmpi -lmpi++
MPIRUN      = $(ESMF_DIR)/scripts/mpirun.pbs
CFLAGS     += -DESMC_HAVE_INT_MPI_COMM
endif


# This section is set up for mpich mpi.   If the mpich include files, libs, and
# mpirun executable are installed in the dirs normally searched by the
# compiler/loader you do not have to set anything.  If installed in a 
# nonstandard place, set MPI_HOME to the location where the include, lib, 
# and bin subdirs will be found.   ESMF_NODES can be set if you must specify 
# which nodes are to be used on the run command.
ifeq ($(ESMF_COMM),mpich)
MPI_INCLUDE    += -DESMF_MPICH=1
MPI_LIB        += -lmpich
MPIRUN         +=  $(ESMF_NODES)
endif

# This section is set up for mpich2 mpi.  These settings are currently the same
# as the mpich section above, but if different flags or settings are needed
# as we look at installing and supporting the newer mpich2 (with support for
# mpi2 functions), this place is ready.  All comments from the mpich section 
# apply to this one as well.
ifeq ($(ESMF_COMM),mpich2)
MPI_INCLUDE    += -DESMF_MPICH=1
MPI_LIB        += -lmpich
MPIRUN         += $(ESMF_NODES)
endif


#
# ################## Compilers, Linkers, and Loaders ########################
#

# the default is to use the intel C and C++ compilers.
# if you want gcc and g++, set ESMF_C_COMPILER to gcc before building.

ifneq ($(ESMF_COMM),mpich)
ifeq ($(ESMF_C_COMPILER),gcc)
C_CC	   = gcc
C_CXX      = g++
C_FC	   = ifort
else
C_CC	   = icc
C_CXX      = icpc
C_FC	   = ifort
endif
endif

# if you are using mpich, then however the mpich wrappers have been built
# will determine which compilers you are using.
ifeq ($(ESMF_COMM),mpich)
C_CC	   = mpicc
C_CXX	   = mpiCC
C_FC	   = mpif90
endif

ifeq ($(ESMF_PREC),64)
CFLAGS	   += -size_lp64
FFLAGS	   += -size_lp64
endif

# the default is to link with the intel C and C++ libraries unless you have
# already set ESMF_C_COMPILER to gcc.  if you want to still compile with intel
# but link with the gcc libs anyway, set ESMF_C_LIBRARY to gcc before building.
ifeq ($(ESMF_C_COMPILER),gcc)
INTEL_C_LIB_FLAG = 
INTEL_C_LIB_NEEDED = -lstdc++
else
ifeq ($(ESMF_C_LIBRARY),gcc)
INTEL_C_LIB_FLAG = -cxxlib-gcc
INTEL_C_LIB_NEEDED = -lstdc++
else
INTEL_C_LIB_FLAG = -cxxlib-icc
INTEL_C_LIB_NEEDED = -lcprts
endif
endif

# add standard flags
CFLAGS  +=  $(INTEL_C_LIB_FLAG)
FFLAGS  +=

# conditionally add pthread compiler flags
ifeq ($(ESMF_PTHREADS),ON)
CFLAGS  +=  -pthread
FFLAGS  +=  -threads
endif

# how to print versions
C_CCV		   = ${C_CC} -V -v
C_CXXV		   = ${C_CXX} -V -v
C_FCV              = ${C_FC} -V -v

# fortran flags
F_FREECPP          = -fpp -FR
F_FIXCPP           = -fpp 
F_FREENOCPP        = -fpp0 -FR
F_FIXNOCPP         = -fpp0


# by default append each directory which is in LD_LIBRARY_PATH to
# the -L flag and also to the run-time load flag.  (on systems which
# support the 'module' command, that is how it works - by adding dirs
# to LD_LIBRARY_PATH.)  if it is not set, default to where the intel
# compilers try to install themselves.  if your compiler is someplace else
# either set LD_LIBRARY_PATH first, or make a site specific file and
# edit the paths explicitly.

ifneq ($(origin LD_LIBRARY_PATH), environment)
# if env var not set, try this because they are the intel default locations.
ifeq ($(ESMF_COMPILER_VERSION),80)
C_LIB_PATHS       = -L/opt/intel_cc_80/lib
C_LD_PATHS        = $(C_SLFLAG)/opt/intel_cc_80/lib
else
C_LIB_PATHS       = -L/opt/intel_cc_81/lib
C_LD_PATHS        = $(C_SLFLAG)/opt/intel_cc_81/lib
endif
else
# add the values from the environment
C_LIB_PATHS  += $(ENV_LIB_PATHS)
C_LD_PATHS   += $(ENV_LD_PATHS)
endif


C_F90CXXLIBS    = $(INTEL_C_LIB_NEEDED) -lrt -ldl
C_CXXF90LIBS    = $(INTEL_C_LIB_NEEDED) -lifcoremt -lunwind -lrt -ldl

###############################################################################

PARCH		   = linux_intel

SL_LIBS_TO_MAKE = 
C_SL_LIBOPTS  =
