# $Id: build_rules.mk,v 1.1 2006/03/03 16:15:56 nscollins Exp $
#
#  Darwin.g95.default
#  T. Wainwright, April 2005, based on Linux.pgi.default
#  updated by nancy collins, after massive makefile updates
#

#
# Default MPI setting.
#
ifeq ($(ESMF_COMM),default)
export ESMF_COMM := mpiuni
endif

# these have not been tested.
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

THREAD_LIB     = -lpthread

#
# compilers or mpich wrappers
#
ifneq ($(ESMF_COMM),mpich)
C_CC		= gcc 
C_CXX		= g++
C_FC		= g95
endif

ifeq ($(ESMF_COMM),mpich)
C_CC    	= mpicc
# MPICH_IGNORE_CXX_SEEK is workaround for MPI-2 bug (see MPICH2 docs)
C_CXX   	= mpiCC -DMPICH_IGNORE_CXX_SEEK
C_FC    	= mpif90
endif

C_CCV		= ${C_CC} -dumpversion
C_CXXV		= ${C_CXX} -dumpversion
C_FCV           = ${C_FC} -dumpversion

FFLAGS          = -fno-second-underscore
ifeq ($(ESMF_PREC),64)
CFLAGS         += -march=k8 -m64 -mcmodel=medium
FFLAGS         += -march=k8 -m64 -mcmodel=medium
endif
F_FREECPP       = -cpp -ffree-form
F_FIXCPP        = -cpp -ffixed-form
F_FREENOCPP     = -ffree-form
F_FIXNOCPP      = -ffixed-form

# by default append each directory which is in LD_LIBRARY_PATH to
# the -L flag and also to the run-time load flag.  (on systems which
# support the 'module' command, that is how it works - by adding dirs
# to LD_LIBRARY_PATH.)  if it is not set, default to one of the many
# possible places the gcc compilers try to install themselves.  
# if your compiler installs these libs someplace else
# either set LD_LIBRARY_PATH first, or make a site specific file and
# edit the paths explicitly.
ifneq ($(origin LD_LIBRARY_PATH), environment)
CXXLIB_PATHS   = -L/usr/lib
F90LIB_PATHS   = -L/usr/lib
C_LIB_PATHS      = ${CXXLIB_PATHS} ${F90LIB_PATHS}
C_LD_PATHS       = ${CXXLIB_PATHS} ${F90LIB_PATHS}
else
C_LIB_PATHS      = $(ENV_LIB_PATHS)
C_LD_PATHS       = $(ENV_LD_PATHS)
endif


C_F90CXXLIBS    = -lrt -lc -lstdc++
C_CXXF90LIBS    = -lrt -lf95 -lstdc++


PARCH	        = mac_osx

SL_LIBS_TO_MAKE = 


