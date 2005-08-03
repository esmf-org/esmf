# $Id: build_rules.mk,v 1.4 2005/08/03 22:14:23 jwolfe Exp $
#
#  Linux.g95.default
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


C_F90CXXLIBS    = -lrt -lc -lstdc++
C_CXXF90LIBS    = -lrt -lf95 -lstdc++


PARCH	        = linux_g95

SL_LIBS_TO_MAKE = 


