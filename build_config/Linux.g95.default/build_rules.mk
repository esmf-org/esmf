# $Id: build_rules.mk,v 1.2 2005/04/22 22:24:37 nscollins Exp $
#
#  Linux.g95.default
#  T. Wainwright, April 2005, based on Linux.pgi.default
#  updated by nancy collins, after massive makefile updates
#


# Make sure ESMF_PREC is set to 32, until g95 supports 64 bit platforms.
ESMF_PREC = 32

# these have not been tested.
ifeq ($(ESMF_COMM),lam)
# with lam-mpi installed:
MPI_LIB        += -lmpi -llam
THREAD_LIB     = -lpthread
endif

ifeq ($(ESMF_COMM),mpich)
# with mpich installed:
MPI_LIB        += -lmpich
MPI_INCLUDE    += -DESMF_MPICH
MPIRUN         += $(ESMF_NODES)
endif


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
C_CXX   	= mpiCC
C_FC    	= mpif90
endif

C_CCV		= ${C_CC} -dumpversion
C_CXXV		= ${C_CXX} -dumpversion
C_FCV           = ${C_FC} -dumpversion

FFLAGS          = -fno-second-underscore
F_FREECPP       = -cpp -ffree-form
F_FIXCPP        = -cpp -ffixed-form
F_FREENOCPP     = -ffree-form
F_FIXNOCPP      = -ffixed-form


C_F90CXXLIBS    = -lrt -lc -lstdc++
C_CXXF90LIBS    = -lrt -lf95



PARCH		   = linux_g95

SL_LIBS_TO_MAKE = 


