# $Id: build_rules.mk,v 1.2 2006/06/07 20:51:50 theurich Exp $
#
#  Linux.gfortran.default
#
#
# Default MPI setting.
#
ifeq ($(ESMF_COMM),default)
export ESMF_COMM := mpiuni
endif

# lam has not been tested.
ifeq ($(ESMF_COMM),lam)
# with lam-mpi installed:
MPI_LIB        += -lmpi -llam
endif

ifeq ($(ESMF_COMM),mpich)
# with mpich installed:
MPI_LIB        += -lpmpich++ -lmpich
MPI_INCLUDE    += -DESMF_MPICH
MPIRUN         += $(ESMF_NODES)
endif

ifeq ($(ESMF_COMM),mpich2)
# with mpich installed:
MPI_LIB        +=
MPI_INCLUDE    +=
MPIRUN         += $(ESMF_NODES)
MPIMPMDRUN      = mpiexec
endif

ifeq ($(ESMF_COMM),openmpi)
# with mpich installed:
MPI_LIB        += -lmpi_cxx
MPI_INCLUDE    +=
MPIRUN         += $(ESMF_NODES)
MPIMPMDRUN      = mpiexec
endif

THREAD_LIB     =

# straight compilers front-ends
ifneq ($(ESMF_COMM),mpich)
C_CC		= gcc 
C_CXX		= g++
C_FC		= gfortran
endif

# mpich compiler front-end wrappers
ifeq ($(ESMF_COMM),mpich)
C_CC    	= mpicc
C_CXX   	= mpiCC
C_FC    	= mpif90
endif

# mpich2 compiler front-end wrappers
ifeq ($(ESMF_COMM),mpich2)
C_CC    	= mpicc
# MPICH_IGNORE_CXX_SEEK is workaround for MPI-2 bug (see MPICH2 docs)
C_CXX   	= mpicxx -DMPICH_IGNORE_CXX_SEEK
C_FC    	= mpif90
endif

# OpenMPI compiler front-end wrappers
ifeq ($(ESMF_COMM),openmpi)
C_CC    	= mpicc
C_CXX   	= mpicxx
C_FC    	= mpif90
endif

# how to get version string out of compiler front-ends
C_CCV		= ${C_CC} -dumpversion
C_CXXV		= ${C_CXX} -dumpversion
C_FCV           = ${C_FC} -dumpversion

# this must match the Fortran symbol convention of any other libraries used
FFLAGS          = -fno-second-underscore

# not tested 64-bit option
ifeq ($(ESMF_PREC),64)
CFLAGS         += -march=k8 -m64 -mcmodel=medium
FFLAGS         += -march=k8 -m64 -mcmodel=medium
endif

# options for how to process four different flavors of Fortran source
F_FREECPP       = -ffree-form
F_FIXCPP        = -ffixed-form
F_FREENOCPP     = -ffree-form
F_FIXNOCPP      = -ffixed-form

# use the GCC -print-file-name option to determine location of stdc++ and 
# Fortran libraries and thus define the C_LIB_PATHS
C_LIB_PATHS     := -L$(dir $(shell $(C_CXX) -print-file-name=libstdc++.so)) \
  -L$(dir $(shell $(C_FC) -print-file-name=libgfortran.so))

# flags required to link against libesmf.a using the F90 linker front-end
C_F90CXXLIBS    = -lrt -lstdc++
# flags required to link against libesmf.a using the C++ linker front-end
C_CXXF90LIBS    = -lrt -lgfortran


PARCH	        = linux_gfortran

SL_LIBS_TO_MAKE = 
