# $Id: build_rules.mk,v 1.54 2007/01/17 19:22:12 theurich Exp $
#
# Linux.intel.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = ifort
ESMF_CXXDEFAULT         = icpc

############################################################
# Default MPI setting.
#
ifeq ($(ESMF_COMM),default)
export ESMF_COMM := mpiuni
endif

############################################################
# MPI dependent settings.
#
ifeq ($(ESMF_COMM),mpiuni)
# MPI stub library -----------------------------------------
ESMF_F90COMPILECPPFLAGS+= -DESMF_MPIUNI
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_MPIUNI
ESMF_CXXCOMPILEPATHS   += -I$(ESMF_DIR)/src/Infrastructure/stubs/mpiuni
ESMF_MPIRUNDEFAULT      = $(ESMF_DIR)/src/Infrastructure/stubs/mpiuni/mpirun
else
ifeq ($(ESMF_COMM),mpi)
# Vendor MPI -----------------------------------------------
ESMF_F90LINKLIBS       += -lmpi -lmpi++
ESMF_CXXLINKLIBS       += -lmpi -lmpi++
ESMF_MPIRUNDEFAULT      = mpirun
ESMF_MPIMPMDRUNDEFAULT  = mpirun
else
ifeq ($(ESMF_COMM),mpich)
# Mpich ----------------------------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpiCC
ESMF_CXXCOMPILEOPTS    += -DESMF_MPICH
ESMF_MPIRUNDEFAULT      = mpirun
else
ifeq ($(ESMF_COMM),mpich2)
# Mpich2 ---------------------------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpicxx
ESMF_MPIRUNDEFAULT      = mpirun
ESMF_MPIMPMDRUNDEFAULT  = mpiexec
else
ifeq ($(ESMF_COMM),intelmpi)
# IntelMPI -------------------------------------------------
ESMF_F90DEFAULT         = mpiifort
ESMF_CXXDEFAULT         = mpiicpc
ESMF_MPIRUNDEFAULT      = mpirun
ESMF_MPIMPMDRUNDEFAULT  = mpiexec
else
ifeq ($(ESMF_COMM),lam)
# LAM (assumed to be built with ifort) ---------------------
ESMF_F90DEFAULT         = mpif77
ESMF_CXXDEFAULT         = mpic++
ESMF_MPIRUNDEFAULT      = mpirun
ESMF_MPIMPMDRUNDEFAULT  = mpiexec
else
ifeq ($(ESMF_COMM),openmpi)
# OpenMPI --------------------------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpicxx
ESMF_MPIRUNDEFAULT      = mpirun
ESMF_MPIMPMDRUNDEFAULT  = mpiexec
else
ifeq ($(ESMF_COMM),user)
# User specified flags -------------------------------------
else
$(error Invalid ESMF_COMM setting: $(ESMF_COMM))
endif
endif
endif
endif
endif
endif
endif
endif

############################################################
# Print compiler version string
#
ESMF_F90COMPILER_VERSION    = ${ESMF_F90COMPILER} -V -v
ESMF_CXXCOMPILER_VERSION    = ${ESMF_CXXCOMPILER} -V -v

############################################################
# Construct the ABISTRING
#
ifeq ($(ESMF_MACHINE),ia64)
ifeq ($(ESMF_ABI),32)
ESMF_ABISTRING := $(ESMF_MACHINE)_32
endif
ifeq ($(ESMF_ABI),64)
ESMF_ABISTRING := $(ESMF_MACHINE)_64
endif
endif
ifeq ($(ESMF_MACHINE),x86_64)
ifeq ($(ESMF_ABI),32)
ESMF_ABISTRING := $(ESMF_MACHINE)_32
endif
ifeq ($(ESMF_ABI),64)
ESMF_ABISTRING := x86_64_small
endif
endif

############################################################
# Set memory model compiler flags according to ABISTRING
#
ifeq ($(ESMF_ABISTRING),x86_64_medium)
ESMF_F90COMPILEOPTS     += -mcmodel=medium
ESMF_F90LINKOPTS        += -mcmodel=medium
ESMF_CXXCOMPILEOPTS     += -mcmodel=medium
ESMF_CXXLINKOPTS        += -mcmodel=medium
endif

############################################################
# On IA64 set long and pointer types to 64-bit
#
ifeq ($(ESMF_ABISTRING),ia64_64)
ESMF_CXXCOMPILEOPTS       += -size_lp64
ESMF_CXXLINKOPTS          += -size_lp64
ESMF_F90COMPILEOPTS       += -size_lp64
ESMF_F90LINKOPTS          += -size_lp64
endif

############################################################
# To compile with Intel's icpc but link with GCC's stdc++ lib
# set ESMF_STDCXX_LIBRARY to gcc before building
#
ifeq ($(ESMF_STDCXX_LIBRARY),gcc)
ESMF_F90LINKPATHS   += -L$(dir $(shell gcc -print-file-name=libstdc++.so))
ESMF_F90LINKLIBS    += -lstdc++
ESMF_CXXCOMPILEOPTS += -cxxlib-gcc
ESMF_CXXLINKOPTS    += -cxxlib-gcc
else
ESMF_F90LINKLIBS    += -lcprts
ESMF_CXXCOMPILEOPTS += -cxxlib-icc
ESMF_CXXLINKOPTS    += -cxxlib-icc
endif

############################################################
# Conditionally add pthread compiler and linker flags
#
ifeq ($(ESMF_PTHREADS),ON)
ESMF_F90COMPILEOPTS +=  -threads
ESMF_CXXCOMPILEOPTS +=  -pthread
ESMF_F90LINKOPTS    += -threads
ESMF_CXXLINKOPTS    += -pthread
endif

############################################################
# Need this until the file convention is fixed (then remove these two lines)
#
ESMF_F90COMPILEFREENOCPP = -fpp0 -FR
ESMF_F90COMPILEFIXCPP    = -fpp

############################################################
# Determine where ifort's libraries are located
#
ESMF_CXXLINKPATHS += -L$(dir $(shell $(ESMF_DIR)/scripts/libpath.ifort $(ESMF_F90COMPILER)))
ESMF_CXXLINKRPATHS += \
  $(ESMF_RPATHPREFIX)$(dir $(shell $(ESMF_DIR)/scripts/libpath.ifort $(ESMF_F90COMPILER)))

############################################################
# Link against libesmf.a using the F90 linker front-end
#
ESMF_F90LINKLIBS += -limf -lm -lcxa -lunwind -lrt -ldl

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ESMF_CXXLINKLIBS += -lifcoremt -lunwind -lrt -ldl

############################################################
# Blank out shared library options
#
ESMF_SL_LIBS_TO_MAKE  =
