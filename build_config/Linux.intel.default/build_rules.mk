# $Id$
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
ESMF_MPIRUNDEFAULT      = mpiexec_mpt $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec_mpt $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),mpich)
# Mpich ----------------------------------------------------
ESMF_F90COMPILECPPFLAGS+= -DESMF_MPICH
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_MPICH
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpiCC
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),mpich2)
# Mpich2 ---------------------------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpicxx
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),mvapich2)
# Mvapich2 ---------------------------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpicxx
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),intelmpi)
# IntelMPI -------------------------------------------------
ESMF_F90DEFAULT         = mpiifort
ESMF_CXXDEFAULT         = mpiicpc
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),scalimpi)
# scaliMPI -------------------------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpic++
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),lam)
# LAM (assumed to be built with ifort) ---------------------
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_NO_SIGUSR2
ESMF_F90DEFAULT         = mpif77
ESMF_CXXDEFAULT         = mpic++
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),openmpi)
# OpenMPI --------------------------------------------------
ifeq ($(shell $(ESMF_DIR)/scripts/available mpifort),mpifort)
ESMF_F90DEFAULT         = mpifort
ESMF_CXXLINKLIBS       += -lmpi_mpifh
else
ESMF_F90DEFAULT         = mpif90
ESMF_CXXLINKLIBS       += -lmpi_f77
endif
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_NO_SIGUSR2
ESMF_F90LINKLIBS       += -lmpi_cxx
ESMF_CXXDEFAULT         = mpicxx
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
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
ifeq ($(ESMF_ABI),64)
ESMF_ABISTRING := $(ESMF_MACHINE)_64
else
$(error Invalid ESMF_MACHINE / ESMF_ABI combination: $(ESMF_MACHINE) / $(ESMF_ABI))
endif
endif
ifeq ($(ESMF_MACHINE),x86_64)
ifeq ($(ESMF_ABI),32)
ESMF_ABISTRING := $(ESMF_MACHINE)_32
endif
ifeq ($(ESMF_ABI),64)
ESMF_ABISTRING := x86_64_small
endif
ifeq ($(ESMF_ABI),mic)
ESMF_ABISTRING := x86_64_mic
endif
endif

############################################################
# Set memory model compiler flags according to ABISTRING
#
ifeq ($(ESMF_ABISTRING),x86_64_32)
ESMF_CXXCOMPILEOPTS       += -m32
ESMF_CXXLINKOPTS          += -m32
ESMF_F90COMPILEOPTS       += -m32
ESMF_F90LINKOPTS          += -m32
endif
ifeq ($(ESMF_ABISTRING),x86_64_small)
ESMF_CXXCOMPILEOPTS       += -m64 -mcmodel=small
ESMF_CXXLINKOPTS          += -m64 -mcmodel=small
ESMF_F90COMPILEOPTS       += -m64 -mcmodel=small
ESMF_F90LINKOPTS          += -m64 -mcmodel=small
endif
ifeq ($(ESMF_ABISTRING),x86_64_medium)
ESMF_CXXCOMPILEOPTS       += -m64 -mcmodel=medium
ESMF_CXXLINKOPTS          += -m64 -mcmodel=medium
ESMF_F90COMPILEOPTS       += -m64 -mcmodel=medium
ESMF_F90LINKOPTS          += -m64 -mcmodel=medium
endif
ifeq ($(ESMF_ABISTRING),x86_64_mic)
ESMF_CXXCOMPILEOPTS       += -mmic
ESMF_CXXLINKOPTS          += -mmic
ESMF_F90COMPILEOPTS       += -mmic -auto
ESMF_F90LINKOPTS          += -mmic -auto
ESMF_SL_LIBOPTS           += -mmic
ESMF_F90COMPILECPPFLAGS+= -DESMF_NO_SEQUENCE
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_NO_SEQUENCE
endif
ifeq ($(ESMF_ABISTRING),ia64_64)
ESMF_CXXCOMPILEOPTS       += -size_lp64
ESMF_CXXLINKOPTS          += -size_lp64
ESMF_F90COMPILEOPTS       += -size_lp64
ESMF_F90LINKOPTS          += -size_lp64
endif

############################################################
# Conditionally add pthread compiler and linker flags
#
ifeq ($(ESMF_PTHREADS),ON)
ESMF_F90COMPILEOPTS += -threads
ESMF_CXXCOMPILEOPTS += -pthread
ESMF_F90LINKOPTS    += -threads
ESMF_CXXLINKOPTS    += -pthread
ESMF_SL_LIBOPTS     += -pthread
endif

############################################################
# OpenMP compiler and linker flags
#
ESMF_OPENMP_F90COMPILEOPTS += -openmp
ESMF_OPENMP_CXXCOMPILEOPTS += -openmp
ESMF_OPENMP_F90LINKOPTS    += -openmp
ESMF_OPENMP_CXXLINKOPTS    += -openmp

############################################################
# Set rpath syntax
#
ESMF_F90RPATHPREFIX         = -Wl,-rpath,
ESMF_CXXRPATHPREFIX         = -Wl,-rpath,

############################################################
# Determine where ifort's libraries are located
#
ESMF_CXXLINKPATHS += -L$(dir $(shell $(ESMF_DIR)/scripts/libpath.ifort $(ESMF_F90COMPILER)))
ESMF_CXXLINKRPATHS += \
  $(ESMF_CXXRPATHPREFIX)$(dir $(shell $(ESMF_DIR)/scripts/libpath.ifort $(ESMF_F90COMPILER)))

############################################################
# Determine where icpc's libraries are located
#
ESMF_F90LINKPATHS += $(addprefix -L,$(shell $(ESMF_DIR)/scripts/libpath.icpc "$(ESMF_CXXCOMPILER) $(ESMF_CXXCOMPILEOPTS)"))

############################################################
# Link against libesmf.a using the F90 linker front-end
#
ESMF_F90LINKLIBS += $(shell $(ESMF_DIR)/scripts/libs.icpc "$(ESMF_CXXCOMPILER) $(ESMF_CXXCOMPILEOPTS)") -lrt -ldl

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ESMF_CXXLINKLIBS += $(shell $(ESMF_DIR)/scripts/libs.ifort "$(ESMF_F90COMPILER) $(ESMF_F90COMPILEOPTS)") -lrt -ldl

############################################################
# Linker option that ensures that the specified libraries are 
# used to also resolve symbols needed by other libraries.
#
ESMF_F90LINKOPTS          += -Wl,--no-as-needed
ESMF_CXXLINKOPTS          += -Wl,--no-as-needed

############################################################
# Shared library options
#
ESMF_SL_LIBOPTS  += -shared

############################################################
# Shared object options
#
ESMF_SO_F90COMPILEOPTS  = -fPIC
ESMF_SO_F90LINKOPTS     = -shared
ESMF_SO_F90LINKOPTSEXE  = -Wl,-export-dynamic
ESMF_SO_CXXCOMPILEOPTS  = -fPIC
ESMF_SO_CXXLINKOPTS     = -shared
ESMF_SO_CXXLINKOPTSEXE  = -Wl,-export-dynamic

############################################################
# 3rd party code dependency: PIO
#
#TODO: activate this once PIO support is stable: ESMF_PIODEFAULT = internal
