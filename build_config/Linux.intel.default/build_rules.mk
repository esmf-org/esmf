# $Id$
#
# Linux.intel.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = ifort
ESMF_CXXDEFAULT         = icpc
ESMF_CDEFAULT           = icc

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
ESMF_CPPFLAGS          += -DESMF_MPIUNI -I$(ESMF_DIR)/src/Infrastructure/stubs/mpiuni
ESMF_MPIRUNDEFAULT      = $(ESMF_DIR)/src/Infrastructure/stubs/mpiuni/mpirun
else
ifeq ($(ESMF_COMM),mpi)
# Vendor MPI -----------------------------------------------
ESMF_F90LINKLIBS       += -lmpi -lmpi++
ESMF_CXXLINKLIBS       += -lmpi -lmpi++
ESMF_MPIRUNDEFAULT      = mpiexec_mpt $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec_mpt $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),mpt)
# MPT with compiler wrappers -------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_F90LINKLIBS       += -lmpi++
ESMF_CXXDEFAULT         = mpicxx
ESMF_CDEFAULT           = mpicc
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPILAUNCHOPTIONS   = -p %g: 
else
ifeq ($(ESMF_COMM),mpich1)
# Mpich1 ---------------------------------------------------
ESMF_F90COMPILECPPFLAGS+= -DESMF_MPICH1
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_MPICH1
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpiCC
ESMF_CDEFAULT           = mpicc
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),mpich2)
# Mpich2 ---------------------------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpicxx
ESMF_CDEFAULT           = mpicc
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),mpich)
# Mpich3 and up --------------------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpicxx
ESMF_CDEFAULT           = mpicc
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),mvapich)
# Mvapich any version --------------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpicxx
ESMF_CDEFAULT           = mpicc
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),intelmpi)
# IntelMPI -------------------------------------------------
ESMF_F90DEFAULT         = mpiifort
ESMF_CXXDEFAULT         = mpiicpc
ESMF_CDEFAULT           = mpiicc
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),scalimpi)
# scaliMPI -------------------------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpic++
ESMF_CDEFAULT           = mpicc
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),lam)
# LAM (assumed to be built with ifort) ---------------------
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_NO_SIGUSR2
ESMF_F90DEFAULT         = mpif77
ESMF_CXXDEFAULT         = mpic++
ESMF_CDEFAULT           = mpicc
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),openmpi)
# OpenMPI --------------------------------------------------
ifeq ($(shell $(ESMF_DIR)/scripts/available mpifort),mpifort)
ESMF_F90DEFAULT         = mpifort
else
ESMF_F90DEFAULT         = mpif90
endif
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_NO_SIGUSR2
ESMF_F90LINKLIBS       += $(shell $(ESMF_DIR)/scripts/libs.openmpif90 $(ESMF_F90DEFAULT))
ESMF_CXXDEFAULT         = mpicxx
ESMF_CDEFAULT           = mpicc
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
endif
endif

############################################################
# Print compiler version string
#
ESMF_F90COMPILER_VERSION    = ${ESMF_F90COMPILER} -V -v -c
ESMF_CXXCOMPILER_VERSION    = ${ESMF_CXXCOMPILER} -V -v -c
ESMF_CCOMPILER_VERSION      = ${ESMF_CCOMPILER} -V -v -c
ESMF_F90MAJORVERSION      = $(shell $(ESMF_DIR)/scripts/version.intel 1 ${ESMF_F90COMPILER} -V)
ESMF_CXXMAJORVERSION      = $(shell $(ESMF_DIR)/scripts/version.intel 1 ${ESMF_CXXCOMPILER} -V)

############################################################
# Special debug flags
#
ESMF_F90OPTFLAG_G       += -traceback -check arg_temp_created,bounds,format,output_conversion,stack
ESMF_CXXOPTFLAG_G       += -traceback -Wcheck

############################################################
# Enable TR15581/F2003 Allocatable array resizing
#
ESMF_F90COMPILEOPTS += -assume realloc_lhs

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
ESMF_F90COMPILEOPTS += -pthread -threads
ESMF_CXXCOMPILEOPTS += -pthread
ESMF_F90LINKOPTS    += -pthread -threads
ESMF_CXXLINKOPTS    += -pthread
ESMF_SL_LIBOPTS     += -pthread
endif

############################################################
# OpenMP compiler and linker flags
#
ifeq ($(shell [ $(ESMF_F90MAJORVERSION) -ge 16 ] && echo true), true)
ESMF_OPENMP_F90COMPILEOPTS += -qopenmp
ESMF_OPENMP_F90LINKOPTS    += -qopenmp
else
ESMF_OPENMP_F90COMPILEOPTS += -openmp
ESMF_OPENMP_F90LINKOPTS    += -openmp
endif
ifeq ($(shell [ $(ESMF_CXXMAJORVERSION) -ge 16 ] && echo true), true)
ESMF_OPENMP_CXXCOMPILEOPTS += -qopenmp
ESMF_OPENMP_CXXLINKOPTS    += -qopenmp
else
ESMF_OPENMP_CXXCOMPILEOPTS += -openmp
ESMF_OPENMP_CXXLINKOPTS    += -openmp
endif

############################################################
# MKL specific options for external LAPACK
#
ifeq ($(ESMF_LAPACK),mkl)
ifndef ESMF_LAPACK_LIBS
ESMF_LAPACK_LIBS = -mkl
endif
endif

############################################################
# Set rpath syntax
#
ESMF_F90RPATHPREFIX         = -Wl,-rpath,
ESMF_CXXRPATHPREFIX         = -Wl,-rpath,
ESMF_CRPATHPREFIX           = -Wl,-rpath,

############################################################
# Determine where ifort's libraries are located
#
ESMF_CXXLINKPATHS += -L$(dir $(shell $(ESMF_DIR)/scripts/libpath.ifort $(ESMF_F90COMPILER)))
ESMF_CXXLINKRPATHS += \
  $(ESMF_CXXRPATHPREFIX)$(dir $(shell $(ESMF_DIR)/scripts/libpath.ifort $(ESMF_F90COMPILER)))

############################################################
# Determine where icpc's libraries are located
#
ESMF_F90LINKPATHS +=

############################################################
# Link against libesmf.a using the F90 linker front-end
#
ESMF_F90LINKOPTS += -cxxlib
ESMF_F90LINKLIBS += -lrt -ldl

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
ESMF_SO_F90COMPILEOPTS  = -fPIC -debug minimal
ESMF_SO_F90LINKOPTS     = -shared
ESMF_SO_F90LINKOPTSEXE  = -Wl,-export-dynamic
ESMF_SO_CXXCOMPILEOPTS  = -fPIC -debug minimal
ESMF_SO_CXXLINKOPTS     = -shared
ESMF_SO_CXXLINKOPTSEXE  = -Wl,-export-dynamic
