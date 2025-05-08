# $Id$
#
# Unicos.nvhpc.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = ftn
ESMF_CXXDEFAULT         = CC
ESMF_CDEFAULT           = cc

############################################################
# Do not use an explicit std switch for C99
#
ESMF_CSTD               = sysdefault
ESMF_CSTDFLAG           =

############################################################
# Default MPI setting.
#
ifeq ($(ESMF_COMM),default)
export ESMF_COMM := mpi
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
ESMF_F90LINKLIBS       += 
ESMF_CXXLINKLIBS       += 
ESMF_MPIRUNDEFAULT      = mpirun.srun
ESMF_MPIMPMDRUNDEFAULT  =
else
ifeq ($(ESMF_COMM),user)
# User specified flags -------------------------------------
else
$(error Invalid ESMF_COMM setting: $(ESMF_COMM))
endif
endif
endif

############################################################
# Print compiler version string
#
ESMF_F90COMPILER_VERSION    = ${ESMF_F90COMPILER} --version -c
ESMF_CXXCOMPILER_VERSION    = ${ESMF_CXXCOMPILER} --version -c
ESMF_CCOMPILER_VERSION      = ${ESMF_CCOMPILER} --version -c

############################################################
# Currently no support the Fortran2018 assumed type feature
#
ESMF_F90COMPILECPPFLAGS += -DESMF_NO_F2018ASSUMEDTYPE
ESMF_CXXCOMPILECPPFLAGS += -DESMF_NO_F2018ASSUMEDTYPE

############################################################
# Construct the ABISTRING
#
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
ifeq ($(ESMF_ABISTRING),x86_64_32)
ESMF_CXXCOMPILEOPTS       += 
ESMF_CXXLINKOPTS          += 
ESMF_F90COMPILEOPTS       += 
ESMF_F90LINKOPTS          += 
endif
ifeq ($(ESMF_ABISTRING),x86_64_small)
ESMF_CXXCOMPILEOPTS       += -mcmodel=small
ESMF_CXXLINKOPTS          += -mcmodel=small
ESMF_F90COMPILEOPTS       += -mcmodel=small
ESMF_F90LINKOPTS          += -mcmodel=small
endif
ifeq ($(ESMF_ABISTRING),x86_64_medium)
ESMF_CXXCOMPILEOPTS       += -mcmodel=medium
ESMF_CXXLINKOPTS          += -mcmodel=medium
ESMF_F90COMPILEOPTS       += -mcmodel=medium
ESMF_F90LINKOPTS          += -mcmodel=medium
endif

############################################################
# Enable TR15581/F2003 Allocatable array resizing
#
ESMF_F90COMPILEOPTS += -Mallocatable=03

############################################################
# Conditionally add pthread compiler and linker flags
#
ifeq ($(ESMF_PTHREADS),ON)
ESMF_F90COMPILEOPTS += -pthread
ESMF_CXXCOMPILEOPTS += -pthread
ESMF_F90LINKOPTS    += -pthread
ESMF_CXXLINKOPTS    += -pthread
endif

############################################################
# OpenMP compiler and linker flags
#
ESMF_OPENMP_F90COMPILEOPTS += -mp
ESMF_OPENMP_CXXCOMPILEOPTS += -mp
ESMF_OPENMP_F90LINKOPTS    += -mp
ESMF_OPENMP_CXXLINKOPTS    += -mp

############################################################
# OpenACC compiler and linker flags (the -Minfo just there for debugging)
#
ESMF_OPENACCDEFAULT = OFF
ESMF_OPENACC_F90COMPILEOPTS += -acc -Minfo
ESMF_OPENACC_CXXCOMPILEOPTS += -acc -Minfo
ESMF_OPENACC_F90LINKOPTS    += -acc -Minfo
ESMF_OPENACC_CXXLINKOPTS    += -acc -Minfo

############################################################
# Explicit flags for handling specific format and cpp combos
#
ESMF_F90COMPILEFREENOCPP = -Mfreeform
ESMF_F90COMPILEFIXCPP    = -Mpreprocess -Mnofreeform

############################################################
# Set rpath syntax
#
ESMF_F90RPATHPREFIX         = -Wl,-rpath,
ESMF_CXXRPATHPREFIX         = -Wl,-rpath,
ESMF_CRPATHPREFIX           = -Wl,-rpath,

############################################################
# Link against libesmf.a using the F90 linker front-end
#
ESMF_F90LINKLIBS += -c++libs

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ESMF_CXXLINKLIBS += -fortranlibs

############################################################
# Linker option that ensures that the specified libraries are 
# used to also resolve symbols needed by other libraries.
#
ESMF_F90LINKOPTS          += -Wl,--no-as-needed
ESMF_CXXLINKOPTS          += -Wl,--no-as-needed

############################################################
# Shared library options
#
ifeq ($(ESMF_OPENACC),ON)
# Currently accelerator code is not supported inside shared libraries.
# Turn off shared lib build if OpenACC code active inside ESMF.
ESMF_SL_LIBS_TO_MAKE  =
endif
ESMF_SL_LIBOPTS  += -shared

############################################################
# Shared object options
#
ESMF_SO_F90COMPILEOPTS  = -fpic
ESMF_SO_F90LINKOPTS     = -shared
ESMF_SO_F90LINKOPTSEXE  = -Wl,-export-dynamic
ESMF_SO_CXXCOMPILEOPTS  = -fpic
ESMF_SO_CXXLINKOPTS     = -shared
ESMF_SO_CXXLINKOPTSEXE  = -Wl,-export-dynamic
