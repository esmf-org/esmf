# $Id$
#
# Linux.absoftintel.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = f90
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
ifeq ($(ESMF_COMM),lam)
# LAM (assumed to be built with absoft f90) ----------------
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

############################################################
# Disable PIO until F2003 C Interop is verified to work
ESMF_PIO               = OFF

############################################################
# currently does not support OpenMP
#
ESMF_OPENMP := OFF

############################################################
# Print compiler version string
#
ESMF_F90COMPILER_VERSION    = ${ESMF_F90COMPILER} -V
ESMF_CXXCOMPILER_VERSION    = ${ESMF_CXXCOMPILER} -V -v
ESMF_CCOMPILER_VERSION      = ${ESMF_CCOMPILER} -V -v

############################################################
# Special debug flags
#
ESMF_CXXOPTFLAG_G       += -traceback

############################################################
# How to specify module directories
#
ESMF_F90IMOD        = -p

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

############################################################
# Conditionally add pthread compiler and linker flags
#
ifeq ($(ESMF_PTHREADS),ON)
ESMF_CXXCOMPILEOPTS += -pthread
ESMF_CXXLINKOPTS    += -pthread
endif

############################################################
# Fortran symbol convention
#
ifeq ($(ESMF_FORTRANSYMBOLS),default)
ESMF_F90COMPILEOPTS       += -YEXT_NAMES=LCS -YEXT_SFX=_
ESMF_F90LINKOPTS          += -YEXT_NAMES=LCS -YEXT_SFX=_
ESMF_CPPFLAGS             += -DESMF_LOWERCASE_SINGLEUNDERSCORE
else
ifeq ($(ESMF_FORTRANSYMBOLS),lowercase_singleunderscore)
ESMF_F90COMPILEOPTS       += -YEXT_NAMES=LCS -YEXT_SFX=_
ESMF_F90LINKOPTS          += -YEXT_NAMES=LCS -YEXT_SFX=_
ESMF_CPPFLAGS             += -DESMF_LOWERCASE_SINGLEUNDERSCORE
else
ifeq ($(ESMF_FORTRANSYMBOLS),lowercase_doubleunderscore)
ESMF_F90COMPILEOPTS       += -YEXT_NAMES=LCS -YEXT_SFX=__
ESMF_F90LINKOPTS          += -YEXT_NAMES=LCS -YEXT_SFX=__
ESMF_CPPFLAGS             += -DESMF_LOWERCASE_DOUBLEUNDERSCORE
else
$(error "ESMF_FORTRANSYMBOLS = $(ESMF_FORTRANSYMBOLS)" not supported by ESMF and/or this platform)
endif
endif
endif

############################################################
# Explicit flags for handling specific format and cpp combos
#
ESMF_F90COMPILEFREENOCPP = -ffree
ESMF_F90COMPILEFIXCPP    = -ffixed

############################################################
# Set rpath syntax
#
ESMF_F90RPATHPREFIX         = -Wl,-rpath,
ESMF_CXXRPATHPREFIX         = -Wl,-rpath,
ESMF_CRPATHPREFIX           = -Wl,-rpath,

############################################################
# Determine where absoft f90's libraries are located
#
ESMF_CXXLINKPATHS += $(addprefix -L,$(shell $(ESMF_DIR)/scripts/libpath.absoft $(ESMF_F90COMPILER) $(ESMF_F90COMPILEOPTS)))
ESMF_RPATHPREFIXFIXED := $(ESMF_CXXRPATHPREFIX)
ESMF_CXXLINKRPATHS += $(addprefix $(ESMF_RPATHPREFIXFIXED), $(shell $(ESMF_DIR)/scripts/libpath.absoft $(ESMF_F90COMPILER) $(ESMF_F90COMPILEOPTS)))

############################################################
# Determine where icpc's libraries are located
#
ESMF_F90LINKPATHS += $(addprefix -L,$(shell $(ESMF_DIR)/scripts/libpath.icpc "$(ESMF_CXXCOMPILER) $(ESMF_CXXCOMPILEOPTS)"))

############################################################
# Link against libesmf.a using the F90 linker front-end
#
ESMF_F90LINKLIBS += -lU77 $(shell $(ESMF_DIR)/scripts/libs.icpc "$(ESMF_CXXCOMPILER) $(ESMF_CXXCOMPILEOPTS)") -lrt -ldl

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ESMF_CXXLINKLIBS += -lU77 -lrt -ldl $(shell $(ESMF_DIR)/scripts/libs.absoft $(ESMF_F90COMPILER) $(ESMF_F90COMPILEOPTS))

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
