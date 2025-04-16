# $Id$
#
# Linux.pgi.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = pgf90
# Use pgc++ if available, otherwise fall back to older pgCC front-end
ifeq ($(shell $(ESMF_DIR)/scripts/available pgc++),pgc++)
ESMF_CXXDEFAULT         = pgc++
else
ESMF_CXXDEFAULT         = pgCC
endif
ESMF_CDEFAULT           = pgcc

############################################################
# Do not use an explicit std switch for C99
#
ESMF_CSTD               = sysdefault
ESMF_CSTDFLAG           =

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
else
ifeq ($(ESMF_COMM),mpich1)
# Mpich1 ---------------------------------------------------
ESMF_F90COMPILECPPFLAGS+= -DESMF_MPICH1
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_MPICH1
ESMF_F90DEFAULT         = mpif90
ESMF_F90LINKLIBS       += -lpmpich++ -lmpich
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
ESMF_F90DEFAULT         = mpipgf90
ESMF_CXXDEFAULT         = mpipgc++
ESMF_CDEFAULT           = mpipgcc
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
# LAM (assumed to be built with pgf90) ---------------------
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
ESMF_F90COMPILER_VERSION    = $(ESMF_DIR)/scripts/version.pgf90 $(ESMF_F90COMPILER)
ESMF_CXXCOMPILER_VERSION    = $(ESMF_DIR)/scripts/version.pgCC $(ESMF_CXXCOMPILER)
ESMF_CCOMPILER_VERSION      = $(ESMF_DIR)/scripts/version.pgCC $(ESMF_CXXCOMPILER)

############################################################
# Determine PGI version
#
ESMF_PGIVERSION_MAJOR = $(shell $(ESMF_DIR)/scripts/version.pgi 1 $(ESMF_F90COMPILER_VERSION))
ESMF_F90COMPILECPPFLAGS += -DESMF_PGIVERSION_MAJOR=$(ESMF_PGIVERSION_MAJOR)
ESMF_CXXCOMPILECPPFLAGS += -DESMF_PGIVERSION_MAJOR=$(ESMF_PGIVERSION_MAJOR)

ESMF_PGIVERSION_MINOR = $(shell $(ESMF_DIR)/scripts/version.pgi 2 $(ESMF_F90COMPILER_VERSION))
ESMF_F90COMPILECPPFLAGS += -DESMF_PGIVERSION_MINOR=$(ESMF_PGIVERSION_MINOR)
ESMF_CXXCOMPILECPPFLAGS += -DESMF_PGIVERSION_MINOR=$(ESMF_PGIVERSION_MINOR)

ESMF_PGIVERSION_PATCH = $(shell $(ESMF_DIR)/scripts/version.pgi 3 $(ESMF_F90COMPILER_VERSION))
ESMF_F90COMPILECPPFLAGS += -DESMF_PGIVERSION_PATCH=$(ESMF_PGIVERSION_PATCH)
ESMF_CXXCOMPILECPPFLAGS += -DESMF_PGIVERSION_PATCH=$(ESMF_PGIVERSION_PATCH)

############################################################
# Currently PGI does not support the Fortran2018 assumed type feature
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
# mcmodel is small by default.  But due to a bug in PGI 14.7 and
# earlier overcompiler, the libso directory is not searched prior to
# the lib directory.  This causes the fullylinked shared lib build to fail.
# ESMF_CXXCOMPILEOPTS       += -mcmodel=small
# ESMF_CXXLINKOPTS          += -mcmodel=small
# ESMF_F90COMPILEOPTS       += -mcmodel=small
# ESMF_F90LINKOPTS          += -mcmodel=small
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
ESMF_F90COMPILEOPTS += -lpthread
ESMF_CXXCOMPILEOPTS += -lpthread
ESMF_F90LINKOPTS    += -lpthread
ESMF_CXXLINKOPTS    += -lpthread
endif

############################################################
# OpenMP compiler and linker flags
#
ESMF_OPENMP_F90COMPILEOPTS += -mp
ESMF_OPENMP_CXXCOMPILEOPTS += -mp --exceptions
ESMF_OPENMP_F90LINKOPTS    += -mp
ESMF_OPENMP_CXXLINKOPTS    += -mp --exceptions
# Newer vers of PGI (>6) have trouble with OpenMP symbols under cross lang. link
ESMF_OPENMP := OFF

############################################################
# OpenACC compiler and linker flags (the -Minfo just there for debugging)
#
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
# Determine where pgf90's libraries are located
#
ESMF_CXXLINKPATHS += -L$(shell $(ESMF_DIR)/scripts/libpath.pgf90 $(ESMF_F90COMPILER))
ESMF_CXXLINKRPATHS += $(ESMF_CXXRPATHPREFIX)$(shell $(ESMF_DIR)/scripts/libpath.pgf90 $(ESMF_F90COMPILER))

############################################################
# Determine where pgCC's libraries are located
#
ESMF_F90LINKPATHS += -L$(shell $(ESMF_DIR)/scripts/libpath.pgCC $(ESMF_CXXCOMPILER))
ESMF_F90LINKRPATHS += $(ESMF_F90RPATHPREFIX)$(shell $(ESMF_DIR)/scripts/libpath.pgCC $(ESMF_CXXCOMPILER))

############################################################
# Link against libesmf.a using the F90 linker front-end
#
ifeq ($(ESMF_PGIVERSION_MAJOR),7)
ESMF_F90LINKLIBS += -lrt -lstd -lC $(shell $(ESMF_DIR)/scripts/libs.pgCC $(ESMF_CXXCOMPILER)) -ldl
else
ifeq ($(shell $(ESMF_DIR)/scripts/compiler.pgcxx $(ESMF_CXXCOMPILER)),pgc++)
ESMF_F90LINKLIBS += -pgc++libs -ldl
else
ESMF_F90LINKLIBS += -pgcpplibs -ldl
endif
endif

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ifeq ($(ESMF_PGIVERSION_MAJOR),7)
ESMF_CXXLINKLIBS += -lrt $(shell $(ESMF_DIR)/scripts/libs.pgf90 $(ESMF_F90COMPILER)) -ldl
else
ESMF_CXXLINKLIBS += -pgf90libs -ldl
endif

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
ESMF_SO_F90COMPILEOPTS  = -fpic
ESMF_SO_F90LINKOPTS     = -shared
ESMF_SO_F90LINKOPTSEXE  = -Wl,-export-dynamic
ESMF_SO_CXXCOMPILEOPTS  = -fpic
ESMF_SO_CXXLINKOPTS     = -shared
ESMF_SO_CXXLINKOPTSEXE  = -Wl,-export-dynamic
