# $Id$
#
# Darwin.nag.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = nagfor
ESMF_F90LINKERDEFAULT   = $(ESMF_CXXLINKER)
ESMF_CXXDEFAULT         = clang++
ESMF_CDEFAULT           = clang
ESMF_CLINKERDEFAULT     = clang++
ESMF_CLINKLIBS          = $(ESMF_CXXLINKLIBS)
ESMF_CPPDEFAULT         = clang -E -P -x c

ESMF_CXXCOMPILEOPTS    += -x c++ -mmacosx-version-min=10.7 -stdlib=libc++

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
ESMF_CLINKERDEFAULT     = mpiCC
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),mpich2)
# Mpich2 ---------------------------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpicxx
ESMF_CDEFAULT           = mpicc
ESMF_CLINKERDEFAULT     = mpicxx
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),mpich)
# Mpich3 and up --------------------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpicxx
ESMF_CDEFAULT           = mpicc
ESMF_CLINKERDEFAULT     = mpicxx
ESMF_CXXLINKLIBS       += $(shell $(ESMF_DIR)/scripts/libs.mpich3f90)
ESMF_F90LINKLIBS       += $(shell $(ESMF_DIR)/scripts/libs.mpich3f90)
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),mvapich)
# Mvapich any version --------------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpicxx
ESMF_CDEFAULT           = mpicc
ESMF_CLINKERDEFAULT     = mpicxx
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),lam)
# LAM (assumed to be built with nag f95) ----------------
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_NO_SIGUSR2
ESMF_F90DEFAULT         = mpif77
ESMF_CXXDEFAULT         = mpic++
ESMF_CDEFAULT           = mpicc
ESMF_CLINKERDEFAULT     = mpic++
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
ESMF_F90LINKLIBS       += $(shell $(ESMF_DIR)/scripts/libs.openmpif90_forcxx $(ESMF_F90DEFAULT))
ESMF_CXXDEFAULT         = mpicxx
ESMF_CDEFAULT           = mpicc
ESMF_CLINKERDEFAULT     = mpicxx
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

############################################################
# Print compiler version string
#
ESMF_F90COMPILER_VERSION    = ${ESMF_F90COMPILER} -v -V -dryrun
ESMF_CXXCOMPILER_VERSION    = ${ESMF_CXXCOMPILER} -v --version
ESMF_CCOMPILER_VERSION      = ${ESMF_CCOMPILER} -v --version

############################################################
# See if g++ is really clang
#
ESMF_CLANGSTR := $(findstring clang, $(shell $(ESMF_CXXCOMPILER) --version))
ifneq ($(ESMF_CLANGSTR), clang)
$(error "The detected C++ compiler is actually not clang. Set ESMF_COMPILER=nag.")
endif

############################################################
# Set NAG unix modules when certain non-Standard system calls
# (e.g., ABORT) are made.
ESMF_F90COMPILEOPTS += -DESMF_NAG_UNIX_MODULE

############################################################
# nag currently does not support OpenMP
#
ESMF_OPENMP := OFF

############################################################
# Some ESMF tests fail for NAG with -O -> turn optimization off by default
#
ESMF_OPTLEVELDEFAULT  = 0

############################################################
# Set kind numbering system to "byte"
#
# ESMF_F90COMPILEOPTS += -kind=byte

############################################################
# Set f95 to be more premissive and issue warning before error
#
ESMF_F90COMPILEOPTS += -dusty

############################################################
# Conditionally add pthread compiler and linker flags
#

ifeq ($(ESMF_PTHREADS),ON)
ESMF_F90COMPILEOPTS += -thread_safe
ESMF_CXXCOMPILEOPTS += -pthread
ESMF_CCOMPILEOPTS   += -pthread
ESMF_F90LINKOPTS    += -pthread
ESMF_CXXLINKOPTS    += -pthread
ESMF_CLINKOPTS      += -pthread
endif

############################################################
# Explicit flags for handling specific format and cpp combos
#
ESMF_F90COMPILEFREECPP   = -free -fpp
ESMF_F90COMPILEFREENOCPP = -free
ESMF_F90COMPILEFIXCPP    = -fixed -fpp
ESMF_F90COMPILEFIXNOCPP  = -fixed

############################################################
# Blank out variables to prevent rpath encoding
#
ESMF_F90LINKRPATHS      =
ESMF_CXXLINKRPATHS      =
ESMF_CLINKRPATHS        =

############################################################
# Link against libesmf.a using the F90 linker front-end
#
ESMF_F90LINKLIBS += -lstdc++
ESMF_F90LINKLIBS += $(shell $(ESMF_DIR)/scripts/libs.nag $(ESMF_F90COMPILER))

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ESMF_CXXLINKLIBS += $(shell $(ESMF_DIR)/scripts/libs.nag $(ESMF_F90COMPILER))

############################################################
# Shared library options
ESMF_SL_LIBOPTS  += -dynamiclib
ESMF_SL_LIBLIBS  += $(ESMF_CXXLINKPATHS) $(ESMF_CXXLINKLIBS)

############################################################
# Static builds on Darwin do not support trace lib due to missing linker option
ifeq ($(ESMF_SHARED_LIB_BUILD),OFF)
ESMF_TRACE_LIB_BUILD = OFF
endif
