# $Id$
#
# Darwin.nag.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = nagfor
ESMF_CXXDEFAULT         = g++

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
ifeq ($(ESMF_COMM),mpich3)
# Mpich3 ---------------------------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpicxx
ESMF_CXXLINKLIBS       += $(shell $(ESMF_DIR)/scripts/libs.mpich3f90)
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),mvapich2)
# Mvapich2 ---------------------------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpicxx
ESMF_CXXLINKLIBS       += $(shell $(ESMF_DIR)/scripts/libs.mvapich2f90)
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),lam)
# LAM (assumed to be built with nag f95) ----------------
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
ESMF_F90LINKLIBS       += $(shell $(ESMF_DIR)/scripts/libs.openmpif90 $(ESMF_F90DEFAULT))
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

############################################################
# Print compiler version string
#
ESMF_F90COMPILER_VERSION    = ${ESMF_F90COMPILER} -v -V -dryrun
ESMF_CXXCOMPILER_VERSION    = ${ESMF_CXXCOMPILER} -v --version

############################################################
# See if g++ is really clang
#
ESMF_CLANGSTR := $(findstring clang, $(shell $(ESMF_CXXCOMPILER) --version))

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
ESMF_F90LINKOPTS    += -thread_safe
endif

############################################################
# Need this until the file convention is fixed (then remove these two lines)
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

############################################################
# Link against libesmf.a using the F90 linker front-end
#
ESMF_F90LINKLIBS += -lstdc++
ifeq ($(ESMF_CLANGSTR), clang)
ESMF_F90LINKLIBS += -lc++
endif

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ESMF_CXXLINKLIBS += $(shell $(ESMF_DIR)/scripts/libs.nag $(ESMF_F90COMPILER))


############################################################
# Shared library options
ESMF_SL_LIBOPTS  += -dynamiclib
ESMF_SL_LIBLIBS  += $(ESMF_F90LINKPATHS) $(ESMF_F90LINKLIBS) $(ESMF_CXXLINKPATHS) $(ESMF_CXXLINKLIBS)

############################################################
# Add compile options required by external libraries
ifneq ($(ESMF_YAMLCPP),)
ESMF_CXXCOMPILEOPTS  += -std=c++11
endif
