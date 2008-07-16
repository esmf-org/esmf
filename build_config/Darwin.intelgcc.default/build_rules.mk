# $Id: build_rules.mk,v 1.2.2.4 2008/07/16 00:19:09 theurich Exp $
#
# Darwin.intelgcc.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = ifort
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
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_MPICH
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpiCC
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),mpich2)
# Mpich2 ---------------------------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpicxx
ESMF_MPIRUNDEFAULT      = mpirun
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),intelmpi)
# IntelMPI -------------------------------------------------
ESMF_F90DEFAULT         = mpiifort
ESMF_CXXDEFAULT         = mpicxx
ESMF_MPIRUNDEFAULT      = mpirun
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),lam)
# LAM (assumed to be built with ifort) ---------------------
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_NO_SIGUSR2
ESMF_F90DEFAULT         = mpif77
ESMF_CXXDEFAULT         = mpic++
ESMF_MPIRUNDEFAULT      = mpirun
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),openmpi)
# OpenMPI --------------------------------------------------
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_NO_SIGUSR2
ESMF_F90DEFAULT         = mpif90
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

############################################################
# Print compiler version string
#
ESMF_F90COMPILER_VERSION    = ${ESMF_F90COMPILER} -V -v
ESMF_CXXCOMPILER_VERSION    = ${ESMF_CXXCOMPILER} -v --version

############################################################
# On IA64 set long and pointer types to 64-bit
#
ifeq ($(ESMF_ABI),64)
ESMF_F90COMPILEOPTS       += -size_lp64
ESMF_F90LINKOPTS          += -size_lp64
endif

############################################################
# Link against GCC's stdc++ library (because g++ is used)
#
ESMF_LIBSTDCXX := $(shell $(ESMF_CXXCOMPILER) -print-file-name=libstdc++.dylib)
ifeq ($(ESMF_LIBSTDCXX),libstdc++.dylib)
ESMF_LIBSTDCXX := $(shell $(ESMF_CXXCOMPILER) -print-file-name=libstdc++.a)
endif
ESMF_F90LINKPATHS += -L$(dir $(ESMF_LIBSTDCXX))
ESMF_F90LINKLIBS  += -lstdc++ -lgcc_eh

############################################################
# Conditionally add pthread compiler and linker flags
#
ifeq ($(ESMF_PTHREADS),ON)
ESMF_F90COMPILEOPTS += -threads
ESMF_F90LINKOPTS    += -threads
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

############################################################
# Blank out variables to prevent rpath encoding
#
ESMF_F90LINKRPATHS      =
ESMF_CXXLINKRPATHS      =

############################################################
# Link against libesmf.a using the F90 linker front-end
#
ESMF_F90LINKLIBS += -lm 

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ESMF_CXXLINKLIBS += -lifcoremt -lunwind
ifeq ($(ESMF_ABI),64)
ESMF_CXXLINKLIBS += -lipr
endif

############################################################
# Blank out shared library options
#
ESMF_SL_LIBS_TO_MAKE  =
