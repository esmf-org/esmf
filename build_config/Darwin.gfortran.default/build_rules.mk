# $Id$
#
# Darwin.gfortran.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = gfortran
ESMF_CXXDEFAULT         = g++
ESMF_CDEFAULT           = gcc

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
ESMF_CXXLINKLIBS       += $(shell $(ESMF_DIR)/scripts/libs.mpich3f90)
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
ifeq ($(ESMF_COMM),lam)
# LAM (assumed to be built with gfortran) ----------------
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

############################################################
# Print compiler version string
#
ESMF_F90COMPILER_VERSION    = ${ESMF_F90COMPILER} --version
ESMF_CXXCOMPILER_VERSION    = ${ESMF_CXXCOMPILER} --version
ESMF_CCOMPILER_VERSION      = ${ESMF_CCOMPILER} --version

############################################################
# See if g++ is really clang
#
ESMF_CLANGSTR := $(findstring clang, $(shell $(ESMF_CXXCOMPILER) --version))
ifeq ($(ESMF_CLANGSTR), clang)
$(error "The detected C++ compiler is actually clang. Set ESMF_COMPILER=gfortranclang.")
endif

############################################################
# Special debug flags
#
ESMF_F90OPTFLAG_G       += -Wall -Wextra -Wconversion -Wno-unused -Wno-unused-dummy-argument -fbacktrace -fimplicit-none -fcheck=all,no-pointer
ESMF_CXXOPTFLAG_G       += -Wall -Wextra -Wno-unused

############################################################
# Fortran symbol convention
#
ifeq ($(ESMF_FORTRANSYMBOLS),default)
ESMF_F90COMPILEOPTS       +=
ESMF_F90LINKOPTS          +=
ESMF_CPPFLAGS             += -DESMF_LOWERCASE_SINGLEUNDERSCORE
else
ifeq ($(ESMF_FORTRANSYMBOLS),lowercase_singleunderscore)
ESMF_F90COMPILEOPTS       += -fno-second-underscore
ESMF_F90LINKOPTS          += -fno-second-underscore
ESMF_CPPFLAGS             += -DESMF_LOWERCASE_SINGLEUNDERSCORE
else
ifeq ($(ESMF_FORTRANSYMBOLS),lowercase_doubleunderscore)
ESMF_F90COMPILEOPTS       += -fsecond-underscore
ESMF_F90LINKOPTS          += -fsecond-underscore
ESMF_CPPFLAGS             += -DESMF_LOWERCASE_DOUBLEUNDERSCORE
else
$(error "ESMF_FORTRANSYMBOLS = $(ESMF_FORTRANSYMBOLS)" not supported by ESMF and/or this platform)
endif
endif
endif

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
ESMF_F90COMPILEOPTS += -pthread
ESMF_CXXCOMPILEOPTS += -pthread
ESMF_F90LINKOPTS    += -pthread
ESMF_CXXLINKOPTS    += -pthread
endif

############################################################
# OpenMP compiler and linker flags
#
ESMF_OPENMP_F90COMPILEOPTS += -fopenmp
ESMF_OPENMP_CXXCOMPILEOPTS += -fopenmp
ESMF_OPENMP_F90LINKOPTS    += -fopenmp
ESMF_OPENMP_CXXLINKOPTS    += -fopenmp

############################################################
# OpenACC compiler and linker flags
#
ESMF_OPENACCDEFAULT = OFF
ESMF_OPENACC_F90COMPILEOPTS += -fopenacc
ESMF_OPENACC_CXXCOMPILEOPTS += -fopenacc
ESMF_OPENACC_F90LINKOPTS    += -fopenacc
ESMF_OPENACC_CXXLINKOPTS    += -fopenacc

############################################################
# Explicit flags for handling specific format and cpp combos
#
ESMF_F90COMPILEFREENOCPP = -ffree-form
ESMF_F90COMPILEFIXCPP    = -cpp -ffixed-form

############################################################
# Set unlimited line length limit for free format files
#
ESMF_F90COMPILEOPTS += -ffree-line-length-none

############################################################
# Determine where gcc's libraries are located
#
# Note that the result of -print-file-name will be the full path to the file if it is found
# within the compiler installation, and simply the file name verbatim if it is NOT found.
ESMF_LIBSTDCXX := $(shell $(ESMF_CXXCOMPILER) -print-file-name=libstdc++.dylib)
ifeq ($(ESMF_LIBSTDCXX),libstdc++.dylib)
ESMF_LIBSTDCXX := $(shell $(ESMF_CXXCOMPILER) -print-file-name=libstdc++.a)
endif
ESMF_F90LINKPATHS += -L$(dir $(ESMF_LIBSTDCXX))

############################################################
# Determine where gfortran's libraries are located
#
# Note that the result of -print-file-name will be the full path to the file if it is found
# within the compiler installation, and simply the file name verbatim if it is NOT found.
ESMF_LIBGFORTRAN := $(shell $(ESMF_F90COMPILER) -print-file-name=libgfortran.dylib)
ifeq ($(ESMF_LIBGFORTRAN),libgfortran.dylib)
ESMF_LIBGFORTRAN := $(shell $(ESMF_F90COMPILER) -print-file-name=libgfortran.a)
endif
ESMF_CXXLINKPATHS += -L$(dir $(ESMF_LIBGFORTRAN))

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

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ESMF_CXXLINKLIBS += -lgfortran

############################################################
# Shared library options
ESMF_SL_LIBOPTS  += -dynamiclib
ESMF_SL_LIBLIBS  += $(ESMF_F90LINKPATHS) $(ESMF_F90LINKLIBS) $(ESMF_CXXLINKPATHS) $(ESMF_CXXLINKLIBS)

############################################################
# Shared object options
#
ESMF_SO_F90COMPILEOPTS  = -fPIC
ESMF_SO_F90LINKOPTS     = -shared
ESMF_SO_CXXCOMPILEOPTS  = -fPIC
ESMF_SO_CXXLINKOPTS     = -shared
