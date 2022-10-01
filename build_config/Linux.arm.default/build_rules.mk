# $Id$
#
# Linux.arm.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = armflang
ESMF_CXXDEFAULT         = armclang
ESMF_CDEFAULT           = armclang
ESMF_CPPDEFAULT		= armclang -E -P -x c

ESMF_CXXCOMPILECPPFLAGS += -x c++

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
ESMF_F90COMPILECPPFLAGS+= -DESMF_NO_MPI3
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_NO_MPI3
else
ifeq ($(ESMF_COMM),mpich2)
# Mpich2 ---------------------------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpicxx
ESMF_CDEFAULT           = mpicc
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
ESMF_F90COMPILECPPFLAGS+= -DESMF_NO_MPI3
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_NO_MPI3
else
ifeq ($(ESMF_COMM),mpich)
# Mpich3 and up --------------------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpicxx
ESMF_CDEFAULT           = mpicc
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),mvapich2)
# Mvapich2 ---------------------------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpicxx
ESMF_CDEFAULT           = mpicc
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),lam)
# LAM (assumed to be built with armflang) -----------------------
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_NO_SIGUSR2
ESMF_F90DEFAULT         = mpif77
ESMF_CXXDEFAULT         = mpic++
ESMF_CDEFAULT           = mpicc
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
ESMF_F90COMPILECPPFLAGS+= -DESMF_NO_MPI3
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_NO_MPI3
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

############################################################
# Print compiler version string
#
ESMF_F90COMPILER_VERSION    = ${ESMF_F90COMPILER} -v --version
ESMF_CXXCOMPILER_VERSION    = ${ESMF_CXXCOMPILER} -v --version
ESMF_CCOMPILER_VERSION      = ${ESMF_CCOMPILER} -v --version

############################################################
# Currently no support for the Fortran2018 assumed type feature
#
ESMF_F90COMPILECPPFLAGS += -DESMF_NO_F2018ASSUMEDTYPE
ESMF_CXXCOMPILECPPFLAGS += -DESMF_NO_F2018ASSUMEDTYPE

############################################################
# Special debug flags
#
ESMF_F90OPTFLAG_G       += -Wall -Wextra -Wconversion -Wno-unused -Wno-unused-dummy-argument -fbacktrace -fimplicit-none -fcheck=all,no-pointer

############################################################
# Fortran symbol convention
#
ifeq ($(ESMF_FORTRANSYMBOLS),default)
ESMF_F90COMPILEOPTS       +=
ESMF_F90LINKOPTS          +=
ESMF_CXXCOMPILEOPTS       += -DESMF_LOWERCASE_SINGLEUNDERSCORE
else
ifeq ($(ESMF_FORTRANSYMBOLS),lowercase_singleunderscore)
ESMF_F90COMPILEOPTS       += -fno-second-underscore
ESMF_F90LINKOPTS          += -fno-second-underscore
ESMF_CXXCOMPILEOPTS       += -DESMF_LOWERCASE_SINGLEUNDERSCORE
else
ifeq ($(ESMF_FORTRANSYMBOLS),lowercase_doubleunderscore)
ESMF_F90COMPILEOPTS       += -fsecond-underscore
ESMF_F90LINKOPTS          += -fsecond-underscore
ESMF_CXXCOMPILEOPTS       += -DESMF_LOWERCASE_DOUBLEUNDERSCORE
else
$(error "ESMF_FORTRANSYMBOLS = $(ESMF_FORTRANSYMBOLS)" not supported by ESMF and/or this platform)
endif
endif
endif

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
ESMF_OPENMP=OFF
# ESMF_OPENMP_F90COMPILEOPTS += -fopenmp
# ESMF_OPENMP_CXXCOMPILEOPTS += -fopenmp
# ESMF_OPENMP_F90LINKOPTS    += -fopenmp
# ESMF_OPENMP_CXXLINKOPTS    += -fopenmp

############################################################
# Need this until the file convention is fixed (then remove these two lines)
#
ESMF_F90COMPILEFREENOCPP = -ffree-form
ESMF_F90COMPILEFIXCPP    = -cpp -ffixed-form

############################################################
# Set unlimited line length limit for free format files
#
ESMF_F90COMPILEOPTS += -ffree-line-length-none

############################################################
# Set rpath syntax
#
ESMF_F90RPATHPREFIX         = -Wl,-rpath,
ESMF_CXXRPATHPREFIX         = -Wl,-rpath,
ESMF_CRPATHPREFIX           = -Wl,-rpath,

############################################################
# Determine where gcc's libraries are located
#
ESMF_LIBSTDCXX := $(shell $(ESMF_CXXCOMPILER) $(ESMF_CXXCOMPILEOPTS) -print-file-name=libstdc++.so)
ifeq ($(ESMF_LIBSTDCXX),libstdc++.so)
ESMF_LIBSTDCXX := $(shell $(ESMF_CXXCOMPILER) $(ESMF_CXXCOMPILEOPTS) -print-file-name=libstdc++.a)
endif
ESMF_F90LINKPATHS += -L$(dir $(ESMF_LIBSTDCXX))
ESMF_F90LINKRPATHS += $(ESMF_F90RPATHPREFIX)$(dir $(ESMF_LIBSTDCXX))

############################################################
# Determine where armflang's libraries are located
#
ESMF_LIBARMFLANG := $(shell $(ESMF_F90COMPILER) $(ESMF_F90COMPILEOPTS) -print-file-name=libarmflang.so)
ifeq ($(ESMF_LIBARMFLANG),libarmflang.so)
ESMF_LIBARMFLANG := $(shell $(ESMF_F90COMPILER) $(ESMF_F90COMPILEOPTS) -print-file-name=libarmflang.a)
endif
ESMF_CXXLINKPATHS += -L$(dir $(ESMF_LIBARMFLANG))
ESMF_CXXLINKRPATHS += $(ESMF_CXXRPATHPREFIX)$(dir $(ESMF_LIBARMFLANG))

############################################################
# Link against libesmf.a using the F90 linker front-end
#
ESMF_F90LINKLIBS += -lrt -lstdc++ -ldl

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ESMF_CXXLINKLIBS += -lrt -larmflang -lstdc++ -lm -ldl

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
