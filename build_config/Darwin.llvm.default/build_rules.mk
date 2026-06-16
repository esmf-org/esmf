# $Id$
#
# Darwin.llvm.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = flang
ESMF_CXXDEFAULT         = clang++
ESMF_CDEFAULT           = clang
ESMF_CPPDEFAULT		= clang -E -P -x c

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
ifeq ($(ESMF_COMM),openmpi)
# OpenMPI --------------------------------------------------
ifeq ($(shell $(ESMF_DIR)/scripts/available mpifort),mpifort)
ESMF_F90DEFAULT         = mpifort
else
ESMF_F90DEFAULT         = mpif90
endif
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_NO_SIGUSR2
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

############################################################
# Print compiler version string
#
ESMF_F90COMPILER_VERSION    = ${ESMF_F90COMPILER} -v --version
ESMF_CXXCOMPILER_VERSION    = ${ESMF_CXXCOMPILER} -v --version
ESMF_CCOMPILER_VERSION      = ${ESMF_CCOMPILER} -v --version

############################################################
# See if this is LLVM Clang or Apple Clang
#
ESMF_CLANGSTR := $(findstring Apple clang, $(shell $(ESMF_CXXCOMPILER) --version))
ifeq ($(ESMF_CLANGSTR),Apple clang)
$(info "The detected C++ compiler is Apple Clang.")
else
$(info "The detected C++ compiler is LLVM Clang.")
endif

############################################################
# Special debug flags
#
ESMF_F90OPTFLAG_G       +=

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
ESMF_CCOMPILEOPTS         += -m32
ESMF_CLINKOPTS            += -m32
ESMF_F90COMPILEOPTS       += 
ESMF_F90LINKOPTS          += 
endif
ifeq ($(ESMF_ABISTRING),x86_64_small)
ESMF_CXXCOMPILEOPTS       += -m64 -mcmodel=small
ESMF_CXXLINKOPTS          += -m64 -mcmodel=small
ESMF_CCOMPILEOPTS         += -m64 -mcmodel=small
ESMF_CLINKOPTS            += -m64 -mcmodel=small
ESMF_F90COMPILEOPTS       += 
ESMF_F90LINKOPTS          += 
endif
ifeq ($(ESMF_ABISTRING),x86_64_medium)
ESMF_CXXCOMPILEOPTS       += -m64 -mcmodel=medium
ESMF_CXXLINKOPTS          += -m64 -mcmodel=medium
ESMF_CCOMPILEOPTS         += -m64 -mcmodel=medium
ESMF_CLINKOPTS            += -m64 -mcmodel=medium
ESMF_F90COMPILEOPTS       += 
ESMF_F90LINKOPTS          += 
endif

############################################################
# Conditionally add pthread compiler and linker flags
#
ifeq ($(ESMF_PTHREADS),ON)
ESMF_F90COMPILEOPTS +=
ESMF_CXXCOMPILEOPTS += -pthread
ESMF_CCOMPILEOPTS   += -pthread
ESMF_F90LINKOPTS    += -pthread
ESMF_CXXLINKOPTS    += -pthread
ESMF_CLINKOPTS      += -pthread
endif

############################################################
# OpenMP compiler and linker flags
#
ifeq ($(ESMF_CLANGSTR),Apple clang)
# Apple Clang does not support OpenMP natively.
# It requires explicit installation of libomp and manually pointing to the
# associated include and lib directories.
ESMF_OPENMPDEFAULT = OFF
ESMF_OPENMP_CXXCOMPILEOPTS += -Xpreprocessor -fopenmp
ESMF_OPENMP_CXXLINKOPTS    += -Xpreprocessor -fopenmp
else
# LLVM Clang supports OpenMP version 4
ESMF_OPENMPDEFAULT = OMP4
ESMF_OPENMP_CXXCOMPILEOPTS += -fopenmp
ESMF_OPENMP_CXXLINKOPTS    += -fopenmp
endif
ESMF_OPENMP_F90COMPILEOPTS += -fopenmp
ESMF_OPENMP_F90LINKOPTS    += -fopenmp

############################################################
# OpenACC compiler and linker flags
#
ifeq ($(ESMF_CLANGSTR),Apple clang)
# Apple Clang does not support OpenACC
ESMF_OPENACCDEFAULT = OFF
else
# LLVM Clang supports OpenACC, use ESMF default
ESMF_OPENACC_CXXCOMPILEOPTS += -fopenacc
ESMF_OPENACC_CXXLINKOPTS    += -fopenacc
endif
ESMF_OPENACC_F90COMPILEOPTS += -fopenacc
ESMF_OPENACC_F90LINKOPTS    += -fopenacc

############################################################
# Explicit flags for handling specific format and cpp combos
#
ESMF_F90COMPILEFREENOCPP = -ffree-form
ESMF_F90COMPILEFIXCPP    = -cpp -ffixed-form

############################################################
# Set rpath syntax
#
ESMF_F90RPATHPREFIX         = -Wl,-rpath,
ESMF_CXXRPATHPREFIX         = -Wl,-rpath,
ESMF_CRPATHPREFIX           = -Wl,-rpath,

############################################################
# Determine where clang's libraries are located
# Use when linking against libesmf with F90 linker front-end
#
# Note that the result of -print-file-name will be the full path to the file if it is found
# within the compiler installation, and simply the file name verbatim if it is NOT found.
ifneq ($(ESMF_CLANGSTR),Apple clang)
ESMF_LIBSTDCXX := $(shell $(ESMF_CXXLINKER) $(ESMF_CXXLINKOPTS) -print-file-name=libc++.dylib)
ifeq ($(ESMF_LIBSTDCXX),libc++.dylib)
ESMF_LIBSTDCXX := $(shell $(ESMF_CXXLINKER) $(ESMF_CXXLINKOPTS) -print-file-name=libc++.a)
endif
ESMF_F90LINKLIBS += $(ESMF_LIBSTDCXX)
ESMF_F90LINKPATHS += -L$(dir $(ESMF_LIBSTDCXX))
ESMF_F90LINKRPATHS += $(ESMF_F90RPATHPREFIX)$(dir $(ESMF_LIBSTDCXX))
else
ESMF_F90LINKPATHS += $(shell $(ESMF_DIR)/scripts/libpath.flang $(ESMF_F90COMPILER) $(ESMF_F90COMPILEOPTS))
ESMF_F90LINKRPATHS += $(patsubst -L%,$(ESMF_F90RPATHPREFIX)%,$(ESMF_F90LINKPATHS))
ESMF_F90LINKLIBS += -lc++
endif

############################################################
# Determine where LLVM libraries are located for C++
#
ESMF_CXXLINKPATHS += $(shell $(ESMF_DIR)/scripts/libpath.flang $(ESMF_F90COMPILER) $(ESMF_F90COMPILEOPTS))
ESMF_CXXLINKRPATHS += $(patsubst -L%,$(ESMF_CXXRPATHPREFIX)%,$(ESMF_CXXLINKPATHS))

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ESMF_CXXLINKLIBS += $(shell $(ESMF_DIR)/scripts/libs.llvm $(ESMF_F90COMPILER) $(ESMF_F90COMPILEOPTS))

############################################################
# Shared library options
ESMF_SL_LIBOPTS  += -dynamiclib
ESMF_SL_LIBLIBS  += $(ESMF_F90LINKPATHS) $(ESMF_F90LINKLIBS) $(ESMF_CXXLINKPATHS) $(ESMF_CXXLINKLIBS)

############################################################
# Shared object options
#
ESMF_SO_F90COMPILEOPTS  = -fPIC
ESMF_SO_F90LINKOPTS     = -dynamiclib
ESMF_SO_CXXCOMPILEOPTS  = -fPIC
ESMF_SO_CXXLINKOPTS     = -dynamiclib
