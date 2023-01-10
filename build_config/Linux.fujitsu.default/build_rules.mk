# $Id$
#
# Linux.fujitsu.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = frt -fw
ESMF_CXXDEFAULT         = FCC -Nclang
ESMF_CDEFAULT           = fcc
ESMF_CPPDEFAULT         = fcc -Nclang -E -P -x c -C -nostdinc

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
ESMF_F90DEFAULT         = mpifrt -fw
ESMF_CXXDEFAULT         = mpiFCC -Nclang
ESMF_CDEFAULT           = mpifcc -Nclang
ESMF_MPIRUNDEFAULT      = mpiexec
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
$(error Invalid ESMF_COMM setting: $(ESMF_COMM))
endif
endif

############################################################
# Print compiler version string
#
ESMF_F90COMPILER_VERSION    = ${ESMF_F90COMPILER} --version
ESMF_CXXCOMPILER_VERSION    = ${ESMF_CXXCOMPILER} --version
ESMF_CCOMPILER_VERSION      = ${ESMF_CCOMPILER} --version

############################################################
# Currently no support the Fortran2018 assumed type feature
#
ESMF_F90COMPILECPPFLAGS += -DESMF_NO_F2018ASSUMEDTYPE
ESMF_CXXCOMPILECPPFLAGS += -DESMF_NO_F2018ASSUMEDTYPE

############################################################
# Special debug flags
#
ESMF_F90OPTFLAG_G       += -Nquickdbg 
ESMF_CXXOPTFLAG_G       += -Nquickdbg

############################################################
# Enable TR15581/F2003 Allocatable array resizing
#
ESMF_F90COMPILEOPTS += -Nalloc_assign

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
ESMF_OPENMP_F90COMPILEOPTS += -Kopenmp
ESMF_OPENMP_F90LINKOPTS    += -Kopenmp
ESMF_OPENMP_CXXCOMPILEOPTS += -Kopenmp
ESMF_OPENMP_CXXLINKOPTS    += -Kopenmp

############################################################
# OpenACC compiler and linker flags
#
ESMF_OPENACCDEFAULT = OFF
ESMF_OPENACC_F90COMPILEOPTS += -fopenacc
ESMF_OPENACC_CXXCOMPILEOPTS += -fopenacc
ESMF_OPENACC_F90LINKOPTS    += -fopenacc
ESMF_OPENACC_CXXLINKOPTS    += -fopenacc

############################################################
# MKL specific options for external LAPACK
#
ifeq ($(ESMF_LAPACK),ssl2)
ifndef ESMF_LAPACK_LIBS
ESMF_LAPACK_LIBS = -SSL2
endif
endif

############################################################
# Set rpath syntax
#
ESMF_F90RPATHPREFIX         = -Wl,-rpath,
ESMF_CXXRPATHPREFIX         = -Wl,-rpath,
ESMF_CRPATHPREFIX           = -Wl,-rpath,

############################################################
# Determine where frt's libraries are located
#
ESMF_CXXLINKPATHS +=

############################################################
# Determine where FCC's libraries are located
#
ESMF_F90LINKPATHS +=

############################################################
# Link against libesmf.a using the F90 linker front-end
#
ESMF_F90LINKLIBS += --linkstl=libstdc++

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ESMF_CXXLINKLIBS += --linkfortran

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
ifeq ($(ESMF_SHARED_LIB_BUILD),OFF)
ESMF_TRACE_LIB_BUILD = OFF
endif
