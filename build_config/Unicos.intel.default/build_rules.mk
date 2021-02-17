# $Id$
#
# Unicos.intel.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = ftn
ESMF_CXXDEFAULT         = CC

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
ESMF_F90COMPILECPPFLAGS+= -DESMF_MPIUNI
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_MPIUNI
ESMF_CXXCOMPILEPATHS   += -I$(ESMF_DIR)/src/Infrastructure/stubs/mpiuni
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
ESMF_F90COMPILER_VERSION    = ${ESMF_F90COMPILER} -V
ESMF_CXXCOMPILER_VERSION    = ${ESMF_CXXCOMPILER} -V

############################################################
# Special debug flags
#
ESMF_F90OPTFLAG_G       += -traceback -check arg_temp_created,bounds,format,output_conversion,stack,uninit
ESMF_CXXOPTFLAG_G       += -traceback -Wcheck

############################################################
# Enable TR15581/F2003 Allocatable array resizing
#
ESMF_F90COMPILEOPTS += -assume realloc_lhs

############################################################
# Disable POSIX IPC (memory mapped files) support on Cray XC
#
ESMF_CXXCOMPILECPPFLAGS += -DESMF_NO_POSIXIPC

############################################################
# Conditionally add pthread compiler and linker flags
#
ifeq ($(ESMF_PTHREADS),ON)
ESMF_F90COMPILEOPTS += 
ESMF_CXXCOMPILEOPTS += -pthread
ESMF_F90LINKOPTS    +=
ESMF_CXXLINKOPTS    += -pthread
ESMF_SL_LIBOPTS     += -pthread
endif

############################################################
# OpenMP compiler and linker flags
#
ESMF_OPENMP_F90COMPILEOPTS += -qopenmp
ESMF_OPENMP_CXXCOMPILEOPTS += -qopenmp
ESMF_OPENMP_F90LINKOPTS    += -qopenmp
ESMF_OPENMP_CXXLINKOPTS    += -qopenmp

############################################################
# MKL specific options for external LAPACK
ifeq ($(ESMF_LAPACK),mkl)
ifndef ESMF_LAPACK_LIBS
ESMF_LAPACK_LIBS = -mkl
endif
endif

############################################################
# Set rpath syntax
#
ESMF_F90RPATHPREFIX         = -Wl,-rpath,
ESMF_CXXRPATHPREFIX         = -Wl,-rpath,

###########################################################
# Determine where ifort's libraries are located
#
ESMF_CXXLINKPATHS += -L$(dir $(shell $(ESMF_DIR)/scripts/libpath.ifort $(ESMF_F90COMPILER)))

############################################################
# Determine where icpc's libraries are located
#
ESMF_F90LINKPATHS +=

############################################################
# Link against libesmf.a using the F90 linker front-end
#
ESMF_F90LINKLIBS += -cxxlib -lrt -ldl

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ESMF_CXXLINKLIBS += -lrt -ldl

############################################################
# Linker option that ensures that the specified libraries are 
# used to also resolve symbols needed by other libraries.
#
ESMF_F90LINKOPTS          += -Wl,--no-as-needed
ESMF_CXXLINKOPTS          += -Wl,--no-as-needed

############################################################
# Link executables dynamically
#
ESMF_EXE_F90LINKOPTS          += -dynamic
ESMF_EXE_CXXLINKOPTS          += -dynamic

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

############################################################
# Disable WebService testing for now
#
# TODO: Remove this variable and associated infrastructure as soon as
# TODO: WebService testing is robust enough to work on all systems.
#
ESMF_NOWEBSERVTESTING = TRUE

############################################################
# Override default C preprocessor on this platform
#
ESMF_CPPDEFAULT       = gcc -E -P -x c
