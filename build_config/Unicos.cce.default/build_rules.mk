# $Id$
#
# Unicos.cce.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = ftn
ESMF_CXXDEFAULT         = CC
ESMF_CDEFAULT           = cc

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
ESMF_CXXCOMPILER_VERSION    = ${ESMF_CXXCOMPILER} --version
ESMF_CCOMPILER_VERSION      = ${ESMF_CCOMPILER} --version

############################################################
# Cray Fortran compiler still needs a numerical opt level default
#
ifeq ($(ESMF_BOPT),O)
ESMF_OPTLEVELDEFAULT  = 2
endif

############################################################
# Disable POSIX IPC (memory mapped files) support on Cray XC
#
ESMF_CXXCOMPILECPPFLAGS += -DESMF_NO_POSIXIPC

############################################################
# OpenMP compiler and linker flags
#
ESMF_F90COMPILEOPTS += -homp
ESMF_CXXCOMPILEOPTS += -fopenmp
ESMF_F90LINKOPTS    += -homp
ESMF_CXXLINKOPTS    += -fopenmp

############################################################
# OpenACC compiler and linker flags
#
ESMF_OPENACCDEFAULT = OFF
ESMF_OPENACC_F90COMPILEOPTS += -hacc
ESMF_OPENACC_CXXCOMPILEOPTS += -hacc
ESMF_OPENACC_F90LINKOPTS    += -hacc
ESMF_OPENACC_CXXLINKOPTS    += -hacc

############################################################
# How to specify module directories
#
ESMF_F90IMOD        = -I

############################################################
# Help ftn to understand Fortran suffices
#
ESMF_F90COMPILEFREECPP   = -f free -N 255 -F
ESMF_F90COMPILEFREENOCPP = -f free -N 255
ESMF_F90COMPILEFIXCPP    = -f fixed -N 132 -F
ESMF_F90COMPILEFIXNOCPP  = -f fixed -N 132

############################################################
# Set rpath syntax
#
ESMF_F90RPATHPREFIX         = -Wl,-rpath,
ESMF_CXXRPATHPREFIX         = -Wl,-rpath,
ESMF_CRPATHPREFIX           = -Wl,-rpath,

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
