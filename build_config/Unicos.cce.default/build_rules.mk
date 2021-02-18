# $Id$
#
# Unicos.cce.default
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
ESMF_CXXCOMPILER_VERSION    = ${ESMF_CXXCOMPILER} --version

############################################################
# Cray Fortran compiler still needs a numerical opt level default
#
ESMF_OPTLEVELDEFAULT  = 2

############################################################
# Disable POSIX IPC (memory mapped files) support on Cray XC
#
ESMF_CXXCOMPILECPPFLAGS += -DESMF_NO_POSIXIPC

############################################################
# Disable POSIX dynamic linking support on Cray XC
#
ESMF_CXXCOMPILECPPFLAGS += -DESMF_NO_DLFCN

############################################################
# Disable "gethostid()" support on Cray XC
#
ESMF_CXXCOMPILECPPFLAGS += -DESMF_NO_GETHOSTID

############################################################
# Disable signals support on Cray XC
#
ESMF_CXXCOMPILECPPFLAGS += -DESMF_NO_SIGNALS

############################################################
# Disable system call support on Cray XC
#
ESMF_CXXCOMPILECPPFLAGS += -DESMF_NO_SYSTEMCALL

############################################################
# Disable Pthreads support on Cray XC
#
ESMF_PTHREADS := OFF

############################################################
# OpenMP compiler and linker flags
#
ESMF_F90COMPILEOPTS += -homp
ESMF_CXXCOMPILEOPTS += -fopenmp

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
