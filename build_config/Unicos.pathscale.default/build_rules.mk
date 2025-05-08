# $Id$
#
# Unicos.pathscale.default
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
ESMF_CPPFLAGS          += -DESMF_MPIUNI -I$(ESMF_DIR)/src/Infrastructure/stubs/mpiuni
ESMF_MPIRUNDEFAULT      = $(ESMF_DIR)/src/Infrastructure/stubs/mpiuni/mpirun
else
ifeq ($(ESMF_COMM),mpi)
# Vendor MPI -----------------------------------------------
ESMF_F90LINKLIBS       += 
ESMF_CXXLINKLIBS       += 
ESMF_MPIRUNDEFAULT      = mpirun.unicos
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
ESMF_F90COMPILER_VERSION    = ${ESMF_F90COMPILER} -v -version
ESMF_CXXCOMPILER_VERSION    = ${ESMF_CXXCOMPILER} -v -version
ESMF_CCOMPILER_VERSION      = ${ESMF_CCOMPILER} -V

############################################################
# XT compute nodes do not have support for POSIX IPC (memory mapped files)
#
ESMF_CXXCOMPILECPPFLAGS += -DESMF_NO_POSIXIPC

############################################################
# XT compute nodes do not have support for POSIX dynamic linking
#
ESMF_CXXCOMPILECPPFLAGS += -DESMF_NO_DLFCN

############################################################
# XT compute nodes do not have support for signals
#
ESMF_CXXCOMPILECPPFLAGS += -DESMF_NO_SIGNALS

############################################################
# XT compute nodes do not have support for system call
#
ESMF_CXXCOMPILECPPFLAGS += -DESMF_NO_SYSTEMCALL

############################################################
# XT compute nodes do not have support for Pthreads
#
ESMF_PTHREADS := OFF

############################################################
# XT compute nodes do not have support for OpenMP
#
ESMF_OPENMP := OFF

############################################################
# Explicit flags for handling specific format and cpp combos
#
ESMF_F90COMPILEFREENOCPP = -freeform
ESMF_F90COMPILEFIXCPP    = -fixedform -cpp

############################################################
# Blank out variables to prevent rpath encoding
#
ESMF_F90LINKRPATHS      =
ESMF_CXXLINKRPATHS      =
ESMF_CLINKRPATHS        =

############################################################
# Link against libesmf.a using the F90 linker front-end
#
ESMF_F90LINKLIBS += -lstl -lcxxrt -ldl

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ESMF_CXXLINKLIBS += -lpathfortran

############################################################
# Blank out shared library options
#
ESMF_SL_LIBS_TO_MAKE  =

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
