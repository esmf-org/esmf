# $Id$
#
# Unicos.cce.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = ftn
ESMF_F90LINKERDEFAULT   = CC
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
ESMF_F90COMPILER_VERSION    = ${ESMF_F90COMPILER} -V
ESMF_CXXCOMPILER_VERSION    = ${ESMF_CXXCOMPILER} -V

############################################################
# on XT with CCE optimization level must be set explicitely
#
ESMF_OPTLEVELDEFAULT  = 2

############################################################
# XT compute nodes do not have support for POSIX IPC (memory mapped files)
#
ESMF_CXXCOMPILECPPFLAGS += -DESMF_NO_POSIXIPC

############################################################
# XT compute nodes do not have support for POSIX dynamic linking
#
ESMF_CXXCOMPILECPPFLAGS += -DESMF_NO_DLFCN

############################################################
# XT compute nodes do not have support for "gethostid()"
#
ESMF_CXXCOMPILECPPFLAGS += -DESMF_NO_GETHOSTID

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
ESMF_F90COMPILEOPTS += -h noomp
ESMF_CXXCOMPILEOPTS += -h noomp

############################################################
# How to specify module directories
#
ESMF_F90IMOD        = -em -J

############################################################
# Help ftn to understand Fortran suffices
#
ESMF_F90COMPILEFREECPP   = -f free -N 255 -F
ESMF_F90COMPILEFREENOCPP = -f free -N 255
ESMF_F90COMPILEFIXCPP    = -f fixed -N 132 -F
ESMF_F90COMPILEFIXNOCPP  = -f fixed -N 132

############################################################
# Blank out variables to prevent rpath encoding
#
ESMF_F90LINKRPATHS      =
ESMF_CXXLINKRPATHS      =

############################################################
# Special libs to link against
#
ESMF_F90LINKLIBS += -lpgas-dmapp
ESMF_CXXLINKLIBS += -lpgas-dmapp

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
