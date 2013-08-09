# $Id$
#
# Linux.intel.default
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
ESMF_F90COMPILER_VERSION    = ${ESMF_F90COMPILER} -V -v
ESMF_CXXCOMPILER_VERSION    = ${ESMF_CXXCOMPILER} -V -v

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

############################################################
# Blank out variables to prevent rpath encoding
#
ESMF_F90LINKRPATHS      =
ESMF_CXXLINKRPATHS      =

###########################################################
# Determine where ifort's libraries are located
#
ESMF_CXXLINKPATHS += -L$(dir $(shell $(ESMF_DIR)/scripts/libpath.ifort $(ESMF_F90COMPILER)))

############################################################
# Determine where icpc's libraries are located
#
ESMF_F90LINKPATHS += $(addprefix -L,$(shell $(ESMF_DIR)/scripts/libpath.icpc "$(ESMF_CXXCOMPILER) $(ESMF_CXXCOMPILEOPTS)"))

############################################################
# Link against libesmf.a using the F90 linker front-end
#
ESMF_F90LINKLIBS += $(shell $(ESMF_DIR)/scripts/libs.icpc "$(ESMF_CXXCOMPILER) $(ESMF_CXXCOMPILEOPTS)") -lrt -ldl

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ESMF_CXXLINKLIBS += $(shell $(ESMF_DIR)/scripts/libs.ifort "$(ESMF_F90COMPILER) $(ESMF_F90COMPILEOPTS)") -lrt -ldl

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
