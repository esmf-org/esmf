# $Id$
#
# Unicos.pgi.default
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
ESMF_F90COMPILER_VERSION    = ${ESMF_F90COMPILER} -V
ESMF_CXXCOMPILER_VERSION    = ${ESMF_CXXCOMPILER} -V

############################################################
# Determine PGI version
#
ESMF_PGIVERSION_MAJOR = $(shell $(ESMF_DIR)/scripts/version.pgi 1 $(ESMF_F90COMPILER_VERSION))
ESMF_F90COMPILECPPFLAGS += -DESMF_PGIVERSION_MAJOR=$(ESMF_PGIVERSION_MAJOR)
ESMF_CXXCOMPILECPPFLAGS += -DESMF_PGIVERSION_MAJOR=$(ESMF_PGIVERSION_MAJOR)

ESMF_PGIVERSION_MINOR = $(shell $(ESMF_DIR)/scripts/version.pgi 2 $(ESMF_F90COMPILER_VERSION))
ESMF_F90COMPILECPPFLAGS += -DESMF_PGIVERSION_MINOR=$(ESMF_PGIVERSION_MINOR)
ESMF_CXXCOMPILECPPFLAGS += -DESMF_PGIVERSION_MINOR=$(ESMF_PGIVERSION_MINOR)

ESMF_PGIVERSION_PATCH = $(shell $(ESMF_DIR)/scripts/version.pgi 3 $(ESMF_F90COMPILER_VERSION))
ESMF_F90COMPILECPPFLAGS += -DESMF_PGIVERSION_PATCH=$(ESMF_PGIVERSION_PATCH)
ESMF_CXXCOMPILECPPFLAGS += -DESMF_PGIVERSION_PATCH=$(ESMF_PGIVERSION_PATCH)

############################################################
# Enable TR15581/F2003 Allocatable array resizing
#
ESMF_F90COMPILEOPTS += -Mallocatable=03

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
# Need this until the file convention is fixed (then remove these two lines)
#
ESMF_F90COMPILEFREENOCPP = -Mfreeform
ESMF_F90COMPILEFIXCPP    = -Mpreprocess -Mnofreeform

############################################################
# Blank out variables to prevent rpath encoding
#
ESMF_F90LINKRPATHS      =
ESMF_CXXLINKRPATHS      =

############################################################
# Determine where pgf90's libraries are located
#gjt: Commented out following line after ORNL support indicated
#gjt: that it is okay to mix different PGI versions for library
#gjt: build and application build.
#ESMF_CXXLINKPATHS += -L$(shell $(ESMF_DIR)/scripts/libpath.pgf90 $(ESMF_F90COMPILER))

############################################################
# Determine where pgCC's libraries are located
#gjt: Commented out following line after ORNL support indicated
#gjt: that it is okay to mix different PGI versions for library
#gjt: build and application build.
#ESMF_F90LINKPATHS += -L$(shell $(ESMF_DIR)/scripts/libpath.pgCC $(ESMF_CXXCOMPILER))

############################################################
# Link against libesmf.a using the F90 linker front-end
#
ifeq ($(ESMF_PGIVERSION_MAJOR),7)
ESMF_F90LINKLIBS += -lstd -lrt -lC -ldl
else
ifeq ($(shell $(ESMF_DIR)/scripts/compiler.pgcxx $(ESMF_CXXCOMPILER)),pgc++)
ESMF_F90LINKLIBS += -pgc++libs -ldl
else
ESMF_F90LINKLIBS += -pgcpplibs
endif
endif

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ifeq ($(ESMF_PGIVERSION_MAJOR),7)
ESMF_CXXLINKLIBS += -lrt -ldl
else
ESMF_CXXLINKLIBS += -pgf90libs
endif

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
