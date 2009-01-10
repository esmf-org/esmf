# $Id: build_rules.mk,v 1.3 2009/01/10 01:41:07 w6ws Exp $
#
# MinGW.intel.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = ifort
ESMF_F90COMPILEOPTS    += -unix -Qfpp
ESMF_CXXDEFAULT         = icl
ESMF_CXXCOMPILEOPTS    += -TP -GX

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
ifeq ($(ESMF_COMM),msmpi)
# Microsofts version of MPICH2 on CCS 2003 is generally at:
# ESMF_MSMPIDIR        = /c/"Program Files"/"Microsoft Compute Cluster Pack"
# and on HPC 2008:
# ESMF_MSMPIDIR        = /c/"Program Files"/"Microsoft HPC Pack 2008 SDK"
ESMF_CXXCOMPILECPPFLAGS+= -D__int64="long long"
ESMF_F90COMPILEPATHS   += -I$(ESMF_MSMPIDIR)/Include
ESMF_CXXCOMPILEPATHS   += -I$(ESMF_MSMPIDIR)/Include
ESMF_F90LINKLIBS       += $(ESMF_MSMPIDIR)/Lib/i386/msmpi.lib
ESMF_CXXLINKLIBS       += $(ESMF_MSMPIDIR)/Lib/i386/msmpi.lib
ESMF_MPIRUNDEFAULT      = mpiexec $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
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
# Force Fortran symbols lower case
#
ESMF_F90COMPILEOPTS += -names:lowercase

############################################################
# Windows does not have support for POSIX IPC (memory mapped files)
#
ESMF_CXXCOMPILECPPFLAGS += -DESMF_NO_POSIXIPC

############################################################
# Windows does not have support for "gethostid()"
#
ESMF_CXXCOMPILECPPFLAGS += -DESMF_NO_GETHOSTID

############################################################
# Windows does not have support for signals
#
ESMF_CXXCOMPILECPPFLAGS += -DESMF_NO_SIGNALS

############################################################
# Windows does not have support for system(3c) call
#
ESMF_CXXCOMPILECPPFLAGS += -DESMF_NO_SYSTEMCALL

############################################################
# Windows does not have support for the times system call
#
ESMF_CXXCOMPILECPPFLAGS += -DNO_TIMES

############################################################
# Windows does not have support for Pthreads
#
ESMF_PTHREADS := OFF

############################################################
# Fortran symbol convention
#
#ifeq ($(ESMF_FORTRANSYMBOLS),default)
#ESMF_F90COMPILEOPTS       += -fno-second-underscore
#ESMF_F90LINKOPTS          += -fno-second-underscore
#ESMF_CXXCOMPILEOPTS       += -DESMF_LOWERCASE_SINGLEUNDERSCORE
#else
#ifeq ($(ESMF_FORTRANSYMBOLS),lowercase_singleunderscore)
#ESMF_F90COMPILEOPTS       += -fno-second-underscore
#ESMF_F90LINKOPTS          += -fno-second-underscore
#ESMF_CXXCOMPILEOPTS       += -DESMF_LOWERCASE_SINGLEUNDERSCORE
#else
#ifeq ($(ESMF_FORTRANSYMBOLS),lowercase_doubleunderscore)
#ESMF_F90COMPILEOPTS       +=
#ESMF_F90LINKOPTS          +=
#ESMF_CXXCOMPILEOPTS       += -DESMF_LOWERCASE_DOUBLEUNDERSCORE
#else
#$(error "ESMF_FORTRANSYMBOLS = $(ESMF_FORTRANSYMBOLS)" not supported by ESMF and/or this platform)
#endif
#endif
#endif

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
ifeq ($(ESMF_ABISTRING),x86_64_medium)
ESMF_F90COMPILEOPTS     += -mcmodel=medium
ESMF_F90LINKOPTS        += -mcmodel=medium
ESMF_CXXCOMPILEOPTS     += -mcmodel=medium
ESMF_CXXLINKOPTS        += -mcmodel=medium
endif
ifeq ($(ESMF_ABISTRING),ia64_64)
ESMF_CXXCOMPILEOPTS       += -size_lp64
ESMF_CXXLINKOPTS          += -size_lp64
ESMF_F90COMPILEOPTS       += -size_lp64
ESMF_F90LINKOPTS          += -size_lp64
endif

############################################################
# Need this until the file convention is fixed (then remove these two lines)
#
ESMF_F90COMPILEFREENOCPP = -fpp0 -FR
ESMF_F90COMPILEFIXCPP    = -fpp

###########################################################
# Windows does not have a ranlib command -> "true" is a noop command
#
ESMF_RANLIBDEFAULT       = true

###########################################################
# Determine where gcc's libraries are located
#
# ESMF_F90LINKPATHS += \
#  -L$(dir $(shell $(ESMF_CXXCOMPILER) -print-file-name=libstdc++.a))
# ESMF_F90LINKRPATHS += \
#  -Wl,-rpath,$(dir $(shell $(ESMF_CXXCOMPILER) -print-file-name=libstdc++.a))

############################################################
# Determine where gfortran's libraries are located
#
# ESMF_CXXLINKPATHS += \
#   -L$(dir $(shell $(ESMF_DIR)/scripts/libpath.ifort $(ESMF_F90COMPILER)))
# ESMF_CXXLINKRPATHS += \
#   -Wl,-rpath,$(dir $(shell $(ESMF_DIR)/scripts/libpath.ifort $(ESMF_F90COMPILER)))

############################################################
# Link against libesmf.a using the F90 linker front-end
#
# ESMF_F90LINKLIBS += -lstdc++

############################################################
# Link against libesmf.a using the C++ linker front-end
#
# ESMF_CXXLINKLIBS += $(shell $(ESMF_DIR)/scripts/libs.ifort "$(ESMF_F90COMPILER) $(ESMF_F90COMPILEOPTS)")

############################################################
# Blank out shared library options
#
ESMF_SL_LIBS_TO_MAKE  =
