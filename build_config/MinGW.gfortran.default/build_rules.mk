# $Id: $
#
# MinGW.gfortran.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = gfortran
ESMF_CXXDEFAULT         = g++

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
# ESMF_MSMPIDIRW       = c:/"Program Files"/"Microsoft Compute Cluster Pack"
# and on HPC 2008:
ESMF_MSMPIDIR		= /c/"Program Files"/"Microsoft HPC Pack 2008 SDK"
ESMF_MSMPIDIRW		= c:/"Program Files"/"Microsoft HPC Pack 2008 SDK"
ESMF_CXXCOMPILECPPFLAGS+= -D__int64="long long"
ESMF_F90COMPILEPATHS   += -I$(ESMF_MSMPIDIR)/Include
ESMF_CXXCOMPILEPATHS   += -I$(ESMF_MSMPIDIR)/Include
ESMF_MPILIBPATH          = -libpath:$(ESMF_MSMPIDIRW)/Lib/amd64
ESMF_F90LINKLIBS       += Ws2_32.lib msmpi.lib
ESMF_CXXLINKLIBS       += Ws2_32.lib msmpi.lib
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
ESMF_F90COMPILER_VERSION    = ${ESMF_F90COMPILER} -v --version
ESMF_CXXCOMPILER_VERSION    = ${ESMF_CXXCOMPILER} -v --version

############################################################
# Special debug flags
#
ESMF_F90OPTFLAG_G       += -Wall -Wno-unused -Wno-unused-dummy-argument -fbacktrace -fbounds-check
ESMF_CXXOPTFLAG_G       += -Wall -Wextra -Wno-unused

############################################################
# Fortran symbol convention
#
ifeq ($(ESMF_FORTRANSYMBOLS),default)
ESMF_F90COMPILEOPTS       += -fno-second-underscore
ESMF_F90LINKOPTS          += -fno-second-underscore
ESMF_CXXCOMPILEOPTS       += -DESMF_LOWERCASE_SINGLEUNDERSCORE
else
ifeq ($(ESMF_FORTRANSYMBOLS),lowercase_singleunderscore)
ESMF_F90COMPILEOPTS       += -fno-second-underscore
ESMF_F90LINKOPTS          += -fno-second-underscore
ESMF_CXXCOMPILEOPTS       += -DESMF_LOWERCASE_SINGLEUNDERSCORE
else
ifeq ($(ESMF_FORTRANSYMBOLS),lowercase_doubleunderscore)
ESMF_F90COMPILEOPTS       +=
ESMF_F90LINKOPTS          +=
ESMF_CXXCOMPILEOPTS       += -DESMF_LOWERCASE_DOUBLEUNDERSCORE
else
$(error "ESMF_FORTRANSYMBOLS = $(ESMF_FORTRANSYMBOLS)" not supported by ESMF and/or this platform)
endif
endif
endif

############################################################
# Construct the ABISTRING
#
ifeq ($(ESMF_MACHINE),i686)
ESMF_ABISTRING := x86_64_small
ifeq ($(ESMF_ABI),32)
ESMF_ABISTRING := x86_64_32
endif
ifeq ($(ESMF_ABI),64)
ESMF_ABISTRING := x86_64_small
endif
else
$(error "ESMF_MACHINE = $(ESMF_MACHINE)" not recognized)
endif

############################################################
# Set memory model compiler flags according to ABISTRING
#
ifeq ($(ESMF_ABISTRING),x86_64_32)
ESMF_CXXCOMPILEOPTS       += -m32
ESMF_CXXLINKOPTS          += -m32
ESMF_F90COMPILEOPTS       += -m32
ESMF_F90LINKOPTS          += -m32
endif
ifeq ($(ESMF_ABISTRING),x86_64_small)
ESMF_CXXCOMPILEOPTS       += -m64 -mcmodel=small
ESMF_CXXLINKOPTS          += -m64 -mcmodel=small
ESMF_F90COMPILEOPTS       += -m64 -mcmodel=small
ESMF_F90LINKOPTS          += -m64 -mcmodel=small
endif
ifeq ($(ESMF_ABISTRING),x86_64_medium)
ESMF_CXXCOMPILEOPTS       += -m64 -mcmodel=medium
ESMF_CXXLINKOPTS          += -m64 -mcmodel=medium
ESMF_F90COMPILEOPTS       += -m64 -mcmodel=medium
ESMF_F90LINKOPTS          += -m64 -mcmodel=medium
endif

############################################################
# MinGW gcc does not have support for OpenMP
#
ESMF_OPENMP := OFF

############################################################
# Windows does not have support for POSIX IPC (memory mapped files)
#
ESMF_CXXCOMPILECPPFLAGS += -DESMF_NO_POSIXIPC

############################################################
# Windows does not have support for POSIX dynamic linking
#
ESMF_CXXCOMPILECPPFLAGS += -DESMF_NO_DLFCN

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
# Need this until the file convention is fixed (then remove these two lines)
#
ESMF_F90COMPILEFREENOCPP = -ffree-form
ESMF_F90COMPILEFIXCPP    = -cpp -ffixed-form

############################################################
# Set unlimited line length limit for free format files
#
ESMF_F90COMPILEOPTS += -ffree-line-length-none

############################################################
# Set rpath syntax
#
ESMF_F90RPATHPREFIX         = -Wl,-rpath,
ESMF_CXXRPATHPREFIX         = -Wl,-rpath,

############################################################
# Determine where gcc's libraries are located
#
ESMF_LIBSTDCXX := $(shell $(ESMF_CXXCOMPILER) $(ESMF_CXXCOMPILEOPTS) -print-file-name=libstdc++.so)
ifeq ($(ESMF_LIBSTDCXX),libstdc++.so)
ESMF_LIBSTDCXX := $(shell $(ESMF_CXXCOMPILER) $(ESMF_CXXCOMPILEOPTS) -print-file-name=libstdc++.a)
endif
ESMF_F90LINKPATHS += -L$(dir $(ESMF_LIBSTDCXX))
ESMF_F90LINKRPATHS += $(ESMF_F90RPATHPREFIX)$(dir $(ESMF_LIBSTDCXX))

############################################################
# Determine where gfortran's libraries are located
#
ESMF_LIBGFORTRAN := $(shell $(ESMF_F90COMPILER) $(ESMF_F90COMPILEOPTS) -print-file-name=libgfortran.so)
ifeq ($(ESMF_LIBSTDCXX),libgfortran.so)
ESMF_LIBGFORTRAN := $(shell $(ESMF_F90COMPILER) $(ESMF_F90COMPILEOPTS) -print-file-name=libgfortran.a)
endif
ESMF_CXXLINKPATHS += -L$(dir $(ESMF_LIBGFORTRAN))
ESMF_CXXLINKRPATHS += $(ESMF_CXXRPATHPREFIX)$(dir $(ESMF_LIBGFORTRAN))

############################################################
# Link against libesmf.a using the F90 linker front-end
#
ESMF_F90LINKLIBS += -lstdc++ -lWs2_32

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ESMF_CXXLINKLIBS += -lgfortran -lWs2_32

############################################################
# Shared library options
#
ESMF_SL_LIBOPTS  += -shared
ESMF_SL_LIBLIBS       += $(ESMF_CXXLINKPATHS) $(ESMF_CXXLINKLIBS) -lgfortran
