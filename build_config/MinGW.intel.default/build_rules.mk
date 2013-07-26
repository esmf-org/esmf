# $Id$
#
# MinGW.intel.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = ifort
ESMF_F90COMPILEOPTS    += -fpp -heap-arrays:1000
ESMF_CXXDEFAULT         = icl

# Turn off some noise
ESMF_CXXCOMPILEOPTS    += -nologo
ESMF_F90COMPILEOPTS    += -nologo
# Specify that .C files are c++
ESMF_CXXCOMPILEOPTS    += -TP
# Specify the c++ exception model
ESMF_CXXCOMPILEOPTS    += -EHsc
# Specify that C99 VLAs are ok
ESMF_CXXCOMPILEOPTS    += -Qvla
# Eliminate warnings about using _s variants of library functions
ESMF_CXXCOMPILEOPTS    += -D_CRT_SECURE_NO_WARNINGS

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
ESMF_MSMPIDIR        = /c/"Program Files"/"Microsoft Compute Cluster Pack"
ESMF_MSMPIDIRW       = c:/"Program Files"/"Microsoft Compute Cluster Pack"
# and on HPC 2008:
# ESMF_MSMPIDIR        = /c/"Program Files"/"Microsoft HPC Pack 2008 SDK"
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
ESMF_F90COMPILER_VERSION    = ${ESMF_F90COMPILER} -logo
ESMF_CXXCOMPILER_VERSION    = ${ESMF_CXXCOMPILER}

############################################################
# Force Fortran symbols lower case
#
ESMF_F90COMPILEOPTS += -names:lowercase

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
ifeq ($(ESMF_ABISTRING),x86_64_32)
ESMF_F90COMPILEOPTS     += -m32
ESMF_F90LINKOPTS        += -m32
ESMF_CXXCOMPILEOPTS     += -m32
ESMF_CXXLINKOPTS        += -m32
endif
ifeq ($(ESMF_ABISTRING),x86_64_small)
ESMF_F90COMPILEOPTS     += -m64 -mcmodel=small
ESMF_F90LINKOPTS        += -m64 -mcmodel=small
ESMF_CXXCOMPILEOPTS     += -m64 -mcmodel=small
ESMF_CXXLINKOPTS        += -m64 -mcmodel=small
endif
ifeq ($(ESMF_ABISTRING),x86_64_medium)
ESMF_F90COMPILEOPTS     += -mcmodel=medium
ESMF_F90LINKOPTS        += -mcmodel=medium
ESMF_CXXCOMPILEOPTS     += -mcmodel=medium
ESMF_CXXLINKOPTS        += -mcmodel=medium
endif

############################################################
# Need this until the file convention is fixed (then remove these two lines)
#
ESMF_F90COMPILEFREENOCPP = -fpp0 -FR
ESMF_F90COMPILEFIXCPP    = -fpp

###########################################################
# Special debugging flags
#
ESMF_F90OPTFLAG_G           = -Od -debug -traceback
ESMF_CXXOPTFLAG_G           = -Od -debug -traceback

###########################################################
# Default optlevel
#
ESMF_OPTLEVELDEFAULT        = 2

###########################################################
# Change default tools, "true" is a noop executable
#
ESMF_ARDEFAULT              = lib
ESMF_ARCREATEFLAGSDEFAULT   =
ESMF_ARCREATEPREFIX         = -OUT:
ESMF_AREXTRACTDEFAULT       = $(ESMF_ARDEFAULT) -extract:
ESMF_RANLIBDEFAULT          = true

############################################################
# Blank out variables to prevent rpath encoding
#
ESMF_F90LINKRPATHS      =
ESMF_CXXLINKRPATHS      =

###########################################################
# OpenMP compiler and linker flags
#
ESMF_OPENMP_F90COMPILEOPTS += -Qopenmp
ESMF_OPENMP_CXXCOMPILEOPTS += -Qopenmp
ESMF_OPENMP_F90LINKOPTS    += -Qopenmp
ESMF_OPENMP_CXXLINKOPTS    += -Qopenmp

###########################################################
# Determine where libraries are located
#
ESMF_F90LINKPATHS =

############################################################
# Determine where libraries are located
#
ESMF_CXXLINKPATHS =

############################################################
# Link against libesmf.lib using the F90 linker front-end
#
ESMF_F90LINKLIBS += -link -debug -libpath:`$(ESMF_DIR)/scripts/path_mingw2win $(ESMF_LDIR)` $(ESMF_MPILIBPATH) 
ESMF_F90ESMFLINKLIBS = libesmf.lib $(ESMF_F90LINKLIBS)

############################################################
# Link against libesmf.lib using the C++ linker front-end
#
ESMF_CXXLINKLIBS += -link -debug -libpath:`$(ESMF_DIR)/scripts/path_mingw2win $(ESMF_LDIR)` $(ESMF_MPILIBPATH)
ESMF_CXXESMFLINKLIBS = libesmf.lib $(ESMF_CXXLINKLIBS)

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

