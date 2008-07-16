# $Id: build_rules.mk,v 1.31.2.1 2008/07/16 00:19:10 theurich Exp $
# 
# IRIX64.default.default
#

############################################################
# Default compiler setting.
#
ESMF_AR                 = CC -ar
ESMF_AREXTRACT          = ar -x 
ESMF_ARCREATEFLAGS      = -o 
ESMF_F90DEFAULT         = f90
ESMF_F90LINKERDEFAULT   = CC
ESMF_CXXDEFAULT         = CC
ESMF_CXXLINKLIBS       += -lC

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
ESMF_F90LINKLIBS       += -lmpi++ -lmpi
ESMF_CXXLINKLIBS       += -lmpi++ -lmpi
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
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
ESMF_F90COMPILER_VERSION    = ${ESMF_F90COMPILER} -version
ESMF_CXXCOMPILER_VERSION    = ${ESMF_CXXCOMPILER} -version

############################################################
# 32- vs. 64-bit ABI
#
ifeq ($(ESMF_ABI),32)
ESMF_CXXCOMPILEOPTS       += -n32
ESMF_CXXLINKOPTS          += -n32
ESMF_F90COMPILEOPTS       += -n32
ESMF_F90LINKOPTS          += -n32
endif
ifeq ($(ESMF_ABI),64)
ESMF_CXXCOMPILEOPTS       += -64
ESMF_CXXLINKOPTS          += -64
ESMF_F90COMPILEOPTS       += -64
ESMF_F90LINKOPTS          += -64
endif

############################################################
# OpenMP on IRIX64 is incompatible with Pthreads thus turn off by default
#
ESMF_PTHREADSDEFAULT       = OFF

############################################################
# Special compiler flags
#
# abide to C++ language standard; don't put libc into std name space
ESMF_CXXCOMPILEOPTS       += -LANG:std=on
#
# allow for multi-processor code (important for shared objects!)
ESMF_CXXCOMPILEOPTS       += -mp
ESMF_CXXLINKOPTS          += -mp
ESMF_F90COMPILEOPTS       += -mp
ESMF_F90LINKOPTS          += -mp

############################################################
# Conditionally add pthread compiler and linker flags
#
ifeq ($(ESMF_PTHREADS),ON)
ESMF_F90LINKLIBS       += -lpthread
ESMF_CXXLINKLIBS       += -lpthread
endif

############################################################
# Special optimization flags
#
ESMF_CXXOPTFLAG_O       += -OPT:Olimit=6500
ESMF_F90OPTFLAG_O       += -OPT:Olimit=6500

############################################################
# Help f90 to understand Fortran suffices
#
ESMF_F90COMPILEFREECPP   = -freeform -cpp
ESMF_F90COMPILEFREENOCPP = -freeform -nocpp
ESMF_F90COMPILEFIXCPP    = -fixedform -cpp -extend_source
ESMF_F90COMPILEFIXNOCPP  = -fixedform -nocpp -extend_source

############################################################
# Prefix for rpath option
#
ESMF_RPATHPREFIX      = -rpath 

############################################################
# IRIX64 does not have a ranlib -> "true" is a noop command
#
ESMF_RANLIBDEFAULT         = true

############################################################
# Link against libesmf.a using the F90 linker front-end
#
ESMF_F90LINKLIBS += -lftn -lfortran -lffio -lm

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ESMF_CXXLINKLIBS += -lftn -lfortran -lffio -lm

############################################################
# Shared library options
#
ESMF_SL_LIBOPTS  += -shared $(ESMF_CXXCOMPILEOPTS)
ESMF_SL_LIBLIBS  += $(ESMF_CXXLINKPATHS) $(ESMF_CXXLINKLIBS)
