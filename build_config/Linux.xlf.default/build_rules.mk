# $Id: build_rules.mk,v 1.13.2.1 2008/07/16 00:19:13 theurich Exp $
#
# Linux.xlf.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = blrts_xlf90
ESMF_CXXDEFAULT         = blrts_xlC

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
ESMF_F90DEFAULT         = mpxlf90
ESMF_CXXDEFAULT         = mpxlC
ESMF_MPIRUNDEFAULT      = mpirun.cqsub
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
ESMF_F90COMPILER_VERSION = ${ESMF_F90COMPILER} -qversion
ESMF_CXXCOMPILER_VERSION = ${ESMF_CXXCOMPILER} -qversion

############################################################
# BlueGene needs to link with F90 front end
#
ESMF_CXXLINKERDEFAULT = $(ESMF_F90LINKERDEFAULT)

############################################################
# BlueGene does not have support for POSIX IPC (memory mapped files)
#
ESMF_CXXCOMPILECPPFLAGS += -DESMF_NO_POSIXIPC

############################################################
# BlueGene does not have support for Pthreads
#
ESMF_PTHREADS := OFF

############################################################
# xlf90 needs flag to indicate FPP options
#
ESMF_FPPPREFIX           = -WF,

############################################################
# Special debug flags
#
ESMF_F90OPTFLAG_G       += -qcheck -qfullpath
ESMF_CXXOPTFLAG_G       += -qcheck -qfullpath

############################################################
# Blank out variables to prevent rpath encoding
#
ESMF_F90LINKRPATHS      =
ESMF_CXXLINKRPATHS      =

############################################################
# xlf90 does not know about Fortran suffices
#
ESMF_F90COMPILEFREECPP   = -qfree=f90 -qsuffix=cpp=F90
ESMF_F90COMPILEFREENOCPP = -qfree=f90 -qsuffix=f=f90
ESMF_F90COMPILEFIXCPP    = -qfixed=132 -qsuffix=cpp=F
ESMF_F90COMPILEFIXNOCPP  = -qfixed=132 -qsuffix=f=f

############################################################
# Determine link path for xlf frontend
#
ESMF_F90LINKPATHS +=

############################################################
# Determine link path for xlC frontend
#
ESMF_CXXLINKPATHS +=

############################################################
# Link against libesmf.a using the F90 linker front-end
#
ESMF_F90LINKLIBS +=

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ESMF_CXXLINKLIBS +=

############################################################
# Blank out shared library options
#
ESMF_SL_LIBS_TO_MAKE  =
