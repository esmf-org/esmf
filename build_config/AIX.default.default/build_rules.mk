# $Id$
#
# AIX.default.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = xlf90_r
ESMF_CXXDEFAULT         = xlC_r

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
ESMF_F90COMPILECPPFLAGS+= -WF,-DESMF_MPIUNI
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_MPIUNI
ESMF_CXXCOMPILEPATHS   += -I$(ESMF_DIR)/src/Infrastructure/stubs/mpiuni
ESMF_MPIRUNDEFAULT      = $(ESMF_DIR)/src/Infrastructure/stubs/mpiuni/mpirun
else
ifeq ($(ESMF_COMM),mpi)
# Vendor MPI -----------------------------------------------
ESMF_F90DEFAULT         = mpxlf90_r
ESMF_F90LINKLIBS       += -lmpi_r
ESMF_CXXDEFAULT         = mpCC_r
ESMF_CXXLINKLIBS       += -lmpi_r
ESMF_MPIRUNDEFAULT      = mpirun.pc604
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
ESMF_F90COMPILER_VERSION    = lslpp -l | fgrep xlf
ESMF_CXXCOMPILER_VERSION    = lslpp -l | fgrep C++

############################################################
# 32- vs. 64-bit ABI
#
ifeq ($(ESMF_ABI),32)
ESMF_CXXCOMPILEOPTS       += -q32
ESMF_CXXLINKOPTS          += -q32
ESMF_F90COMPILEOPTS       += -q32
ESMF_F90LINKOPTS          += -q32
ESMF_ARDEFAULT             = ar -X32
ESMF_RANLIBDEFAULT         = ranlib -X32
endif
ifeq ($(ESMF_ABI),64)
ESMF_CXXCOMPILEOPTS       += -q64
ESMF_CXXLINKOPTS          += -q64
ESMF_F90COMPILEOPTS       += -q64
ESMF_F90LINKOPTS          += -q64
ESMF_ARDEFAULT             = ar -X64
ESMF_RANLIBDEFAULT         = ranlib -X64
endif

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
# OpenMP compiler and linker flags
#
ESMF_OPENMP_F90COMPILEOPTS += -qsmp=omp
ESMF_OPENMP_CXXCOMPILEOPTS += -qsmp=omp
ESMF_OPENMP_F90LINKOPTS    += -qsmp=omp
ESMF_OPENMP_CXXLINKOPTS    += -qsmp=omp
# Library build slows down to a crawl on bluefire with OpenMP enabled for lib
ESMF_OPENMP := OFF

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
# Link against libesmf.a using the F90 linker front-end
#
ESMF_F90LINKLIBS += -lxlf90_r -lC_r

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ESMF_CXXLINKLIBS += -lm_r -lxlf90_r -lC_r

############################################################
# Shared library options
#
ESMF_SL_LIBOPTS  += -Wl,-bbigtoc
ESMF_SL_LIBOPTS  += -G -qmkshrobj -lf
ifeq ($(ESMF_ABI),64)
ESMF_SL_LIBOPTS  += -q64
endif
ESMF_SO_F90LINKOPTSEXE = -bdynamic -brtl
ESMF_SO_CXXLINKOPTSEXE = -bdynamic -brtl

############################################################
# Add compile options required by external libraries
ifneq ($(ESMF_YAMLCPP),)
ESMF_CXXCOMPILEOPTS  += -qlanglvl=c++11
endif
