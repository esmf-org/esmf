# $Id: build_rules.mk,v 1.14.6.5 2008/07/16 00:19:14 theurich Exp $
# 
# SunOS.default.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = f90
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
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpiCC
ESMF_CXXLINKERDEFAULT   = mpif90
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
ESMF_F90COMPILER_VERSION    = ${ESMF_F90COMPILER} -V
ESMF_CXXCOMPILER_VERSION    = ${ESMF_CXXCOMPILER} -V

############################################################
# How to specify module directories
#
ESMF_F90IMOD        = -M

############################################################
# Use cpp for Fortran preprocessing
#
ESMF_F90COMPILEOPTS += -xpp=cpp

############################################################
# 32- vs. 64-bit ABI
#
ifeq ($(ESMF_ABI),32)
ESMF_CXXCOMPILEOPTS       += -m32
ESMF_CXXLINKOPTS          += -m32
ESMF_F90COMPILEOPTS       += -m32
ESMF_F90LINKOPTS          += -m32
endif
ifeq ($(ESMF_ABI),64)
ESMF_CXXCOMPILEOPTS       += -m64
ESMF_CXXLINKOPTS          += -m64
ESMF_F90COMPILEOPTS       += -m64
ESMF_F90LINKOPTS          += -m64
endif

############################################################
# Conditionally add pthread compiler and linker flags
#
ifeq ($(ESMF_PTHREADS),ON)
ESMF_CXXCOMPILEOPTS       += -mt
ESMF_CXXLINKOPTS          += -mt
ESMF_F90COMPILEOPTS       += -mt
ESMF_F90LINKOPTS          += -mt
endif

############################################################
# Blank out variables to prevent rpath encoding
#
ESMF_F90LINKRPATHS      =
ESMF_CXXLINKRPATHS      =

############################################################
# Link against libesmf.a using the F90 linker front-end
#
ESMF_F90LINKLIBS += -lCrun -lrt -lCstd

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ESMF_CXXLINKLIBS += -lCrun -lrt -lCstd

############################################################
# Blank out shared library options
#
ESMF_SL_LIBS_TO_MAKE  =
