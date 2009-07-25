# $Id: build_rules.mk,v 1.23 2009/07/25 04:08:14 theurich Exp $
# 
# SunOS.default.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = f90
ESMF_CXXDEFAULT         = CC
ESMF_F90LINKERDEFAULT   = CC

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
ESMF_F90LINKERDEFAULT   = mpiCC
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
# On SunOS the default C++ library is _not_ standard STL compliant
#
ESMF_CXXCOMPILEOPTS       += -library=stlport4 -template=no%extdef
ESMF_CXXLINKOPTS          += -library=stlport4 -template=no%extdef
ESMF_F90LINKOPTS          += -library=stlport4

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
# OpenMP compiler and linker flags
#
ESMF_OPENMP_F90COMPILEOPTS += -xopenmp
ESMF_OPENMP_F90LINKOPTS    += -xopenmp
ESMF_OPENMP_CXXCOMPILEOPTS += -xopenmp
ESMF_OPENMP_CXXLINKOPTS    += -xopenmp

############################################################
# Blank out variables to prevent rpath encoding
#
ESMF_F90LINKRPATHS      =
ESMF_CXXLINKRPATHS      =

############################################################
# Determine where required libraries are located
#
ESMF_F90LINKPATHS += $(addprefix -L,$(shell $(ESMF_DIR)/scripts/libpath.sunf90 "$(ESMF_F90COMPILER) $(ESMF_F90COMPILEOPTS)"))

############################################################
# Determine where required libraries are located
#
ESMF_CXXLINKPATHS += $(addprefix -L,$(shell $(ESMF_DIR)/scripts/libpath.sunf90 "$(ESMF_F90COMPILER) $(ESMF_F90COMPILEOPTS)"))

############################################################
# Link against libesmf.a using the F90 linker front-end
#
ESMF_F90LINKLIBS += $(shell $(ESMF_DIR)/scripts/libs.sunf90 "$(ESMF_F90COMPILER) $(ESMF_F90COMPILEOPTS)")`

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ESMF_CXXLINKLIBS += $(shell $(ESMF_DIR)/scripts/libs.sunf90 "$(ESMF_F90COMPILER) $(ESMF_F90COMPILEOPTS)")`

############################################################
# Blank out shared library options
#
ESMF_SL_LIBS_TO_MAKE  =
