# $Id: build_rules.mk,v 1.32.2.1 2008/07/16 00:19:14 theurich Exp $
#
# OSF1.default.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = f90
ESMF_CXXDEFAULT         = cxx

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
ESMF_F90LINKLIBS       += -lmpi
ESMF_CXXLINKLIBS       += -lmpi
ESMF_MPIRUNDEFAULT      = mpirun.lsf.rms
ESMF_MPIMPMDRUNDEFAULT  = mpimpmdrun.alpha
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
ESMF_CXXCOMPILER_VERSION    = ${ESMF_CXXCOMPILER} -V

############################################################
# Conditionally add pthread compiler and linker flags
#
ifeq ($(ESMF_PTHREADS),ON)
ESMF_F90COMPILEOPTS += -pthread -reentrancy threaded
ESMF_F90LINKOPTS    += -pthread -reentrancy threaded
ESMF_CXXCOMPILEOPTS += -pthread
ESMF_CXXLINKOPTS    += -pthread
endif

############################################################
# Special debug flags
#
ESMF_F90OPTFLAG_G       += -assume gfullpath
ESMF_CXXOPTFLAG_G       += -assume gfullpath

############################################################
# Special optimization flags
#
ESMF_F90OPTFLAG_O       += -w
ESMF_CXXOPTFLAG_O       += -w

############################################################
# Help f90 to understand Fortran suffices
#
ESMF_F90COMPILEFREECPP   = -free -cpp
ESMF_F90COMPILEFREENOCPP = -free
ESMF_F90COMPILEFIXCPP    = -cpp -extend_source
ESMF_F90COMPILEFIXNOCPP  = -extend_source

############################################################
# Prefix for rpath option
#
ESMF_RPATHPREFIX      = -rpath 

############################################################
# Determine where f90's libraries are located
#
ESMF_CXXLINKPATHS += $(addprefix -L, $(shell $(ESMF_DIR)/scripts/libpath.compaqf90 $(ESMF_F90COMPILER)))

############################################################
# Link against libesmf.a using the F90 linker front-end
#
ESMF_F90LINKLIBS += -lcxx -lrt -lm

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ESMF_CXXLINKLIBS += -lfor -lrt -lm

############################################################
# Shared library options
#
ESMF_SL_LIBOPTS  += -shared $(ESMF_CXXCOMPILEOPTS)
ESMF_SL_LIBLIBS  += $(ESMF_CXXLINKPATHS) $(ESMF_CXXLINKLIBS)
