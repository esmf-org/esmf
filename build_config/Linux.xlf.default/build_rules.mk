# $Id: build_rules.mk,v 1.7 2006/09/22 23:55:40 theurich Exp $
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
# ESMF_F90DEFAULT         = blrts_xlf90
ESMF_F90LINKLIBS       += 
# ESMF_CXXDEFAULT         = blrts_xlC
ESMF_CXXLINKLIBS       += 
ESMF_MPIRUNDEFAULT      = $(ESMF_DIR)/scripts/mpirun.rs6000_sp
ifeq ($(ESMF_BATCH),lsf.ibmpjl)
ESMF_MPIRUNDEFAULT      = $(ESMF_DIR)/scripts/mpirun.lsf.ibmpjl
endif
else
ifeq ($(ESMF_COMM),user)
# User specified flags -------------------------------------
else
$(error Invalid ESMF_COMM setting: $(ESMF_COMM))
endif
endif
endif

############################################################
# Set ESMF_MPIRUNDEFAULT according to ESMF_BATCH setting
#
ifeq ($(ESMF_BATCH),lsf.ibmpjl)
ESMF_MPIRUNDEFAULT      = $(ESMF_DIR)/scripts/mpirun.lsf.ibmpjl
endif

############################################################
# Print compiler version string
#
ESMF_F90COMPILER_VERSION    = ${ESMF_F90COMPILER} -qversion
ESMF_CXXCOMPILER_VERSION    = ${ESMF_CXXCOMPILER} -qversion

############################################################
# 32- vs. 64-bit ABI
#
ifeq ($(ESMF_ABI),32)
ESMF_CXXCOMPILEOPTS       +=  -I/bgl/BlueLight/ppcfloor/bglsys/include
ESMF_CXXLINKOPTS          +=  -L/bgl/BlueLight/ppcfloor/bglsys/lib -lmpich.rts -lmsglayer.rts -lrts.rts -ldevices.rts
ESMF_F90COMPILEOPTS       +=  -I/bgl/BlueLight/ppcfloor/bglsys/include
#ESMF_F90LINKOPTS          +=  -L/bgl/BlueLight/ppcfloor/bglsys/lib -lmpich.rts -lmsglayer.rts -lrts.rts -ldevices.rts
ESMF_F90LINKOPTS          +=  -L/bgl/BlueLight/ppcfloor/bglsys/lib \
                              -L/opt/ibmcmp/vacpp/bg/8.0/blrts_lib -libmc++ -lxlopt -lxl \
                              -lstdc++ -lm -lc -lgcc -lcxxmpich.rts -lmpich.rts \
			      -lmsglayer.rts -lrts.rts -ldevices.rts

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
ESMF_F90LINKLIBS += -L/opt/ibmcmp/vacpp/bg/8.0/lib -libmc++ -lxl -lxlopt

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ESMF_CXXLINKLIBS += -lm -lxlf90 -lC

############################################################
# Shared library options
#
ESMF_SL_LIBOPTS  += -G -qmkshrobj
ifeq ($(ESMF_ABI),64)
ESMF_SL_LIBOPTS  += -q64
endif
