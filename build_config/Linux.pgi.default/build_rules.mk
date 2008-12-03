# $Id: build_rules.mk,v 1.29.2.11 2008/12/03 00:35:51 theurich Exp $
#
# Linux.pgi.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = pgf90
ESMF_CXXDEFAULT         = pgCC

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
ifeq ($(ESMF_COMM),mpich)
# Mpich ----------------------------------------------------
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_MPICH
ESMF_F90DEFAULT         = mpif90
ESMF_F90LINKLIBS       += -lpmpich++ -lmpich
ESMF_CXXDEFAULT         = mpiCC
ESMF_CXXLINKLIBS       += -lmpich
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),mpich2)
# Mpich2 ---------------------------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpicxx
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),scalimpi)
# scaliMPI -------------------------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpic++
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),lam)
# LAM (assumed to be built with pgf90) ---------------------
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_NO_SIGUSR2
ESMF_F90DEFAULT         = mpif77
ESMF_CXXDEFAULT         = mpic++
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),openmpi)
# OpenMPI --------------------------------------------------
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_NO_SIGUSR2
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpicxx
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),user)
# User specified flags -------------------------------------
else
$(error Invalid ESMF_COMM setting: $(ESMF_COMM))
endif
endif
endif
endif
endif
endif
endif

############################################################
# Print compiler version string
#
ESMF_F90COMPILER_VERSION    = $(ESMF_DIR)/scripts/version.pgf90 $(ESMF_F90COMPILER)
ESMF_CXXCOMPILER_VERSION    = $(ESMF_DIR)/scripts/version.pgCC $(ESMF_CXXCOMPILER)

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
ESMF_CXXCOMPILEOPTS       += 
ESMF_CXXLINKOPTS          += 
ESMF_F90COMPILEOPTS       += 
ESMF_F90LINKOPTS          += 
endif
ifeq ($(ESMF_ABISTRING),x86_64_small)
ESMF_CXXCOMPILEOPTS       += -mcmodel=small
ESMF_CXXLINKOPTS          += -mcmodel=small
ESMF_F90COMPILEOPTS       += -mcmodel=small
ESMF_F90LINKOPTS          += -mcmodel=small
endif
ifeq ($(ESMF_ABISTRING),x86_64_medium)
ESMF_CXXCOMPILEOPTS       += -mcmodel=medium
ESMF_CXXLINKOPTS          += -mcmodel=medium
ESMF_F90COMPILEOPTS       += -mcmodel=medium
ESMF_F90LINKOPTS          += -mcmodel=medium
endif

############################################################
# Need this until the file convention is fixed (then remove these two lines)
#
ESMF_F90COMPILEFREENOCPP = -Mfreeform
ESMF_F90COMPILEFIXCPP    = -Mpreprocess -Mnofreeform

############################################################
# Determine where pgf90's libraries are located
#
ESMF_CXXLINKPATHS += -L$(shell $(ESMF_DIR)/scripts/libpath.pgf90 $(ESMF_F90COMPILER))
ESMF_CXXLINKRPATHS += $(ESMF_RPATHPREFIX)$(shell $(ESMF_DIR)/scripts/libpath.pgf90 $(ESMF_F90COMPILER))

############################################################
# Determine where pgCC's libraries are located
#
ESMF_F90LINKPATHS += -L$(shell $(ESMF_DIR)/scripts/libpath.pgCC $(ESMF_CXXCOMPILER))
ESMF_F90LINKRPATHS += $(ESMF_RPATHPREFIX)$(shell $(ESMF_DIR)/scripts/libpath.pgCC $(ESMF_CXXCOMPILER))

############################################################
# Link against libesmf.a using the F90 linker front-end
#
ESMF_F90LINKLIBS += -lrt -lstd -lC $(shell $(ESMF_DIR)/scripts/libs.pgCC $(ESMF_CXXCOMPILER))

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ESMF_CXXLINKLIBS += -lrt $(shell $(ESMF_DIR)/scripts/libs.pgf90 $(ESMF_F90COMPILER))

############################################################
# Blank out shared library options
#
ESMF_SL_LIBS_TO_MAKE  =
