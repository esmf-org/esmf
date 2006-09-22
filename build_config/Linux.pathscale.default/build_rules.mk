# $Id: build_rules.mk,v 1.6 2006/09/22 23:55:39 theurich Exp $
#
# Linux.pathscale.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = pathf95
ESMF_CXXDEFAULT         = pathCC

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
ESMF_F90DEFAULT         = mpif90
ESMF_F90LINKLIBS       += -lpmpich++
ESMF_CXXDEFAULT         = mpiCC
ESMF_CXXCOMPILEOPTS    += -DESMF_MPICH
ESMF_CXXLINKLIBS       += 
ESMF_MPIRUNDEFAULT      = mpirun
ifeq ($(ESMF_BATCH),lsf.ibmpjl)
ESMF_MPIRUNDEFAULT      = $(ESMF_DIR)/scripts/mpirun.lsf.ibmpjl
endif
else
ifeq ($(ESMF_COMM),mpich2)
# Mpich2 ---------------------------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpicxx
ESMF_MPIRUNDEFAULT      = mpirun
ESMF_MPIMPMDRUNDEFAULT  = mpiexec
else
ifeq ($(ESMF_COMM),lam)
# LAM (assumed to be built with pathf95) ---------------------
ESMF_F90DEFAULT         = mpif77
ESMF_CXXDEFAULT         = mpic++
ESMF_MPIRUNDEFAULT      = mpirun
ESMF_MPIMPMDRUNDEFAULT  = mpiexec
ifeq ($(ESMF_BATCH),lsf.ibmpjl)
ESMF_MPIRUNDEFAULT      = $(ESMF_DIR)/scripts/mpirun.lsf.ibmpjl
endif
else
ifeq ($(ESMF_COMM),openmpi)
# OpenMPI --------------------------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpicxx
ESMF_MPIRUNDEFAULT      = mpirun
ESMF_MPIMPMDRUNDEFAULT  = mpiexec
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

############################################################
# Print compiler version string
#
ESMF_F90COMPILER_VERSION    = ${ESMF_F90COMPILER} -version
ESMF_CXXCOMPILER_VERSION    = ${ESMF_CXXCOMPILER} -version

############################################################
# Chose ABI flags
#
ifeq ($(ESMF_ABI),32)
ESMF_CXXCOMPILEOPTS       += -m32
ESMF_CXXLINKOPTS          += -m64
ESMF_F90COMPILEOPTS       += -m64
ESMF_F90LINKOPTS          += -m64
endif
ifeq ($(ESMF_ABI),64)
ESMF_CXXCOMPILEOPTS       += -m64
ESMF_CXXLINKOPTS          += -m64
ESMF_F90COMPILEOPTS       += -m64
ESMF_F90LINKOPTS          += -m64
endif

############################################################
# Need this until the file convention is fixed (then remove these two lines)
#
ESMF_F90COMPILEFREENOCPP = -freeform
ESMF_F90COMPILEFIXCPP    = -fixedform -cpp

############################################################
# Link against libesmf.a using the F90 linker front-end
#
ESMF_F90LINKLIBS += -lrt -lstdc++

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ESMF_CXXLINKLIBS += -lrt -lpathfortran

############################################################
# Blank out shared library options
#
ESMF_SL_LIBS_TO_MAKE  =
