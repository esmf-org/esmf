# $Id: build_rules.mk,v 1.7 2008/04/08 17:18:23 w6ws Exp $
#
# Linux.absoftintel.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = f90
ESMF_CXXDEFAULT         = icpc

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
ESMF_F90LINKLIBS       += 
ESMF_CXXDEFAULT         = mpiCC
ESMF_CXXCOMPILEOPTS    += -DESMF_MPICH
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPIRUNOPTIONS)
else
ifeq ($(ESMF_COMM),mpich2)
# Mpich2 ---------------------------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpicxx
ESMF_MPIRUNDEFAULT      = mpirun
ESMF_MPIMPMDRUNDEFAULT  = mpiexec
else
ifeq ($(ESMF_COMM),lam)
# LAM (assumed to be built with absoft f90) ----------------
ESMF_F90DEFAULT         = mpif77
ESMF_CXXDEFAULT         = mpic++
ESMF_MPIRUNDEFAULT      = mpirun
ESMF_MPIMPMDRUNDEFAULT  = mpiexec
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
ESMF_F90COMPILER_VERSION    = ${ESMF_F90COMPILER} -V
ESMF_CXXCOMPILER_VERSION    = ${ESMF_CXXCOMPILER} -V -v

############################################################
# How to specify module directories
#
ESMF_F90IMOD        = -p

############################################################
# Force Fortran symbols lower case
#
ESMF_F90COMPILEOPTS += -YEXT_NAMES=LCS -YEXT_SFX=_

############################################################
# Conditionally add pthread compiler and linker flags
#
ifeq ($(ESMF_PTHREADS),ON)
ESMF_CXXCOMPILEOPTS +=  -pthread
ESMF_CXXLINKOPTS    += -pthread
endif

############################################################
# Need this until the file convention is fixed (then remove these two lines)
#
ESMF_F90COMPILEFREENOCPP = -ffree
ESMF_F90COMPILEFIXCPP    = -ffixed

############################################################
# Add LAPACK libraries
#
# Use the Absoft version of LAPACK
#
ESMF_LAPACK         = 1
# ESMF_LAPACK_LIBPATH =
ESMF_LAPACK_LIBS    = -llapack -lblas

############################################################
# Determine where absoft f90's libraries are located
#
ESMF_CXXLINKPATHS += $(addprefix -L,$(shell $(ESMF_DIR)/scripts/libpath.absoft $(ESMF_F90COMPILER)))
ESMF_RPATHPREFIXFIXED := $(ESMF_RPATHPREFIX)
ESMF_CXXLINKRPATHS += $(addprefix $(ESMF_RPATHPREFIXFIXED), $(shell $(ESMF_DIR)/scripts/libpath.absoft $(ESMF_F90COMPILER)))

############################################################
# Determine where icpc's libraries are located
#
ESMF_F90LINKPATHS += $(addprefix -L,$(shell $(ESMF_DIR)/scripts/libpath.icpc "$(ESMF_CXXCOMPILER) $(ESMF_CXXCOMPILEOPTS)"))

############################################################
# Link against libesmf.a using the F90 linker front-end
#
ESMF_F90LINKLIBS += $(shell $(ESMF_DIR)/scripts/libs.icpc "$(ESMF_CXXCOMPILER) $(ESMF_CXXCOMPILEOPTS)") -lrt -ldl

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ESMF_CXXLINKLIBS += -lrt -ldl $(shell $(ESMF_DIR)/scripts/libs.absoft $(ESMF_F90COMPILER))

############################################################
# Blank out shared library options
#
ESMF_SL_LIBS_TO_MAKE  =
