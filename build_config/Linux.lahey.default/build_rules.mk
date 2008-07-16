# $Id: build_rules.mk,v 1.22.2.6 2008/07/16 00:19:12 theurich Exp $
#
# Linux.lahey.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = lf95
ESMF_F90LINKERDEFAULT   = g++
ESMF_CXXDEFAULT         = g++

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
ESMF_F90LINKERDEFAULT   = mpiCC
ESMF_CXXDEFAULT         = mpiCC
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),mpich2)
# Mpich2 ---------------------------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_F90LINKERDEFAULT   = mpicxx
ESMF_CXXDEFAULT         = mpicxx
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),lam)
# LAM (assumed to be built with lf95) ----------------------
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_NO_SIGUSR2
ESMF_F90DEFAULT         = mpif77
ESMF_F90LINKERDEFAULT   = mpic++
ESMF_CXXDEFAULT         = mpic++
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),openmpi)
# OpenMPI --------------------------------------------------
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_NO_SIGUSR2
ESMF_F90DEFAULT         = mpif90
ESMF_F90LINKERDEFAULT   = mpicxx
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

############################################################
# Print compiler version string
#
ESMF_F90COMPILER_VERSION    = ${ESMF_F90COMPILER} --version
ESMF_CXXCOMPILER_VERSION    = ${ESMF_CXXCOMPILER} --version

############################################################
# Compiler flag to produce position independent code for shared object
#
ESMF_CXXCOMPILEOPTS       += -fPIC

############################################################
# Need this until the file convention is fixed (then remove these two lines)
#
ESMF_F90COMPILEFREENOCPP = --nfix
ESMF_F90COMPILEFIXCPP    = --fix -Cpp

############################################################
# Determine where lf95's libraries are located
#
ESMF_CXXLINKPATHS += $(addprefix -L,$(shell $(ESMF_DIR)/scripts/libpath.lf95 $(ESMF_F90COMPILER)))
ESMF_RPATHPREFIXFIXED := $(ESMF_RPATHPREFIX)
ESMF_CXXLINKRPATHS += $(addprefix $(ESMF_RPATHPREFIXFIXED), $(shell $(ESMF_DIR)/scripts/libpath.lf95 $(ESMF_F90COMPILER)))

############################################################
# Determine where lf95's libraries are located as to find it during runtime
#
ESMF_F90LINKPATHS += $(addprefix -L,$(shell $(ESMF_DIR)/scripts/libpath.lf95 $(ESMF_F90COMPILER)))
ESMF_RPATHPREFIXFIXED := $(ESMF_RPATHPREFIX)
ESMF_F90LINKRPATHS += $(addprefix $(ESMF_RPATHPREFIXFIXED), $(shell $(ESMF_DIR)/scripts/libpath.lf95 $(ESMF_F90COMPILER)))

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ESMF_CXXLINKLIBS += -lrt $(shell $(ESMF_DIR)/scripts/libs.lf95 $(ESMF_F90COMPILER))

############################################################
# Link against libesmf.a using the F90 linker front-end
#
ESMF_F90LINKLIBS += -lrt $(shell $(ESMF_DIR)/scripts/libs.lf95 $(ESMF_F90COMPILER)) $(shell $(ESMF_DIR)/scripts/f90rtobjects.lf95 $(ESMF_F90COMPILER))

############################################################
# Shared library options
#
ESMF_SL_LIBOPTS  += -shared
ESMF_SL_LIBLIBS  += $(ESMF_CXXLINKPATHS) $(ESMF_CXXLINKLIBS)

