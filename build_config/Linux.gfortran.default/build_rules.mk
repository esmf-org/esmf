# $Id: build_rules.mk,v 1.5.2.2 2007/10/03 17:29:21 theurich Exp $
#
# Linux.gfortran.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = gfortran
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
ESMF_F90DEFAULT         = mpif90
ESMF_F90LINKLIBS       += -lpmpich++ -lmpich
ESMF_CXXDEFAULT         = mpiCC
ESMF_CXXCOMPILEOPTS    += -DESMF_MPICH
ESMF_MPIRUNDEFAULT      = mpirun
else
ifeq ($(ESMF_COMM),mpich2)
# Mpich2 ---------------------------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpicxx
ESMF_MPIRUNDEFAULT      = mpirun
ESMF_MPIMPMDRUNDEFAULT  = mpiexec
else
ifeq ($(ESMF_COMM),lam)
# LAM (assumed to be built with gfortran) -----------------------
ESMF_F90DEFAULT         = mpif77
ESMF_CXXDEFAULT         = mpic++
ESMF_MPIRUNDEFAULT      = mpirun
ESMF_MPIMPMDRUNDEFAULT  = mpiexec
else
ifeq ($(ESMF_COMM),openmpi)
# OpenMPI --------------------------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_F90LINKLIBS       += -lmpi_cxx
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
ESMF_F90COMPILER_VERSION    = ${ESMF_F90COMPILER} -v --version
ESMF_CXXCOMPILER_VERSION    = ${ESMF_CXXCOMPILER} -v --version

############################################################
# Fortran symbol convention must match other libraries used
#
ESMF_F90COMPILEOPTS       += -fno-second-underscore
ESMF_F90LINKOPTS          += -fno-second-underscore

############################################################
# On IA64 set long and pointer types to 64-bit
#
ifeq ($(ESMF_ABI),64)
ESMF_CXXCOMPILEOPTS       += -march=k8 -m64 -mcmodel=medium
ESMF_CXXLINKOPTS          += -march=k8 -m64 -mcmodel=medium
ESMF_F90COMPILEOPTS       += -march=k8 -m64 -mcmodel=medium
ESMF_F90LINKOPTS          += -march=k8 -m64 -mcmodel=medium
endif

############################################################
# Need this until the file convention is fixed (then remove these two lines)
#
ESMF_F90COMPILEFREENOCPP = -ffree-form
ESMF_F90COMPILEFIXCPP    = -cpp -ffixed-form

############################################################
# Determine where gcc's libraries are located
#
ESMF_F90LINKPATHS += \
  -L$(dir $(shell $(ESMF_CXXCOMPILER) -print-file-name=libstdc++.so))
ESMF_F90LINKRPATHS += \
  -Wl,-rpath,$(dir $(shell $(ESMF_CXXCOMPILER) -print-file-name=libstdc++.so))

############################################################
# Determine where gfortran's libraries are located
#
ESMF_CXXLINKPATHS += \
  -L$(dir $(shell $(ESMF_F90COMPILER) -print-file-name=libgfortran.so))
ESMF_CXXLINKRPATHS += \
  -Wl,-rpath,$(dir $(shell $(ESMF_F90COMPILER) -print-file-name=libgfortran.so))

############################################################
# Link against libesmf.a using the F90 linker front-end
#
ESMF_F90LINKLIBS += -lrt -lstdc++

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ESMF_CXXLINKLIBS += -lrt -lgfortran

############################################################
# Blank out shared library options
#
ESMF_SL_LIBS_TO_MAKE  =
