# $Id: build_rules.mk,v 1.20.2.8 2006/09/15 16:19:44 svasquez Exp $
#
# Linux.lahey.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = lf95
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
ESMF_CXXCOMPILEOPTS    += -DESMF_MPIUNI
ESMF_CXXCOMPILEPATHS   += -I$(ESMF_DIR)/src/Infrastructure/stubs/mpiuni
ESMF_MPIRUNDEFAULT      = $(ESMF_DIR)/src/Infrastructure/stubs/mpiuni/mpirun
else
ifeq ($(ESMF_COMM),mpich)
# Mpich ----------------------------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_F90LINKLIBS       += 
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
# LAM (assumed to be built with lf95) ----------------------
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
#
# location of external libs.  if you want to use any of these,
# define ESMF_SITE to my_site so the build system can find it,
# copy this file into Linux.intel.my_site, and uncomment the
# libs you want included.  remove the rest of this file since
# both this file and the site file will be included.

# LAPACK_INCLUDE   = 
# LAPACK_LIB       = -L/usr/local/lib -llapack
# NETCDF_INCLUDE   = -I/usr/local/include/netcdf
# NETCDF_LIB       = -L/usr/local/lib -lnetcdf
# HDF_INCLUDE      = -I/usr/local/include/hdf
# HDF_LIB          = -L/usr/local/lib/ -lmfhdf -ldf -ljpeg -lz
# BLAS_INCLUDE     = 
# BLAS_LIB         = -latlas -lscs

############################################################
# Compiler flag to produce position independent code for shared object
#
ESMF_CXXCOMPILEOPTS       += -fPIC

############################################################
# Compiler options to print version string
#
ESMF_F90VOPT        = --version
ESMF_CXXVOPT        = --version

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
# Shared library options
#
ESMF_SL_LIBOPTS  += -shared
ESMF_SL_LIBLIBS  += $(ESMF_CXXLINKPATHS) $(ESMF_CXXLINKLIBS)

