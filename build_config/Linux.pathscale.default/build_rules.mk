#  $Id: build_rules.mk,v 1.5.4.1 2006/07/31 22:32:40 theurich Exp $
#
#  Linux.pathscale.default makefile
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
ESMF_F90LINKLIBS       += -lmpiuni
ESMF_CXXCOMPILEOPTS    += -DESMF_MPIUNI
ESMF_CXXCOMPILEPATHS   += -I$(ESMF_DIR)/src/Infrastructure/stubs/mpiuni
ESMF_CXXLINKLIBS       += -lmpiuni
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
# Chose ABI flags
#
ifeq ($(ESMF_PREC),32)
ESMF_CXXCOMPILEOPTS       += -m32
ESMF_CXXLINKOPTS          += -m64
ESMF_F90COMPILEOPTS       += -m64
ESMF_F90LINKOPTS          += -m64
endif
ifeq ($(ESMF_PREC),64)
ESMF_CXXCOMPILEOPTS       += -m64
ESMF_CXXLINKOPTS          += -m64
ESMF_F90COMPILEOPTS       += -m64
ESMF_F90LINKOPTS          += -m64
endif
ifeq ($(ESMF_PREC),64x86)
ESMF_CXXCOMPILEOPTS       += -m64
ESMF_CXXLINKOPTS          += -m64
ESMF_F90COMPILEOPTS       += -m64
ESMF_F90LINKOPTS          += -m64
endif

############################################################
# Compiler options to print version string
#
ESMF_F90VOPT        = -version
ESMF_CXXVOPT        = -version

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
