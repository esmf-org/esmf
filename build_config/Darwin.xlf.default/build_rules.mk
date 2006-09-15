# $Id: build_rules.mk,v 1.16.4.6 2006/09/15 22:26:20 theurich Exp $
#
# Darwin.xlf.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = xlf90_r
ESMF_CXXDEFAULT         = xlC_r

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
ESMF_F90COMPILECPPFLAGS+= -WF,-DESMF_MPIUNI
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
# LAM (assumed to be built with xlf90) ---------------------
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
ESMF_F90COMPILER_VERSION    = which ${ESMF_F90COMPILER}
ESMF_CXXCOMPILER_VERSION    = ${ESMF_CXXCOMPILER} -qversion

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
ESMF_F90COMPILEFREENOCPP = -qfree=f90 -qsuffix=f=F
ESMF_F90COMPILEFIXCPP    = -qfixed=132 -qsuffix=cpp=f90
ESMF_F90COMPILEFIXNOCPP  = -qfixed=132 -qsuffix=f=f

############################################################
# Determine where xlf's libraries are located
#
ESMF_CXXLINKPATHS += -L$(shell $(ESMF_DIR)/scripts/libpath.xl $(ESMF_F90COMPILER))

############################################################
# Determine where xlc's libraries are located
#
ESMF_F90LINKPATHS += -L$(shell $(ESMF_DIR)/scripts/libpath.xl $(ESMF_CXXCOMPILER))

############################################################
# Link against libesmf.a using the F90 linker front-end
#
ESMF_F90LINKLIBS += -libmc++ -lstdc++

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ESMF_CXXLINKLIBS += -lxlf90_r -lxlfmath

############################################################
# Blank out shared library options
#
ESMF_SL_LIBS_TO_MAKE  =
