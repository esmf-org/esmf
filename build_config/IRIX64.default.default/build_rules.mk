# $Id: build_rules.mk,v 1.26.2.4 2006/09/07 19:46:06 theurich Exp $
# 
# IRIX64.default.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = f90
ESMF_F90LINKERDEFAULT   = CC
ESMF_CXXDEFAULT         = CC

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
ESMF_F90LINKLIBS       += -lmpiuni
ESMF_CXXCOMPILEOPTS    += -DESMF_MPIUNI
ESMF_CXXCOMPILEPATHS   += -I$(ESMF_DIR)/src/Infrastructure/stubs/mpiuni
ESMF_CXXLINKLIBS       += -lmpiuni
ESMF_MPIRUNDEFAULT      = $(ESMF_DIR)/src/Infrastructure/stubs/mpiuni/mpirun
else
ifeq ($(ESMF_COMM),mpi)
# Vendor MPI -----------------------------------------------
ESMF_F90LINKLIBS       += -lmpi++ -lmpi
ESMF_CXXLINKLIBS       += -lmpi++ -lmpi
ESMF_MPIRUNDEFAULT      = mpirun
else
ifeq ($(ESMF_COMM),user)
# User specified flags -------------------------------------
else
$(error Invalid ESMF_COMM setting: $(ESMF_COMM))
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
# 32- vs. 64-bit ABI
#
ifeq ($(ESMF_ABI),32)
ESMF_CXXCOMPILEOPTS       += -n32
ESMF_CXXLINKOPTS          += -n32
ESMF_F90COMPILEOPTS       += -n32
ESMF_F90LINKOPTS          += -n32
endif
ifeq ($(ESMF_ABI),64)
ESMF_CXXCOMPILEOPTS       += -64
ESMF_CXXLINKOPTS          += -64
ESMF_F90COMPILEOPTS       += -64
ESMF_F90LINKOPTS          += -64
endif

############################################################
# Special compiler flags
#
# abide to C++ language standard; don't put libc into std name space
ESMF_CXXCOMPILEOPTS       += -LANG:std=on:libc_in_namespace_std=off
#
# allow for multi-processor code (important for shared objects!)
ESMF_CXXCOMPILEOPTS       += -mp
ESMF_CXXLINKOPTS          += -mp
ESMF_F90COMPILEOPTS       += -mp
ESMF_F90LINKOPTS          += -mp

############################################################
# Conditionally add pthread compiler and linker flags
#
ifeq ($(ESMF_PTHREADS),ON)
ESMF_F90LINKLIBS       += -lpthread
ESMF_CXXLINKLIBS       += -lpthread
endif

############################################################
# Compiler options to print version string
#
ESMF_F90VOPT        = -version
ESMF_CXXVOPT        = -version

############################################################
# Special optimization flags
#
ESMF_CXXOPTFLAG_O       += -OPT:Olimit=6500
ESMF_F90OPTFLAG_O       += -OPT:Olimit=6500

############################################################
# Help f90 to understand Fortran suffices
#
ESMF_F90COMPILEFREECPP   = -freeform -cpp
ESMF_F90COMPILEFREENOCPP = -freeform -nocpp
ESMF_F90COMPILEFIXCPP    = -fixedform -cpp -extend_source
ESMF_F90COMPILEFIXNOCPP  = -fixedform -nocpp -extend_source

############################################################
# Prefix for rpath option
#
ESMF_RPATHPREFIX      = -rpath 

############################################################
# IRIX64 does not have a ranlib -> "true" is a noop command
#
ESMF_RANLIBDEFAULT         = true

############################################################
# Link against libesmf.a using the F90 linker front-end
#
ESMF_F90LINKLIBS += -lftn -lfortran -lm

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ESMF_CXXLINKLIBS += -lftn -lfortran -lm

############################################################
# Shared library options
#
ESMF_SL_LIBOPTS  += -shared $(ESMF_CXXCOMPILEOPTS)
ESMF_SL_LIBLIBS  += $(ESMF_CXXLINKPATHS) $(ESMF_CXXLINKLIBS)
