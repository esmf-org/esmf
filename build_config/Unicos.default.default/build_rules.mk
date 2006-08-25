# $Id: build_rules.mk,v 1.8.2.4 2006/08/25 22:19:04 theurich Exp $
# 
# Unicos.default.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = ftn -h ssp -dy
ESMF_F90LINKERDEFAULT   = CC -h new_for_init -h ssp
ESMF_CXXDEFAULT         = CC -h new_for_init -h ssp

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
ESMF_F90LINKLIBS       += -lmpi
ESMF_CXXLINKLIBS       += -lmpi
ESMF_MPIRUNDEFAULT      = $(ESMF_DIR)/scripts/mpirun.unicos
ESMF_MPIMPMDRUNDEFAULT  =
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
# on X1 optimization level must be > 1 else no optimization
#
ESMF_OPTLEVELDEFAULT  = 2

############################################################
# Compiler options to print version string
#
ESMF_CXXVOPT        = -V
ESMF_F90VOPT        = -V

############################################################
# Blank out variables to prevent rpath encoding
#
ESMF_F90LINKRPATHS      =
ESMF_CXXLINKRPATHS      =

############################################################
# Help ftn to understand Fortran suffices
#
ESMF_F90COMPILEFREECPP   = -f free -N 255 -F -M1549
ESMF_F90COMPILEFREENOCPP = -f free -N 255
ESMF_F90COMPILEFIXCPP    = -f fixed -N 132 -F -M1549
ESMF_F90COMPILEFIXNOCPP  = -f fixed -N 132

############################################################
# Tell ftn to use module files
#
ESMF_F90IMOD        = -em -J

############################################################
# X1 does not have a ranlib -> "true" is a noop command
#
ESMF_RANLIBDEFAULT         = true

############################################################
# Link against libesmf.a using the F90 linker front-end
#
ESMF_F90LINKLIBS +=

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ESMF_CXXLINKLIBS +=

############################################################
# Blank out shared library options
#
ESMF_SL_LIBS_TO_MAKE  =

############################################################
# Exclude I1 and I2 data kinds
#
ESMF_F90COMPILECPPFLAGS += -DESMF_NO_INTEGER_1_BYTE
ESMF_F90COMPILECPPFLAGS += -DESMF_NO_INTEGER_2_BYTE

############################################################
# Define CPPRULES here instead of using those in common.mk
#
ESMF_CPPRULES         = defined
ESMF_CPPDEFAULT       = cpp

# change to not depend upon gcc -E -P behavior, remove -P and add filter 
# to delete #line
# amend bad cpp ".TRUE.", ". NOT." , "=>' output
# fix leading space that seems to show up on some lines
%.F90 : %.cpp
	${ESMF_CPP} -E -I${ESMF_INCDIR} $< | tr "@^" "\n#" | sed -e 's/^ //' -e '/^#line/d' -e 's/ \. /./g' -e 's/\. not\./.not./g' -e 's/= >/=>/g' > $(dir$<)$(notdir $@)
