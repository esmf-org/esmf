#  $Id: build_rules.mk,v 1.5 2005/04/11 15:53:38 nscollins Exp $
#
#  Linux.xlf.default - which currently means Blue Gene L
#   which is the only semi-Linux platform which runs xlf.
#

# Blue Gene L is only 32:
ESMF_PREC=32

#
# Default MPI setting.
#
ifeq ($(ESMF_COMM),default)
export ESMF_COMM := mpi
endif

############################################################
#
# location of external libs.  if you want to use any of these,
# define ESMF_SITE to my_site so the build system can find it,
# copy this file into Linux.absoft.my_site, and uncomment the
# libs you want included.  remove the rest of this file since
# both this file and the site file will be included.

# LAPACK_INCLUDE   = 
# LAPACK_LIB       = -L/bgl/local/lib  -llapack440 
# NETCDF_INCLUDE   = -I/bgl/local/netcdf-3.5.1/include
# NETCDF_LIB       = -L/bgl/local/netcdf-3.5.1/lib -lnetcdf
# HDF_INCLUDE      = -I/usr/local/include/hdf
# HDF_LIB          = -L/usr/local/lib/ -lmfhdf -ldf -ljpeg -lz
# BLAS_LIB         = -L/bgl/local/lib -lblas440 ${FC_LIB}

#
############################################################

# stubs for pthreads - pthreads are not supported on bgl
CPPFLAGS        += -DESMF_NO_PTHREADS



#
# Location of MPI (Message Passing Interface) software
#
ifeq ($(ESMF_COMM),mpi)
MPIRUN         = ${ESMF_TOP_DIR}/scripts/mpirun.bgl
endif


# ######################### Common compiler options #####################
DARCH			= -DLINUX
DsysARCH                = -DsysAIX

COM_MEMCHECK_FLAG      = -qcheck
COM_FULLPATH_FLAG      = -qfullpath
COM_ALL_DEBUG_FLAGS    = $(COM_MEMCHECK_FLAG) $(COM_FULLPATH_FLAG)
COM_MAXMEM_FLAG        = -qmaxmem=4000
COM_NOWARN_FLAG        = -w
COM_SPILL_FLAG         = -qspill=3000
COM_ALL_OPT_FLAGS      = $(COM_MAXMEM_FLAG) $(COM_NOWARN_FLAG) $(COM_SPILL_FLAG)

RESTRICTED_POINTERS	= -qkeyword=restrict
STRICT			= -qstrict

NO_AUTO_PARALLEL	= 
NO_INLINING		= -Q
NO_LINE_DIRECTIVES	= -P

FPP_PREFIX		= -WF,
F_FREECPP               = -qsuffix=cpp=F90
F_FIXCPP                = -qfixed=132 -qsuffix=cpp=f90
F_FREENOCPP             = -qsuffix=f=F
F_FIXNOCPP              = -qfixed=132 -qsuffix=f=f        

C_SLFLAG	        = -L

# misc flags
REAL8		= -qrealsize=8

# compilers
C_CC		= mpcc 
C_CXX		= mpCC 
C_FC		= mpxlf90 

C_CLINKER	= $(C_CC) -bmaxdata:0x70000000  -qcheck 
C_FLINKER	= $(C_FC) -bmaxdata:0x70000000 -lC -qcheck 


# version info
C_CCV		= $(C_CC) --version
C_CXXV		= $(C_CXX) --version
C_FCV		= $(C_FC) --version

# compiler flags
G_CFLAGS	+= $(COM_ALL_DEBUG_FLAGS)
G_FFLAGS	+= $(COM_ALL_DEBUG_FLAGS)

O_CFLAGS	+= $(COM_ALL_OPT_FLAGS)
O_FFLAGS	+= $(COM_OPT_FLAG) $(COM_WARN_FLAG)


C_CXXF90LIBS	= -L/opt/ibmcmp/xlf/9.1/blrts_lib \
                  -L/opt/ibmcmp/vacpp/7.0/blrts_lib \
                  -lm -lxlf90 -lxl -lxlopt -libmc++ -lxlfmath -lm -lrt

C_F90CXXLIBS	= -L/opt/ibmcmp/xlf/9.1/blrts_lib \
                  -L/opt/ibmcmp/vacpp/7.0/blrts_lib \
                  -lxlf90 -lxl -lxlopt -libmc++ -lxlfmath -lm -lrt


############################################################

PARCH		= Linux


SL_LIBS_TO_MAKE = 
C_SL_LIBOPTS  = -G


