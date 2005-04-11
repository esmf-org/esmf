#  $Id: build_rules.mk,v 1.22 2005/04/11 16:51:51 nscollins Exp $
#
#  OSF1.default.default
#
#

#
#  Make sure that ESMF_PREC is set to 64
#
ESMF_PREC = 64

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
# LAPACK_LIB       = -L/usr/lib -lcxml
# NETCDF_INCLUDE   = -I/usr/local/unsupported/netcdf-3.5.0/include
# NETCDF_LIB       = -L/usr/local/unsupported/netcdf-3.5.0/lib -lnetcdf
# HDF_INCLUDE      = -I/usr/local/unsupported/HDF4.1r5/include
# HDF_LIB          = -L/usr/local/unsupported/HDF4.1r5/lib/ -lmfhdf -ldf -ljpeg -lz
# BLAS_INCLUDE     = 
# BLAS_LIB         = -latlas
#
############################################################


#
# Location of MPI (Message Passing Interface) software
#
MPI_HOME         = /usr/opt/mpi/lib
MPI_INCLUDE      = -I/usr/opt/mpi/include
MPI_LIB          = -L/usr/opt/mpi/lib -lmpi
MPIRUN           = ${ESMF_TOP_DIR}/scripts/mpirun.alpha
# is this needed?  TODO: remove this
#MPI64_LIB        = 



#
# Fortran compiler options 
#
BIG_ENDIAN		= -convert big_endian
C_64BIT			= 
DARCH			= -Dalpha -DOSF1
DsysARCH		= -DsysOSF1
INF_CONTROL		= -fpe1
NO_LINE_DIRECTIVES	= -wl,P
NO_AUTO_PARALLEL	= -nomp
NO_INLINING		= -noinline
REAL8			= -r8
STRICT			= -nopipeline
EXPAND_TEMPLATES        = -tweak

FPPFLAGS		= $(FPPOPTS)
F_FREECPP               = -free -cpp
F_FIXCPP                = -cpp -extend_source
F_FREENOCPP             = -free
F_FIXNOCPP              = -extend_source

#
# compilers and flags
#
C_CC		= cc
C_CXX		= cxx -x cxx
C_FC		= f90

C_CCV		= $(C_CC) -V
C_CXXV  	= $(C_CXX) -V
C_FCV		= $(C_FC) -version


G_CFLAGS	+= -assume gfullpath
G_FFLAGS	+= -assume gfullpath

O_CFLAGS	+= -w
O_FFLAGS	+= -w

#
C_F90CXXLIBS       = -L/usr/ccs/lib/cmplrs/cxx -lcxx -lrt
C_CXXF90LIBS       = -L/usr/opt/F55A/usr/shlib -lfor -lrt -lm
# for older Fortran compilers (551, 551A - 551F) the previous line 
# must be replaced by:
#C_CXXF90LIBS       = -L/usr/opt/F551/usr/shlib -lfor -lrt

# conditionally add pthread compiler flags
ifeq ($(ESMF_PTHREADS),ON)
G_CFLAGS    +=  -pthread
G_FFLAGS    +=  -pthread -reentrancy threaded
X_CFLAGS    +=  -pthread
X_FFLAGS    +=  -pthread -reentrancy threaded
O_CFLAGS    +=  -pthread
O_FFLAGS    +=  -pthread -reentrancy threaded
THREAD_LIB  = -lpthread
#C_CXXF90LD   +=  -pthread
#C_F90CXXLD   +=  -pthread -reentrancy threaded
endif

###############################################################################

PARCH			= alpha

# this platform does make shared libs

# the options are needed before the .o list, the libs after
C_SL_LIBOPTS = -shared -rpath .:$(ESMF_LIBDIR) -L$(ESMF_LIBDIR)

C_SL_LIBLIBS = ${C_F90CXXLIBS} ${C_CXXF90LIBS} ${MPI_LIB} ${EXTRA_LIBS} -lm

