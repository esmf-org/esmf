#  $Id: build_rules.mk,v 1.28.2.1 2005/07/06 20:59:49 nscollins Exp $
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
# these work for halem at goddard; but do not overwrite them
# if MPI_HOME is already set by the user.
ifndef MPI_HOME
MPI_HOME         = /usr/opt/mpi
endif

ifeq ($(ESMF_COMM),mpi)
MPI_LIB          += -lmpi
endif

ifeq ($(ESMF_COMM),lam)
# with lam-mpi installed:
MPI_LIB        += -lmpi -llam
endif

ifeq ($(ESMF_COMM),mpich)
# with mpich installed:
MPI_INCLUDE    += -DESMF_MPICH
MPI_LIB        += -lmpich
MPIRUN         += $(ESMF_NODES)
endif

# name of the lib which includes the posix thread support.
THREAD_LIB     = -lpthread


# on halem we provide a system-specific job submission script, but if the user
# has already set it, do not overwrite the value of MPIRUN.
# note that there is always a default MPIRUN setting in the top level
# common.mk makefile, so only override MPIRUN if the definition was not
# from an environment variable setting.   if this needs to be different on
# your system, add an assignment in a site file to set MPIRUN, or set MPIRUN
# as an environment variable before running.
ifneq ($(origin MPIRUN), environment)
MPIRUN           = ${ESMF_TOP_DIR}/scripts/mpirun.alpha
endif

# is this needed?  TODO: remove this because it seems obsolete.
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
ifneq ($(ESMF_COMM),mpich)
C_CC		= cc
C_CXX		= cxx -x cxx
C_FC		= f90

C_CCV		= $(C_CC) -V
C_CXXV  	= $(C_CXX) -V
C_FCV		= $(C_FC) -version
endif

ifeq ($(ESMF_COMM),mpich)
C_CC            = mpicc
C_CXX           = mpiCC 
C_FC            = mpif90

C_CCV		= $(C_CC) --version
C_CXXV  	= $(C_CXX) --version
C_FCV		= $(C_FC) --version
endif




G_CFLAGS	+= -assume gfullpath
G_FFLAGS	+= -assume gfullpath

O_CFLAGS	+= -w
O_FFLAGS	+= -w

# add the LD_LIBRARY_PATHs, but the ld on this system only takes the
# last -rpath argument, and expects it to be dir:dir:dir format, not
# adding on each -rpath to the existing one like most other systems.
# note that here i am using = and not += because i want to override
# the default values for these which are computed in common.mk
ifeq ($(origin LD_LIBRARY_PATH), environment)
ENV_LIB_PATHS  = $(addprefix -L, $(subst :, ,$(LD_LIBRARY_PATH)))
ENV_LD_PATHS   = -rpath $(LDIR):$(LD_LIBRARY_PATH)
endif

# explicitly turn off the SLFLAG here after constructing the ld path.
C_SLFLAG =  
C_LIB_PATHS += $(ENV_LIB_PATHS)
C_LD_PATHS  += $(ENV_LD_PATHS)

#
C_F90CXXLIBS       = -L/usr/ccs/lib/cmplrs/cxx -lcxx -lrt -lm
C_CXXF90LIBS       = -lfor -lrt -lm
# for older Fortran compilers (551, 551A - 551F) the previous line 
# must be replaced by:
#C_CXXF90LIBS       = -L/usr/opt/F551/usr/shlib -lfor -lrt

# conditionally add pthread compiler flags
ifeq ($(ESMF_PTHREADS),ON)
G_CFLAGS    +=  -pthread
G_FFLAGS    +=  -pthread -reentrancy threaded
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
C_SL_LIBOPTS = -shared $(LIB_PATHS) $(LD_PATHS)

C_SL_LIBLIBS = ${C_F90CXXLIBS} ${C_CXXF90LIBS} ${MPI_LIB} ${EXTRA_LIBS} -lm

