#  $Id: build_rules.mk,v 1.16 2004/12/17 21:01:40 nscollins Exp $
#
#  OSF1.default.default.mk
#
#

#
#  Make sure that ESMF_PREC is set to 64
#
ESMF_PREC = 64

#
# Default MPI setting.
#
ifndef ESMF_COMM
export ESMF_COMM := mpi
endif
ifeq ($(ESMF_COMM),default)
export ESMF_COMM := mpi
endif


############################################################
#
#  The following naming convention is used:
#     XXX_LIB - location of library XXX
#     XXX_INCLUDE - directory for include files needed for library XXX
#
# Location of BLAS and LAPACK.  See ${ESMF_DIR}/docs/instllation.html
# for information on retrieving them.
#
#
ifeq ($(ESMF_NO_IOCODE),true)
BLAS_LIB         =
LAPACK_LIB       =
NETCDF_LIB       = -lnetcdf_stubs
NETCDF_INCLUDE   = -I${ESMF_DIR}/src/Infrastructure/stubs/netcdf_stubs
HDF_LIB          =
HDF_INCLUDE      =
else
BLAS_LIB         = 
LAPACK_LIB       = -L/usr/lib -lcxml
NETCDF_LIB       = -L/usr/local/unsupported/netcdf-3.5.0/lib -lnetcdf
NETCDF_INCLUDE   = -I/usr/local/unsupported/netcdf-3.5.0/include
HDF_LIB          = -L/usr/local/unsupported/HDF4.1r5/lib/ -lmfhdf -ldf -ljpeg -lz
HDF_INCLUDE      = -I/usr/local/unsupported/HDF4.1r5/include
endif

#
# Location of MPI (Message Passing Interface) software
#
MPI_LIB          = -L/usr/opt/mpi/lib -lmpi
MPI_INCLUDE      = -I/usr/opt/mpi/include
MPIRUN           = ${ESMF_TOP_DIR}/scripts/mpirun.alpha
MPI64_LIB        = 

# For pthreads (or omp)
THREAD_LIB        = -pthread -lrt


############################################################
#
AR			= ar
AR_FLAGS		= cr
AR_EXTRACT              = -x 
RM			= rm -f
OMAKE			= ${MAKE}
RANLIB			= ranlib
SHELL			= /bin/sh
SED			= /bin/sed
#
# Fortran compiler options 
#
AR32_64			= ${AR}
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
# C and Fortran compiler 
#
C_CC			= cc
C_FC			= f90
C_FC_MOD		= -I
C_CLINKER_SLFLAG	= -Wl,-rpath,
C_FLINKER_SLFLAG	= -Wl,-rpath,
C_CLINKER		= cc
C_FLINKER		= f90
C_CCV			= $(C_CC) -V
C_FCV			= $(C_FC) -version
C_SYS_LIB		= -lutil -lFutil -lots
# ---------------------------- BOPT - g options ----------------------------
G_COPTFLAGS		= -g -assume gfullpath -pthread
G_FOPTFLAGS		= -g -assume gfullpath -pthread -reentrancy threaded -omp
# ----------------------------- BOPT - O options -----------------------------
O_COPTFLAGS		= -O2 -w -pthread
O_FOPTFLAGS		= -O2 -w -pthread -reentrancy threaded -omp
#
# C++ compiler 
#
CXX_CC		   = cxx -x cxx
CXX_FC		   = f90
CXX_CLINKER_SLFLAG = -Wl,-rpath,
CXX_FLINKER_SLFLAG = -Wl,-rpath,
CXX_CLINKER	   = cxx
CXX_FLINKER	   = f90
CXX_CCV		   = $(CXX_CC) -V
CXX_SYS_LIB	   = -lcomplex -lutil -lFutil -lots
C_F90CXXLIBS       = -L/usr/ccs/lib/cmplrs/cxx -lcxx 
C_F90CXXLD         = f90
C_CXXF90LD         = cxx
C_CXXF90LIBS       = 
# ------------------------- BOPT - g_c++ options ------------------------------
GCXX_COPTFLAGS		= -g  -qfullpath
GCXX_FOPTFLAGS		= -g  -qfullpath 
# ------------------------- BOPT - O_c++ options ------------------------------
OCXX_COPTFLAGS		= -O2  -qmaxmem=4000 -qspill=3000
OCXX_FOPTFLAGS		= -O2
# -------------------------- BOPT - g_complex options ------------------------
GCOMP_COPTFLAGS		= -g  -qfullpath
GCOMP_FOPTFLAGS		= -g  -qfullpath
# --------------------------- BOPT - O_complex options -------------------------
OCOMP_COPTFLAGS		= -O2  -qmaxmem=4000 -qspill=3000
OCOMP_FOPTFLAGS		= -O2
###############################################################################

PARCH			= alpha

SL_LIBS_TO_MAKE = libesmf 

SL_SUFFIX   = so
SL_LIBOPTS  = -shared -rpath .:$(ESMF_LIBDIR)  ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} ${NETCDF_LIB} 
SL_LINKOPTS = 
SL_F_LINKER = $(F90CXXLD)
SL_C_LINKER = $(CXXF90LD)
SL_LIB_LINKER = $(CXXF90LD)


