#  $Id: build_rules.mk,v 1.3 2003/11/11 18:52:14 nscollins Exp $
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
#  File base.site
#

#
#  This file contains site-specific information.  The definitions below
#  should be changed to match the locations of libraries at your site.
#  The following naming convention is used:
#     XXX_LIB - location of library XXX
#     XXX_INCLUDE - directory for include files needed for library XXX
#
# Location of BLAS and LAPACK.  See ${ESMF_DIR}/docs/instllation.html
# for information on retrieving them.
#
# Important: If you use the IBM version of lapack you must include 
#  -lessl at the end of the line defining the BLAS libraries.
#
#  If you list -lessl or -lesslp2 below you must have -DESMC_HAVE_ESSL listed in the 
# PCONF line at the bottom of this file.
#
BLAS_LIB         = 
LAPACK_LIB       = -L/usr/lib -lcxml
NETCDF_LIB       = -L/usr/local/unsupported/netcdf-3.5.0/lib -lnetcdf
NETCDF_INCLUDE   = -I/usr/local/unsupported/netcdf-3.5.0/include
NETCDF64_LIB     = -L/usr/local/unsupported/netcdf-3.5.0/lib -lnetcdf
NETCDF64_INCLUDE = -I/usr/local/unsupported/netcdf-3.5.0/include
HDF_LIB          = /usr/local/unsupported/HDF4.1r5/lib/libmfhdf.a \
                   /usr/local/unsupported/HDF4.1r5/lib/libdf.a \
                   /usr/local/unsupported/HDF4.1r5/lib/libjpeg.a \
                   /usr/local/unsupported/HDF4.1r5/lib/libz.a
HDF_INCLUDE      = -I/usr/local/unsupported/HDF4.1r5/include
#
# Location of MPI (Message Passing Interface) software
#
MPI_LIB          = -L/usr/opt/mpi/lib -lmpi
MPI_INCLUDE      = -I/usr/opt/mpi/include
MPIRUN           = ${ESMF_TOP_DIR}/scripts/mpirun.alpha
MPI64_LIB        = 

# For pthreads (or omp)
THREAD_LIB        = -lpthread


############################################################
#
# File base_variables
#

#
#     See the file build/base_variables.defs for a complete explanation of all these fields
#
AR			= ar
AR_FLAGS		= cr
AR_EXTRACT              = -x 
RM			= rm -f
OMAKE			= ${MAKE}
RANLIB			= ranlib
SHELL			= /bin/sh
SED			= /bin/sed
# ######################### Fortran compiler options ######################
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
# ######################### C and Fortran compiler ########################
#
C_CC			= cc
C_FC			= f90
C_FC_MOD		= -I
C_CLINKER_SLFLAG	= -Wl,-rpath,
C_FLINKER_SLFLAG	= -Wl,-rpath,
C_CLINKER		= cc
C_FLINKER		= f90
C_CCV			= -V
C_FCV			= -version
C_SYS_LIB		= -lfor -lutil -lFutil -lots
# ---------------------------- BOPT - g options ----------------------------
G_COPTFLAGS		= -g -assume gfullpath -pthread
G_FOPTFLAGS		= -g -assume gfullpath 
# ----------------------------- BOPT - O options -----------------------------
O_COPTFLAGS		= -O3 -w -pthread
O_FOPTFLAGS		= -O3 -w
# ########################## C++ compiler ##################################
#
CXX_CC		   = cxx -x cxx
CXX_FC		   = f90
CXX_CLINKER_SLFLAG = -Wl,-rpath,
CXX_FLINKER_SLFLAG = -Wl,-rpath,
CXX_CLINKER	   = cxx
CXX_FLINKER	   = f90
CXX_CCV		   = -V
CXX_SYS_LIB	   = -lcomplex -lfor -lutil -lFutil -lots
C_F90CXXLIBS       = -L/usr/ccs/lib/cmplrs/cxx -lcxx 
C_F90CXXLD         = f90
C_CXXF90LD         = cxx
C_CXXF90LIBS       = -lfor
# ------------------------- BOPT - g_c++ options ------------------------------
GCXX_COPTFLAGS		= -g  -qfullpath
GCXX_FOPTFLAGS		= -g  -qfullpath 
# ------------------------- BOPT - O_c++ options ------------------------------
OCXX_COPTFLAGS		= -O3  -qmaxmem=4000 -qspill=3000
OCXX_FOPTFLAGS		= -O3
# -------------------------- BOPT - g_complex options ------------------------
GCOMP_COPTFLAGS		= -g  -qfullpath
GCOMP_FOPTFLAGS		= -g  -qfullpath
# --------------------------- BOPT - O_complex options -------------------------
OCOMP_COPTFLAGS		= -O3  -qmaxmem=4000 -qspill=3000
OCOMP_FOPTFLAGS		= -O3
################################################################################

PARCH			= alpha

SL_SUFFIX   = so
SL_LIBOPTS  = -shared -rpath .:$(ESMF_LIBDIR)  ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} 
SL_LINKOPTS = 
SL_F_LINKER = $(F90CXXLD)
SL_C_LINKER = $(CXXF90LD)
SL_LIB_LINKER = $(CXXF90LD)
SL_LIBS_TO_MAKE = libesmf liboldworld

############################################################
#
#  File base
#

#########

.F90.o:
	${FC} -c ${C_FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} -free -cpp ${FFLAGS} ${FCPPFLAGS} ${ESMC_INCLUDE} $<

.F.o:
	${FC} -c ${C_FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} -free  ${FFLAGS} ${ESMC_INCLUDE} $<

.f90.o:
	${FC} -c ${FOPTFLAGS} -extend_source -cpp ${FFLAGS} ${FCPPFLAGS} ${ESMC_INCLUDE} $<

.f.o:
	${FC} -c ${FOPTFLAGS} -extend_source ${FFLAGS} ${ESMC_INCLUDE} $<

.c.o:
	${CC} -c ${COPTFLAGS} ${CFLAGS} ${CCPPFLAGS} ${ESMC_INCLUDE} $<

.C.o:
	${CXX} -c ${COPTFLAGS} ${CFLAGS} ${CCPPFLAGS} ${ESMC_INCLUDE} $<

.F90.a:
	${FC} -c ${C_FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} -free -cpp ${FFLAGS} ${FCPPFLAGS} ${ESMC_INCLUDE} $<
	${AR} ${AR_FLAGS} ${LIBNAME} $*.o
	${RM} $*.o

.F.a:
	${FC} -c ${C_FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} -free ${FFLAGS} ${ESMC_INCLUDE} $<
	${AR} ${AR_FLAGS} ${LIBNAME} $*.o
	${RM} $*.o

.f90.a:
	${FC} -c ${FOPTFLAGS} -extend_source -cpp ${FFLAGS} ${FCPPFLAGS} ${ESMC_INCLUDE} $<
	${AR} ${AR_FLAGS} ${LIBNAME} $*.o
	${RM} $*.o

.f.a:
	${FC} -c ${FOPTFLAGS} -extend_source ${FFLAGS} ${ESMC_INCLUDE} $<
	${AR} ${AR_FLAGS} ${LIBNAME} $*.o
	${RM} $*.o

.c.a:
	${CC} -c ${COPTFLAGS} ${CFLAGS} ${CCPPFLAGS} ${ESMC_INCLUDE} $<
	${AR} ${AR_FLAGS} ${LIBNAME} $*.o
	${RM} $*.o

.C.a:
	${CXX} -c ${COPTFLAGS} ${CFLAGS} ${CCPPFLAGS} ${ESMC_INCLUDE} $<
	${AR} ${AR_FLAGS} ${LIBNAME} $*.o
	${RM} $*.o
#
# set shared dependent on build_shared to build .so lib
#
shared: build_shared

