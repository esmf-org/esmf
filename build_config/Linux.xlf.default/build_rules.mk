#  $Id: build_rules.mk,v 1.3 2005/02/23 05:15:51 theurich Exp $
#
#  
#

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
# ? Location of MPI.  the following is an older comment.
# Location of BLAS and LAPACK.  
#
# Important: If you use the IBM version of lapack you must include 
#  -lessl at the end of the line defining the BLAS libraries.
#
#  If you list -lessl or -lesslp2 below you must have -DESMC_HAVE_ESSL listed in the 
# PCONF line at the bottom of this file.
#

# stubs for pthreads - pthreads are not supported on bgl
CPPFLAGS        += -DESMF_NO_PTHREADS
# another bgl exclusive:
ESMF_PREC=32

ifeq ($(ESMF_NO_IOCODE),true)
BLAS_LIB         =
LAPACK_LIB       =
NETCDF_LIB       = -lnetcdf_stubs
NETCDF_INCLUDE   = -I${ESMF_DIR}/src/Infrastructure/stubs/netcdf_stubs
HDF_LIB          =
HDF_INCLUDE      =
else
BLAS_LIB         = -L/bgl/local/lib -lblas440 ${FC_LIB}
LAPACK_LIB       = -L/bgl/local/lib  -llapack440 
NETCDF_INCLUDE   = -I/bgl/local/netcdf-3.5.1/include
NETCDF_LIB       = -L/bgl/local/netcdf-3.5.1/lib -lnetcdf
HDF_INCLUDE	 = 
HDF_LIB		 = 
endif
#end of io bypass section

#
# Location of MPI (Message Passing Interface) software
#
ifeq ($(ESMF_COMM),mpi)
MPI_LIB        = 
MPI_INCLUDE    = 
MPIRUN         = ${ESMF_TOP_DIR}/scripts/mpirun.bgl
MPI64_LIB      = 
endif

ifeq ($(ESMF_COMM),mpiuni)
endif

# ######################### Common compiler options #####################
DARCH			= -DLINUX
DsysARCH                = -DsysAIX

COM_MEMCHECK_FLAG      = -qcheck
COM_FULLPATH_FLAG      = -qfullpath
COM_DEBUG_FLAG         = -g
COM_ALL_DEBUG_FLAGS    = -g $(COM_MEMCHECK_FLAG) $(COM_FULLPATH_FLAG)
COM_MAXMEM_FLAG        = -qmaxmem=4000
COM_NOWARN_FLAG        = -w
COM_SPILL_FLAG         = -qspill=3000
COM_OPT_FLAG           = -O2
COM_ALL_OPT_FLAGS      = -O2 $(COM_MAXMEM_FLAG) $(COM_NOWARN_FLAG) $(COM_SPILL_FLAG)
COM_PLAIN_FLAG         =

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

C_CLINKER_SLFLAG	= -L
C_FLINKER_SLFLAG	= -L
CXX_CLINKER_SLFLAG	= -L
CXX_FLINKER_SLFLAG	= -L

AR_FLAGS		= cr
AR_EXTRACT              = -x
OMAKE			= ${MAKE}
SHELL			= /bin/sh
SED			= /bin/sed
RM			= rm -f

############################################################

PARCH			= Linux
AR			= ar
RANLIB			= ranlib
# Fortran compiler 
AR32_64			= ar 
C_64BIT			= 
REAL8			= -qrealsize=8
FPPFLAGS		= $(addprefix $(FPP_PREFIX), $(FPPOPTS))
# C and Fortran compiler
C_CC			= mpcc 
C_FC			= mpxlf90 
C_FC_MOD		= -I
C_CLINKER		= mpcc -bmaxdata:0x70000000  -qcheck 
# you may need to add -bI:/usr/lpp/xlf/lib/lowsys.exp to C_LINKER
C_FLINKER		= mpxlf90 -bmaxdata:0x70000000 -lC -qcheck 
# C++ compiler
CXX_CC			= mpCC 
CXX_FC			= mpxlf90
CXX_CLINKER		= mpCC -qcheck 
CXX_FLINKER		= mpxlf90 -qcheck 
SL_LIB_LINKER 		= mpCC  -L$(ESMF_LIBDIR)


# start of common section
# ######################### C and Fortran compiler ########################
#
C_CCV			= 
C_FCV			= 
C_SYS_LIB		= 
# ---------------------------- BOPT - g options ----------------------------
G_COPTFLAGS		= $(COM_ALL_DEBUG_FLAGS)
G_FOPTFLAGS		= $(COM_ALL_DEBUG_FLAGS)
# ----------------------------- BOPT - O options -----------------------------
O_COPTFLAGS		= $(COM_ALL_OPT_FLAGS)
O_FOPTFLAGS		= $(COM_OPT_FLAG) $(COM_WARN_FLAG)
FCPPFLAGS		= ${ESMC_INCLUDE} ${PCONF} ${ESMC_PARCH} ${FPPFLAGS} $(FCPP_EXHAUSTIVE)
# ########################## C++ compiler ##################################
CXX_CCV			= 
CXX_SYS_LIB		= 
CXX_SYS_LIBS		= -lC
C_CXXF90LD		= ${CXX_CC}
C_F90CXXLD		= ${CXX_FC}

C_CXXF90LIBS		= -L/opt/ibmcmp/xlf/9.1/lib -L/opt/ibmcmp/vacpp/7.0/lib -lm -lxlf90 -libmc++

C_F90CXXLIBS		= -L/opt/ibmcmp/xlf/9.1/lib -L/opt/ibmcmp/vacpp/7.0/lib -lxlf90 -libmc++

C_CXXSO			= mpCC -G

C_CXXSOLIBS		= -L/opt/ibmcmp/xlf/9.1/lib -L/opt/ibmcmp/vacpp/7.0/lib -lm -lxlf90 -libmc++

# ------------------------- BOPT - g_c++ options ------------------------------
GCXX_COPTFLAGS		= $(COM_ALL_DEBUG_FLAGS)
GCXX_FOPTFLAGS		= $(COM_ALL_DEBUG_FLAGS)
# ------------------------- BOPT - O_c++ options ------------------------------
OCXX_COPTFLAGS		= $(COM_ALL_OPT_FLAGS)
OCXX_FOPTFLAGS		= $(COM_OPT_FLAG) $(COM_WARN_FLAG)
# -------------------------- BOPT - g_complex options ------------------------
GCOMP_COPTFLAGS		= $(COM_ALL_DEBUG_FLAGS)
GCOMP_FOPTFLAGS		= $(COM_ALL_DEBUG_FLAGS)
# --------------------------- BOPT - O_complex options -------------------------
OCOMP_COPTFLAGS		= $(COM_ALL_OPT_FLAGS)
OCOMP_FOPTFLAGS		= $(COM_OPT_FLAG) $(COM_WARN_FLAG)
#

SL_LIBS_TO_MAKE = libesmf

SL_SUFFIX   = so
SL_LIBOPTS  = -G -qmkshrobj $(C_F90CXXLIBS) $(MPI_LIB)
SL_LINKOPTS = -brtl
SL_F_LINKER = $(F90CXXLD) 
SL_C_LINKER = $(CXXF90LD) 

# end of common settings

