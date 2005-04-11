#  $Id: build_rules.mk,v 1.21 2005/04/11 15:53:37 nscollins Exp $
#
#  AIX.default.default
#

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
# copy this file into AIX.default.my_site, and uncomment the
# libs you want included.  remove the rest of this file since
# both this file and the site file will be included.

# Note: If you use the IBM version of lapack you must include 
#  -lessl at the end of the line defining the BLAS libraries.
#

# ifeq ($(ESMF_PREC),32)
# LAPACK_INCLUDE   = 
# LAPACK_LIB       = -L/usr/local/lib32/r4i4 -llapack -lessl
# NETCDF_INCLUDE   = -I/usr/local/include
# NETCDF_LIB       = -L/usr/local/lib32/r4i4 -lnetcdf
# HDF_INCLUDE      = -I/usr/local/include/hdf
# HDF_LIB          = -L/usr/local/lib32/r4i4 -lmfhdf
# BLAS_INCLUDE     = 
# BLAS_LIB         = -lblas
# endif
# # end 32 bit section
# ifeq ($(ESMF_PREC),64)
# LAPACK_INCLUDE   = 
# LAPACK_LIB       = -L/usr/local/lib64/r4i4 -llapack -lessl
# NETCDF_INCLUDE   = -I/usr/local/include
# NETCDF_LIB       = -L/usr/local/lib64/r4i4 -lnetcdf
# HDF_INCLUDE      = -I/usr/local/include/hdf
# HDF_LIB          = -L/usr/local/lib64/r4i4 -lmfhdf
# BLAS_INCLUDE     = 
# BLAS_LIB         = -lblas
# endif
# # end 64 bit section


#
# Location of the vendor supplied MPI (Message Passing Interface) software
#
ifeq ($(ESMF_COMM),mpi)
MPI_INCLUDE    += 
MPI_LIB        += -lmpi_r
MPIRUN         = ${ESMF_TOP_DIR}/scripts/mpirun.rs6000_sp
endif


#
# Location of STL files for C++
#
LOCAL_INCLUDE += -I/usr/vacpp/include

# UNUSED??
#
# location of smp library
#
# XLSMP_LIB      = -L/usr/lpp/xlsmp/aix51 -lxlsmp


# ######################### Common compiler options #####################
DARCH			= -DAIX
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

NO_AUTO_PARALLEL	= -qsmp=noauto
NO_INLINING		= -Q
NO_LINE_DIRECTIVES	= -P

FPP_PREFIX		= -WF,
F_FREECPP               = -qsuffix=cpp=F90
F_FIXCPP                = -qfixed=132 -qsuffix=cpp=f90
F_FREENOCPP             = -qsuffix=f=F
F_FIXNOCPP              = -qfixed=132 -qsuffix=f=f        

C_SLFLAG		= -L


############################################################

# default name
C_CC = cc_r

ifeq ($(ESMF_COMM),mpi)
  MPIPREFIX=mp
  C_CC = mpCC_r
  # is this still needed?
  MPI64_LIB  = -lmpi_r -lvtd_r -lmpci_r
endif
ifeq ($(ESMF_COMM),mpiuni)
  C_CC=xlC_r
endif

C_CXX			= $(MPIPREFIX)CC_r 
C_FC			= $(MPIPREFIX)xlf90_r 

############################################################

ifeq ($(ESMF_PREC),32)
PARCH			= rs6000
AR32_64			= ar -X 32_64

# compilers and flags
REAL8			= -qrealsize=8

# linkopts are used when linking an executable using the shared lib
C_LINKOPTS += -brtl -bmaxdata:0x80000000 -qcheck

# linking with shared libs
C_SL_LIBLINKER 		= $(C_CC) 

endif
# end 32 bit section

############################################################

ifeq ($(ESMF_PREC),64)
PARCH		= rs6000_64
AR		= ar -X64
RANLIB		= ranlib -X64
AR32_64		= ar -X 32_64

# compilers
C_64BIT		= -q64
REAL8		= -qrealsize=8

C_CC		+= $(C_64BIT)
C_CXX		+= $(C_64BIT)
C_FC		+= $(C_64BIT)

# linkopts are used when linking an executable using the shared lib
C_LINKOPTS += -brtl -bmaxdata:0x80000000 -bmaxstack:0x1000000 \
               -bloadmap:loadmap.txt

# linking with shared libs
C_SL_LIBLINKER 	= $(C_CC) $(C_64BIT)

endif
# end 64 bit section


# start of common section
#
C_CCV		= lslpp -l | fgrep xlC
C_CXXV		= lslpp -l | fgrep xlC
C_FCV		= lslpp -l | fgrep xlf

C_CXXF90LIBS	= -L. -lm_r -lxlf90_r -lC_r
C_F90CXXLIBS	= -L. -lxlf90_r -lC_r


# ------------------------- BOPT - g options ------------------------------
G_CFLAGS	+= $(COM_ALL_DEBUG_FLAGS)
G_FFLAGS	+= $(COM_ALL_DEBUG_FLAGS)

# ------------------------- BOPT - none options ------------------------------
X_CFLAGS	+=
X_FFLAGS	+=

# ------------------------- BOPT - O options ------------------------------
O_CFLAGS	+= $(COM_ALL_OPT_FLAGS)
O_FFLAGS	+= $(COM_OPT_FLAG) $(COM_WARN_FLAG)
#


# shared lib section - this platform does make a shared lib.

# libopts are the options used when converting lib.a to lib.so
SL_LIBOPTS  += -G -qmkshrobj $(C_F90CXXLIBS) $(MPI_LIB)


# unused?  TODO: remove
#C_CXXSOLIBS	= -L. -lm_r -lxlf90_r -lC_r
# C_SL_LIBLINKER is defined in the 32/64 bit sections above. 

# end of common settings

