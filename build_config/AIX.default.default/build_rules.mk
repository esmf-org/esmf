#  $Id: build_rules.mk,v 1.19 2004/11/22 23:17:08 nscollins Exp $
#
#  AIX.default.default.mk
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

ifeq ($(ESMF_COMM),mpi)
  MPIPREFIX=mp
  CC = mpCC_r
endif
ifeq ($(ESMF_COMM),mpiuni)
  CC=xlC_r
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

ifeq ($(ESMF_NO_IOCODE),true)
BLAS_LIB         =
LAPACK_LIB       =
NETCDF_LIB       = -lnetcdf_stubs
NETCDF_INCLUDE   = -I${ESMF_DIR}/src/Infrastructure/stubs/netcdf_stubs
HDF_LIB          =
HDF_INCLUDE      =
else

BLAS_LIB         = -lblas ${FC_LIB}

ifeq ($(ESMF_PREC),32)
# LAPACK_LIB     =  -lesslp2 -L/usr/local/lib32/r4i4 -llapack
LAPACK_LIB       =  -L/usr/local/lib32/r4i4 -llapack -lessl
NETCDF_INCLUDE   = -I/usr/local/include
NETCDF_LIB       = -L/usr/local/lib32/r4i4 -lnetcdf
HDF_INCLUDE	 = -I/usr/local/include/hdf
HDF_LIB		 = -L/usr/local/lib32/r4i4 -lmfhdf
endif
# end 32 bit section
ifeq ($(ESMF_PREC),64)
# LAPACK_LIB     =  -lesslp2 -L/usr/local/lib64/r4i4 -llapack
LAPACK_LIB       =  -L/usr/local/lib64/r4i4 -llapack -lessl
NETCDF_INCLUDE = -I/usr/local/include
NETCDF_LIB     = -L/usr/local/lib64/r4i4 -lnetcdf
HDF_INCLUDE	 = -I/usr/local/include/hdf
HDF_LIB		 = -L/usr/local/lib64/r4i4 -lmfhdf
endif
# end 64 bit section
endif
#end of io bypass section

#
# Location of MPI (Message Passing Interface) software
#
ifeq ($(ESMF_COMM),mpi)
#MPI_LIB        = -L/usr/lpp/ppe.poe/lib -lmpi_r
MPI_LIB        = -lmpi_r
MPI_INCLUDE    = 
MPIRUN         = ${ESMF_TOP_DIR}/scripts/mpirun.rs6000_sp
#MPI64_LIB      = -L/usr/local/mpi64/ppe.poe/lib -lmpi_r -lvtd_r \
#		 -L/usr/local/mpi64/ppe.poe/lib/us -lmpci_r
MPI64_LIB      = -lmpi_r -lvtd_r -lmpci_r
endif

ifeq ($(ESMF_COMM),mpiuni)
MPI_HOME       = ${ESMF_DIR}/src/Infrastructure/stubs/mpiuni
MPI_INCLUDE    = -I${MPI_HOME}
endif

#
# Location of STL files for C++
#
LOCAL_INCLUDE += -I/usr/vacpp/include

#
# location of smp library
#
XLSMP_LIB      = -L/usr/lpp/xlsmp/aix51 -lxlsmp


# ######################### Common compiler options #####################
DARCH			= -DAIX
DsysARCH                = -DsysAIX

COM_MEMCHECK_FLAG      = -qcheck
COM_FULLPATH_FLAG      = -qfullpath
COM_DEBUG_FLAG         = -g
COM_ALL_DEBUG_FLAGS    = -g $(COM_MEMCHECK_FLAG) $(COM_FULLPATH_FLAG)
COM_MAXMEM_FLAG        = -qmaxmem=4000
COM_NOWARN_FLAG        = -w
COM_SPILL_FLAG         = -qspill=3000
COM_OPT_FLAG           = -O3
COM_ALL_OPT_FLAGS      = -O3 $(COM_MAXMEM_FLAG) $(COM_NOWARN_FLAG) $(COM_SPILL_FLAG)
COM_PLAIN_FLAG         =

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

ifeq ($(ESMF_PREC),32)
PARCH			= rs6000
AR			= ar
RANLIB			= ranlib
# Fortran compiler 
AR32_64			= ar -X 32_64
C_64BIT			= -q64
REAL8			= -qrealsize=8
FPPFLAGS		= $(addprefix $(FPP_PREFIX), $(FPPOPTS))
# C and Fortran compiler
C_CC			= $(MPIPREFIX)cc_r 
C_FC			= $(MPIPREFIX)xlf90_r 
C_FC_MOD		= -I
C_CLINKER		= $(MPIPREFIX)cc_r -bmaxdata:0x70000000  -qcheck 
# you may need to add -bI:/usr/lpp/xlf/lib/lowsys.exp to C_LINKER
C_FLINKER		= $(MPIPREFIX)xlf90_r -bmaxdata:0x70000000 -lC_r -qcheck 
# C++ compiler
CXX_CC			= $(CC) 
CXX_FC			= $(MPIPREFIX)xlf90_r
CXX_CLINKER		= $(CC) -qcheck 
CXX_FLINKER		= $(MPIPREFIX)xlf90_r -qcheck 
SL_LIB_LINKER 		= $(CC) -bloadmap:loadmap.txt -L$(ESMF_LIBDIR)
endif
# end 32 bit section

############################################################

ifeq ($(ESMF_PREC),64)
PARCH			= rs6000_64
AR			= ar -X64
RANLIB			= ranlib -X64
# Fortran compiler 
AR32_64			= ar -X 32_64
C_64BIT			= -q64
REAL8			= -qrealsize=8
FPPFLAGS		= $(addprefix $(FPP_PREFIX), $(FPPOPTS))
# C and Fortran
C_CC			= $(MPIPREFIX)cc_r -q64
C_FC			= $(MPIPREFIX)xlf90_r -q64
C_FC_MOD		= -I
C_CLINKER		= $(MPIPREFIX)cc_r -q64
# you may need to add -bI:/usr/lpp/xlf/lib/lowsys.exp to C_LINKER
C_FLINKER		= $(MPIPREFIX)xlf90_r -q64 -lC_r
# C++ compiler
CXX_CC			= $(CC) -q64
CXX_FC			= $(MPIPREFIX)xlf90_r -q64
CXX_CLINKER		= $(CC) -q64
CXX_FLINKER		= $(CC) -q64
SL_LIB_LINKER 		= $(CC) -q64 -bloadmap:loadmap.txt -L$(ESMF_LIBDIR)
endif
# end 64 bit section


# start of common section
# ######################### C and Fortran compiler ########################
#
C_CCV			= lslpp -l | fgrep xlC
C_FCV			= lslpp -l | fgrep xlf
C_SYS_LIB		= /usr/lib/libxlf_r.a /usr/lib/libxlf90_r.a  -lisode
# ---------------------------- BOPT - g options ----------------------------
G_COPTFLAGS		= $(COM_ALL_DEBUG_FLAGS)
G_FOPTFLAGS		= $(COM_ALL_DEBUG_FLAGS)
# ----------------------------- BOPT - O options -----------------------------
O_COPTFLAGS		= $(COM_ALL_OPT_FLAGS)
O_FOPTFLAGS		= $(COM_OPT_FLAG) $(COM_WARN_FLAG)
FCPPFLAGS		= ${ESMC_INCLUDE} ${PCONF} ${ESMC_PARCH} ${FPPFLAGS} $(FCPP_EXHAUSTIVE)
# ########################## C++ compiler ##################################
CXX_CCV			= lslpp -l | fgrep xlC
CXX_SYS_LIB		= /usr/lib/libxlf_r.a /usr/lib/libxlf90_r.a  -lcomplex -lisode
CXX_SYS_LIBS		= -lC_r
C_CXXF90LD		= ${CXX_CC}
C_F90CXXLD		= ${CXX_FC}

C_CXXF90LIBS		= -L. -lm_r -lxlf90_r -lC_r

C_F90CXXLIBS		= -L. -lxlf90_r -lC_r

C_CXXSO			= $(CC) -G

C_CXXSOLIBS		= -L. -lm_r -lxlf90_r -lC_r

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
SL_F_LINKER = $(F90CXXLD) -bmaxdata:0x80000000 -bmaxstack:0x1000000 -bloadmap:loadmap.txt
SL_C_LINKER = $(CXXF90LD) -bmaxdata:0x80000000 -bmaxstack:0x1000000 -bloadmap:loadmap.txt

# end of common settings

