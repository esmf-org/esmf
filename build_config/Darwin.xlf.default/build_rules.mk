#  $Id: build_rules.mk,v 1.3 2004/05/18 11:31:00 nscollins Exp $
#
#  Darwin.xlf.default.mk
#
#  (WARNING: just copied from AIX - needs massive cleanup!)
#

#
# Default MPI setting.
#
ifndef ESMF_COMM
export ESMF_COMM := mpiuni
endif
ifeq ($(ESMF_COMM),default)
export ESMF_COMM := mpiuni
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
NETCDF_LIB       =
NETCDF_INCLUDE   =
HDF_LIB          =
HDF_INCLUDE      =
else
BLAS_LIB         = -L/sw/lib -latlas
LAPACK_LIB       = -L/sw/lib -llapack
NETCDF_LIB       = -L/sw/lib -lnetcdf
NETCDF_INCLUDE   = -I/sw/include
HDF_LIB          = -L/sw/lib/ -lmfhdf -ldf -ljpeg -lz
HDF_INCLUDE      = -I/sw/include
endif

# Location of MPI (Message Passing Interface) software

# comment in one or the other, depending on whether you have
# installed the mpich library.  (the first section assumes
# it is installed under /usr/local - change MPI_HOME if other dir.)

ifeq ($(ESMF_COMM),lam)
# with lam-mpi installed in /usr/local:
MPI_HOME       =
MPI_LIB        = -lmpi -llam
MPI_INCLUDE    =
MPIRUN         =  mpirun
endif

ifeq ($(ESMF_COMM),mpich)
# with mpich installed in /usr/local:
ESMC_MPIRUN      = mpirun
MPI_HOME       =  /usr/local
MPI_LIB        = -lmpich -lpmpich
MPI_INCLUDE    = -I${MPI_HOME}/include -DESMF_MPICH=1
MPIRUN         =  ${MPI_HOME}/bin/mpirun
endif

ifeq ($(ESMF_COMM),mpiuni)
# without mpich installed:
MPI_HOME       = ${ESMF_DIR}/src/Infrastructure/mpiuni
MPI_LIB        = -lmpiuni
MPI_INCLUDE    = -I${MPI_HOME}
MPIRUN         =  ${MPI_HOME}/mpirun
endif


# ######################### Common compiler options #####################
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


############################################################
#

#
AR			= ar
AR_FLAGS		= cr
AR_EXTRACT              = -x
RM			= rm -f
OMAKE			= ${MAKE}
RANLIB			= ranlib
SHELL			= /bin/sh
SED			= /usr/bin/sed

#
# C compiler
#
RESTRICTED_POINTERS	= -qkeyword=restrict
NO_AUTO_PARALLEL	= -qsmp=noauto
NO_INLINING		= -Q
NO_LINE_DIRECTIVES	= -P
#
# Fortran compiler
#
REAL8			= -qrealsize=8
STRICT			= -qstrict
FPP_PREFIX		= -WF,
FPPFLAGS		= $(addprefix $(FPP_PREFIX), $(FPPOPTS))
F_FREECPP               = -qsuffix=cpp=F90
F_FIXCPP                = -qfixed=132 -qsuffix=cpp=f90
F_FREENOCPP             = -qsuffix=f=F
F_FIXNOCPP              = -qfixed=132 -qsuffix=f=f        
#
# C and Fortran compiler
#
C_CC			= xlc_r 
C_FC			= xlf95_r 
C_FC_MOD		= -I
C_CLINKER_SLFLAG	= -L
C_FLINKER_SLFLAG	= -L
C_CLINKER		= xlc_r -bmaxdata:0x70000000  -qcheck 
# you may need to add -bI:/usr/lpp/xlf/lib/lowsys.exp to C_LINKER
C_FLINKER		= xlf90_r -bmaxdata:0x70000000 -lC_r -qcheck 
C_CCV			= which xlC_r
C_FCV			= which xlf95_r
C_SYS_LIB		= /usr/lib/libxlf_r.a /usr/lib/libxlf90_r.a  -lisode
# ---------------------------- BOPT - g options ----------------------------
G_COPTFLAGS		= $(COM_ALL_DEBUG_FLAGS)
G_FOPTFLAGS		= $(COM_ALL_DEBUG_FLAGS)
# ----------------------------- BOPT - O options -----------------------------
O_COPTFLAGS		= $(COM_ALL_OPT_FLAGS)
O_FOPTFLAGS		= $(COM_OPT_FLAG) $(COM_WARN_FLAG)
FCPPFLAGS		= ${ESMC_INCLUDE} ${PCONF} ${ESMC_PARCH} ${FPPFLAGS} $(FCPP_EXHAUSTIVE)
#
# C++ compiler
#
###
CXX_CC			= xlC_r 
CXX_FC			= xlf90_r
CXX_CLINKER_SLFLAG	= -L
CXX_FLINKER_SLFLAG	= -L
CXX_CLINKER		= xlC_r -qcheck 
CXX_FLINKER		= xlf90_r -qcheck 
CXX_CCV			= which xlc_r
CXX_SYS_LIB		= /usr/lib/libxlf_r.a /usr/lib/libxlf90_r.a  -lcomplex -lisode
CXX_SYS_LIBS		= -lC_r
C_CXXF90LD		= ${CXX_CC}
C_F90CXXLD		= ${CXX_FC}

C_CXXF90LIBS		= -L. -lm_r -lxlf90_r -lC_r

C_F90CXXLIBS		= -L. -lxlf90_r -lC_r

C_CXXSO			= xlC_r -G

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
PARCH			= rs6000


SL_SUFFIX   = so
SL_LIBOPTS  = -G -qmkshrobj $(C_F90CXXLIBS) $(MPI_LIB)
SL_LINKOPTS = -brtl
SL_LIB_LINKER = xlC_r -bloadmap:loadmap.txt -L$(ESMF_LIBDIR)
#SL_LIB_LINKER = xlC -bloadmap:loadmap.txt -L$(ESMF_LIBDIR)
SL_F_LINKER = $(F90CXXLD) -bmaxdata:0x80000000 -bmaxstack:0x1000000 -bloadmap:loadmap.txt
SL_C_LINKER = $(CXXF90LD) -bmaxdata:0x80000000 -bmaxstack:0x1000000 -bloadmap:loadmap.txt
SL_LIBS_TO_MAKE = libesmf liboldworld

# SL_SUFFIX   =
# SL_LIBOPTS  =
# SL_LINKOPTS =
# SL_F_LINKER = $(F90CXXLD) 
# SL_C_LINKER = $(CXXF90LD) 


#############
#
# Set shared dependent on build_shared to build .so lib.
#
#shared: build_shared
shared:



