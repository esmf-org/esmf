#  $Id: build_rules.mk,v 1.5 2003/11/11 19:01:47 nscollins Exp $
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
BLAS_LIB         = -lblas ${FC_LIB}
# LAPACK_LIB     =  -lesslp2 -L/usr/local/lib32/r4i4 -llapack
LAPACK_LIB       =  -L/usr/local/lib32/r4i4 -llapack -lessl
NETCDF_INCLUDE   = -I/usr/local/include
NETCDF_LIB       = -L/usr/local/lib32/r4i4 -lnetcdf
NETCDF64_INCLUDE = -I/usr/local/include
NETCDF64_LIB     = -L/usr/local/lib64/r4i4 -lnetcdf
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
HDF_INCLUDE    = -I/usr/local/include/hdf
endif

ifeq ($(ESMF_COMM),mpiuni)
endif

#
# Location of STL files for C++
#
LOCAL_INCLUDE += -I/usr/vacpp/include

#
# location of smp library
#
XLSMP_LIB      = -L/usr/lpp/xlsmp/aix51 -lxlsmp



############################################################
#
# File rs6000_sp/base_variables
#
ifeq ($(ESMF_PREC),32)

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
# ######################### Common compiler options #####################
COM_MEMCHECK_FLAG      = -qcheck
COM_FULLPATH_FLAG      = -qfullpath
COM_DEBUG_FLAG         = -g 
COM_ALL_DEBUG_FLAGS    = -g $(COM_MEMCHECK_FLAG) $(COM_FULLPATH_FLAG)
COM_MAXMEM_FLAG	       = -qmaxmem=4000
COM_NOWARN_FLAG        = -w
COM_SPILL_FLAG         = -qspill=3000
COM_OPT_FLAG           = -O3
COM_ALL_OPT_FLAGS      = -O3 $(COM_MAXMEM_FLAG) $(COM_NOWARN_FLAG) $(COM_SPILL_FLAG)
COM_PLAIN_FLAG         = 

# ######################### C compiler options ######################
#
RESTRICTED_POINTERS	= -qkeyword=restrict
# ######################### Fortran compiler options ######################
#
AR32_64			= ar -X 32_64
C_64BIT			= -q64
DARCH			= -DAIX
DsysARCH                = -DsysAIX
NO_AUTO_PARALLEL	= -qsmp=noauto
NO_INLINING		= -Q
NO_LINE_DIRECTIVES	= -P
REAL8			= -qrealsize=8
STRICT			= -qstrict
FPP_PREFIX		= -WF,
FPPFLAGS		= $(addprefix $(FPP_PREFIX), $(FPPOPTS))
F_FREECPP               = -qsuffix=cpp=F90
F_FIXCPP                = -qfixed=132 -qsuffix=cpp=f90
F_FREENOCPP             = -qsuffix=f=F
F_FIXNOCPP              = -qfixed=132 -qsuffix=f=f        
# ######################### C and Fortran compiler ########################
#
C_CC			= mpcc_r 
C_FC			= mpxlf90_r 
C_FC_MOD		= -I
C_CLINKER_SLFLAG	= -L
C_FLINKER_SLFLAG	= -L
C_CLINKER		= mpcc_r -bmaxdata:0x70000000  -qcheck 
# you may need to add -bI:/usr/lpp/xlf/lib/lowsys.exp to C_LINKER
C_FLINKER		= mpxlf90_r -bmaxdata:0x70000000 -lC_r -qcheck 
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
#
###  on one of our rs6000s the mpCC_r system cover script will not find 
###  the actual xlC_r compiler without adding the dir /usr/vacpp/bin to 
###  your search path.  on the other rs6000 it works as expected
###  without any environment changes.  we were told that mpcc_r should behave
###  the same as mpCC_r and it works without an additional dir in the search
###  path, but it does *not* add the same default libraries and causes
###  problems, especially with cross linking in fortran and with -qheapdebug.
###  so i am choosing to go back to calling mpCC_r and putting up with
###  the user support problems i know this is going to cause.   nsc 4/2/2003
###
###  call mpcc_r instead of mpCC_r.   nsc  1/29/2003
CXX_CC			= mpCC_r 
CXX_FC			= mpxlf90_r
CXX_CLINKER_SLFLAG	= -L
CXX_FLINKER_SLFLAG	= -L
CXX_CLINKER		= mpCC_r -qcheck 
CXX_FLINKER		= mpxlf90_r -qcheck 
CXX_CCV			= lslpp -l | fgrep xlC
CXX_SYS_LIB		= /usr/lib/libxlf_r.a /usr/lib/libxlf90_r.a  -lcomplex -lisode
CXX_SYS_LIBS		= -lC_r
C_CXXF90LD		= ${CXX_CC}
C_F90CXXLD		= ${CXX_FC}

C_CXXF90LIBS		= -L. -lm_r -lxlf90_r -lC_r

C_F90CXXLIBS		= -L. -lxlf90_r -lC_r

C_CXXSO			= mpCC_r -G

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
SL_LIB_LINKER = mpCC_r -bloadmap:loadmap.txt -L$(ESMF_LIBDIR)
#SL_LIB_LINKER = xlC -bloadmap:loadmap.txt -L$(ESMF_LIBDIR)
SL_F_LINKER = $(F90CXXLD) -bmaxdata:0x80000000 -bmaxstack:0x1000000 -bloadmap:loadmap.txt
SL_C_LINKER = $(CXXF90LD) -bmaxdata:0x80000000 -bmaxstack:0x1000000 -bloadmap:loadmap.txt
SL_LIBS_TO_MAKE = libesmf liboldworld

# SL_SUFFIX   =
# SL_LIBOPTS  =
# SL_LINKOPTS =
# SL_F_LINKER = $(F90CXXLD) 
# SL_C_LINKER = $(CXXF90LD) 

endif

############################################################
#
#  File rs6000_64/base_variables
#

ifeq ($(ESMF_PREC),64)

#
#     See the file build/base_variables.defs for a complete explanation of all these fields
#
AR			= ar -X64
AR_FLAGS		= cr
AR_EXTRACT              = -x
RM			= rm -f
OMAKE			= ${MAKE}
RANLIB			= ranlib -X64
SHELL			= /bin/sh
SED			= /bin/sed
# ######################### C compiler options ######################
#
RESTRICTED_POINTERS     = -qkeyword=restrict
# ######################### Fortran compiler options ######################
#
AR32_64			= ar -X 32_64
C_64BIT			= -q64
DARCH			= -DAIX
DsysARCH                = -DsysAIX
NO_AUTO_PARALLEL	= -qsmp=noauto
NO_INLINING		= -Q
NO_LINE_DIRECTIVES	= -P
REAL8			= -qrealsize=8
STRICT			= -qstrict
FPP_PREFIX		= -WF,
FPPFLAGS		= $(addprefix $(FPP_PREFIX), $(FPPOPTS))
F_FREECPP               = -qsuffix=cpp=F90
F_FIXCPP                = -qfixed=132 -qsuffix=cpp=f90
F_FREENOCPP             = -qsuffix=f=F
F_FIXNOCPP              = -qfixed=132 -qsuffix=f=f
# ######################### C and Fortran compiler ########################
#
C_CC			= mpcc_r -q64
C_FC			= mpxlf90_r -q64
C_FC_MOD		= -I
C_CLINKER_SLFLAG	= -L
C_FLINKER_SLFLAG	= -L
C_CLINKER		= mpcc_r -q64
# you may need to add -bI:/usr/lpp/xlf/lib/lowsys.exp to C_LINKER
C_FLINKER		= mpxlf90_r -q64 -lC_r
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
#
###  this section used to call mpCC_r, but it is currently broken on
###  bluesky.  i was told that mpcc_r should compile both C and C++,
###  and it seems to work on what i've tried so far.  but if something
###  doesn't work, try setting the mpcc_r lines in this section back to
###  mpCC_r, and add /usr/vacpp/bin to your search path if you get errors
###  about xlC_r not being found.   nsc  1/29/2003
CXX_CC			= mpCC_r -q64
CXX_FC			= mpxlf90_r -q64
CXX_CLINKER_SLFLAG	= -L
CXX_FLINKER_SLFLAG	= -L
CXX_CLINKER		= mpCC_r -q64
CXX_FLINKER		= mpCC_r -q64
CXX_CCV			= lslpp -l | fgrep xlC
CXX_SYS_LIB		= /usr/lib/libxlf_r.a /usr/lib/libxlf90_r.a  -lcomplex -lisode
CXX_SYS_LIBS            = -lC_r
C_CXXF90LD		= ${CXX_CC}
C_F90CXXLD		= ${CXX_FC}

C_CXXF90LIBS            = -L. -lm_r -lxlf90_r -lC_r

C_F90CXXLIBS            = -L. -lxlf90_r -lC_r

C_CXXSO                 = mpCC_r -G

C_CXXSOLIBS             = -L. -lm_r -lxlf90_r -lC_r

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
PARCH			= rs6000_64

# SL_SUFFIX   = 
# SL_LIBOPTS  = 
# SL_LINKOPTS = 
# SL_F_LINKER = $(F90CXXLD) 
# SL_C_LINKER = $(CXXF90LD) 

SL_SUFFIX   = so
SL_LIBOPTS  = -G -qmkshrobj -q64 $(C_F90CXXLIBS) $(MPI_LIB)
SL_LINKOPTS = -brtl
SL_LIB_LINKER = mpCC_r -bloadmap:loadmap.txt -L$(ESMF_LIBDIR)
#SL_LIB_LINKER = xlC -bloadmap:loadmap.txt -L$(ESMF_LIBDIR)
SL_F_LINKER = $(F90CXXLD) -bloadmap:loadmap.txt
SL_C_LINKER = $(CXXF90LD) -bloadmap:loadmap.txt
SL_LIBS_TO_MAKE = libesmf liboldworld


endif

############################################################
#
#  File base
#
#

.F90.o:
	${FC} -c ${C_FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} -qsuffix=cpp=F90 ${FFLAGS} ${FCPPFLAGS} ${ESMC_INCLUDE} $<

.F.o:
	${FC} -c ${C_FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} -qsuffix=f=F ${FFLAGS} ${ESMC_INCLUDE} $<

.f90.o:
	${FC} -c ${FOPTFLAGS} -qfixed=132 -qsuffix=cpp=f90 ${FFLAGS} ${FCPPFLAGS} ${ESMC_INCLUDE} $<

.f.o:
	${FC} -c ${FOPTFLAGS} -qfixed=132 -qsuffix=f=f ${FFLAGS} ${ESMC_INCLUDE} $<

.c.o:
	${CC} -c ${COPTFLAGS} ${CFLAGS} ${CCPPFLAGS} ${ESMC_INCLUDE} $<

.C.o:
	${CXX} -c ${COPTFLAGS} ${CFLAGS} ${CCPPFLAGS} ${ESMC_INCLUDE} $<

.F90.a:
	${FC} -c ${C_FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} -qsuffix=cpp=F90 ${FFLAGS} ${FCPPFLAGS} ${ESMC_INCLUDE} $<
	${AR} ${AR_FLAGS} ${LIBNAME} $*.o
	${RM} $*.o

.F.a:
	${FC} -c ${C_FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} -qsuffix=f=F ${FFLAGS} ${ESMC_INCLUDE} $<
	${AR} ${AR_FLAGS} ${LIBNAME} $*.o
	${RM} $*.o

.f90.a:
	${FC} -c ${FOPTFLAGS} -qfixed=132 -qsuffix=cpp=f90 ${FFLAGS} ${FCPPFLAGS} ${ESMC_INCLUDE} $<
	${AR} ${AR_FLAGS} ${LIBNAME} $*.o
	${RM} $*.o

.f.a:
	${FC} -c ${FOPTFLAGS} -qfixed=72 -qsuffix=f=f ${FFLAGS} ${ESMC_INCLUDE} $<
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

#############
#
# Set shared dependent on build_shared to build .so lib.
#
shared: build_shared



