# $Id: Linux.intel.default.mk,v 1.1 2003/09/25 21:21:53 flanigan Exp $
#
# Linux.intel.default.mk
#

#
# Default MPI setting.
#
ifndef ESMF_COMM
export ESMF_COMM := mpiuni
endif


# Location of MPI (Message Passing Interface) software

ifeq ($(ESMF_COMM),lam)
# this section is set up for LAM mpi
#ESMC_MPIRUN   = 
MPI_HOME       = 
MPI_LIB        = -lmpi -llam
MPI_INCLUDE    = 
MPIRUN         =  mpirun
endif

ifeq ($(ESMF_COMM),mpich)
# this section is set up for MPICH
#ESMC_MPIRUN   = 
MPI_HOME       = /soft/apps/packages/mpich-gm-1.2.5..9-pre6-gm-1.6.3-intel-7.0
MPI_LIB        = -L$(MPI_HOME)/lib -lmpi 
MPI_INCLUDE    = -I$(MPI_HOME)/include
MPIRUN         =  mpirun
endif

ifeq ($(ESMF_COMM),mpiuni)
# this section is set up to bypass all MPI
# #ESMC_MPIRUN      = 
# MPI_HOME       = ${ESMF_DIR}/src/Infrastructure/mpiuni
# MPI_LIB        = -lmpiuni
# MPI_INCLUDE    = -I${MPI_HOME}
# MPIRUN         =  ${MPI_HOME}/mpirun
endif


# MP_LIB is for openMP
MP_LIB          = 
# For pthreads (or omp)
THREAD_LIB      = -lpthread

#
AR		   = ar
AR_FLAGS	   = cr
RM		   = rm -f
OMAKE		   = ${MAKE}
RANLIB		   = ranlib
SHELL		   = /bin/sh
SED		   = /bin/sed
SH_LD		   = ecc 
# ################## Compilers, Linkers, and Loaders ########################
#
ifeq ($(ESMF_PREC),64)
C_CC		   = ecc -size_lp64
C_FC		   = efc -size_lp64
C_CLINKER	   = ecc -size_lp64
C_FLINKER	   = efc -size_lp64
CXX_CC		   = ecc  -size_lp64
CXX_FC		   = efc -mp  -size_lp64
CXX_CLINKER	   = ecc -size_lp64
CXX_FLINKER	   = ecc -size_lp64
C_F90CXXLD         = efc -mp -size_lp64
C_CXXF90LD         = ecc  -size_lp64
C_CXXSO            = ecc -shared -size_lp64
endif
ifeq ($(ESMF_PREC),32)
C_CC		   = icc
C_FC		   = ifc
C_CLINKER	   = icc
C_FLINKER	   = ifc
CXX_CC		   = icc
CXX_FC		   = ifc -mp
CXX_CLINKER	   = icc
CXX_FLINKER	   = icc
C_F90CXXLD         = ifc -mp
C_CXXF90LD         = icc 
C_CXXSO            = icc -shared 
endif
# ######################### C and Fortran compiler flags ####################
C_FC_MOD           = -I
C_CLINKER_SLFLAG   = -Wl,-rpath,
C_FLINKER_SLFLAG   = -Wl,-rpath,
C_CCV		   = ${C_CC} -V -c -w -x c
C_FCV              = efc -V -c -w
C_SYS_LIB	   = -ldl -lc -lg2c -lm
#C_SYS_LIB	   = -ldl -lc -lf2c -lm
#C_SYS_LIB	   = -ldl -lc /usr/lib/libf2c.a -lm  #Use /usr/lib/libf2c.a if that's what your f77 uses.
# ---------------------------- BOPT - g options ----------------------------
G_COPTFLAGS	   = -g 
G_FOPTFLAGS	   = -g
# ----------------------------- BOPT - O options -----------------------------
O_COPTFLAGS	   = -O 
O_FOPTFLAGS	   = -O
# ########################## C++ compiler flags ##############################
#
CXX_CLINKER_SLFLAG = -Wl,-rpath,
CXX_FLINKER_SLFLAG = -Wl,-rpath,
CXX_CCV		   = ${CXX_CC} -V -c -w -x c++
#CXX_SYS_LIB	   = -ldl -lc -lf2c -lm
CXX_SYS_LIB	   = -ldl -lc -lg2c -lm
#CXX_SYS_LIB	   = -ldl -lc /usr/lib/libf2c.a -lm
C_F90CXXLIBS       = -lcprts
C_CXXF90LIBS       = -lCEPCF90 -lIEPCF90 -lF90 -lintrins
# ------------------------- BOPT - g_c++ options ------------------------------
GCXX_COPTFLAGS	   = -g 
GCXX_FOPTFLAGS	   = -g
# ------------------------- BOPT - O_c++ options ------------------------------
OCXX_COPTFLAGS	   = -O 
OCXX_FOPTFLAGS	   = -O
# -------------------------- BOPT - g_complex options ------------------------
GCOMP_COPTFLAGS	   = -g
GCOMP_FOPTFLAGS	   = -g
# --------------------------- BOPT - O_complex options -------------------------
OCOMP_COPTFLAGS	   = -O
OCOMP_FOPTFLAGS	   = -O
##################################################################################

PARCH		   = linux_intel

SL_SUFFIX   =
SL_LIBOPTS  =
SL_LINKOPTS =
SL_F_LINKER = $(F90CXXLD)
SL_C_LINKER = $(CXXF90LD)
SL_LIB_LINKER = $(CXXF90LD)
SL_LIBS_TO_MAKE = libesmf liboldworld

#########

.F90.o:
	${FC} -c ${C_FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} ${FFLAGS} -cpp -FR ${FCPPFLAGS} ${ESMC_INCLUDE} $<

.F.o:
	${FC} -c ${C_FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} ${FFLAGS} -cpp0 -FR ${ESMC_INCLUDE} $<

.f90.o:
	${FC} -c ${FOPTFLAGS} ${FFLAGS} -cpp -FI ${FCPPFLAGS} ${ESMC_INCLUDE} $<

.f.o:
	${FC} -c ${FOPTFLAGS} ${FFLAGS} -cpp0 -FI ${ESMC_INCLUDE} $<

.c.o:
	${CC} -c ${COPTFLAGS} ${CFLAGS} ${CCPPFLAGS} ${ESMC_INCLUDE} $<

.C.o:
	${CXX} -c ${COPTFLAGS} ${CFLAGS} ${CCPPFLAGS} ${ESMC_INCLUDE} $<

.F90.a:
	${FC} -c ${C_FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} ${FFLAGS} -cpp -FR ${FCPPFLAGS} ${ESMC_INCLUDE} $<
	${AR} ${AR_FLAGS} ${LIBNAME} $*.o
	${RM} $*.o

.F.a:
	${FC} -c ${C_FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} ${FFLAGS} -cpp0 -FR ${ESMC_INCLUDE} $<
	${AR} ${AR_FLAGS} ${LIBNAME} $*.o
	${RM} $*.o

.f90.a:
	${FC} -c ${FOPTFLAGS} ${FFLAGS} -cpp -FI ${FCPPFLAGS} ${ESMC_INCLUDE} $<
	${AR} ${AR_FLAGS} ${LIBNAME} $*.o
	${RM} $*.o

.f.a:
	${FC} -c ${FOPTFLAGS} ${FFLAGS} -cpp0 -FI ${ESMC_INCLUDE} $<
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
shared:


