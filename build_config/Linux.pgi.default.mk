# $Id: Linux.pgi.default.mk,v 1.1 2003/09/25 21:21:53 flanigan Exp $
#
#  Linux.pgi.default.mk
#

#
#  Make sure precession is 32.
#
ESMF_PREC = 32

#
# Default MPI setting.
#
ifndef ESMF_COMM
export ESMF_COMM := mpiuni
endif


#############


# Location of MPI (Message Passing Interface) software

#ESMC_MPIRUN      = 
MPI_HOME       = ${ESMF_DIR}/src/Infrastructure/mpiuni
MPI_LIB        = -lmpiuni
MPI_INCLUDE    = -I${MPI_HOME}
MPIRUN         =  ${MPI_HOME}/mpirun

# MP_LIB is for openMP
MP_LIB          = -lpgmp
# For pthreads (or omp)
THREAD_LIB      = -lpthread

#############
#
AR		   = ar
AR_FLAGS	   = cr
RM		   = rm -f
OMAKE		   = ${MAKE}
RANLIB		   = ranlib
SHELL		   = /bin/sh
SED		   = /bin/sed
SH_LD		   = pgcc 
# ######################### C and Fortran compiler ########################
#
C_CC		   = pgcc -mp
C_FC		   = pgf90 -mp
C_FC_MOD           = -I
C_CLINKER_SLFLAG   = -Wl,-rpath,
C_FLINKER_SLFLAG   = -Wl,-rpath,
C_CLINKER	   = pgcc
C_FLINKER	   = pgf90
C_CCV		   = ${C_CC} --version
C_FCV              = pgf90 -mp
C_SYS_LIB	   = -ldl -lc -lg2c -lm
#C_SYS_LIB	   = -ldl -lc -lf2c -lm
#C_SYS_LIB	   = -ldl -lc /usr/lib/libf2c.a -lm  #Use /usr/lib/libf2c.a if that's what your f77 uses.
# ---------------------------- BOPT - g options ----------------------------
G_COPTFLAGS	   = -g 
G_FOPTFLAGS	   = -g
# ----------------------------- BOPT - O options -----------------------------
O_COPTFLAGS	   = -O 
O_FOPTFLAGS	   = -O
# ########################## C++ compiler ##################################
#
CXX_CC		   = pgCC -tlocal
# TODO:  upgrade PGI suite?  "pgCC -mp" disables C++ exception handling !
#        should be ok for now since we're not using OpenMP in the 1st ESMF ver.
#CXX_CC		   = pgCC -mp -tlocal
CXX_FC		   = pgf90 -mp -tlocal
CXX_CLINKER_SLFLAG = -Wl,-rpath,
CXX_FLINKER_SLFLAG = -Wl,-rpath,
CXX_CLINKER	   = pgCC
CXX_FLINKER	   = pgCC
CXX_CCV		   = ${CXX_CC} --version
#CXX_SYS_LIB	   = -ldl -lc -lf2c -lm
CXX_SYS_LIB	   = -ldl -lc -lg2c -lm
#CXX_SYS_LIB	   = -ldl -lc /usr/lib/libf2c.a -lm
C_F90CXXLD         = pgf90 -mp
C_F90CXXLIBS       = -lpgc -lstd -lC
C_CXXF90LD         = pgCC 
C_CXXF90LIBS       = -lpgf90 -lpgf90_rpm1 -lpgf902 -lpgf90rtl -lpgftnrtl
C_CXXSO            = pgCC -shared
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

PARCH		   = linux

SL_SUFFIX   =
SL_LIBOPTS  =
SL_LINKOPTS =
SL_F_LINKER = $(F90CXXLD)
SL_C_LINKER = $(CXXF90LD)
SL_LIB_LINKER = $(CXXF90LD)
SL_LIBS_TO_MAKE = libesmf liboldworld


#########

.F90.o:
	${FC} -c ${C_FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} ${FFLAGS} ${FCPPFLAGS} -Mpreprocess -Mfreeform ${ESMC_INCLUDE} $<

.F.o:
	${FC} -c ${C_FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} ${FFLAGS} -Mfreeform ${ESMC_INCLUDE} $<

.f90.o:
	${FC} -c ${FOPTFLAGS} ${FFLAGS} ${FCPPFLAGS} -Mpreprocess -Mnofreeform ${ESMC_INCLUDE} $<

.f.o:
	${FC} -c ${FOPTFLAGS} ${FFLAGS} -Mnofreeform ${ESMC_INCLUDE} $<

.c.o:
	${CC} -c ${COPTFLAGS} ${CFLAGS} ${CCPPFLAGS} ${ESMC_INCLUDE} $<

.C.o:
	${CXX} -c ${COPTFLAGS} ${CFLAGS} ${CCPPFLAGS} ${ESMC_INCLUDE} $<

.F90.a:
	${FC} -c ${C_FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} ${FFLAGS} ${FCPPFLAGS} -Mpreprocess -Mfreeform ${ESMC_INCLUDE} $<
	${AR} ${AR_FLAGS} ${LIBNAME} $*.o
	${RM} $*.o

.F.a:
	${FC} -c ${C_FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} ${FFLAGS} -Mfreeform ${ESMC_INCLUDE} $<
	${AR} ${AR_FLAGS} ${LIBNAME} $*.o
	${RM} $*.o

.f90.a:
	${FC} -c ${FOPTFLAGS} ${FFLAGS} ${FCPPFLAGS} -Mpreprocess -Mnofreeform ${ESMC_INCLUDE} $<
	${AR} ${AR_FLAGS} ${LIBNAME} $*.o
	${RM} $*.o

.f.a:
	${FC} -c ${FOPTFLAGS} ${FFLAGS} -Mnofreeform ${ESMC_INCLUDE} $<
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


