#  $Id: build_rules.mk,v 1.2 2003/10/20 22:39:48 nscollins Exp $
#
#  Linus.absoft.default.mk
#


#
#  Make sure that ESMF_PREC is set to 32
#
ESMF_PREC = 32

#
# Default MPI setting.
#
ifndef ESMF_COMM
export ESMF_COMM := mpiuni
endif
ifeq ($(ESMF_COMM),default)
export ESMF_COMM := mpiuni
endif

# if using PBS system, export this for run time
ifdef PBS_NODEFILE
export ESMF_NODES := -machinefile $(PBS_NODEFILE)
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

# Location of MPI (Message Passing Interface) software

# comment in one or the other, depending on whether you have
# installed the mpich or lam library. 

ifeq ($(ESMF_COMM),lam)
# with lam-mpi installed:
MPI_HOME       = 
MPI_LIB        = -lmpi -llam 
MPI_INCLUDE    = 
MPIRUN         =  mpirun
endif

ifeq ($(ESMF_COMM),mpich)
# with mpich installed:
MPI_LIB        = -lmpich
MPI_INCLUDE    = -DESMF_MPICH=1
MPIRUN         = mpirun $(ESMF_NODES)
endif

ifeq ($(ESMF_COMM),mpiuni)
# without mpich installed:
MPI_HOME       = ${ESMF_DIR}/src/Infrastructure/mpiuni
MPI_LIB        = -lmpiuni
MPI_INCLUDE    = -I${MPI_HOME}
MPIRUN         =  ${MPI_HOME}/mpirun
endif

# MP_LIB is for openMP
#MP_LIB          = 
#MP_INCLUDE      = 
# For pthreads (or omp)
THREAD_LIB      = 


############################################################
#
#  File base_variables
#
#

#
#     See the file build/base_variables.defs for a complete explanation of all these fields
#
AR		   = ar
AR_FLAGS	   = cr
RM		   = rm -f
OMAKE		   = ${MAKE}
RANLIB		   = ranlib
SHELL		   = /bin/sh
SED		   = /usr/bin/sed
SH_LD		   = cc
# ######################### C and Fortran compiler ########################
#
ifneq ($(ESMF_COMM),mpich)
C_CC                = cc
C_FC                = f95
CXX_CC              = g++ -fPIC
CXX_FC              = f95 -YEXT_NAMES=LCS -s 
endif

ifeq ($(ESMF_COMM),mpich)
C_CC               = mpicc
C_FC               = mpif90 
CXX_CC             = mpiCC -fPIC
CXX_FC             = mpif90 -YEXT_NAMES=LCS -s 
endif

C_FC_MOD           = -p
C_CLINKER_SLFLAG   = -Wl,-rpath,
C_FLINKER_SLFLAG   = -Wl,-rpath,
C_CLINKER	   = ${C_CC}
C_FLINKER	   = ${C_FC}
C_CCV		   = ${C_CC} --version
C_FCV              = f90fe -V    # docs say f95 -V should work but causes error
C_SYS_LIB	   = ${MPI_LIB} -ldl -lc -lg2c -lm
#C_SYS_LIB	   = -ldl -lc /usr/lib/libf2c.a -lm  #Use /usr/lib/libf2c.a if that's what your f77 uses.
# ---------------------------- BOPT - g options ----------------------------
G_COPTFLAGS	   = -g 
G_FOPTFLAGS	   = -g 
# ----------------------------- BOPT - O options -----------------------------
O_COPTFLAGS	   = -O 
O_FOPTFLAGS	   = -O
# ########################## Fortran compiler ##############################
#
FFLAGS          = -YEXT_NAMES=LCS -s 
F_FREECPP       = -ffree
F_FIXCPP        = -ffixed
F_FREENOCPP     = -ffree
F_FIXNOCPP      = -ffixed
# ########################## C++ compiler ##################################
#
CXX_CLINKER_SLFLAG = -Wl,-rpath,
CXX_FLINKER_SLFLAG = -Wl,-rpath,
CXX_CLINKER	   = ${CXX_CC}
CXX_FLINKER	   = ${CXX_CC}
CXX_CCV		   = ${CXX_CC} --version
CXX_SYS_LIB	   = ${MPI_LIB} -ldl -lc -lg2c -lm
C_F90CXXLD         = ${CXX_CC}
C_F90CXXLIBS       = ${MPI_LIB} -lstdc++ -lf90math -lfio -lf77math
C_CXXF90LD         = ${CXX_CC} 
C_CXXF90LIBS       = ${MPI_LIB} -lstdc++ -lf90math -lfio -lf77math
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

############################################################
#
#  File base
#
#

libc: ${LIBNAME}(${OBJSC})
libf: ${LIBNAME}(${OBJSF})



#############
#
# Set shared dependent on build_shared to build .so lib.
#
shared: 



#########

.F90.o:
	${FC} -c ${C_FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} ${FFLAGS} ${F_FREECPP} ${FCPPFLAGS} ${ESMC_INCLUDE} $<

.F.o:
	${FC} -c ${C_FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} ${FFLAGS} ${F_FREENOCPP} ${ESMC_INCLUDE} $<

.f90.o:
	${FC} -c ${FOPTFLAGS} ${FFLAGS} ${F_FIXCPP} ${FCPPFLAGS} ${ESMC_INCLUDE} $<

.f.o:
	${FC} -c ${FOPTFLAGS} ${FFLAGS} ${F_FIXNOCPP} ${ESMC_INCLUDE} $<

.c.o:
	${CC} -c ${COPTFLAGS} ${CFLAGS} ${CCPPFLAGS} ${ESMC_INCLUDE} $<

.C.o:
	${CXX} -c ${COPTFLAGS} ${CFLAGS} ${CCPPFLAGS} ${ESMC_INCLUDE} $<

.F90.a:
	${FC} -c ${C_FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} ${FFLAGS} ${FCPPFLAGS} ${F_FREECPP} ${ESMC_INCLUDE} $<
	${AR} ${AR_FLAGS} ${LIBNAME} $*.o
	${RM} $*.o

.F.a:
	${FC} -c ${C_FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} ${FFLAGS} ${F_FREENOCPPP} ${ESMC_INCLUDE} $<
	${AR} ${AR_FLAGS} ${LIBNAME} $*.o
	${RM} $*.o

.f90.a:
	${FC} -c ${FOPTFLAGS} ${FFLAGS} ${FCPPFLAGS} ${F_FIXCPP} ${ESMC_INCLUDE} $<
	${AR} ${AR_FLAGS} ${LIBNAME} $*.o
	${RM} $*.o

.f.a:
	${FC} -c ${FOPTFLAGS} ${FFLAGS} ${F_FIXNOCPP} ${ESMC_INCLUDE} $<
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



