# $Id: SunOS.default.mk,v 1.2 2003/09/09 21:45:44 flanigan Exp $
#
# SunOs.default.mk
#


############################################################
#
# File base.site
#

#
#  This file contains site-specific information.  The definitions below
#  should be changed to match the locations of libraries at your site.
#  The following naming convention is used:
#     XXX_LIB - location of library XXX
#     XXX_INCLUDE - directory for include files needed for library XXX
#

# Location of MPI (Message Passing Interface) software

ESMC_MPIRUN      = 
MPI_HOME       = /opt/SUNWhpc
MPI_LIB        = -L${MPI_HOME}/lib -R${MPI_HOME}/lib -lmpi
MPI_INCLUDE    = -I${MPI_HOME}/include
MPIRUN         =  ${MPI_HOME}/bin/mprun

# Location of the OpenMP library
MP_LIB         = 

# Location of threading library
THREAD_LIB     = 


############################################################
#
# File base_variables
#

#
#     See the file build/base_variables.defs for a complete explanation of all these fields
#
AR		   = /usr/ccs/bin/ar
AR_FLAGS	   = cr
RM		   = rm -f
OMAKE		   = ${MAKE}
RANLIB		   = /usr/ccs/bin/ranlib
SHELL		   = /bin/sh
SED		   = /bin/sed
SH_LD		   = /opt/SUNWspro/bin/CC 
# ######################### C and Fortran compiler ########################
#
C_CC		   = /opt/SUNWspro/bin/cc -KPIC -dalign -xtarget=native
C_FC		   = /opt/SUNWspro/bin/f90 -openmp -xpp=cpp -dalign
C_FC_MOD           = -M
C_CLINKER_SLFLAG   = 
C_FLINKER_SLFLAG   = 
# Must use f90 to link C to get omp libs
C_CLINKER	   = /opt/SUNWspro/bin/f90 -openmp -dalign
C_FLINKER	   = /opt/SUNWspro/bin/f90 -openmp -dalign -R .
C_CCV		   = ${C_CC} -V
C_FCV              = /opt/SUNWspro/bin/f90 -openmp -dalign
C_SYS_LIB	   = -L/opt/SUNWspro/SC5.0/lib -lF77 -lM77 -lfsu -lsunmath -lnsl -lsocket -lgen -ldl -lm
# ---------------------------- BOPT - g options ----------------------------
G_COPTFLAGS	   = -g -xs
G_FOPTFLAGS	   = -g -xs
# ----------------------------- BOPT - O options -----------------------------
O_COPTFLAGS	   = -fast -xO4 -fsimple=2 -xtarget=native
O_FOPTFLAGS	   = -fast
# ########################## Fortran compiler ##############################
#
F_FREECPP               = -free -fpp
F_FIXCPP                = -fixed -fpp
F_FREENOCPP             = -free
F_FIXNOCPP              = -fixed
# ########################## C++ compiler ##################################
#
CXX_CC		   = /opt/SUNWspro/bin/CC -instances=static
CXX_FC		   = /opt/SUNWspro/bin/f90 -openmp
CXX_CLINKER_SLFLAG = -Wl,-rpath,
CXX_FLINKER_SLFLAG = -Wl,-rpath,
CXX_CLINKER	   = /opt/SUNWspro/bin/CC 
CXX_FLINKER	   = /opt/SUNWspro/bin/CC 
CXX_CCV		   = ${CXX_CC} -V
#CXX_SYS_LIB	   = -ldl -lc -lf2c -lm
CXX_SYS_LIB	   = -ldl -lc -lg2c -lm
#CXX_SYS_LIB	   = -ldl -lc /usr/lib/libf2c.a -lm
C_F90CXXLD         = /opt/SUNWspro/bin/f90 -openmp
C_F90CXXLIBS       = -lfui -lfai -lfai2 -lfsumai -lfprodai -lfminlai -lfmaxlai -lfminvai -lfmaxvai -lfsu -lsunmath -lCrun -lCstd -lCrun -lm -lcx -lc
C_CXXF90LD         = /opt/SUNWspro/bin/CC
C_CXXF90LIBS       = -L/opt/SUNWspro/lib -lfui -lfai -lfai2 -lfsumai -lfprodai -lfminlai -lfmaxlai -lfminvai -lfmaxvai -lfsu -lsunmath -lm -lc
C_CXXSO            = /opt/SUNWspro/bin/CC -G
C_CXXSOLIBS        = -Kpic  -lCstd -lCrun -lm -lw -lcx -lc
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

PARCH		   = solaris

SL_SUFFIX   = 
SL_LIBOPTS  = 
SL_LINKOPTS = 
SL_F_LINKER = $(F90CXXLD)
SL_C_LINKER = $(CXXF90LD)
SL_LIB_LINKER = $(CXXF90LD)
SL_LIBS_TO_MAKE = libesmf liboldworld



############################################################
#
# File base
#

libc: ${LIBNAME}(${OBJSC})
libf: ${LIBNAME}(${OBJSF})

#########

.F90.o:
	${FC} -c ${C_FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} ${FFLAGS} ${FCPPFLAGS} -fpp -free ${ESMC_INCLUDE} $<

.F.o:
	${FC} -c ${C_FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} ${FFLAGS} -free ${ESMC_INCLUDE} $<

.f90.o:
	${FC} -c ${FOPTFLAGS} ${FFLAGS} ${FCPPFLAGS} -fpp -fixed ${ESMC_INCLUDE} $<

.f.o:
	${FC} -c ${FOPTFLAGS} ${FFLAGS} -fixed ${ESMC_INCLUDE} $<

.c.o:
	${CC} -c ${COPTFLAGS} ${CFLAGS} ${CCPPFLAGS} ${ESMC_INCLUDE} $<

.C.o:
	echo big .C to .o rule, remove mpi_include when CFLAGS starts working
	${CXX} -c ${COPTFLAGS} ${CFLAGS} ${MPI_INCLUDE} ${CCPPFLAGS} ${ESMC_INCLUDE} $<

.F90.a:
	${FC} -c ${C_FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} ${FFLAGS} ${FCPPFLAGS} -fpp -free ${ESMC_INCLUDE} $<
	${AR} ${AR_FLAGS} ${LIBNAME} $*.o
	${RM} $*.o

.F.a:
	${FC} -c ${C_FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} ${FFLAGS} -free ${ESMC_INCLUDE} $<
	${AR} ${AR_FLAGS} ${LIBNAME} $*.o
	${RM} $*.o

.f90.a:
	${FC} -c ${FOPTFLAGS} ${FFLAGS} ${FCPPFLAGS} -fpp -fixed ${ESMC_INCLUDE} $<
	${AR} ${AR_FLAGS} ${LIBNAME} $*.o
	${RM} $*.o

.f.a:
	${FC} -c ${FOPTFLAGS} ${FFLAGS} -fixed ${ESMC_INCLUDE} $<
	${AR} ${AR_FLAGS} ${LIBNAME} $*.o
	${RM} $*.o

.c.a:
	${CC} -c ${COPTFLAGS} ${CFLAGS} ${CCPPFLAGS} ${ESMC_INCLUDE} $<
	${AR} ${AR_FLAGS} ${LIBNAME} $*.o
	${RM} $*.o

.C.a:
	echo big .C to .a rule, remove mpi_include when CFLAGS starts working
	${CXX} -c ${COPTFLAGS} ${CFLAGS} ${MPI_INCLUDE} ${CCPPFLAGS} ${ESMC_INCLUDE} $<
	${AR} ${AR_FLAGS} ${LIBNAME} $*.o
	${RM} $*.o


#############
#
# Set shared dependent on build_shared to build .so lib.
#
shared: 


