#  $Id: build_rules.mk,v 1.2 2005/03/03 23:49:22 nscollins Exp $
#
#  Linux.pathscale.default makefile
#


#
#  ESMF_PREC defaults to 64 bits
#
ifndef ESMF_PREC
export ESMF_PREC := 64
endif
ifeq ($(ESMF_PREC),default)
export ESMF_PREC := 64
endif

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
NETCDF_LIB       = -lnetcdf_stubs
NETCDF_INCLUDE   = -I${ESMF_DIR}/src/Infrastructure/stubs/netcdf_stubs
HDF_LIB          =
HDF_INCLUDE      =
else
BLAS_LIB         = -L/usr/local/lib -latlas
LAPACK_LIB       = -L/usr/local/lib -llapack
NETCDF_LIB       = -L/usr/local/lib -lnetcdf
NETCDF_INCLUDE   = -I/usr/local/include
HDF_LIB          = -L/usr/local/lib/ -lmfhdf -ldf -ljpeg -lz
HDF_INCLUDE      = -I/usr/local/include
endif

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
MPI_INCLUDE    = -DESMF_MPICH
MPIRUN         = mpirun $(ESMF_NODES)
endif

ifeq ($(ESMF_COMM),mpiuni)
# without mpich installed:
MPI_HOME       = ${ESMF_DIR}/src/Infrastructure/stubs/mpiuni
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

AR		   = ar
AR_FLAGS	   = cr
RM		   = rm -f
OMAKE		   = ${MAKE}
RANLIB		   = ranlib
SHELL		   = /bin/sh
SED		   = /bin/sed
SH_LD		   = cc
#
# C and Fortran compiler 
#
ifneq ($(ESMF_COMM),mpich)
C_CC                = pathcc
C_FC                = pathf90
CXX_CC              = pathCC
CXX_FC              = pathf90
FFLAGS              = 
endif

ifeq ($(ESMF_COMM),mpich)
C_CC               = mpicc
C_FC               = mpif90 
CXX_CC             = mpiCC 
CXX_FC             = mpif90
FFLAGS             = 
endif

ifeq ($(ESMF_PREC),64)
C_CC   += -default64
CXX_CC += -default64
C_FC   += -default64
CXX_FC += -default64
endif

C_FC_MOD           = -I
C_CLINKER_SLFLAG   = -Wl,-rpath,
C_FLINKER_SLFLAG   = -Wl,-rpath,
C_CLINKER	   = ${C_CC}
C_FLINKER	   = ${C_FC}
C_CCV		   = ${C_CC} -v
C_FCV              = ${C_F} -v 
C_SYS_LIB	   = ${MPI_LIB} -ldl -lc -lg2c -lm
# ---------------------------- BOPT - g options ----------------------------
G_COPTFLAGS	   = -g 
G_FOPTFLAGS	   = -g 
# ----------------------------- BOPT - O options -----------------------------
O_COPTFLAGS	   = -O 
O_FOPTFLAGS	   = -O
# ########################## Fortran compiler ##############################
#
F_FREECPP       = -freeform -cpp
F_FIXCPP        = -fixedform -cpp
F_FREENOCPP     = -freeform
F_FIXNOCPP      = -fixedform
# ########################## C++ compiler ##################################
#
CXX_CLINKER_SLFLAG = -Wl,-rpath,
CXX_FLINKER_SLFLAG = -Wl,-rpath,
CXX_CLINKER	   = ${CXX_CC}
CXX_FLINKER	   = ${CXX_CC}
CXX_CCV		   = ${CXX_CC} -v
CXX_SYS_LIB	   = ${MPI_LIB} -ldl -lc -lg2c -lm
C_F90CXXLD         = ${CXX_FC}
CXXLIBBASE         = /usr/lib
C_F90CXXLIBS       = ${MPI_LIB} -L${CXXLIBBASE}
C_CXXF90LD         = ${CXX_CC} 
F90LIBBASE         = /usr/lib
C_CXXF90LIBS       = ${MPI_LIB} -L${F90LIBBASE}
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

SL_LIBS_TO_MAKE = 

SL_SUFFIX   = 
SL_LIBOPTS  = 
SL_LINKOPTS = 
SL_F_LINKER = $(F90CXXLD)
SL_C_LINKER = $(CXXF90LD)
SL_LIB_LINKER = $(CXXF90LD)


