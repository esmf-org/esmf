# $Id: build_rules.mk,v 1.9 2004/05/18 11:31:02 nscollins Exp $
#
#  Linux.pgi.default.mk
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
BLAS_LIB         = -L/usr/local/lib -latlas
LAPACK_LIB       = -L/usr/local/lib -llapack
NETCDF_LIB       = -L/usr/local/lib -lnetcdf
NETCDF_INCLUDE   = -I/usr/local/include
HDF_LIB          = -L/usr/local/lib/ -lmfhdf -ldf -ljpeg -lz
HDF_INCLUDE      = -I/usr/local/include
endif

# Location of MPI (Message Passing Interface) software

ifeq ($(ESMF_COMM),lam)
# this section is set up for LAM mpi
MPI_HOME       =
MPI_LIB        = -lmpi -llam
MPI_INCLUDE    =
MPIRUN         =  mpirun
endif

ifeq ($(ESMF_COMM),mpich)
# set up to use MPICH
MPI_HOME       = 
MPI_LIB        = -lmpich
MPI_INCLUDE    = -DESMF_MPICH=1
MPIRUN         = mpirun
endif

ifeq ($(ESMF_COMM),mpiuni)
# this section is set up to bypass all MPI
MPI_HOME       = ${ESMF_DIR}/src/Infrastructure/mpiuni
MPI_LIB        = -lmpiuni
MPI_INCLUDE    = -I${MPI_HOME}
MPIRUN         =  ${MPI_HOME}/mpirun
endif

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
#
# C and Fortran compiler 
#
ifneq ($(ESMF_COMM),mpich)
C_CC		   = pgcc -mp
C_FC		   = pgf90 -mp
CXX_CC		   = pgCC -tlocal
CXX_FC		   = pgf90 -mp 
endif

ifeq ($(ESMF_COMM),mpich)
C_CC		   = mpicc -mp
C_FC		   = mpif90 -mp
CXX_CC		   = mpiCC -tlocal
CXX_FC		   = mpif90 -mp
endif

C_FC_MOD           = -I
C_CLINKER_SLFLAG   = -Wl,-rpath,
C_FLINKER_SLFLAG   = -Wl,-rpath,
C_CLINKER	   = ${C_CC}
C_FLINKER	   = ${C_FC}
C_CCV		   = ${C_CC} -V
C_FCV              = ${C_FC} -V
C_SYS_LIB	   = -ldl -lc -lg2c -lm

F_FREECPP          = -Mpreprocess -Mfreeform
F_FIXCPP           = -Mpreprocess -Mnofreeform
F_FREENOCPP        = -Mfreeform
F_FIXNOCPP         = -Mnofreeform
# ---------------------------- BOPT - g options ----------------------------
G_COPTFLAGS	   = -g 
G_FOPTFLAGS	   = -g
# ----------------------------- BOPT - O options -----------------------------
O_COPTFLAGS	   = -O 
O_FOPTFLAGS	   = -O
#
# C++ compiler 
#
CXX_CLINKER_SLFLAG = -Wl,-rpath,
CXX_FLINKER_SLFLAG = -Wl,-rpath,
CXX_CLINKER	   = ${CXX_CC}
CXX_FLINKER	   = ${CXX_CC}
CXX_CCV		   = ${CXX_CC} -V
CXX_SYS_LIB	   = -ldl -lc -lg2c -lm
C_F90CXXLD         = ${CXX_FC} -mp
C_F90CXXLIBS       = -lpgc -lrt -lstd -lC
C_CXXF90LD         = ${CXX_CC} 
C_CXXF90LIBS       = -lrt -lpgf90 -lpgf90_rpm1 -lpgf902 -lpgf90rtl -lpgftnrtl
C_CXXSO            = ${CXX_CC} -shared
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
###############################################################################

PARCH		   = linux_pgi

SL_SUFFIX   =
SL_LIBOPTS  =
SL_LINKOPTS =
SL_F_LINKER = $(F90CXXLD)
SL_C_LINKER = $(CXXF90LD)
SL_LIB_LINKER = $(CXXF90LD)
SL_LIBS_TO_MAKE = libesmf 


#############
#
# Set shared dependent on build_shared to build .so lib.
#
shared:


