# $Id: build_rules.mk,v 1.9 2004/05/18 11:31:01 nscollins Exp $
#
# Linux.lahey.default.mk
#
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
# with lam-mpi installed in /usr/local:
MPI_HOME       =
MPI_LIB        = -lmpi -llam
MPI_INCLUDE    =
MPIRUN         =  mpirun
endif

ifeq ($(ESMF_COMM),mpich)
MPI_HOME       =
MPI_LIB        = -lmpich
MPI_INCLUDE    = -DESMF_MPICH=1
MPIRUN         =  mpirun $(ESMF_NODES)
endif

ifeq ($(ESMF_COMM),mpiuni)
MPI_HOME       = ${ESMF_TOP_DIR}/src/Infrastructure/mpiuni
MPI_LIB        = -lmpiuni
MPI_INCLUDE    = -I${MPI_HOME}
MPIRUN         =  ${MPI_HOME}/mpirun
endif

# MP_LIB is for openMP
#MP_LIB          = 
#MP_INCLUDE      = 
# For pthreads (or omp)
THREAD_LIB      = -lpthread

############################################################
#
#
AR		   = ar
AR_FLAGS	   = cr
AR_EXTRACT         = -x
RM		   = rm -f
OMAKE		   = ${MAKE}
RANLIB		   = ranlib
SHELL		   = /bin/sh
SED		   = /bin/sed
SH_LD		   = gcc
#
# C and Fortran compiler 
#
C_CC		   = gcc
C_FC		   = lf95
C_FC_MOD           = -I
C_CLINKER_SLFLAG   = -Wl,-rpath,
C_FLINKER_SLFLAG   = -Wl,-rpath,
C_CLINKER	   = gcc
C_FLINKER	   = lf95
C_CCV		   = ${C_CC} --version
C_FCV              = lf95
C_SYS_LIB	   = -ldl -lc -lg2c -lm
#C_SYS_LIB	   = -ldl -lc -lf2c -lm
#C_SYS_LIB	   = -ldl -lc /usr/lib/libf2c.a -lm  #Use /usr/lib/libf2c.a if that's what your f77 uses.
# ---------------------------- BOPT - g options ----------------------------
G_COPTFLAGS	   = -g 
G_FOPTFLAGS	   = -g
# ----------------------------- BOPT - O options -----------------------------
O_COPTFLAGS	   = -O 
O_FOPTFLAGS	   = -O
#
# Fortran compiler 
#
F_FREECPP          = --nfix -Cpp
F_FIXCPP           = --fix -Cpp
F_FREENOCPP        = --nfix 
F_FIXNOCPP         = --fix
#
# C++ compiler 
#
CXX_CC		   = g++ -fPIC
CXX_FC		   = lf95
CXX_CLINKER_SLFLAG = -Wl,-rpath,
CXX_FLINKER_SLFLAG = -Wl,-rpath,
CXX_CLINKER	   = g++
CXX_FLINKER	   = g++
CXX_CCV		   = ${CXX_CC} --version

CXX_SYS_LIB	   = -ldl -lc -lm
#CXX_SYS_LIB	   = -ldl -lc -lf2c -lm
#CXX_SYS_LIB	   = -ldl -lc /usr/lib/libf2c.a -lm

C_F90CXXLD         = lf95 -verbose

C_F90CXXLIBS       = -Wl,-rpath /usr/lib/gcc-lib/i386-redhat-linux/3.2.2 \
                     -Wl,-rpath /usr/local/lf9560/lib \
                     -L/usr/lib/gcc-lib/i386-redhat-linux/3.2.2 \
                     -lstdc++ -lgcc -lg2c -lrt

C_CXXF90LD         = g++ 

C_CXXF90LIBS       = -L/usr/local/lf9560/lib -lfj9i6 -lfj9ipp -lfj9f6 -lfj9fpp \
                      -lfj9e6 -lfccx86_6a -lrt

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
##############################################################################

PARCH		   = linux_lf95

SL_SUFFIX   = so
SL_LIBOPTS  = -shared
SL_LINKOPTS = 
SL_F_LINKER = $(F90CXXLD) -rpath $(ESMF_LIBDIR) -rpath /usr/local/lf9560/lib
SL_C_LINKER = $(CXXF90LD) -Wl,-rpath $(ESMF_LIBDIR) -Wl,-rpath /usr/local/lf9560/lib
SL_LIB_LINKER = $(CXXF90LD) -Wl,-rpath $(ESMF_LIBDIR)
SL_LIBS_TO_MAKE = libesmf 

############################################################
#
# Set shared dependent on build_shared to build .so lib.
#
shared: build_shared


#
# Notes:
#
#  -ldl is the dynamic link library that allows one to use dlopen() etc
#
