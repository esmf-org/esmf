# $Id: build_rules.mk,v 1.9 2004/05/18 11:29:18 nscollins Exp $
#
# Linux.intel.default.mk
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
# BLAS usually comes with SGI. Do NOT use the parallel (library names with 
# mp in them) version of the SGI BLAS.
#
ifeq ($(ESMF_NO_IOCODE),true)
BLAS_LIB         =
LAPACK_LIB       =
NETCDF_LIB       =
NETCDF_INCLUDE   =
HDF_LIB          =
HDF_INCLUDE      =
else
BLAS_LIB       = -lscs ${FC_LIB}
LAPACK_LIB     = -lscs
ifeq ($(ESMF_PREC),32)
NETCDF_LIB       = -L/usr/freeware/lib32 -lnetcdf
NETCDF_INCLUDE   = -I/usr/freeware/include
HDF_LIB          = -L /usr/freeware/lib32 -lmfhdf -ldf -ljpeg -lz
HDF_INCLUDE      = -I /usr/freeware/include
endif
# end 32 bit section
ifeq ($(ESMF_PREC),64)
NETCDF_LIB       = -L/usr/freeware/lib64 -lnetcdf
NETCDF_INCLUDE   = -I/usr/freeware/include
HDF_LIB          = -L /usr/freeware/lib64 -lmfhdf -ldf -ljpeg -lz
HDF_INCLUDE      = -I /usr/freeware/include
endif
# end 64 bit section
endif
# end of io bypass section

#
# Location of MPI (Message Passing Interface) software  
#
# We recommend using SGI's MPI implementation over MPICH on the Origin and 
# Powerchallenge.
#
# If you are using the MPICH implementation of MPI with version BELOW 1.1,
# you should remove the -DESMC_HAVE_INT_MPI_COMM. If you are using MPICH 
# Version 1.1 or SGI's version of MPI you MUST retain it.
#
ifeq ($(ESMF_COMM),mpi)
ESMC_MPIRUN      = mpirun 
MPI_LIB        = -lmpi -lmpi++
MPI_INCLUDE     = -DESMC_HAVE_INT_MPI_COMM
MPIRUN          = ${ESMC_MPIRUN}
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
MPI_HOME       = 
MPI_LIB        = -lmpich 
MPI_INCLUDE    = -DESMF_MPICH=1
MPIRUN         =  mpirun $(ESMF_NODES)
endif

ifeq ($(ESMF_COMM),mpiuni)
# this section is set up to bypass all MPI
MPI_HOME       = ${ESMF_DIR}/src/Infrastructure/mpiuni
MPI_LIB        = -lmpiuni
MPI_INCLUDE    = -I${MPI_HOME}
MPIRUN         =  ${MPI_HOME}/mpirun
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
# ################## Compilers, Linkers, and Loaders ########################
#
ifneq ($(ESMF_COMM),mpich)
ifeq ($(ESMF_PREC),64)
C_CC		   = icc -size_lp64
C_FC		   = ifort -size_lp64
endif
ifeq ($(ESMF_PREC),32)
C_CC		   = icc
C_FC		   = ifort
endif
endif

ifeq ($(ESMF_COMM),mpich)
ifeq ($(ESMF_PREC),64)
C_CC		   = mpiCC -size_lp64
C_FC		   = mpif90 -size_lp64
endif
ifeq ($(ESMF_PREC),32)
C_CC		   = mpiCC
C_FC		   = mpif90
endif
endif

C_CLINKER	   = ${C_CC}
C_FLINKER	   = ${C_FC}
CXX_CC		   = ${C_CC}
CXX_FC		   = ${C_FC} -mp
CXX_CLINKER	   = ${C_CC}
CXX_FLINKER	   = ${C_CC}
C_F90CXXLD         = ${C_FC} -mp
C_CXXF90LD         = ${C_CC}
C_CXXSO            = ${C_CC} -shared
SH_LD              = ${C_CC}

#
# C and Fortran compiler flags 
#
C_FC_MOD           = -I
C_CLINKER_SLFLAG   = -Wl,-rpath,
C_FLINKER_SLFLAG   = -Wl,-rpath,
C_CCV		   = ${C_CC} -V -c -w -x c
C_FCV              = ${C_FC} -V -c -w
C_SYS_LIB	   = -ldl -lc -lg2c -lm
F_FREECPP          = -cpp -FR
F_FIXCPP           = -cpp
F_FREENOCPP        = -cpp0 -FR
F_FIXNOCPP         = -cpp0
# ---------------------------- BOPT - g options ----------------------------
G_COPTFLAGS	   = -g 
G_FOPTFLAGS	   = -g
# ----------------------------- BOPT - O options -----------------------------
O_COPTFLAGS	   = -O 
O_FOPTFLAGS	   = -O
#
# C++ compiler flags 
#
CXX_CLINKER_SLFLAG = -Wl,-rpath,
CXX_FLINKER_SLFLAG = -Wl,-rpath,
CXX_CCV		   = ${CXX_CC} -V -c -w -x c++
CXX_SYS_LIB	   = -ldl -lc -lg2c -lm
#CXX_SYS_LIB	   = -ldl -lc /usr/lib/libf2c.a -lm
C_F90CXXLIBS       = -Wl,-rpath,/opt/intel_cc_80/lib -L/opt/intel_cc_80/lib \
                     -lcprts -lrt
#C_F90CXXLIBS       = -lcprts
C_CXXF90LIBS       =  -lrt
#C_CXXF90LIBS       = -lCEPCF90 -lIEPCF90 -lF90 -lintrins 
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
SL_LIBS_TO_MAKE = libesmf 

#########

#
# Set shared dependent on build_shared to build .so lib.
#
shared:


