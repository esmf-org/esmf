# $Id: build_rules.mk,v 1.26 2005/02/08 20:23:05 nscollins Exp $
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
NETCDF_LIB       = -lnetcdf_stubs
NETCDF_INCLUDE   = -I${ESMF_DIR}/src/Infrastructure/stubs/netcdf_stubs
HDF_LIB          =
HDF_INCLUDE      =
else
BLAS_LIB       = -lscs ${FC_LIB}
LAPACK_LIB     = -lscs
#if 0
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
#endif
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

ifeq ($(ESMF_COMM),mpich2)
MPI_HOME       = 
MPI_LIB        = -lmpich
MPI_INCLUDE    = -DESMF_MPICH=1
MPIRUN         =  mpirun $(ESMF_NODES)
endif

ifeq ($(ESMF_COMM),mpiuni)
# this section is set up to bypass all MPI
MPI_HOME       = ${ESMF_DIR}/src/Infrastructure/stubs/mpiuni
MPI_LIB        = -lmpiuni
MPI_INCLUDE    = -I${MPI_HOME}
MPIRUN         =  ${MPI_HOME}/mpirun
endif

# MP_LIB is for openMP
MP_LIB          = 
# For pthreads (or omp)
THREAD_LIB      =

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

# the default is to use the intel C and C++ compilers.
# if you want gcc and g++, set ESMF_C_COMPILER to gcc before building.

ifneq ($(ESMF_COMM),mpich)
ifeq ($(ESMF_C_COMPILER),gcc)
C_CC	   = gcc
CXX_CC	   = g++
C_FC	   = ifort
else
C_CC	   = icc
CXX_CC	   = icpc
C_FC	   = ifort
endif
endif

# if you are using mpich, then however the mpich wrappers have been built
# will determine which compilers you are using.
ifeq ($(ESMF_COMM),mpich)
C_CC	   = mpicc
CXX_CC	   = mpiCC
C_FC	   = mpif90
endif

ifeq ($(ESMF_PREC),64)
C_CC	   += -size_lp64
CXX_CC	   += -size_lp64
C_FC	   += -size_lp64
endif

# the default is to link with the intel C and C++ libraries unless you have
# already set ESMF_C_COMPILER to gcc.  if you want to still compile with intel
# but link with the gcc libs anyway, set ESMF_C_LIBRARY to gcc before building.
ifeq ($(ESMF_C_COMPILER),gcc)
INTEL_C_LIB_FLAG = 
INTEL_C_LIB_NEEDED = -lstdc++
else
ifeq ($(ESMF_C_LIBRARY),gcc)
INTEL_C_LIB_FLAG = -cxxlib-gcc
INTEL_C_LIB_NEEDED = -lstdc++
else
INTEL_C_LIB_FLAG = -cxxlib-icc
INTEL_C_LIB_NEEDED = -lcprts
endif
endif

# add standard flags
C_CC    +=  -pthread $(INTEL_C_LIB_FLAG)
CXX_CC  +=  -pthread $(INTEL_C_LIB_FLAG)
C_FC    +=  -threads 

# Which compiler to call when
C_CLINKER	   = ${C_CC}
C_FLINKER	   = ${C_FC}
CXX_FC		   = ${C_FC} -mp
CXX_CLINKER	   = ${C_CC}
CXX_FLINKER	   = ${C_CC}
C_F90CXXLD         = ${C_FC} -mp
C_CXXF90LD         = ${C_CC}
C_CXXSO            = ${C_CC} -shared
SH_LD              = ${C_CC}

#
# C, C++, and Fortran compiler flags 
#
C_CLINKER_SLFLAG   = -Wl,-rpath,
C_FLINKER_SLFLAG   = -Wl,-rpath,
CXX_CLINKER_SLFLAG = -Wl,-rpath,
CXX_FLINKER_SLFLAG = -Wl,-rpath,

# version stuff
C_CCV		   = ${C_CC} -V -v
C_FCV              = ${C_FC} -V -v
CXX_CCV		   = ${CXX_CC} -V -v

# default system libs
C_SYS_LIB	   = -ldl -lc -lg2c -lm
CXX_SYS_LIB	   = -ldl -lc -lg2c -lm

# fortran flags
C_FC_MOD           = -I
F_FREECPP          = -fpp -FR
F_FIXCPP           = -fpp 
F_FREENOCPP        = -fpp0 -FR
F_FIXNOCPP         = -fpp0

# compile time debugging or optimization flags
# ---------------------------- BOPT - g options ----------------------------
G_COPTFLAGS	   = -g 
G_FOPTFLAGS	   = -g
# ----------------------------- BOPT - O options -----------------------------
O_COPTFLAGS	   = -O 
O_FOPTFLAGS	   = -O
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


# by default append each directory which is in LD_LIBRARY_PATH to
# the -L flag and also to the run-time load flag.  (on systems which
# support the 'module' command, that is how it works - by adding dirs
# to LD_LIBRARY_PATH.)  if it is not set, default to where the intel
# compilers try to install themselves.  if your compiler is someplace else
# either set LD_LIBRARY_PATH first, or make a site specific file and
# edit the paths explicitly.

ifeq ($(origin LD_LIBRARY_PATH), environment)
LIB_PATHS   = $(addprefix -L, $(subst :, ,$(LD_LIBRARY_PATH)))
LD_PATHS    = $(addprefix $(C_FLINKER_SLFLAG), $(subst :, ,$(LD_LIBRARY_PATH)))
else
ifeq ($(ESMF_COMPILER_VERSION),80)
LIB_PATHS       = -L/opt/intel_cc_80/lib
LD_PATHS        = $(C_FLINKER_SLFLAG)/opt/intel_cc_80/lib
else
LIB_PATHS       = -L/opt/intel_cc_81/lib
LD_PATHS        = $(C_FLINKER_SLFLAG)/opt/intel_cc_81/lib
endif
endif

C_F90CXXLIBS    = $(LD_PATHS) $(LIB_PATHS) $(INTEL_C_LIB_NEEDED) -lrt -ldl
C_CXXF90LIBS    = $(LD_PATHS) $(LIB_PATHS) $(INTEL_C_LIB_NEEDED) -lifcoremt -lunwind -lrt -ldl
###############################################################################

PARCH		   = linux_intel

SL_LIBS_TO_MAKE = 

SL_SUFFIX   =
SL_LIBOPTS  =
SL_LINKOPTS =
SL_F_LINKER = $(F90CXXLD)
SL_C_LINKER = $(CXXF90LD)
SL_LIB_LINKER = $(CXXF90LD)

