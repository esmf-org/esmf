# $Id: build_rules.mk,v 1.18 2005/03/14 21:59:06 jwolfe Exp $
#
#  Linux.pgi.default.mk
#

#
#  Default processor word size setting.
#

ifndef ESMF_PREC
export ESMF_PREC := 32
endif
ifeq ($(ESMF_PREC),default)
export ESMF_PREC := 32
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

ifeq ($(ESMF_COMM),lam)
# this section is set up for LAM mpi
MPI_HOME       =
MPI_LIB        = -lmpi -llam
MPI_INCLUDE    =
MPIRUN         =  mpirun
endif

# for mpich: if your system sets either MPI_HOME or MPICH to the location of
# the mpi include files, libraries, and binary executables, this makefile 
# should work unchanged.  (MPI_HOME takes precedence over MPICH if both are 
# set.)  if your system has mpich installed but not in either /usr or
# /usr/local, and neither of these environment variables are set, you must 
# set one of them to the location where the include, lib, and bin dirs will 
# be found.

# set up to use MPICH
ifeq ($(ESMF_COMM),mpich)
ifndef MPI_HOME
 ifdef MPICH
  export MPI_HOME := $(MPICH)
 else
  export MPI_HOME := /usr/local
 endif
endif
MPI_LIB        = -L${MPI_HOME}/lib -lmpich
MPI_INCLUDE    = -I${MPI_HOME}/include -DESMF_MPICH=1
MPIRUN         = ${MPI_HOME}/bin/mpirun
endif

ifeq ($(ESMF_COMM),mpiuni)
# this section is set up to bypass all MPI
MPI_HOME       = ${ESMF_DIR}/src/Infrastructure/stubs/mpiuni
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

# ################## Compilers, Linkers, and Loaders ########################
#

# the default is to use the pgi C and C++ compilers.
# if you want gcc and g++, set ESMF_C_COMPILER to gcc before building.

ifneq ($(ESMF_COMM),mpich)
ifeq ($(ESMF_C_COMPILER),gcc)
C_CC               = gcc
CXX_CC             = g++
C_FC               = pgf90
else
C_CC		   = pgcc 
C_FC		   = pgf90
# CXX_CC		   = pgCC -tlocal   note: this was here, but not recognized on lightning
CXX_CC		   = pgCC
CXX_FC		   = pgf90
endif
endif

# if you are using mpich, then however the mpich wrappers have been built
# will determine which compilers you are using.
ifeq ($(ESMF_COMM),mpich)
ifeq ($(ESMF_C_COMPILER),gcc)
C_CC		   = mpicc
C_FC		   = mpif90
CXX_CC		   = mpiCC
CXX_FC		   = mpif90
else
C_CC		   = mpicc
C_FC		   = mpif90
# CXX_CC		   = mpiCC -tlocal   note: this was here, but not recognized on lightning
CXX_CC		   = mpiCC
CXX_FC		   = mpif90
endif
endif

# the default is to link with the pgi C and C++ libraries unless you have
# already set ESMF_C_COMPILER to gcc.  if you want to still compile with pgi
# but link with the gcc libs anyway, set ESMF_C_LIBRARY to gcc before building.
ifeq ($(ESMF_C_COMPILER),gcc)
PGI_C_LIB_FLAG =
PGI_C_LIB_NEEDED = -lstdc++
else 
ifeq ($(ESMF_C_LIBRARY),gcc)
PGI_C_LIB_FLAG = -cxxlib-gcc
PGI_C_LIB_NEEDED = -lstdc++
else
PGI_C_LIB_FLAG =
PGI_C_LIB_NEEDED =
endif
endif

# add standard flags
C_CC    +=  $(PGI_C_LIB_FLAG)
CXX_CC  +=  $(PGI_C_LIB_FLAG)
C_FC    +=

# Which compiler to call when
C_CLINKER          = ${C_CC}
C_FLINKER          = ${C_FC}
CXX_FC             = ${C_FC} -mp
CXX_CLINKER        = ${C_CC}
CXX_FLINKER        = ${C_CC}
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
C_CCV              = ${C_CC} -V -v
C_FCV              = ${C_FC} -V -v
CXX_CCV            = ${CXX_CC} -V -v

# default system libs
C_SYS_LIB          = -ldl -lc -lg2c -lm
CXX_SYS_LIB	   = -ldl -lc -lg2c -lm

# fortran flags
C_FC_MOD           = -I
F_FREECPP          = -Mpreprocess -Mfreeform
F_FIXCPP           = -Mpreprocess -Mnofreeform
F_FREENOCPP        = -Mfreeform
F_FIXNOCPP         = -Mnofreeform

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
# to LD_LIBRARY_PATH.)  if it is not set, default to where the pgi
# compilers try to install themselves.  if your compiler is someplace else
# either set LD_LIBRARY_PATH first, or make a site specific file and
# edit the paths explicitly.
ifeq ($(origin LD_LIBRARY_PATH), environment)
LIB_PATHS   = $(addprefix -L, $(subst :, ,$(LD_LIBRARY_PATH)))
LD_PATHS    = $(addprefix $(C_FLINKER_SLFLAG), $(subst :, ,$(LD_LIBRARY_PATH)))
else
LIB_PATHS   = -L/opt/pgi-5.2/linux86/5.2/lib
LD_PATHS    = $(C_FLINKER_SLFLAG)/opt/pgi/pgi-5.2/linux86/5.2/lib
endif

C_F90CXXLIBS       = $(LD_PATHS) $(LIB_PATHS) $(PGI_C_LIB_NEEDED) -lrt -lC -lc
C_CXXF90LIBS       = $(LD_PATHS) $(LIB_PATHS) $(PGI_C_LIB_NEEDED) -lrt -lC -lpgf90 -lpgf90_rpm1 -lpgf902 -lpgf90rtl -lpgftnrtl
###############################################################################

PARCH		   = linux_pgi

SL_LIBS_TO_MAKE = 

SL_SUFFIX   =
SL_LIBOPTS  =
SL_LINKOPTS =
SL_F_LINKER = $(F90CXXLD)
SL_C_LINKER = $(CXXF90LD)
SL_LIB_LINKER = $(CXXF90LD)


