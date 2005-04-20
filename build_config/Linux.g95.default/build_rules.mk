#
#  Linux.gnu.default.mk
#  T. Wainwright, April 2005, based on Linux.pgi.default.mk
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
export ESMF_COMM := mpiuni

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
# Not tested with g95!!
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
#MP_LIB          = -lpgmp
MP_LIB          = 
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
SH_LD		   = cc 
#
# C and Fortran compiler 
#
C_CC		   = gcc 
C_FC		   = g95

C_FC_MOD           = -I
C_CLINKER_SLFLAG   = -Wl,-rpath,
C_FLINKER_SLFLAG   = -Wl,-rpath,
C_CLINKER	   = ${C_CC}
C_FLINKER	   = ${C_FC}
C_CCV		   = ${C_CC} -dumpversion
C_FCV              = ${C_FC} -dumpversion
C_SYS_LIB	   = -ldl -lc -lg2c -lm

FFLAGS             = -fno-second-underscore
F_FREECPP          = -cpp -ffree-form
F_FIXCPP           = -cpp -ffixed-form
F_FREENOCPP        = -ffree-form
F_FIXNOCPP         = -ffixed-form
# ---------------------------- BOPT - g options ----------------------------
G_COPTFLAGS	   = -g 
G_FOPTFLAGS	   = -g
# ----------------------------- BOPT - O options -----------------------------
O_COPTFLAGS	   = -O 
O_FOPTFLAGS	   = -O
#
# C++ compiler 
#
CXX_CC		   = g++
CXX_FC		   = g95
CXX_CLINKER_SLFLAG = -Wl,-rpath,
CXX_FLINKER_SLFLAG = -Wl,-rpath,
CXX_CLINKER	   = ${CXX_CC}
CXX_FLINKER	   = ${CXX_CC}
CXX_CCV		   = ${CXX_CC} -dumpversion
CXX_SYS_LIB	   = -ldl -lc -lg2c -lm
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
LIB_PATHS = 
LD_PATHS = 
endif
C_F90CXXLD         = ${CXX_FC} ${LD_PATHS}
C_F90CXXLIBS       = ${LIB_PATHS} -lrt -lc -lstdc++
C_CXXF90LD         = ${CXX_CC}  ${LD_PATHS}
C_CXXF90LIBS       = ${LIB_PATHS} -lrt -lf95
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

PARCH		   = linux_g95

SL_LIBS_TO_MAKE = 

SL_SUFFIX   =
SL_LIBOPTS  =
SL_LINKOPTS =
SL_F_LINKER = $(F90CXXLD)
SL_C_LINKER = $(CXXF90LD)
SL_LIB_LINKER = $(CXXF90LD)


