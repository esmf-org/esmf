#  $Id: build_rules.mk,v 1.13.2.1 2005/03/03 17:24:09 nscollins Exp $
#
#  Linux.absoft.default makefile fragment
#


#
#  Make sure that ESMF_PREC is set to 32
#  When a 64-bit compiler comes out, then just make the default 32, but
#  allow it to be set to 64 ahead of time.  (We will also need to add
#  sections for 32 vs 64 bit if the flags are different.)
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
C_CC                = cc
C_FC                = f95
CXX_CC              = g++ -fPIC
CXX_FC              = f95
FFLAGS              = -YEXT_NAMES=LCS -s  -YEXT_SFX=_
endif

ifeq ($(ESMF_COMM),mpich)
C_CC               = mpicc
C_FC               = mpif90 
CXX_CC             = mpiCC -fPIC
CXX_FC             = mpif90
FFLAGS             = -YEXT_NAMES=LCS -s 
endif

C_FC_MOD           = -p
C_CLINKER_SLFLAG   = -Wl,-rpath,
C_FLINKER_SLFLAG   = -Wl,-rpath,
C_CLINKER	   = ${C_CC}
C_FLINKER	   = ${C_FC}
C_CCV		   = ${C_CC} --version
# on absoft 8 and before, docs say f95 -V should work, but it causes an error.
# f90fe -V prints good version info.   absoft 9 and later, however, fixes this
# and now f90 -V prints good info, and f90fe gives license errors.  so try
# doing both - you will get an error but will at least get some version info.
C_FCV              = f90 -V && f90fe -V
C_SYS_LIB	   = ${MPI_LIB} -ldl -lc -lg2c -lm
# ---------------------------- BOPT - g options ----------------------------
G_COPTFLAGS	   = -g 
G_FOPTFLAGS	   = -g 
# ----------------------------- BOPT - O options -----------------------------
O_COPTFLAGS	   = -O 
O_FOPTFLAGS	   = -O
# ########################## Fortran compiler ##############################
#
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
C_F90CXXLD         = ${CXX_FC}
C_CXXF90LD         = ${CXX_CC} 
# by default append each directory which is in LD_LIBRARY_PATH to
# the -L flag and also to the run-time load flag.  (on systems which
# support the 'module' command, that is how it works - by adding dirs
# to LD_LIBRARY_PATH.)  if it is not set, default to where the absoft
# compilers try to install themselves.  if your compiler is someplace else
# either set LD_LIBRARY_PATH first, or make a site specific file and
# edit the paths explicitly.
ifeq ($(origin LD_LIBRARY_PATH), environment)
LIB_PATHS      = $(addprefix -L, $(subst :, ,$(LD_LIBRARY_PATH)))
CXXLIB_PATHS   = 
F90LIB_PATHS   = 
LD_PATHS   = $(addprefix $(C_FLINKER_SLFLAG), $(subst :, ,$(LD_LIBRARY_PATH)))
else
CXXLIB_PATHS   = -L/usr/lib
F90LIB_PATHS   = -L/soft/com/packages/absoft-9.0/opt/absoft/lib
LIB_PATHS      = ${CXXLIB_PATHS} ${F90LIB_PATHS}
endif

C_F90CXXLIBS       = ${MPI_LIB} ${LIB_PATHS} -lf90math -lfio -lf77math \
                      -lrt -lstdc++ 
C_CXXF90LIBS       = ${MPI_LIB} ${LIB_PATHS} -lstdc++ -lf90math -lrt \
                      -lfio -lf77math
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


