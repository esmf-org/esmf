#  $Id: build_rules.mk,v 1.8 2004/06/07 16:01:57 nscollins Exp $
#
#  Darwin.nag.default.mk
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
BLAS_LIB         = -L/sw/lib -latlas
LAPACK_LIB       = -L/sw/lib -llapack
NETCDF_LIB       = -L/sw/lib -lnetcdf
NETCDF_INCLUDE   = -I/sw/include
HDF_LIB          = -L/sw/lib/ -lmfhdf -ldf -ljpeg -lz
HDF_INCLUDE      = -I/sw/include
endif

# Location of MPI (Message Passing Interface) software

# comment in one or the other, depending on whether you have
# installed the mpich library.  (the first section assumes
# it is installed under /usr/local - change MPI_HOME if other dir.)

ifeq ($(ESMF_COMM),lam)
# with lam-mpi installed in /usr/local:
MPI_HOME       = 
MPI_LIB        = -lmpi -llam 
MPI_INCLUDE    = 
MPIRUN         =  mpirun
endif

ifeq ($(ESMF_COMM),mpich)
# with mpich installed in /usr/local:
ESMC_MPIRUN      = mpirun
MPI_HOME       =  /usr/local
MPI_LIB        = -lmpich -lpmpich
MPI_INCLUDE    = -I${MPI_HOME}/include -DESMF_MPICH=1
MPIRUN         =  ${MPI_HOME}/bin/mpirun
endif

ifeq ($(ESMF_COMM),mpiuni)
# without mpich installed:
MPI_HOME       = ${ESMF_DIR}/src/Infrastructure/stubs/mpiuni
MPI_LIB        = -lmpiuni
MPI_INCLUDE    = -I${MPI_HOME}
MPIRUN         =  ${MPI_HOME}/mpirun
endif

# if you are using esmf with any VTK (visualization tool kit) code, set
# ESMF_VTK to include these libraries.
ifeq ($(ESMF_VTK),1)
MPI_LIB += -L/usr/local/lib/vtk -L/usr/X11R6/lib -lvtkRendering -lvtkIO \
           -lvtkGraphics -lvtkImaging -lSM -lICE \
           -lX11 -lXext -framework Carbon -lvtkftgl \
           -framework AGL -framework OpenGL -lvtkfreetype \
           -lXt -lvtkFiltering -lvtkCommon -framework AppKit -lpthread \
           -lm -lvtkpng -lvtktiff -lvtkzlib -lvtkjpeg -lvtkexpat 
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
AR_EXTRACT         = -x
RM		   = rm -f
OMAKE		   = ${MAKE}
RANLIB		   = ranlib -s
SHELL		   = /bin/sh
SED		   = /usr/bin/sed
SH_LD		   = cc
#
# C and Fortran compiler
#
C_CC		   = cc
C_FC		   = f95 
CXX_CC		   = g++ -fPIC
CXX_FC		   = f95 -YEXT_NAMES=LCS -s 

C_FC_MOD           = -I
C_CLINKER_SLFLAG   = -Wl,-rpath,
C_FLINKER_SLFLAG   = -Wl,-rpath,
C_CLINKER	   = ${C_CC}
C_FLINKER	   = ${C_FC}
C_CCV		   = ${C_CC} --version
C_FCV              = ${C_FC} -V 
C_SYS_LIB	   = ${MPI_LIB} -ldl -lc -lg2c -lm
# Add /usr/lib/libf2c.a if that's what your f77 uses.
# ---------------------------- BOPT - g options ----------------------------
G_COPTFLAGS	   = -g 
G_FOPTFLAGS	   = -g 
# ----------------------------- BOPT - O options -----------------------------
O_COPTFLAGS	   = -O 
O_FOPTFLAGS	   = -O
#
# Fortran compiler 
#
FFLAGS          = -w=x77 -kind=byte -dusty -mismatch_all-gline
F_FREECPP       = -ffree -fpp
F_FIXCPP        = -ffixed -fpp
F_FREENOCPP     = -ffree
F_FIXNOCPP      = -ffixed
#
# C++ compiler
#
CXX_CLINKER_SLFLAG = -Wl,-rpath,
CXX_FLINKER_SLFLAG = -Wl,-rpath,
CXX_CLINKER	   = ${CXX_CC}
CXX_FLINKER	   = ${CXX_CC}
CXX_CCV		   = ${CXX_CC} --version
LOCAL_INCLUDE      =
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
###############################################################################

PARCH		   = mac_osx

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



