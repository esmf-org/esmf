# $Id: build_rules.mk,v 1.12 2004/06/07 17:07:15 slswift Exp $
#
# SunOS.default.default.mk
#


#
# Default MPI setting.
#
ifndef ESMF_COMM
export ESMF_COMM := mpi
endif
ifeq ($(ESMF_COMM),default)
export ESMF_COMM := mpi
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

ifeq ($(ESMF_COMM),mpi)
MPI_HOME       = /opt/SUNWhpc
MPI_LIB        = -L${MPI_HOME}/lib -R${MPI_HOME}/lib -lmpi
MPI_INCLUDE    = -I${MPI_HOME}/include
MPIRUN         =  ${MPI_HOME}/bin/mprun
endif

ifeq ($(ESMF_COMM),mpiuni)
MPI_HOME       = ${ESMF_TOP_DIR}/src/Infrastructure/stubs/mpiuni
MPI_LIB        = -lmpiuni
MPI_INCLUDE    = -I${MPI_HOME}
MPIRUN         =  ${MPI_HOME}/mpirun
endif

# Location of the OpenMP library
MP_LIB         = 

# Location of threading library
THREAD_LIB     = 


############################################################
#
AR		   = /usr/ccs/bin/ar
AR_FLAGS	   = cr
RM		   = rm -f
OMAKE		   = ${MAKE}
RANLIB		   = /usr/ccs/bin/ranlib
SHELL		   = /bin/sh
SED		   = /bin/sed
SH_LD		   = /opt/SUNWspro/bin/CC 
#
# C and Fortran compiler 
#
C_CC		   = /opt/SUNWspro/bin/cc -KPIC -dalign -xtarget=native
C_FC		   = /opt/SUNWspro/bin/f90 -xpp=cpp -dalign
C_FC_MOD           = -M
C_CLINKER_SLFLAG   = 
C_FLINKER_SLFLAG   = 
# Must use f90 to link C to get omp libs
C_CLINKER	   = /opt/SUNWspro/bin/f90 -dalign
C_FLINKER	   = /opt/SUNWspro/bin/f90 -dalign -R .
C_CCV		   = ${C_CC} -V
C_FCV              = /opt/SUNWspro/bin/f90 -dalign
C_SYS_LIB	   = -L/opt/SUNWspro/prod/lib -lF77 -lM77 -lfsu -lsunmath -lnsl -lsocket -lgen -ldl -lm
# ---------------------------- BOPT - g options ----------------------------
G_COPTFLAGS	   = 
G_FOPTFLAGS	   = 
# ----------------------------- BOPT - O options -----------------------------
O_COPTFLAGS	   = -fast -xO4 -fsimple=2 -xtarget=native
O_FOPTFLAGS	   = -fast
#
# Fortran compiler 
#
F_FREECPP               = -free -fpp
F_FIXCPP                = -fixed -fpp
F_FREENOCPP             = -free
F_FIXNOCPP              = -fixed
#
# C++ compiler 
#
CXX_CC		   = /opt/SUNWspro/bin/CC -instances=static
CXX_FC		   = /opt/SUNWspro/bin/f90 
CXX_CLINKER_SLFLAG = -Wl,-rpath,
CXX_FLINKER_SLFLAG = -Wl,-rpath,
CXX_CLINKER	   = /opt/SUNWspro/bin/CC 
CXX_FLINKER	   = /opt/SUNWspro/bin/CC 
CXX_CCV		   = ${CXX_CC} -V
CXX_SYS_LIB	   = -ldl -lc -lg2c -lm -lrt
C_F90CXXLD         = /opt/SUNWspro/bin/f90 
C_F90CXXLIBS       = -lfui -lfai -lfai2 -lfsumai -lfprodai -lfminlai \
		     -lfmaxlai -lfminvai -lfmaxvai -lfsu -lsunmath \
                     -lCrun -lCstd -lCrun -lm -lcx -lc -lrt
C_CXXF90LD         = /opt/SUNWspro/bin/CC
C_CXXF90LIBS       = -L/opt/SUNWspro/prod/lib -lfui -lfai -lfai2 -lfsumai \
                     -lfprodai -lfminlai -lfmaxlai -lfminvai -lfmaxvai \
                     -lfsu -lsunmath -lm -lc -lrt
C_CXXSO            = /opt/SUNWspro/bin/CC -G
C_CXXSOLIBS        = -Kpic  -lCstd -lCrun -lm -lw -lcx -lc -lrt
# ------------------------- BOPT - g_c++ options ------------------------------
GCXX_COPTFLAGS	   = 
GCXX_FOPTFLAGS	   = 
# ------------------------- BOPT - O_c++ options ------------------------------
OCXX_COPTFLAGS	   = -O 
OCXX_FOPTFLAGS	   = -O
# -------------------------- BOPT - g_complex options ------------------------
GCOMP_COPTFLAGS	   = 
GCOMP_FOPTFLAGS	   = 
# --------------------------- BOPT - O_complex options -------------------------
OCOMP_COPTFLAGS	   = -O
OCOMP_FOPTFLAGS	   = -O
###############################################################################

PARCH		   = solaris

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


