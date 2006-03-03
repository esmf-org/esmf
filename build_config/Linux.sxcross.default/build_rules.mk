# $Id: build_rules.mk,v 1.1 2006/03/03 21:26:50 nscollins Exp $
# 
# IRIX64.default.default.mk
#
export ESMF_PREC := 64
#
# Default MPI setting.
#
ifndef ESMF_COMM
export ESMF_COMM := sxmpi
endif
ifeq ($(ESMF_COMM),default)
export ESMF_COMM := sxmpi
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
BLAS_LIB         = -lblas
LAPACK_LIB       = -llapack
NETCDF_LIB       = -lnetcdf_stubs
NETCDF_INCLUDE   = -I${ESMF_DIR}/src/Infrastructure/stubs/netcdf_stubs
HDF_LIB          =
HDF_INCLUDE      =
else
BLAS_LIB         = -lblas
LAPACK_LIB       = -llapack
ifeq ($(ESMF_PREC),64)
NETCDF_LIB       = -L /usr/local/lib -lnetcdf
NETCDF_INCLUDE   = -I /usr/local/include
HDF_LIB          =
HDF_INCLUDE      =
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
# Version 1.1 or NEC's version of MPI you MUST retain it.
#
ifeq ($(ESMF_COMM),sxmpi)
MPI_INCLUDE     = -DESMC_HAVE_INT_MPI_COMM
MPIRUN          = ${ESMC_MPIRUN}
endif

#
# The following is for mpiuni
#
ifeq ($(ESMF_COMM),mpiuni)
MPI_HOME        = ${ESMF_DIR}/src/Infrastructure/stubs/mpiuni
MPI_LIB         = -lmpiuni
MPI_INCLUDE     = -I${MPI_HOME}
MPIRUN          = ${MPI_HOME}/mpirun
endif

#
# The following lines can be used with MPIUNI
# (what is the difference between this and the one above?)
#
#MPI_LIB         =${LDIR}/libmpiuni.a
#MPI_INCLUDE     = -I${ESMF_DIR}/src/sys/mpiuni -DESMC_HAVE_INT_MPI_COMM
#MPIRUN          = ${ESMF_DIR}/src/sys/mpiuni/mpirun


############################################################

ifeq ($(ESMF_PREC),32)

echo "system is 64 bit#
exit

endif


############################################################
#
ifeq ($(ESMF_PREC), 64)

LD		   = sxmpic++ 
#
# C and Fortran compiler 
#
C_CC		   = sxmpic++ -Xa 
C_FC		   = sxmpif90 -EP -dW
C_CLINKER	   = sxmpic++ -Xa
C_FLINKER	   = sxmpif90
#
# C++ compiler 
#
CXX_CC		   = sxmpic++ -K exceptions
CXX_FC		   = sxmpif90 -EP -dW
CXX_CLINKER	   = sxmpic++
CXX_FLINKER	   = sxmpif90
C_CXXF90LD         = sxmpic++
C_F90CXXLD         = sxmpif90
C_CXXF90LIBS       =
C_F90CXXLIBS       = -lpthread
C_CXXSO            =
SL_ABIOPTS         =
###########################################################################

endif

############################################################
# common

AR		   = sxar
AR_FLAGS	   = cr
AR_EXTRACT         = -x
RM		   = rm -f
RANLIB		   = true
OMAKE		   = ${MAKE}
SHELL		   = /bin/sh
SED		   = /bin/sed
#
# C and Fortran compiler 
#
C_FC_MOD           = -I
C_CLINKER_SLFLAG   =
C_FLINKER_SLFLAG   =
C_CCV		   = sxmpic++ -version
C_FCV              = f90 -V
C_SYS_LIB	   =  
# ---------------------------- BOPT - g options ----------------------------
G_COPTFLAGS	   = -g 
G_FOPTFLAGS	   = -g 
# ----------------------------- BOPT - O options -----------------------------
O_COPTFLAGS	   =
O_FOPTFLAGS	   =
# ########################## Fortran compiler ##############################
#
F_FREECPP       = -f4 -EP
F_FIXCPP        = -f3 -EP
F_FREENOCPP     = -f4
F_FIXNOCPP      = -f3
# ########################## C++ compiler ##################################
#
CXX_CLINKER_SLFLAG =
CXX_FLINKER_SLFLAG =
CXX_CCV		   = mpisxc++ -V
CXX_SYS_LIB	   = 
# ------------------------- BOPT - g_c++ options ------------------------------
GCXX_COPTFLAGS	   = -g
GCXX_FOPTFLAGS	   = -g
# ------------------------- BOPT - O_c++ options ------------------------------
OCXX_COPTFLAGS	   =
OCXX_FOPTFLAGS	   =
# -------------------------- BOPT - g_complex options ------------------------
GCOMP_COPTFLAGS	   = -g 
GCOMP_FOPTFLAGS	   = -g
# --------------------------- BOPT - O_complex options -------------------------
OCOMP_COPTFLAGS	   =
OCOMP_FOPTFLAGS	   =
###############################################################################

PARCH		   = sxcross

# set this to libesmf to build a shared library
SL_LIBS_TO_MAKE = libesmf

SL_SUFFIX   = so
SL_LIBOPTS  = $(SL_ABIOPTS) -L $(ESMF_LIBDIR) -shared
SL_LINKOPTS = 
SL_F_LINKER = $(F90CXXLD)
SL_C_LINKER = $(CXXF90LD)
SL_LIB_LINKER = $(CXXF90LD)


