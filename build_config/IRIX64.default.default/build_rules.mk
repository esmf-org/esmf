# $Id: build_rules.mk,v 1.10 2004/06/07 17:06:41 slswift Exp $
# 
# IRIX64.default.default.mk
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
BLAS_LIB       = -latlas ${FC_LIB}
LAPACK_LIB     = -llapacko
ifeq ($(ESMF_PREC),32)
NETCDF_LIB       = -L/usr/local/lib -lnetcdf
NETCDF_INCLUDE   = -I/usr/local/include
HDF_LIB          = -L /usr/local/lib -lmfhdf -ldf -ljpeg -lz
HDF_INCLUDE      = -I /usr/local/include
endif
# end 32 bit section
ifeq ($(ESMF_PREC),64)
NETCDF_LIB       = -L/usr/local/lib -lnetcdf
NETCDF_INCLUDE   = -I/usr/local/include
HDF_LIB          = -L /usr/local/lib -lmfhdf -ldf -ljpeg -lz
HDF_INCLUDE      = -I /usr/local/include
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
# The following lines can be used with MPICH
#
ifeq ($(ESMF_COMM),mpich)
MPI_LIB        = -L/home/alice/mpich/lib/IRIX64/ch_p4 -lmpi
MPI_INCLUDE    = -DESMC_HAVE_INT_MPI_COMM -I/home/alice/mpich/include
MPIRUN         =  /home/alice/mpich/lib/IRIX64/ch_p4/mpirun
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

LD		   = ld -n32 
#
# C and Fortran compiler
#
C_CC		   = CC -n32 -mp -woff 1164 -LANG:std
C_FC		   = f90 -cpp -n32 -mp -macro_expand
C_CLINKER	   = cc  -n32 -mp -Wl,-woff,84,-woff,85,-woff,134 -MP:open_mp=ON
C_FLINKER	   = f90  -n32 -mp -Wl,-woff,84,-woff,85,-woff,134 -MP:open_mp=ON
#
# C++ compiler
#
CXX_CC		   = CC -n32 -mp -woff 1164 -LANG:std
CXX_FC		   = f90 -cpp -n32 -mp -macro_expand
CXX_CLINKER	   = CC -n32 -mp -Wl,-woff,84,-woff,85,-woff,134
CXX_FLINKER	   = CC -n32 -mp -Wl,-woff,84,-woff,85,-woff,134
C_CXXF90LD         = CC -n32
C_F90CXXLD         = f90 -n32
C_CXXF90LIBS       = -rpath . -lftn90 -lftn -lfortran
C_F90CXXLIBS       = -rpath . -lCsup -lC -lCio -lc 
endif


############################################################
#
ifeq ($(ESMF_PREC), 64)

LD		   = ld -64 
#
# C and Fortran compiler 
#
C_CC		   = CC -64 -mp -woff 1164 -no_prelink -ptused -LANG:std
C_FC		   = f90 -cpp -64 -mp -macro_expand
C_CLINKER	   = cc  -64 -mp -Wl,-woff,84,-woff,85,-woff,134 -MP:open_mp=ON
C_FLINKER	   = f90  -64 -mp -Wl,-woff,84,-woff,85,-woff,134 -MP:open_mp=ON
#
# C++ compiler 
#
CXX_CC		   = CC -64 -mp -woff 1164 -no_prelink -ptused -LANG:std
CXX_FC		   = f90 -cpp -64 -mp -macro_expand
CXX_CLINKER	   = CC -64 -mp -Wl,-woff,84,-woff,85,-woff,134
CXX_FLINKER	   = CC -64 -mp -Wl,-woff,84,-woff,85,-woff,134
C_CXXF90LD         = CC -64
C_F90CXXLD         = f90 -64
C_CXXF90LIBS       = -rpath . -lftn90 -lftn -lfortran
C_F90CXXLIBS       = -rpath . -lC -lCio -lc
C_CXXSO            =  CC -64 -shared -rpath .
###########################################################################

endif

############################################################
# common

AR		   = ar
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
C_CLINKER_SLFLAG   = -rpath
C_FLINKER_SLFLAG   = -rpath
C_CCV		   = cc -version
C_FCV              = f90 -version
C_SYS_LIB	   = -lfpe -lfortran -lftn -lfastm -lmalloc 
# ---------------------------- BOPT - g options ----------------------------
G_COPTFLAGS	   = -g 
G_FOPTFLAGS	   = -g 
# ----------------------------- BOPT - O options -----------------------------
#O_COPTFLAGS	   = -Ofast=ip27
O_COPTFLAGS	   = -O3
#O_FOPTFLAGS	   = -Ofast=ip27 -IPA:cprop=OFF -OPT:IEEE_arithmetic=1
O_FOPTFLAGS	   = -O3
# ########################## Fortran compiler ##############################
#
F_FREECPP       = -freeform -cpp
F_FIXCPP        = -fixedform -cpp -extend_source
F_FREENOCPP     = -freeform -nocpp
F_FIXNOCPP      = -fixedform -nocpp -extend_source
# ########################## C++ compiler ##################################
#
CXX_CLINKER_SLFLAG = -rpath
CXX_FLINKER_SLFLAG = -rpath
CXX_CCV		   = CC -version
CXX_SYS_LIB	   = -lfpe -lcomplex -lfortran -lftn -lfastm -lmalloc 
# ------------------------- BOPT - g_c++ options ------------------------------
GCXX_COPTFLAGS	   = -g
GCXX_FOPTFLAGS	   = -g
# ------------------------- BOPT - O_c++ options ------------------------------
OCXX_COPTFLAGS	   = -O3 -OPT:Olimit=6500
OCXX_FOPTFLAGS	   = -O3
# -------------------------- BOPT - g_complex options ------------------------
GCOMP_COPTFLAGS	   = -g 
GCOMP_FOPTFLAGS	   = -g
# --------------------------- BOPT - O_complex options -------------------------
OCOMP_COPTFLAGS	   = -O3 -OPT:Olimit=6500
OCOMP_FOPTFLAGS	   = -O3
##################################################################################

PARCH		   = IRIX

SL_SUFFIX   = so
SL_LIBOPTS  = -rpath $(ESMF_LIBDIR) -shared
SL_LINKOPTS = 
SL_F_LINKER = $(F90CXXLD)
SL_C_LINKER = $(CXXF90LD)
SL_LIB_LINKER = $(CXXF90LD)
SL_LIBS_TO_MAKE = libesmf 

##### end common section


###################
#
shared: build_shared

#
# Notes:
#
# -lpthread is required by AMS package. It should follow -lmpi, hence, is specified
# in the variable SYS_LIB. If libpthread.so is not avilable on this machine, you can
# comment out this line.
#
# -trapuv initializes memory with NaNs, so that uninitialized errors are caught.

# For IRIX version less than 6.2, 
#         replace "-OPT:Olimit=5000" with "-OPT:fprop_limit=5000"

#	IF your O2K has ip25 processor, please change
#       ip27 to ip25
#
#COPTFLAGS  = -OPT:Olimit=6000 -Ofast=ip27
#FOPTFLAGS  = -Ofast=ip27 -IPA:cprop=OFF -OPT:IEEE_arithmetic=1
#
# Some examples crash (ts/ex/tests/ex1f) with the cprop optimization
#
# For IRIX Release less than,6.2 the above should probably be replaced by
# FC_LIB         = -lsun -lF77 -lU77 -lI77 -lisam
# FC_LIB         = -lF77 -lU77 -lI77 -lisam
