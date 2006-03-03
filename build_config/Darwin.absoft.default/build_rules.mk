#  $Id: build_rules.mk,v 1.19 2006/03/03 16:15:56 nscollins Exp $
#
#  Darwin.absoft.default
#


#
#  Make sure that ESMF_PREC is set to 32
#
ESMF_PREC = 32

#
# Default MPI setting.
#
ifeq ($(ESMF_COMM),default)
export ESMF_COMM := mpiuni
endif


############################################################
#
# location of external libs.  if you want to use any of these,
# define ESMF_SITE to my_site so the build system can find it,
# copy this file into Darwin.absoft.my_site, and uncomment the
# libs you want included.  remove the rest of this file since
# both this file and the site file will be included.

# LAPACK_INCLUDE   = 
# LAPACK_LIB       = -L/usr/local/lib -llapack -lessl
# NETCDF_INCLUDE   = -I/usr/local/include/netcdf
# NETCDF_LIB       = -L/usr/local/lib -lnetcdf
# HDF_INCLUDE      = -I/usr/local/include/hdf
# HDF_LIB          = -L/usr/local/lib -lmfhdf
# BLAS_INCLUDE     = 
# BLAS_LIB         = -lblas

#
############################################################

# Location of MPI (Message Passing Interface) software

# Set ESMF_COMM depending on whether you have installed the mpich 
# or lam library.  The default location is to have the include files
# and libs installed in /usr/local - if they are someplace else,
# set MPI_HOME first.  (the mpiuni case is handled in the common.mk file.)

ifeq ($(ESMF_COMM),lam)
# with lam-mpi installed in $MPI_HOME or /usr/local:
# yes, -lmpi is in there twice.  this seems to be necessary to avoid
# an undefined reference to _MPI_Comm_dup for some reason.
MPI_LIB        += -lmpi -llam -llamf77mpi -lmpi
endif

ifeq ($(ESMF_COMM),mpich)
# with mpich installed in $MPI_HOME or /usr/local:
MPI_INCLUDE    += -DESMF_MPICH=1
MPI_LIB        += -lmpich -lpmpich
endif


############################################################
# if you are using ESMF with any VTK (visualization tool kit) code on the Mac,
# set ESMF_VTK to include these libraries.
ifeq ($(ESMF_VTK),1)
MPI_LIB += -L/usr/local/lib/vtk -L/usr/X11R6/lib -lvtkRendering -lvtkIO \
           -lvtkGraphics -lvtkImaging -lSM -lICE \
           -lX11 -lXext -framework Carbon -lvtkftgl \
           -framework AGL -framework OpenGL -lvtkfreetype \
           -lXt -lvtkFiltering -lvtkCommon -framework AppKit -lpthread \
           -lm -lvtkpng -lvtktiff -lvtkzlib -lvtkjpeg -lvtkexpat 
endif
############################################################




############################################################

# commands which are not in the default location
SED		   = /usr/bin/sed

#
# compilers
#
C_CC		= cc
C_CXX		= g++
C_FC		= f95 
C_FC_MOD        = -p


# version info

C_CCV		= ${C_CC} --version
C_CXXV		= ${C_CXX} --version
C_FCV           = (x="`f95 -V 2>/dev/null`"; \
                  if [ -n "$$x" ] ; then echo $$x ; else f90fe -V ; fi)
# on absoft 8 and before, docs say f95 -V should work, but it causes an error.
# f90fe -V prints good version info.   absoft 9 and later, however, fixes this
# and now f90 -V prints good info, and f90fe gives license errors.  so try
# doing both and take the one which is not null.

#
# Fortran flags
#
CFLAGS          += -fPIC
FFLAGS          += -YEXT_NAMES=LCS -s
F_FREECPP       = -ffree
F_FIXCPP        = -ffixed
F_FREENOCPP     = -ffree
F_FIXNOCPP      = -ffixed


# for fortran executables use c++ for linking
C_F90CXXLD      = $(C_CXX)
C_FLINKER       = $(C_CXX)

# no -rpath flag supported
C_SLFLAG =

C_F90CXXLIBS    = -lstdc++ -L/Applications/Absoft/lib -lf90math -lfio -lf77math
C_CXXF90LIBS    = -lstdc++ -L/Applications/Absoft/lib -lf90math -lfio -lf77math 

##############################################################################

PARCH		   = mac_osx

# set this to libesmf.so to build a shared lib, and add the proper flags
SL_LIBS_TO_MAKE = 
C_SL_LIBOPTS  = 


