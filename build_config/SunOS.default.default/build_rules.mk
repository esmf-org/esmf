# $Id: build_rules.mk,v 1.14 2005/04/11 15:53:38 nscollins Exp $
#
# SunOS.default.default
#


#
# Default MPI setting.
#
ifeq ($(ESMF_COMM),default)
export ESMF_COMM := mpi
endif



############################################################
#
# location of external libs.  if you want to use any of these,
# define ESMF_SITE to my_site so the build system can find it,
# copy this file into Linux.absoft.my_site, and uncomment the
# libs you want included.  remove the rest of this file since
# both this file and the site file will be included.

# LAPACK_INCLUDE   = 
# LAPACK_LIB       = -L/usr/local/lib -llapack
# NETCDF_INCLUDE   = -I/usr/local/include/netcdf
# NETCDF_LIB       = -L/usr/local/lib -lnetcdf
# HDF_INCLUDE      = -I/usr/local/include/hdf
# HDF_LIB          = -L/usr/local/lib/ -lmfhdf -ldf -ljpeg -lz
# BLAS_INCLUDE     = 
# BLAS_LIB         = -latlas

#
############################################################

# Location of MPI (Message Passing Interface) software

ifeq ($(ESMF_COMM),mpi)
MPI_HOME       = /opt/SUNWhpc
MPI_LIB        += -R${MPI_HOME}/lib -lmpi
endif


# commands in a different place from defaults
AR		   = /usr/ccs/bin/ar
RANLIB		   = /usr/ccs/bin/ranlib

# compilers
C_CC		   = /opt/SUNWspro/bin/cc -KPIC -dalign -xtarget=native
C_CXX		   = /opt/SUNWspro/bin/CC -instances=static
C_FC		   = /opt/SUNWspro/bin/f90 -xpp=cpp -dalign

C_FC_MOD           = -M

# Must use f90 to link C to get omp libs
C_CLINKER	   = /opt/SUNWspro/bin/f90 -dalign
C_FLINKER	   = /opt/SUNWspro/bin/f90 -dalign -R .

C_CCV		   = ${C_CC} -V
C_CXXV		   = ${C_CXX} -V
C_FCV              = /opt/SUNWspro/bin/f90 -dalign


# compiler flags
O_CFLAGS	   += -fast -xO4 -fsimple=2 -xtarget=native
O_FFLAGS	   += -fast

# Fortran compiler flags
F_FREECPP               = -free -fpp
F_FIXCPP                = -fixed -fpp
F_FREENOCPP             = -free
F_FIXNOCPP              = -fixed


C_LINKOPTS = -Kpic  -lCstd -lCrun -lm -lw -lcx -lc -lrt

C_F90CXXLIBS       = -lfui -lfai -lfai2 -lfsumai -lfprodai -lfminlai \
		     -lfmaxlai -lfminvai -lfmaxvai -lfsu -lsunmath \
                     -lCrun -lCstd -lCrun -lm -lcx -lc -lrt
C_CXXF90LIBS       = -L/opt/SUNWspro/prod/lib -lfui -lfai -lfai2 -lfsumai \
                     -lfprodai -lfminlai -lfmaxlai -lfminvai -lfmaxvai \
                     -lfsu -lsunmath -lm -lc -lrt


###############################################################################

PARCH		   = solaris

SL_LIBS_TO_MAKE = 

C_SL_LIBOPTS  = -G


