# $Id: build_rules.mk,v 1.14 2005/04/11 15:53:37 nscollins Exp $
#
# Linux.lahey.default
#
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
# copy this file into Linux.lahey.my_site, and uncomment the
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

ifeq ($(ESMF_COMM),lam)
# with lam-mpi installed in MPI_HOME or /usr/local:
MPI_LIB        += -lmpi -llam
# lam needs pthreads
THREAD_LIB      = -lpthread
endif

ifeq ($(ESMF_COMM),mpich)
# with mpich installed in MPI_HOME or /usr/local:
MPI_INCLUDE    += -DESMF_MPICH=1
MPI_LIB        += -lmpich
MPIRUN         +=  $(ESMF_NODES)
endif


############################################################

#
# compilers and flags
#
C_CC		   = gcc
C_CXX		   = g++ -fPIC
C_FC		   = lf95

C_CCV		   = ${C_CC} --version
C_CXXV		   = ${C_CXX} --version
C_FCV              = lf95 --version

#
# Fortran compiler flags
#
F_FREECPP          = --nfix -Cpp
F_FIXCPP           = --fix -Cpp
F_FREENOCPP        = --nfix 
F_FIXNOCPP         = --fix


# use dir values in LD_LIBRARY_PATH, others are defaults if no other choice.
ifeq ($(origin LD_LIBRARY_PATH), environment)
C_F90CXXLIBS       = $(LIB_PATHS) $(LD_PATHS) \
                     -lstdc++ -lgcc -lg2c -lrt

C_CXXF90LIBS       = $(LIB_PATHS) $(LD_PATHS) -lfj9i6 -lfj9ipp -lfj9f6 \
                      -lfj9fpp -lfj9e6 -lfccx86_6a -lrt
else
C_F90CXXLIBS       = -Wl,-rpath /usr/lib/gcc-lib/i386-redhat-linux/3.2.2 \
                     -Wl,-rpath /usr/local/lf9560/lib \
                     -L/usr/lib/gcc-lib/i386-redhat-linux/3.2.2 \
                     -lstdc++ -lgcc -lg2c -lrt

C_CXXF90LIBS       = -L/usr/local/lf9560/lib -lfj9i6 -lfj9ipp -lfj9f6 -lfj9fpp \
                      -lfj9e6 -lfccx86_6a -lrt
endif

##############################################################################

PARCH		   = linux_lf95

# this platform makes a shared lib
C_SL_LIBOPTS       = -shared


