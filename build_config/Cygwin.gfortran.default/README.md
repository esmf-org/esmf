## Cygwin, using the GNU compilers

Activated by setting: `ESMF_COMPILER=gfortran`

ESMF has been tested with both 32-bit cygwin and 64-bit cygwin64.
Cygwin should have the following optional packages installed:

  - binutils
  - gcc (be sure to deselect source code box unless you need it)
  - gcc-core, gcc-fortran, and gcc-g++
  - make

ESMF requires gfortran 4.3 or later.  The build procedures assume
that the gfortran and g++ packages have been installed via the
cygwin setup installer.

If you are running an older version of cygwin where the gnu compilers
are named gfortran-4 and/or g++-4, the following environment variables
should be set as appropriate:

 export ESMF_F90=gfortran-4
 export ESMF_CXX=g++-4


### NetCDF installation

When installing NetCDF via the Cygwin Setup package tool, the following
packages must be selected:

  - libnetcdf-devel
  - libnetcdf-fortran-devel (also selects libnetcdf-fortran_5)
  - netcdf (also selects libnetcdf7)
  - netcdf-fortran

The following environment variables should then be set:

  export ESMF_NETCDF=split
  export ESMF_NETCDF_INCLUDE=/usr/include
 

### Accessing the ESMF shared library

A shared (.dll.a) library is automatically created while building
the ESMF library.  On Windows, applications typically find needed
.dlls via the PATH environment variable.

Prior to running the check-out tests, or any application, please
set the PATH environment variable to the location of the ESMF library.
E.g.:

  export PATH=$PATH:$ESMF_DIR/lib/libO/Cygwin.gfortran.64.mpiuni.default


### Windows Compute Cluster Server (CCS) 2003 notes

ESMF has been tested on Windows CCS using the Microsoft MPI.  To
use MSMPI, set:

  export ESMF_COMM=msmpi.

Two issues need to be addressed, prior to compiling ESMF:

1.) Typically, the CCS MSMPI header and library files reside
in /cygdrive/c/"Program Files"/"Microsoft Compute Cluster Pack".
However the spaces in the path name can confuse the ESMF makefile
system.

To work around this problem, create a symbolic link from the
ESMF_DIR directory which points to the HPC directory:

  cd $ESMF_DIR
  ln -s /cygdrive/c/"Program Files"/"Microsoft Compute Cluster Pack" msmpidir

Then set the ESMF_MSMPIDIR environment variable to point to the
symbolic link:

  ESMF_MSMPIDIR $ESMF_DIR/msmpidir

2.) The Microsoft mpif.h header file contains conditional code
which is written for the Intel Fortran compiler.  Gfortran does
not know about the !DEC$ directives which are used to implement
the conditional code.  A local copy of mpif.h must be created
in this directory ($ESMF_DIR/build_config/Cygwin.gfortran.default)
with the necessary corrections.

  cp $ESMF_MSMPIDIR/Include/mpif.h .

Using the editor of your choice, find the following statements,
near line 308:

!DEC$ IF DEFINED(_WIN64)
       PARAMETER (MPI_ADDRESS_KIND=8)
!DEC$ ELSE
       PARAMETER (MPI_ADDRESS_KIND=4)
!DEC$ ENDIF

Comment out the first PARAMETER statement as follows:

!DEC$ IF DEFINED (_WIN64)
!      PARAMETER (MPI_ADDRESS_KIND=8)
!DEC$ ELSE
       PARAMETER (MPI_ADDRESS_KIND=4)
!DEC$ ENDIF

You may now proceed to building ESMF.
