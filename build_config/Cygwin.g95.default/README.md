## Cygwin, using the g95 Fortran compiler

Activated by setting: `ESMF_COMPILER=g95`

ESMF has been tested with 32-bit cygwin and the g95 compiler.
Cygwin should have the following optional packages installed:

  - binutils
  - gcc (be sure to deselect source code box unless you need it)
  - gcc-core and gcc-g++
  - make

The ESMF build procedures assume that the g++ package has been
installed via the cygwin setup installer.

G95 may be downloaded from http://www.g95.org.  To indicate that
you wish to use g95 as the Fortran compiler, set:

 export ESMF_COMPILER=g95

If you are running an older version of cygwin where the g++ compiler
is named g++-4, the following environment variable should be set as
appropriate:

 export ESMF_CXX=g++-4


### Accessing the ESMF shared library

A shared (.dll.a) library is automatically created while building
the ESMF library.  On Windows, applications typically find needed
.dlls via the PATH environment variable.

Prior to running the check-out tests, or any application, please
set the PATH environment variable to the location of the ESMF library.
E.g.:

  export PATH=$PATH:$ESMF_DIR/lib/libO/Cygwin.g95.32.mpiuni.default
