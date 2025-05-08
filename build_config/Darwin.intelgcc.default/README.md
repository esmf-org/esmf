## Darwin, using the Intel Fortran compiler and GNU C/C++ compiler

Activated by setting: `ESMF_COMPILER=intelgcc`

Settings for Darwin (Mac OS X) machines with Intel processors, using the
commercial Intel Fortran (ifort) compiler and GNU C++ compiler (g++).

On some Darwin systems, the clang compiler is provided as an alias to g++.
This is detected and properly supported.  On systems where both g++ and clang
exist, ensure that the PATH environement variable is set correctly such that
the desired c++ environment is used.  For example, if clang is the desired
compiler, the path to it should be prior to the path to the gcc bin directory.
