## MinGW, using the Intel compilers

Activated by setting: `ESMF_COMPILER=intel`

Settings for MinGW/MSYS, using the Intel ifort Fortran compiler, and the
Intel icl C++ compiler.

Notes:

1.) This port was performed under Windows CCS Server 2003,
using MinGW 5.1.4 and MSYS 1.0.11.  MinGW and MSYS are available
from http://www.mingw.org.

2.) The MinGW version of gcc is required for preprocessing.

3.) Prior to running the ESMF make, ensure that all compiler
environment variables (in particular, PATH, LIB, and INCLUDE)
are set up.  These should point to the correct Intel compiler,
Visual Studio, and SDK directories.
