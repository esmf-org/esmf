## MinGW, using the Intel Fortran compiler with Microsoft C/C++

Activated by setting: `ESMF_COMPILER=intelcl`

Settings for MinGW/MSYS, using the Intel ifort Fortran compiler, and the
Microsoft 'cl' C++ compiler.  The cl compiler is part of Visual Studio.

Notes:

1.) This port was performed under Windows CCS Server 2003,
using MinGW 5.1.4 and MSYS 1.0.11.  MinGW and MSYS are available
from http://www.mingw.org.

2.) The MinGW version of gcc is required for preprocessing.

3.) Prior to running the ESMF make, ensure that all compiler
environment variables (in particular, PATH, LIB, and INCLUDE)
are set up.  These should point to the correct Intel compiler,
Visual Studio, and SDK directories.
