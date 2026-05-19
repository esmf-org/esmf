## Darwin, using the NAG Fortran compiler with Apple Clang C/C++ compiler

Activated by setting: `ESMF_COMPILER=nagclang`

Settings for Darwin, using the commercial NAGware Fortran compiler and the
Apple Clang C/C++ compiler (clang/clang++).

CAUTION:
For ESMF regression testing we set ESMF_F90LINKERDEFAULT to the C++
compiler in build_rules.mk because this is needed for clean handling of
exceptions for this compiler combination. For applications with main
programs in Fortran we have found that this is not always necessary.
However, if your application aborts with a message like "libc++abi:
terminating with uncaught exception", that is a sign that you may need
to link with a C++ compiler rather than a Fortran compiler.
