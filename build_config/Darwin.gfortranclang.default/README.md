## Darwin, using the GNU Fortran compiler with LLVM Clang C/C++ compiler

Activated by setting: `ESMF_COMPILER=gfortranclang`

Settings for Darwin (Mac OS X), using the GNU gfortran compiler and the
clang C++ compiler (clang++). Note that, on Mac OS X, the system-level
g++ invokes clang++, so unless you ensure that you have a true g++ early
in your path (or build the MPI compiler wrappers to ensure that they
wrap the true g++), you will end up using clang++ even if you think you
are using the GNU C++ compiler, and so you should use this
configuration.

CAUTION:
For ESMF regression testing we set ESMF_F90LINKERDEFAULT to the C++
compiler in build_rules.mk because this is needed for clean handling of
exceptions for this compiler combination. For applications with main
programs in Fortran we have found that this is not always necessary.
However, if your application aborts with a message like "libc++abi:
terminating with uncaught exception", that is a sign that you may need
to link with a C++ compiler rather than a Fortran compiler.
