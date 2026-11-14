## Darwin, using the NAG Fortran compiler and GNU C/C++ compiler

Activated by setting: `ESMF_COMPILER=nag`

Settings for Darwin, using the commercial NAGware Fortran compiler and the
GNU C++ compiler (g++).

On Mac OS X, the system-level g++ invokes clang++, so unless you ensure
that you have a true g++ early in your path (or build the MPI compiler
wrappers to ensure that they wrap the true g++), you will end up using
clang++ even if you think you are using the GNU C++ compiler. In that
case, you should use the `nagclang` configuration instead of this
`nag` configuration.
