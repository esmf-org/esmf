## Darwin, using the GNU compilers

Activated by setting: `ESMF_COMPILER=gfortran`

Settings for Darwin (Mac OS X), using the GNU gfortran compiler
and GNU C++ compiler (g++).

On Mac OS X, the system-level g++ invokes clang++, so unless you ensure
that you have a true g++ early in your path (or build the MPI compiler
wrappers to ensure that they wrap the true g++), you will end up using
clang++ even if you think you are using the GNU C++ compiler. In that
case, you should use the gfortranclang configuration instead of this
gfortran configuration.
