## Linux, using the NAG Fortran compiler with Intel C/C++

Activated by setting: `ESMF_COMPILER=nagintel`

Settings for Linux, using the commercial NAGware Fortran compiler and the
commercial Intel C++ compiler (icpc).

As of NAG 5.2, the compiler is invoked with the 'nagfor' command.  If you
are using an older level of the compiler, set the ESMF_F90COMPILER and
ESMF_F90LINKER environment variables to 'f95' or whatever it is called on
your system.

Also if using a pre-5.2 compiler, you will need to enable the
ESMF_NAG_UNIXIO_MODULE and ESMF_IOFlushMacro macros in the
ESMF_Conf.inc file.
