## IRIX64, using the MIPSPro compilers

Activated by default.

Settings for SGI IRIX64 operating system, using the MIPSPro Fortran
and C++ compilers. (If you are looking for the Altix makefiles,
see the Linux.intel directory.)

Recommended software levels:

  IRIX		6.5.22 or later
  Compilers	7.4.3m or later
  MPT		1.9
  SCSL		1.4.1.3
  gcc		v3.3 or later
  gmake		v3.80 or later

Inst-compatible versions of gcc (used in ESMF for preprocessing)
and gmake are available from http://freeware.sgi.com and from
http://www.nekochan.net.

The default ESMF_ABI is 64.  To compile for 32-bit, set ESMF_ABI
to 32.


Compiling on older IRIX systems:

For systems running a 32-bit only kernel, such as the O2 and Indy,
simply create a symbolic link called IRIX.default.default and
link it to IRIX64.default.default.  Then set ESMF_ABI to 32.
