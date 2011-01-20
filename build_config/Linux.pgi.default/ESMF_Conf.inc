#if 0
$Id: ESMF_Conf.inc,v 1.14 2011/01/20 22:35:15 w6ws Exp $

Earth System Modeling Framework
Copyright 2002-2011, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the University of Illinois-NCSA License.
#endif

#if 0
Define the proper way to flush Fortran units.  Typically this will
be one of the following:

  * Fortran-2003 FLUSH statement (preferred and default)
  * Two argument FLUSH call
  * One argument FLUSH call

Use the following macro for pre-7.1 PGI compilers to enable
the flush library call.
#endif

#if 0
-#-define ESMF_IOFlushMacro(unit,status) call flush (unit)
#endif

#if 0
Define the proper way to read the command line.

By default, the F2003 command line instrinsics will be used.
Use the following macro for pre-7.1 PGI compilers to enable
'getarg' code.
#endif

#if 0
-#-define ESMF_NEEDSGETARG
#endif

#if 0
the PGI compiler is strict F90, which does not allow members of a type
to have initializers.  this allows all other architectures to have them
by letting us write conditional code.
#endif

#define ESMF_NO_INITIALIZERS 1

#if 0
the PGI compiler has a bug which prevents forward pointer references from
working if also using the sequence keyword.  for PGI only, dont sequence
the State derived types.
#endif

#define ESMF_SEQUENCE_BUG 1


