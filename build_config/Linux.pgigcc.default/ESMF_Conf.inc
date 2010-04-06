#if 0
$Id: ESMF_Conf.inc,v 1.10 2010/04/06 00:01:05 w6ws Exp $

Earth System Modeling Framework
Copyright 2002-2010, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the University of Illinois-NCSA License.
#endif

#if 0
Define the proper way to flush Fortran units.  Typically this will
be one of the following:

  * Fortran-2003 FLUSH statement (preferred)
  * Two argument FLUSH call
  * One argument FLUSH call

#endif

#define ESMF_IOFlushMacro(unit,status) call flush (unit)

#if 0
Define the proper way to read the command line.  Since PGI did not
have the Fortran-2003 command line intrinsics until 10.0, nor 
does it have Posix calls, use use getarg for now.

#endif

#define ESMF_NEEDSGETARG

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


