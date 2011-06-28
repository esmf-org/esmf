#if 0
$Id: ESMF_Conf.inc,v 1.13 2011/06/28 19:56:21 w6ws Exp $

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

#endif

#if (ESMF_PGIVERSION_MAJOR < 8)
#define ESMF_IOFlushMacro(unit,status) call flush (unit)
#endif

#if 0
Define the proper way to read the command line.

By default, the F2003 command line instrinsics will be used.

Use getarg code for pre-11.2 compilers to avoid PGI library bug.
#endif

#if (ESMF_PGIVERSION_MAJOR < 11)
#define ESMF_NEEDSGETARG
#endif

#if (ESMF_PGIVERSION_MAJOR == 11) && (ESMF_PGIVERSION_MINOR < 2)
#define ESMF_NEEDSGETARG
#endif

#if 0
PGI 11.5 and previous have a bug when creating named constants which
are arrays of zero length.  See ticket #3341575.
#endif

#define ESMF_PGI_NAMEDCONSTANT_BUG

#if 0
Older PGI compiler were strict F90, and did not allow derived type
components to have F95 initializers.  This allows all other architectures
to have them by letting us write conditional code.
#endif

#if (ESMF_PGIVERSION_MAJOR < 8)
#define ESMF_NO_INITIALIZERS 1
#endif

#if 0
Older PGI compilers had a bug which prevents forward pointer references from
working if also using the sequence keyword.  for PGI only, dont sequence
the State derived types.
#endif

#if (ESMF_PGIVERSION_MAJOR < 8)
#define ESMF_SEQUENCE_BUG 1
#endif
