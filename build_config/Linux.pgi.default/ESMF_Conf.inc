#if 0
$Id: ESMF_Conf.inc,v 1.2 2005/01/13 00:02:53 nscollins Exp $

Earth System Modeling Framework
Copyright 2002-2003, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the GPL.
#endif


#if 0
the PGI compiler is strict F90, which does not allow members of a type
to have initializers.  this allows all other architectures to have them
by letting us write conditional code.
#endif

#define ESMF_NO_INITIALIZERS 1

#if 0
the PGI compiler has a bug which prevents forward pointer references from
working if also using the sequence keyword.  for PGI only, don't sequence
the State derived types.
#endif

#define ESMF_SEQUENCE_BUG 1

#if 0
this build is not running the field exclusive system test;
it is hanging in the state reconcile call.  to prevent the entire
build from freezing on this one test, simply error out until we
find and fix the problem.
#endif

#define ESMF_FieldExcl_AUTO_FAIL 1

