#if 0
$Id: ESMF_Conf.inc,v 1.3 2005/01/11 17:17:26 nscollins Exp $

Earth System Modeling Framework
Copyright 2002-2003, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the GPL.
#endif

#if 0
in theory absoft supports initializers, but in practice with the number
of modules we include, it seems to cause problems when enabled.
#endif

#define ESMF_NO_INITIALIZERS 1

#if 0
this build is not running the field exclusive system test;
it is hanging in the state reconcile call.  to prevent the entire
build from freezing on this one test, simply error out until we
find and fix the problem.
#endif

#define ESMF_FieldExcl_AUTO_FAIL 1

