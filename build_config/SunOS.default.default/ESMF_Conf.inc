#if 0
$Id: ESMF_Conf.inc,v 1.3 2004/03/18 00:29:04 nscollins Exp $

Earth System Modeling Framework
Copyright 2002-2003, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the GPL.
#endif

#if 0
prevent use of esmf derived type initializers.  causes some compilers problems.
#endif

#define ESMF_NO_INITIALIZERS 1
