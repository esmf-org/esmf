#if 0
$Id: ESMF_Conf.inc,v 1.2 2004/06/29 14:46:36 nscollins Exp $

Earth System Modeling Framework
Copyright 2002-2003, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the GPL.
#endif

#if 0
Apparently the AIX xlf 8.x compiler stopped being able to handle
initializers for derived type contents.  It accepts them
in some places, so I have not defined the normal symbol
which is ESMF_NO_INITIALIZERS that disables all initialization.
This symbol selectively disables initialization in the
places where it causes a compiler internal error.
#endif

#define ESMF_NO_INITIALIZERS 1
#define ESMF_AIX_8_INITBUG 1

