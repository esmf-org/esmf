#if 0
$Id: ESMF_Conf.inc,v 1.4 2004/06/18 15:36:05 nscollins Exp $

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

#if 0
#-define ESMF_NO_INITIALIZERS 1
#-define ESMF_AIX_8_INITBUG 1
#endif

