#if 0
$Id: ESMF_Conf.inc,v 1.8.2.2 2009/01/21 21:25:17 cdeluca Exp $

Earth System Modeling Framework
Copyright 2002-2009, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the University of Illinois-NCSA License.
#endif

#if 0
Apparently some versions the AIX xlf 8.x compiler are unable to 
handle initializers for derived type contents.  If this is a problem
on your system, remove the #if 0/#endif lines and fix the #define line
before you compile the framework.
#endif

#if 0
-#-define ESMF_NO_INITIALIZERS 1
#endif

