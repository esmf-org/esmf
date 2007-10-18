#if 0
$Id: ESMF_Conf.inc,v 1.5.4.4 2007/10/18 02:41:42 cdeluca Exp $

Earth System Modeling Framework
Copyright 2002-2007, University Corporation for Atmospheric Research,
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

