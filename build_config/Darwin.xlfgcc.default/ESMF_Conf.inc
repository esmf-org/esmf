#if 0
$Id: ESMF_Conf.inc,v 1.2 2006/09/22 23:55:38 theurich Exp $

Earth System Modeling Framework
Copyright 2002-2003, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the GPL.
#endif

#if 0
Apparently some versions the AIX xlf 8.x compiler are unable to
handle initializers for derived type contents.  We have set the most
conservative setting possible.  If you want to try to set some of
the initializers but exclude the few which were problematic before,
remove the NO_INITIALIZERS line below and uncomment out the AIX_8_INITBUG
line - which only disables a few of the initializers.  If you want to
try the entire build with initializers, remove both defines and try
to rebuild the framework.   In both cases, the symptom something is
wrong is an incomprehensible "internal compiler" error message during 
the compile of the framework.
#endif

#if 0
-#-define ESMF_AIX_8_INITBUG 1
#endif

#define ESMF_NO_INITIALIZERS 1

