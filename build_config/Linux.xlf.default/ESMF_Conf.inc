#if 0
$Id: ESMF_Conf.inc,v 1.1.2.1 2005/02/08 23:58:40 nscollins Exp $

Earth System Modeling Framework
Copyright 2002-2003, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the GPL.
#endif

#if 0
Apparently some versions of the AIX xlf 8.x compiler are unable to
handle initializers for derived type contents.  If this is a problem
on your system, first try only enabling the INITBUG definition below.  
(This will selectively disable a few key derived types with initializers.)   
If still unsuccessful, enable the #define of ESMF_NO_INITIALIZERS which 
will effectively remove all attempts to give derived type members initial 
values.  In both cases, the symptom something is wrong is an 
incomprehensible "internal compiler" error message during the compile 
of the framework.
#endif

#if 0
-#-define ESMF_AIX_8_INITBUG 1
-#-define ESMF_NO_INITIALIZERS 1
#endif

