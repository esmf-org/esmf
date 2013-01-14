#if 0
$Id: ESMF_Conf.inc,v 1.14 2012/01/06 20:15:00 svasquez Exp $

Earth System Modeling Framework
Copyright 2002-2013, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the University of Illinois-NCSA License.
#endif

#if 0
Define the proper way to read the command line.  Use getarg for now.
#endif

#define ESMF_NEEDSGETARG

#if 0
prevent use of esmf derived type initializers.  causes some compilers problems.
#endif

#define ESMF_NO_INITIALIZERS 1
