#if 0
$Id: ESMF_Conf.inc,v 1.3 2004/03/16 18:00:52 nscollins Exp $

Earth System Modeling Framework
Copyright 2002-2003, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the GPL.
#endif

#if 0
Initializers compile & link, but cause several TimeMgr examples and tests
to crash when trying to "Get" a time or timeinterval or their properties.
#endif

#define ESMF_NO_INITIALIZERS 1

