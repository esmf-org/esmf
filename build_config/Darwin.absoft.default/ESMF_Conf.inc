#if 0
$Id: ESMF_Conf.inc,v 1.4 2004/08/04 20:51:23 nscollins Exp $

Earth System Modeling Framework
Copyright 2002-2003, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the GPL.
#endif

#if 0
Current behavior - if Initializers are used we get the following error at
compile time:

tmod-2612 tmod: INTERNAL 
  Internal: IR xlate; SOURCE line: 48, file: src/Infrastructure/TimeMgr/interface/ESMF_Time.F90; INTERNAL site: 1437, tag: 0, val: -1
tmod-2612 tmod: INTERNAL 
  Internal: IR xlate; SOURCE line: 48, file: src/Infrastructure/TimeMgr/interface/ESMF_Time.F90; INTERNAL site: 18466, tag: 0, val: -1
error on line 353, source file line 467, invalid identifier after handle
Contact technical support at support@absoft.com

#endif

#define ESMF_NO_INITIALIZERS 1

