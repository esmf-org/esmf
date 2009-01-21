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
In the past we had problems with initializers using Absoft up to version 9.0. 
We get errors like the following at compile time:

tmod-2612 tmod: INTERNAL 
  Internal: IR xlate; SOURCE line: 48, file: src/Infrastructure/TimeMgr/interface/ESMF_Time.F90; INTERNAL site: 1437, tag: 0, val: -1
tmod-2612 tmod: INTERNAL 
  Internal: IR xlate; SOURCE line: 48, file: src/Infrastructure/TimeMgr/interface/ESMF_Time.F90; INTERNAL site: 18466, tag: 0, val: -1
error on line 353, source file line 467, invalid identifier after handle
Contact technical support at support@absoft.com

#endif


#define ESMF_NO_INITIALIZERS 1
