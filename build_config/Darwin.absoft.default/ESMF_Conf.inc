#if 0
$Id: ESMF_Conf.inc,v 1.5.4.2 2006/09/20 22:02:59 theurich Exp $

Earth System Modeling Framework
Copyright 2002-2003, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the GPL.
#endif

#if 0
In the past we had problems with initializers using Absoft. We got errors like
the following at compile time:

tmod-2612 tmod: INTERNAL 
  Internal: IR xlate; SOURCE line: 48, file: src/Infrastructure/TimeMgr/interface/ESMF_Time.F90; INTERNAL site: 1437, tag: 0, val: -1
tmod-2612 tmod: INTERNAL 
  Internal: IR xlate; SOURCE line: 48, file: src/Infrastructure/TimeMgr/interface/ESMF_Time.F90; INTERNAL site: 18466, tag: 0, val: -1
error on line 353, source file line 467, invalid identifier after handle
Contact technical support at support@absoft.com

Now, however, things seem to hold up under Absoft so we decided to take the
following macro definition out again. If we see problems in the future we can
always put it back in.

#endif

#if 0
-#-define ESMF_NO_INITIALIZERS 1
#endif
