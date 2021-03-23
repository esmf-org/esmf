#if 0
$Id$

Earth System Modeling Framework
Copyright 2002-2021, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the University of Illinois-NCSA License.

#endif

#ifndef ESMF_TIMING_INC
#define ESMF_TIMING_INC

#ifdef ESMF_PROFILE_INTERNAL
#define ESMF_METHOD_ENTER(localrc) call ESMF_TraceRegionEnter(ESMF_METHOD, rc=localrc)
#define ESMF_METHOD_EXIT(localrc) call ESMF_TraceRegionExit(ESMF_METHOD, rc=localrc)
#define ESMF_REGION_ENTER(name, localrc) call ESMF_TraceRegionEnter(name, rc=localrc)
#define ESMF_REGION_EXIT(name, localrc) call ESMF_TraceRegionExit(name, rc=localrc)
#else
#define ESMF_METHOD_ENTER(rc)
#define ESMF_METHOD_EXIT(rc)
#define ESMF_REGION_ENTER(name, localrc)
#define ESMF_REGION_EXIT(name, localrc)
#endif

#endif 

