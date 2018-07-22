// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// Trace include file for C++

#ifndef ESMCI_TRACE_H
#define ESMCI_TRACE_H

#include <string.h>
#include "ESMCI_Comp.h"

#define ESMF_CLOCK_REALTIME       1
#define ESMF_CLOCK_MONOTONIC      2
#define ESMF_CLOCK_MONOTONIC_SYNC 3

#define TRACE_WRAP_NONE    0  /* no wrappers */
#define TRACE_WRAP_DYNAMIC 1  /* dynamic linker does wrapping */
#define TRACE_WRAP_STATIC  2  /* wrappers statically compiled in */

#if (!defined ESMF_OS_Darwin || defined ESMF_NO_DLFCN)
extern "C" {
  int c_esmftrace_notify_wrappers(int initialized);
  int c_esmftrace_isinitialized();
}
#endif

#ifdef ESMF_OS_MinGW
#define DLL_EXPORT __declspec(dllexport)
#else
#define DLL_EXPORT 
#endif

namespace ESMCI { 
  void TraceOpen(std::string trace_dir, int *rc);
  void TraceClose(int *rc);
  bool TraceInitialized();
  bool TraceIsEnabledForPET(int *rc);
  std::string TraceGetMetadataString();
  int TraceMapVmId(VMId *vmid, int *rc);

  //////////////// IO Tracing //////////
  void DLL_EXPORT TraceIOOpenStart(const char *path);
  void DLL_EXPORT TraceIOOpenEnd();
  void DLL_EXPORT TraceIOCloseStart();
  void DLL_EXPORT TraceIOCloseEnd();
  
  void DLL_EXPORT TraceIOWriteStart();
  void DLL_EXPORT TraceIOWriteEnd(size_t nbytes);
  void DLL_EXPORT TraceIOReadStart();
  void DLL_EXPORT TraceIOReadEnd(size_t nbytes);
  /////////////////////////////////////
  
  ///////////// MPI ////////////
  void TraceMPIBarrierStart();
  void TraceMPIBarrierEnd();
  void TraceMPIWaitStart();
  void TraceMPIWaitEnd();

  //These used only for testing
  void TraceTest_GetMPIWaitStats(int *count, long long *time);
  //////////////////////////////
    
  
  ////////////////////////////////

  void TraceEventPhaseEnter(int *ep_vmid, int *ep_baseid, 
			    int *ep_method, int *ep_phase);
  void TraceEventPhaseExit(int *ep_vmid, int *ep_baseid, 
			   int *ep_method, int *ep_phase);
  void TraceEventPhasePrologueEnter(int *ep_vmid, int *ep_baseid, 
                                    int *ep_method, int *ep_phase);
  void TraceEventPhaseEpilogueExit(int *ep_vmid, int *ep_baseid, 
                                    int *ep_method, int *ep_phase);
  
  void TraceEventRegionEnter(std::string name);
  void TraceEventRegionExit(std::string name);
  
  void TraceEventComponentInfo(Comp *comp, int *ep_vmid, int *ep_baseid,
                               const char *ep_name,
                               std::string attributeKeys,
                               std::string attributeVals);
  void TraceEventMemInfo();
  void TraceEventClock(int *ep_year, int *ep_month, int *ep_day,
                       int *ep_hour, int *ep_minute, int *ep_second);
}

#endif
