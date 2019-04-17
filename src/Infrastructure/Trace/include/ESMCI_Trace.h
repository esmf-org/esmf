// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research, 
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
#include "ESMCI_TraceRegion.h"
#include <esmftrc.h>

#define TO_VOID_PTR(_value)           static_cast<void *>(_value)
#define FROM_VOID_PTR(_type, _value)  static_cast<_type *>(_value)

#define ESMF_CLOCK_REALTIME       1
#define ESMF_CLOCK_MONOTONIC      2
#define ESMF_CLOCK_MONOTONIC_SYNC 3

#define TRACE_WRAP_NONE    0  /* no wrappers */
#define TRACE_WRAP_DYNAMIC 1  /* dynamic linker does wrapping */
#define TRACE_WRAP_STATIC  2  /* wrappers statically compiled in */

#define TRACE_REGIONTYPE_PHASE 0
#define TRACE_REGIONTYPE_USER  1

#define NODENAME_LEN 100      /* string length of compute node hostname */

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

  struct esmftrc_platform_filesys_ctx {
    struct esmftrc_default_ctx ctx;
    FILE *fh;
    int stream_id;
    char nodename[NODENAME_LEN];
    uint64_t latch_ts;  /* latched timestamp */
  };

  void TraceInitializeClock(int *rc);
  uint64_t TraceGetClock(void *data);
  void TraceClockLatch(struct esmftrc_platform_filesys_ctx *ctx);
  void TraceClockUnlatch(struct esmftrc_platform_filesys_ctx *ctx);
  void TraceOpen(std::string trace_dir, int *profileToLog, int *rc);
  void TraceClose(int *rc);
  bool TraceInitialized();
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
  void TraceTest_CheckMPIRegion(std::string name, int *exists);
  //////////////////////////////
    
  
  ////////////////////////////////

  void TraceEventPhaseEnter(int *ep_vmid, int *ep_baseid, 
			    int *ep_method, int *ep_phase, int *rc);
  void TraceEventPhaseExit(int *ep_vmid, int *ep_baseid, 
			   int *ep_method, int *ep_phase, int *rc);
  void TraceEventPhasePrologueEnter(int *ep_vmid, int *ep_baseid, 
                                    int *ep_method, int *ep_phase);
  void TraceEventPhaseEpilogueExit(int *ep_vmid, int *ep_baseid, 
                                    int *ep_method, int *ep_phase);
  void TraceEventComponentInfo(int *ep_vmid, int *ep_baseid,
                               const char *ep_name,
                               std::vector<std::string> IPM, std::vector<std::string> IIPM,
                               std::vector<std::string> RPM, std::vector<std::string> FPM);
  void TraceEventMemInfo();
  void TraceEventClock(int *ep_year, int *ep_month, int *ep_day,
                       int *ep_hour, int *ep_minute, int *ep_second);

}

#endif
