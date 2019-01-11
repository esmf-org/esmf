#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "ESMCI_Macros.h"
#include "ESMCI_Util.h"
#include "ESMCI_Trace.h"
#include "ESMCI_Comp.h"
#include "ESMCI_LogErr.h"

using std::string;

// C interface called from Fortran

static const char *const version = "$Id$";

extern "C" {
  
#undef ESMC_METHOD
#define ESMC_METHOD "c_esmftrace_open()"
  void FTN_X(c_esmftrace_open)
     (                          
      const char *trace_dir,
      int *profileToLog,
      int *rc,
      ESMCI_FortranStrLenArg nlen)  //strlen for trace_dir
  {
    string dirname = string(trace_dir, ESMC_F90lentrim(trace_dir, nlen));
    ESMCI::TraceOpen(dirname, profileToLog, rc);
  }

#undef ESMC_METHOD
#define ESMC_METHOD "c_esmftrace_close()"
  void FTN_X(c_esmftrace_close)(int *rc)
  {
    ESMCI::TraceClose(rc);
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmftrace_mapvmpid()"
  void FTN_X(c_esmftrace_mapvmid)(ESMCI::VMId **vmid, int *mappedId, int *rc)
  {
    if (mappedId!=NULL) {
      *mappedId = ESMCI::TraceMapVmId(*vmid, rc);
    }
    else {
      ESMC_LogDefault.MsgFoundError(
        ESMF_RC_ARG_BAD, "Null pointer for mappedId", ESMC_CONTEXT, rc);
    }
  }

#undef ESMC_METHOD
#define ESMC_METHOD "c_esmftrace_phase_enter()"
  void FTN_X(c_esmftrace_phase_enter)(int *vmid, int *baseid, int *method, int *phase, int *rc)
  {
    int localrc;
    ESMCI::TraceEventPhaseEnter(vmid, baseid, method, phase, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc))
      return;
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

#undef ESMC_METHOD
#define ESMC_METHOD "c_esmftrace_phase_exit()"
  void FTN_X(c_esmftrace_phase_exit)(int *vmid, int *baseid, int *method, int *phase, int *rc)
  {
    int localrc;
    ESMCI::TraceEventPhaseExit(vmid, baseid, method, phase, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc))
      return;
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

#undef ESMC_METHOD
#define ESMC_METHOD "c_esmftrace_phase_prologue_enter()"
  void FTN_X(c_esmftrace_phase_prologue_enter)(int *vmid, int *baseid, int *method, int *phase, int *rc)
  {
    ESMCI::TraceEventPhasePrologueEnter(vmid, baseid, method, phase);
    if (rc != NULL) *rc = ESMF_SUCCESS;
  }

  //void FTN_X(c_esmftrace_phase_prologue_exit)(int *vmid, int *baseid, int *method, int *phase, int *rc)
  //{
  //  ESMCI::TraceEventPhasePrologueExit(vmid, baseid, method, phase);
  //  if (rc != NULL) *rc = ESMF_SUCCESS;
  //}

  //void FTN_X(c_esmftrace_phase_epilogue_enter)(int *vmid, int *baseid, int *method, int *phase, int *rc)
  //{
  //  ESMCI::TraceEventPhaseEpilogueEnter(vmid, baseid, method, phase);
  //  if (rc != NULL) *rc = ESMF_SUCCESS;
  //}

#undef ESMC_METHOD
#define ESMC_METHOD "c_esmftrace_epliogue_enter()"
  void FTN_X(c_esmftrace_phase_epilogue_exit)(int *vmid, int *baseid, int *method, int *phase, int *rc)
  {
    ESMCI::TraceEventPhaseEpilogueExit(vmid, baseid, method, phase);
    if (rc != NULL) *rc = ESMF_SUCCESS;
  }

#undef ESMC_METHOD
#define ESMC_METHOD "c_esmftrace_component_info()"
  void FTN_X(c_esmftrace_component_info)(int *vmid, int *baseid, const char *name,
                                         const char *attributeKeys, const char *attributeVals, int *rc,
                                         ESMCI_FortranStrLenArg nlen,  //name
                                         ESMCI_FortranStrLenArg aklen,  //attributeKeys
                                         ESMCI_FortranStrLenArg avlen)  //attributeValues
  {
    string cname = string(name, ESMC_F90lentrim (name, nlen));
    string aKeys = string(attributeKeys, ESMC_F90lentrim (attributeKeys, aklen));
    string aVals = string(attributeVals, ESMC_F90lentrim (attributeVals, avlen));

    ESMCI::TraceEventComponentInfo(vmid, baseid, cname.c_str(), aKeys, aVals);
    if (rc != NULL) *rc = ESMF_SUCCESS;

  }

#undef ESMC_METHOD
#define ESMC_METHOD "c_esmftrace_region_enter()"
  void FTN_X(c_esmftrace_region_enter)(const char *name, int *rc, ESMCI_FortranStrLenArg nlen) {
    //TODO: optimize trim by not creating string object
    int localrc;
    string cname = string(name, ESMC_F90lentrim(name, nlen));
    ESMCI::TraceEventRegionEnter(cname, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc))
      return;
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

#undef ESMC_METHOD
#define ESMC_METHOD "c_esmftrace_region_exit()"
  void FTN_X(c_esmftrace_region_exit)(const char *name, int *rc, ESMCI_FortranStrLenArg nlen) {
    int localrc;
    string cname = string(name, ESMC_F90lentrim(name, nlen));
    ESMCI::TraceEventRegionExit(cname, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, rc))
      return;
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

#undef ESMC_METHOD
#define ESMC_METHOD "c_esmftrace_mem_info()"
  void FTN_X(c_esmftrace_mem_info)(int *rc) {
    ESMCI::TraceEventMemInfo();
    if (rc != NULL) *rc = ESMF_SUCCESS;
  }

#undef ESMC_METHOD
#define ESMC_METHOD "c_esmftrace_clock()"  
  void FTN_X(c_esmftrace_clock)(int *ep_year, int *ep_month, int *ep_day,
                                int *ep_hour, int *ep_minute, int *ep_second, int *rc) {
    ESMCI::TraceEventClock(ep_year, ep_month, ep_day, ep_hour, ep_minute, ep_second);
    if (rc != NULL) *rc = ESMF_SUCCESS;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmftracetest_getmpiwaitstats()"  
  /* These functions exposed only for use in unit tests. */
  void FTN_X(c_esmftracetest_getmpiwaitstats)(int *count, long long *time) {
    ESMCI::TraceTest_GetMPIWaitStats(count, time);
  }
  
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmftracetest_checkmpiregion()"  
  /* These functions exposed only for use in unit tests. */
  void FTN_X(c_esmftracetest_checkmpiregion)(const char *name, int *exists, ESMCI_FortranStrLenArg nlen) {
    string cname = string(name, ESMC_F90lentrim(name, nlen));
    ESMCI::TraceTest_CheckMPIRegion(cname, exists);
  }
  
} // extern "C"
