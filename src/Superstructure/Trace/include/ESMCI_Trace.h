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


//how often to flush streams to file
#define BT_FLUSH_STREAM_INTERVAL 100

//constants used in tracing events
#define BT_ESMF_TRACE_VERSION "0.2"

#define BT_CNTL_START 0
#define BT_CNTL_END 1
#define BT_CNTL_STARTP 2
#define BT_CNTL_ENDP 3
#define BT_CNTL_STARTE 4
#define BT_CNTL_ENDE 5

#define BT_METHOD_INIT 0
#define BT_METHOD_RUN 1
#define BT_METHOD_FINAL 2

#define BT_REGION_ENTER 0
#define BT_REGION_EXIT 1

namespace ESMCI {
  void TraceOpen(std::string trace_dir, int *rc);
  void TraceClose(int *rc);
  void TraceSetupTypes(int *rc);
  bool TraceIsEnabledForPET(int *rc);
  std::string TraceGetMetadataString();
  int TraceMapVmId(VMId *vmid, int *rc);

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
