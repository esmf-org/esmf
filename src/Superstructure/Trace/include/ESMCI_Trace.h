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

//how often to flush streams to file
#define BT_FLUSH_STREAM_INTERVAL 100

//constants used in tracing events
#define BT_ESMF_TRACE_VERSION "0.1"

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
  void TraceOpen(const char *trace_dir, int *rc);
  void TraceClose(int *rc);
  void TraceSetupTypes(int *rc);  
  void TraceCheckPetList(int *traceLocalPet, int *rc);
  std::string TraceGetMetadataString();
  
  ////////////////////////////////

  void TraceEventPhase(int ctrl, int *ep_vmid,
		       int *ep_baseid, int *ep_method, int *ep_phase);
  void TraceEventPhaseEnter(int *ep_vmid, int *ep_baseid, 
			    int *ep_method, int *ep_phase);
  void TraceEventPhaseExit(int *ep_vmid, int *ep_baseid, 
			   int *ep_method, int *ep_phase);
  void TraceEventPhasePrologueEnter(int *ep_vmid, int *ep_baseid, 
			    int *ep_method, int *ep_phase);
  void TraceEventPhasePrologueExit(int *ep_vmid, int *ep_baseid, 
                                   int *ep_method, int *ep_phase);
  void TraceEventPhaseEpilogueEnter(int *ep_vmid, int *ep_baseid, 
                                    int *ep_method, int *ep_phase);
  void TraceEventPhaseEpilogueExit(int *ep_vmid, int *ep_baseid, 
                                   int *ep_method, int *ep_phase);

  void TraceEventComponentInfo(Comp *comp, int *ep_vmid, int *ep_baseid,
                               const char *ep_name, std::string attributeKeys, std::string attributeVals);

  void TraceEventRegion(int ctrl, const char*name);
}

#endif
