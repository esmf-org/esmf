// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.


#ifndef ESMCI_TRACEMACROS_INC
#define ESMCI_TRACEMACROS_INC

#include "ESMCI_TraceRegion.h"
#include "ESMCI_VM.h"

#ifdef ESMF_PROFILE_INTERNAL
#define ESMCI_METHOD_ENTER(localrc) ESMCI::TraceEventRegionEnter(ESMC_METHOD, &(localrc)); \
  ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
#define ESMCI_METHOD_EXIT(localrc)  ESMCI::TraceEventRegionExit(ESMC_METHOD, &(localrc)); \
  ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
#define ESMCI_REGION_ENTER(name, localrc)      ESMCI::TraceEventRegionEnter(name, &(localrc)); \
  ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
#define ESMCI_REGION_EXIT(name, localrc)       ESMCI::TraceEventRegionExit(name, &(localrc)); \
  ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
#else
#define ESMCI_METHOD_ENTER(localrc)
#define ESMCI_METHOD_EXIT(localrc)
#define ESMCI_REGION_ENTER(name, localrc)
#define ESMCI_REGION_EXIT(name, localrc) 
#endif

#ifdef ESMF_PROFILE_MESHCREATE
#define ESMCI_MESHCREATE_TRACE_ENTER(name) {char before[100] = "before "; \
  VM::getCurrent(NULL)->logMemInfo(strcat(before, name)); \
  ESMCI::TraceEventRegionEnter(name, NULL);}
#define ESMCI_MESHCREATE_TRACE_EXIT(name) {char after[100] = "after "; \
  ESMCI::TraceEventRegionExit(name, NULL); \
  VM::getCurrent(NULL)->logMemInfo(strcat(after, name));}
#else
#define ESMCI_MESHCREATE_TRACE_ENTER(name)
#define ESMCI_MESHCREATE_TRACE_EXIT(name)
#endif

#ifdef ESMF_PROFILE_MESHREDIST
#define ESMCI_MESHREDIST_TRACE_ENTER(name) {char before[100] = "before "; \
  VM::getCurrent(NULL)->logMemInfo(strcat(before, name)); \
  ESMCI::TraceEventRegionEnter(name, NULL);}
#define ESMCI_MESHREDIST_TRACE_EXIT(name) {char after[100] = "after "; \
  ESMCI::TraceEventRegionExit(name, NULL); \
  VM::getCurrent(NULL)->logMemInfo(strcat(after, name));}
#else
#define ESMCI_MESHREDIST_TRACE_ENTER(name)
#define ESMCI_MESHREDIST_TRACE_EXIT(name)
#endif

#ifdef ESMF_PROFILE_REGRID
#define ESMCI_REGRID_TRACE_ENTER(name) {char before[100] = "before "; \
  VM::getCurrent(NULL)->logMemInfo(strcat(before, name)); \
  ESMCI::TraceEventRegionEnter(name, NULL);}
#define ESMCI_REGRID_TRACE_EXIT(name) {char after[100] = "after "; \
  ESMCI::TraceEventRegionExit(name, NULL); \
  VM::getCurrent(NULL)->logMemInfo(strcat(after, name));}
#else
#define ESMCI_REGRID_TRACE_ENTER(name)
#define ESMCI_REGRID_TRACE_EXIT(name)
#endif

#ifdef ESMF_PROFILE_DUALMESH
#define ESMCI_DUALMESH_TRACE_ENTER(name) {char before[100] = "before "; \
  VM::getCurrent(NULL)->logMemInfo(strcat(before, name)); \
  ESMCI::TraceEventRegionEnter(name, NULL);}
#define ESMCI_DUALMESH_TRACE_EXIT(name) {char after[100] = "after "; \
  ESMCI::TraceEventRegionExit(name, NULL); \
  VM::getCurrent(NULL)->logMemInfo(strcat(after, name));}
#else
#define ESMCI_DUALMESH_TRACE_ENTER(name)
#define ESMCI_DUALMESH_TRACE_EXIT(name)
#endif

#ifdef ESMF_PROFILE_RENDEZVOUS
#define ESMCI_RENDEZVOUS_TRACE_ENTER(name) {char before[200] = "before "; \
  VM::getCurrent(NULL)->logMemInfo(strcat(before, name)); \
  ESMCI::TraceEventRegionEnter(name, NULL);}
#define ESMCI_RENDEZVOUS_TRACE_EXIT(name) {char after[200] = "after "; \
  ESMCI::TraceEventRegionExit(name, NULL); \
  VM::getCurrent(NULL)->logMemInfo(strcat(after, name));}
#else
#define ESMCI_RENDEZVOUS_TRACE_ENTER(name)
#define ESMCI_RENDEZVOUS_TRACE_EXIT(name)
#endif

#endif
