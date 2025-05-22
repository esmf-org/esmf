// $Id$
//
// Earth System Modeling Framework
// Copyright (c) 2002-2025, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

#ifndef ESMCI_TRACEREGION_H
#define ESMCI_TRACEREGION_H

#include "ESMCI_Comp.h"

namespace ESMCI {

  void TraceEventRegionEnter(std::string name, int *rc);
  void TraceEventRegionExit(std::string name, int *rc);
  void TraceEventCompPhaseEnter(ESMCI::Comp *comp, enum ESMCI::method *method, int *phase, int *rc);
  void TraceEventCompPhaseExit(ESMCI::Comp *comp, enum ESMCI::method *method, int *phase, int *rc);

  // Trace by specific type info
  enum ESMC_ProfileType {ESMC_PROFILETYPE_REGRID=0, ESMC_PROFILETYPE_NUM};
  
  int TraceGetProfileTypeInfo(enum ESMC_ProfileType type);

}

#endif
