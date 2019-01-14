// $Id$
/*
 * Earth System Modeling Framework
 * Copyright 2002-2019, University Corporation for Atmospheric Research, 
 * Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
 * Laboratory, University of Michigan, National Centers for Environmental 
 * Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
 * NASA Goddard Space Flight Center.
 * Licensed under the University of Illinois-NCSA License.
 */

#include <stdio.h>
#include "ESMCI_Trace.h"

extern "C" {

  /* will be overridden if preloader present */
  int c_esmftrace_notify_wrappers(int initialized) {
    //returning TRACE_WRAP_NONE, indicating that there are no wrappers present
    //this function may be replaced by the linker if wrappers are present
    return TRACE_WRAP_NONE;
  }
  
  /* will be overridden if preloader present */
  int c_esmftrace_isinitialized() {
    if (ESMCI::TraceInitialized()) return 1;
    else return 0;
  }
  
}

