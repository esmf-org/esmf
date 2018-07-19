// $Id$
/*
 * Earth System Modeling Framework
 * Copyright 2002-2018, University Corporation for Atmospheric Research, 
 * Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
 * Laboratory, University of Michigan, National Centers for Environmental 
 * Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
 * NASA Goddard Space Flight Center.
 * Licensed under the University of Illinois-NCSA License.
 */

#include <stdio.h>
#include "ESMCI_Trace.h"

/* 
 *  Darwin platforms use dlopen()/dlsym() to load these
 *  wrappers, so we do not need to define them here.
 *  The reason for this is that the Darwin dynamic
 *  linker will not replace these with the symbols
 *  defined in the preload library, so we need to
 *  explicitly use dlsym() to find those symbols.
 */
#ifndef ESMF_OS_Darwin

extern "C" {

  /* will be overridden if preloader present */
  void c_esmftrace_notify_wrappers(int initialized) {
    printf("IGNORING call to c_esmftrace_notify_wrappers: %d\n", initialized);
    //nothing to do here -- linker will replace this function with a different one
  }
  
  /* will be overridden if preloader present */
  int c_esmftrace_isinitialized() {
    if (ESMCI::TraceInitialized()) return 1;
    else return 0;
  }
  
}

#endif
