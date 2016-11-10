/*
 * Fortran callable functions into C interfaces
 * for writing tracing events.
 */

#include <stdint.h>
#include <string.h>
#include <assert.h>

#include "ESMCI_Macros.h"

extern "C" {

  void FTN_X(esmftrc_default_trace_phase_enter) 
       (int *ep_method, int *ep_phase);

}


