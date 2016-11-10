/*
 * Fortran callable functions into C interfaces
 * for writing tracing events.
 */

#include <stdint.h>
#include <string.h>
#include <assert.h>

#include "ESMCI_Macros.h"
#include "esmftrc_f.h"
#include "esmftrc_filesys.h"

void FTN_X(esmftrc_default_trace_phase_enter) 
     (int *ep_method, int *ep_phase) 
{
     esmftrc_default_trace_phase_enter(esmftrc_platform_get_default_ctx(),
            *ep_method, *ep_phase);

}


