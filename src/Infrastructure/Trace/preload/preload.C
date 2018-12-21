/**
 *
 * preload.c
 *
 */

#include <stdio.h>
#include "ESMCI_Trace.h"

extern "C" {

  static int traceInitialized = 0;

  /**
   * Called into from ESMCI_Trace.C when the trace
   * is opened and can receive events.  The linker will preload
   * this symbol when LD_PRELOAD is specified so we can
   * catch this notification.
   */
  int c_esmftrace_notify_wrappers(int initialized) {
    if (initialized == 1) {
      traceInitialized = 1;
    }
    else {
      traceInitialized = 0;
    }
    return TRACE_WRAP_DYNAMIC;
  }

  /**
   * Called by wrappers
   */
  int c_esmftrace_isinitialized() {
    return traceInitialized;
  }  
  
}
