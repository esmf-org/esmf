/**
 *
 * preload.c
 *
 */

#include <stdio.h>

extern "C" {

  static int traceInitialized = 0;

  /**
   * Called into from ESMCI_Trace.C when the trace
   * is opened and can receive events.  The linker will preload
   * this symbol when LD_PRELOAD is specified so we can
   * catch this notification.
   */
  void c_esmftrace_notify_wrappers(int initialized) {
    if (initialized == 1) {
      printf("ESMF Tracing enabled with dynamic instrumentation\n"); 
      traceInitialized = 1;
    }
    else {
      traceInitialized = 0;
    }
  }

  /**
   * Called by wrappers
   */
  int c_esmftrace_isinitialized() {
    return traceInitialized;
  }  
  
}
