/**
 *
 * preload.c
 *
 */

#include <stdio.h>

extern "C" {

  static int traceActive = 0;

  /**
   * Called into from ESMCI_Trace.C when the trace
   * is opened and can receive events.
   */
  void c_esmftrace_setactive(int active) {
    if (active == 1) {
      printf("ESMF Tracing enabled with dynamic instrumentation\n"); 
      traceActive = 1;
    }
    else {
      traceActive = 0;
    }
  }

  /**
   * Called by wrappers
   */
  int c_esmftrace_isactive() {
    return traceActive;
  }  
  
}
