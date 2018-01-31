/**
 * preload.h
 */

#ifndef _PRELOAD_H
#define _PRELOAD_H

extern "C" {

  /*
   * Called into from ESMCI_Trace.C when the trace
   * is opened and can receive events.
   */
  void c_esmftrace_setactive(int active);

  /**
   * Called by wrappers
   */
  int c_esmftrace_isactive();
  
}

#endif
