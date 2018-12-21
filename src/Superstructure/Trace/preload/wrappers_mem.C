/**
 *  wrappers_mem.C
 *
 *  Wrappers for memory related calls so we can trace them.
 *  These are linked statically into the executable using
 *  --wrap=SYMBOL flags to the linker.
 *
 */

#include <stdio.h>
#include <stdlib.h>

#include "ESMCI_Macros.h"
#include "ESMCI_Trace.h"
#include "preload.h"

extern "C" {

  static int ignoreMalloc = 0;
  static int ignorerc = 0;
  
  /* malloc */
  extern void *__real_malloc(size_t size);

  void *__wrap_malloc(size_t size) {
    if (c_esmftrace_isinitialized() == 1 && ignoreMalloc == 0) {
      ignoreMalloc = 1; //ignore mallocs inside the tracer
      ESMCI::TraceEventRegionEnter("malloc", &ignorerc);
      void *ret = __real_malloc(size);
      ESMCI::TraceEventRegionExit("malloc", &ignorerc);
      ignoreMalloc = 0;
      return ret;
    }    
    else {
      return __real_malloc(size);
    }
  }
    
}
