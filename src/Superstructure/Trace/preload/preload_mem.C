/**
 *
 * preload_mem.C
 *
 * Functions that will be preloaded with LD_PRELOAD, thereby
 * overriding system library functions so we can call into our
 * wrapper function.
 *
 * Since we are using dynamic linking, the __real_<SYMBOL>
 * functions are looked up at runtime using dlsym().
 */

#ifdef ESMF_PGIVERSION_MAJOR
/* required for RTLD_NEXT */
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#endif

#include <dlfcn.h>
#include <stdlib.h>

#include "ESMCI_Macros.h"
#include "ESMCI_Trace.h"
#include "wrappers_mem.h"

extern "C" {

  /* malloc */
  static void * (*__real_ptr_malloc)(size_t size) = NULL;  
  void *__real_malloc(size_t size) {   
    if (__real_ptr_malloc == NULL) {
      __real_ptr_malloc = (void * (*)(size_t)) dlsym(RTLD_NEXT, "malloc");
    }
    return __real_ptr_malloc(size);
  }
  
  void *malloc(size_t size) {
    return __wrap_malloc(size);
  }
  
}
