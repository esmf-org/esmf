/**
 *
 * preload.c
 *
 * Functions that will be preloaded with LD_PRELOAD, thereby
 * overriding system library functions so we can call into our
 * wrapper function.
 *
 * Since we are using dynamic linking, the __real_<SYMBOL>
 * functions are looked up at runtime using dlsym().
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <dlfcn.h>
#include <stdio.h>
#include <string.h>

#include "ESMCI_Macros.h"
#include "ESMCI_Trace.h"
#include "wrappers_io.h"

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


  
  /* write */
  static ssize_t (*__real_ptr_write)(int fildes, const void *buf, size_t nbyte) = NULL;
  
  ssize_t __real_write(int fd, const void *buf, size_t nbytes) {   
    if (__real_ptr_write == NULL) {
      __real_ptr_write = (ssize_t (*)(int, const void *, size_t)) dlsym(RTLD_NEXT, "write");
    }
    return __real_ptr_write(fd, buf, nbytes);
  }
  
  ssize_t write(int fd, const void *buf, size_t nbytes) {
    return __wrap_write(fd, buf, nbytes);
  }


  
  /* read */
  static ssize_t (*__real_ptr_read)(int fd, void *buf, size_t nbytes) = NULL;
  ssize_t __real_read(int fd, void *buf, size_t nbytes) {
    if (__real_ptr_read == NULL) {
      __real_ptr_read = (ssize_t (*)(int, void *, size_t)) dlsym(RTLD_NEXT, "read");
    }
    return __real_ptr_read(fd, buf, nbytes);
  }

  ssize_t read(int fd, void *buf, size_t nbytes) {
    return __wrap_read(fd, buf, nbytes);
  }
  
  
}
