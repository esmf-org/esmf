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


extern "C" {

  static int traceActive = 0;
  
  extern ssize_t __wrap_write(int fd, const void *buf, size_t nbytes);
  
  static ssize_t (*__real_ptr_write)(int fildes, const void *buf, size_t nbyte) = NULL; 

  extern void __set_esmftrace_active(int);
  
  void c_esmftrace_setactive(int active) {
    if (active == 1) {
      printf("ESMF Tracing enabled with dynamic instrumentation\n"); 
      traceActive = 1;
    }
    else {
      traceActive = 0;
    }
  }

  int c_esmftrace_isactive() {
    return traceActive;
  }
  
  ssize_t __real_write(int fd, const void *buf, size_t nbytes) {   
    if (__real_ptr_write == NULL) {
      __real_ptr_write = (ssize_t (*)(int, const void *, size_t)) dlsym(RTLD_NEXT, "write");
    }
    return __real_ptr_write(fd, buf, nbytes);
  }
  
  ssize_t write(int fd, const void *buf, size_t nbytes) {
    return __wrap_write(fd, buf, nbytes);
  }

  
}
