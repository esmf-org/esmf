/**
 *  wrappers_io.c
 *
 *  Wrappers for low level IO calls so we can trace them.
 *  These are linked statically into the executable using
 *  --wrap=SYMBOL flags to the linker.
 *
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>

#include "ESMCI_Macros.h"
#include "ESMCI_Trace.h"

extern "C" {

  extern int c_esmftrace_isactive();
  
  extern ssize_t __real_write(int fd, const void *buf, size_t nbytes);

  static int insideRegion = 0;  /* prevents recursion */

  ssize_t __wrap_write(int fd, const void *buf, size_t nbytes) {
    //printf("__wrap_write: %lu\n", nbytes);
    int localrc;

    //TODO:  this should call c_esmftrace_isactive
    // - in the case of dynamic preload, it is defined
    //     in preload.c and is set by a callback from TraceOpen
    // - in the case of static, it calls directly
    //     into the trace code
    if (c_esmftrace_isactive() == 1 && insideRegion == 0) {
      insideRegion = 1;
      ESMCI::TraceIOWriteStart(nbytes);
    }    
    
    ssize_t ret = __real_write(fd, buf, nbytes);
    
    if (c_esmftrace_isactive() == 1 && insideRegion == 1) {
      ESMCI::TraceIOWriteEnd();
      insideRegion = 0;
    }
    
    return ret;
  }
  
  /*
  ssize_t read(int fildes, void *buf, size_t nbyte) {

    if (traceReady == 1) {
      ESMCI::TraceIOReadStart(nbyte);
    }
    
    if (real_read == NULL) {
      real_read = (ssize_t (*)(int, void *, size_t)) dlsym(RTLD_NEXT, "read");
    }
    ssize_t ret = real_read(fildes, buf, nbyte);

    if (traceReady == 1) {
      ESMCI::TraceIOReadEnd();
    }
    
    return ret;
    
  }
  */
  
}
