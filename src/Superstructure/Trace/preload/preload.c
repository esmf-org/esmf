#include <sys/types.h>
#include <sys/stat.h>
#include <dlfcn.h>
#include <stdio.h>
#include <string.h>

#include "ESMCI_Macros.h"

namespace ESMCI {
  extern void TraceIOWriteStart(size_t nbytes);
  extern void TraceIOWriteEnd();
  extern void TraceIOReadStart(size_t nbytes);
  extern void TraceIOReadEnd();
}

extern "C" {

  static ssize_t (*real_write)(int fildes, const void *buf, size_t nbyte) = NULL;
  static ssize_t (*real_read)(int fildes, void *buf, size_t nbyte) = NULL;
   
  static int insideRegion = 0;  /* prevents recursion */
  static int traceReady = 0;

  ssize_t write(int fd, const void *buf, size_t nbytes) {
    
    int localrc;
    /*
    struct stat sb;
    if (fstat(fd, &sb) == -1) {
      printf("fstat error\n");
    }
    else {
      if (S_ISREG(sb.st_mode)) {
        regularFile = 1;
      }
    }
    */
    
    if (traceReady == 1 && insideRegion == 0) {
      insideRegion = 1;
      ESMCI::TraceIOWriteStart(nbytes);
    }

    if (real_write == NULL) {
      real_write = (ssize_t (*)(int, const void *, size_t)) dlsym(RTLD_NEXT, "write");
    }
    ssize_t ret = real_write(fd, buf, nbytes);
    
    if (traceReady == 1 && insideRegion == 1) {
      ESMCI::TraceIOWriteEnd();
      insideRegion = 0;
    }
    
    return ret;
  }

  
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

  
  int c_esmf_settraceready(int ready) {
    if (ready != 0) {
      printf("***Loading ESMF Tracing Instrumentation***\n");
      traceReady = 1;
    }
    else {
      traceReady = 0;
    }
  }

}
