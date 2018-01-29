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
#include <fcntl.h>
#include <stdio.h>
#include <string.h>

#include "ESMCI_Macros.h"
#include "ESMCI_Trace.h"

extern "C" {

static int insideRegion = 0;  /* prevents recursion */

  /**
   * - in the case of dynamic linking (with LD_PRELOAD), this
   *    function is defined in preload.c
   * - in the case of static linking, this function is 
   *    defined in Trace/src/ESMCI_Trace.C
   */
  extern int c_esmftrace_isactive();

  /* write */
  extern ssize_t __real_write(int fd, const void *buf, size_t nbytes);

  ssize_t __wrap_write(int fd, const void *buf, size_t nbytes) {
    int localrc;

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
  

  /* read */
  extern ssize_t __real_read(int fd, void *buf, size_t nbytes);

  ssize_t __wrap_read(int fd, void *buf, size_t nbyte) {

    if (c_esmftrace_isactive() == 1) {
      ESMCI::TraceIOReadStart(nbyte);
    }
    
    ssize_t ret = __real_read(fd, buf, nbyte);

    if (c_esmftrace_isactive() == 1) {
      ESMCI::TraceIOReadEnd();
    }
    
    return ret;
    
  }


  /* open */
  extern int __real_open(const char *path, int oflag, ...);
  
  int __wrap_open(const char *path, int oflag, ... ) {
    printf("__wrap_open: %s\n", path);

    va_list args;
    va_start(args, oflag);
    mode_t mode = va_arg(args, int);
    va_end(args);
    
    if (c_esmftrace_isactive() == 1) {
      ESMCI::TraceIOOpenStart(path);
    }

    int ret =  __real_open(path, oflag, mode);

    if (c_esmftrace_isactive() == 1) {
      ESMCI::TraceIOOpenEnd();
    }
    
    return ret;
  }

  
  
}
