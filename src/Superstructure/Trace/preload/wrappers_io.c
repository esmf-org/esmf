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
#ifndef ESMF_OS_MinGW
#include <sys/uio.h>
#endif
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#include "ESMCI_Macros.h"
#include "ESMCI_Trace.h"
#include "preload.h"

extern "C" {
  
  static int insideWrite = 0;  /* prevents recursion */

  /* write */
  extern ssize_t __real_write(int fd, const void *buf, size_t nbytes);

  ssize_t __wrap_write(int fd, const void *buf, size_t nbytes) {
    if (c_esmftrace_isinitialized() == 1 && insideWrite == 0) {
      insideWrite = 1;
      ESMCI::TraceIOWriteStart();
      ssize_t ret = __real_write(fd, buf, nbytes);
      ESMCI::TraceIOWriteEnd(ret > 0 ? ret : 0);
      insideWrite = 0;
      return ret;
    }    
    else {
      return __real_write(fd, buf, nbytes);
    }
  }
  

  /* writev */
  extern ssize_t __real_writev(int fd, const struct iovec *iov, int iovcnt);

  ssize_t __wrap_writev(int fd, const struct iovec *iov, int iovcnt) {    
    if (c_esmftrace_isinitialized() == 1 && insideWrite == 0) {
      insideWrite = 1;
      ESMCI::TraceIOWriteStart();
      ssize_t ret = __real_writev(fd, iov, iovcnt);
      ESMCI::TraceIOWriteEnd(ret > 0 ? ret : 0);
      insideWrite = 0;
      return ret;
    }    
    else {
      return __real_writev(fd, iov, iovcnt);
    }    
  }

  /* pwrite */
  extern ssize_t __real_pwrite(int fd, const void *buf, size_t nbyte, off_t offset);

  ssize_t __wrap_pwrite(int fd, const void *buf, size_t nbytes, off_t offset) {
    if (c_esmftrace_isinitialized() == 1 && insideWrite == 0) {
      insideWrite = 1;
      ESMCI::TraceIOWriteStart();
      ssize_t ret = __real_pwrite(fd, buf, nbytes, offset);
      ESMCI::TraceIOWriteEnd(ret > 0 ? ret : 0);
      insideWrite = 0;
      return ret;
    }    
    else {
      return __real_pwrite(fd, buf, nbytes, offset);
    }
  }

  
  
  /* read */
  extern ssize_t __real_read(int fd, void *buf, size_t nbytes);

  ssize_t __wrap_read(int fd, void *buf, size_t nbyte) {

    if (c_esmftrace_isinitialized() == 1) {
      ESMCI::TraceIOReadStart();
    }
    
    ssize_t ret = __real_read(fd, buf, nbyte);

    if (c_esmftrace_isinitialized() == 1) {
      ESMCI::TraceIOReadEnd(ret > 0 ? ret : 0);
    }
    
    return ret;
    
  }


  /* open */
  extern int __real_open(const char *path, int oflag, ...);
  
  int __wrap_open(const char *path, int oflag, ... ) {
    //    printf("__wrap_open: %s\n", path);

    va_list args;
    va_start(args, oflag);
    mode_t mode = va_arg(args, int);
    va_end(args);
    
    if (c_esmftrace_isinitialized() == 1) {
      ESMCI::TraceIOOpenStart(path);
    }

    int ret =  __real_open(path, oflag, mode);

    if (c_esmftrace_isinitialized() == 1) {
      ESMCI::TraceIOOpenEnd();
    }
    
    return ret;
  }

  
  
}
