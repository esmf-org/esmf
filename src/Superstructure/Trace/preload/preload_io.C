/**
 *
 * preload_io.c
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
#define _GNU_SOURCE
#endif

#include <sys/types.h>
#include <sys/stat.h>
#ifndef ESMF_OS_MinGW
#include <sys/uio.h>
#endif
#include <unistd.h>
#include <dlfcn.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <stdarg.h>

#include "ESMCI_Macros.h"
#include "ESMCI_Trace.h"
#include "wrappers_io.h"

extern "C" {

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

  /* writev */
  static ssize_t (*__real_ptr_writev)(int fd, const struct iovec *iov, int iovcnt) = NULL;
  ssize_t __real_writev(int fd, const struct iovec *iov, int iovcnt) {
    if (__real_ptr_writev == NULL) {
      __real_ptr_writev = (ssize_t (*)(int, const struct iovec *, int)) dlsym(RTLD_NEXT, "writev");
    }
    return __real_ptr_writev(fd, iov, iovcnt);
  }

  ssize_t writev(int fd, const struct iovec *iov, int iovcnt) {
    return __wrap_writev(fd, iov, iovcnt);
  }

  /* pwrite */
  static ssize_t (*__real_ptr_pwrite)(int fd, const void *buf, size_t nbytes, off_t offset) = NULL;
  ssize_t __real_pwrite(int fd, const void *buf, size_t nbytes, off_t offset) {
    if (__real_ptr_pwrite == NULL) {
      __real_ptr_pwrite = (ssize_t (*)(int, const void *, size_t, off_t)) dlsym(RTLD_NEXT, "pwrite");
    }
    return __real_ptr_pwrite(fd, buf, nbytes, offset);
  }

  ssize_t pwrite(int fd, const void *buf, size_t nbytes, off_t offset) {
    return __wrap_pwrite(fd, buf, nbytes, offset);
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


  /* open */
  static int (*__real_ptr_open)(const char *path, int oflag, ...) = NULL;
  int __real_open(const char *path, int oflag, ...) {
    if (__real_ptr_open == NULL) {
      __real_ptr_open = (int (*) (const char *, int, ...)) dlsym(RTLD_NEXT, "open");
    }
    
    va_list args;
    va_start(args, oflag);
    mode_t mode = va_arg(args, int);
    va_end(args);

    return __real_ptr_open(path, oflag, mode);
  }
  
  int open(const char *path, int oflag, ...) {
    va_list args;
    va_start(args, oflag);
    mode_t mode = va_arg(args, int);
    va_end(args);

    return __wrap_open(path, oflag, mode);
  }
  
}
