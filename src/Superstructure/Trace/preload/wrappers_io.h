/**
 *  wrappers_io.h
 *
 *  Wrappers for low level IO calls so we can trace them.
 *  These are linked statically into the executable using
 *  --wrap=SYMBOL flags to the linker.
 *
 */

#ifndef WRAPPERS_IO_H
#define WRAPPERS_IO_H

#include <sys/types.h>
#include <sys/stat.h>

extern "C" {  
  ssize_t __wrap_write(int fd, const void *buf, size_t nbytes);
  ssize_t __wrap_read(int fd, void *buf, size_t nbyte);
  int __wrap_open(const char *path, int oflag, ...);
}

#endif
