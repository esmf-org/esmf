/* #define _GNU_SOURCE */

#include <sys/types.h>
#include <sys/stat.h>
#include <dlfcn.h>
#include <stdio.h>
#include <string.h>

#include "ESMCI_Macros.h"

static ssize_t (*real_write)(int fd, const void *buf, size_t count) = NULL;
//static int (*real_puts)(const char* str) = NULL;

extern void FTN_X(c_esmftrace_region_enter)(const char *name, int *rc, ESMCI_FortranStrLenArg namelen);
extern void FTN_X(c_esmftrace_region_exit)(const char *name, int *rc, ESMCI_FortranStrLenArg namelen);

static int insideRegion = 0;
static int traceReady = 0;
static size_t totalWrite = 0;
static size_t totalPuts = 0;

ssize_t write(int fd, const void *buf, size_t count) {

  int localrc;
  totalWrite += count;
  int regularFile = 0;
  
  struct stat sb;
  if (fstat(fd, &sb) == -1) {
    printf("fstat error\n");
  }
  else {
    if (S_ISREG(sb.st_mode)) {
      regularFile = 1;
    }
  }
    
  if (regularFile == 1 && traceReady == 1 && insideRegion == 0) {
    insideRegion = 1;
    FTN_X(c_esmftrace_region_enter)("regfile", &localrc, 7);
  }

  real_write = (ssize_t (*)(int, const void *, size_t)) dlsym(RTLD_NEXT, "write");
  ssize_t ret = real_write(fd, buf, count);

  if (regularFile == 1 && traceReady == 1 && insideRegion == 1) {
    FTN_X(c_esmftrace_region_exit)("regfile", &localrc, 7);
    insideRegion = 0;
  }
  
  return ret;
}

/*
int puts(const char* str) {

  totalPuts += strlen(str);
  
  //printf("puts total: %lu\n", totalPuts);
  //fflush(stdout);
  
  real_puts(str);
}
*/


int c_esmf_settraceready(int ready) {
  printf("PRELOAD: Setting trace ready: %d\n", ready);
  if (ready != 0) {
    traceReady = 1;
  }
  else {
    traceReady = 0;
  }
}
