/**
 *  wrappers_mem.h
 *
 *  Wrappers for memory related calls so we can trace them.
 *  These are linked statically into the executable using
 *  --wrap=SYMBOL flags to the linker.
 *
 */

#ifndef WRAPPERS_MEM_H
#define WRAPPERS_MEM_H

extern "C" {  
  void *__wrap_malloc(size_t size);
}

#endif
