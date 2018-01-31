/**
 *
 * preload_mpi.c
 *
 * Functions that will be preloaded with LD_PRELOAD, thereby
 * overriding system library functions so we can call into our
 * wrapper function.
 *
 * Since we are using dynamic linking, the __real_<SYMBOL>
 * functions are looked up at runtime using dlsym().
 */

#include <dlfcn.h>
#include <mpi.h>

#include "ESMCI_Macros.h"
#include "ESMCI_Trace.h"
#include "wrappers_mpi.h"

extern "C" {

  /* MPI_Barrier */
  static int (*__real_ptr_MPI_Barrier)(MPI_Comm comm) = NULL;
  
  int __real_MPI_Barrier(MPI_Comm comm) {   
    if (__real_ptr_MPI_Barrier == NULL) {
      __real_ptr_MPI_Barrier = (int (*)(MPI_Comm)) dlsym(RTLD_NEXT, "MPI_Barrier");
    }
    return __real_ptr_MPI_Barrier(comm);
  }
  
  int MPI_Barrier(MPI_Comm comm) {
    return __wrap_MPI_Barrier(comm);
  }


  /* MPI_Ibarrier */
  /*
  static int  (*__real_ptr_MPI_Ibarrier)(MPI_Comm comm, MPI_Request *request) = NULL;

  int __real_MPI_Ibarrier(MPI_Comm comm, MPI_Request *request) {   
    if (__real_ptr_MPI_Ibarrier == NULL) {
      __real_ptr_MPI_Ibarrier = (int (*)(MPI_Comm, MPI_Request *)) dlsym(RTLD_NEXT, "MPI_Ibarrier");
    }
    return __real_ptr_MPI_Ibarrier(comm, request);
  }
  
  int MPI_Ibarrier(MPI_Comm comm, MPI_Request *request) {
    return __wrap_MPI_Ibarrier(comm, request);
  }
  */
  

  /* MPI_Wait */
  static int (*__real_ptr_MPI_Wait)(MPI_Request *request, MPI_Status *status) = NULL;

  int __real_MPI_Wait(MPI_Request *request, MPI_Status *status) {
    if (__real_ptr_MPI_Wait == NULL) {
      __real_ptr_MPI_Wait = (int (*)(MPI_Request *, MPI_Status *)) dlsym(RTLD_NEXT, "MPI_Wait");
    }
    return __real_ptr_MPI_Wait(request, status);
  }

  int MPI_Wait(MPI_Request *request, MPI_Status *status) {
    return __wrap_MPI_Wait(request, status);
  }
    
}
