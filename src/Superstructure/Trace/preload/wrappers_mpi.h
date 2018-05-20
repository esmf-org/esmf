/**
 *  wrappers_mpi.h
 *
 *  Wrappers for MPI calls so we can trace them.
 *  These are linked statically into the executable using
 *  --wrap=SYMBOL flags to the linker.
 *
 */

#ifndef _WRAPPERS_MPI_H
#define _WRAPPERS_MPI_H

#include <mpi.h>

extern "C" {
  int __wrap_MPI_Barrier(MPI_Comm comm);
  /*int __wrap_MPI_Ibarrier(MPI_Comm comm, MPI_Request *request);*/
  int __wrap_MPI_Wait(MPI_Request *request, MPI_Status *status);

  int __wrap_MPI_Allreduce(const void *sendbuf, void *recvbuf, int count,
                            MPI_Datatype datatype, MPI_Op op, MPI_Comm comm);
  void FTN_X(__wrap_mpi_allreduce)(MPI_Fint *sendbuf, MPI_Fint *recvbuf, MPI_Fint *count,
                                   MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *ierr);
 
}

#endif
