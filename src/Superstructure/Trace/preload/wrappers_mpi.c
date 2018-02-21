/**
 *  wrappers_mpi.c
 *
 *  Wrappers for MPI calls so we can trace them.
 *  These are linked statically into the executable using
 *  --wrap=SYMBOL flags to the linker.
 *
 */

#include <mpi.h>

#include "ESMCI_Macros.h"
#include "ESMCI_Trace.h"
#include "preload.h"

extern "C" {

  extern int __real_MPI_Barrier(MPI_Comm comm);
  
  int __wrap_MPI_Barrier(MPI_Comm comm) {

    //printf("__wrap_MPI_Barrier\n");
    
    if (c_esmftrace_isactive() == 1) {
      ESMCI::TraceMPIBarrierStart();
    }
    int ret = __real_MPI_Barrier(comm);
    if (c_esmftrace_isactive() == 1) {
      ESMCI::TraceMPIBarrierEnd();
    }

    return ret;
  }


  extern void FTN_X(__real_mpi_barrier)(MPI_Fint *comm, MPI_Fint *ierr);

  void FTN_X(__wrap_mpi_barrier)(MPI_Fint *comm, MPI_Fint *ierr) {
    //printf("__wrap_mpi_barrier_ (Fortran)\n");
    if (c_esmftrace_isactive() == 1) {
      ESMCI::TraceMPIBarrierStart();
    }
    FTN_X(__real_mpi_barrier)(comm, ierr);
    if (c_esmftrace_isactive() == 1) {
      ESMCI::TraceMPIBarrierEnd();
    }
  }


  /*
  extern int __real_MPI_Ibarrier(MPI_Comm comm, MPI_Request *request);

  int __wrap_MPI_Ibarrier(MPI_Comm comm, MPI_Request *request) {
      
    if (c_esmftrace_isactive() == 1) {
      //TODO: need a custom event for this
      ESMCI::TraceEventRegionEnter("mpi_ibarrier");
    }
    int ret = __real_MPI_Ibarrier(comm, request);
    if (c_esmftrace_isactive() == 1) {
      ESMCI::TraceEventRegionExit("mpi_ibarrier");
    }
    
    return ret;
  }
  */
  
  /* MPI_Wait */
  extern int __real_MPI_Wait(MPI_Request *request, MPI_Status *status);

  int __wrap_MPI_Wait(MPI_Request *request, MPI_Status *status) {
    if (c_esmftrace_isactive() == 1) {
      ESMCI::TraceMPIWaitStart();
    }
    int ret = __real_MPI_Wait(request, status);
    if (c_esmftrace_isactive() == 1) {
      ESMCI::TraceMPIWaitEnd();
    }
    return ret;
  }

  extern void FTN_X(__real_mpi_wait)(MPI_Fint *request, MPI_Fint *status, MPI_Fint *ierr);
  
  void FTN_X(__wrap_mpi_wait)(MPI_Fint *request, MPI_Fint *status, MPI_Fint *ierr) {
    //printf("__wrap_mpi_wait_ (Fortran)\n");
    if (c_esmftrace_isactive() == 1) {
      ESMCI::TraceMPIWaitStart();
    }
    FTN_X(__real_mpi_wait)(request, status, ierr);
    if (c_esmftrace_isactive() == 1) {
      ESMCI::TraceMPIWaitEnd();
    }
  }


  /*
    MPI_ALLGATHER(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT,
       RECVTYPE, COMM, IERROR)
    <type>    SENDBUF (*), RECVBUF (*)
    INTEGER    SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE, COMM,
    INTEGER    IERROR
  */

  extern void FTN_X(__real_mpi_allgather)(MPI_Fint *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, 
					  MPI_Fint *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, 
					  MPI_Fint *comm, MPI_Fint *ierr);

  void FTN_X(__wrap_mpi_allgather)(MPI_Fint *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, 
				   MPI_Fint *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, 
				   MPI_Fint *comm, MPI_Fint *ierr) {
    if (c_esmftrace_isactive() == 1) {
      ESMCI::TraceMPIWaitStart();
    }
    FTN_X(__real_mpi_allgather)(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, ierr);
    if (c_esmftrace_isactive() == 1) {
      ESMCI::TraceMPIWaitEnd();
    }  
  }


  /*
    MPI_ALLGATHERV(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF,
    RECVCOUNT, DISPLS, RECVTYPE, COMM, IERROR)
    <type>    SENDBUF(*), RECVBUF(*)
    INTEGER    SENDCOUNT, SENDTYPE, RECVCOUNT(*)
    INTEGER    DISPLS(*), RECVTYPE, COMM, IERROR
  */
  
  extern void FTN_X(__real_mpi_allgatherv)(MPI_Fint *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, 
					   MPI_Fint *recvbuf, MPI_Fint *recvcount, MPI_Fint *displs, 
					   MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *ierr);
  
  void FTN_X(__wrap_mpi_allgatherv)(MPI_Fint *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, 
				    MPI_Fint *recvbuf, MPI_Fint *recvcount, MPI_Fint *displs, 
				    MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *ierr) {
    if (c_esmftrace_isactive() == 1) {
      ESMCI::TraceMPIWaitStart();
    }
    FTN_X(__real_mpi_allgatherv)(sendbuf, sendcount, sendtype, recvbuf, recvcount, displs, recvtype, comm, ierr);
    if (c_esmftrace_isactive() == 1) {
      ESMCI::TraceMPIWaitEnd();
    }  
  }


  /*
    MPI_ALLREDUCE(SENDBUF, RECVBUF, COUNT, DATATYPE, OP, COMM, IERROR)
    <type>    SENDBUF(*), RECVBUF(*)
    INTEGER    COUNT, DATATYPE, OP, COMM, IERROR
  */
  
  extern void FTN_X(__real_mpi_allreduce)(MPI_Fint *sendbuf, MPI_Fint *recvbuf, MPI_Fint *count, 
					  MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *ierr);
  
  void FTN_X(__wrap_mpi_allreduce)(MPI_Fint *sendbuf, MPI_Fint *recvbuf, MPI_Fint *count, 
				   MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *ierr) {
    //printf("__wrap_mpi_allreduce_ (Fortran)\n");
    //TODO:  DEAL with different MPI calls in trace
    if (c_esmftrace_isactive() == 1) {
      ESMCI::TraceMPIWaitStart();
    }
    FTN_X(__real_mpi_allreduce)(sendbuf, recvbuf, count, datatype, op, comm, ierr);
    if (c_esmftrace_isactive() == 1) {
      ESMCI::TraceMPIWaitEnd();
    }  
  }



  
    
}
