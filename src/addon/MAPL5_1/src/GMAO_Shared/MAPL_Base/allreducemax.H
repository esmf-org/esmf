! $Id$

#ifdef NAME_
#undef NAME_
#endif

#define NAME_ MAPL_CommsAllReduceMax_

#include "overload.macro"

  subroutine SUB_ (vm, sendbuf, recvbuf, cnt, RC)
  type(ESMF_VM),        intent(in   )            :: vm
    TYPE_(kind=EKIND_), intent(IN   )            :: sendbuf DIMENSIONS_
    TYPE_(kind=EKIND_), intent(INOUT)            :: recvbuf DIMENSIONS_
    integer,            intent(IN   )            :: cnt
    integer,            intent(  OUT),  optional :: RC

    character(len=ESMF_MAXSTR), parameter :: IAM='MAPL_CommsAllReduceMax'
    integer                               :: status
    integer                               :: comm

    call ESMF_VMGet(vm, mpiCommunicator=COMM, rc=status)
    VERIFY_(STATUS)

    call mpi_allreduce ( sendbuf, recvbuf, cnt, MPITYPE_, &
                         MPI_MAX, comm, status )
    VERIFY_(STATUS)


    RETURN_(ESMF_SUCCESS)
  END SUBROUTINE SUB_

#undef NAME_

#undef DIMENSIONS_
#undef RANK_
#undef VARTYPE_

