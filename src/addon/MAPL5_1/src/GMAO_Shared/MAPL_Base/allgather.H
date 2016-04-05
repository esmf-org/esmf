! $Id$

#ifdef NAME_
#undef NAME_
#endif

#define NAME_ MAPL_CommsAllGather_

#include "overload.macro"

  subroutine SUB_ (vm, sendbuf, sendcnt, &
                   recvbuf, recvcnt, RC)
  type(ESMF_VM),        intent(in   )            :: vm
    TYPE_,              intent(IN   )            :: sendbuf DIMENSIONS_
    integer,            intent(IN   )            :: sendcnt
    TYPE_,              intent(INOUT)            :: recvbuf DIMENSIONS_
    integer,            intent(IN   )            :: recvcnt
    integer,            intent(  OUT),  optional :: RC

    character(len=ESMF_MAXSTR), parameter :: IAM='MAPL_CommsAllGather'
    integer                               :: status
    integer                               :: comm

    call ESMF_VMGet(vm, mpiCommunicator=COMM, rc=status)
    VERIFY_(STATUS)

    call mpi_allgather ( sendbuf, sendcnt, MPITYPE_, &
                         recvbuf, recvcnt, MPITYPE_, &
                         comm, status )
    VERIFY_(STATUS)


    RETURN_(ESMF_SUCCESS)
  END SUBROUTINE SUB_

#undef NAME_

#undef DIMENSIONS_
#undef RANK_
#undef VARTYPE_

