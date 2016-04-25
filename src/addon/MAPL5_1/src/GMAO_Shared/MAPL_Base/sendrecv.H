! $Id$

#ifdef NAME_
#undef NAME_
#endif

#define NAME_ MAPL_CommsSendRecv_

#include "overload.macro"

  subroutine SUB_ ( layout, SENDBUF, SENDCOUNT, DEST, RECVBUF, RECVCOUNT, SOURCE, RC)

    type (ESMF_DELayout)                         :: layout
#if (VARTYPE_ > 0)
    TYPE_(kind=EKIND_), intent(INOUT)            :: sendbuf DIMENSIONS_
    TYPE_(kind=EKIND_), intent(INOUT)            :: recvbuf DIMENSIONS_
#else
    character(LEN=*),   intent(INOUT)            :: sendbuf DIMENSIONS_
    character(LEN=*),   intent(INOUT)            :: recvbuf DIMENSIONS_
#endif

    integer,            intent(in   )            :: sendcount
    integer,            intent(in   )            :: recvcount
    integer,            intent(in   )            :: DEST
    integer,            intent(in   )            :: SOURCE
    integer         ,   intent(  out),  optional :: RC

    character(len=ESMF_MAXSTR), parameter :: IAM='MAPL_SendRecv'
    integer                               :: status
    integer                               :: mpstatus(MPI_STATUS_SIZE)
    integer                               :: ierr
    integer                               :: comm
    type(ESMF_VM)                         :: vm

    call ESMF_DELayoutGet(layout, vm=vm, rc=status)
    VERIFY_(STATUS)

    call ESMF_VMGet(vm, mpiCommunicator=COMM, rc=status)
    VERIFY_(STATUS)

    call MPI_SendRecv( sendbuf, sendcount, MPITYPE_, dest,   msg_tag, &
                       recvbuf, recvcount, MPITYPE_, source, msg_tag, &
                       COMM, mpstatus, IERR )
    VERIFY_(IERR)
  
    RETURN_(ESMF_SUCCESS)
  END SUBROUTINE SUB_

#undef NAME_

#undef DIMENSIONS_
#undef RANK_
#undef VARTYPE_


