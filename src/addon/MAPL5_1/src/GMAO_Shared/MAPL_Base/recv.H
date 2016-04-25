! $Id$

#ifdef NAME_
#undef NAME_
#endif

#define NAME_ MAPL_CommsRecv_

#include "overload.macro"

  subroutine SUB_ ( layout, DATA, COUNT, SOURCE, RC)

    type (ESMF_DELayout)                         :: layout
#if (VARTYPE_ > 0)
    TYPE_(kind=EKIND_), intent(INOUT)            :: data DIMENSIONS_
#else
    character(LEN=*),   intent(INOUT)            :: data DIMENSIONS_
#endif

    integer,            intent(in   )            :: COUNT
    integer,            intent(in   )            :: SOURCE
    integer         ,   intent(  out),  optional :: RC

    character(len=ESMF_MAXSTR), parameter :: IAM='MAPL_Recv'
    integer                               :: status
    integer                               :: mpstatus(MPI_STATUS_SIZE)
    integer                               :: ierr
    integer                               :: comm
    type(ESMF_VM)                         :: vm

    call ESMF_DELayoutGet(layout, vm=vm, rc=status)
    VERIFY_(STATUS)

    call ESMF_VMGet(vm, mpiCommunicator=COMM, rc=status)
    VERIFY_(STATUS)

    call MPI_Recv(DATA, count, MPITYPE_, source, msg_tag, COMM, MPSTATUS, IERR)
    VERIFY_(IERR)

    RETURN_(ESMF_SUCCESS)
  END SUBROUTINE SUB_

#undef NAME_

#undef DIMENSIONS_
#undef RANK_
#undef VARTYPE_


