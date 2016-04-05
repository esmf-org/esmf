! $Id$

#ifdef NAME_
#undef NAME_
#endif

#define NAME_ MAPL_CommsBcast_

#include "overload.macro"

  subroutine SUB_ ( layout, DATA, N, ROOT, RC)

    type (ESMF_DELayout)                         :: layout
#if (VARTYPE_ == 2)
    logical,   intent(INOUT)            :: data DIMENSIONS_
#else
    TYPE_(kind=EKIND_), intent(INOUT)            :: data DIMENSIONS_
#endif

    integer,            intent(in   )            :: N
    integer,            intent(in   )            :: ROOT
    integer         ,   intent(  out),  optional :: RC

    character(len=ESMF_MAXSTR), parameter :: IAM='MAPL_Bcast'
    integer                               :: status
    integer                               :: comm
    type(ESMF_VM)                         :: vm

    call ESMF_DELayoutGet(layout, vm=vm, rc=status)
    VERIFY_(STATUS)

    call ESMF_VMGet(vm, mpiCommunicator=COMM, rc=status)
    VERIFY_(STATUS)

    call MPI_Bcast(DATA, N, MPITYPE_, ROOT, COMM, STATUS)
    VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)
  END SUBROUTINE SUB_

#undef NAME_

#define NAME_ MAPL_CommsBcastVm_

  subroutine SUB_ ( vm, DATA, N, ROOT, RC)

    type (ESMF_VM)                               :: vm
#if (VARTYPE_ == 2)
    logical,   intent(INOUT)            :: data DIMENSIONS_
#else
    TYPE_(kind=EKIND_), intent(INOUT)            :: data DIMENSIONS_
#endif

    integer,            intent(in   )            :: N
    integer,            intent(in   )            :: ROOT
    integer         ,   intent(  out),  optional :: RC

    character(len=ESMF_MAXSTR), parameter :: IAM='MAPL_BcastVm'
    integer                               :: status
    integer                               :: comm

    call ESMF_VMGet(vm, mpiCommunicator=COMM, rc=status)
    VERIFY_(STATUS)

    call MPI_Bcast(DATA, N, MPITYPE_, ROOT, COMM, STATUS)
    VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)
  END SUBROUTINE SUB_

#undef NAME_

#undef DIMENSIONS_
#undef RANK_
#undef VARTYPE_


