! $Id$
!
! Example/test code which shows User Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!
! !DESCRIPTION:
!  User-supplied Coupler
!
!
!\begin{verbatim}

module user_coupler

  ! ESMF Framework module
  use ESMF

  implicit none

  public usercpl_register

  ! global data
  type(ESMF_RouteHandle), save :: rhandle(4)

  contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.

  subroutine usercpl_register(comp, rc)
    type(ESMF_CplComp) :: comp
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Register the callback routines.
    call ESMF_CplCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, userRoutine=user_init, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_CplCompSetEntryPoint(comp, ESMF_METHOD_RUN, userRoutine=user_run, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_CplCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, userRoutine=user_final, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine

!-------------------------------------------------------------------------
!   !User Comp Component created by higher level calls, here is the
!   ! Initialization routine.

  subroutine user_init(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    integer :: i, itemcount
    integer :: petCount, xprocs, yprocs
    character(len=ESMF_MAXSTR) :: name,stateItemNames(4)
    type(ESMF_Array) :: srcArray, dstArray
    type(ESMF_State) :: state
    type(ESMF_VM) :: vm
    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_DistGrid)  :: distgrid
    ! Initialize return code
    rc = ESMF_SUCCESS

    ! get vm
    call ESMF_CplCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_VMGet(vm, petCount = petCount, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_StateGet(importState, name=name, itemNameList=stateItemNames, itemcount=itemcount, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! each item in the import state contains one export state from the ensemble, let's get them one by one
    ! Also call ArrayRedistStore to regrid them to the destination array and add them to the export state
    do i=1,itemcount
        call ESMF_StateGet(importState, stateItemNames(i), state, rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail out

        ! Get source Array out of import state
        call ESMF_StateGet(state, "array data", srcArray, rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail out

        ! Create the destination Array
        call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail out

        ! create a destArray of equal size to do the redist
        xprocs = 2
        yprocs = petCount/2
        distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/100,150/), &
           regDecomp=(/xprocs,yprocs/), rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail out
        dstArray = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
           indexflag=ESMF_INDEX_GLOBAL, rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail out
        call ESMF_ArraySet(dstArray, name=stateItemNames(i), rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail out

        ! Precompute and store an ArrayRedist routehandle for each import array
        call ESMF_ArrayRedistStore(srcArray=srcArray, dstArray=dstArray, &
                routehandle=rhandle(i), rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail out

        ! Add the dstArray into the exportState using the component export state name
        call ESMF_StateAdd(exportState, (/dstArray/), rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail out

    end do

  end subroutine user_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is coupled.
!   !

  subroutine user_run(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_Array) :: srcArray, dstArray
    character(len=ESMF_MAXSTR) :: imStateName, exStateName, stateItemNames(4)
    integer :: itemcount1, itemcount2, i
    type(ESMF_State) :: state
    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Get import State information
    call ESMF_StateGet(importState, name=imStateName, itemNameList=stateItemNames, itemcount=itemcount1, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! Get export state information
    call ESMF_StateGet(exportState, name=exStateName, itemcount=itemcount2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    if (itemcount1 .ne. itemcount2) then
       print *, 'Coupler: the import state and export state items do not match:', itemcount1, itemcount2
       rc=ESMF_FAILURE
       return
    end if

    ! Get the srcArray from the import state item and dstArray from the export state and do a regrid
    do i=1,itemcount1
        call ESMF_StateGet(importState, stateItemNames(i), state, rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail out
        ! Get source Array out of import state
        call ESMF_StateGet(state, "array data", srcArray, rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail out

        ! Get destination Array out of export state
        call ESMF_StateGet(exportState, stateItemNames(i), dstArray, rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail out

        ! Use ArrayRedist() to take data from srcArray to dstArray according to import state name
        call ESMF_ArrayRedist(srcArray=srcArray, dstArray=dstArray, &
                        routehandle=rhandle(i), rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail out
    end do

  end subroutine user_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !

  subroutine user_final(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    character(len=ESMF_MAXSTR) :: stateItemNames(4)
    integer :: itemcount, i
    type(ESMF_Array) :: array
    type(ESMF_Distgrid) :: distgrid

    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Get export state information
    call ESMF_StateGet(exportState, itemNameList=stateItemNames, itemcount=itemcount, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Release resources stored for the ArrayRedist.
    do i=1,itemcount
       call ESMF_ArrayRedistRelease(routehandle=rhandle(i), rc=rc)
       if (rc/=ESMF_SUCCESS) return ! bail out

       ! destroy array and distgrid in the export state
       call ESMF_StateGet(exportState, stateItemNames(i), array, rc=rc)
       if (rc/=ESMF_SUCCESS) return ! bail out
       call ESMF_ArrayGet(array, distgrid=distgrid, rc=rc)
       if (rc/=ESMF_SUCCESS) return ! bail out
       call ESMF_ArrayDestroy(array, rc=rc)
       if (rc/=ESMF_SUCCESS) return ! bail out
       call ESMF_DistGridDestroy(distgrid, rc=rc)
       if (rc/=ESMF_SUCCESS) return ! bail out
    end do

  end subroutine user_final


end module user_coupler

!\end{verbatim}

