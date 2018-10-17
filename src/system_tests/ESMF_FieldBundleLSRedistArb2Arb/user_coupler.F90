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
  type(ESMF_RouteHandle), save :: routehandle

  contains

  subroutine usercpl_register(comp, rc)
    type(ESMF_CplComp) :: comp
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Coupler Register starting"
    
    ! Register the callback routines.
    call ESMF_CplCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, userRoutine=user_init, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return
    call ESMF_CplCompSetEntryPoint(comp, ESMF_METHOD_RUN, userRoutine=user_run, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return
    call ESMF_CplCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, userRoutine=user_final, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return

    print *, "Registered Initialize, Run, and Finalize routines"
    print *, "User Coupler Register returning"

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
    integer :: itemcount, localPet, i
    type(ESMF_FieldBundle) :: srcFieldBundle, dstFieldBundle
    type(ESMF_VM) :: vm

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Coupler Init starting"

    call ESMF_StateGet(importState, itemcount=itemcount, rc=rc)
    if (rc/=ESMF_SUCCESS) return
    print *, "Import State contains ", itemcount, " items."

    ! Need to reconcile import and export states
    call ESMF_CplCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return
    call ESMF_StateReconcile(importState, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return
    call ESMF_StateReconcile(exportState, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return

    ! Get source FieldBundle out of import State
    call ESMF_StateGet(importState, "fieldbundle data", srcFieldBundle, rc=rc)
    if (rc/=ESMF_SUCCESS) return

    ! Get destination FieldBundle out of export State
    call ESMF_StateGet(exportState, "fieldbundle data", dstFieldBundle, rc=rc)
    if (rc/=ESMF_SUCCESS) return

    call ESMF_FieldBundleRedistStore(srcFieldBundle=srcFieldBundle, dstFieldBundle=dstFieldBundle, &
      routehandle=routehandle, rc=rc)
    if (rc/=ESMF_SUCCESS) return

    print *, "User Coupler Init returning"
   
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
    type(ESMF_FieldBundle) :: srcFieldBundle, dstFieldBundle

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Coupler Run starting"

    ! Get source FieldBundle out of import State
    call ESMF_StateGet(importState, "fieldbundle data", srcFieldBundle, rc=rc)
    if (rc/=ESMF_SUCCESS) return

    ! Get destination FieldBundle out of export State
    call ESMF_StateGet(exportState, "fieldbundle data", dstFieldBundle, rc=rc)
    if (rc/=ESMF_SUCCESS) return

    ! Use FieldBundleRedist() to take data from srcFieldBundle to dstFieldBundle
    call ESMF_FieldBundleRedist(srcFieldBundle=srcFieldBundle, dstFieldBundle=dstFieldBundle, &
      routehandle=routehandle, rc=rc)
    if (rc/=ESMF_SUCCESS) return
  
    print *, "User Coupler Run returning"

  end subroutine user_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
  subroutine user_final(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Coupler Final starting"
  
    ! Release resources stored for the FieldBundleRedist.
    call ESMF_FieldBundleRedistRelease(routehandle=routehandle, rc=rc)
    if (rc/=ESMF_SUCCESS) return

    print *, "User Coupler Final returning"
  
  end subroutine user_final


end module user_coupler
    
!\end{verbatim}

