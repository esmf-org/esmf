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
   
  public usercpl_setvm, usercpl_register
        
  ! global data
  type(ESMF_RouteHandle), save :: routehandle

  contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
  subroutine usercpl_setvm(comp, rc)
    type(ESMF_CplComp) :: comp
    integer, intent(out) :: rc
#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm
    logical :: pthreadsEnabled
#endif

    ! Initialize return code
    rc = ESMF_SUCCESS

#ifdef ESMF_TESTWITHTHREADS
    ! The following call will turn on ESMF-threading (single threaded)
    ! for this component. If you are using this file as a template for
    ! your own code development you probably don't want to include the
    ! following call unless you are interested in exploring ESMF's
    ! threading features.

    ! First test whether ESMF-threading is supported on this machine
    call ESMF_VMGetGlobal(vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMGet(vm, pthreadsEnabledFlag=pthreadsEnabled, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    if (pthreadsEnabled) then
      call ESMF_CplCompSetVMMinThreads(comp, rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
    endif
#endif

  end subroutine

  subroutine usercpl_register(comp, rc)
    type(ESMF_CplComp) :: comp
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Coupler Register starting"
    
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
    integer :: itemcount
    type(ESMF_ArrayBundle):: srcArraybundle, dstArraybundle
    type(ESMF_VM) :: vm

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Coupler Init starting"

    call ESMF_StateGet(importState, itemcount=itemcount, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    print *, "Import State contains ", itemcount, " items."

    ! Need to reconcile import and export states
    call ESMF_CplCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateReconcile(importState, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateReconcile(exportState, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Get source ArrayBundle out of import state
    call ESMF_StateGet(importState, "srcAryBndl", srcArraybundle, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Get destination ArrayBundle out of export state
    call ESMF_StateGet(exportState, "dstAryBndl", dstArraybundle, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Precompute and store an ArrayBundleRedist operation
    call ESMF_ArrayBundleRedistStore(srcArrayBundle=srcArraybundle, &
      dstArrayBundle=dstArraybundle, routehandle=routehandle, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
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
    type(ESMF_ArrayBundle):: srcArraybundle, dstArraybundle

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Coupler Run starting"

    ! Get source ArrayBundle out of import state
    call ESMF_StateGet(importState, "srcAryBndl", srcArrayBundle, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Get destination ArrayBundle out of export state
    call ESMF_StateGet(exportState, "dstAryBndl", dstArraybundle, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Use ESMF_ArrayBundleRedist() to take data
    ! from srcArraybundle to dstArraybundle
    call ESMF_ArrayBundleRedist(srcArrayBundle=srcArraybundle, &
      dstArrayBundle=dstArraybundle, routehandle=routehandle, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
  
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
  
    ! Release resources stored for the ESMF_ArrayBundleRedist
    call ESMF_ArrayBundleRedistRelease(routehandle=routehandle, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print *, "User Coupler Final returning"
  
  end subroutine user_final


end module user_coupler
    
!\end{verbatim}

