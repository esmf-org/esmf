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
    type(ESMF_Array) :: srcArray1, srcArray2
    integer          :: srcF90(100,150)
    type(ESMF_VM) :: vm

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Coupler Init starting"

    ! Need to reconcile import and export states
    call ESMF_CplCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateReconcile(importState, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateReconcile(exportState, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Get the Arrays from the states
    call ESMF_StateGet(importState, "srcArray1", srcArray1, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateGet(exportState, "srcArray2", srcArray2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! ArrayGather() srcArray1 into srcF90 on rootPet=5
    call ESMF_ArrayGather(srcArray1, srcF90, rootPet=5, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! ArrayScatter() srcF90 on rootPet=5 across srcArray2 
    call ESMF_ArrayScatter(srcArray2, srcF90, rootPet=5, rc=rc)
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
    type(ESMF_Array) :: dstArray1, dstArray2
    integer          :: dstF90(100,150)

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Coupler Run starting"

    ! Get the Arrays from the states
    call ESMF_StateGet(exportState, "dstArray1", dstArray1, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateGet(exportState, "dstArray2", dstArray2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! ArrayGather() dstArray2 into dstF90 on rootPet=2
    call ESMF_ArrayGather(dstArray2, dstF90, rootPet=2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! ArrayScatter() dstF90 on rootPet=2 across dstArray1
    call ESMF_ArrayScatter(dstArray1, dstF90, rootPet=2, rc=rc)
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
  
    print *, "User Coupler Final returning"
  
  end subroutine user_final


end module user_coupler
    
!\end{verbatim}

