! $Id: user_modelA.F90,v 1.9 2009/05/29 19:24:43 theurich Exp $
!
! Example/test code which shows User Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!
! !DESCRIPTION:
!  User-supplied Component, most recent interface revision.
!
!
!\begin{verbatim}

module user_modelA

  ! ESMF Framework module
  use ESMF_Mod

  implicit none
    
  public userA_setvm, userA_register
        
  contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
  subroutine userA_setvm(comp, rc)
    type(ESMF_GridComp)  :: comp
    integer, intent(out) :: rc

#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm
    logical :: pthreadsEnabled
    ! The following call will turn on ESMF-threading (single threaded)
    ! for this component. If you are using this file as a template for 
    ! your own code development you probably don't want to include the 
    ! following call unless you are interested in exploring ESMF's 
    ! threading features.

    ! First test whether ESMF-threading is supported on this machine
    call ESMF_VMGetGlobal(vm, rc=rc)
    call ESMF_VMGet(vm, pthreadsEnabledFlag=pthreadsEnabled, rc=rc)
    if (pthreadsEnabled) then
      call ESMF_GridCompSetVMMinThreads(comp, rc=rc)
    endif
#endif

    rc = ESMF_SUCCESS
  end subroutine

  subroutine userA_register(comp, rc)
    type(ESMF_GridComp)  :: comp
    integer, intent(out) :: rc

    print *, "in user register routine"

    ! Register the callback routines.

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, userRoutine=user_init, &
      rc=rc)
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, userRoutine=user_run, &
      rc=rc)
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, userRoutine=user_final, &
      rc=rc)

    print *, "Registered Initialize, Run, and Finalize routines"

    rc = ESMF_SUCCESS
  end subroutine

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
  subroutine user_init(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    integer               :: initConditionA
    
    rc = ESMF_SUCCESS

    ! Set initial condition.
    initConditionA = 0

    ! Add the integer to the export State
    call ESMF_AttributeSet(exportState, name="CondA", value=initConditionA, rc=rc)
   
  end subroutine user_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
  subroutine user_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    integer               :: ConditionA
    
    rc = ESMF_SUCCESS

    ! Get the value from the State
    call ESMF_AttributeGet(exportState, name="CondA", value=ConditionA, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! increment condition A and put back in the export State
    ConditionA = ConditionA + 1
    call ESMF_AttributeSet(exportState, name="CondA", value=ConditionA, rc=rc)
   
    print *, "User Comp A Run returning"

  end subroutine user_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
  subroutine user_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) ::importState,  exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    integer               :: ConditionA

    print *, "User Comp Final starting"

    rc = ESMF_SUCCESS

    ! Get the value from the State
    call ESMF_AttributeGet(exportState, name="CondA", value=ConditionA, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    if ( ConditionA.eq.5) then
      rc = ESMF_SUCCESS
    else
      rc = ESMF_FAILURE
      print *, "ConditionA = ", ConditionA
    end if

    print *, "User Comp Final returning"

  end subroutine user_final


end module user_modelA
    
!\end{verbatim}
    
