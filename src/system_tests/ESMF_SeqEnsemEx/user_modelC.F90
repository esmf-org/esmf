! $Id: user_modelC.F90,v 1.8 2009/03/31 17:39:30 theurich Exp $
!
! Example/test code which shows User Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  User-supplied Component, most recent interface revision.
!
!
!\begin{verbatim}

module user_modelC

  ! ESMF Framework module
  use ESMF_Mod

  implicit none
    
  public userC_setvm, userC_register
        
  contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
  subroutine userC_setvm(comp, rc)
    type(ESMF_GridComp)  :: comp
    integer, intent(out) :: rc

#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm
    logical :: supportPthreads
    ! The following call will turn on ESMF-threading (single threaded)
    ! for this component. If you are using this file as a template for 
    ! your own code development you probably don't want to include the 
    ! following call unless you are interested in exploring ESMF's 
    ! threading features.

    ! First test whether ESMF-threading is supported on this machine
    call ESMF_VMGetGlobal(vm, rc=rc)
    call ESMF_VMGet(vm, supportPthreadsFlag=supportPthreads, rc=rc)
    if (supportPthreads) then
      call ESMF_GridCompSetVMMinThreads(comp, rc=rc)
    endif
#endif

    rc = ESMF_SUCCESS
  end subroutine

  subroutine userC_register(comp, rc)
    type(ESMF_GridComp)  :: comp
    integer, intent(out) :: rc

    print *, "in user register routine"

    ! Register the callback routines.

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, userRoutine=user_init, &
      rc=rc)
    if (rc .ne. ESMF_SUCCESS) return
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, userRoutine=user_run, &
      rc=rc)
    if (rc .ne. ESMF_SUCCESS) return
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, userRoutine=user_final, &
      rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

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
    integer               :: initConditionC
    
    rc = ESMF_SUCCESS

    ! Set initial condition.
    initConditionC = 2

    ! Add the integer to the export State
    call ESMF_AttributeSet(exportState, name="CondC", value=initConditionC, rc=rc)
   
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
    integer               :: ConditionC
    
    rc = ESMF_SUCCESS

    ! Get the value from the State
    call ESMF_AttributeGet(exportState, name="CondC", value=ConditionC, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! increment condition C and put back in the export State
    ConditionC = ConditionC + 1
    call ESMF_AttributeSet(exportState, name="CondC", value=ConditionC, rc=rc)
   
    print *, "User Comp C Run returning"

  end subroutine user_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
  subroutine user_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables

    integer               :: ConditionC

    rc = ESMF_SUCCESS

    print *, "User Comp Final starting"

    ! Get the value from the State
    call ESMF_AttributeGet(exportState, name="CondC", value=ConditionC, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return
    
    if ( ConditionC.eq.7) then
      rc = ESMF_SUCCESS
    else
      rc = ESMF_FAILURE
      print *, "ConditionC = ", ConditionC
    end if
    
    print *, "User Comp Final returning"

  end subroutine user_final


end module user_modelC
    
!\end{verbatim}
    
