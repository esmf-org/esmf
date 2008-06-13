! $Id: user_modelB.F90,v 1.3 2008/06/13 00:29:36 theurich Exp $
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

module user_modelB

  ! ESMF Framework module
  use ESMF_Mod

  implicit none
    
  public userB_register
        
  contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
  subroutine userB_register(comp, rc)
    type(ESMF_GridComp)  :: comp
    integer, intent(out) :: rc

#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm
    logical :: supportPthreads
#endif

    print *, "in user register routine"

    ! Register the callback routines.

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, user_init, &
      ESMF_SINGLEPHASE, rc)
    if (rc .ne. ESMF_SUCCESS) return

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, user_run, &
      ESMF_SINGLEPHASE, rc)
    if (rc .ne. ESMF_SUCCESS) return

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, user_final, &
      ESMF_SINGLEPHASE, rc)

    print *, "Registered Initialize, Run, and Finalize routines"

#ifdef ESMF_TESTWITHTHREADS
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

  end subroutine

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
  subroutine user_init(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp), intent(inout) :: comp
    type(ESMF_State), intent(inout) :: importState, exportState
    type(ESMF_Clock), intent(in) :: clock
    integer, intent(out) :: rc

    ! Local variables
    integer               :: initConditionB
    
    ! Set initial condition.
    initConditionB = 1

    ! Add the integer to the import State
    call ESMF_AttributeSet(exportState, name="CondB", value=initConditionB, rc=rc)
   
    return
    
  end subroutine user_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
  subroutine user_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp), intent(inout) :: comp
    type(ESMF_State), intent(inout) :: importState, exportState
    type(ESMF_Clock), intent(in) :: clock
    integer, intent(out) :: rc

    ! Local variables
    integer               :: ConditionB
    
    ! Get the value from the State
    call ESMF_AttributeGet(exportState, name="CondB", value=ConditionB, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! increment condition B and put back in the export State
    ConditionB = ConditionB + 1
    call ESMF_AttributeSet(exportState, name="CondB", value=ConditionB, rc=rc)
   
    print *, "User Comp B Run returning"

  end subroutine user_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
  subroutine user_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp), intent(inout) :: comp
    type(ESMF_State), intent(inout) ::  importState, exportState
    type(ESMF_Clock), intent(in) :: clock
    integer, intent(out) :: rc

    ! Local variables

    integer               :: ConditionB

    print *, "User Comp Final starting"

   ! Get the value from the State
    call ESMF_AttributeGet(exportState, name="CondB", value=ConditionB, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return
    
   if ( ConditionB.eq.6) then
     rc = ESMF_SUCCESS
   else
     rc = ESMF_FAILURE
     print *, "ConditionB = ", ConditionB
   end if
    
    print *, "User Comp Final returning"

  end subroutine user_final


end module user_modelB
    
!\end{verbatim}
    
