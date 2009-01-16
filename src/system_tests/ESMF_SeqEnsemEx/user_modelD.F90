! $Id: user_modelD.F90,v 1.6 2009/01/16 05:28:25 theurich Exp $
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

module user_modelD

  ! ESMF Framework module
  use ESMF_Mod

  implicit none
    
  public userD_setvm, userD_register
        
  contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
  subroutine userD_setvm(comp, rc)
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

  subroutine userD_register(comp, rc)
    type(ESMF_GridComp)  :: comp
    integer, intent(out) :: rc

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

    ! Nothing to do
    rc = ESMF_SUCCESS 
    return
    
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
    integer               :: ConditionA, ConditionB, ConditionC
    real               :: average
    
    ! Get the values from the State
    call ESMF_AttributeGet(importState, name="CondA", value=ConditionA, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    call ESMF_AttributeGet(importState, name="CondB", value=ConditionB, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    call ESMF_AttributeGet(importState, name="CondC", value=ConditionC, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! Calculate average of the 3 conditions
    average = ((ConditionA + ConditionB + ConditionC) / 3.0 )
    print *, "ConditionA = ", ConditionA
    print *, "ConditionB = ", ConditionB
    print *, "ConditionC = ", ConditionC
    print *, "The average is  = ", average
   
    print *, "User Comp A Run returning"

  end subroutine user_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
  subroutine user_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) ::  importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    integer               :: ConditionA, ConditionB, ConditionC
    
    print *, "User Comp Final starting"

    ! Get the values from the State
    call ESMF_AttributeGet(importState, name="CondA", value=ConditionA, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    call ESMF_AttributeGet(importState, name="CondB", value=ConditionB, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    call ESMF_AttributeGet(importState, name="CondC", value=ConditionC, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return


    if ((ConditionA.eq.25).and.(ConditionB.eq.36).and.(ConditionC.eq.49)) then
	rc = ESMF_SUCCESS
    else
	rc = ESMF_FAILURE
    	print *, "ConditionA = ", ConditionA
    	print *, "ConditionB = ", ConditionB
    	print *, "ConditionC = ", ConditionC
    end if

    print *, "User Comp Final returning"

  end subroutine user_final


end module user_modelD
    
!\end{verbatim}
    
