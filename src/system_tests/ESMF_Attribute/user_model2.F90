! $Id: user_model2.F90,v 1.2 2007/12/17 15:44:36 rokuingh Exp $
!
! Example/test code which shows User Component calls.

!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  User-supplied Component
!
!
!\begin{verbatim}

module user_model2

  ! ESMF Framework module
  use ESMF_Mod

  implicit none
    
  public userm2_register
        
  contains

!--------------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
  subroutine userm2_register(comp, rc)
    type(ESMF_GridComp), intent(inout) :: comp
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Register the callback routines.

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, user_init, &
      ESMF_SINGLEPHASE, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, user_run, &
      ESMF_SINGLEPHASE, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, user_final, &
      ESMF_SINGLEPHASE, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    !print *, "Registered Initialize, Run, and Finalize routines"

#ifdef ESMF_TESTWITHTHREADS
    ! The following call will turn on ESMF-threading (single threaded)
    ! for this component. If you are using this file as a template for 
    ! your own code development you probably don't want to include the 
    ! following call unless you are interested in exploring ESMF's 
    ! threading features.
    call ESMF_GridCompSetVMMinThreads(comp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
#endif

  end subroutine

!--------------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
  subroutine user_init(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp), intent(inout) :: comp
    type(ESMF_State), intent(inout) :: importState, exportState
    type(ESMF_Clock), intent(in) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_VM)         :: vm
    integer               :: petCount
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Determine petCount
    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMGet(vm, petCount=petCount, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    rc = ESMF_SUCCESS
    return
    
    ! get here only on error exit
10  continue
    rc = ESMF_FAILURE
    print *, "FAILURE in COM2 INIT!!!!"

  end subroutine user_init


!--------------------------------------------------------------------------------
!   !  The Run routine where data is validated.
!   !
 
  subroutine user_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp), intent(inout) :: comp
    type(ESMF_State), intent(inout) :: importState, exportState
    type(ESMF_Clock), intent(in) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_VM)               :: vm
    integer                     :: petCount
            
    ! Initialize return code
    rc = ESMF_SUCCESS

    !woopee

    rc = ESMF_SUCCESS
    return
    
    ! get here only on error exit
20  continue
    rc = ESMF_FAILURE
    print *, "FAILURE in COMP2 RUN!!!"   
 
  end subroutine user_run


!--------------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
  subroutine user_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp), intent(inout) :: comp
    type(ESMF_State), intent(inout) :: importState, exportState
    type(ESMF_Clock), intent(in) :: clock
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS
    return
                                                                                                                  
    ! get here only on error exit
30  continue
    rc = ESMF_FAILURE
    print *, "FAILURE in COMP2 Finalize!!!"
    return
   
  end subroutine user_final

end module user_model2
!\end{verbatim}
