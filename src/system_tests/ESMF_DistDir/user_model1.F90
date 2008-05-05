! $Id: user_model1.F90,v 1.5.2.2 2008/05/05 18:45:28 theurich Exp $
!
! Example/test code which shows User Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  User-supplied Component
!
!
!\begin{verbatim}

module user_model1

  ! ESMF Framework module
  use ESMF_Mod

  implicit none
    
  public userm1_register
        
  contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
  subroutine userm1_register(comp, rc)
    type(ESMF_GridComp) :: comp
    integer, intent(out) :: rc
#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm
    type(ESMF_Logical) :: supportPthreads
#endif

    print *, "in user register routine"

    ! Register the callback routines.

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, user_init, &
      ESMF_SINGLEPHASE, rc)
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, user_run, &
      ESMF_SINGLEPHASE, rc)
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
    if (supportPthreads == ESMF_True) then
      call ESMF_GridCompSetVMMinThreads(comp, rc=rc)
    endif
#endif

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
!    type(ESMF_ArraySpec)  :: arrayspec
!    type(ESMF_DistGrid)   :: distgrid
!    type(ESMF_Array)      :: array
    type(ESMF_VM)         :: vm
    integer               :: petCount, status
    
    ! Determine petCount
    call ESMF_GridCompGet(comp, vm=vm, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 10
    call ESMF_VMGet(vm, petCount=petCount, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 10
    
    rc = ESMF_SUCCESS
    return
    
    ! get here only on error exit
10  continue
    rc = ESMF_FAILURE

  end subroutine user_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
  subroutine user_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    type(ESMF_VM)        :: vm
    integer              :: status, petCount

    ! Local variables
    
    print *, "User Comp Run starting"
    
    ! Determine petCount
    call ESMF_GridCompGet(comp, vm=vm, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 20
    call ESMF_VMGet(vm, petCount=petCount, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 20

    call cdistdir_test(vm, rc);

    print *, "User Comp Run returning"
    return

20  continue
    rc = ESMF_FAILURE

  end subroutine user_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
  subroutine user_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    print *, "User Comp Final starting"

    print *, "User Comp Final returning"

    rc = ESMF_SUCCESS
  end subroutine user_final


end module user_model1
    
!\end{verbatim}
    
