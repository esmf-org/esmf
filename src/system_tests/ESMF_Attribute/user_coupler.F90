! $Id: user_coupler.F90,v 1.7 2008/03/13 14:32:53 rokuingh Exp $
!
! Example/test code which shows User Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  User-supplied Coupler
!
!
!\begin{verbatim}

module user_coupler

  ! ESMF Framework module
  use ESMF_Mod
    
  implicit none
   
  public usercpl_register
        
  contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
  subroutine usercpl_register(comp, rc)
    type(ESMF_CplComp) :: comp
    integer :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS
    
    ! Register the callback routines.
    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETINIT, user_init, &
      ESMF_SINGLEPHASE, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETRUN, user_run, &
      ESMF_SINGLEPHASE, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETFINAL, user_final, &
      ESMF_SINGLEPHASE, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

#ifdef ESMF_TESTWITHTHREADS
    ! The following call will turn on ESMF-threading (single threaded)
    ! for this component. If you are using this file as a template for 
    ! your own code development you probably don't want to include the 
    ! following call unless you are interested in exploring ESMF's 
    ! threading features.
    call ESMF_CplCompSetVMMinThreads(comp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
#endif

  end subroutine

!-------------------------------------------------------------------------
!   !User Comp Component created by higher level calls, here is the
!   ! Initialization routine.
    
  subroutine user_init(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer :: rc

    ! Local variables
    type(ESMF_VM)                    :: vm
    integer                          :: myPet, status, petCount
    integer                          :: imp_items, exp_items, forward_init, backward_init
    
    ! Initialize return code
    rc = ESMF_SUCCESS
    
    forward_init=0
    backward_init=0

    call ESMF_StateGet(importState, itemcount=imp_items, rc=rc)
    call ESMF_StateGet(exportState, itemcount=exp_items, rc=rc)
    !print  *, 'Import state has ', imp_items, ' items' 
    !print  *, 'Export state has ', exp_items, ' items' 

    ! Need to reconcile import and export states
    call ESMF_CplCompGet(comp, vm=vm, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 10
!   call ESMF_StateReconcile(importState, vm, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 10
!   call ESMF_StateReconcile(exportState, vm, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 10

    call ESMF_VMGet(vm=vm, localPet=myPet, petCount=petCount, rc=rc)
    if (status .ne. ESMF_SUCCESS) goto 10

    ! Get the direction of coupling initialization
    call ESMF_StateAttributeGetInfo(exportState, "forward_init", rc=forward_init)
    call ESMF_StateAttributeGetInfo(exportState, "backward_init", rc=backward_init)

    ! Forward coupling initialization
    if (forward_init .eq. ESMF_SUCCESS) then
      !woopee!
    endif
    
    ! Backward coupling initialization
    if (backward_init .eq. ESMF_SUCCESS) then
      !woopee!
    endif
 
    rc = ESMF_SUCCESS
    return
                                                                  
    ! get here only on error exit
10  continue
    rc = ESMF_FAILURE
    print *, "FAILURE in Coupler INIT!!!"  

  end subroutine user_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is coupled.
!   !
 
  subroutine user_run(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer :: rc

    ! Local variables
    type(ESMF_VM)               :: vm
    integer                     :: forward_run, backward_run, status, imp_items, exp_items, &
                                   myPet, petCount
    character(len=ESMF_MAXSTR)  :: name, value, conv, purp 

    ! Initialize return code
    rc = ESMF_SUCCESS

    forward_run=0
    backward_run=0

    call ESMF_StateGet(importState, itemcount=imp_items, rc=rc)
    call ESMF_StateGet(exportState, itemcount=exp_items, rc=rc)
    !print  *, 'Import state has ', imp_items, ' items' 
    !print  *, 'Export state has ', exp_items, ' items' 

    ! Need to reconcile import and export states
    call ESMF_CplCompGet(comp, vm=vm, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 20
!   call ESMF_StateReconcile(importState, vm, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 20
!   call ESMF_StateReconcile(exportState, vm, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 20
    call ESMF_VMGet(vm, localPet=myPet, petCount=petCount, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 20

    ! Get the direction from the exportState
    call ESMF_StateAttributeGetInfo(exportState, "forward_run", rc=forward_run)
    call ESMF_StateAttributeGetInfo(exportState, "backward_run", rc=backward_run)

    ! If this is forward coupling
    if (forward_run .eq. ESMF_SUCCESS) then
      !woopee!
    endif
    
    ! If this is backward coupling
    if (backward_run .eq. ESMF_SUCCESS) then
      conv = 'ESG-CDP'  
      purp = 'general'
      name = 'organization'
      value = 'changed attpack attribute organization in user coupler run 2'
      call ESMF_StateAttPackSet(exportState, name, value, convention=conv, purpose=purp, rc=rc)
      if (status .ne. ESMF_SUCCESS) goto 20
    endif

    rc = ESMF_SUCCESS
    return

    ! get here only on error exit
20  continue
    rc = ESMF_FAILURE
    print *, "FAILURE in Coupler RUN!!!"

    end subroutine user_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
  subroutine user_final(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer :: rc

    rc = ESMF_SUCCESS
    return
                                                                  
    ! get here only on error exit
30  continue
    rc = ESMF_FAILURE
    print *, "FAILURE in Coupler FINAL!!!"

    rc = ESMF_SUCCESS

  end subroutine user_final

end module user_coupler
    
!\end{verbatim}

