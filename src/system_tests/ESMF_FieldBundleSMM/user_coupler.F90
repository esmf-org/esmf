! $Id: user_coupler.F90,v 1.6 2009/10/22 14:40:48 feiliu Exp $
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
  use ESMF_Mod
    
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
    call ESMF_VMGet(vm, pthreadsEnabledFlag=pthreadsEnabled, rc=rc)
    if (pthreadsEnabled) then
      call ESMF_CplCompSetVMMinThreads(comp, rc=rc)
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
    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETINIT, userRoutine=user_init, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETRUN, userRoutine=user_run, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETFINAL, userRoutine=user_final, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

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
    integer :: itemcount, localPet
    type(ESMF_FieldBundle) :: srcFieldBundle, dstFieldBundle
    type(ESMF_VM) :: vm
    real(ESMF_KIND_R8):: factorList(10000)
    integer:: i, factorIndexList(2,10000)

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Coupler Init starting"

    call ESMF_StateGet(importState, itemcount=itemcount, rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
    print *, "Import State contains ", itemcount, " items."

    ! Need to reconcile import and export states
    call ESMF_CplCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
    call ESMF_StateReconcile(importState, vm, rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
    call ESMF_StateReconcile(exportState, vm, rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

    ! Get source FieldBundle out of import State
    call ESMF_StateGet(importState, "fieldbundle data", srcFieldBundle, rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

    ! Get destination FieldBundle out of export State
    call ESMF_StateGet(exportState, "fieldbundle data", dstFieldBundle, rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

    ! get localPet
    call ESMF_VMGet(vm, localPet=localPet, rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

    ! Setup identity sparse matrix as a combination of PET 0 and PET 4
    ! there are 15000 elements on the diagonal, defined as two overlapping
    ! lists of each 10000 elements (overlapping 5000).
    if (localPet==0) then
      factorIndexList(1,:) = (/(i,i=1,10000)/)
      factorIndexList(2,:) = factorIndexList(1,:)
      do i=1, 5000
        factorList(i) = 1.d0
      enddo
      do i=5001, 10000
        factorList(i) = .253d0
      enddo
    endif
    if (localPet==4) then
      factorIndexList(1,:) = (/(i,i=5001,15000)/)
      factorIndexList(2,:) = factorIndexList(1,:)
      do i=1, 5000
        factorList(i) = .747d0
      enddo
      do i=5001, 10000
        factorList(i) = 1.d0
      enddo
    endif

    ! Precompute and store an FieldBundleSMM operation
    if (localPet==0 .or. localPet==4) then
      ! only PET 0 and PET 4 provide factors
      call ESMF_FieldBundleSMMStore(srcFieldBundle=srcFieldBundle, &
        dstFieldBundle=dstFieldBundle, &
        routehandle=routehandle, factorList=factorList, &
        factorIndexList=factorIndexList, rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
    else
      call ESMF_FieldBundleSMMStore(srcFieldBundle=srcFieldBundle, dstFieldBundle=dstFieldBundle, &
        routehandle=routehandle, rc=rc)
      if (rc/=ESMF_SUCCESS) call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
    endif
    
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
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

    ! Get destination FieldBundle out of export State
    call ESMF_StateGet(exportState, "fieldbundle data", dstFieldBundle, rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

    ! Use FieldBundleSMM() to take data from srcFieldBundle to dstFieldBundle
    call ESMF_FieldBundleSMM(srcFieldBundle=srcFieldBundle, dstFieldBundle=dstFieldBundle, &
      routehandle=routehandle, rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)
  
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
  
    ! Release resources stored for the FieldBundleSMM.
    call ESMF_FieldBundleSMMRelease(routehandle=routehandle, rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(rc=rc, terminationflag=ESMF_ABORT)

    print *, "User Coupler Final returning"
  
  end subroutine user_final


end module user_coupler
    
!\end{verbatim}

