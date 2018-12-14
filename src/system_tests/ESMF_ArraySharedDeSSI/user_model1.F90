! $Id$
!
! Example/test code which shows User Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!
! !DESCRIPTION:
!  User-supplied Component
!
!
!\begin{verbatim}

module user_model1

  ! ESMF Framework module
  use ESMF

  implicit none
    
  public userm1_setvm, userm1_register
        
  contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
  subroutine userm1_setvm(comp, rc)
    type(ESMF_GridComp) :: comp
    integer, intent(out) :: rc
#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm
    logical       :: pthreadsEnabled
#endif

    ! Initialize return code
    rc = ESMF_SUCCESS
    
    call ESMF_LogWrite("Executing 'userm1_setvm'", ESMF_LOGMSG_INFO, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

#ifdef ESMF_TESTWITHTHREADS
    ! The following call will turn on ESMF-threading (single threaded)
    ! for this component. If you are using this file as a template for
    ! your own code development you probably don't want to include the
    ! following call unless you are interested in exploring ESMF's
    ! threading features.

    ! First test whether ESMF-threading is supported on this machine
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMGet(vm, pthreadsEnabledFlag=pthreadsEnabled, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    if (pthreadsEnabled) then
      call ESMF_GridCompSetVMMinThreads(comp, rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
    endif
#endif

  end subroutine

  subroutine userm1_register(comp, rc)
    type(ESMF_GridComp) :: comp
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    call ESMF_LogWrite("Executing 'userm1_register'", ESMF_LOGMSG_INFO, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Register the callback routines.
    call ESMF_GridCompSetEntryPoint(comp, methodflag=ESMF_METHOD_RUN, &
      userRoutine=user1_run, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine

!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
  subroutine user1_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    real(ESMF_KIND_R8)    :: pi
    type(ESMF_Array)      :: array
    real(ESMF_KIND_R8), pointer :: farrayPtr(:,:)   ! matching F90 array pointer
    integer               :: i, j
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    call ESMF_LogWrite("Executing 'user1_run'", ESMF_LOGMSG_INFO, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    pi = 3.14159d0

    ! Get the source Array from the export State
    call ESMF_StateGet(exportState, "MyArray", array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Gain access to actual data via F90 array pointer
    call ESMF_ArrayGet(array, localDe=0, farrayPtr=farrayPtr, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Fill source Array with data
    do j = lbound(farrayPtr, 2), ubound(farrayPtr, 2)
      do i = lbound(farrayPtr, 1), ubound(farrayPtr, 1)
        farrayPtr(i,j) = 10.0d0 &
          + 5.0d0 * sin(real(i,ESMF_KIND_R8)/100.d0*pi) &
          + 2.0d0 * sin(real(j,ESMF_KIND_R8)/150.d0*pi)
      enddo
    enddo
 
  end subroutine user1_run


end module user_model1
    
!\end{verbatim}
