! $Id$
!
! Example/test code which shows User Component calls.

!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

!
! !DESCRIPTION:
!  User-supplied Component
!
!
!\begin{verbatim}

module user_model2

  ! ESMF Framework module
  use ESMF

  implicit none
    
  public userm2_setvm, userm2_register
        
  contains

!--------------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
  subroutine userm2_setvm(comp, rc)
    type(ESMF_GridComp)   :: comp
    integer, intent(out)  :: rc
    
#ifdef ESMF_TESTWITHTHREADS_disabled
    type(ESMF_VM) :: vm
    logical       :: pthreadsEnabled
#endif

    ! Initialize user return code
    rc = ESMF_SUCCESS

#ifdef ESMF_TESTWITHTHREADS_disabled
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
      call ESMF_GridCompSetVMMinThreads(comp, rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
    endif
#endif

  end subroutine

  subroutine userm2_register(comp, rc)

!    use complianceIC  !!!!!!!!!!!!!!!!!  I C  !!!!!!!!!!!!!!!!!

    type(ESMF_GridComp)   :: comp
    integer, intent(out)  :: rc
    

    ! Local variables
    integer               :: localPet

    ! Initialize user return code
    rc = ESMF_SUCCESS

    call ESMF_GridCompGet(comp, localPet=localPet, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print *, localPet, "User Comp2 Register starting"

    ! Register the callback routines.

    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, userRoutine=user_init, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_RUN, userRoutine=user_run, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
!    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, userRoutine=user_final, &
!      rc=rc)
!    if (rc/=ESMF_SUCCESS) return ! bail out
    
    
!    call registerIC(comp, rc=rc) !!!!!!!!!!!!!!!!!  I C  !!!!!!!!!!!!!!!!!
!    if (rc/=ESMF_SUCCESS) return ! bail out
    

    print *, localPet, "User Comp2 Register returning"
    
  end subroutine

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
  subroutine user_init(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables
    integer               :: localPet
	type(ESMF_AttPack)    :: attpack
    type(ESMF_Field)      :: field
    type(ESMF_FieldBundle):: fieldbundle
    type(ESMF_Clock)      :: clockInternal
    
    ! Initialize user return code
    rc = ESMF_SUCCESS

    call ESMF_GridCompGet(comp, localPet=localPet, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print *, localPet, "User Comp2 Init starting"

    call ESMF_AttributeAdd(comp, "CIM 1.5", "ModelComp", &
      attpack=attpack, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    call ESMF_AttributeSet(comp, name="ShortName", value="ABC", &
      attpack=attpack, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
   
    field = ESMF_FieldEmptyCreate(name="myTestField", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
   
    call ESMF_StateAdd(exportState, (/field/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    fieldbundle = ESMF_FieldBundleCreate(name="myTestFieldBundle", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_StateAdd(exportState, (/fieldbundle/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    field = ESMF_FieldEmptyCreate(name="myTestField 1 in Bundle", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
   
    call ESMF_FieldBundleAdd(fieldbundle, (/field/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    field = ESMF_FieldEmptyCreate(name="myTestField 2 in Bundle", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_FieldBundleAdd(fieldbundle, (/field/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    clockInternal = ESMF_ClockCreate(clock, rc=rc)  ! make a copy of incoming
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    call ESMF_GridCompSet(comp, clock=clockInternal, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print *, localPet, "User Comp2 Init returning"

  end subroutine user_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
  subroutine user_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables
    integer               :: localPet
    
    ! Initialize user return code
    rc = ESMF_SUCCESS

    call ESMF_GridCompGet(comp, localPet=localPet, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print *, localPet, "User Comp2 Run starting"

    print *, localPet, "User Comp2 Run returning"

  end subroutine user_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
  subroutine user_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables
    integer               :: localPet
    
    ! Initialize user return code
    rc = ESMF_SUCCESS

    call ESMF_GridCompGet(comp, localPet=localPet, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print *, localPet, "User Comp2 Final starting"

    print *, localPet, "User Comp2 Final returning"

  end subroutine user_final

end module user_model2
!\end{verbatim}
