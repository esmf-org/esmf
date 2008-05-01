! $Id: user_model1.F90,v 1.20 2008/05/01 22:26:03 rokuingh Exp $
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

!-------------------------------------------------------------------------
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
    print *, 'FAILURE in Comp 1 Init!!!!'
    rc = ESMF_FAILURE

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
    type(ESMF_State)            :: MyState, physics
    type(ESMF_VM)               :: vm
    type(ESMF_DistGrid)         :: distgrid
    type(ESMF_ArraySpec)        :: arrayspec
    type(ESMF_Array)            :: array
    type(ESMF_Field)            :: uwind, ozone, co, tke, wt, field
    type(ESMF_FieldBundle)      :: bundle, tracers, turbulence
    type(ESMF_Grid)             :: grid
    integer                     :: petCount, status, myPet
    character(len=ESMF_MAXSTR)  :: name, value, conv, purp, fconv, fpurp, sname, svalue, outvalue
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Determine petCount
    call ESMF_GridCompGet(comp, vm=vm, rc=status)
    if (status .ne. ESMF_SUCCESS) return
    call ESMF_VMGet(vm, petCount=petCount, localPet=myPet, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    ! Get the export state
    call ESMF_StateGet(exportState, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  !!! Arlindo's State Pretty Print example
  !!!
  !!!
      if (myPet .eq. 0) then
        print *, "--------------------------------------- "
        print *, 'Start of State Pretty Print example'
        print *, "--------------------------------------- "
      endif
      
      sname = 'name'
      svalue = 'My Nice State'
 
      MyState = ESMF_StateCreate("MyState", ESMF_STATE_IMPORT, rc=status)
      if (status .ne. ESMF_SUCCESS) return

      call ESMF_AttributeSet(MyState, sname, svalue, rc=status)
      if (status .ne. ESMF_SUCCESS) return

      physics = ESMF_StateCreate("physics", ESMF_STATE_IMPORT, rc=status)
      if (status .ne. ESMF_SUCCESS) return
      
      ! field stuff
      fconv = 'netCDF'
      fpurp = 'basic'
      name = 'longname'
      value = 'zonal wind'
      
      uwind = ESMF_FieldCreate("uwind", rc=status)
      if (status .ne. ESMF_SUCCESS) return
      call ESMF_AttributeAdd(uwind, convention=fconv, purpose=fpurp, rc=status)
      if (status .ne. ESMF_SUCCESS) return
      call ESMF_AttributeSet(uwind, name, value, convention=fconv, purpose=fpurp, rc=status)
      if (status .ne. ESMF_SUCCESS) return
      
      value = 'ozone'
      ozone = ESMF_FieldCreate("ozone", rc=status)
      if (status .ne. ESMF_SUCCESS) return
      call ESMF_AttributeAdd(ozone, convention=fconv, purpose=fpurp, rc=status)
      if (status .ne. ESMF_SUCCESS) return
      call ESMF_AttributeSet(ozone, name, value, convention=fconv, purpose=fpurp, rc=status)
      if (status .ne. ESMF_SUCCESS) return
      
      value = 'carbon monoxide'
      co = ESMF_FieldCreate("co", rc=status)
      if (status .ne. ESMF_SUCCESS) return
      call ESMF_AttributeAdd(co, convention=fconv, purpose=fpurp, rc=status)
      if (status .ne. ESMF_SUCCESS) return
      call ESMF_AttributeSet(co, name, value, convention=fconv, purpose=fpurp, rc=status)
      if (status .ne. ESMF_SUCCESS) return
      
      value = 'turbulent kinetic energy'
      tke = ESMF_FieldCreate("tke", rc=status)
      if (status .ne. ESMF_SUCCESS) return
      call ESMF_AttributeAdd(tke, convention=fconv, purpose=fpurp, rc=status)
      if (status .ne. ESMF_SUCCESS) return
      call ESMF_AttributeSet(tke, name, value, convention=fconv, purpose=fpurp, rc=status)
      if (status .ne. ESMF_SUCCESS) return
      
      value = 'heat fluxes'
      wt = ESMF_FieldCreate("wt", rc=status)
      if (status .ne. ESMF_SUCCESS) return
      call ESMF_AttributeAdd(wt, convention=fconv, purpose=fpurp, rc=status)
      if (status .ne. ESMF_SUCCESS) return
      call ESMF_AttributeSet(wt, name, value, convention=fconv, purpose=fpurp, rc=status)
      if (status .ne. ESMF_SUCCESS) return

      ! bundle stuff
      tracers = ESMF_FieldBundleCreate(name="tracers", rc=status)
      if (status .ne. ESMF_SUCCESS) return
      
      turbulence = ESMF_FieldBundleCreate(name="turbulence", rc=status)
      if (status .ne. ESMF_SUCCESS) return
      
      ! connect the attribute hierarchy
      call ESMF_AttributeSet(turbulence, tke, rc=status)
      if (status .ne. ESMF_SUCCESS) return
      call ESMF_AttributeSet(turbulence, wt, rc=status)
      if (status .ne. ESMF_SUCCESS) return

      call ESMF_AttributeSet(tracers, ozone, rc=status)
      if (status .ne. ESMF_SUCCESS) return
      call ESMF_AttributeSet(tracers, co, rc=status)
      if (status .ne. ESMF_SUCCESS) return

      call ESMF_AttributeSet(physics, turbulence, rc=status)
      if (status .ne. ESMF_SUCCESS) return

      call ESMF_AttributeSet(MyState, uwind, rc=status)
      if (status .ne. ESMF_SUCCESS) return
      call ESMF_AttributeSet(MyState, tracers, rc=status)
      if (status .ne. ESMF_SUCCESS) return
      call ESMF_AttributeSet(MyState, physics, rc=status)
      if (status .ne. ESMF_SUCCESS) return
 
      if (myPet .eq. 0) then
        !call ESMF_StatePrint(MyState, rc=status)
        if (status .ne. ESMF_SUCCESS) return
        print *, "--------------------------------------- "
        print *, 'End of State Pretty Print example'
        print *, "--------------------------------------- "
      endif
  !!!
  !!!
  !!! Arlindo's State Pretty Print example

    ! copy all attribute information into export state
    call ESMF_AttributeCopy(MyState, exportState, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_FieldDestroy(uwind, rc=rc)
    call ESMF_FieldDestroy(ozone, rc=rc)
    call ESMF_FieldDestroy(co, rc=rc)
    call ESMF_FieldDestroy(tke, rc=rc)
    call ESMF_FieldDestroy(wt, rc=rc)
    call ESMF_FieldBundleDestroy(tracers, rc=rc)
    call ESMF_FieldBundleDestroy(turbulence, rc=rc)
    call ESMF_StateDestroy(physics, rc=rc)
    call ESMF_StateDestroy(MyState, rc=rc)

    rc = ESMF_SUCCESS
    return
                                                             
    ! get here only on error exit
20  continue
    rc = ESMF_FAILURE
    print *, "FAILURE in COMP1 Run!!!"
    return

  end subroutine user_run

!-------------------------------------------------------------------------
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
    print *, "FAILURE in COMP1 Finalize!!!"
    return

  end subroutine user_final


end module user_model1
    
!\end{verbatim}
