! $Id: user_model1.F90,v 1.13 2008/02/13 01:49:42 rokuingh Exp $
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
    type(ESMF_Field)            :: uwind, ozone, co, tke, wt
    type(ESMF_Bundle)           :: bundle, tracers, turbulence
    type(ESMF_Grid)             :: grid
    integer                     :: petCount, backward_run, status, myPet
    character(len=ESMF_MAXSTR)  :: name, value, conv, purp, fconv, fpurp, sname, svalue
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Determine petCount
    call ESMF_GridCompGet(comp, vm=vm, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 20
    call ESMF_VMGet(vm, petCount=petCount, localPet=myPet, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 20

    ! Get the direction of the run
    call ESMF_StateAttributeGetInfo(importState, "backward_run", rc=backward_run)
    if (status .ne. ESMF_SUCCESS) goto 20

    ! if this is the forward interpolation
    if (backward_run .ne. ESMF_SUCCESS) then
      conv = 'ESG-CDP'  
      purp = 'general'
      name = 'name'
      value = 'State attribute package'
      call ESMF_StateAttPackSet(importState, name, value, convention=conv, purpose=purp, rc=rc)
      if (status .ne. ESMF_SUCCESS) goto 20
      
      if (myPet .eq. 0) then
        print *, 'Write the Attpack from the first run of component 1.'
    
        call ESMF_StateAttPackWrite(importState, convention=conv, purpose=purp, rc=rc)
        if (status .ne. ESMF_SUCCESS) goto 20
      endif
    endif

    ! if this is the backward interpolation check error
    if (backward_run .eq. ESMF_SUCCESS) then
      ! state stuff
      conv = 'ESG-CDP'  
      purp = 'general'
      name = 'discipline'
      value = 'Sir, Yes! Sir.'
      call ESMF_StateAttPackSet(importState, name, value, convention=conv, purpose=purp, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      
  !!! Arlindo's State Pretty Print example
  !!!
  !!!
      sname = 'name'
      svalue = 'My Nice State'
 
      MyState = ESMF_StateCreate("MyState", ESMF_STATE_IMPORT, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20

      call ESMF_StateAttributeSet(MyState, sname, svalue, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20

      physics = ESMF_StateCreate("physics", ESMF_STATE_IMPORT, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      
      ! field stuff
      fconv = 'netCDF'
      fpurp = 'basic'
      name = 'longname'
      value = 'zonal wind'
      
      uwind = ESMF_FieldCreateEmpty("uwind", rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      call ESMF_FieldAttPackCreate(uwind, convention=fconv, purpose=fpurp, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      call ESMF_FieldAttPackSet(uwind, name, value, convention=fconv, purpose=fpurp, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      
      value = 'ozone'
      ozone = ESMF_FieldCreateEmpty("ozone", rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      call ESMF_FieldAttPackCreate(ozone, convention=fconv, purpose=fpurp, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      call ESMF_FieldAttPackSet(ozone, name, value, convention=fconv, purpose=fpurp, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      
      value = 'carbon monoxide'
      co = ESMF_FieldCreateEmpty("co", rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      call ESMF_FieldAttPackCreate(co, convention=fconv, purpose=fpurp, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      call ESMF_FieldAttPackSet(co, name, value, convention=fconv, purpose=fpurp, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      
      value = 'turbulent kinetic energy'
      tke = ESMF_FieldCreateEmpty("tke", rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      call ESMF_FieldAttPackCreate(tke, convention=fconv, purpose=fpurp, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      call ESMF_FieldAttPackSet(tke, name, value, convention=fconv, purpose=fpurp, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      
      value = 'heat fluxes'
      wt = ESMF_FieldCreateEmpty("wt", rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      call ESMF_FieldAttPackCreate(wt, convention=fconv, purpose=fpurp, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      call ESMF_FieldAttPackSet(wt, name, value, convention=fconv, purpose=fpurp, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20

      ! bundle stuff
      tracers = ESMF_BundleCreate(name="tracers", rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      
      turbulence = ESMF_BundleCreate(name="turbulence", rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      
      ! connect the attribute hierarchy
      call ESMF_BundleAttributeSetLink(turbulence, tke, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      call ESMF_BundleAttributeSetLink(turbulence, wt, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20

      call ESMF_BundleAttributeSetLink(tracers, ozone, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      call ESMF_BundleAttributeSetLink(tracers, co, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20

      call ESMF_StateAttributeSetLink(physics, turbulence, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20

      call ESMF_StateAttributeSetLink(MyState, uwind, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      call ESMF_StateAttributeSetLink(MyState, tracers, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      call ESMF_StateAttributeSetLink(MyState, physics, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
 
      if (myPet .eq. 0) then
        call ESMF_StatePrint(MyState, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 20
      endif
  !!!
  !!!
  !!! Arlindo's State Pretty Print example
      
      ! bundle stuff
      bundle = ESMF_BundleCreate(name="bundle1", rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      call ESMF_BundleAttPackCreate(bundle, convention=fconv, purpose=fpurp, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      call ESMF_BundleAttPackSet(bundle, name, value, convention=fconv, purpose=fpurp, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20

      ! grid stuff
      grid = ESMF_GridCreateEmpty(rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      call ESMF_GridAttPackCreate(grid, convention=fconv, purpose=fpurp, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      call ESMF_GridAttPackSet(grid, name, value, convention=fconv, purpose=fpurp, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20

      ! array stuff
      call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
      distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/5,5/), &
        regDecomp=(/2,3/), rc=rc)
      array = ESMF_ArrayCreate(arrayspec, distgrid, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      call ESMF_ArrayAttPackCreate(array, convention=fconv, purpose=fpurp, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      call ESMF_ArrayAttPackSet(array, name, value, convention=fconv, purpose=fpurp, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20

      if (myPet .eq. 0) then
        conv = 'ESG-CDP'
        purp = 'general'
      
        print *, 'Write the State Attpack from the second run of component 1.'
        !call ESMF_StateAttPackWrite(importState, convention=conv, purpose=purp, rc=rc)
        if (status .ne. ESMF_SUCCESS) goto 20

        print *, 'Write the Bundle Attpack from the second run of component 1.'
        !call ESMF_BundleAttPackWrite(bundle, convention=fconv, purpose=fpurp, rc=rc)
        if (status .ne. ESMF_SUCCESS) goto 20

        print *, 'Write the Grid Attpack from the second run of component 1.'
        !call ESMF_GridAttPackWrite(grid, convention=fconv, purpose=fpurp, rc=rc)
        if (status .ne. ESMF_SUCCESS) goto 20
        
        print *, 'Write the Array Attpack from the second run of component 1.'
        !call ESMF_ArrayAttPackWrite(array, convention=fconv, purpose=fpurp, rc=rc)
        if (status .ne. ESMF_SUCCESS) goto 20
      endif
    endif

    call ESMF_FieldDestroy(uwind, rc=rc)
    call ESMF_FieldDestroy(ozone, rc=rc)
    call ESMF_FieldDestroy(co, rc=rc)
    call ESMF_FieldDestroy(tke, rc=rc)
    call ESMF_FieldDestroy(wt, rc=rc)
    call ESMF_BundleDestroy(tracers, rc=rc)
    call ESMF_BundleDestroy(turbulence, rc=rc)
    call ESMF_StateDestroy(physics, rc=rc)
    call ESMF_StateDestroy(MyState, rc=rc)

    call ESMF_ArrayDestroy(array, rc=rc)
    call ESMF_DistGridDestroy(distgrid, rc=rc)
    call ESMF_BundleDestroy(bundle, rc=rc)
    call ESMF_GridDestroy(grid, rc=rc)

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
