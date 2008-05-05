! $Id: component.F90,v 1.1.2.7 2008/05/05 18:45:29 theurich Exp $
!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

module componentMod

  ! ESMF Framework module
  use ESMF_Mod

  implicit none
    
  public componentReg
        
  type myComponents
    type(ESMF_GridComp) :: component1, component2
  end type

  type myComponentsWrapper
    type(myComponents), pointer :: wrap
  end type
    
!-------------------------------------------------------------------------

  contains

!-------------------------------------------------------------------------

  subroutine componentReg(comp, rc)
    type(ESMF_GridComp) :: comp
    integer, intent(out) :: rc
#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm
    type(ESMF_Logical) :: supportPthreads
#endif

    ! Initialize
    rc = ESMF_SUCCESS

    ! Register Init, Run, Finalize
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, compInit, &
      ESMF_SINGLEPHASE, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, compRun, &
      ESMF_SINGLEPHASE, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, compFinal, &
      ESMF_SINGLEPHASE, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

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

  end subroutine

!-------------------------------------------------------------------------
    
  recursive subroutine compInit(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    
    ! Local variables
    type(ESMF_VM)           :: vm
    integer                 :: petCount
    type(ESMF_GridComp)     :: component1, component2
    type(myComponents), pointer :: myComps
    type(myComponentsWrapper) :: myCompsWrapper
    
    ! Initialize
    rc = ESMF_SUCCESS

    ! Determine petCount
    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMGet(vm, petCount=petCount, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Do local initialization work (may or may not depend on petCount)
    
    ! Depending on petCount recursively create subcomponents
    if (petCount==6) then
      ! Create components and SetServices
      component1 = ESMF_GridCompCreate(name="component012", petList=(/0,1,2/), &
        rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
      call ESMF_GridCompSetServices(component1, componentReg, rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
      component2 = ESMF_GridCompCreate(name="component345", petList=(/3,4,5/), &
        rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
      call ESMF_GridCompSetServices(component2, componentReg, rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
      ! Initialize component concurrently
      call ESMF_GridCompInitialize(component1, importState=importState, &
        exportState=exportState, rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
      call ESMF_GridCompInitialize(component2, importState=importState, &
        exportState=exportState, rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
      
      ! Set newly created components in internal State
      allocate(myComps)
      myComps%component1=component1
      myComps%component2=component2
      myCompsWrapper%wrap => myComps
      call ESMF_GridCompSetInternalState(comp, myCompsWrapper, rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
      
    endif
    if (petCount==3) then
      ! Create components and SetServices
      component1 = ESMF_GridCompCreate(name="component0", petList=(/0/), &
        rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
      call ESMF_GridCompSetServices(component1, componentReg, rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
      component2 = ESMF_GridCompCreate(name="component12", petList=(/1,2/), &
        rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
      call ESMF_GridCompSetServices(component2, componentReg, rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
      ! Initialize component concurrently
      call ESMF_GridCompInitialize(component1, importState=importState, &
        exportState=exportState, rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
      call ESMF_GridCompInitialize(component2, importState=importState, &
        exportState=exportState, rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out

      ! Set newly created components in internal State
      allocate(myComps)
      myComps%component1=component1
      myComps%component2=component2
      myCompsWrapper%wrap => myComps
      call ESMF_GridCompSetInternalState(comp, myCompsWrapper, rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
      
    endif
    
  end subroutine

!-------------------------------------------------------------------------
 
  recursive subroutine compRun(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_VM)           :: vm
    integer                 :: petCount
    type(ESMF_GridComp)     :: component1, component2
    type(myComponents), pointer :: myComps
    type(myComponentsWrapper) :: myCompsWrapper
    
    ! Initialize
    rc = ESMF_SUCCESS

    ! Determine petCount
    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMGet(vm, petCount=petCount, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    print *, "Run() method before recursive call"
    call ESMF_GridCompPrint(comp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Depending on petCount recursively call subcomponents Run() methods
    if (petCount==6 .or. petCount==3) then
      ! Get sub components from internal State
      nullify(myCompsWrapper%wrap)
      call ESMF_GridCompGetInternalState(comp, myCompsWrapper, rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
      ! Get a local copy of the component objects
      myComps => myCompsWrapper%wrap
      component1 = myComps%component1
      component2 = myComps%component2
      ! Recursive Run()
      call ESMF_GridCompRun(component1, importState=importState, &
        exportState=exportState, rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
      call ESMF_GridCompRun(component2, importState=importState, &
        exportState=exportState, rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
    endif
        
    print *, "Run() method after recursive call"
    call ESMF_GridCompPrint(comp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine

!-------------------------------------------------------------------------
 
  recursive subroutine compFinal(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_VM)           :: vm
    integer                 :: petCount
    type(ESMF_GridComp)     :: component1, component2
    type(myComponents), pointer :: myComps
    type(myComponentsWrapper) :: myCompsWrapper
    
    ! Initialize
    rc = ESMF_SUCCESS

    ! Determine petCount
    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMGet(vm, petCount=petCount, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Do local finalization work (may or may not depend on petCount)
    
    ! Depending on petCount recursively delete subcomponents
    if (petCount==6 .or. petCount==3) then
      ! Get sub components from internal State
      nullify(myCompsWrapper%wrap)
      call ESMF_GridCompGetInternalState(comp, myCompsWrapper, rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
      ! Get a local copy of the component objects
      myComps => myCompsWrapper%wrap
      component1 = myComps%component1
      component2 = myComps%component2
      ! Recursive Finalize()
      call ESMF_GridCompFinalize(component1, importState=importState, &
        exportState=exportState, rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
      call ESMF_GridCompFinalize(component2, importState=importState, &
        exportState=exportState, rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
      ! Destroy subcomponents
      call ESMF_GridCompDestroy(component1, rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
      call ESMF_GridCompDestroy(component2, rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
      ! Deallocate data structure that was stored in internal state
      deallocate(myComps)
    endif
        
  end subroutine

!-------------------------------------------------------------------------
 
end module componentMod
    
