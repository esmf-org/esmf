! $Id: modelBComp.F90,v 1.1.2.4 2008/02/12 00:26:33 theurich Exp $
!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

module modelBCompMod

  ! ESMF Framework module
  use ESMF_Mod

  implicit none
    
  public modelBCompReg
        
!-------------------------------------------------------------------------

  contains

!-------------------------------------------------------------------------

  subroutine modelBCompReg(comp, rc)
    type(ESMF_GridComp), intent(inout) :: comp
    integer, intent(out) :: rc

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
    call ESMF_GridCompSetVMMinThreads(comp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
#endif

  end subroutine

!-------------------------------------------------------------------------
    
  subroutine compInit(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp), intent(inout) :: comp
    type(ESMF_State), intent(inout) :: importState, exportState
    type(ESMF_Clock), intent(in) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_ArraySpec)  :: arrayspec
    type(ESMF_DistGrid)   :: distgrid
    type(ESMF_Array)      :: array
    type(ESMF_VM)         :: vm
    integer               :: petCount
    
    ! Initialize
    rc = ESMF_SUCCESS

    ! Determine petCount
    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMGet(vm, petCount=petCount, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Create the Array and add it to the import and export State
    call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/100,150/), &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
      indexflag=ESMF_INDEX_GLOBAL, name="modelB.array", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateAddArray(importState, array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateAddArray(exportState, array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
      
  end subroutine

!-------------------------------------------------------------------------
 
  subroutine compRun(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp), intent(inout) :: comp
    type(ESMF_State), intent(inout) :: importState, exportState
    type(ESMF_Clock), intent(in) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_Array)        :: array
    type(ESMF_RouteHandle)  :: modelA2BRedist, model2ioRedist
    integer                 :: n
    
    ! Initialize
    rc = ESMF_SUCCESS

    ! Get the Array from the import State
    call ESMF_StateGetArray(importState, "modelB.array", array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Gain access to RouteHandles for direct coupling to modelAComp and ioComp
    call ESMF_StateGetRouteHandle(importState, "modelA2BRedist", &
      routehandle=modelA2BRedist, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateGetRouteHandle(exportState, "model2ioRedist", &
      routehandle=model2ioRedist, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Main Run Loop - with direct coupling to modelAComp and  ioComp
    do n=1, 3
      
      ! ArrayRedist() "receive" from modelAComp
      call ESMF_ArrayRedist(dstArray=array, routehandle=modelA2BRedist, rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
      
      ! -> do something with array here
      
      ! ArrayRedist() "send" to ioComp
      call ESMF_ArrayRedist(srcArray=array, routehandle=model2ioRedist, rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
      
    enddo
 
  end subroutine

!-------------------------------------------------------------------------
 
  subroutine compFinal(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp), intent(inout) :: comp
    type(ESMF_State), intent(inout) :: importState, exportState
    type(ESMF_Clock), intent(in) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_DistGrid)   :: distgrid
    type(ESMF_Array)      :: array

    ! Initialize
    rc = ESMF_SUCCESS
    
    ! Garbage collection of objects explicitly created in this component
    call ESMF_StateGetArray(importState, "modelB.array", array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArrayGet(array, distgrid=distgrid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArrayDestroy(array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_DistGridDestroy(distgrid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine

!-------------------------------------------------------------------------
 
end module modelBCompMod
    
