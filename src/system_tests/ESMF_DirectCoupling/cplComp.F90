! $Id: cplComp.F90,v 1.1.2.6 2008/05/06 04:31:45 cdeluca Exp $
!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

module cplCompMod

  ! ESMF Framework module
  use ESMF_Mod

  implicit none
    
  public cplCompReg
        
!-------------------------------------------------------------------------

  contains

!-------------------------------------------------------------------------

  subroutine cplCompReg(comp, rc)
    type(ESMF_CplComp) :: comp
    integer, intent(out) :: rc
#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm
    type(ESMF_Logical) :: supportPthreads
#endif

    ! Initialize
    rc = ESMF_SUCCESS

    ! Register Init, Finalize (this component does not provide Run)
    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETINIT, compInit1, 1, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETINIT, compInit2, 2, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETFINAL, compFinal1, 1, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETFINAL, compFinal2, 2, rc)
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
      call ESMF_CplCompSetVMMinThreads(comp, rc=rc)
    endif
#endif

  end subroutine

!-------------------------------------------------------------------------
    
  subroutine compInit1(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    
    ! Local variables
    type(ESMF_VM)           :: vm
    type(ESMF_Array)        :: arraySrc, arrayDst
    type(ESMF_RouteHandle)  :: routehandle
    
    ! Initialize
    rc = ESMF_SUCCESS

    ! Reconcile import and export States
    call ESMF_CplCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateReconcile(importState, vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateReconcile(exportState, vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Get access to src and dst Arrays in States
    call ESMF_StateGet(importState, "ioComp.arraySrc", arraySrc, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateGet(exportState, "modelA.array", arrayDst, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Precompute and store ArrayRedist: arraySrc -> arrayDst
    call ESMF_ArrayRedistStore(srcArray=arraySrc, dstArray=arrayDst, &
      routehandle=routehandle, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Give a name to RouteHandle
    call ESMF_RouteHandleSet(routehandle, name="io2modelRedist", rc=rc )
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Add RouteHandle to import and export State for direct coupling
    call ESMF_StateAdd(importState, routehandle, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateAdd(exportState, routehandle, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
  end subroutine

!-------------------------------------------------------------------------
 
  subroutine compInit2(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    
    ! Local variables
    type(ESMF_VM)           :: vm
    type(ESMF_Array)        :: arraySrc, arrayDst
    type(ESMF_RouteHandle)  :: routehandle
    
    ! Initialize
    rc = ESMF_SUCCESS

    ! Reconcile import and export States
    call ESMF_CplCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateReconcile(importState, vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateReconcile(exportState, vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Get access to src and dst Arrays in States
    call ESMF_StateGet(importState, "modelB.array", arraySrc, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateGet(exportState, "ioComp.arrayDst", arrayDst, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Precompute and store ArrayRedist: arraySrc -> arrayDst
    call ESMF_ArrayRedistStore(srcArray=arraySrc, dstArray=arrayDst, &
      routehandle=routehandle, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Give a name to RouteHandle
    call ESMF_RouteHandleSet(routehandle, name="model2ioRedist", rc=rc )
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Add RouteHandle to import and export State for direct coupling
    call ESMF_StateAdd(importState, routehandle, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateAdd(exportState, routehandle, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
  end subroutine

!-------------------------------------------------------------------------
 
  subroutine compFinal1(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_RouteHandle)  :: routehandle
    
    ! Initialize
    rc = ESMF_SUCCESS
    
    ! Get access to the RouteHandle and release
    call ESMF_StateGet(importState, "io2modelRedist", routehandle, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArrayRedistRelease(routehandle=routehandle, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine

!-------------------------------------------------------------------------
 
  subroutine compFinal2(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_RouteHandle)  :: routehandle

    ! Initialize
    rc = ESMF_SUCCESS
    
    ! Get access to the RouteHandle and destroy
    call ESMF_StateGet(importState, "model2ioRedist", routehandle, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArrayRedistRelease(routehandle=routehandle, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine

!-------------------------------------------------------------------------
 
end module cplCompMod
    
