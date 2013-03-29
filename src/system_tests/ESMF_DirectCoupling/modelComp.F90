! $Id$
!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

module modelCompMod

  ! ESMF Framework module
  use ESMF

  ! Model components
  use modelACompMod,     only : modelACompSetVM, modelACompReg
  use modelBCompMod,     only : modelBCompSetVM, modelBCompReg

  implicit none

  public modelCompSetVM, modelCompReg

  ! internal module wide objects
  type(ESMF_GridComp), save :: modelAComp, modelBComp
  type(ESMF_State), save :: modelAExp, modelBImp

!-------------------------------------------------------------------------

  contains

!-------------------------------------------------------------------------

  subroutine modelCompSetVM(comp, rc)
    type(ESMF_GridComp) :: comp
    integer, intent(out) :: rc
#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm
    logical :: pthreadsEnabled
#endif

    ! Initialize
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
      call ESMF_GridCompSetVMMinThreads(comp, rc=rc)
    endif
#endif

  end subroutine

  subroutine modelCompReg(comp, rc)
    type(ESMF_GridComp) :: comp
    integer, intent(out) :: rc

    ! Initialize
    rc = ESMF_SUCCESS

    ! Register Init, Run, Finalize
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, userRoutine=compInit, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_RUN, userRoutine=compRun, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, userRoutine=compFinal, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine

!-------------------------------------------------------------------------

  subroutine compInit(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_VM)           :: vm
    integer                 :: petCount, userrc
    type(ESMF_Array)        :: arraySrc, arrayDst
    type(ESMF_RouteHandle)  :: routehandle

    ! Initialize
    rc = ESMF_SUCCESS

    ! Determine petCount
    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMGet(vm, petCount=petCount, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Check petCount -> component expects to run on 5 PETs
    if (petCount/=5) then
      rc=ESMF_FAILURE
      return  ! bail out with failure
    endif

    ! Create modelAComp on PET 0,3
    modelAComp = ESMF_GridCompCreate(name="modelAComp", petList=(/0,3/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Create modelBComp on PET 1,2,4
    modelBComp = ESMF_GridCompCreate(name="modelBComp", petList=(/1,2,4/), &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! SetServices for modelAComp
    call ESMF_GridCompSetVM(modelAComp, userRoutine=modelACompSetVM, &
        userRc=userrc, rc=rc)
    if ((rc/=ESMF_SUCCESS) .or. (userrc/=ESMF_SUCCESS)) return ! bail out
    call ESMF_GridCompSetServices(modelAComp, userRoutine=modelACompReg, &
        userRc=userrc, rc=rc)
    if ((rc/=ESMF_SUCCESS) .or. (userrc/=ESMF_SUCCESS)) return ! bail out

    ! SetServices for modelBComp
    call ESMF_GridCompSetVM(modelBComp, userRoutine=modelBCompSetVM, &
        userRc=userrc, rc=rc)
    if ((rc/=ESMF_SUCCESS) .or. (userrc/=ESMF_SUCCESS)) return ! bail out
    call ESMF_GridCompSetServices(modelBComp, userRoutine=modelBCompReg, &
        userRc=userrc, rc=rc)
    if ((rc/=ESMF_SUCCESS) .or. (userrc/=ESMF_SUCCESS)) return ! bail out

    ! Create State and initialize modelAComp
    modelAExp = ESMF_StateCreate(name="modelAComp export",  &
                                 stateintent=ESMF_STATEINTENT_EXPORT, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompInitialize(modelAComp, importState=importState, &
      exportState=modelAExp, userRc=userrc, rc=rc)
    if ((rc/=ESMF_SUCCESS) .or. (userrc/=ESMF_SUCCESS)) return ! bail out

    ! Create State and initialize modelBComp
    modelBImp = ESMF_StateCreate(name="modelBComp import",  &
                                 stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompInitialize(modelBComp, importState=modelBImp, &
      exportState=exportState, userRc=userrc, rc=rc)
    if ((rc/=ESMF_SUCCESS) .or. (userrc/=ESMF_SUCCESS)) return ! bail out

    ! Reconcile module wide import and export States
    call ESMF_StateReconcile(modelAExp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateReconcile(modelBImp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Get access to src and dst Arrays in States
    call ESMF_StateGet(modelAExp, "modelA.array", arraySrc, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateGet(modelBImp, "modelB.array", arrayDst, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Precompute and store ArrayRedist: arraySrc -> arrayDst
    call ESMF_ArrayRedistStore(srcArray=arraySrc, dstArray=arrayDst, &
      routehandle=routehandle, factor=-2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Give a name to RouteHandle
    call ESMF_RouteHandleSet(routehandle, name="modelA2BRedist", rc=rc )
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Add RouteHandle to import and export State for direct coupling
    call ESMF_StateAdd(modelAExp, (/routehandle/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateAdd(modelBImp, (/routehandle/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine

!-------------------------------------------------------------------------

  subroutine compRun(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    integer :: userrc
    ! Initialize
    rc = ESMF_SUCCESS

    ! Run modelAComp and modelBComp concurrently -> direct coupling
    call ESMF_GridCompRun(modelAComp, importState=importState, &
      exportState=modelAExp, userRc=userrc, rc=rc)
    if ((rc/=ESMF_SUCCESS) .or. (userrc/=ESMF_SUCCESS)) return ! bail out
    call ESMF_GridCompRun(modelBComp, importState=modelBImp, &
      exportState=exportState, userRc=userrc, rc=rc)
    if ((rc/=ESMF_SUCCESS) .or. (userrc/=ESMF_SUCCESS)) return ! bail out

  end subroutine

!-------------------------------------------------------------------------

  subroutine compFinal(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_RouteHandle)  :: routehandle

    ! Initialize
    rc = ESMF_SUCCESS

    ! Finalize modelAComp
    call ESMF_GridCompFinalize(modelAComp, importState=importState, &
      exportState=modelAExp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Finalize modelBComp
    call ESMF_GridCompFinalize(modelBComp, importState=modelBImp, &
      exportState=exportState, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Get access to the RouteHandle and release
    call ESMF_StateGet(modelAExp, "modelA2BRedist", routehandle, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArrayRedistRelease(routehandle=routehandle, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Destroy both internal model Components
    call ESMF_GridCompDestroy(modelAComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompDestroy(modelBComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Destroy both internal model States
    call ESMF_StateDestroy(modelAExp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateDestroy(modelBImp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine

!-------------------------------------------------------------------------

end module modelCompMod

