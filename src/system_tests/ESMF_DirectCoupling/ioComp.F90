! $Id: ioComp.F90,v 1.1.2.8 2008/05/06 04:31:45 cdeluca Exp $
!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

module ioCompMod

  ! ESMF Framework module
  use ESMF_Mod

  implicit none
    
  public ioCompReg
        
!-------------------------------------------------------------------------

  contains

!-------------------------------------------------------------------------

  subroutine ioCompReg(comp, rc)
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
    
  subroutine compInit(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_ArraySpec)  :: arrayspec
    type(ESMF_DistGrid)   :: distgrid
    type(ESMF_Array)      :: arraySrc, arrayDst
    type(ESMF_VM)         :: vm
    integer               :: petCount
    
    ! Initialize
    rc = ESMF_SUCCESS

    ! Determine petCount
    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMGet(vm, petCount=petCount, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Create the source Array and add it to the export State
    call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R4, rank=2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/100,150/), &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    arraySrc = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
      indexflag=ESMF_INDEX_GLOBAL, name="ioComp.arraySrc", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateAdd(exportState, arraySrc, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
   
    ! Create the destination Array and add it to the import State
    arrayDst = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
      indexflag=ESMF_INDEX_GLOBAL, name="ioComp.arrayDst", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateAdd(importState, arrayDst, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
   
  end subroutine

!-------------------------------------------------------------------------
 
  subroutine compRun(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    real(ESMF_KIND_R4)    :: pi
    type(ESMF_Array)      :: arraySrc, arrayDst
    real(ESMF_KIND_R4), pointer :: farraySrcPtr(:,:), farrayDstPtr(:,:)
    integer               :: i, j, n
    real(ESMF_KIND_R4)    :: result
    type(ESMF_RouteHandle)  :: io2modelRedist, model2ioRedist
    
    ! Initialize
    rc = ESMF_SUCCESS
    pi = 3.14159d0

    ! Get the source Array from the export State
    call ESMF_StateGet(exportState, "ioComp.arraySrc", arraySrc, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Gain access to actual data via F90 array pointer
    call ESMF_ArrayGet(arraySrc, localDe=0, farrayPtr=farraySrcPtr, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Fill source Array with data
    do j = lbound(farraySrcPtr, 2), ubound(farraySrcPtr, 2)
      do i = lbound(farraySrcPtr, 1), ubound(farraySrcPtr, 1)
        farraySrcPtr(i,j) = real(10.0d0 &
          + 5.0d0 * sin(real(i,ESMF_KIND_R4)/100.d0*pi) &
          + 2.0d0 * sin(real(j,ESMF_KIND_R4)/150.d0*pi), ESMF_KIND_R4)
      enddo
    enddo
    
    ! Get the destination Array from the import State
    call ESMF_StateGet(importState, "ioComp.arrayDst", arrayDst, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Gain access to actual data via F90 array pointer
    call ESMF_ArrayGet(arrayDst, localDe=0, farrayPtr=farrayDstPtr, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Initialize destination Array
    farrayDstPtr = real(0.,ESMF_KIND_R4)

    ! Gain access to RouteHandles for direct coupling to modelA and modelB
    call ESMF_StateGet(exportState, "io2modelRedist", &
      routehandle=io2modelRedist, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateGet(importState, "model2ioRedist", &
      routehandle=model2ioRedist, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Main Run Loop - with direct coupling to modelAComp and modelBComp
    do n=1, 3
    
      ! ArrayRedist() "send" to modelAComp
      call ESMF_ArrayRedist(srcArray=arraySrc, routehandle=io2modelRedist, &
        rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
      
      ! ArrayRedist() "receive" from modelBComp
      call ESMF_ArrayRedist(dstArray=arrayDst, routehandle=model2ioRedist, &
        rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
      
      ! Copy received data into the source array for next iteration
      farraySrcPtr = farrayDstPtr
      
    enddo
 
    ! Check if result in arrayDst matches analytical result
    do j = lbound(farraySrcPtr, 2), ubound(farraySrcPtr, 2)
      do i = lbound(farraySrcPtr, 1), ubound(farraySrcPtr, 1)
        result = real(-8. * (10.0d0 &
          + 5.0d0 * sin(real(i,ESMF_KIND_R4)/100.d0*pi) &
          + 2.0d0 * sin(real(j,ESMF_KIND_R4)/150.d0*pi)), ESMF_KIND_R4)
        if (abs(result-farrayDstPtr(i,j))>real(1.d-5,ESMF_KIND_R4)) then
          print *, "Mismatch in results detected: ", i, j, result, &
            farrayDstPtr(i,j), result-farrayDstPtr(i,j)
          rc = ESMF_FAILURE
        endif
      enddo
    enddo
    
    if (rc == ESMF_SUCCESS) then
      print *, "Validation of results successful! :-)"
    endif
      
  end subroutine

!-------------------------------------------------------------------------
 
  subroutine compFinal(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_DistGrid)   :: distgrid
    type(ESMF_Array)      :: arraySrc, arrayDst

    ! Initialize
    rc = ESMF_SUCCESS
    
    ! Garbage collection of objects explicitly created in this component
    call ESMF_StateGet(exportState, "ioComp.arraySrc", arraySrc, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArrayDestroy(arraySrc, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out    
    call ESMF_StateGet(importState, "ioComp.arrayDst", arrayDst, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArrayGet(arrayDst, distgrid=distgrid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArrayDestroy(arrayDst, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_DistGridDestroy(distgrid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
   
  end subroutine

!-------------------------------------------------------------------------
 
end module ioCompMod
    
