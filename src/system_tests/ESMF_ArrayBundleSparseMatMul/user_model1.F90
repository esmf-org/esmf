! $Id: user_model1.F90,v 1.1.2.5 2008/11/13 04:54:55 theurich Exp $
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
  use ESMF_Mod

  implicit none
    
  public userm1_register
        
  contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
  subroutine userm1_register(comp, rc)
    type(ESMF_GridComp) :: comp
    integer, intent(out) :: rc
#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm
    type(ESMF_Logical) :: supportPthreads
#endif

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Register starting"

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

    print *, "Registered Initialize, Run, and Finalize routines"

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

    print *, "User Comp1 Register returning"
    
  end subroutine

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
  subroutine user_init(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_ArraySpec)  :: arrayspec
    type(ESMF_DistGrid)   :: distgrid
    type(ESMF_Array)      :: array(3)
    type(ESMF_ArrayBundle):: arraybundle
    type(ESMF_VM)         :: vm
    integer               :: petCount
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Init starting"

    ! Determine petCount
    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMGet(vm, petCount=petCount, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Create the source Array and add it to the export State
    call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/100,150/), &
      regDecomp=(/petCount,1/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    array(1) = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
      indexflag=ESMF_INDEX_GLOBAL, name="array data 1", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    array(2) = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
      indexflag=ESMF_INDEX_GLOBAL, totalUWidth=(/1,0/), &
      name="array data 2", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    array(3) = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
      indexflag=ESMF_INDEX_GLOBAL, name="array data 3", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    arraybundle = ESMF_ArrayBundleCreate(arrayList=array, name="srcAryBndl", &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateAdd(exportState, arraybundle, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
   
    print *, "User Comp1 Init returning"

  end subroutine user_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
  subroutine user_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    real(ESMF_KIND_R8)    :: pi
    type(ESMF_Array)      :: array(3)
    type(ESMF_ArrayBundle):: arraybundle
    real(ESMF_KIND_R8), pointer :: farrayPtr1(:,:)   ! matching F90 array ptr
    real(ESMF_KIND_R8), pointer :: farrayPtr2(:,:)   ! matching F90 array ptr
    real(ESMF_KIND_R8), pointer :: farrayPtr3(:,:)   ! matching F90 array ptr
    integer               :: i, j
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Run starting"

    pi = 3.14159d0

    ! Get the source ArrayBundle from the export State
    call ESMF_StateGet(exportState, "srcAryBndl", arraybundle, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Get the source Arrays from the ArrayBundle
    call ESMF_ArrayBundleGet(arraybundle, arrayList=array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Gain access to actual data via F90 array pointer
    call ESMF_ArrayGet(array(1), localDe=0, farrayPtr=farrayPtr1, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArrayGet(array(2), localDe=0, farrayPtr=farrayPtr2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArrayGet(array(3), localDe=0, farrayPtr=farrayPtr3, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Fill source Array with data
    do j = lbound(farrayPtr1, 2), ubound(farrayPtr1, 2)
      do i = lbound(farrayPtr1, 1), ubound(farrayPtr1, 1)
        farrayPtr1(i,j) = 10.0d0 &
          + 5.0d0 * sin(real(i,ESMF_KIND_R8)/100.d0*pi) &
          + 2.0d0 * sin(real(j,ESMF_KIND_R8)/150.d0*pi)
        farrayPtr2(i,j) = farrayPtr1(i,j) + 123.456d0
        farrayPtr3(i,j) = farrayPtr1(i,j) - 123.456d0
      enddo
    enddo
 
    print *, "User Comp1 Run returning"

  end subroutine user_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
  subroutine user_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_DistGrid)   :: distgrid
    type(ESMF_Array)      :: array(3)
    type(ESMF_ArrayBundle):: arraybundle
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Final starting"

    call ESMF_StateGet(exportState, "srcAryBndl", arraybundle, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArrayBundleGet(arraybundle, arrayList=array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArrayBundleDestroy(arraybundle, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArrayGet(array(1), distgrid=distgrid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArrayDestroy(array(1), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArrayDestroy(array(2), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArrayDestroy(array(3), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_DistGridDestroy(distgrid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print *, "User Comp1 Final returning"

  end subroutine user_final


end module user_model1
    
!\end{verbatim}
