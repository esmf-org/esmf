! $Id$
!
! Example/test code which shows User Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!
! !DESCRIPTION:
!  User-supplied Component, most recent interface revision.
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
    logical :: pthreadsEnabled
#endif

    ! Initialize return code
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

  subroutine userm1_register(comp, rc)
    type(ESMF_GridComp) :: comp
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Register starting"

    ! Register the callback routines.

    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, userRoutine=user_init, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_RUN, userRoutine=user_run, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, userRoutine=user_final, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    print *, "Registered Initialize, Run, and Finalize routines"
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
    type(ESMF_Grid)       :: grid
    type(ESMF_FieldBundle):: fieldbundle
    type(ESMF_VM)         :: vm
    integer               :: petCount, i
    character(len=ESMF_MAXSTR) :: names(3)
    real(ESMF_KIND_R8), pointer :: farrayPtr(:,:,:)   ! matching F90 array pointer
    integer               :: compLBnd(2),compUBnd(2)
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Init starting"

    ! Determine petCount
    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
    call ESMF_VMGet(vm, petCount=petCount, rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
    
    ! Create the source FieldBundle and add it to the export State
    call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    grid = ESMF_GridCreateNoPeriDimUfrm( &
    maxIndex=(/100,150/), &
    regDecomp=(/petCount,1/), &
    minCornerCoord=(/0.0_ESMF_KIND_R8,-75.0_ESMF_KIND_R8/), &
    maxCornerCoord=(/100.0_ESMF_KIND_R8,75.0_ESMF_KIND_R8/), &
    staggerLocList=(/ESMF_STAGGERLOC_CORNER, ESMF_STAGGERLOC_CENTER/), &
    rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)


    ! Get Bounds
    call ESMF_GridGet(grid, localDe=0, staggerloc=ESMF_STAGGERLOC_CENTER, &
           computationalLBound=compLBnd, computationalUBound=compUBnd, rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    allocate(farrayPtr(3,compLBnd(1):compUBnd(1),compLBnd(2):compUBnd(2)))
    names(1) = 'field01'
    names(2) = 'field02'
    names(3) = 'field03'

    fieldbundle = ESMF_FieldBundleCreate(fieldNameList=names, fieldDim=1, &
        grid=grid, farrayPtr=farrayPtr, &
        gridToFieldMap=(/2,3/), &
        name="fieldbundle data", rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    call ESMF_StateAdd(exportState, fieldbundleList=(/fieldbundle/), rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
   
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
    type(ESMF_FieldBundle):: fieldbundle
    type(ESMF_Field)      :: field
    type(ESMF_Array)      :: array
    type(ESMF_Grid)      :: grid
    real(ESMF_KIND_R8), pointer :: farrayPtr(:,:,:)   ! matching F90 array pointer
    integer               :: i, j, k
    integer               :: compLBnd(2),compUBnd(2)
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Run starting"

    pi = 3.14159d0

    ! Get the source Field from the export State
    call ESMF_StateGet(exportState, "fieldbundle data", fieldbundle, rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    ! Get Grid from fieldbundle
    call ESMF_FieldBundleGet(fieldbundle, grid=grid, rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    ! Get Bounds
    call ESMF_GridGet(grid, localDe=0, staggerloc=ESMF_STAGGERLOC_CENTER, &
           computationalLBound=compLBnd, computationalUBound=compUBnd, rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    ! Gain access to actual data via F90 array pointer
    call ESMF_FieldBundleGet(fieldbundle, farrayPtr=farrayPtr, rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    print *, shape(farrayPtr), compLBnd, compUbnd

    do k = 1, 3

        ! Fill source FieldBundle with data
        do j = compLBnd(2), compUbnd(2)
          do i = compLBnd(1), compUBnd(1)
            farrayPtr(k,i,j) = real(k,ESMF_KIND_R8) * 10.0d0 &
              + 5.0d0 * sin(real(i,ESMF_KIND_R8)/100.d0*pi) &
              + 2.0d0 * sin(real(j,ESMF_KIND_R8)/150.d0*pi)
          enddo
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
    type(ESMF_Grid)        :: grid
    type(ESMF_Field)       :: field
    type(ESMF_FieldBundle) :: fieldbundle
    integer                :: k
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Final starting"

    call ESMF_StateGet(exportState, "fieldbundle data", fieldbundle, rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
    call ESMF_FieldBundleGet(fieldbundle, grid=grid, rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
    call ESMF_FieldBundleDestroy(fieldbundle, rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
    call ESMF_GridDestroy(grid, rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    print *, "User Comp1 Final returning"

  end subroutine user_final


end module user_model1
    
!\end{verbatim}
