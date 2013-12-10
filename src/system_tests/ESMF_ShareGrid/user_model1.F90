! $Id$
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
  use ESMF

  implicit none
    
  public userm1_setvm, userm1_register
        
  contains

!-------------------------------------------------------------------------
 
  subroutine userm1_setvm(comp, rc)
    type(ESMF_GridComp)   :: comp
    integer, intent(out)  :: rc
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
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMGet(vm, pthreadsEnabledFlag=pthreadsEnabled, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    if (pthreadsEnabled) then
      call ESMF_GridCompSetVMMinThreads(comp, rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
    endif
#endif

  end subroutine

!-------------------------------------------------------------------------

  subroutine userm1_register(comp, rc)
    type(ESMF_GridComp)   :: comp
    integer, intent(out)  :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Register starting"

    ! Register the callback routines.

    call ESMF_GridCompSetEntryPoint(comp, methodflag=ESMF_METHOD_INITIALIZE, &
      userRoutine=user_initP1, phase=1, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, methodflag=ESMF_METHOD_INITIALIZE, &
      userRoutine=user_initP2, phase=2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, methodflag=ESMF_METHOD_RUN, &
      userRoutine=user_run, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, methodflag=ESMF_METHOD_FINALIZE, &
      userRoutine=user_final, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print *, "Registered Initialize, Run, and Finalize routines"
    print *, "User Comp1 Register returning"
    
  end subroutine

!-------------------------------------------------------------------------

  ! In phase 1 Initialize Comp1 creates an empty Field with a Grid. The Grid
  ! at this point only contains index space information, i.e. DistGrid, and
  ! no physical space coordinates.
    
  subroutine user_initP1(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables
    type(ESMF_Grid)       :: grid
    type(ESMF_Field)      :: field
    type(ESMF_VM)         :: vm
    integer               :: petCount
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Init phase=1 starting"

    ! Determine petCount
    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMGet(vm, petCount=petCount, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Create the source Field on a Grid and add it to the export State
    grid = ESMF_GridCreate1PeriDim(minIndex=(/1,1/), maxIndex=(/100,150/), &
      regDecomp=(/petCount,1/), indexflag=ESMF_INDEX_GLOBAL, &
      coordSys=ESMF_COORDSYS_SPH_DEG, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    field = ESMF_FieldEmptyCreate(name="srcField", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldEmptySet(field, grid=grid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateAdd(exportState, (/field/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
   
    print *, "User Comp1 Init phase=1 returning"

  end subroutine user_initP1

!-------------------------------------------------------------------------
 
  ! In phase 2 Initialize Comp1 finishes creating the Field that was started
  ! under phase 1 Initialize. This means that memory is allocated for the data.
  ! Furthermore physical coordinates are added to the Grid.

  subroutine user_initP2(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables
    type(ESMF_Grid)                   :: grid
    type(ESMF_Field)                  :: field
    integer                           :: i, j
    real(kind=ESMF_KIND_R8),  pointer :: lonPtr(:,:), latPtr(:,:)
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Init phase=2 starting"

    ! Access the Field and its Grid in the exportState
    call ESMF_StateGet(exportState, "srcField", field=field, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldGet(field, grid=grid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Allocate memory for data by finishing the creation of the Field
    call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Add coordinates to the Grid, get the pointer to the memory and fill
    call ESMF_GridAddCoord(grid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridGetCoord(grid, coordDim=1, farrayPtr=lonPtr, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridGetCoord(grid, coordDim=2, farrayPtr=latPtr, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    do j=lbound(lonPtr,2),ubound(lonPtr,2)
    do i=lbound(lonPtr,1),ubound(lonPtr,1)
      lonPtr(i,j) = 360./real(100) * (i-1)
      latPtr(i,j) = 100./real(150) * (j-1) - 50.
    enddo
    enddo
   
    print *, "User Comp1 Init phase=2 returning"

  end subroutine user_initP2

!-------------------------------------------------------------------------
 
  ! Fill data into the exported Field.
  
  subroutine user_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables
    type(ESMF_Field)                  :: field
    integer                           :: i, j
    real(kind=ESMF_KIND_R8),  pointer :: dataPtr(:,:)
    integer                           :: localPet
    
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Run starting"

    ! Access the Field in the exportState
    call ESMF_StateGet(exportState, "srcField", field=field, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Get localPet to have something to fill in
    call ESMF_GridCompGet(comp, localPet=localPet, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Fill in data into Field
    call ESMF_FieldGet(field, farrayPtr=dataPtr, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    do j=lbound(dataPtr,2),ubound(dataPtr,2)
    do i=lbound(dataPtr,1),ubound(dataPtr,1)
      dataPtr(i,j) = real(localPet)
    enddo
    enddo

    print *, "User Comp1 Run returning"

  end subroutine user_run

!-------------------------------------------------------------------------
 
  subroutine user_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Final starting"

    print *, "User Comp1 Final returning"

  end subroutine user_final

!--------------------------------------------------------------------------------

end module user_model1
    
!\end{verbatim}
