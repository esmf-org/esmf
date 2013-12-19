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
    call ESMF_GridCompSetEntryPoint(comp, methodflag=ESMF_METHOD_INITIALIZE, &
      userRoutine=user_initP3, phase=3, rc=rc)
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

  ! In phase 1 Initialize, Comp1 creates an empty Field with a Grid. The Grid
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
    
    ! Create the source Grid
    grid = ESMF_GridCreate1PeriDim(minIndex=(/1,1/), maxIndex=(/100,150/), &
      regDecomp=(/petCount,1/), indexflag=ESMF_INDEX_GLOBAL, &
      coordSys=ESMF_COORDSYS_SPH_DEG, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Create an empty Field on the Grid
    field = ESMF_FieldEmptyCreate(name="srcField", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldEmptySet(field, grid=grid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Add the Field to the exportState
    call ESMF_StateAdd(exportState, (/field/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
   
    print *, "User Comp1 Init phase=1 returning"

  end subroutine user_initP1

!-------------------------------------------------------------------------
 
  ! In phase 2 Initialize, Comp1 finishes creating the Field that was started
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
 
  ! In phase 3 Initialize, Comp1 creates another Field on the same Grid
  ! that was shared with Comp2. This Field will be used for testing the 
  ! correct re-mapping between Comp1 -> Comp2 -> Comp1.

  subroutine user_initP3(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables
    type(ESMF_Grid)                   :: grid
    type(ESMF_Field)                  :: field, finalField
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Init phase=3 starting"

    ! Access the already created Field and its Grid in the exportState
    call ESMF_StateGet(exportState, "srcField", field=field, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldGet(field, grid=grid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Create the finalField on the same Grid with allocate memory
    finalField = ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8, &
      name="finalField", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Add the Field to the exportState
    call ESMF_StateAdd(exportState, (/finalField/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print *, "User Comp1 Init phase=3 returning"

  end subroutine user_initP3

!-------------------------------------------------------------------------
 
  ! Fill data into the exported Field.
  
  subroutine user_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables
    type(ESMF_Field)                  :: field
    type(ESMF_Grid)                   :: grid
    integer                           :: i, j
    real(kind=ESMF_KIND_R8),  pointer :: dataPtr(:,:)
    real(kind=ESMF_KIND_R8),  pointer :: lonPtr(:,:), latPtr(:,:)
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Run starting"

    ! Access the Field in the exportState
    call ESMF_StateGet(exportState, "srcField", field=field, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Get the Grid coordinate arrays
    call ESMF_FieldGet(field, grid=grid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridGetCoord(grid, coordDim=1, farrayPtr=lonPtr, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridGetCoord(grid, coordDim=2, farrayPtr=latPtr, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Fill in data into Field
    call ESMF_FieldGet(field, farrayPtr=dataPtr, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    do j=lbound(dataPtr,2),ubound(dataPtr,2)
    do i=lbound(dataPtr,1),ubound(dataPtr,1)
      dataPtr(i,j) = sin(3.1416*lonPtr(i,j)/180.) * cos(3.1416*latPtr(i,j)/180.)
    enddo
    enddo
      
#if 0
    ! -> Works, but using PIO leads to SEGV during Finalize on some platforms
    ! Write the Field to file
    if (ESMF_IO_PIO_PRESENT .and. &
      (ESMF_IO_NETCDF_PRESENT .or. ESMF_IO_PNETCDF_PRESENT)) then
      call ESMF_FieldWrite(field, file="srcField.nc", &
        status=ESMF_FILESTATUS_REPLACE, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif
#endif

    print *, "User Comp1 Run returning"

  end subroutine user_run

!-------------------------------------------------------------------------
 
  subroutine user_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables
    type(ESMF_Field)                  :: field, finalField
    integer                           :: i, j
    real(kind=ESMF_KIND_R8),  pointer :: dataPtr(:,:), finalDataPtr(:,:)
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Final starting"

    ! Access the Fields in the exportState
    call ESMF_StateGet(exportState, "srcField", field=field, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateGet(exportState, "finalField", field=finalField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

#if 0
    ! -> Works, but using PIO leads to SEGV during Finalize on some platforms
    ! Write the Field to file
    if (ESMF_IO_PIO_PRESENT .and. &
      (ESMF_IO_NETCDF_PRESENT .or. ESMF_IO_PNETCDF_PRESENT)) then
      call ESMF_FieldWrite(finalField, file="finalField.nc", &
        status=ESMF_FILESTATUS_REPLACE, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif
#endif

    ! Access the data in the Fields
    call ESMF_FieldGet(field, farrayPtr=dataPtr, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldGet(finalField, farrayPtr=finalDataPtr, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    do j=lbound(dataPtr,2),ubound(dataPtr,2)
    do i=lbound(dataPtr,1),ubound(dataPtr,1)
      if (abs(dataPtr(i,j)-finalDataPtr(i,j)) > 1.E-15) then
        print *, "error at (",i,",",j,"): ", abs(dataPtr(i,j)-finalDataPtr(i,j))
        rc = ESMF_RC_VAL_WRONG
      endif
    enddo
    enddo

    print *, "User Comp1 Final returning"

  end subroutine user_final

!--------------------------------------------------------------------------------

end module user_model1
    
!\end{verbatim}
