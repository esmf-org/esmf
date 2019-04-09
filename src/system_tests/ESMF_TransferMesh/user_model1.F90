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
    ! The following call will turn on ESMF-threading support
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
      call ESMF_GridCompSetVMMaxPEs(comp, maxPeCountPerPet=2, rc=rc)
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

  ! In phase 1 Initialize, Comp1 creates an empty Field with a Mesh. The Mesh
  ! creation assums that this component runs on exactly 4 PETs.
    
  subroutine user_initP1(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables
    type(ESMF_Mesh)       :: mesh
    type(ESMF_Field)      :: field
    integer               :: localPet
    
    integer                     :: numNodes, numElems
    integer, pointer            :: nodeIds(:), nodeOwners(:)
    integer, pointer            :: elemIds(:), elemTypes(:), elemConn(:)
    real(ESMF_KIND_R8), pointer :: nodeCoords(:)
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Init phase=1 starting"

    ! Determine localPet
    call ESMF_GridCompGet(comp, localPet=localPet, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Setup Mesh parameters depending on PET
    if (localPet == 0) then
      ! Fill in node data
      numNodes=4

      !! node ids
      allocate(nodeIds(numNodes))
      nodeIds=(/1,2,4,5/)

      !! node Coords
      allocate(nodeCoords(numNodes*2))
      nodeCoords=(/0.0,0.0, &
                   1.0,0.0, &
                   0.0,1.0, &
                   1.0,1.0/)

      !! node owners
      allocate(nodeOwners(numNodes))
      nodeOwners=(/0,0,0,0/)

      ! Fill in elem data
      numElems=1

      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/1/)

      !! elem type
      allocate(elemTypes(numElems))
      elemTypes=ESMF_MESHELEMTYPE_QUAD

      !! elem conn
      allocate(elemConn(numElems*4))
      elemConn=(/1,2,4,3/)
      
    else if (localPet == 1) then
      ! Fill in node data
      numNodes=4

      !! node ids
      allocate(nodeIds(numNodes))
      nodeIds=(/2,3,5,6/)

      !! node Coords
      allocate(nodeCoords(numNodes*2))
      nodeCoords=(/1.0,0.0, &
                   2.0,0.0, &
                   1.0,1.0, &
                   2.0,1.0/)

      !! node owners
      allocate(nodeOwners(numNodes))
      nodeOwners=(/0,1,0,1/)

      ! Fill in elem data
      numElems=1

      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/2/)

      !! elem type
      allocate(elemTypes(numElems))
      elemTypes=ESMF_MESHELEMTYPE_QUAD

      !! elem conn
      allocate(elemConn(numElems*4))
      elemConn=(/1,2,4,3/)
      
    else if (localPet == 2) then
      ! Fill in node data
      numNodes=4

      !! node ids
      allocate(nodeIds(numNodes))
      nodeIds=(/4,5,7,8/)

      !! node Coords
      allocate(nodeCoords(numNodes*2))
      nodeCoords=(/0.0,1.0, &
                   1.0,1.0, &
                   0.0,2.0, &
                   1.0,2.0/)

      !! node owners
      allocate(nodeOwners(numNodes))
      nodeOwners=(/0,0,2,2/)

      ! Fill in elem data
      numElems=1

      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/3/)

      !! elem type
      allocate(elemTypes(numElems))
      elemTypes=ESMF_MESHELEMTYPE_QUAD

      !! elem conn
      allocate(elemConn(numElems*4))
      elemConn=(/1,2,4,3/)
      
    else if (localPet == 3) then
      ! Fill in node data
      numNodes=4

      !! node ids
      allocate(nodeIds(numNodes))
      nodeIds=(/5,6,8,9/)

      !! node Coords
      allocate(nodeCoords(numNodes*2))
      nodeCoords=(/1.0,1.0, &
                   2.0,1.0, &
                   1.0,2.0, &
                   2.0,2.0/)

      !! node owners
      allocate(nodeOwners(numNodes))
      nodeOwners=(/0,1,2,3/)

      ! Fill in elem data
      numElems=1

      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/4/)

      !! elem type
      allocate(elemTypes(numElems))
      elemTypes=ESMF_MESHELEMTYPE_QUAD

      !! elem conn
      allocate(elemConn(numElems*4))
      elemConn=(/1,2,4,3/)
    endif

    ! Create the Mesh
    mesh = ESMF_MeshCreate(parametricDim=2,spatialDim=2, nodeIds=nodeIds, &
      coordSys=ESMF_COORDSYS_CART, &
      nodeCoords=nodeCoords, nodeOwners=nodeOwners, elementIds=elemIds, &
      elementTypes=elemTypes, elementConn=elemConn, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Clean up after mesh create
    deallocate(nodeIds)
    deallocate(nodeOwners)
    deallocate(elemIds)
    deallocate(elemTypes)
    deallocate(elemConn)
    deallocate(nodeCoords)

    ! Create an empty Field on the Mesh
    field = ESMF_FieldEmptyCreate(name="srcField", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldEmptySet(field, mesh=mesh, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Add the Field to the exportState
    call ESMF_StateAdd(exportState, (/field/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
   
    print *, "User Comp1 Init phase=1 returning"

  end subroutine user_initP1

!-------------------------------------------------------------------------
 
  ! In phase 2 Initialize, Comp1 finishes creating the Field that was started
  ! under phase 1 Initialize. This means that memory is allocated for the data.

  subroutine user_initP2(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables
    type(ESMF_Field)                  :: field
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Init phase=2 starting"

    ! Access the Field and its Grid in the exportState
    call ESMF_StateGet(exportState, "srcField", field=field, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Allocate memory for data by finishing the creation of the Field
    call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
       
    print *, "User Comp1 Init phase=2 returning"

  end subroutine user_initP2

!-------------------------------------------------------------------------
 
  ! In phase 3 Initialize, Comp1 creates another Field on the same Mesh
  ! that was shared with Comp2. This Field will be used for testing the 
  ! correct re-mapping between Comp1 -> Comp2 -> Comp1.

  subroutine user_initP3(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables
    type(ESMF_Mesh)                   :: mesh
    type(ESMF_Field)                  :: field, finalField
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Init phase=3 starting"

    ! Access the already created Field and its Mesh in the exportState
    call ESMF_StateGet(exportState, "srcField", field=field, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldGet(field, mesh=mesh, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Create the finalField on the same Mesh with allocate memory
    finalField = ESMF_FieldCreate(mesh, typekind=ESMF_TYPEKIND_R8, &
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
    type(ESMF_Mesh)                   :: Mesh
    integer                           :: i
    real(kind=ESMF_KIND_R8),  pointer :: dataPtr(:)
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Run starting"

    ! Access the Field in the exportState
    call ESMF_StateGet(exportState, "srcField", field=field, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Fill data into Field
    call ESMF_FieldGet(field, farrayPtr=dataPtr, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    do i=lbound(dataPtr,1),ubound(dataPtr,1)
      dataPtr(i) = real(i)
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
    integer                           :: i
    real(kind=ESMF_KIND_R8),  pointer :: dataPtr(:), finalDataPtr(:)
    
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

    do i=lbound(dataPtr,1),ubound(dataPtr,1)
      if (abs(dataPtr(i)-finalDataPtr(i)) > 1.E-15) then
        print *, "error at (",i,"): ", abs(dataPtr(i)-finalDataPtr(i))
        rc = ESMF_RC_VAL_WRONG
      endif
    enddo

    print *, "User Comp1 Final returning"

  end subroutine user_final

!--------------------------------------------------------------------------------

end module user_model1
    
!\end{verbatim}
