! $Id$
!
! Example/test code which shows User Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
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
    
    public userm1_register
        
    contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
    subroutine userm1_register(comp, rc)
        type(ESMF_GridComp) :: comp
        integer, intent(out) :: rc

        rc = ESMF_SUCCESS
        print *, "in user register routine"

        ! Register the callback routines.

        call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, user_init, rc=rc)
        if(rc/=ESMF_SUCCESS) return
        call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_RUN, user_run, rc=rc)
        if(rc/=ESMF_SUCCESS) return
        call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, user_final, rc=rc)
        if(rc/=ESMF_SUCCESS) return

        print *, "Registered Initialize, Run, and Finalize routines"

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
        type(ESMF_Field) :: srcField
        type(ESMF_VM) :: vm
        type(ESMF_Mesh) :: srcMesh
        type(ESMF_ArraySpec) :: arrayspec
        integer :: petCount, localPet, localrc
        real(ESMF_KIND_R8), pointer :: fptr1D(:)
        integer, pointer :: nodeIds(:),nodeOwners(:)
        real(ESMF_KIND_R8), pointer :: nodeCoords(:)
        integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
        integer :: numNodes, numElems
        integer :: numQuadElems,numTriElems, numTotElems
        integer :: i1,i2
        real(ESMF_KIND_R8) :: x,y

        ! Query component for VM and create a layout with the right breakdown
        call ESMF_GridCompGet(comp, vm=vm, rc=rc)
        if(rc/=ESMF_SUCCESS) return
        call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
        if(rc/=ESMF_SUCCESS) return

        rc = ESMF_SUCCESS

        !!!!!! Setup source Mesh !!!!!!!!!

        ! Set number of nodes
        numNodes=9

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/1,2,3,4,5,6,7,8,9/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/0.0,0.0, & ! node id 1
                     1.0,0.0, & ! node id 2
                     2.0,0.0, & ! node id 3
                     0.0,1.0, & ! node id 4
                     1.0,1.0, & ! node id 5
                     2.0,1.0, & ! node id 6
                     0.0,2.0, & ! node id 7
                     1.0,2.0, & ! node id 8
                     2.0,2.0 /) ! node id 9

        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
        allocate(nodeOwners(numNodes))
        nodeOwners=0 ! everything on PET 0

        ! Set the number of each type of element, plus the total number.
        numQuadElems=3
        numTriElems=2
        numTotElems=numQuadElems+numTriElems

       ! Allocate and fill the element id array.
       allocate(elemIds(numTotElems))
       elemIds=(/1,2,3,4,5/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numTotElems))
       elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! elem id 1
                   ESMF_MESHELEMTYPE_TRI,  & ! elem id 2
                   ESMF_MESHELEMTYPE_TRI,  & ! elem id 3
                   ESMF_MESHELEMTYPE_QUAD, & ! elem id 4
                   ESMF_MESHELEMTYPE_QUAD/)  ! elem id 5


       ! Allocate and fill the element connection type array.
       ! Note that entries in this array refer to the 
       ! positions in the nodeIds, etc. arrays and that
       ! the order and number of entries for each element
       ! reflects that given in the Mesh options 
       ! section for the corresponding entry
       ! in the elemTypes array.
       allocate(elemConn(4*numQuadElems+3*numTriElems))
       elemConn=(/1,2,5,4, &  ! elem id 1
                  2,3,5,   &  ! elem id 2
                  3,6,5,   &  ! elem id 3
                  4,5,8,7, &  ! elem id 4
                  5,6,9,8/)   ! elem id 5

      ! Create Mesh structure in 1 step
      srcMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
                    nodeIds=nodeIds, nodeCoords=nodeCoords, &
                    nodeOwners=nodeOwners, elementIds=elemIds,&
                    elementTypes=elemTypes, elementConn=elemConn, rc=localrc)
      if (localrc /=ESMF_SUCCESS) then
          rc=ESMF_FAILURE
          return
      endif

      ! Create source field
      call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

      srcField = ESMF_FieldCreate(srcMesh, arrayspec, &
                        name="src", rc=localrc)
      if (localrc /=ESMF_SUCCESS) then
         rc=ESMF_FAILURE
         return
      endif

      ! Load test data into the source Field
      ! Should only be 1 localDE
      call ESMF_FieldGet(srcField, 0, fptr1D,  rc=localrc)
      if (localrc /=ESMF_SUCCESS) then
          rc=ESMF_FAILURE
          return
      endif

      ! set interpolated function
      i2=1
      do i1=1,numNodes

         if (nodeOwners(i1) .eq. localPet) then
            ! Get coordinates
            x=nodeCoords(2*i1-1)
            y=nodeCoords(2*i1)

            ! Set source function
            fptr1D(i2) = 20.0+x+y

            ! Advance to next owner
            i2=i2+1
        endif
      enddo

      ! deallocate node data
      deallocate(nodeIds)
      deallocate(nodeCoords)
      deallocate(nodeOwners)

      ! deallocate elem data
      deallocate(elemIds)
      deallocate(elemTypes)
      deallocate(elemConn)


        ! Add Field to State
        call ESMF_StateAdd(exportState, (/srcField/), rc=rc)
        if (rc .ne. ESMF_SUCCESS) return
     !   call ESMF_StatePrint(exportState, rc=rc)

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
        type(ESMF_Field) :: humidity
        type(ESMF_grid) :: grid
        real(ESMF_KIND_R8) :: pi
        real(ESMF_KIND_R8), dimension(:,:), pointer :: idata, coordX, coordY
        integer :: i, j, i1, j1

        rc = ESMF_SUCCESS
        print *, "User Comp Run starting"

        print *, "User Comp Run returning"

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
        type(ESMF_Field) :: srcField
        type(ESMF_Mesh) :: srcMesh

        rc = ESMF_SUCCESS
        print *, "User Comp Final starting"

        ! garbage collection   
        call ESMF_StateGet(exportState, "src", srcField, rc=rc)
        if (rc .ne. ESMF_SUCCESS) return
        call ESMF_FieldGet(srcField, mesh=srcMesh, rc=rc)
        if (rc .ne. ESMF_SUCCESS) return
        call ESMF_FieldDestroy(srcField, rc=rc)
        if (rc .ne. ESMF_SUCCESS) return
        call ESMF_MeshDestroy(srcMesh, rc=rc)
        if (rc .ne. ESMF_SUCCESS) return

        print *, "User Comp Final returning"

    end subroutine user_final


    end module user_model1
    
!\end{verbatim}
    
