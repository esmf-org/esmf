! $Id$
!
! Example/test code which shows User Component calls.

!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  User-supplied Component
!
!
!\begin{verbatim}

    module user_model2

    ! ESMF Framework module
    use ESMF

    implicit none

    public userm2_register

    contains

!--------------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.

    subroutine userm2_register(comp, rc)
        type(ESMF_GridComp) :: comp
        integer, intent(out) :: rc

        rc = ESMF_SUCCESS
        print *, "In user register routine"

        ! Register the callback routines.

        call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, user_init, rc=rc)
        if(rc/=ESMF_SUCCESS) return
        call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_RUN, user_run, rc=rc)
        if(rc/=ESMF_SUCCESS) return
        call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, user_final, rc=rc)
        if(rc/=ESMF_SUCCESS) return

        print *, "Registered Initialize, Run, and Finalize routines"

    end subroutine

!--------------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.

    subroutine user_init(comp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: comp
      type(ESMF_State) :: importState, exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

!   ! Local variables
      type(ESMF_Field) :: dstField
      type(ESMF_VM) :: vm
      type(ESMF_Mesh) :: dstMesh
      type(ESMF_ArraySpec) :: arrayspec
      integer :: localPET, petCount,localrc
      integer i
      real(ESMF_KIND_R8), pointer :: fptr1D(:)
      integer, pointer :: nodeIds(:),nodeOwners(:)
      real(ESMF_KIND_R8), pointer :: nodeCoords(:)
      integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
      integer :: numNodes, numElems
      integer :: numQuadElems,numTriElems, numTotElems

      ! Query component for VM and create a layout with the right breakdown
      call ESMF_GridCompGet(comp, vm=vm, rc=rc)
      if(rc/=ESMF_SUCCESS) return
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if(rc/=ESMF_SUCCESS) return

      ! Setup mesh data depending on PET
      if (localPET .eq. 0) then !!! This part only for PET 0
         ! Set number of nodes
         numNodes=4

         ! Allocate and fill the node id array.
         allocate(nodeIds(numNodes))
         nodeIds=(/1,2,4,5/)

         ! Allocate and fill node coordinate array.
         ! Since this is a 2D Mesh the size is 2x the
         ! number of nodes.
         allocate(nodeCoords(2*numNodes))
         nodeCoords=(/0.0,0.0, & ! node id 1
                      1.0,0.0, & ! node id 2
                      0.0,1.0, & ! node id 4
                      1.0,1.0 /) ! node id 5

         ! Allocate and fill the node owner array.
         allocate(nodeOwners(numNodes))
         nodeOwners=(/0, & ! node id 1
                    0, & ! node id 2
                    0, & ! node id 4
                    0/)  ! node id 5

         ! Set the number of each type of element, plus the total number.
         numQuadElems=1
         numTriElems=0
         numTotElems=numQuadElems+numTriElems

         ! Allocate and fill the element id array.
         allocate(elemIds(numTotElems))
         elemIds=(/1/)

         ! Allocate and fill the element topology type array.
         allocate(elemTypes(numTotElems))
         elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 1

         ! Allocate and fill the element connection type array.
         ! Note that entry are local indices
         allocate(elemConn(4*numQuadElems+3*numTriElems))
         elemConn=(/1,2,4,3/) ! elem id 1

       else if (localPET .eq. 1) then !!! This part only for PET 1
         ! Set number of nodes
         numNodes=4

         ! Allocate and fill the node id array.
         allocate(nodeIds(numNodes))
         nodeIds=(/2,3,5,6/)

         ! Allocate and fill node coordinate array.
         ! Since this is a 2D Mesh the size is 2x the
         ! number of nodes.
         allocate(nodeCoords(2*numNodes))
         nodeCoords=(/1.0,0.0, & ! node id 2
                    2.0,0.0, & ! node id 3
                    1.0,1.0, & ! node id 5
                    2.0,1.0 /) ! node id 6

         ! Allocate and fill the node owner array.
         allocate(nodeOwners(numNodes))
         nodeOwners=(/0, & ! node id 2
                    1, & ! node id 3
                    0, & ! node id 5
                    1/)  ! node id 6

         ! Set the number of each type of element, plus the total number.
         numQuadElems=0
         numTriElems=2
         numTotElems=numQuadElems+numTriElems

         ! Allocate and fill the element id array.
         allocate(elemIds(numTotElems))
         elemIds=(/2,3/)

         ! Allocate and fill the element topology type array.
         allocate(elemTypes(numTotElems))
         elemTypes=(/ESMF_MESHELEMTYPE_TRI, & ! elem id 2
                   ESMF_MESHELEMTYPE_TRI/)  ! elem id 3

         ! Allocate and fill the element connection type array.
         allocate(elemConn(4*numQuadElems+3*numTriElems))
         elemConn=(/1,2,3, & ! elem id 2
                    2,4,3/)  ! elem id 3

      else if (localPET .eq. 2) then !!! This part only for PET 2
         ! Set number of nodes
         numNodes=4

         ! Allocate and fill the node id array.
         allocate(nodeIds(numNodes))
         nodeIds=(/4,5,7,8/)

         ! Allocate and fill node coordinate array.
         ! Since this is a 2D Mesh the size is 2x the
         ! number of nodes.
         allocate(nodeCoords(2*numNodes))
         nodeCoords=(/0.0,1.0, & ! node id 4
                      1.0,1.0, & ! node id 5
                      0.0,2.0, & ! node id 7
                      1.0,2.0 /) ! node id 8

         ! Allocate and fill the node owner array.
         ! Since this Mesh is all on PET 0, it's just set to all 0.
         allocate(nodeOwners(numNodes))
         nodeOwners=(/0, & ! node id 4
                      0, & ! node id 5
                      2, & ! node id 7
                      2/)  ! node id 8

         ! Set the number of each type of element, plus the total number.
         numQuadElems=1
         numTriElems=0
         numTotElems=numQuadElems+numTriElems

         ! Allocate and fill the element id array.
         allocate(elemIds(numTotElems))
         elemIds=(/4/)

         ! Allocate and fill the element topology type array.
         allocate(elemTypes(numTotElems))
         elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 4

         ! Allocate and fill the element connection type array.
         allocate(elemConn(4*numQuadElems+3*numTriElems))
         elemConn=(/1,2,4,3/) ! elem id 4

      else if (localPET .eq. 3) then !!! This part only for PET 3
         ! Set number of nodes
         numNodes=4

         ! Allocate and fill the node id array.
         allocate(nodeIds(numNodes))
         nodeIds=(/5,6,8,9/)

         ! Allocate and fill node coordinate array.
         ! Since this is a 2D Mesh the size is 2x the
         ! number of nodes.
         allocate(nodeCoords(2*numNodes))
         nodeCoords=(/1.0,1.0, &  ! node id 5
                      2.0,1.0, &  ! node id 6
                      1.0,2.0, &  ! node id 8
                      2.0,2.0 /)  ! node id 9

         ! Allocate and fill the node owner array.
         allocate(nodeOwners(numNodes))
         nodeOwners=(/0, & ! node id 5
                      1, & ! node id 6
                      2, & ! node id 8
                      3/)  ! node id 9

         ! Set the number of each type of element, plus the total number.
         numQuadElems=1
         numTriElems=0
         numTotElems=numQuadElems+numTriElems

         ! Allocate and fill the element id array.
         allocate(elemIds(numTotElems))
         elemIds=(/5/)

         ! Allocate and fill the element topology type array.
         allocate(elemTypes(numTotElems))
         elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 5

         ! Allocate and fill the element connection type array.
         allocate(elemConn(4*numQuadElems+3*numTriElems))
         elemConn=(/1,2,4,3/) ! elem id 5
    endif


    ! Create Mesh structure in 1 step
    dstMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
           coordSys=ESMF_COORDSYS_CART, &
           nodeIds=nodeIds, nodeCoords=nodeCoords, &
           nodeOwners=nodeOwners, elementIds=elemIds,&
           elementTypes=elemTypes, elementConn=elemConn, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
         rc=ESMF_FAILURE
         return
     endif

     ! deallocate node data
     deallocate(nodeIds)
     deallocate(nodeCoords)
     deallocate(nodeOwners)

     ! deallocate elem data
     deallocate(elemIds)
     deallocate(elemTypes)
     deallocate(elemConn)


    ! Create dest field
    call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

    dstField = ESMF_FieldCreate(dstMesh, arrayspec, &
                        name="dst", rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
       rc=ESMF_FAILURE
       return
    endif


    ! clear destination Field
    ! Should only be 1 localDE
    call ESMF_FieldGet(dstField, 0, fptr1D,  rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
    endif

    fptr1D=0.0

    ! Set Field Into State
    call ESMF_StateAdd(importState, (/dstField/), rc=rc)
    if (rc/=ESMF_SUCCESS) return

    ! Return success
    rc = ESMF_SUCCESS

    end subroutine user_init


!--------------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !

    subroutine user_run(comp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: comp
      type(ESMF_State) :: importState, exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

!   ! Local variables
      type(ESMF_Field) :: dstField
      type(ESMF_Mesh) :: dstMesh
      type(ESMF_VM) :: vm
      integer :: localrc,i
      integer :: localPet, petCount
      real(ESMF_KIND_R8), pointer :: fptr1D(:)
      real(ESMF_KIND_R8) :: x,y
      integer :: numOwnedNodes
      real(ESMF_KIND_R8), pointer :: ownedNodeCoords(:)

      rc = ESMF_SUCCESS

      ! Query component for VM and create a layout with the right breakdown
      call ESMF_GridCompGet(comp, vm=vm, rc=rc)
      if(rc/=ESMF_SUCCESS) return
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if(rc/=ESMF_SUCCESS) return

      ! Get information from the component.
      call ESMF_StateGet(importState, "dst", dstField, rc=rc)
      if(rc/=ESMF_SUCCESS) return


      ! Get Grid from field
      call ESMF_FieldGet(dstField, mesh=dstMesh, rc=localrc)
      if (localrc /=ESMF_SUCCESS) then
          rc=ESMF_FAILURE
          return
      endif

      ! Check destination field
      ! Should only be 1 localDE
      call ESMF_FieldGet(dstField, 0, fptr1D,  rc=localrc)
      if (localrc /=ESMF_SUCCESS) then
         rc=ESMF_FAILURE
         return
      endif


      ! Get number of local nodes to allocate space
      ! to hold local node coords
      call ESMF_MeshGet(dstMesh, numOwnedNodes=numOwnedNodes, &
             rc=rc)

      ! Allocate space to hold local node coordinates
      ! (spatial dimension of Mesh*number of local nodes)
      allocate(ownedNodeCoords(2*numOwnedNodes))

      ! Get local node coordinates
      call ESMF_MeshGet(dstMesh, &
             ownedNodeCoords=ownedNodeCoords, rc=rc)


      ! loop through nodes and make sure interpolated values are reasonable
      do i=1,numOwnedNodes
         ! Get coordinates
         x=ownedNodeCoords(2*i-1)
         y=ownedNodeCoords(2*i)

         !! if error is too big report an error
         if ( abs( fptr1D(i)-(x+y+20.0) ) > 0.0001) then
              rc=ESMF_FAILURE
              return
         endif  
      enddo

      ! deallocate space to hold local node coordinates
      deallocate(ownedNodeCoords)

      ! RESET DESTINATION BACK TO 0
      fptr1D=0.0

      ! Return success
      rc = ESMF_SUCCESS

    end subroutine user_run


!--------------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !

    subroutine user_final(comp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: comp
      type(ESMF_State) :: importState, exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      ! Local variables
      type(ESMF_Field) :: dstField
      type(ESMF_Mesh) :: dstMesh

      rc = ESMF_SUCCESS
      print *, "User Comp Final starting"

      ! check validity of results
      ! Get Fields from import state
      call ESMF_StateGet(importState, "dst", dstField, rc=rc)
      if(rc/=ESMF_SUCCESS) return

      ! garbage collection
      call ESMF_FieldGet(dstField, mesh=dstMesh, rc=rc)
      if (rc .ne. ESMF_SUCCESS) return

      call ESMF_FieldDestroy(dstField, rc=rc)
      if (rc .ne. ESMF_SUCCESS) return

      call ESMF_MeshDestroy(dstMesh, rc=rc)
      if (rc .ne. ESMF_SUCCESS) return

      print *, "User Comp Final returning"

      ! Return success
      rc = ESMF_SUCCESS
    end subroutine user_final


    end module user_model2

!\end{verbatim}

