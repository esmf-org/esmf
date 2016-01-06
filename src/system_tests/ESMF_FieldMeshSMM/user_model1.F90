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

        ! local variables

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
        type(ESMF_Field) :: humidity
        type(ESMF_Mesh)  :: mesh
        type(ESMF_VM) :: vm
        type(ESMF_ArraySpec) :: arrayspec
        integer(ESMF_KIND_I4), dimension(:), pointer :: srcfptr
        integer :: npets, localPet, i

        integer, pointer :: nodeIds(:),nodeOwners(:)
        real(ESMF_KIND_R8), pointer :: nodeCoords(:)
        integer :: numNodes
        integer :: numElems
        integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)

        ! Query component for VM and create a layout with the right breakdown
        call ESMF_GridCompGet(comp, vm=vm, rc=rc)
        if(rc/=ESMF_SUCCESS) return
        call ESMF_VMGet(vm, localPet=localPet, petCount=npets, rc=rc)
        if(rc/=ESMF_SUCCESS) return

        rc = ESMF_SUCCESS
        print *, localPet, "User Comp 1 Init starting"

        ! Set up a src humidity field on a mesh
        ! Setup mesh data depending on PET
        if (localPet .eq. 0) then
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
            nodeOwners=(/0,0,0,0/) ! everything on proc 0

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
          else if (localPet .eq. 1) then
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
          else if (localPet .eq. 2) then
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
          else 
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

        ! Create Mesh structure in 1 step
        mesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
             coordSys=ESMF_COORDSYS_CART, &
               nodeIds=nodeIds, nodeCoords=nodeCoords, &
               nodeOwners=nodeOwners, elementIds=elemIds,&
               elementTypes=elemTypes, elementConn=elemConn, &
               rc=rc)
        if(rc .ne. ESMF_SUCCESS) return

        call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_I4, rc=rc)
        if (rc .ne. ESMF_SUCCESS) return

        humidity = ESMF_FieldCreate(mesh, arrayspec, name="humidity", rc=rc)
        if (rc .ne. ESMF_SUCCESS) return

        call ESMF_FieldGet(humidity, localDe=0, farrayPtr=srcfptr, rc=rc)
        if (rc .ne. ESMF_SUCCESS) return

        do i = lbound(srcfptr, 1), ubound(srcfptr, 1)
            srcfptr(i) = i
        enddo

        call ESMF_StateAdd(exportState, (/humidity/), rc=rc)
        if (rc .ne. ESMF_SUCCESS) return
     !   call ESMF_StatePrint(exportState, rc=rc)

        print *, localPet, "User Comp 1 Init returning"
        deallocate(nodeCoords, nodeIds, nodeOwners)
        deallocate(elemIds, elemTypes, elemConn)

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
        type(ESMF_Field) :: humidity
        type(ESMF_Mesh)  :: mesh

        rc = ESMF_SUCCESS
        print *, "User Comp Final starting"

        ! Get our local info
        call ESMF_StateGet(exportState, "humidity", humidity, rc=rc)
        if(rc/=ESMF_SUCCESS) return
        call ESMF_FieldGet(humidity, mesh=mesh, rc=rc)
        if(rc/=ESMF_SUCCESS) return

        call ESMF_FieldDestroy(humidity, rc=rc)
        if(rc/=ESMF_SUCCESS) return
        call ESMF_MeshDestroy(mesh, rc=rc)
        if(rc/=ESMF_SUCCESS) return
        print *, "User Comp Final returning"

    end subroutine user_final


    end module user_model1
    
!\end{verbatim}
    
