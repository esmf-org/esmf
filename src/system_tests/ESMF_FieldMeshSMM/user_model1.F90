! $Id: user_model1.F90,v 1.2 2009/07/24 18:46:16 theurich Exp $
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
    use ESMF_Mod

    implicit none
    
    public userm1_register
        
    contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
    subroutine userm1_register(comp, rc)
        type(ESMF_GridComp), intent(inout) :: comp
        integer, intent(out) :: rc

        ! local variables

        rc = ESMF_SUCCESS
        print *, "in user register routine"

        ! Register the callback routines.

        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, user_init, rc=rc)
        if(rc/=ESMF_SUCCESS) return
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, user_run, rc=rc)
        if(rc/=ESMF_SUCCESS) return
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, user_final, rc=rc)
        if(rc/=ESMF_SUCCESS) return

        print *, "Registered Initialize, Run, and Finalize routines"

    end subroutine

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
    subroutine user_init(comp, importState, exportState, clock, rc)
        type(ESMF_GridComp), intent(inout) :: comp
        type(ESMF_State), intent(inout) :: importState, exportState
        type(ESMF_Clock), intent(in) :: clock
        integer, intent(out) :: rc

       ! Local variables
        type(ESMF_Field) :: humidity
        type(ESMF_Mesh)  :: mesh
        type(ESMF_VM) :: vm
        type(ESMF_ArraySpec) :: arrayspec
        integer(ESMF_KIND_I4), dimension(:), pointer :: srcfptr
        integer :: npets, de_id

        integer                                     :: n_node, n_elem, i, j, l
        integer, allocatable :: nodeId(:)
        real(ESMF_KIND_R8), allocatable :: nodeCoord(:)
        integer, allocatable :: nodeOwner(:)

        integer, allocatable :: elemId(:)
        integer, allocatable :: elemType(:)
        integer, allocatable :: elemConn(:)
        integer              :: conn(16) = (/1,2,5,4,2,3,6,5,4,5,8,7,5,6,9,8/)

        ! Query component for VM and create a layout with the right breakdown
        call ESMF_GridCompGet(comp, vm=vm, rc=rc)
        if(rc/=ESMF_SUCCESS) return
        call ESMF_VMGet(vm, localPet=de_id, petCount=npets, rc=rc)
        if(rc/=ESMF_SUCCESS) return

        rc = ESMF_SUCCESS
        print *, de_id, "User Comp 1 Init starting"

        ! Add a "humidity" field to the export state.
        n_node = 9
        n_elem = 4
        
        allocate(nodeId(n_node), nodeCoord(2*n_node), nodeOwner(n_node))
        allocate(elemId(n_elem), elemType(n_elem), elemConn(n_elem*4))

        do i = 1, n_node
            nodeId(i) = i
        enddo
        do i = 1, 3
            do j = 1, 3
                l = (i-1)*3+j
                nodeCoord(2*l-1) = i
                nodeCoord(2*l) = j
            enddo
        enddo
        nodeOwner = 0
        do i = 1, n_elem
            elemId(i) = i
            elemType(i) = 9             ! Quard
        enddo
        elemConn = conn

        mesh = ESMF_MeshCreate(2,2,nodeId, nodeCoord, nodeOwner, elemId, elemType,elemConn,rc)
        if(rc/=ESMF_SUCCESS) return

        call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_I4, rc=rc)
        if (rc .ne. ESMF_SUCCESS) return

        humidity = ESMF_FieldCreate(mesh, arrayspec, &
            name="humidity", rc=rc)
        if (rc .ne. ESMF_SUCCESS) return

        call ESMF_FieldGet(humidity, localDe=0, farray=srcfptr, rc=rc)
        if (rc .ne. ESMF_SUCCESS) return

        srcfptr = 1

        call ESMF_StateAdd(exportState, humidity, rc=rc)
        if (rc .ne. ESMF_SUCCESS) return
     !   call ESMF_StatePrint(exportState, rc=rc)

        print *, de_id, "User Comp 1 Init returning"

    end subroutine user_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
    subroutine user_run(comp, importState, exportState, clock, rc)
        type(ESMF_GridComp), intent(inout) :: comp
        type(ESMF_State), intent(inout) :: importState, exportState
        type(ESMF_Clock), intent(in) :: clock
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
        type(ESMF_GridComp), intent(inout) :: comp
        type(ESMF_State), intent(inout) :: importState, exportState
        type(ESMF_Clock), intent(in) :: clock
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

        call ESMF_FieldDestroy(humidity, rc)
        if(rc/=ESMF_SUCCESS) return
        call ESMF_MeshDestroy(mesh, rc)
        if(rc/=ESMF_SUCCESS) return
        print *, "User Comp Final returning"

    end subroutine user_final


    end module user_model1
    
!\end{verbatim}
    
