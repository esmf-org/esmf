! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_FieldMeshSMMUTest

!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
#include "ESMF_Macros.inc"
!
!==============================================================================
!BOPI
! !PROGRAM: ESMF_FieldMeshSMMUTest - This test verifies FieldMeshSMM functionality.
!
! !DESCRIPTION:
!
! The code in this file specializes on testing the usage of FiledSMM.
!EOPI
!
!-----------------------------------------------------------------------------
! !USES:
    use ESMF_TestMod     ! test methods
    use ESMF
  
    implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter :: version = &
    '$Id$'
!------------------------------------------------------------------------------

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0

    ! individual test result code
    integer :: rc = ESMF_SUCCESS

    ! individual test name
    character(ESMF_MAXSTR) :: name

    ! individual test failure messages
    character(ESMF_MAXSTR*2) :: failMsg

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
    if(rc /= ESMF_SUCCESS) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)

    if (.not. ESMF_TestMinPETs(4, ESMF_SRCLINE)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)

#ifdef ESMF_TESTEXHAUSTIVE

        !------------------------------------------------------------------------
        !E-X_UTest_Multi_Proc_Only
        call test_smm_1db(rc)
        write(failMsg, *) ""
        write(name, *) "FieldMeshSMM test using localPet for both src and dst, with halos"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#endif
    call ESMF_TestEnd(ESMF_SRCLINE)

#ifdef ESMF_TESTEXHAUSTIVE

contains

#undef ESMF_METHOD
#define ESMF_METHOD "test_smm_1d"
 
    subroutine test_smm_1db(rc)
        integer, intent(out)                        :: rc

        ! local arguments used to create field etc
        type(ESMF_Field)                            :: srcField, dstField
        type(ESMF_Mesh)                             :: mesh
        type(ESMF_Grid)                             :: grid
        type(ESMF_DistGrid)                         :: distgrid
        type(ESMF_VM)                               :: vm
        type(ESMF_RouteHandle)                      :: routehandle
        type(ESMF_ArraySpec)                        :: arrayspec
        integer                                     :: finalrc

        integer, pointer                            :: srcfptr(:), dstfptr(:)
        integer, pointer                            :: fptr(:)
        integer                                     :: exlb(1), exub(1)
        
        integer(ESMF_KIND_I4), allocatable          :: factorList(:)
        integer, allocatable                        :: factorIndexList(:,:)

        integer                         :: i, localPet, petCount

        integer, pointer :: nodeIds(:),nodeOwners(:)
        real(ESMF_KIND_R8), pointer :: nodeCoords(:)
        integer :: numNodes
        integer :: numElems
        integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)

        rc = ESMF_SUCCESS
        finalrc = ESMF_SUCCESS

        ! get global VM
        call ESMF_VMGetGlobal(vm, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

       if (petCount .eq. 4) then
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
                 nodeIds=nodeIds, nodeCoords=nodeCoords, &
                 nodeOwners=nodeOwners, elementIds=elemIds,&
                 elementTypes=elemTypes, elementConn=elemConn, &
                 rc=rc)
          if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

          ! deallocate node data
          deallocate(nodeIds)
          deallocate(nodeCoords)
          deallocate(nodeOwners)

          ! deallocate elem data
          deallocate(elemIds)
          deallocate(elemTypes)
          deallocate(elemConn)
          call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_I4, rc=rc)
          if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

          srcField = ESMF_FieldCreate(mesh, arrayspec, &
              rc=rc)
          if (ESMF_LogFoundError(rc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return

          call ESMF_FieldGet(srcField, localDe=0, farrayPtr=srcfptr, rc=rc)
          if (ESMF_LogFoundError(rc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return

          do i = lbound(srcfptr, 1), ubound(srcfptr, 1)
              srcfptr(i) = i
          enddo

          ! a one dimensional grid whose index space is an isomorphism with the mesh's
          distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/9/), rc=rc)
          if (ESMF_LogFoundError(rc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return

          grid = ESMF_GridCreate(distgrid=distgrid, rc=rc)
          if (ESMF_LogFoundError(rc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return

          dstField = ESMF_FieldCreate(grid, arrayspec, rc=rc)
          if (ESMF_LogFoundError(rc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return

          call ESMF_FieldGet(dstField, localDe=0, farrayPtr=dstfptr, rc=rc)
          if (ESMF_LogFoundError(rc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return

          dstfptr = 0

          ! initialize factorList and factorIndexList
          ! Nodal distribution of indices:
          ! 1 3 1
          ! 2 4 2
          ! 1 2 1
          ! src data = ((1 2 3 4) (1 2) (1 2) (1))
          ! the diagonal of the 9x9 diagonal matrix on 4 PETs is ((1 2 3) (1 2) (1 2) (1 4))
          ! result = ((1 4 9) (1 4) (1 4) (1 4))
          if (localPet == 0) then
              ! 4 -> 3
              allocate(factorList(3))
              allocate(factorIndexList(2,3))
              factorList = (/1,2,3/)
              factorIndexList(1,:) = (/1, 2, 4/)
              factorIndexList(2,:) = (/1, 2, 3/)
              call ESMF_FieldSMMStore(srcField, dstField, routehandle, &
                  factorList, factorIndexList, rc=rc)
              if (ESMF_LogFoundError(rc,  ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return
              deallocate(factorList, factorIndexList)
          else if (localPet == 1) then
              ! 2 -> 2
              allocate(factorList(2))
              allocate(factorIndexList(2,2))
              factorList = (/1,2/)
              factorIndexList(1,:) = (/3, 6/)
              factorIndexList(2,:) = (/4, 5/)
              call ESMF_FieldSMMStore(srcField, dstField, routehandle, &
                  factorList, factorIndexList, rc=rc)
              if (ESMF_LogFoundError(rc,  ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return
              deallocate(factorList, factorIndexList)
          else if (localPet == 2) then
              ! 2 -> 2
              allocate(factorList(2))
              allocate(factorIndexList(2,2))
              factorList = (/1,2/)
              factorIndexList(1,:) = (/7, 8/)
              factorIndexList(2,:) = (/6, 7/)
              call ESMF_FieldSMMStore(srcField, dstField, routehandle, &
                  factorList, factorIndexList, rc=rc)
              if (ESMF_LogFoundError(rc,  ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return
              deallocate(factorList, factorIndexList)
          else if (localPet == 3) then
              ! 1 -> 2
              allocate(factorList(2))
              allocate(factorIndexList(2,2))
              factorList = (/1,4/)
              factorIndexList(1,:) = (/9,9/)
              factorIndexList(2,:) = (/8,9/)
              call ESMF_FieldSMMStore(srcField, dstField, routehandle, &
                  factorList, factorIndexList, rc=rc)
              if (ESMF_LogFoundError(rc,  ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return
              deallocate(factorList, factorIndexList)
          endif

          ! perform smm
          call ESMF_FieldSMM(srcField, dstField, routehandle, rc=rc)
          if (ESMF_LogFoundError(rc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return

          ! verify smm
          call ESMF_FieldGet(dstField, localDe=0, farrayPtr=fptr, &
              exclusiveLBound=exlb, exclusiveUBound=exub, rc=rc)
          if (ESMF_LogFoundError(rc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return

          ! Verify that the smm data in dstField(l) is correct.
          ! Before the smm op, the dst Field contains all 0. 
          ! The smm op reset the values to the index value, verify this is the case.
          ! This is a result of the collapsing index and matrix multiplication with
          ! the diagonal matrix A and a column vector of all 1
          !write(*, '(9I3)') localPet, fptr
          do i = exlb(1), exub(1)
              if(fptr(i) .ne. i*i) finalrc = ESMF_FAILURE
          enddo
          if (ESMF_LogFoundError(rc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return

          ! release SMM route handle
          call ESMF_FieldSMMRelease(routehandle, rc=rc)
          if (ESMF_LogFoundError(rc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return

          ! release all acquired resources
          call ESMF_FieldDestroy(srcField)
          call ESMF_FieldDestroy(dstField)
          call ESMF_MeshDestroy(mesh)
          call ESMF_GridDestroy(grid)
          call ESMF_DistGridDestroy(distgrid)
        ! endif for skip for != 4 procs
        endif 
        rc = finalrc
    end subroutine test_smm_1db
#endif

end program ESMF_FieldMeshSMMUTest
