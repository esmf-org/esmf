! $Id: ESMF_FieldMeshSMMUTest.F90,v 1.2 2009/07/29 16:38:52 feiliu Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
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
    use ESMF_Mod
  
    implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter :: version = &
    '$Id: ESMF_FieldMeshSMMUTest.F90,v 1.2 2009/07/29 16:38:52 feiliu Exp $'
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
        call ESMF_Finalize(terminationflag=ESMF_ABORT)

    if (.not. ESMF_TestMinPETs(3, ESMF_SRCLINE)) &
        call ESMF_Finalize(terminationflag=ESMF_ABORT)

#ifdef ESMF_TESTEXHAUSTIVE

        !------------------------------------------------------------------------
        !E-X_UTest_Multi_Proc_Only
        call test_smm_1db(rc)
        write(failMsg, *) ""
        write(name, *) "FieldMeshSMM test using lpe for both src and dst, with halos"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#endif
    call ESMF_TestEnd(result, ESMF_SRCLINE)

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
        integer                                     :: localrc, lpe, i

        integer, pointer                            :: srcfptr(:), dstfptr(:)
        integer, pointer                            :: fptr(:)
        integer                                     :: exlb(1), exub(1)
        
        integer(ESMF_KIND_I4), allocatable          :: factorList(:)
        integer, allocatable                        :: factorIndexList(:,:)

        integer                                     :: n_node, n_elem, j, l
        integer, allocatable :: nodeId(:)
        real(ESMF_KIND_R8), allocatable :: nodeCoord(:)
        integer, allocatable :: nodeOwner(:)

        integer, allocatable :: elemId(:)
        integer, allocatable :: elemType(:)
        integer, allocatable :: elemConn(:)
        integer              :: conn(16) = (/1,2,5,4,2,3,6,5,4,5,8,7,5,6,9,8/)

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        call ESMF_VMGetCurrent(vm, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! create 2x2 mesh on a Euclidean surface and Fields
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

        mesh = ESMF_MeshCreate(2,2,nodeId, nodeCoord, nodeOwner, elemId, elemType,elemConn,localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_I4, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        srcField = ESMF_FieldCreate(mesh, arrayspec, &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_FieldGet(srcField, localDe=0, farray=srcfptr, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        srcfptr = 1

        ! a one dimensional grid whose index space is an isomorphism with the mesh's
        distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/n_node/), rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        grid = ESMF_GridCreate(distgrid=distgrid, gridEdgeLWidth=(/0/), gridEdgeUWidth=(/0/), rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        dstField = ESMF_FieldCreate(grid, arrayspec, &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_FieldGet(dstField, localDe=0, farray=dstfptr, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        dstfptr = 0

        ! initialize factorList and factorIndexList
        ! the diagonal of the 9x9 diagonal matrix on 3 PETs is (1 2 3 1 2 3 1 2 3)
        allocate(factorList(3))
        allocate(factorIndexList(2,3))
        factorList = (/1,2,3/)
        factorIndexList(1,:) = (/lpe*3+1,lpe*3+2,lpe*3+3/)
        factorIndexList(2,:) = (/lpe*3+1,lpe*3+2,lpe*3+3/)
        call ESMF_FieldSMMStore(srcField, dstField, routehandle, &
            factorList, factorIndexList, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! perform smm
        call ESMF_FieldSMM(srcField, dstField, routehandle, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! verify smm
        call ESMF_FieldGet(dstField, localDe=0, farray=fptr, &
            exclusiveLBound=exlb, exclusiveUBound=exub, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! Verify that the smm data in dstField(l) is correct.
        ! Before the smm op, the dst Field contains all 0. 
        ! The smm op reset the values to the index value, verify this is the case.
        ! This is a result of the collapsing index and matrix multiplication with
        ! the diagonal matrix A and a column vector of all 1
        !write(*, '(9I3)') l, lpe, fptr
        do i = exlb(1), exub(1)
            if(fptr(i) .ne. i) localrc = ESMF_FAILURE
        enddo
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! release SMM route handle
        call ESMF_FieldSMMRelease(routehandle, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! release all acquired resources
        call ESMF_FieldDestroy(srcField)
        call ESMF_FieldDestroy(dstField)
        call ESMF_MeshDestroy(mesh)
        call ESMF_GridDestroy(grid)
        call ESMF_DistGridDestroy(distgrid)
        deallocate(factorList, factorIndexList)
        deallocate(nodeId, nodeCoord, nodeOwner)
        deallocate(elemId, elemType, elemConn)
        rc = ESMF_SUCCESS
    end subroutine test_smm_1db
#endif

end program ESMF_FieldMeshSMMUTest
