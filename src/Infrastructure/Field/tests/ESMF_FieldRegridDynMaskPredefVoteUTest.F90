! $Id$
!
! Earth System Modeling Framework
! Copyright (c) 2002-2025, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_FieldRegridDynMaskPredefVoteUTest

!------------------------------------------------------------------------------

#define FILENAME "ESMF_FieldRegridDynMaskPredefVoteUTest.F90"
#define _VERIFY(A) if(A /= ESMF_SUCCESS) then; rc=ESMF_FAILURE; print*,__FILE__,__LINE__;return; endif
#define _VERIFY_PASS(A) if(ESMF_LogFoundError(A, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rctoReturn=rc)) return

#include "ESMF.h"

#if defined (ESMF_LAPACK)
#if defined (ESMF_LAPACK_INTERNAL)
#include "ESMF_LapackBlas.inc"
#endif
#endif


!==============================================================================
!BOPI
! !PROGRAM: ESMF_FieldRegridDynMaskTest - Unit tests for Field Regrid methods
!
! !DESCRIPTION:
!
! The code in this file drives F90 Field Regrid unit tests.
!
!EOPI
!-----------------------------------------------------------------------------
! !USES:
    use ESMF_TestMod     ! test methods
    use ESMF
    use ESMF_GridUtilMod
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan


    implicit none

    integer :: virtMemPet, physMemPet

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter :: version = &
      '$Id$'

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0

       ! individual test result code
    integer :: rc = 1

    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(512) :: name

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    !--------------------------------------
    !EX_UTest
    ! Test regridding using predefined voteMask R8R8R8
    write(failMsg, *) "Test unsuccessful"
    write(name, *) "Regrid between fields using predefined voteMask R8R8R8"

    rc = ESMF_SUCCESS
    ! do test
    call test_regridPredefinedvoteMaskR8R8R8(rc)

    ! return result
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------
    !EX_UTest
    ! Test regridding using predefined voteMask R8R8R8V
    write(failMsg, *) "Test unsuccessful"
    write(name, *) "Regrid between fields using predefined voteMask R8R8R8V"

    rc = ESMF_SUCCESS
    ! do test
    call test_regridPredefinedvoteMaskR8R8R8V(rc)

    ! return result
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------
    ! Test regridding using predefined voteMask R4R8R4
    write(failMsg, *) "Test unsuccessful"
    write(name, *) "Regrid between fields using predefined voteMask R4R8R4"

    rc = ESMF_SUCCESS
    ! do test
    call test_regridPredefinedvoteMaskR4R8R4(rc)

    ! return result
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------
    !EX_UTest
    ! Test regridding using predefined voteMask R4R8R4V
    write(failMsg, *) "Test unsuccessful"
    write(name, *) "Regrid between fields using predefined voteMask R4R8R4V"

    rc = ESMF_SUCCESS
    ! do test
    call test_regridPredefinedvoteMaskR4R8R4V(rc)

    !!return result
    !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------
    ! Test regridding using predefined voteMask R4R8R4 and ungridded dim
    write(failMsg, *) "Test unsuccessful"
    write(name, *) "Regrid between fields with ungridded dim using predefined voteMask R4R8R4"

    rc = ESMF_SUCCESS
    ! do test
    call test_regridPredefinedvoteMaskR4R8R4_ungridded(rc)

    !return result
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------
    !EX_UTest
    ! Test regridding using predefined voteMask R4R8R4V and ungridded dim
    write(failMsg, *) "Test unsuccessful"
    write(name, *) "Regrid between fields with ungridded dim using predefined voteMask R4R8R4V"

    rc = ESMF_SUCCESS
    ! do test
    call test_regridPredefinedvoteMaskR4R8R4V_ungridded(rc)

    !return result
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    call ESMF_TestEnd(ESMF_SRCLINE)

contains

#define ESMF_METHOD "ESMF_TESTS"

    subroutine test_regridPredefinedvoteMaskR8R8R8(rc)
       integer, intent(out) :: rc

       type(ESMF_Grid)  :: srcGrid, dstGrid
       type(ESMF_Field) :: srcField, dstField
       real(ESMF_KIND_R8), pointer :: srcPtr(:,:), dstPtr(:,:)

       integer :: localrc, src_nx, src_ny, dst_nx, dst_ny, localDECount, lde, i1, i2, srcTermProcessing
       integer :: def_count
       integer :: clbnd(2),cubnd(2)
       real(ESMF_KIND_R8) :: undef_value, def_value1, def_value2, zero_value
       type(ESMF_DynamicMask) :: dyn_mask
       type(ESMF_RouteHandle) :: rh
       type(ESMF_VM) :: vm
       logical :: correct

       call ESMF_VMGetGlobal(vm, rc=localrc)
       _VERIFY_PASS(localrc)

       src_nx = 20
       src_ny = 20
       dst_nx = 10
       dst_ny = 10
       def_value1 = 17.d0
       def_value2 = 47.d0
       undef_value = 20000.d0
       zero_value = 0.d0
       srcGrid = create_grid(src_nx,src_ny, 1, .true., localrc)
       _VERIFY(localrc)
       dstGrid = create_grid(dst_nx,dst_ny, 2, .false., localrc)
       _VERIFY(localrc)
       srcField = ESMF_FieldCreate(srcGrid, ESMF_TYPEKIND_R8, rc=localrc)
       _VERIFY(localrc)
       dstField = ESMF_FieldCreate(dstGrid, ESMF_TYPEKIND_R8, rc=localrc)
       _VERIFY(localrc)
       call ESMF_FieldFill(srcField, dataFillScheme="const", const1=def_value1, rc=localrc)
       _VERIFY(localrc)
       call ESMF_FieldFill(dstField, dataFillScheme="const", const1=zero_value, rc=localrc)
       _VERIFY(localrc)
       call ESMF_GridGet(srcGrid, localDECount=localDECount, rc=localrc)
       _VERIFY(localrc)
       do lde = 0,localDECount-1
          call ESMF_GridGet(srcGrid, ESMF_STAGGERLOC_CENTER, lDE, computationalLBound=clbnd, computationalUBound=cubnd, rc=localrc)
          call ESMF_FieldGet(srcField, lde, farrayPtr=srcPtr, rc=localrc)
          _VERIFY(localrc)
          do i1=clbnd(1),cubnd(1)
             do i2=clbnd(2),cubnd(2)
                if (mod(i1,2) == 0)  srcPtr(i1,i2)= def_value2
             enddo
          enddo
       enddo

       call ESMF_DynamicMaskPredefinedSetR8R8R8(dyn_mask, ESMF_PREDEFINEDDYNAMICMASK_MASKVOTE, &
        & handleAllElements=.true., dynamicSrcMaskValue=undef_value, rc=localrc)
       _VERIFY(localrc)
 
       srcTermProcessing=0 
       call ESMF_FieldRegridStore(srcField, dstField, regridMethod=ESMF_REGRIDMETHOD_CONSERVE, &
            linetype=ESMF_LINETYPE_GREAT_CIRCLE, srcTermProcessing=srcTermProcessing, routeHandle=rh, rc=localrc)
       _VERIFY(localrc) 
        call ESMF_FieldRegrid(srcField, dstField, routeHandle=rh, dynamicMask=dyn_mask, rc=localrc)
       _VERIFY(localrc) 

       call ESMF_GridGet(dstGrid, localDECount=localDECount, rc=localrc)
       _VERIFY(localrc)
       def_count = count_value_in_field_r8_2d(dstField, def_value2, 0.001d0, rc=localrc)
       _VERIFY(localrc)


       call ESMF_FieldDestroy(srcField, noGarbage=.true., rc=localrc)
       _VERIFY(localrc)
       call ESMF_FieldDestroy(dstField, noGarbage=.true., rc=localrc)
       _VERIFY(localrc)
       call ESMF_GridDestroy(srcGrid, noGarbage=.true., rc=localrc)
       _VERIFY(localrc)
       call ESMF_GridDestroy(dstGrid, noGarbage=.true., rc=localrc)
       _VERIFY(localrc)
       call ESMF_RouteHandleDestroy(rh, noGarbage=.true., rc=localrc)
       _VERIFY(localrc)

       correct = (dst_ny*dst_nx) == def_count
       if (correct) then
          rc=ESMF_SUCCESS
       else
          rc=ESMF_FAILURE
       endif

    end subroutine test_regridPredefinedvoteMaskR8R8R8

    subroutine test_regridPredefinedvoteMaskR8R8R8V(rc)
       integer, intent(out) :: rc

       type(ESMF_Grid)  :: srcGrid, dstGrid
       type(ESMF_Field) :: srcField, dstField
       real(ESMF_KIND_R8), pointer :: srcPtr(:,:), dstPtr(:,:)

       integer :: localrc, src_nx, src_ny, dst_nx, dst_ny, localDECount, lde, i1, i2, srcTermProcessing
       integer :: def_count
       integer :: clbnd(2),cubnd(2)
       real(ESMF_KIND_R8) :: undef_value, def_value1, def_value2, zero_value
       type(ESMF_DynamicMask) :: dyn_mask
       type(ESMF_RouteHandle) :: rh
       type(ESMF_VM) :: vm
       logical :: correct

       call ESMF_VMGetGlobal(vm, rc=localrc)
       _VERIFY_PASS(localrc)

       src_nx = 20
       src_ny = 20
       dst_nx = 10
       dst_ny = 10
       def_value1 = 17.d0
       def_value2 = 47.d0
       undef_value = 20000.d0
       zero_value = 0.d0
       srcGrid = create_grid(src_nx,src_ny, 1, .true., localrc)
       _VERIFY(localrc)
       dstGrid = create_grid(dst_nx,dst_ny, 2, .false., localrc)
       _VERIFY(localrc)
       srcField = ESMF_FieldCreate(srcGrid, ESMF_TYPEKIND_R8, rc=localrc)
       _VERIFY(localrc)
       dstField = ESMF_FieldCreate(dstGrid, ESMF_TYPEKIND_R8, rc=localrc)
       _VERIFY(localrc)
       call ESMF_FieldFill(srcField, dataFillScheme="const", const1=def_value1, rc=localrc)
       _VERIFY(localrc)
       call ESMF_FieldFill(dstField, dataFillScheme="const", const1=zero_value, rc=localrc)
       _VERIFY(localrc)
       call ESMF_GridGet(srcGrid, localDECount=localDECount, rc=localrc)
       _VERIFY(localrc)
       do lde = 0,localDECount-1
          call ESMF_GridGet(srcGrid, ESMF_STAGGERLOC_CENTER, lDE, computationalLBound=clbnd, computationalUBound=cubnd, rc=localrc)
          call ESMF_FieldGet(srcField, lde, farrayPtr=srcPtr, rc=localrc)
          _VERIFY(localrc)
          do i1=clbnd(1),cubnd(1)
             do i2=clbnd(2),cubnd(2)
                if (mod(i1,2) == 0)  srcPtr(i1,i2)= def_value2
             enddo
          enddo
       enddo

       call ESMF_DynamicMaskPredefinedSetR8R8R8V(dyn_mask, ESMF_PREDEFINEDDYNAMICMASK_MASKVOTE, &
        & handleAllElements=.true., dynamicSrcMaskValue=undef_value, rc=localrc)
       _VERIFY(localrc)
 
       srcTermProcessing=0 
       call ESMF_FieldRegridStore(srcField, dstField, regridMethod=ESMF_REGRIDMETHOD_CONSERVE, &
            linetype=ESMF_LINETYPE_GREAT_CIRCLE, srcTermProcessing=srcTermProcessing, routeHandle=rh, rc=localrc)
       _VERIFY(localrc) 
        call ESMF_FieldRegrid(srcField, dstField, routeHandle=rh, dynamicMask=dyn_mask, rc=localrc)
       _VERIFY(localrc) 

       call ESMF_GridGet(dstGrid, localDECount=localDECount, rc=localrc)
       _VERIFY(localrc)
       def_count = count_value_in_field_r8_2d(dstField, def_value2, 0.001d0, rc=localrc)
       _VERIFY(localrc)


       call ESMF_FieldDestroy(srcField, noGarbage=.true., rc=localrc)
       _VERIFY(localrc)
       call ESMF_FieldDestroy(dstField, noGarbage=.true., rc=localrc)
       _VERIFY(localrc)
       call ESMF_GridDestroy(srcGrid, noGarbage=.true., rc=localrc)
       _VERIFY(localrc)
       call ESMF_GridDestroy(dstGrid, noGarbage=.true., rc=localrc)
       _VERIFY(localrc)
       call ESMF_RouteHandleDestroy(rh, noGarbage=.true., rc=localrc)
       _VERIFY(localrc)

       correct = (dst_ny*dst_nx) == def_count
       if (correct) then
          rc=ESMF_SUCCESS
       else
          rc=ESMF_FAILURE
       endif

    end subroutine test_regridPredefinedvoteMaskR8R8R8V

    subroutine test_regridPredefinedvoteMaskR4R8R4(rc)
       integer, intent(out) :: rc

       type(ESMF_Grid)  :: srcGrid, dstGrid
       type(ESMF_Field) :: srcField, dstField
       real(ESMF_KIND_R4), pointer :: srcPtr(:,:), dstPtr(:,:)

       integer :: localrc, src_nx, src_ny, dst_nx, dst_ny, localDECount, lde, i0, i1, i2, srcTermProcessing
       integer :: def_count
       integer :: clbnd(2),cubnd(2)
       real(ESMF_KIND_R4) :: undef_value, def_value1, def_value2, zero_value
       type(ESMF_DynamicMask) :: dyn_mask
       type(ESMF_RouteHandle) :: rh
       type(ESMF_VM) :: vm
       logical :: correct

       call ESMF_VMGetGlobal(vm, rc=localrc)
       _VERIFY_PASS(localrc)

       src_nx = 20
       src_ny = 20
       dst_nx = 10
       dst_ny = 10
       def_value1 = 17.0
       def_value2 = 47.0
       undef_value = 20000.0
       zero_value = 0.0
       srcGrid = create_grid(src_nx,src_ny, 1, .true., localrc)
       _VERIFY(localrc)
       dstGrid = create_grid(dst_nx,dst_ny, 2, .false., localrc)
       _VERIFY(localrc)
       srcField = ESMF_FieldCreate(srcGrid, ESMF_TYPEKIND_R4, rc=localrc)
       _VERIFY(localrc)
       dstField = ESMF_FieldCreate(dstGrid, ESMF_TYPEKIND_R4, rc=localrc)
       _VERIFY(localrc)
       call ESMF_FieldFill(srcField, dataFillScheme="const", const1=real(def_value1,kind=ESMF_KIND_R8), rc=localrc)
       _VERIFY(localrc)
       call ESMF_FieldFill(dstField, dataFillScheme="const", const1=real(zero_value,kind=ESMF_KIND_R8), rc=localrc)
       _VERIFY(localrc)
       call ESMF_GridGet(srcGrid, localDECount=localDECount, rc=localrc)
       _VERIFY(localrc)
       do lde = 0,localDECount-1
          call ESMF_GridGet(srcGrid, ESMF_STAGGERLOC_CENTER, lDE, computationalLBound=clbnd, computationalUBound=cubnd, rc=localrc)
          call ESMF_FieldGet(srcField, lde, farrayPtr=srcPtr, rc=localrc)
          _VERIFY(localrc)
          do i1=clbnd(1),cubnd(1)
             do i2=clbnd(2),cubnd(2)
                if (mod(i1,2) == 0)  srcPtr(i1,i2)= def_value2
             enddo
          enddo
       enddo

       call ESMF_DynamicMaskPredefinedSetR4R8R4(dyn_mask, ESMF_PREDEFINEDDYNAMICMASK_MASKVOTE, &
        & handleAllElements=.true., dynamicSrcMaskValue=undef_value, rc=localrc)
       _VERIFY(localrc)
 
       srcTermProcessing=0 
       call ESMF_FieldRegridStore(srcField, dstField, regridMethod=ESMF_REGRIDMETHOD_CONSERVE, &
            linetype=ESMF_LINETYPE_GREAT_CIRCLE, srcTermProcessing=srcTermProcessing, routeHandle=rh, rc=localrc)
       _VERIFY(localrc) 
        call ESMF_FieldRegrid(srcField, dstField, routeHandle=rh, dynamicMask=dyn_mask, rc=localrc)
       _VERIFY(localrc) 

       call ESMF_GridGet(dstGrid, localDECount=localDECount, rc=localrc)
       _VERIFY(localrc)
       def_count = count_value_in_field_r4_2d(dstField, def_value2, 0.001, rc=localrc)
       _VERIFY(localrc)


       call ESMF_FieldDestroy(srcField, noGarbage=.true., rc=localrc)
       _VERIFY(localrc)
       call ESMF_FieldDestroy(dstField, noGarbage=.true., rc=localrc)
       _VERIFY(localrc)
       call ESMF_GridDestroy(srcGrid, noGarbage=.true., rc=localrc)
       _VERIFY(localrc)
       call ESMF_GridDestroy(dstGrid, noGarbage=.true., rc=localrc)
       _VERIFY(localrc)
       call ESMF_RouteHandleDestroy(rh, noGarbage=.true., rc=localrc)
       _VERIFY(localrc)

       correct = (dst_ny*dst_nx) == def_count
       if (correct) then
          rc=ESMF_SUCCESS
       else
          rc=ESMF_FAILURE
       endif

    end subroutine test_regridPredefinedvoteMaskR4R8R4

    subroutine test_regridPredefinedvoteMaskR4R8R4V(rc)
       integer, intent(out) :: rc

       type(ESMF_Grid)  :: srcGrid, dstGrid
       type(ESMF_Field) :: srcField, dstField
       real(ESMF_KIND_R4), pointer :: srcPtr(:,:), dstPtr(:,:)

       integer :: localrc, src_nx, src_ny, dst_nx, dst_ny, localDECount, lde, i0, i1, i2, srcTermProcessing, lm
       integer :: def_count
       integer :: clbnd(2),cubnd(2)
       real(ESMF_KIND_R4) :: undef_value, def_value1, def_value2, zero_value
       type(ESMF_DynamicMask) :: dyn_mask
       type(ESMF_RouteHandle) :: rh
       type(ESMF_VM) :: vm
       logical :: correct

       call ESMF_VMGetGlobal(vm, rc=localrc)
       _VERIFY_PASS(localrc)

       src_nx = 20
       src_ny = 20
       dst_nx = 10
       dst_ny = 10
       def_value1 = 17.0
       def_value2 = 47.0
       undef_value = 20000.0
       zero_value = 0.d0
       lm = 3
       srcGrid = create_grid(src_nx,src_ny, 1, .true., localrc)
       _VERIFY(localrc)
       dstGrid = create_grid(dst_nx,dst_ny, 2, .false., localrc)
       _VERIFY(localrc)
       srcField = ESMF_FieldCreate(srcGrid, ESMF_TYPEKIND_R4, gridToFieldMap=[2,3], ungriddedLBound=[1], ungriddedUBound=[lm], rc=localrc)
       _VERIFY(localrc)
       dstField = ESMF_FieldCreate(dstGrid, ESMF_TYPEKIND_R4, gridToFieldMap=[2,3], ungriddedLBound=[1], ungriddedUBound=[lm], rc=localrc)
       _VERIFY(localrc)
       call ESMF_FieldFill(srcField, dataFillScheme="const", const1=real(def_value1,kind=ESMF_KIND_R8), rc=localrc)
       _VERIFY(localrc)
       call ESMF_FieldFill(dstField, dataFillScheme="const", const1=real(zero_value,kind=ESMF_KIND_R8), rc=localrc)
       _VERIFY(localrc)
       call ESMF_GridGet(srcGrid, localDECount=localDECount, rc=localrc)
       _VERIFY(localrc)
       do lde = 0,localDECount-1
          call ESMF_GridGet(srcGrid, ESMF_STAGGERLOC_CENTER, lDE, computationalLBound=clbnd, computationalUBound=cubnd, rc=localrc)
          call ESMF_FieldGet(srcField, lde, farrayPtr=srcPtr, rc=localrc)
          _VERIFY(localrc)
          do i0=1,lm
             do i1=clbnd(1),cubnd(1)
                do i2=clbnd(2),cubnd(2)
                   if (mod(i1,2) == 0)  srcPtr(i1,i2)= def_value2
                enddo
             enddo
          enddo
       enddo

       call ESMF_DynamicMaskPredefinedSetR4R8R4V(dyn_mask, ESMF_PREDEFINEDDYNAMICMASK_MASKVOTE, &
        & handleAllElements=.true., dynamicSrcMaskValue=undef_value, rc=localrc)
       _VERIFY(localrc)
 
       srcTermProcessing=0 
       call ESMF_FieldRegridStore(srcField, dstField, regridMethod=ESMF_REGRIDMETHOD_CONSERVE, &
            linetype=ESMF_LINETYPE_GREAT_CIRCLE, srcTermProcessing=srcTermProcessing, routeHandle=rh, rc=localrc)
       _VERIFY(localrc) 
        call ESMF_FieldRegrid(srcField, dstField, routeHandle=rh, dynamicMask=dyn_mask, rc=localrc)
       _VERIFY(localrc) 

       call ESMF_GridGet(dstGrid, localDECount=localDECount, rc=localrc)
       _VERIFY(localrc)
       def_count = count_value_in_field_r4_3d(dstField, def_value2, 0.001, rc=localrc)
       _VERIFY(localrc)


       call ESMF_FieldDestroy(srcField, noGarbage=.true., rc=localrc)
       _VERIFY(localrc)
       call ESMF_FieldDestroy(dstField, noGarbage=.true., rc=localrc)
       _VERIFY(localrc)
       call ESMF_GridDestroy(srcGrid, noGarbage=.true., rc=localrc)
       _VERIFY(localrc)
       call ESMF_GridDestroy(dstGrid, noGarbage=.true., rc=localrc)
       _VERIFY(localrc)
       call ESMF_RouteHandleDestroy(rh, noGarbage=.true., rc=localrc)
       _VERIFY(localrc)

       correct = (dst_ny*dst_nx) == def_count
       if (correct) then
          rc=ESMF_SUCCESS
       else
          rc=ESMF_FAILURE
       endif

    end subroutine test_regridPredefinedvoteMaskR4R8R4V

    subroutine test_regridPredefinedvoteMaskR4R8R4_ungridded(rc)
       integer, intent(out) :: rc

       type(ESMF_Grid)  :: srcGrid, dstGrid
       type(ESMF_Field) :: srcField, dstField
       real(ESMF_KIND_R4), pointer :: srcPtr(:,:), dstPtr(:,:)

       integer :: localrc, src_nx, src_ny, dst_nx, dst_ny, localDECount, lde, i1, i2, srcTermProcessing
       integer :: def_count
       integer :: clbnd(2),cubnd(2)
       real(ESMF_KIND_R4) :: undef_value, def_value1, def_value2, zero_value
       type(ESMF_DynamicMask) :: dyn_mask
       type(ESMF_RouteHandle) :: rh
       type(ESMF_VM) :: vm
       logical :: correct

       call ESMF_VMGetGlobal(vm, rc=localrc)
       _VERIFY_PASS(localrc)

       src_nx = 20
       src_ny = 20
       dst_nx = 10
       dst_ny = 10
       def_value1 = 17.0
       def_value2 = 47.0
       undef_value = 20000.0
       zero_value = 0.0
       srcGrid = create_grid(src_nx,src_ny, 1, .true., localrc)
       _VERIFY(localrc)
       dstGrid = create_grid(dst_nx,dst_ny, 2, .false., localrc)
       _VERIFY(localrc)
       srcField = ESMF_FieldCreate(srcGrid, ESMF_TYPEKIND_R4, rc=localrc)
       _VERIFY(localrc)
       dstField = ESMF_FieldCreate(dstGrid, ESMF_TYPEKIND_R4, rc=localrc)
       _VERIFY(localrc)
       call ESMF_FieldFill(srcField, dataFillScheme="const", const1=real(def_value1,kind=ESMF_KIND_R8), rc=localrc)
       _VERIFY(localrc)
       call ESMF_FieldFill(dstField, dataFillScheme="const", const1=real(zero_value,kind=ESMF_KIND_R8), rc=localrc)
       _VERIFY(localrc)
       call ESMF_GridGet(srcGrid, localDECount=localDECount, rc=localrc)
       _VERIFY(localrc)
       do lde = 0,localDECount-1
          call ESMF_GridGet(srcGrid, ESMF_STAGGERLOC_CENTER, lDE, computationalLBound=clbnd, computationalUBound=cubnd, rc=localrc)
          call ESMF_FieldGet(srcField, lde, farrayPtr=srcPtr, rc=localrc)
          _VERIFY(localrc)
          do i1=clbnd(1),cubnd(1)
             do i2=clbnd(2),cubnd(2)
                if (mod(i1,2) == 0)  srcPtr(i1,i2)= def_value2
             enddo
          enddo
       enddo

       call ESMF_DynamicMaskPredefinedSetR4R8R4(dyn_mask, ESMF_PREDEFINEDDYNAMICMASK_MASKVOTE, &
        & handleAllElements=.true., dynamicSrcMaskValue=undef_value, rc=localrc)
       _VERIFY(localrc)
 
       srcTermProcessing=0 
       call ESMF_FieldRegridStore(srcField, dstField, regridMethod=ESMF_REGRIDMETHOD_CONSERVE, &
            linetype=ESMF_LINETYPE_GREAT_CIRCLE, srcTermProcessing=srcTermProcessing, routeHandle=rh, rc=localrc)
       _VERIFY(localrc) 
        call ESMF_FieldRegrid(srcField, dstField, routeHandle=rh, dynamicMask=dyn_mask, rc=localrc)
       _VERIFY(localrc) 

       call ESMF_GridGet(dstGrid, localDECount=localDECount, rc=localrc)
       _VERIFY(localrc)
       def_count = count_value_in_field_r4_2d(dstField, def_value2, 0.001, rc=localrc)
       _VERIFY(localrc)


       call ESMF_FieldDestroy(srcField, noGarbage=.true., rc=localrc)
       _VERIFY(localrc)
       call ESMF_FieldDestroy(dstField, noGarbage=.true., rc=localrc)
       _VERIFY(localrc)
       call ESMF_GridDestroy(srcGrid, noGarbage=.true., rc=localrc)
       _VERIFY(localrc)
       call ESMF_GridDestroy(dstGrid, noGarbage=.true., rc=localrc)
       _VERIFY(localrc)
       call ESMF_RouteHandleDestroy(rh, noGarbage=.true., rc=localrc)
       _VERIFY(localrc)

       correct = (dst_ny*dst_nx) == def_count
       if (correct) then
          rc=ESMF_SUCCESS
       else
          rc=ESMF_FAILURE
       endif

    end subroutine test_regridPredefinedvoteMaskR4R8R4_ungridded

    subroutine test_regridPredefinedvoteMaskR4R8R4V_ungridded(rc)
       integer, intent(out) :: rc

       type(ESMF_Grid)  :: srcGrid, dstGrid
       type(ESMF_Field) :: srcField, dstField
       real(ESMF_KIND_R4), pointer :: srcPtr(:,:), dstPtr(:,:)

       integer :: localrc, src_nx, src_ny, dst_nx, dst_ny, localDECount, lde, i1, i2, srcTermProcessing
       integer :: def_count
       integer :: clbnd(2),cubnd(2)
       real(ESMF_KIND_R4) :: undef_value, def_value1, def_value2, zero_value
       type(ESMF_DynamicMask) :: dyn_mask
       type(ESMF_RouteHandle) :: rh
       type(ESMF_VM) :: vm
       logical :: correct

       call ESMF_VMGetGlobal(vm, rc=localrc)
       _VERIFY_PASS(localrc)

       src_nx = 20
       src_ny = 20
       dst_nx = 10
       dst_ny = 10
       def_value1 = 17.0
       def_value2 = 47.0
       undef_value = 20000.0
       zero_value = 0.d0
       srcGrid = create_grid(src_nx,src_ny, 1, .true., localrc)
       _VERIFY(localrc)
       dstGrid = create_grid(dst_nx,dst_ny, 2, .false., localrc)
       _VERIFY(localrc)
       srcField = ESMF_FieldCreate(srcGrid, ESMF_TYPEKIND_R4, rc=localrc)
       _VERIFY(localrc)
       dstField = ESMF_FieldCreate(dstGrid, ESMF_TYPEKIND_R4, rc=localrc)
       _VERIFY(localrc)
       call ESMF_FieldFill(srcField, dataFillScheme="const", const1=real(def_value1,kind=ESMF_KIND_R8), rc=localrc)
       _VERIFY(localrc)
       call ESMF_FieldFill(dstField, dataFillScheme="const", const1=real(zero_value,kind=ESMF_KIND_R8), rc=localrc)
       _VERIFY(localrc)
       call ESMF_GridGet(srcGrid, localDECount=localDECount, rc=localrc)
       _VERIFY(localrc)
       do lde = 0,localDECount-1
          call ESMF_GridGet(srcGrid, ESMF_STAGGERLOC_CENTER, lDE, computationalLBound=clbnd, computationalUBound=cubnd, rc=localrc)
          call ESMF_FieldGet(srcField, lde, farrayPtr=srcPtr, rc=localrc)
          _VERIFY(localrc)
          do i1=clbnd(1),cubnd(1)
             do i2=clbnd(2),cubnd(2)
                if (mod(i1,2) == 0)  srcPtr(i1,i2)= def_value2
             enddo
          enddo
       enddo

       call ESMF_DynamicMaskPredefinedSetR4R8R4V(dyn_mask, ESMF_PREDEFINEDDYNAMICMASK_MASKVOTE, &
        & handleAllElements=.true., dynamicSrcMaskValue=undef_value, rc=localrc)
       _VERIFY(localrc)
 
       srcTermProcessing=0 
       call ESMF_FieldRegridStore(srcField, dstField, regridMethod=ESMF_REGRIDMETHOD_CONSERVE, &
            linetype=ESMF_LINETYPE_GREAT_CIRCLE, srcTermProcessing=srcTermProcessing, routeHandle=rh, rc=localrc)
       _VERIFY(localrc) 
        call ESMF_FieldRegrid(srcField, dstField, routeHandle=rh, dynamicMask=dyn_mask, rc=localrc)
       _VERIFY(localrc) 

       call ESMF_GridGet(dstGrid, localDECount=localDECount, rc=localrc)
       _VERIFY(localrc)
       def_count = count_value_in_field_r4_2d(dstField, def_value2, 0.001, rc=localrc)
       _VERIFY(localrc)


       call ESMF_FieldDestroy(srcField, noGarbage=.true., rc=localrc)
       _VERIFY(localrc)
       call ESMF_FieldDestroy(dstField, noGarbage=.true., rc=localrc)
       _VERIFY(localrc)
       call ESMF_GridDestroy(srcGrid, noGarbage=.true., rc=localrc)
       _VERIFY(localrc)
       call ESMF_GridDestroy(dstGrid, noGarbage=.true., rc=localrc)
       _VERIFY(localrc)
       call ESMF_RouteHandleDestroy(rh, noGarbage=.true., rc=localrc)
       _VERIFY(localrc)

       correct = (dst_ny*dst_nx) == def_count
       if (correct) then
          rc=ESMF_SUCCESS
       else
          rc=ESMF_FAILURE
       endif

    end subroutine test_regridPredefinedvoteMaskR4R8R4V_ungridded

    function count_value_in_field_r8_2d(field, value, tolerance, rc) result(num_found)
       integer :: num_found
       type(ESMF_Field), intent(in) :: field 
       real(ESMF_KIND_R8), intent(in) :: value       
       real(ESMF_KIND_R8), intent(in) :: tolerance
       integer, intent(out) :: rc
       
       type(ESMF_Grid) :: grid
       integer :: localrc, localDECount, lDE
       integer :: clbnd(2),cubnd(2), i1, i2
       integer :: val_count, global_val_count(1) 
       type(ESMF_VM) :: vm
       real(ESMF_KIND_R8), pointer :: ptr(:,:)

       call ESMF_VMGetGlobal(vm, rc=localrc)
       _VERIFY_PASS(localrc)
       call ESMF_FieldGet(field, grid=grid, rc=localrc)
       _VERIFY(localrc) 
       call ESMF_GridGet(grid, localDECount=localDECount, rc=localrc)
       _VERIFY(localrc) 
       val_count = 0
       do lde = 0,localDECount-1
          call ESMF_GridGet(grid, ESMF_STAGGERLOC_CENTER, lDE, computationalLBound=clbnd, computationalUBound=cubnd, rc=localrc)
          call ESMF_FieldGet(field, lde, farrayPtr=ptr, rc=localrc)
          _VERIFY(localrc)
          do i1=clbnd(1),cubnd(1)
             do i2=clbnd(2),cubnd(2)
                if (value - tolerance < ptr(i1,i2) .and. ptr(i1,i2) < value + tolerance) val_count=val_count+1
             enddo
          enddo
       enddo

       call ESMF_VMAllReduce(vm, [val_count], global_val_count, 1, ESMF_REDUCE_SUM, rc=localrc)
       _VERIFY(localrc)

       num_found = global_val_count(1)

       rc=ESMF_SUCCESS
    end function count_value_in_field_r8_2d

    function count_value_in_field_r4_2d(field, value, tolerance, rc) result(num_found)
       integer :: num_found
       type(ESMF_Field), intent(in) :: field 
       real(ESMF_KIND_R4), intent(in) :: value       
       real(ESMF_KIND_R4), intent(in) :: tolerance
       integer, intent(out) :: rc
       
       type(ESMF_Grid) :: grid
       integer :: localrc, localDECount, lDE
       integer :: clbnd(2),cubnd(2), i1, i2
       integer :: val_count, global_val_count(1) 
       type(ESMF_VM) :: vm
       real(ESMF_KIND_R4), pointer :: ptr(:,:)

       call ESMF_VMGetGlobal(vm, rc=localrc)
       _VERIFY_PASS(localrc)
       call ESMF_FieldGet(field, grid=grid, rc=localrc)
       _VERIFY(localrc) 
       call ESMF_GridGet(grid, localDECount=localDECount, rc=localrc)
       _VERIFY(localrc) 
       val_count = 0
       do lde = 0,localDECount-1
          call ESMF_GridGet(grid, ESMF_STAGGERLOC_CENTER, lDE, computationalLBound=clbnd, computationalUBound=cubnd, rc=localrc)
          call ESMF_FieldGet(field, lde, farrayPtr=ptr, rc=localrc)
          _VERIFY(localrc)
          do i1=clbnd(1),cubnd(1)
             do i2=clbnd(2),cubnd(2)
                if (value - tolerance < ptr(i1,i2) .and. ptr(i1,i2) < value + tolerance) val_count=val_count+1
             enddo
          enddo
       enddo

       call ESMF_VMAllReduce(vm, [val_count], global_val_count, 1, ESMF_REDUCE_SUM, rc=localrc)
       _VERIFY(localrc)

       num_found = global_val_count(1)

       rc=ESMF_SUCCESS
    end function count_value_in_field_r4_2d

    function count_value_in_field_r4_3d(field, value, tolerance, rc) result(num_found)
       integer :: num_found
       type(ESMF_Field), intent(in) :: field 
       real(ESMF_KIND_R4), intent(in) :: value       
       real(ESMF_KIND_R4), intent(in) :: tolerance
       integer, intent(out) :: rc
       
       type(ESMF_Grid) :: grid
       integer :: localrc, localDECount, lDE
       integer :: clbnd(2),cubnd(2), i1, i2, i0
       integer :: val_count, global_val_count(1) 
       type(ESMF_VM) :: vm
       real(ESMF_KIND_R4), pointer :: ptr(:,:,:)

       call ESMF_VMGetGlobal(vm, rc=localrc)
       _VERIFY_PASS(localrc)
       call ESMF_FieldGet(field, grid=grid, rc=localrc)
       _VERIFY(localrc) 
       call ESMF_GridGet(grid, localDECount=localDECount, rc=localrc)
       _VERIFY(localrc) 
       val_count = 0
       do lde = 0,localDECount-1
          call ESMF_GridGet(grid, ESMF_STAGGERLOC_CENTER, lDE, computationalLBound=clbnd, computationalUBound=cubnd, rc=localrc)
          call ESMF_FieldGet(field, lde, farrayPtr=ptr, rc=localrc)
          _VERIFY(localrc)
          do i0=1,size(ptr,1)
             do i1=clbnd(1),cubnd(1)
                do i2=clbnd(2),cubnd(2)
                   if (value - tolerance < ptr(i0,i1,i2) .and. ptr(i0,i1,i2) < value + tolerance) val_count=val_count+1
                enddo 
             enddo
          enddo
       enddo

       call ESMF_VMAllReduce(vm, [val_count], global_val_count, 1, ESMF_REDUCE_SUM, rc=localrc)
       _VERIFY(localrc)

       num_found = global_val_count(1)

       rc=ESMF_SUCCESS
    end function count_value_in_field_r4_3d

    function create_grid(nx, ny, distributed_dim, shift, rc) result(grid)
       type(ESMF_Grid) :: grid
       integer, intent(in) :: nx
       integer, intent(in) :: ny
       integer, intent(in) :: distributed_dim
       logical, intent(in) :: shift
       integer, intent(out) :: rc

       integer :: localrc, petCount, i1, i2, localPet, localDECount, lde
       real(ESMF_KIND_R8), pointer :: farrayPtrX(:,:)
       real(ESMF_KIND_R8), pointer :: farrayPtrY(:,:)
       real(ESMF_KIND_R8) :: dx, dy, dx2, dx4
       integer :: clbnd(2),cubnd(2), regDecomp(2)
       type(ESMF_VM) :: vm

       call ESMF_VMGetGlobal(vm, rc=localrc)
       _VERIFY_PASS(localrc)
       call ESMF_VMGet(vm, petCount=petCount, rc=localrc)
       _VERIFY_PASS(localrc)

       dx=360.0d0/nx
       dy=180.0d0/ny
       dx2=dx/2.0d0
       dx4=dx/4.0d0

       if (distributed_dim==1) regDecomp=[petCount,1]
       if (distributed_dim==2) regDecomp=[1,petCount]

       Grid=ESMF_GridCreate1PeriDim(minIndex=[1,1],maxIndex=[nx,ny],regDecomp=regDecomp, &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
       _VERIFY(localrc)
       call ESMF_GridGet(grid,localDECount=localDECount, rc=localrc)
       _VERIFY(localrc)

       call ESMF_GridAddCoord(Grid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
       _VERIFY(localrc)

       do lDE=0,localDECount-1
          call ESMF_GridGetCoord(Grid, localDE=lDE, staggerloc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
                              computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrX, rc=localrc)
          _VERIFY(localrc)
          call ESMF_GridGetCoord(Grid, localDE=lDE, staggerloc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
                              computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrY, rc=localrc)
          _VERIFY(localrc)
          do i1=clbnd(1),cubnd(1)
             do i2=clbnd(2),cubnd(2)
                ! Set source coordinates as 0 to 360
                farrayPtrX(i1,i2) = REAL(i1-1)*dx
                if (mod(i1,2) == 0 .and. shift) farrayPtrX(i1,i2) = farrayPtrX(i1,i2) - dx4
                farrayPtrY(i1,i2) = -90. + REAL(i2-1)*dy
             enddo
          enddo
       enddo

       rc = ESMF_SUCCESS

    end function create_grid

end program ESMF_FieldRegridDynMaskPredefVoteUTest
