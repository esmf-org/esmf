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
      program ESMF_FieldRegridDynMaskTest

!------------------------------------------------------------------------------

#define FILENAME "ESMF_FieldRegridDynMaskTest.F90"
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
    ! Test regridding using predefined SrcMask R8R8R8
    write(failMsg, *) "Test unsuccessful"
    write(name, *) "Regrid between fields using predefined SrcMask R8R8R8"

    rc = ESMF_SUCCESS
    ! do test
    call test_regridPredefinedSrcMaskR8R8R8(rc)

    ! return result
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    call ESMF_TestEnd(ESMF_SRCLINE)

contains

#define ESMF_METHOD "ESMF_TESTS"

    subroutine test_regridPredefinedSrcMaskR8R8R8(rc)
       integer, intent(out) :: rc

       type(ESMF_Grid)  :: srcGrid, dstGrid
       type(ESMF_Field) :: srcField, dstField
       real(ESMF_KIND_R8), pointer :: srcPtr(:,:), dstPtr(:,:)

       integer :: localrc, src_nx, src_ny, dst_nx, dst_ny, localDECount, lde, i1, i2, srcTermProcessing
       integer :: def_count, undef_count, total_count, global_def_count(1), global_undef_count(1) 
       real(ESMF_KIND_R8) :: undef_value, def_value, zero_value, delta
       integer :: clbnd(2),cubnd(2)
       type(ESMF_DynamicMask) :: dyn_mask
       type(ESMF_RouteHandle) :: rh
       type(ESMF_VM) :: vm
       logical :: correct

       call ESMF_VMGetGlobal(vm, rc=localrc)
       _VERIFY_PASS(localrc)

       src_nx = 20
       src_ny = 20
       dst_nx = 27
       dst_ny = 27
       def_value = 17.d0
       undef_value = 20000.d0
       zero_value = 0.d0
       srcGrid = create_grid(src_nx,src_ny, 1, localrc)
       _VERIFY(localrc)
       dstGrid = create_grid(dst_nx,dst_ny, 2, localrc)
       _VERIFY(localrc)
       srcField = ESMF_FieldCreate(srcGrid, ESMF_TYPEKIND_R8, rc=localrc)
       _VERIFY(localrc)
       dstField = ESMF_FieldCreate(dstGrid, ESMF_TYPEKIND_R8, rc=localrc)
       _VERIFY(localrc)
       call ESMF_FieldFill(srcField, dataFillScheme="const", const1=def_value, rc=localrc)
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
                if (i1 .lt. src_nx/2 .and. i2 .lt. src_ny/2) srcPtr(i1,i2)= undef_value
             enddo
          enddo
       enddo

       call ESMF_DynamicMaskPredefinedSetR8R8R8(dyn_mask, ESMF_PREDEFINEDDYNAMICMASK_MASKSRC, &
        & dynamicSrcMaskValue=undef_value, rc=localrc)
       _VERIFY(localrc)
 
       srcTermProcessing=0 
       call ESMF_FieldRegridStore(srcField, dstField, regridMethod=ESMF_REGRIDMETHOD_BILINEAR, &
            linetype=ESMF_LINETYPE_GREAT_CIRCLE, srcTermProcessing=srcTermProcessing, routeHandle=rh, rc=localrc)
       _VERIFY(localrc) 
        call ESMF_FieldRegrid(srcField, dstField, routeHandle=rh, dynamicMask=dyn_mask, rc=localrc)
       _VERIFY(localrc) 

       call ESMF_GridGet(dstGrid, localDECount=localDECount, rc=localrc)
       _VERIFY(localrc)
       undef_count=0
       def_count=0
       delta = undef_value * 0.0001d0
       do lde = 0,localDECount-1
          call ESMF_GridGet(dstGrid, ESMF_STAGGERLOC_CENTER, lDE, computationalLBound=clbnd, computationalUBound=cubnd, rc=localrc)
          call ESMF_FieldGet(dstField, lde, farrayPtr=dstPtr, rc=localrc)
          _VERIFY(localrc)
          do i1=clbnd(1),cubnd(1)
             do i2=clbnd(2),cubnd(2)
                if (undef_value - delta < dstPtr(i1,i2) .and. dstPtr(i1,i2) < undef_value + delta) undef_count=undef_count+1
                if (def_value - delta < dstPtr(i1,i2) .and. dstPtr(i1,i2) < def_value + delta) def_count=def_count+1
             enddo
          enddo
       enddo
      
       call ESMF_VMAllReduce(vm, [undef_count], global_undef_count, 1, ESMF_REDUCE_SUM, rc=localrc)
       _VERIFY(localrc)
       call ESMF_VMAllReduce(vm, [def_count], global_def_count, 1, ESMF_REDUCE_SUM, rc=localrc)
       _VERIFY(localrc)

       correct = (dst_ny*dst_nx) == (global_def_count(1)+global_undef_count(1))
       if (correct) then
          rc=ESMF_SUCCESS
       else
          rc=ESMF_FAILURE
       endif

    end subroutine test_regridPredefinedSrcMaskR8R8R8

    function create_grid(nx, ny, distributed_dim, rc) result(grid)
       type(ESMF_Grid) :: grid
       integer, intent(in) :: nx
       integer, intent(in) :: ny
       integer, intent(in) :: distributed_dim
       integer, intent(out) :: rc

       integer :: localrc, petCount, i1, i2, localPet, localDECount, lde
       real(ESMF_KIND_R8), pointer :: farrayPtrX(:,:)
       real(ESMF_KIND_R8), pointer :: farrayPtrY(:,:)
       real(ESMF_KIND_R8) :: dx, dy
       integer :: clbnd(2),cubnd(2), regDecomp(2)
       type(ESMF_VM) :: vm

       call ESMF_VMGetGlobal(vm, rc=localrc)
       _VERIFY_PASS(localrc)
       call ESMF_VMGet(vm, petCount=petCount, rc=localrc)
       _VERIFY_PASS(localrc)

       dx=360.0/nx
       dy=180.0/ny

       if (distributed_dim==1) regDecomp=[petCount,1]
       if (distributed_dim==2) regDecomp=[1,petCount]

       Grid=ESMF_GridCreate1PeriDim(minIndex=[1,1],maxIndex=[nx,ny],regDecomp=regDecomp, &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
       _VERIFY(localrc)
       call ESMF_GridGet(grid,localDECount=localDECount, rc=localrc)
       _VERIFY(localrc)

       call ESMF_GridAddCoord(Grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
       _VERIFY(localrc)


       do lDE=0,localDECount-1
          call ESMF_GridGetCoord(Grid, localDE=lDE, staggerloc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                              computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrX, rc=localrc)
          _VERIFY(localrc)
          call ESMF_GridGetCoord(Grid, localDE=lDE, staggerloc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                              computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrY, rc=localrc)
          _VERIFY(localrc)
          do i1=clbnd(1),cubnd(1)
             do i2=clbnd(2),cubnd(2)
                ! Set source coordinates as 0 to 360
                farrayPtrX(i1,i2) = REAL(i1-1)*dx
                farrayPtrY(i1,i2) = -90. + (REAL(i2-1)*dy + 0.5*dy)
             enddo
          enddo
       enddo

       rc = ESMF_SUCCESS

    end function create_grid

end program ESMF_FieldRegridDynMaskTest
