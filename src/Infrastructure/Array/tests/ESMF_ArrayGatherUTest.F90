! $Id: ESMF_ArrayGatherUTest.F90,v 1.14.2.1 2010/02/05 19:52:57 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_ArrayGatherUTest

!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
#include "ESMF_Macros.inc"
!
!==============================================================================
!BOPI
! !PROGRAM: ESMF_ArrayGatherUTest - This test verifies ArrayGather functionality.
!
! !DESCRIPTION:
!
! The code in this file specializes on testing the usage of ArrayGather.
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
    '$Id: ESMF_ArrayGatherUTest.F90,v 1.14.2.1 2010/02/05 19:52:57 svasquez Exp $'
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

    if (.not. ESMF_TestMinPETs(4, ESMF_SRCLINE)) &
        call ESMF_Finalize(terminationflag=ESMF_ABORT)

    !------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    ! 1D ArrayGather() test contiguous Array
    call test_gather_1d(totalLWidth=(/0/), totalUWidth=(/0/), rc=rc)
    write(failMsg, *) ""
    write(name, *) "ArrayGather 1d test, contiguous Array"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    ! 1D ArrayGather() test non-contiguous Array
    call test_gather_1d(totalLWidth=(/2/), totalUWidth=(/3/), rc=rc)
    write(failMsg, *) ""
    write(name, *) "ArrayGather 1d test, non-contiguous Array"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    ! 2D ArrayGather() test contiguous Array
    call test_gather_2d(totalLWidth=(/0,0/), totalUWidth=(/0,0/), rc=rc)
    write(failMsg, *) ""
    write(name, *) "ArrayGather 2d test, contiguous Array"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    ! 2D ArrayGather() test non-contiguous Array
    call test_gather_2d(totalLWidth=(/2,3/), totalUWidth=(/4,7/), rc=rc)
    write(failMsg, *) ""
    write(name, *) "ArrayGather 2d test, non-contiguous Array"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    ! 3D ArrayGather() test contiguous Array
    call test_gather_3d(totalLWidth=(/0,0,0/), totalUWidth=(/0,0,0/), rc=rc)
    write(failMsg, *) ""
    write(name, *) "ArrayGather 3d test, contiguous Array"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    ! 3D ArrayGather() test non-contiguous Array
    call test_gather_3d(totalLWidth=(/11,21,31/), totalUWidth=(/9,4,3/), rc=rc)
    write(failMsg, *) ""
    write(name, *) "ArrayGather 3d test, non-contiguous Array"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    call ESMF_TestEnd(result, ESMF_SRCLINE)


contains

#undef ESMF_METHOD
#define ESMF_METHOD "test_gather_1d"
    subroutine test_gather_1d(totalLWidth, totalUWidth, rc)
        integer, intent(in)   :: totalLWidth(:), totalUWidth(:)
        integer, intent(out)  :: rc

        ! local arguments used to create field etc
        type(ESMF_DistGrid)                         :: distgrid
        type(ESMF_VM)                               :: vm
        type(ESMF_Array)                            :: array
        type(ESMF_ArraySpec)                        :: arrayspec
        integer                                     :: localrc, lpe, i, j

        integer, pointer                            :: farray(:)
        integer, pointer                            :: farrayDst(:)

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

        distgrid = ESMF_DistGridCreate(minIndex =(/1/), maxIndex=(/16/), &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_I4, rank=1, &
          rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        array = ESMF_ArrayCreate(arrayspec, distgrid=distgrid, &
          totalLWidth=totalLWidth, totalUWidth=totalUWidth, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_ArrayGet(array, farrayPtr=farray, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        farray = lpe  ! fill array with values

        if(lpe .eq. 0) allocate(farrayDst(16))  ! rootPet
        call ESMF_ArrayGather(array, farrayDst, rootPet=0, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! check that the values gathered on rootPet are correct
        if(lpe .eq. 0) then
            do i = 1, 4
                do j = 1, 4
                    if(farrayDst((i-1)*4+j) .ne. i-1) localrc=ESMF_FAILURE
                enddo
            enddo
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
        endif

        call ESMF_ArrayDestroy(array, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
                    
        call ESMF_DistGridDestroy(distgrid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        
        if(lpe .eq. 0) deallocate(farrayDst)

        rc = ESMF_SUCCESS
    end subroutine test_gather_1d

#undef ESMF_METHOD
#define ESMF_METHOD "test_gather_2d"
    subroutine test_gather_2d(totalLWidth, totalUWidth, rc)
        integer, intent(in)   :: totalLWidth(:), totalUWidth(:)
        integer, intent(out)  :: rc

        ! local arguments used to create field etc
        type(ESMF_DistGrid)                         :: distgrid
        type(ESMF_VM)                               :: vm
        type(ESMF_Array)                            :: array
        type(ESMF_ArraySpec)                        :: arrayspec
        integer                                     :: localrc, lpe, i, j

        integer, pointer                            :: farray(:,:)
        integer, pointer                            :: farrayDst(:,:)

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

        distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,20/), &
            regDecomp=(/2,2/), rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_I4, rank=2, &
          rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        array = ESMF_ArrayCreate(arrayspec, distgrid=distgrid, &
          totalLWidth=totalLWidth, totalUWidth=totalUWidth, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_ArrayGet(array, farrayPtr=farray, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        farray = lpe  ! fill array with values

        if(lpe .eq. 0) allocate(farrayDst(10,20))  ! rootPet
        call ESMF_ArrayGather(array, farrayDst, rootPet=0, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! check that the values gathered on rootPet are correct
        if(lpe .eq. 0) then
            do i = 1, 2
                do j = 1, 2
                    if(farrayDst(i*5, j*10) .ne. (i-1)+(j-1)*2) localrc=ESMF_FAILURE
                enddo
            enddo
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
        endif

        call ESMF_ArrayDestroy(array, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
                    
        call ESMF_DistGridDestroy(distgrid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        
        if(lpe .eq. 0) deallocate(farrayDst)
        rc = ESMF_SUCCESS
    end subroutine test_gather_2d


#undef ESMF_METHOD
#define ESMF_METHOD "test_gather_3d"
    subroutine test_gather_3d(totalLWidth, totalUWidth, rc)
        integer, intent(in)   :: totalLWidth(:), totalUWidth(:)
        integer, intent(out)  :: rc

        ! local arguments used to create field etc
        type(ESMF_DistGrid)                         :: distgrid
        type(ESMF_VM)                               :: vm
        type(ESMF_Array)                            :: array
        type(ESMF_ArraySpec)                        :: arrayspec
        integer                                     :: localrc, lpe, i, j, k

        integer, pointer                            :: farray(:,:,:)
        integer, pointer                            :: farrayDst(:,:,:)

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

        distgrid = ESMF_DistGridCreate(minIndex=(/1,1,1/), &
            maxIndex=(/10,20,5/), regDecomp=(/2,2,1/), rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_I4, rank=3, &
          rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        array = ESMF_ArrayCreate(arrayspec, distgrid=distgrid, &
          totalLWidth=totalLWidth, totalUWidth=totalUWidth, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_ArrayGet(array, farrayPtr=farray, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        farray = lpe  ! fill array with values

        if(lpe .eq. 0) allocate(farrayDst(10,20,5))  ! rootPet
        call ESMF_ArrayGather(array, farrayDst, rootPet=0, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! check that the values gathered on rootPet are correct
        if(lpe .eq. 0) then
            do k = 1, 5
                do j = 1, 2
                    do i = 1, 2
                        if(farrayDst(i*5, j*10, k) .ne. (i-1)+(j-1)*2) localrc=ESMF_FAILURE
                    enddo
                enddo
            enddo
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
        endif

        call ESMF_ArrayDestroy(array, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
                    
        call ESMF_DistGridDestroy(distgrid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        
        if(lpe .eq. 0) deallocate(farrayDst)
        rc = ESMF_SUCCESS
    end subroutine test_gather_3d

end program ESMF_ArrayGatherUTest
