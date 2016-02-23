! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research,
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

    !------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    ! 1D ArrayGather() test contiguous Array
    call test_gather_1d(totalLWidth=(/0/), totalUWidth=(/0/), &
      dgCase="regDecomp", rc=rc)
    write(failMsg, *) ""
    write(name, *) "ArrayGather 1d test, contiguous Array"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    ! 1D ArrayGather() test non-contiguous Array
    call test_gather_1d(totalLWidth=(/2/), totalUWidth=(/3/), &
      dgCase="regDecomp", rc=rc)
    write(failMsg, *) ""
    write(name, *) "ArrayGather 1d test, non-contiguous Array"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    ! 1D ArrayGather() test contiguous Array with deBlockList DistGrid
    call test_gather_1d(totalLWidth=(/0/), totalUWidth=(/0/), &
      dgCase="deBlockList", rc=rc)
    write(failMsg, *) ""
    write(name, *) "ArrayGather 1d test, contiguous Array, deBlockList DistGrid"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest_Multi_Proc_Only
    ! 1D ArrayGather() test non-contiguous Array with deBlockList DistGrid
    call test_gather_1d(totalLWidth=(/2/), totalUWidth=(/3/), &
      dgCase="deBlockList", rc=rc)
    write(failMsg, *) ""
    write(name, *) "ArrayGather 1d test, non-contiguous Array, deBlockList DistGrid"
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

    call ESMF_TestEnd(ESMF_SRCLINE)

contains

#undef ESMF_METHOD
#define ESMF_METHOD "test_gather_1d"
    subroutine test_gather_1d(totalLWidth, totalUWidth, dgCase, rc)
        integer, intent(in)       :: totalLWidth(:), totalUWidth(:)
        character(*), intent(in)  :: dgCase
        integer, intent(out)      :: rc

        ! local arguments used to create field etc
        type(ESMF_DistGrid)                         :: distgrid
        type(ESMF_VM)                               :: vm
        type(ESMF_Array)                            :: array
        type(ESMF_ArraySpec)                        :: arrayspec
        integer                                     :: localrc, localPet, i, j
        integer, allocatable                        :: deBlockList(:,:,:)

        integer, pointer                            :: farray(:)
        integer, pointer                            :: farrayDst(:)

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        call ESMF_VMGetCurrent(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_VMGet(vm, localPet=localPet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
          
        if (trim(dgCase)=="regDecomp") then
          ! default DistGrid with regDecomp
          distgrid = ESMF_DistGridCreate(minIndex =(/1/), maxIndex=(/16/), &
            rc=localrc)
          if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        else if (trim(dgCase)=="deBlockList") then
          ! DistGrid with deBlockList
          allocate(deBlockList(1, 2, 4))  ! dimCount, 2, deCount
          deBlockList(1, 1, 1) = 5        ! 1st DE minIndex
          deBlockList(1, 2, 1) = 8        ! 1st DE maxIndex
          deBlockList(1, 1, 2) = 9        ! 2nd DE minIndex
          deBlockList(1, 2, 2) = 12       ! 2nd DE maxIndex
          deBlockList(1, 1, 3) = 13       ! 3rd DE minIndex
          deBlockList(1, 2, 3) = 16       ! 3rd DE maxIndex
          deBlockList(1, 1, 4) = 1        ! 4th DE minIndex
          deBlockList(1, 2, 4) = 4        ! 4th DE maxIndex
          distgrid = ESMF_DistGridCreate(minIndex =(/1/), maxIndex=(/16/), &
            deBlockList=deBlockList, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          deallocate(deBlockList)
        else
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="An invalid 'option' argument was provided.", &
            ESMF_CONTEXT, rcToReturn=rc)
          return
        endif

        call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_I4, rank=1, &
          rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        array = ESMF_ArrayCreate(distgrid, arrayspec, &
          totalLWidth=totalLWidth, totalUWidth=totalUWidth, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_ArrayGet(array, farrayPtr=farray, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        farray = 0  ! initialize the entire local array
        do i=1, 4
          if (trim(dgCase)=="regDecomp") then
            farray(lbound(farray,1)+totalLWidth(1)-1+i) = localPet * 10 + i
          else if (trim(dgCase)=="deBlockList") then
            farray(lbound(farray,1)+totalLWidth(1)-1+i) = mod(localPet+1,4) * 10 + i
          endif
        enddo

        if(localPet .eq. 0) then
          allocate(farrayDst(16))  ! rootPet
        else
          allocate(farrayDst(1))
        end if
        call ESMF_ArrayGather(array, farrayDst, rootPet=0, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
            
        ! check that the values gathered on rootPet are correct
        if(localPet .eq. 0) then
          do j = 1, 4
            do i = 1, 4
              if(farrayDst((j-1)*4+i) .ne. (j-1)*10+i) then
                localrc=ESMF_FAILURE
              endif
            enddo
          enddo
          if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        call ESMF_ArrayDestroy(array, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
                    
        call ESMF_DistGridDestroy(distgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        
        deallocate(farrayDst)

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
        integer                                     :: localrc, localPet, i, j

        integer, pointer                            :: farray(:,:)
        integer, pointer                            :: farrayDst(:,:)

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        call ESMF_VMGetCurrent(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_VMGet(vm, localPet=localPet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,20/), &
          regDecomp=(/2,2/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_I4, rank=2, &
          rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        array = ESMF_ArrayCreate(distgrid, arrayspec, &
          totalLWidth=totalLWidth, totalUWidth=totalUWidth, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_ArrayGet(array, farrayPtr=farray, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        farray = 0  ! initialize the entire local array
        do j=1, 10
        do i=1, 5
          farray(lbound(farray,1)+totalLWidth(1)-1+i, &
                 lbound(farray,2)+totalLWidth(2)-1+j) &
            = localPet * 100 + ((j-1)*5+i)
        enddo
        enddo

        if(localPet .eq. 0) then
          allocate(farrayDst(10,20))  ! rootPet
        else
          allocate(farrayDst(1,1))
        end if
        call ESMF_ArrayGather(array, farrayDst, rootPet=0, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
          
        ! check that the values gathered on rootPet are correct
        if(localPet .eq. 0) then
          ! from DE 0
          do j=1, 10
          do i=1, 5
            if(farrayDst(i, j) /= ((j-1)*5+i)) then
              localrc=ESMF_FAILURE
            endif
          enddo
          enddo
          ! from DE 1
          do j=1, 10
          do i=1, 5
            if(farrayDst(5+i, j) /= 1*100 + ((j-1)*5+i)) then
              localrc=ESMF_FAILURE
            endif
          enddo
          enddo
          ! from DE 2
          do j=1, 10
          do i=1, 5
            if(farrayDst(i, 10+j) /= 2*100 + ((j-1)*5+i)) then
              localrc=ESMF_FAILURE
            endif
          enddo
          enddo
          ! from DE 3
          do j=1, 10
          do i=1, 5
            if(farrayDst(5+i, 10+j) /= 3*100 + ((j-1)*5+i)) then
              localrc=ESMF_FAILURE
            endif
          enddo
          enddo
          if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        call ESMF_ArrayDestroy(array, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
                    
        call ESMF_DistGridDestroy(distgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        
        deallocate(farrayDst)
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
        integer                                     :: localrc, localPet, i, j, k

        integer, pointer                            :: farray(:,:,:)
        integer, pointer                            :: farrayDst(:,:,:)

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        call ESMF_VMGetCurrent(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_VMGet(vm, localPet=localPet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        distgrid = ESMF_DistGridCreate(minIndex=(/1,1,1/), &
          maxIndex=(/10,20,5/), regDecomp=(/2,2,1/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_I4, rank=3, &
          rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        array = ESMF_ArrayCreate(distgrid, arrayspec, &
          totalLWidth=totalLWidth, totalUWidth=totalUWidth, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_ArrayGet(array, farrayPtr=farray, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        farray = 0  ! initialize the entire local array
        do k=1, 5
        do j=1, 10
        do i=1, 5
          farray(lbound(farray,1)+totalLWidth(1)-1+i, &
                 lbound(farray,2)+totalLWidth(2)-1+j, &
                 lbound(farray,3)+totalLWidth(3)-1+k) &
            = localPet * 1000 + ((k-1)*50+(j-1)*5+i)
        enddo
        enddo
        enddo

        if(localPet .eq. 0) then
          allocate(farrayDst(10,20,5))  ! rootPet
        else
          allocate(farrayDst(1,1,1))  ! rootPet
        end if
        call ESMF_ArrayGather(array, farrayDst, rootPet=0, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        ! check that the values gathered on rootPet are correct
        if(localPet .eq. 0) then
          ! from DE 0
          do k=1, 5
          do j=1, 10
          do i=1, 5
            if(farrayDst(i, j, k) /= ((k-1)*50+(j-1)*5+i)) then
              localrc=ESMF_FAILURE
            endif
          enddo
          enddo
          enddo
          ! from DE 1
          do k=1, 5
          do j=1, 10
          do i=1, 5
            if(farrayDst(5+i, j, k) /= 1*1000 + ((k-1)*50+(j-1)*5+i)) then
              localrc=ESMF_FAILURE
            endif
          enddo
          enddo
          enddo
          ! from DE 2
          do k=1, 5
          do j=1, 10
          do i=1, 5
            if(farrayDst(i, 10+j, k) /= 2*1000 + ((k-1)*50+(j-1)*5+i)) then
              localrc=ESMF_FAILURE
            endif
          enddo
          enddo
          enddo
          ! from DE 3
          do k=1, 5
          do j=1, 10
          do i=1, 5
            if(farrayDst(5+i, 10+j, k) /= 3*1000 + ((k-1)*50+(j-1)*5+i)) then
              localrc=ESMF_FAILURE
            endif
          enddo
          enddo
          enddo
        endif

        call ESMF_ArrayDestroy(array, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
                    
        call ESMF_DistGridDestroy(distgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        
        deallocate(farrayDst)
        rc = ESMF_SUCCESS
    end subroutine test_gather_3d

end program ESMF_ArrayGatherUTest
