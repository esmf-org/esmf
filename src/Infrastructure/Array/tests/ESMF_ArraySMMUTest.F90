! $Id: ESMF_ArraySMMUTest.F90,v 1.3 2011/08/23 23:50:09 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2011, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

#define FILENAME "src/Infrastructure/Array/tests/ESMF_ArraySMMUTest.F90"

program ESMF_ArraySMMUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_ArraySMMUTest -  Tests ArraySMM()
!
! !DESCRIPTION:
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id: ESMF_ArraySMMUTest.F90,v 1.3 2011/08/23 23:50:09 theurich Exp $'
!------------------------------------------------------------------------------

!-------------------------------------------------------------------------
!=========================================================================

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  integer       :: rc, petCount
  type(ESMF_VM) :: vm
  ! cumulative result: count failures; no failures equals "all pass"
  integer       :: result = 0


!-------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------

  !------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  !------------------------------------------------------------------------

  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_VMGet(vm, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "1 DE/PET ASMM Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call test_smm(regDecomp=(/1,petCount/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  ! must abort to prevent possible hanging due to communications
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "2 DEs/PET ASMM Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call test_smm(regDecomp=(/2,petCount/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  ! must abort to prevent possible hanging due to communications
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "more than one DE/PET (irregular) ASMM Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call test_smm(regDecomp=(/2,4/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  ! must abort to prevent possible hanging due to communications
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "some PETs with 0 DEs ASMM Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call test_smm(regDecomp=(/2,2/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  ! must abort to prevent possible hanging due to communications
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
10 continue
  !------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
contains
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

  subroutine test_smm(regDecomp, rc)
    integer :: regDecomp(:), rc
    
    ! Local variables
    type(ESMF_VM)         :: vm
    type(ESMF_DistGrid)   :: srcDistgrid, dstDistgrid
    type(ESMF_Array)      :: srcArray, dstArray
    integer               :: i, j, petCount, localPet, localDeCount
    integer, allocatable  :: localDeList(:)
    integer, pointer      :: farrayPtr(:)
    integer               :: seed(4,6), value
    integer               :: factorList(18), factorIndexList(2,18)
    type(ESMF_RouteHandle):: rh
    
    rc = ESMF_SUCCESS

    !---------------------------------------------------------------------------
    ! get current VM and pet info
    
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    !---------------------------------------------------------------------------
    ! set up srcArray
    
    srcDistGrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/4,6/), &
      regDecomp=regDecomp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    srcArray = ESMF_ArrayCreate(srcDistGrid, ESMF_TYPEKIND_I4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    !---------------------------------------------------------------------------
    ! initialize seed on PET 0 and scatter into srcArray

    if (localPet==0) then
      value = 1 ! initialize start value
      do j=1, 6
        do i=1, 4
          seed(i, j) = value
          value = value + 1
        enddo
      enddo
    endif

    call ESMF_ArrayScatter(srcArray, seed, rootPet=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    !---------------------------------------------------------------------------
    ! set up dstArray

    dstDistGrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/4/), &
      regDecomp=(/4/), rc=rc) ! fix this DistGrid to have 4 DEs
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    dstArray = ESMF_ArrayCreate(dstDistGrid, ESMF_TYPEKIND_I4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    !---------------------------------------------------------------------------
    ! initialize factorIndexList and factorList (but only on PET 0)

    if (localPet == 0) then
      factorIndexList(1,1)  = 3
      factorIndexList(2,1)  = 1
      factorList(1)         = 8

      factorIndexList(1,2)  = 10
      factorIndexList(2,2)  = 1
      factorList(2)         = -17

      factorIndexList(1,3)  = 16
      factorIndexList(2,3)  = 1
      factorList(3)         = 5

      factorIndexList(1,4)  = 12
      factorIndexList(2,4)  = 2
      factorList(4)         = -11

      factorIndexList(1,5)  = 13
      factorIndexList(2,5)  = 2
      factorList(5)         = 15

      factorIndexList(1,6)  = 2
      factorIndexList(2,6)  = 2
      factorList(6)         = -3

      factorIndexList(1,7)  = 8
      factorIndexList(2,7)  = 2
      factorList(7)         = 12

      factorIndexList(1,8)  = 15
      factorIndexList(2,8)  = 3
      factorList(8)         = 10

      factorIndexList(1,9)  = 8
      factorIndexList(2,9)  = 3
      factorList(9)         = -14

      factorIndexList(1,10) = 14
      factorIndexList(2,10) = 3
      factorList(10)        = -6

      factorIndexList(1,11) = 5
      factorIndexList(2,11) = 3
      factorList(11)        = 4

      factorIndexList(1,12) = 1
      factorIndexList(2,12) = 3
      factorList(12)        = 20

      factorIndexList(1,13) = 11
      factorIndexList(2,13) = 4
      factorList(13)        = 2

      factorIndexList(1,14) = 7
      factorIndexList(2,14) = 4
      factorList(14)        = 16

      factorIndexList(1,15) = 9
      factorIndexList(2,15) = 4
      factorList(15)        = -7

      factorIndexList(1,16) = 4
      factorIndexList(2,16) = 4
      factorList(16)        = 13

      factorIndexList(1,17) = 15
      factorIndexList(2,17) = 4
      factorList(17)        = 1

      factorIndexList(1,18) = 6
      factorIndexList(2,18) = 4
      factorList(18)        = 9
    endif
    
    !---------------------------------------------------------------------------
    ! ASMMStore
    
    if (localPet == 0) then
      call ESMF_ArraySMMStore(srcArray, dstArray, factorList=factorList, &
        factorIndexList=factorIndexList, routehandle=rh, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    else
      call ESMF_ArraySMMStore(srcArray, dstArray, routehandle=rh, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    endif

    !---------------------------------------------------------------------------
    ! ASMM
    
    call ESMF_ArraySMM(srcArray, dstArray, routehandle=rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    !---------------------------------------------------------------------------
    ! Verification
    
    call ESMF_ArrayGet(dstArray, localDeCount=localDeCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    allocate(localDeList(0:localDeCount-1))
    
    call ESMF_ArrayGet(dstArray, localDeList=localDeList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    do i=0, localDeCount-1
    
      call ESMF_ArrayGet(dstArray, localDe=i, farrayPtr=farrayPtr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      
      select case (localDeList(i))
      case (0)
        if (farrayPtr(1) == -66) then
          call ESMF_LogWrite("Correct result verified in dstArray on DE 0", &
            ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
        else
          call ESMF_LogSetError(rcToCheck=ESMF_RC_VAL_WRONG, &
            msg = "Incorrect result detected in dstArray on DE 0", &
            line=__LINE__, &
            file=FILENAME, &
            rcToReturn=rc)
          return  ! bail out
        endif
      case (1)
        if (farrayPtr(1) == 153) then
          call ESMF_LogWrite("Correct result verified in dstArray on DE 1", &
            ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
        else
          call ESMF_LogSetError(rcToCheck=ESMF_RC_VAL_WRONG, &
            msg = "Incorrect result detected in dstArray on DE 1", &
            line=__LINE__, &
            file=FILENAME, &
            rcToReturn=rc)
          return  ! bail out
        endif
      case (2)
        if (farrayPtr(1) == -6) then
          call ESMF_LogWrite("Correct result verified in dstArray on DE 2", &
            ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
        else
          call ESMF_LogSetError(rcToCheck=ESMF_RC_VAL_WRONG, &
            msg = "Incorrect result detected in dstArray on DE 2", &
            line=__LINE__, &
            file=FILENAME, &
            rcToReturn=rc)
          return  ! bail out
        endif
      case (3)
        if (farrayPtr(1) == 192) then
          call ESMF_LogWrite("Correct result verified in dstArray on DE 3", &
            ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
        else
          call ESMF_LogSetError(rcToCheck=ESMF_RC_VAL_WRONG, &
            msg = "Incorrect result detected in dstArray on DE 3", &
            line=__LINE__, &
            file=FILENAME, &
            rcToReturn=rc)
          return  ! bail out
        endif
      end select
          
    enddo
    
    deallocate(localDeList)
    
    call ESMF_ArrayDestroy(srcArray, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    call ESMF_DistGridDestroy(srcDistGrid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call ESMF_ArrayDestroy(dstArray, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    call ESMF_DistGridDestroy(dstDistGrid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

  end subroutine

end program ESMF_ArraySMMUTest
