! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

#define FILENAME "src/Infrastructure/Array/tests/ESMF_ArraySMMUTest.F90"

#include "ESMF_Macros.inc"
#include "ESMF.h"

module ESMF_ArraySMMUTest_comp_mod

  ! modules
  use ESMF_TestMod     ! test methods
  use ESMF
  
  implicit none
  
  private
  
  public setvm, setservices, test_smm

  contains !--------------------------------------------------------------------

  subroutine setvm(gcomp, rc)
    ! arguments
    type(ESMF_GridComp):: gcomp
    integer, intent(out):: rc
#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm
    logical :: pthreadsEnabled
#endif
    
    ! Initialize
    rc = ESMF_SUCCESS

#ifdef ESMF_TESTWITHTHREADS
    ! The following call will turn on ESMF-threading (single threaded)
    ! for this component. If you are using this file as a template for
    ! your own code development you probably don't want to include the
    ! following call unless you are interested in exploring ESMF's
    ! threading features.

    ! First test whether ESMF-threading is supported on this machine
    call ESMF_VMGetGlobal(vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMGet(vm, pthreadsEnabledFlag=pthreadsEnabled, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    if (pthreadsEnabled) then
      !TODO: use the following call to change the VM threading level + comm sets
      call ESMF_GridCompSetVMMinThreads(gcomp, rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
    endif
#endif

  end subroutine !--------------------------------------------------------------

  recursive subroutine setservices(gcomp, rc)
    ! arguments
    type(ESMF_GridComp):: gcomp
    integer, intent(out):: rc
    
    ! Initialize
    rc = ESMF_SUCCESS

    ! register RUN method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, userRoutine=run, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine !--------------------------------------------------------------
  
  recursive subroutine run(gcomp, istate, estate, clock, rc)
    ! arguments
    type(ESMF_GridComp):: gcomp
    type(ESMF_State):: istate, estate
    type(ESMF_Clock):: clock
    integer, intent(out):: rc
    
    ! local variables
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name
    integer                :: petCount, i, result
    integer, allocatable   :: petList(:)
#if 0
    type(ESMF_VM)           :: vm
#endif

    ! Initialize
    rc = ESMF_SUCCESS
    result = 0
    
    call ESMF_GridCompGet(gcomp, petCount=petCount, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
#if 0
    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMPrint(vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
#endif

#if 1
    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "ComponentizedSMMSuite: src 1 DE/PET -> dst default 4DEs ASMM Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    call test_smm(srcRegDecomp=(/1,petCount/), rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    ! must bail out to prevent possible hanging due to communications
    if (rc /= ESMF_SUCCESS) return ! bail out
    !------------------------------------------------------------------------
#endif

  end subroutine !--------------------------------------------------------------

  recursive subroutine test_smm(srcRegDecomp, dstPetList, vectorLength, &
    srcTermProcessing, pipelineDepth, termorderflag, testUnmatched, rc)
    integer                             :: srcRegDecomp(:)
    integer,                   optional :: dstPetList(:)
    integer,                   optional :: vectorLength
    integer,                   optional :: srcTermProcessing
    integer,                   optional :: pipelineDepth
    type(ESMF_TermOrder_Flag), optional :: termorderflag
    logical,                   optional :: testUnmatched
    integer                             :: rc

    ! Local variables
    type(ESMF_VM)         :: vm
    type(ESMF_DELayout)   :: delayout
    type(ESMF_DistGrid)   :: srcDistgrid, dstDistgrid
    type(ESMF_Array)      :: srcArray, dstArray
    integer               :: i, j, petCount, localPet, localDeCount
    integer, allocatable  :: localDeToDeMap(:)
    integer, pointer      :: farrayPtr(:), farrayPtrV(:,:) 
    integer, target       :: seedV(4,6,10), resultV(4,6,10), validationV(4,6,10)
    integer, pointer      :: seed(:,:), result(:,:), validation(:,:)
    integer               :: factorList(19), factorIndexList(2,19), value
    type(ESMF_RouteHandle):: rh, trh
    integer               :: vectorLengthOpt
    logical               :: testUnmatchedOpt
    character(len=160)    :: msg
    
    rc = ESMF_SUCCESS
    
    !---------------------------------------------------------------------------
    ! deal with optional arguments
    vectorLengthOpt = 0 ! default
    if (present(vectorLength)) vectorLengthOpt = vectorLength
    testUnmatchedOpt = .false.  ! default
    if (present(testUnmatched)) testUnmatchedOpt = testUnmatched
    
    !---------------------------------------------------------------------------
    ! checking for invalid input
    if (vectorLengthOpt < 0 .or. vectorLengthOpt > 10) then
      call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
        msg="vectorLengh out of range", &
        line=__LINE__, &
        file=FILENAME, rcToReturn=rc) 
      return  ! bail out
    endif
    
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
      regDecomp=srcRegDecomp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    if (vectorLengthOpt>0) then
      srcArray = ESMF_ArrayCreate(srcDistGrid, ESMF_TYPEKIND_I4, &
        undistLBound=(/1/), undistUBound=(/vectorLengthOpt/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    else
      srcArray = ESMF_ArrayCreate(srcDistGrid, ESMF_TYPEKIND_I4, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    endif

    !---------------------------------------------------------------------------
    ! initialize seed on PET 0 and scatter into srcArray

    seed => seedV(:,:,1)
    if (localPet==0) then
      value = 1 ! initialize start value
      do j=1, 6
        do i=1, 4
          seed(i,j) = value
          value = value + 1
        enddo
      enddo
    endif

    if (vectorLengthOpt>0) then
      call ESMF_ArrayScatter(srcArray, seedV(:,:,1:vectorLengthOpt), &
        rootPet=0, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    else
      call ESMF_ArrayScatter(srcArray, seed, rootPet=0, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    endif
      
    !---------------------------------------------------------------------------
    ! set up array for transpose validation

    validation => validationV(:,:,1)
    if (localPet==0) then
      validation(1,1) = -120
      validation(2,1) = -459
      validation(3,1) = -528
      validation(4,1) = 2496
      validation(1,2) = -24
      validation(2,2) = 1728
      validation(3,2) = 3072
      validation(4,2) = 1920
      validation(1,3) = -1344
      validation(2,3) = 1122
      validation(3,3) = 384
      validation(4,3) = -1683
      validation(1,4) = 2295
      validation(2,4) = 36
      validation(3,4) = 132
      validation(4,4) = -330
      validation(1,5) = 0
      validation(2,5) = 0
      validation(3,5) = 0
      validation(4,5) = 0
      validation(1,6) = 0
      validation(2,6) = 0
      validation(3,6) = 0
      validation(4,6) = 0
    endif

    !---------------------------------------------------------------------------
    ! set up dstArray
    
    delayout = ESMF_DELayoutCreate(deCount=4, petList=dstPetList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    dstDistGrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/4/), &
      delayout=delayout, rc=rc) ! One data point on each of the 4 DEs
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    if (vectorLengthOpt>0) then
      dstArray = ESMF_ArrayCreate(dstDistGrid, ESMF_TYPEKIND_I4, &
        undistLBound=(/1/), undistUBound=(/vectorLengthOpt/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    else
      dstArray = ESMF_ArrayCreate(dstDistGrid, ESMF_TYPEKIND_I4, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    endif

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

      if (testUnmatchedOpt) then
        factorIndexList(1,19) = 15  ! valid src sequence index
        factorIndexList(2,19) = 40  ! invalid dst sequence index
        factorList(19)        = 100 ! will never be used
      else
        factorIndexList(1,19) = 1   ! inside
        factorIndexList(2,19) = 1   ! inside
        factorList(19)        = 0   ! does not change anything
      endif

    endif
    
    !---------------------------------------------------------------------------
    ! ASMMStore
    
    if (localPet == 0) then
      call ESMF_ArraySMMStore(srcArray, dstArray, factorList=factorList, &
        factorIndexList=factorIndexList, routehandle=rh, &
        ignoreUnmatchedIndices=testUnmatchedOpt, &
        srcTermProcessing=srcTermProcessing, pipelineDepth=pipelineDepth, &
        transposeRoutehandle=trh, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    else
      call ESMF_ArraySMMStore(srcArray, dstArray, routehandle=rh, &
        ignoreUnmatchedIndices=testUnmatchedOpt, &
        srcTermProcessing=srcTermProcessing, pipelineDepth=pipelineDepth, &
        transposeRoutehandle=trh, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    endif

    !---------------------------------------------------------------------------
    ! Re-set the data in srcArray, because it will have been modified due to
    ! the transposeRoutehandle option in ESMF_ArraySMMStore()

    if (vectorLengthOpt>0) then
      call ESMF_ArrayScatter(srcArray, seedV(:,:,1:vectorLengthOpt), &
        rootPet=0, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    else
      call ESMF_ArrayScatter(srcArray, seed, rootPet=0, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    endif

    !---------------------------------------------------------------------------
    ! ASMM
    
    call ESMF_ArraySMM(srcArray, dstArray, termorderflag=termorderflag, &
      routehandle=rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    !---------------------------------------------------------------------------
    ! ASMMRelease

    call ESMF_ArraySMMRelease(routehandle=rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    !---------------------------------------------------------------------------
    ! Verification dstArray
    
    call ESMF_ArrayGet(dstArray, localDeCount=localDeCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    allocate(localDeToDeMap(0:localDeCount-1))
    
    call ESMF_ArrayGet(dstArray, localDeToDeMap=localDeToDeMap, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    do i=0, localDeCount-1
    
      if (vectorLengthOpt>0) then
        call ESMF_ArrayGet(dstArray, localDe=i, farrayPtr=farrayPtrV, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
        value = farrayPtrV(1,1)
      else
        call ESMF_ArrayGet(dstArray, localDe=i, farrayPtr=farrayPtr, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
        value = farrayPtr(1)
      endif
      
      select case (localDeToDeMap(i))
      case (0)
        if (value == -66) then
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
        if (value == 153) then
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
        if (value == -6) then
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
        if (value == 192) then
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
    
    deallocate(localDeToDeMap)
    
    !---------------------------------------------------------------------------
    ! ASMM transpose
    
    call ESMF_ArraySMM(dstArray, srcArray, termorderflag=termorderflag, &
      routehandle=trh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    !---------------------------------------------------------------------------
    ! ASMMRelease transpose

    call ESMF_ArraySMMRelease(routehandle=trh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    !---------------------------------------------------------------------------
    ! Verification transpose
    
    result => resultV(:,:,1)
    if (vectorLengthOpt>0) then
      call ESMF_ArrayGather(srcArray, resultV(:,:,1:vectorLengthOpt), &
        rootPet=0, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    else
      call ESMF_ArrayGather(srcArray, result, rootPet=0, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    endif

    if (localPet==0) then
      do j=1, 6
        do i=1, 4
          if (result(i,j)==validation(i,j)) then
            write(msg,*) "Correct transpose results verified in result(",i,",",j,")."
            call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=FILENAME)) &
              return  ! bail out
          else
            write(msg,*) "Incorrect transpose results detected in result(",i,",",j,")"//&
              ": ", result(i,j), "/=", validation(i,j)
            call ESMF_LogSetError(rcToCheck=ESMF_RC_VAL_WRONG, &
              msg = msg, &
              line=__LINE__, &
              file=FILENAME, &
              rcToReturn=rc)
            return  ! bail out
          endif
        enddo
      enddo
    endif

    !---------------------------------------------------------------------------
    ! Clean-up

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

    call ESMF_DELayoutDestroy(delayout, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

  end subroutine

end module

!==============================================================================
!==============================================================================
!==============================================================================

program ESMF_ArraySMMUTest

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

  use ESMF_ArraySMMUTest_comp_mod, only: setvm, setservices, test_smm

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id$'
!------------------------------------------------------------------------------

!-------------------------------------------------------------------------
!=========================================================================

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  integer               :: rc, petCount, i
  integer, allocatable  :: petList(:)
  type(ESMF_VM)         :: vm
  type(ESMF_GridComp)   :: gcomp
  ! cumulative result: count failures; no failures equals "all pass"
  integer               :: result = 0

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
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_VMGet(vm, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "src 1 DE/PET -> dst default 4DEs ASMM Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call test_smm(srcRegDecomp=(/1,petCount/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  ! must abort to prevent possible hanging due to communications
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "src 1 DE/PET -> dst default 4DEs, vectorLength=4 ASMM Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call test_smm(srcRegDecomp=(/1,petCount/), vectorLength=4, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  ! must abort to prevent possible hanging due to communications
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "src 1 DE/PET -> dst default 4DEs, ESMF_TERMORDER_SRCSEQ ASMM Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call test_smm(srcRegDecomp=(/1,petCount/), &
    termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  ! must abort to prevent possible hanging due to communications
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "src 1 DE/PET -> dst default 4DEs, vectorLength=4, ESMF_TERMORDER_SRCSEQ ASMM Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call test_smm(srcRegDecomp=(/1,petCount/), vectorLength=4, &
    srcTermProcessing=1, termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  ! must abort to prevent possible hanging due to communications
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "src 1 DE/PET -> dst default 4DEs, vectorLength=4, ESMF_TERMORDER_SRCSEQ ASMM Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call test_smm(srcRegDecomp=(/1,petCount/), vectorLength=4, &
    srcTermProcessing=0, termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  ! must abort to prevent possible hanging due to communications
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "src 1 DE/PET -> dst default 4DEs, testUnmatched ASMM Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call test_smm(srcRegDecomp=(/1,petCount/), testUnmatched=.true., rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  ! must abort to prevent possible hanging due to communications
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "src 1 DE/PET -> dst default 4DEs ASMM Test w/ tuning parameters"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call test_smm(srcRegDecomp=(/1,petCount/), srcTermProcessing=10, &
    pipelineDepth=4, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  ! must abort to prevent possible hanging due to communications
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "src 1 DE/PET -> dst default 4DEs ASMM Test w/ tuning parameters, testUnmatched"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call test_smm(srcRegDecomp=(/1,petCount/), srcTermProcessing=10, &
    pipelineDepth=4, testUnmatched=.true., rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  ! must abort to prevent possible hanging due to communications
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "src 1 DE/PET -> dst all 4 DEs on PET 0 ASMM Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call test_smm(srcRegDecomp=(/1,petCount/), dstPetList=(/0/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  ! must abort to prevent possible hanging due to communications
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "src 2 DEs/PET -> dst default 4DEs ASMM Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call test_smm(srcRegDecomp=(/2,petCount/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  ! must abort to prevent possible hanging due to communications
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "src 2 DEs/PET -> dst default 4DEs, testUnmatched ASMM Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call test_smm(srcRegDecomp=(/2,petCount/), testUnmatched=.true., rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  ! must abort to prevent possible hanging due to communications
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  allocate(petList(0:petCount/2-1))
  do i=0, petCount/2-1
    petList(i) = i*2
  enddo
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "src 2 DEs/PET -> dst skipping PETs ASMM Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call test_smm(srcRegDecomp=(/2,petCount/), dstPetList=petList, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  ! must abort to prevent possible hanging due to communications
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "src more than one DE/PET (irregular) -> dst default 4DEs ASMM Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call test_smm(srcRegDecomp=(/2,4/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  ! must abort to prevent possible hanging due to communications
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "src more than one DE/PET (irregular) -> dst all 4 DEs on PET 0 ASMM Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call test_smm(srcRegDecomp=(/2,4/), dstPetList=(/0/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  ! must abort to prevent possible hanging due to communications
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "src some PETs with 0 DEs -> dst default 4DEs ASMM Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call test_smm(srcRegDecomp=(/2,2/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  ! must abort to prevent possible hanging due to communications
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "src some PETs with 0 DEs -> dst skipping PETs ASMM Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call test_smm(srcRegDecomp=(/2,2/), dstPetList=petList, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  ! must abort to prevent possible hanging due to communications
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "src some PETs with 0 DEs -> dst skipping PETs w/ tuning parameters ASMM Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call test_smm(srcRegDecomp=(/2,2/), dstPetList=petList, srcTermProcessing=10,&
    pipelineDepth=4, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  ! must abort to prevent possible hanging due to communications
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  deallocate(petlist)
  
  !------------------------------------------------------------------------
  !------------------------------------------------------------------------
  ! Run the componentized SMM test suite
  
  gcomp = ESMF_GridCompCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  call ESMF_GridCompSetVM(gcomp, userRoutine=setvm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompSetServices(gcomp, userRoutine=setservices, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompRun(gcomp, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------

end program ESMF_ArraySMMUTest
