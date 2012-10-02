! $Id: ESMF_RHandleBitForBitEx.F90,v 1.1 2012/10/02 04:06:46 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2012, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

!==============================================================================
!ESMF_MULTI_PROC_EXAMPLE        String used by test script to count examples.
!==============================================================================

program ESMF_RHandleBitForBitEx
#include "ESMF.h"

  use ESMF
  use ESMF_TestMod
  
  implicit none
  
  ! local variables
  integer               :: rc, i
  integer               :: petCount, localPet
  type(ESMF_VM)         :: vm
  type(ESMF_DistGrid)   :: distgrid
  type(ESMF_Array)      :: srcArray, dstArray
  integer, allocatable  :: indexList(:)
  real(ESMF_KIND_R4), pointer:: farrayPtr(:)
  real(ESMF_KIND_R4)    :: sumA, sumB
  
  integer               :: smmElementCount
  integer, allocatable  :: factorIndexList(:,:)
  real(ESMF_KIND_R4), allocatable:: factorList(:)
  type(ESMF_RouteHandle):: rh
  
  integer               :: srcTermProcessing

  ! result code
  integer :: finalrc, result
  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg
  
  
  finalrc = ESMF_SUCCESS

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_RHandleBitForBitEx"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_Initialize(vm=vm, defaultlogfilename="RHandleBitForBitEx.Log", &
    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  if (petCount /= 4) then
    finalrc = ESMF_FAILURE
    goto 10
  endif
  
!BOE
! \subsubsection{Bit-for-bit reproducibility}
! \label{RH:bfb}
!
! Bit-for-bit (bfb) reproducibility is at the core of the regression testing
! schemes of many scientific model codes. The bfb requirement makes it possible
! to compare the numerical results of a simulation using standard binary diff
! tools.
!
! While bfb reproducibility is desirable (and often required) for regression
! testing, it can at the same time prevent otherwise possible performance
! optimizations. Especially in highly parallelized code, best performance is 
! often achieved by allowing operations to occur in flexible order. Under the
! right conditions, however, a change in the order of numerical operations will
! lead to small numerical differences in the results, and thus break bfb
! reproducibility.
!
! The following discussion uses a very simple numerical example to demonstrate
! how the order of terms in a three part sum can lead to results that are not
! bit-for-bit identical.
! 
!EOE

!BOC
  sumA = (0.5 + 0.1) + 0.1  ! results in 0.700000048
  sumB = 0.5 + (0.1 + 0.1)  ! results in 0.699999988
!EOC

  print *, "sumA = ", sumA
  print *, "sumB = ", sumB

  ! -- srcArray --

  allocate(indexList(3))
  if (localPet == 0) then
    indexList(1) = 1
    indexList(2) = 6
    indexList(3) = 9
  elseif (localPet == 1) then
    indexList(1) = 4
    indexList(2) = 3
    indexList(3) = 10
  elseif (localPet == 2) then
    indexList(1) = 5
    indexList(2) = 7
    indexList(3) = 11
  elseif (localPet == 3) then
    indexList(1) = 8
    indexList(2) = 2
    indexList(3) = 12
  endif
  
  distgrid = ESMF_DistGridCreate(indexList, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  srcArray = ESMF_ArrayCreate(distgrid=distgrid, typekind=ESMF_TYPEKIND_R4, &
    rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  call ESMF_ArrayGet(srcArray, farrayPtr=farrayPtr, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  do i=1, 3
!BOC
    select case (indexList(i))
    case (1)
      farrayPtr(i) = 0.5_ESMF_KIND_R4
    case (2)
      farrayPtr(i) = 0.1_ESMF_KIND_R4
    case (3)
      farrayPtr(i) = 0.1_ESMF_KIND_R4
    case (4)
      farrayPtr(i) = 0.5_ESMF_KIND_R4
    case (5)
      farrayPtr(i) = 0.1_ESMF_KIND_R4
    case (6)
      farrayPtr(i) = 0.1_ESMF_KIND_R4    
    case (7)
      farrayPtr(i) = 0.1_ESMF_KIND_R4    
    case (8)
      farrayPtr(i) = 0.5_ESMF_KIND_R4    
    case (9)
      farrayPtr(i) = 0.1_ESMF_KIND_R4    
    case (10)
      farrayPtr(i) = 0.1_ESMF_KIND_R4    
    case (11)
      farrayPtr(i) = 0.5_ESMF_KIND_R4    
    case (12)
      farrayPtr(i) = 0.1_ESMF_KIND_R4    
    end select
!EOC
  enddo
  
  deallocate(indexList)

!  call ESMF_ArrayPrint(srcArray)
 
  ! -- dstArray --

  if (localPet == 0) then
    allocate(indexList(1))
    indexList(1) = 1
  else
    allocate(indexList(0))
  endif
  
  distgrid = ESMF_DistGridCreate(indexList, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  deallocate(indexList)

  dstArray = ESMF_ArrayCreate(distgrid=distgrid, typekind=ESMF_TYPEKIND_R4, &
    rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
  call ESMF_ArrayGet(dstArray, farrayPtr=farrayPtr, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! -- sparse matrix --
    
  smmElementCount = 0
  if (localPet == 0) then
    smmElementCount = 3
  endif
  allocate(factorIndexList(2,smmElementCount), factorList(smmElementCount))
  
  
  if (localPet == 0) then
    ! -> factors into dst element 1
    ! * sum with 3 addends: 0.5 + 0.1 + 0.1
    ! * addend 1 & 2 are on PET 0, addend 3 is on PET 1
    ! * due to the arrangement of addends the order is preserved for different
    !   values of srcTermProcessing
    factorIndexList(1,1) = 1  ! src
    factorIndexList(2,1) = 1  ! dst
    factorList(1) = 1.
    factorIndexList(1,2) = 3  ! src
    factorIndexList(2,2) = 1  ! dst
    factorList(2) = 1.
    factorIndexList(1,3) = 7  ! src
    factorIndexList(2,3) = 1  ! dst
    factorList(3) = 1.
  endif
  
  ! -- SMM
  srcTermProcessing=0
  
  call ESMF_ArraySMMStore(srcArray, dstArray, factorIndexList=factorIndexList, &
    factorList=factorList, routehandle=rh, &
    srcTermProcessing=srcTermProcessing, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
  call ESMF_ArraySMM(srcArray, dstArray, routehandle=rh, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  if (localPet == 0) then
    print *, "result #1 = ", farrayPtr(1)
  endif

  call ESMF_ArraySMM(srcArray, dstArray, routehandle=rh, &
    termorderflag=ESMF_TERMORDER_SRCPET, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  if (localPet == 0) then
    print *, "result #2 = ", farrayPtr(1)
  endif

  call ESMF_ArraySMMRelease(rh, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)


10 continue

  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

  call ESMF_Finalize(rc=rc)
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_RHandleBitForBitEx.F90"
  else
    print *, "FAIL: ESMF_RHandleBitForBitEx.F90"
  endif

end program
