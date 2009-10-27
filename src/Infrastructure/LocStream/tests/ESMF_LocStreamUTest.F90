! $Id: ESMF_LocStreamUTest.F90,v 1.11 2009/10/27 19:53:46 w6ws Exp $
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
program ESMF_LocStreamCreateUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_LocStreamCreateTest - Check LocStream Create Routines
!
! !DESCRIPTION:
!
! The code in this file drives F90 LocStream Create unit tests.
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF_Mod

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id: ESMF_LocStreamUTest.F90,v 1.11 2009/10/27 19:53:46 w6ws Exp $'
!------------------------------------------------------------------------------
    
  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: localrc, rc, petCount, localPet
  logical :: correct

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name, locstream_name
  character(ESMF_MAXSTR) :: keyNames(3)
  character(ESMF_MAXSTR) :: keyUnits,keyLongName
  type(ESMF_VM) :: vm
  type(ESMF_DistGrid) :: distgrid, distgridOut
  type(ESMF_DELayout) :: delayout
  type(ESMF_IndexFlag) :: indexflag
  type(ESMF_LocStream) :: locstream, locstream2
  integer :: ec,el,eu,cc,cl,cu,tc,tl,tu
  integer :: keyCount, localDECount, localDECountOut, i
  real(ESMF_KIND_R8), pointer :: keyDataR8(:),tmpR8(:)
  real(ESMF_KIND_R4), pointer :: keyDataR4(:),tmpR4(:)
  integer (ESMF_KIND_I4), pointer :: keyDataI4(:),tmpI4(:)
  integer :: bufCount, offset
  character, pointer :: buf(:)

  !-----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  !-----------------------------------------------------------------------------

  ! get global VM
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! prepare DistGrid
  distgrid=ESMF_DistGridCreate(minIndex=(/1/),maxIndex=(/10/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing LocStream Validate"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! First make sure validate fails for an uncreated Grid
  call ESMF_LocStreamValidate(locstream,rc=localrc)
  if (localrc .eq. ESMF_SUCCESS) correct=.false.

  ! Now make sure that a created grid validates successfully
  !! Create Grid
  locstream=ESMF_LocStreamCreate(name="ted",distgrid=distgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
     
  !! Check that validate returns true
  call ESMF_LocStreamValidate(locstream, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.
  
  call ESMF_LocStreamDestroy(locstream,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  !! Check that validate returns false
  call ESMF_LocStreamValidate(locstream, rc=localrc)
  if (localrc .eq. ESMF_SUCCESS) correct=.false.

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test ESMF_LocStreamCreate from Regular distribution"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! Create Grid
  locstream=ESMF_LocStreamCreate(minIndex=2, maxIndex=20, regDecomp=4, indexflag=ESMF_INDEX_GLOBAL, &
             rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  if (petCount .eq. 1) then
     ! Check non-key bounds
     call  ESMF_LocStreamGet(locstream, localDE=0, &
             exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
             computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
             rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     ! Bounds
     if (el .ne. 2) correct=.false.
     if (eu .ne. 6) correct=.false.
     if (ec .ne. 5) correct=.false.
     if (cl .ne. 2) correct=.false.
     if (cu .ne. 6) correct=.false.
     if (cc .ne. 5) correct=.false.

     ! Check non-key bounds
     call  ESMF_LocStreamGet(locstream, localDE=1, &
             exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
             computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
             rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     ! Bounds
     if (el .ne. 7) correct=.false.
     if (eu .ne. 11) correct=.false.
     if (ec .ne. 5) correct=.false.
     if (cl .ne. 7) correct=.false.
     if (cu .ne. 11) correct=.false.
     if (cc .ne. 5) correct=.false.

     ! Check non-key bounds
     call  ESMF_LocStreamGet(locstream, localDE=2, &
             exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
             computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
             rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     ! Bounds
     if (el .ne. 12) correct=.false.
     if (eu .ne. 16) correct=.false.
     if (ec .ne. 5) correct=.false.
     if (cl .ne. 12) correct=.false.
     if (cu .ne. 16) correct=.false.
     if (cc .ne. 5) correct=.false.

!write(*,*) "2", correct, el, eu, ec
     ! Check non-key bounds
     call  ESMF_LocStreamGet(locstream, localDE=3, &
             exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
             computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
             rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     ! Bounds
     if (el .ne. 17) correct=.false.
     if (eu .ne. 20) correct=.false.
     if (ec .ne. 4) correct=.false.
     if (cl .ne. 17) correct=.false.
     if (cu .ne. 20) correct=.false.
     if (cc .ne. 4) correct=.false.

!write(*,*) "3", correct, el, eu, ec
  else   if (petCount .eq. 4) then
     if (localPet .eq. 0) then
        ! Check non-key bounds
        call  ESMF_LocStreamGet(locstream, localDE=0, &
                exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
                computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
                rc=localrc)
        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

        ! Bounds
        if (el .ne. 2) correct=.false.
        if (eu .ne. 6) correct=.false.
        if (ec .ne. 5) correct=.false.
        if (cl .ne. 2) correct=.false.
        if (cu .ne. 6) correct=.false.
        if (cc .ne. 5) correct=.false.
     else if (localPet .eq. 1) then
        ! Check non-key bounds
        call  ESMF_LocStreamGet(locstream, localDE=0, &
                exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
                computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
                rc=localrc)
        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

        ! Bounds
        if (el .ne. 7) correct=.false.
        if (eu .ne. 11) correct=.false.
        if (ec .ne. 5) correct=.false.
        if (cl .ne. 7) correct=.false.
        if (cu .ne. 11) correct=.false.
        if (cc .ne. 5) correct=.false.

     else if (localPet .eq. 2) then
        ! Check non-key bounds
        call  ESMF_LocStreamGet(locstream, localDE=0, &
               exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
               computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
               rc=localrc)
        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

        ! Bounds
        if (el .ne. 12) correct=.false.
        if (eu .ne. 16) correct=.false.
        if (ec .ne. 5) correct=.false.
        if (cl .ne. 12) correct=.false.
        if (cu .ne. 16) correct=.false.
        if (cc .ne. 5) correct=.false.

     else if (localPet .eq. 3) then
        ! Check non-key bounds
        call  ESMF_LocStreamGet(locstream, localDE=0, &
               exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
               computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
               rc=localrc)
        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

        ! Bounds
        if (el .ne. 17) correct=.false.
        if (eu .ne. 20) correct=.false.
        if (ec .ne. 4) correct=.false.
        if (cl .ne. 17) correct=.false.
        if (cu .ne. 20) correct=.false.
        if (cc .ne. 4) correct=.false.
    endif
  endif

  call ESMF_LocStreamDestroy(locstream,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test ESMF_LocStreamCreate from Irregular distribution"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! Create Grid
  locstream=ESMF_LocStreamCreate(minIndex=2, countsPerDE=(/5,5,5,4/), indexflag=ESMF_INDEX_GLOBAL, &
             rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  if (petCount .eq. 1) then
     ! Check non-key bounds
     call  ESMF_LocStreamGet(locstream, localDE=0, &
             exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
             computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
             rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     ! Bounds
     if (el .ne. 2) correct=.false.
     if (eu .ne. 6) correct=.false.
     if (ec .ne. 5) correct=.false.
     if (cl .ne. 2) correct=.false.
     if (cu .ne. 6) correct=.false.
     if (cc .ne. 5) correct=.false.

!write(*,*) "0", correct, el, eu, ec
     ! Check non-key bounds
     call  ESMF_LocStreamGet(locstream, localDE=1, &
             exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
             computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
             rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     ! Bounds
     if (el .ne. 7) correct=.false.
     if (eu .ne. 11) correct=.false.
     if (ec .ne. 5) correct=.false.
     if (cl .ne. 7) correct=.false.
     if (cu .ne. 11) correct=.false.
     if (cc .ne. 5) correct=.false.

!write(*,*) "1", correct, el, eu, ec
     ! Check non-key bounds
     call  ESMF_LocStreamGet(locstream, localDE=2, &
             exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
             computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
             rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     ! Bounds
     if (el .ne. 12) correct=.false.
     if (eu .ne. 16) correct=.false.
     if (ec .ne. 5) correct=.false.
     if (cl .ne. 12) correct=.false.
     if (cu .ne. 16) correct=.false.
     if (cc .ne. 5) correct=.false.

!write(*,*) "2", correct, el, eu, ec
     ! Check non-key bounds
     call  ESMF_LocStreamGet(locstream, localDE=3, &
             exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
             computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
             rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     ! Bounds
     if (el .ne. 17) correct=.false.
     if (eu .ne. 20) correct=.false.
     if (ec .ne. 4) correct=.false.
     if (cl .ne. 17) correct=.false.
     if (cu .ne. 20) correct=.false.
     if (cc .ne. 4) correct=.false.

!write(*,*) "3", correct, el, eu, ec
  else   if (petCount .eq. 4) then
     if (localPet .eq. 0) then
        ! Check non-key bounds
        call  ESMF_LocStreamGet(locstream, localDE=0, &
                exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
                computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
                rc=localrc)
        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

        ! Bounds
        if (el .ne. 2) correct=.false.
        if (eu .ne. 6) correct=.false.
        if (ec .ne. 5) correct=.false.
        if (cl .ne. 2) correct=.false.
        if (cu .ne. 6) correct=.false.
        if (cc .ne. 5) correct=.false.
     else if (localPet .eq. 1) then
        ! Check non-key bounds
        call  ESMF_LocStreamGet(locstream, localDE=0, &
                exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
                computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
                rc=localrc)
        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

        ! Bounds
        if (el .ne. 7) correct=.false.
        if (eu .ne. 11) correct=.false.
        if (ec .ne. 5) correct=.false.
        if (cl .ne. 7) correct=.false.
        if (cu .ne. 11) correct=.false.
        if (cc .ne. 5) correct=.false.

     else if (localPet .eq. 2) then
        ! Check non-key bounds
        call  ESMF_LocStreamGet(locstream, localDE=0, &
               exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
               computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
               rc=localrc)
        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

        ! Bounds
        if (el .ne. 12) correct=.false.
        if (eu .ne. 16) correct=.false.
        if (ec .ne. 5) correct=.false.
        if (cl .ne. 12) correct=.false.
        if (cu .ne. 16) correct=.false.
        if (cc .ne. 5) correct=.false.

     else if (localPet .eq. 3) then
        ! Check non-key bounds
        call  ESMF_LocStreamGet(locstream, localDE=0, &
               exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
               computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
               rc=localrc)
        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

        ! Bounds
        if (el .ne. 17) correct=.false.
        if (eu .ne. 20) correct=.false.
        if (ec .ne. 4) correct=.false.
        if (cl .ne. 17) correct=.false.
        if (cu .ne. 20) correct=.false.
        if (cc .ne. 4) correct=.false.
    endif
  endif

  call ESMF_LocStreamDestroy(locstream,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing LocStream Create from localCount and indexflag=ESMF_INDEX_DELOCAL"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! Now make sure that a created grid validates successfully
  !! Create Grid
  locstream=ESMF_LocStreamCreate(localCount=10, indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
     
  ! Get Bounds
  call ESMF_LocStreamGet(locstream, localDE=0, &
         exclusiveCount=ec, exclusiveLBound=el, exclusiveUBound=eu, &
         computationalCount=cc, computationalLBound=cl, computationalUBound=cu, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Check Bounds
  if (ec .ne. 10) correct=.false.
  if (el .ne. 1) correct=.false.
  if (eu .ne. 10) correct=.false.

  if (cc .ne. 10) correct=.false.
  if (cl .ne. 1) correct=.false.
  if (cu .ne. 10) correct=.false.

  
  call ESMF_LocStreamDestroy(locstream,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing LocStream Create from localCount and indexflag=ESMF_INDEX_GLOBAL"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! Now make sure that a created grid validates successfully
  !! Create Grid
  locstream=ESMF_LocStreamCreate(localCount=10, indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
     
  ! Get Bounds
  call ESMF_LocStreamGet(locstream, localDE=0, &
         exclusiveCount=ec, exclusiveLBound=el, exclusiveUBound=eu, &
         computationalCount=cc, computationalLBound=cl, computationalUBound=cu, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Check Bounds
  if (ec .ne. 10) correct=.false.
  if (el .ne. (localPet*10)+1) correct=.false.
  if (eu .ne. (localPet+1)*10) correct=.false.

  if (cc .ne. 10) correct=.false.
  if (cl .ne. (localPet*10)+1) correct=.false.
  if (cu .ne. (localPet+1)*10) correct=.false.

  call ESMF_LocStreamDestroy(locstream,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing LocStream Create defaults and LocStream Get"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! Create Grid
  locstream=ESMF_LocStreamCreate(name="LOCSTREAMXYZ", maxIndex=20, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
     
  ! Get Info about LocStream
  call ESMF_LocStreamGet(locstream, distgrid=distgridOut, &
       keyCount=keyCount, localDECount=localDECount, indexflag=indexflag, name=locstream_name, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check info
  !! Make sure we start with keyCount = 0
  if (keyCount .ne. 0) correct=.false.

  !! make sure localDECount matches
  call ESMF_DistGridGet(distgridOut,delayout=delayout, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_DELayoutGet(delayout,localDECount=localDECountOut, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  if (localDECount .ne. localDECountOut) correct=.false.

  !! Check indexflag 
  if (.not. (indexflag .eq. ESMF_INDEX_DELOCAL)) correct=.false.

  !! Check name
  if (trim(locstream_name) .ne. "LOCSTREAMXYZ") correct=.false.
  
  call ESMF_LocStreamDestroy(locstream,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------




  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test keyCount and keyNames response to ESMF_LocStreamAddKey"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! Create Grid
  locstream=ESMF_LocStreamCreate(maxIndex=20, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
     
  ! Get keyCount
  call ESMF_LocStreamGet(locstream, keyCount=keyCount, keyNames=keyNames, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check info
  !! Make sure we start with keyCount = 0
  if (keyCount .ne. 0) correct=.false.

  ! Add key
  call ESMF_LocStreamAddKey(locstream, keyName="A1", rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get keyCount
  call ESMF_LocStreamGet(locstream, keyCount=keyCount, keyNames=keyNames, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check info
  if (keyCount .ne. 1) correct=.false.
  if (trim(keyNames(1)) .ne. "A1") correct=.false.

  ! Add key
  call ESMF_LocStreamAddKey(locstream, keyName="A2", rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get keyCount
  call ESMF_LocStreamGet(locstream, keyCount=keyCount, keyNames=keyNames, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! check info
  if (keyCount .ne. 2) correct=.false.
  if (trim(keyNames(1)) .ne. "A1") correct=.false.
  if (trim(keyNames(2)) .ne. "A2") correct=.false.

  call ESMF_LocStreamDestroy(locstream,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test ESMF_LocStreamGetKey getting an I4 pointer and bounds"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! Create Grid
  locstream=ESMF_LocStreamCreate(localCount=10,  rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
     
  ! allocate array
  allocate(keyDataI4(10))

  ! Add key
  call ESMF_LocStreamAddKey(locstream, keyName="A1", farray=keyDataI4,  rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Fill key
  do i=1,10
     keyDataI4(i)=REAL(i,ESMF_KIND_I4)
  enddo

  ! Get key and bounds
  call  ESMF_LocStreamGetKey(locstream, localDE=0, keyName="A1", &
          exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
          computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
          totalLBound=tl, totalUBound=tu, totalCount=tc, & 
          farray=tmpI4, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check info
  !! data pointer
  do i=1,10
     if (tmpI4(i) .ne. REAL(i,ESMF_KIND_I4)) correct=.false.
  enddo

  !! Bounds
  if (el .ne. 1) correct=.false.
  if (eu .ne. 10) correct=.false.
  if (ec .ne. 10) correct=.false.
  if (cl .ne. 1) correct=.false.
  if (cu .ne. 10) correct=.false.
  if (cc .ne. 10) correct=.false.
  if (tl .ne. 1) correct=.false.
  if (tu .ne. 10) correct=.false.
  if (tc .ne. 10) correct=.false.


  call ESMF_LocStreamDestroy(locstream,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! deallocate array
  deallocate(keyDataI4)


  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test ESMF_LocStreamGetKey getting an R4 pointer and bounds"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! Create Grid
  locstream=ESMF_LocStreamCreate(localCount=10,  rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
     
  ! allocate array
  allocate(keyDataR4(10))

  ! Add key
  call ESMF_LocStreamAddKey(locstream, keyName="A1", farray=keyDataR4,  rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Fill key
  do i=1,10
     keyDataR4(i)=REAL(i,ESMF_KIND_R4)
  enddo

  ! Get key and bounds
  call  ESMF_LocStreamGetKey(locstream, localDE=0, keyName="A1", &
          exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
          computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
          totalLBound=tl, totalUBound=tu, totalCount=tc, & 
          farray=tmpR4, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check info
  !! data pointer
  do i=1,10
     if (tmpR4(i) .ne. REAL(i,ESMF_KIND_R4)) correct=.false.
  enddo

  !! Bounds
  if (el .ne. 1) correct=.false.
  if (eu .ne. 10) correct=.false.
  if (ec .ne. 10) correct=.false.
  if (cl .ne. 1) correct=.false.
  if (cu .ne. 10) correct=.false.
  if (cc .ne. 10) correct=.false.
  if (tl .ne. 1) correct=.false.
  if (tu .ne. 10) correct=.false.
  if (tc .ne. 10) correct=.false.


  call ESMF_LocStreamDestroy(locstream,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! deallocate array
  deallocate(keyDataR4)


  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test ESMF_LocStreamGetKey getting an R8 pointer and bounds"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! Create Grid
  locstream=ESMF_LocStreamCreate(localCount=10,  rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
     
  ! allocate array
  allocate(keyDataR8(10))

  ! Add key
  call ESMF_LocStreamAddKey(locstream, keyName="A1", farray=keyDataR8,  rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Fill key
  do i=1,10
     keyDataR8(i)=REAL(i,ESMF_KIND_R8)
  enddo

  ! Get key and bounds
  call  ESMF_LocStreamGetKey(locstream, localDE=0, keyName="A1", &
          exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
          computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
          totalLBound=tl, totalUBound=tu, totalCount=tc, & 
          farray=tmpR8, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check info
  !! data pointer
  do i=1,10
     if (tmpR8(i) .ne. REAL(i,ESMF_KIND_R8)) correct=.false.
  enddo

  !! Bounds
  if (el .ne. 1) correct=.false.
  if (eu .ne. 10) correct=.false.
  if (ec .ne. 10) correct=.false.
  if (cl .ne. 1) correct=.false.
  if (cu .ne. 10) correct=.false.
  if (cc .ne. 10) correct=.false.
  if (tl .ne. 1) correct=.false.
  if (tu .ne. 10) correct=.false.
  if (tc .ne. 10) correct=.false.


  call ESMF_LocStreamDestroy(locstream,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! deallocate array
  deallocate(keyDataR8)


  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test getting keyUnits and keyLongName"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! Create Grid
  locstream=ESMF_LocStreamCreate(maxIndex=20, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
     
  ! Add key A1
  call ESMF_LocStreamAddKey(locstream, keyName="A1", keyUnits="U1", keyLongName="LN1", rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Add key A2
  call ESMF_LocStreamAddKey(locstream, keyName="A2", keyUnits="U2", keyLongName="LN2", rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get info for A1
  call ESMF_LocStreamGetKey(locstream, keyName="A1", keyUnits=keyUnits, keyLongName=keyLongName, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check info
  if (trim(keyUnits) .ne. "U1") correct=.false.
  if (trim(keyLongName) .ne. "LN1") correct=.false.


  ! Get info for A2
  call ESMF_LocStreamGetKey(locstream, keyName="A2", keyUnits=keyUnits, keyLongName=keyLongName, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check info
  if (trim(keyUnits) .ne. "U2") correct=.false.
  if (trim(keyLongName) .ne. "LN2") correct=.false.

  call ESMF_LocStreamDestroy(locstream,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  ! NOTE THAT SERIALIZE/DESERIALIZE IS AN INTERNAL INTERFACE AND NOT INTENDED FOR PUBLIC USE
  !NEX_UTest
  write(name, *) "Test LocStream Serialize/Deserialize"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! Create Grid
  locstream=ESMF_LocStreamCreate(maxIndex=20, indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
     
  ! Add key A1
  call ESMF_LocStreamAddKey(locstream, keyName="A1", keyUnits="U1", keyLongName="LN1", rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Add key A2
  call ESMF_LocStreamAddKey(locstream, keyName="A2", keyUnits="U2", keyLongName="LN2", rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Create a buffer to put the locstream in
  offset=0
  bufCount = 1
  allocate (buf(bufCount))
  call ESMF_LocStreamSerialize(locstream, buf, bufCount, offset,  &
    inquireflag=ESMF_INQUIREONLY, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE                            
  deallocate (buf)

  bufCount=offset
  print *, 'ESMF_LocStreamUTest: serialization buffer size =', bufCount
  allocate(buf(bufCount))

  ! Serialize
  offset=0
  call ESMF_LocStreamSerialize(locstream, buf, bufCount, offset, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Deserialize
  offset=0
  locstream2=ESMF_LocStreamDeserialize(buf, offset, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Check loc stream info
  call ESMF_LocStreamGet(locstream2, keyCount=keyCount, indexflag=indexflag, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check info
  if (keyCount .ne. 2) correct=.false.
  if (.not. (indexflag .eq. ESMF_INDEX_GLOBAL)) correct=.false.

  ! Get info for A1
  call ESMF_LocStreamGetKey(locstream2, keyName="A1", keyUnits=keyUnits, keyLongName=keyLongName, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check info
  if (trim(keyUnits) .ne. "U1") correct=.false.
  if (trim(keyLongName) .ne. "LN1") correct=.false.

  ! Get info for A2
  call ESMF_LocStreamGetKey(locstream2, keyName="A2", keyUnits=keyUnits, keyLongName=keyLongName, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check info
  if (trim(keyUnits) .ne. "U2") correct=.false.
  if (trim(keyLongName) .ne. "LN2") correct=.false.

  call ESMF_LocStreamDestroy(locstream,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_LocStreamDestroy(locstream2,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get rid of buffer
  deallocate(buf)

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test Print"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! Create Grid
  locstream=ESMF_LocStreamCreate(maxIndex=20, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
     
  ! Add key A1
  call ESMF_LocStreamAddKey(locstream, keyName="A1", keyUnits="U1", keyLongName="LN1", rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Add key A2
  call ESMF_LocStreamAddKey(locstream, keyName="A2", keyUnits="U2", keyLongName="LN2", rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Test Print
  call ESMF_LocStreamPrint(locstream, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_LocStreamDestroy(locstream,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test ESMF_LocStreamCreate from Regular distribution"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! Create Grid
  locstream=ESMF_LocStreamCreate(minIndex=2, maxIndex=20, regDecomp=4, indexflag=ESMF_INDEX_GLOBAL, &
             rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  if (petCount .eq. 1) then
     ! Check non-key bounds
     call  ESMF_LocStreamGet(locstream, localDE=0, &
             exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
             computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
             rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     ! Bounds
     if (el .ne. 2) correct=.false.
     if (eu .ne. 6) correct=.false.
     if (ec .ne. 5) correct=.false.
     if (cl .ne. 2) correct=.false.
     if (cu .ne. 6) correct=.false.
     if (cc .ne. 5) correct=.false.

!write(*,*) "0", correct, el, eu, ec
     ! Check non-key bounds
     call  ESMF_LocStreamGet(locstream, localDE=1, &
             exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
             computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
             rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     ! Bounds
     if (el .ne. 7) correct=.false.
     if (eu .ne. 11) correct=.false.
     if (ec .ne. 5) correct=.false.
     if (cl .ne. 7) correct=.false.
     if (cu .ne. 11) correct=.false.
     if (cc .ne. 5) correct=.false.

!write(*,*) "1", correct, el, eu, ec
     ! Check non-key bounds
     call  ESMF_LocStreamGet(locstream, localDE=2, &
             exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
             computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
             rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     ! Bounds
     if (el .ne. 12) correct=.false.
     if (eu .ne. 16) correct=.false.
     if (ec .ne. 5) correct=.false.
     if (cl .ne. 12) correct=.false.
     if (cu .ne. 16) correct=.false.
     if (cc .ne. 5) correct=.false.

!write(*,*) "2", correct, el, eu, ec
     ! Check non-key bounds
     call  ESMF_LocStreamGet(locstream, localDE=3, &
             exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
             computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
             rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     ! Bounds
     if (el .ne. 17) correct=.false.
     if (eu .ne. 20) correct=.false.
     if (ec .ne. 4) correct=.false.
     if (cl .ne. 17) correct=.false.
     if (cu .ne. 20) correct=.false.
     if (cc .ne. 4) correct=.false.

!write(*,*) "3", correct, el, eu, ec
  else   if (petCount .eq. 4) then
     if (localPet .eq. 0) then
        ! Check non-key bounds
        call  ESMF_LocStreamGet(locstream, localDE=0, &
                exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
                computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
                rc=localrc)
        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

        ! Bounds
        if (el .ne. 2) correct=.false.
        if (eu .ne. 6) correct=.false.
        if (ec .ne. 5) correct=.false.
        if (cl .ne. 2) correct=.false.
        if (cu .ne. 6) correct=.false.
        if (cc .ne. 5) correct=.false.
     else if (localPet .eq. 1) then
        ! Check non-key bounds
        call  ESMF_LocStreamGet(locstream, localDE=0, &
                exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
                computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
                rc=localrc)
        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

        ! Bounds
        if (el .ne. 7) correct=.false.
        if (eu .ne. 11) correct=.false.
        if (ec .ne. 5) correct=.false.
        if (cl .ne. 7) correct=.false.
        if (cu .ne. 11) correct=.false.
        if (cc .ne. 5) correct=.false.

     else if (localPet .eq. 2) then
        ! Check non-key bounds
        call  ESMF_LocStreamGet(locstream, localDE=0, &
               exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
               computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
               rc=localrc)
        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

        ! Bounds
        if (el .ne. 12) correct=.false.
        if (eu .ne. 16) correct=.false.
        if (ec .ne. 5) correct=.false.
        if (cl .ne. 12) correct=.false.
        if (cu .ne. 16) correct=.false.
        if (cc .ne. 5) correct=.false.

     else if (localPet .eq. 3) then
        ! Check non-key bounds
        call  ESMF_LocStreamGet(locstream, localDE=0, &
               exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
               computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
               rc=localrc)
        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

        ! Bounds
        if (el .ne. 17) correct=.false.
        if (eu .ne. 20) correct=.false.
        if (ec .ne. 4) correct=.false.
        if (cl .ne. 17) correct=.false.
        if (cu .ne. 20) correct=.false.
        if (cc .ne. 4) correct=.false.
    endif
  endif

  call ESMF_LocStreamDestroy(locstream,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test ESMF_LocStreamCreate from Irregular distribution"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! Create Grid
  locstream=ESMF_LocStreamCreate(minIndex=2, countsPerDE=(/5,5,5,4/), indexflag=ESMF_INDEX_GLOBAL, &
             rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  if (petCount .eq. 1) then
     ! Check non-key bounds
     call  ESMF_LocStreamGet(locstream, localDE=0, &
             exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
             computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
             rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     ! Bounds
     if (el .ne. 2) correct=.false.
     if (eu .ne. 6) correct=.false.
     if (ec .ne. 5) correct=.false.
     if (cl .ne. 2) correct=.false.
     if (cu .ne. 6) correct=.false.
     if (cc .ne. 5) correct=.false.

!write(*,*) "0", correct, el, eu, ec
     ! Check non-key bounds
     call  ESMF_LocStreamGet(locstream, localDE=1, &
             exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
             computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
             rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     ! Bounds
     if (el .ne. 7) correct=.false.
     if (eu .ne. 11) correct=.false.
     if (ec .ne. 5) correct=.false.
     if (cl .ne. 7) correct=.false.
     if (cu .ne. 11) correct=.false.
     if (cc .ne. 5) correct=.false.

!write(*,*) "1", correct, el, eu, ec
     ! Check non-key bounds
     call  ESMF_LocStreamGet(locstream, localDE=2, &
             exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
             computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
             rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     ! Bounds
     if (el .ne. 12) correct=.false.
     if (eu .ne. 16) correct=.false.
     if (ec .ne. 5) correct=.false.
     if (cl .ne. 12) correct=.false.
     if (cu .ne. 16) correct=.false.
     if (cc .ne. 5) correct=.false.

!write(*,*) "2", correct, el, eu, ec
     ! Check non-key bounds
     call  ESMF_LocStreamGet(locstream, localDE=3, &
             exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
             computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
             rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     ! Bounds
     if (el .ne. 17) correct=.false.
     if (eu .ne. 20) correct=.false.
     if (ec .ne. 4) correct=.false.
     if (cl .ne. 17) correct=.false.
     if (cu .ne. 20) correct=.false.
     if (cc .ne. 4) correct=.false.

!write(*,*) "3", correct, el, eu, ec
  else   if (petCount .eq. 4) then
     if (localPet .eq. 0) then
        ! Check non-key bounds
        call  ESMF_LocStreamGet(locstream, localDE=0, &
                exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
                computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
                rc=localrc)
        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

        ! Bounds
        if (el .ne. 2) correct=.false.
        if (eu .ne. 6) correct=.false.
        if (ec .ne. 5) correct=.false.
        if (cl .ne. 2) correct=.false.
        if (cu .ne. 6) correct=.false.
        if (cc .ne. 5) correct=.false.
     else if (localPet .eq. 1) then
        ! Check non-key bounds
        call  ESMF_LocStreamGet(locstream, localDE=0, &
                exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
                computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
                rc=localrc)
        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

        ! Bounds
        if (el .ne. 7) correct=.false.
        if (eu .ne. 11) correct=.false.
        if (ec .ne. 5) correct=.false.
        if (cl .ne. 7) correct=.false.
        if (cu .ne. 11) correct=.false.
        if (cc .ne. 5) correct=.false.

     else if (localPet .eq. 2) then
        ! Check non-key bounds
        call  ESMF_LocStreamGet(locstream, localDE=0, &
               exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
               computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
               rc=localrc)
        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

        ! Bounds
        if (el .ne. 12) correct=.false.
        if (eu .ne. 16) correct=.false.
        if (ec .ne. 5) correct=.false.
        if (cl .ne. 12) correct=.false.
        if (cu .ne. 16) correct=.false.
        if (cc .ne. 5) correct=.false.

     else if (localPet .eq. 3) then
        ! Check non-key bounds
        call  ESMF_LocStreamGet(locstream, localDE=0, &
               exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
               computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
               rc=localrc)
        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

        ! Bounds
        if (el .ne. 17) correct=.false.
        if (eu .ne. 20) correct=.false.
        if (ec .ne. 4) correct=.false.
        if (cl .ne. 17) correct=.false.
        if (cu .ne. 20) correct=.false.
        if (cc .ne. 4) correct=.false.
    endif
  endif

  call ESMF_LocStreamDestroy(locstream,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  ! Destroy distgrid
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

end program ESMF_LocStreamCreateUTest
