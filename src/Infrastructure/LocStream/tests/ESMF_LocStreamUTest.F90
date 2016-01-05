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
  integer :: localrc, rc, petCount, localPet
  logical :: correct
  logical :: isCreated

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name, locstream_name
  character(ESMF_MAXSTR) :: keyNames(3)
  character(ESMF_MAXSTR) :: keyUnits,keyLongName
  type(ESMF_VM) :: vm
  type(ESMF_DistGrid) :: distgrid, distgridOut
  type(ESMF_DELayout) :: delayout
  type(ESMF_Index_Flag) :: indexflag
  type(ESMF_LocStream) :: locstream, locstream2, newlocstream, locstreamAlias
  integer :: ec,el,eu,cc,cl,cu,tc,tl,tu
  integer :: keyCount, localDECount, localDECountOut, i
  real(ESMF_KIND_R8), pointer :: keyDataR8(:),tmpR8(:)
  real(ESMF_KIND_R4), pointer :: keyDataR4(:),tmpR4(:)
  integer (ESMF_KIND_I4), pointer :: keyDataI4(:),tmpI4(:)
  integer :: bufCount, offset
  character, pointer :: buf(:)
  integer :: pntCount
  type(ESMF_Mesh) :: mesh
  real(ESMF_KIND_R8), allocatable :: X(:),Y(:)
  real(ESMF_KIND_R8), pointer :: tstX(:),tstY(:)
  integer, allocatable :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), allocatable :: nodeCoords(:)
  integer :: numNodes, numElems
  integer, allocatable :: elemIds(:),elemTypes(:),elemConn(:)
  logical:: locstreamBool



  !-----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !-----------------------------------------------------------------------------

  ! get global VM
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


  ! prepare DistGrid
  distgrid=ESMF_DistGridCreate(minIndex=(/1/),maxIndex=(/10/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing LocStream IsCreated for uncreated object"
  write(failMsg, *) "Did not return .false."
  isCreated = ESMF_LocStreamIsCreated(locstream)
  call ESMF_Test((isCreated .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing LocStream IsCreated for uncreated object"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_LocStreamIsCreated(locstream, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Create test LocStream for IsCreated"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  locstream=ESMF_LocStreamCreate(name="test",distgrid=distgrid, rc=localrc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing LocStream IsCreated for created object"
  write(failMsg, *) "Did not return .true."
  isCreated = ESMF_LocStreamIsCreated(locstream)
  call ESMF_Test((isCreated .eqv. .true.), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing LocStream IsCreated for created object"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_LocStreamIsCreated(locstream, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroy test LocStream for IsCreated"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_LocStreamDestroy(locstream, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing LocStream IsCreated for destroyed object"
  write(failMsg, *) "Did not return .false."
  isCreated = ESMF_LocStreamIsCreated(locstream)
  call ESMF_Test((isCreated .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing LocStream IsCreated for destroyed object"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_LocStreamIsCreated(locstream, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

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
     call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
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
     call  ESMF_LocStreamGetBounds(locstream, localDE=1, &
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
     call  ESMF_LocStreamGetBounds(locstream, localDE=2, &
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
     call  ESMF_LocStreamGetBounds(locstream, localDE=3, &
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
        call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
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
        call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
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
        call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
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
        call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
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

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "LocStream equality before assignment Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  locstreamBool = (locstreamAlias.eq.locstream)
  call ESMF_Test(.not.locstreamBool, name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  ! Testing ESMF_LocStreamAssignment(=)()
  write(name, *) "LocStream assignment and equality Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  locstreamAlias = locstream
  locstreamBool = (locstreamAlias.eq.locstream)
  call ESMF_Test(locstreamBool, name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "LocStreamDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_LocStreamDestroy(locstream, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  ! Testing ESMF_LocStreamOperator(==)()
  write(name, *) "LocStream equality after destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  locstreamBool = (locstreamAlias==locstream)
  call ESMF_Test(.not.locstreamBool, name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  ! Testing ESMF_LocStreamOperator(/=)()
  write(name, *) "LocStream non-equality after destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  locstreamBool = (locstreamAlias/=locstream)
  call ESMF_Test(locstreamBool, name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Double LocStreamDestroy through alias Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_LocStreamDestroy(locstreamAlias, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  


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
     call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
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
     call  ESMF_LocStreamGetBounds(locstream, localDE=1, &
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
     call  ESMF_LocStreamGetBounds(locstream, localDE=2, &
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
     call  ESMF_LocStreamGetBounds(locstream, localDE=3, &
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
        call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
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
        call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
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
        call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
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
        call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
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
  call ESMF_LocStreamGetBounds(locstream, localDE=0, &
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
  call ESMF_LocStreamGetBounds(locstream, localDE=0, &
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
  call  ESMF_LocStreamGetKey(locstream, keyName="A1", &
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
  call  ESMF_LocStreamGetKey(locstream, keyName="A1", &
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
  call  ESMF_LocStreamGetKey(locstream, keyName="A1", &
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
     call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
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
     call  ESMF_LocStreamGetBounds(locstream, localDE=1, &
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
     call  ESMF_LocStreamGetBounds(locstream, localDE=2, &
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
     call  ESMF_LocStreamGetBounds(locstream, localDE=3, &
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
        call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
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
        call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
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
        call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
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
        call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
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
     call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
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
     call  ESMF_LocStreamGetBounds(locstream, localDE=1, &
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
     call  ESMF_LocStreamGetBounds(locstream, localDE=2, &
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
     call  ESMF_LocStreamGetBounds(locstream, localDE=3, &
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
        call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
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
        call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
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
        call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
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
        call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
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
  write(name, *) "Test ESMF_LocStream Create From Background Mesh"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  !!!!!!!! Create Mesh !!!!!!!!!!!
  ! Create Mesh
  ! Only do this if we have 4 PETs
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
         coordSys=ESMF_COORDSYS_SPH_DEG, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, &
         rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! deallocate node data
  deallocate(nodeIds)
  deallocate(nodeCoords)
  deallocate(nodeOwners)

  ! deallocate elem data
  deallocate(elemIds)
  deallocate(elemTypes)
  deallocate(elemConn)


  !!!!!!!! Create LocStream !!!!!!!!!!!
  ! Set number of points
  if (localPet .eq. 0) then  
      pntCount=2
  else if (localPet .eq. 1) then  
      pntCount=3
  else if (localPet .eq. 2) then  
      pntCount=1
  else if (localPet .eq. 3) then  
      pntCount=1
  endif


  ! Create LocStream
  locstream=ESMF_LocStreamCreate(localCount=pntCount,  rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    goto 100
  endif
     
  ! Allocate X array
  allocate(X(pntCount))

  ! allocate Y array
  allocate(Y(pntCount))


 ! Fill in points
  if (localPet .eq. 0) then  
      X(1)=1.0
      Y(1)=1.0
      X(2)=0.5
      Y(2)=1.5
  else if (localPet .eq. 1) then  
      X(1)=1.5
      Y(1)=0.5
      X(2)=1.5
      Y(2)=1.5
      X(3)=1.9
      Y(3)=1.75
  else if (localPet .eq. 2) then  
      X(1)=0.5
      Y(1)=0.5
  else if (localPet .eq. 3) then  
      X(1)=1.9
      Y(1)=0.1
  endif


  ! Add key X
  call ESMF_LocStreamAddKey(locstream, keyName="ESMF:Lon", farray=X,  rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    goto 100
  endif

  ! Add key Y
  call ESMF_LocStreamAddKey(locstream, keyName="ESMF:Lat", farray=Y,  rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    goto 100
  endif

  ! Do locStream create from background mesh
  newLocstream=ESMF_LocStreamCreate(locstream, &
                 background=mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     goto 100
  endif

  call ESMF_LocStreamDestroy(locstream,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    goto 100
  endif

  ! deallocate array
  deallocate(X)
  deallocate(Y)


  ! Get rid of Mesh
  call ESMF_MeshDestroy(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    goto 100
  endif

  !!!!!!!!! Check results !!!!!!!!!!!!!!!!!
  call ESMF_LocStreamGetKey(newLocStream,keyName="ESMF:Lon", &
         farray=tstX, &
         exclusiveLBound=el, exclusiveUBound=eu, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    goto 100
  endif

  call ESMF_LocStreamGetKey(newLocStream,keyName="ESMF:Lat", &
         farray=tstY, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    goto 100
  endif

  ! Test points
  if (localPet .eq. 0) then  
      do i=el,eu
         if ((tstX(i) <  0.0) .or. tstX(i) >  1.0) correct=.false.
         if ((tstY(i) <  0.0) .or. tstY(i) >  1.0) correct=.false.
      enddo
  else if (localPet .eq. 1) then  
      do i=el,eu
         if ((tstX(i) <= 1.0) .or. tstX(i) > 2.0) correct=.false.
         if ((tstY(i) <  0.0) .or. tstY(i) > 1.0) correct=.false.
      enddo
  else if (localPet .eq. 2) then  
      do i=el,eu
         if ((tstX(i) <  0.0) .or. tstX(i) >  1.0) correct=.false.
         if ((tstY(i) <= 1.0) .or. tstY(i) >  2.0) correct=.false.
      enddo 
  else if (localPet .eq. 3) then  
      do i=el,eu
         if ((tstX(i) <= 1.0) .or. tstX(i) >  2.0) correct=.false.
         if ((tstY(i) <= 1.0) .or. tstY(i) >  2.0) correct=.false.
      enddo 
  endif  


  call ESMF_LocStreamDestroy(newLocstream,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    goto 100
  endif

  ! endif for skip for ==4 proc
  endif 

100 continue
  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test ESMF_LocStream Create From Background Grid"
  write(failMsg, *) "Test unsuccessful"
 
  ! initialize 
  rc=ESMF_SUCCESS
      
  ! do test
  call test_locstreambkg(rc)
 
  ! return result
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
 !-----------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test ESMF_LocStream Create From Background Grid non-default Align"
  write(failMsg, *) "Test unsuccessful"
 
  ! initialize 
  rc=ESMF_SUCCESS
      
  ! do test
  call test_locstreambkgnda(rc)

  ! return result
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
 !-----------------------------------------------------------------------------


  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test ESMF_LocStream Create From Background Grid on Sphere"
  write(failMsg, *) "Test unsuccessful"
 
  ! initialize 
  rc=ESMF_SUCCESS
      
  ! do test
  call test_locstreambkgSph(rc)

  ! return result
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
 !-----------------------------------------------------------------------------


  ! Destroy distgrid
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


contains

      subroutine test_locstreambkg(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: gridA
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: i1,i2,i3, index(2)
  real(ESMF_KIND_R8) :: coord(2)
  integer A_nx, A_ny
  real(ESMF_KIND_R8) :: A_minx,A_miny
  real(ESMF_KIND_R8) :: A_maxx,A_maxy
  integer :: localPet, petCount
  real(ESMF_KIND_R8) :: de_minx, de_maxx
  real(ESMF_KIND_R8) :: de_miny, de_maxy
  integer :: pntCount
  real(ESMF_KIND_R8), pointer :: X(:),Y(:)
  real(ESMF_KIND_R8), pointer :: tstX(:),tstY(:)
  type(ESMF_LocStream) :: locstream,  newlocstream
  real(ESMF_KIND_R8) :: tmpXC, tmpYC

  ! result code
  integer :: finalrc
  
  ! init success flag
  correct=.true.
  rc=ESMF_SUCCESS

  ! get pet info
  call ESMF_VMGetGlobal(vm, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif
 
  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Establish the resolution of the grids
  A_nx = 16
  A_ny = 16


  ! Establish the coordinates of the grids
  A_minx = 0.0
  A_miny = 0.0
  
  A_maxx = 2.0
  A_maxy = 2.0
  
  ! setup source grid
  gridA=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/A_nx,A_ny/),regDecomp=(/petCount,1/), &
                                 coordSys=ESMF_COORDSYS_CART, &
                                 indexflag=ESMF_INDEX_GLOBAL, &
                                 rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Allocate coordinates
  call ESMF_GridAddCoord(gridA, staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Construct Grid A
  ! (Get memory and set coords for src)
  !! get coord 1
  call ESMF_GridGetCoord(gridA, localDE=0, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  call ESMF_GridGetCoord(gridA, localDE=0, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  !! set coords, interpolated function
  do i1=clbnd(1),cubnd(1)
  do i2=clbnd(2),cubnd(2)

     ! Set source coordinates
     farrayPtrXC(i1,i2) = ((A_maxx-A_minx)*REAL(i1-1)/REAL(A_nx-1))+A_minx
     farrayPtrYC(i1,i2) = ((A_maxy-A_miny)*REAL(i2-1)/REAL(A_ny-1))+A_miny

  enddo
  enddo


  !!!!!!!! Create LocStream !!!!!!!!!!!
  ! Set number of points
  if (localPet .eq. 0) then  
      pntCount=2
  else if (localPet .eq. 1) then  
      pntCount=3
  else if (localPet .eq. 2) then  
      pntCount=1
  else if (localPet .eq. 3) then  
      pntCount=1
  endif


  ! Create LocStream
  locstream=ESMF_LocStreamCreate(localCount=pntCount, &
                                 coordSys=ESMF_COORDSYS_CART, &
                                 rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
     
  ! Allocate X array
  allocate(X(pntCount))

  ! allocate Y array
  allocate(Y(pntCount))


 ! Fill in points
  if (localPet .eq. 0) then  
      X(1)=1.0
      Y(1)=1.0
      X(2)=0.5
      Y(2)=1.5
  else if (localPet .eq. 1) then  
      X(1)=1.5
      Y(1)=0.5
      X(2)=1.5
      Y(2)=1.5
      X(3)=1.9
      Y(3)=1.75
  else if (localPet .eq. 2) then  
      X(1)=0.5
      Y(1)=0.5
  else if (localPet .eq. 3) then  
      X(1)=1.9
      Y(1)=0.1
  endif


  ! Add key X
  call ESMF_LocStreamAddKey(locstream, keyName="ESMF:X", farray=X,  rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Add key Y
  call ESMF_LocStreamAddKey(locstream, keyName="ESMF:Y", farray=Y,  rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Do locStream create from background mesh
  newLocstream=ESMF_LocStreamCreate(locstream, &
                 background=gridA, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
  

  call ESMF_LocStreamDestroy(locstream,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! deallocate array
  deallocate(X)
  deallocate(Y)



  ! Test Newly Created LocStream
  ! Since the grid is setup to be rectilinear checking the min/max should be fine
 
  !! get coord 1
  call ESMF_GridGetCoord(gridA, localDE=0, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
                            farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

  call ESMF_GridGetCoord(gridA, localDE=0, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  !! Init min/max of DE
  de_minx=A_maxx
  de_miny=A_maxy
  de_maxx=A_minx
  de_maxy=A_miny

  !! Adjust loop to cover min-max of cells not just nodes
  !! i.e. if not the last proc extend by 1 to cover other point of cell
  if (localPet .lt. petCount-1) cubnd(1) =cubnd(1)+1

  !! set coords, interpolated function
  do i1=clbnd(1),cubnd(1)
  do i2=clbnd(2),cubnd(2)

     ! Set source coordinates
     tmpXC = ((A_maxx-A_minx)*REAL(i1-1)/REAL(A_nx-1))+A_minx
     tmpYC = ((A_maxy-A_miny)*REAL(i2-1)/REAL(A_ny-1))+A_miny

     ! Min/max off coordinates
     if (tmpXC < de_minx) de_minx=tmpXC 
     if (tmpXC > de_maxx) de_maxx=tmpXC

     if (tmpYC < de_miny) de_miny=tmpYC
     if (tmpYC > de_maxy) de_maxy=tmpYC

  enddo
  enddo


  !!!!!!!!! Check locstream points vs Grid min max !!!!!!!!!!!!!!!!!
  call ESMF_LocStreamGetKey(newlocStream,keyName="ESMF:X", &
         farray=tstX, &
         exclusiveLBound=el, exclusiveUBound=eu, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_LocStreamGetKey(newLocStream,keyName="ESMF:Y", &
         farray=tstY, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Test points
  do i=el,eu
     if ((tstX(i) <  de_minx) .or. (tstX(i) >  de_maxx)) then
        write(*,*) tstX(i),"not in [",de_minx,de_maxx,"]"
        correct=.false.
     endif

     if ((tstY(i) <  de_miny) .or. (tstY(i) >  de_maxy)) then
        write(*,*) tstY(i),"not in [",de_miny,de_maxy,"]"
        correct=.false.
     endif
   enddo


  ! Get rid of the new locstream
  call ESMF_LocStreamDestroy(newLocstream,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  
  ! Free the grids
  call ESMF_GridDestroy(gridA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_locstreambkg



      subroutine test_locstreambkgnda(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: gridA
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: i1,i2,i3, index(2)
  real(ESMF_KIND_R8) :: coord(2)
  integer A_nx, A_ny
  real(ESMF_KIND_R8) :: A_minx,A_miny
  real(ESMF_KIND_R8) :: A_maxx,A_maxy
  integer :: localPet, petCount
  real(ESMF_KIND_R8) :: de_minx, de_maxx
  real(ESMF_KIND_R8) :: de_miny, de_maxy
  integer :: pntCount
  real(ESMF_KIND_R8), pointer :: X(:),Y(:)
  real(ESMF_KIND_R8), pointer :: tstX(:),tstY(:)
  type(ESMF_LocStream) :: locstream,  newlocstream
  real(ESMF_KIND_R8) :: tmpXC, tmpYC

  ! result code
  integer :: finalrc
  
  ! init success flag
  correct=.true.
  rc=ESMF_SUCCESS

  ! get pet info
  call ESMF_VMGetGlobal(vm, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif
 
  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Establish the resolution of the grids
  A_nx = 16
  A_ny = 16


  ! Establish the coordinates of the grids
  A_minx = 0.0
  A_miny = 0.0
  
  A_maxx = 2.0
  A_maxy = 2.0
  
  ! setup source grid
  gridA=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/A_nx,A_ny/),regDecomp=(/petCount,1/), &
                                 coordSys=ESMF_COORDSYS_CART, &
                                 gridAlign=(/1,1/),indexflag=ESMF_INDEX_GLOBAL, &
                                 rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Allocate coordinates
  call ESMF_GridAddCoord(gridA, staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Construct Grid A
  ! (Get memory and set coords for src)
  !! get coord 1
  call ESMF_GridGetCoord(gridA, localDE=0, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  call ESMF_GridGetCoord(gridA, localDE=0, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  !! set coords, interpolated function
  do i1=clbnd(1),cubnd(1)
  do i2=clbnd(2),cubnd(2)

     ! Set source coordinates
     farrayPtrXC(i1,i2) = ((A_maxx-A_minx)*REAL(i1-1)/REAL(A_nx-1))+A_minx
     farrayPtrYC(i1,i2) = ((A_maxy-A_miny)*REAL(i2-1)/REAL(A_ny-1))+A_miny

  enddo
  enddo


  !!!!!!!! Create LocStream !!!!!!!!!!!
  ! Set number of points
  if (localPet .eq. 0) then  
      pntCount=2
  else if (localPet .eq. 1) then  
      pntCount=3
  else if (localPet .eq. 2) then  
      pntCount=1
  else if (localPet .eq. 3) then  
      pntCount=1
  endif


  ! Create LocStream
  locstream=ESMF_LocStreamCreate(localCount=pntCount, &
                                 coordSys=ESMF_COORDSYS_CART, &
                                 rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
     
  ! Allocate X array
  allocate(X(pntCount))

  ! allocate Y array
  allocate(Y(pntCount))


 ! Fill in points
  if (localPet .eq. 0) then  
      X(1)=1.0
      Y(1)=1.0
      X(2)=0.5
      Y(2)=1.5
  else if (localPet .eq. 1) then  
      X(1)=1.5
      Y(1)=0.5
      X(2)=1.5
      Y(2)=1.5
      X(3)=1.9
      Y(3)=1.75
  else if (localPet .eq. 2) then  
      X(1)=0.5
      Y(1)=0.5
  else if (localPet .eq. 3) then  
      X(1)=1.9
      Y(1)=0.1
  endif


  ! Add key X
  call ESMF_LocStreamAddKey(locstream, keyName="ESMF:X", farray=X,  rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Add key Y
  call ESMF_LocStreamAddKey(locstream, keyName="ESMF:Y", farray=Y,  rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Do locStream create from background mesh
  newLocstream=ESMF_LocStreamCreate(locstream, &
                 background=gridA, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
  

  call ESMF_LocStreamDestroy(locstream,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! deallocate array
  deallocate(X)
  deallocate(Y)



  ! Test Newly Created LocStream
  ! Since the grid is setup to be rectilinear checking the min/max should be fine
 
  !! get coord 1
  call ESMF_GridGetCoord(gridA, localDE=0, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
                            farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

  call ESMF_GridGetCoord(gridA, localDE=0, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  !! Init min/max of DE
  de_minx=A_maxx
  de_miny=A_maxy
  de_maxx=A_minx
  de_maxy=A_miny

  !! Adjust loop to cover min-max of cells not just nodes
  !! i.e. if not the first proc extend by 1 downward to cover other point of cell
  !!      Note that this is the other direction than the other test because of
  !!      the difference in GridAlign
  if (localPet .gt. 0) clbnd(1) =clbnd(1)-1

  !! set coords, interpolated function
  do i1=clbnd(1),cubnd(1)
  do i2=clbnd(2),cubnd(2)

     ! Set source coordinates
     tmpXC = ((A_maxx-A_minx)*REAL(i1-1)/REAL(A_nx-1))+A_minx
     tmpYC = ((A_maxy-A_miny)*REAL(i2-1)/REAL(A_ny-1))+A_miny

     ! Min/max off coordinates
     if (tmpXC < de_minx) de_minx=tmpXC 
     if (tmpXC > de_maxx) de_maxx=tmpXC

     if (tmpYC < de_miny) de_miny=tmpYC
     if (tmpYC > de_maxy) de_maxy=tmpYC

  enddo
  enddo


  !!!!!!!!! Check locstream points vs Grid min max !!!!!!!!!!!!!!!!!
  call ESMF_LocStreamGetKey(newlocStream,keyName="ESMF:X", &
         farray=tstX, &
         exclusiveLBound=el, exclusiveUBound=eu, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_LocStreamGetKey(newLocStream,keyName="ESMF:Y", &
         farray=tstY, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Test points
  do i=el,eu
     if ((tstX(i) <  de_minx) .or. (tstX(i) >  de_maxx)) then
        write(*,*) tstX(i),"not in [",de_minx,de_maxx,"]"
        correct=.false.
     endif

     if ((tstY(i) <  de_miny) .or. (tstY(i) >  de_maxy)) then
        write(*,*) tstY(i),"not in [",de_miny,de_maxy,"]"
        correct=.false.
     endif
   enddo


  ! Get rid of the new locstream
  call ESMF_LocStreamDestroy(newLocstream,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  
  ! Free the grids
  call ESMF_GridDestroy(gridA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_locstreambkgnda


      subroutine test_locstreambkgsph(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: gridA
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrLonC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrLatC(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: i1,i2,i3, index(2)
  real(ESMF_KIND_R8) :: coord(2)
  integer A_nlon, A_nlat
  integer :: localPet, petCount
  real(ESMF_KIND_R8) :: de_minlon, de_maxlon
  real(ESMF_KIND_R8) :: de_minlat, de_maxlat
  integer :: pntCount
  real(ESMF_KIND_R8), allocatable :: Lon(:),Lat(:)
  real(ESMF_KIND_R8), pointer :: tstLon(:),tstLat(:)
  type(ESMF_LocStream) :: locstream,  newlocstream
  real(ESMF_KIND_R8) :: tmpLonC, tmpLatC
  real(ESMF_KIND_R8) :: A_dlon, A_dlat

  ! result code
  integer :: finalrc
  
  ! init success flag
  correct=.true.
  rc=ESMF_SUCCESS

  ! get pet info
  call ESMF_VMGetGlobal(vm, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif
 
  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Establish the resolution of the grids
  A_nlon = 16
  A_nlat = 16

  
  ! setup source grid
  gridA=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/A_nlon,A_nlat/),regDecomp=(/petCount,1/), &
                              coordSys=ESMF_COORDSYS_SPH_DEG, &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Allocate coordinates
  call ESMF_GridAddCoord(gridA, staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Construct Grid A
  ! (Get memory and set coords for src)
  !! get coord 1
  call ESMF_GridGetCoord(gridA, localDE=0, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrLonC, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  call ESMF_GridGetCoord(gridA, localDE=0, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrLatC, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  A_dlon=360.0/A_nlon
  A_dlat=180.0/A_nlat

  !! set coords, interpolated function
  do i1=clbnd(1),cubnd(1)
  do i2=clbnd(2),cubnd(2)

    ! Set source coordinates as 0 to 360
    farrayPtrLonC(i1,i2) = REAL(i1-1)*A_dlon

    ! Set source coordinates as -90 to 90
    farrayPtrLatC(i1,i2) = -90. + (REAL(i2-1)*A_dlat + 0.5*A_dlat)

  enddo
  enddo


  !!!!!!!! Create LocStream !!!!!!!!!!!
  ! Set number of points
  if (localPet .eq. 0) then  
      pntCount=2
  else if (localPet .eq. 1) then  
      pntCount=3
  else if (localPet .eq. 2) then  
      pntCount=1
  else if (localPet .eq. 3) then  
      pntCount=1
  endif


  ! Create LocStream
  locstream=ESMF_LocStreamCreate(localCount=pntCount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
     

  ! Allocate Lon array
  allocate(Lon(pntCount))

  ! allocate Lat array
  allocate(Lat(pntCount))


 ! Fill in points
  if (localPet .eq. 0) then  
      Lon(1)=190.0
      Lat(1)=10.0
      Lon(2)=280.0
      Lat(2)=-15.0
  else if (localPet .eq. 1) then  
      Lon(1)=10.0
      Lat(1)=50.0
      Lon(2)=20.0
      Lat(2)=40.0
      Lon(3)=100.0
      Lat(3)=0.0
  else if (localPet .eq. 2) then  
      Lon(1)=355.0
      Lat(1)=-60.0
  else if (localPet .eq. 3) then  
      Lon(1)=170.0
      Lat(1)=-80.0
  endif


  ! Add key Lon
  call ESMF_LocStreamAddKey(locstream, keyName="ESMF:Lon", farray=Lon,  rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Add key Lat
  call ESMF_LocStreamAddKey(locstream, keyName="ESMF:Lat", farray=Lat,  rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Do locStream create from background mesh
  newLocstream=ESMF_LocStreamCreate(locstream, &
                 background=gridA, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  
  call ESMF_LocStreamDestroy(locstream,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! deallocate array
  deallocate(Lon)
  deallocate(Lat)



  ! Test Newly Created LocStream
  ! Since the grid is setup to be rectilinear checking the min/max should be fine
 
  !! get coord 1
  call ESMF_GridGetCoord(gridA, localDE=0, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
                            farrayPtr=farrayPtrLonC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

  call ESMF_GridGetCoord(gridA, localDE=0, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrLatC, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  !! Init min/max of DE
  de_minlon=360.0
  de_minlat=90.0
  de_maxlon=0.0
  de_maxlat=-90.0

  !! Adjust loop to cover min-max of cells not just nodes
  !! i.e. extend by 1 to cover other point of cell.
  if (localPet .lt. PetCount-1) cubnd(1) =cubnd(1)+1

  !! set coords, interpolated function
  do i1=clbnd(1),cubnd(1)
  do i2=clbnd(2),cubnd(2)

     ! Set source coordinates
    ! Set source coordinates as 0 to 360
    tmpLonC = REAL(i1-1)*A_dlon

    ! Set source coordinates as -90 to 90
    tmpLatC = -90. + (REAL(i2-1)*A_dlat + 0.5*A_dlat)
 
     ! Min/max off coordinates
     if (tmpLonC < de_minlon) de_minlon=tmpLonC 
     if (tmpLonC > de_maxlon) de_maxlon=tmpLonC

     if (tmpLatC < de_minlat) de_minlat=tmpLatC
     if (tmpLatC > de_maxlat) de_maxlat=tmpLatC

  enddo
  enddo

 !  write(*,*) localPet," [",de_minlon,de_maxlon,"]"

  !!!!!!!!! Check locstream points vs Grid min max !!!!!!!!!!!!!!!!!
  call ESMF_LocStreamGetKey(newlocStream,keyName="ESMF:Lon", &
         farray=tstLon, &
         exclusiveLBound=el, exclusiveUBound=eu, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_LocStreamGetKey(newLocStream,keyName="ESMF:Lat", &
         farray=tstLat, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Test points
  do i=el,eu
     if ((tstLon(i) <  de_minlon) .or. (tstLon(i) >  de_maxlon)) then
        write(*,*) tstLon(i),"not in [",de_minlon,de_maxlon,"]"
        correct=.false.
     endif

     if ((tstLat(i) <  de_minlat) .or. (tstLat(i) >  de_maxlat)) then
        write(*,*) tstLat(i),"not in [",de_minlat,de_maxlat,"]"
        correct=.false.
     endif
   enddo


  ! Get rid of the new locstream
  call ESMF_LocStreamDestroy(newLocstream,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  
  ! Free the grids
  call ESMF_GridDestroy(gridA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_locstreambkgsph


end program ESMF_LocStreamCreateUTest
