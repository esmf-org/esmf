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
  type(ESMF_CoordSys_Flag) :: coordSys
  

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


!!!XXX  !------------------------------------------------------------------------
!!!XXX  !NEX_UTest
!!!XXX  write(name, *) "Testing LocStream IsCreated for uncreated object"
!!!XXX  write(failMsg, *) "Did not return .false."
!!!XXX  isCreated = ESMF_LocStreamIsCreated(locstream)
!!!XXX  call ESMF_Test((isCreated .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)
!!!XXX  !------------------------------------------------------------------------
!!!XXX
!!!XXX  !------------------------------------------------------------------------
!!!XXX  !NEX_UTest
!!!XXX  write(name, *) "Testing LocStream IsCreated for uncreated object"
!!!XXX  write(failMsg, *) "Did not return ESMF_SUCCESS"
!!!XXX  isCreated = ESMF_LocStreamIsCreated(locstream, rc=rc)
!!!XXX  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!!!XXX  !------------------------------------------------------------------------
!!!XXX
!!!XXX  !------------------------------------------------------------------------
!!!XXX  !NEX_UTest
!!!XXX  write(name, *) "Create test LocStream for IsCreated"
!!!XXX  write(failMsg, *) "Did not return ESMF_SUCCESS"
!!!XXX  locstream=ESMF_LocStreamCreate(name="test",distgrid=distgrid, rc=localrc)
!!!XXX  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!!!XXX  !------------------------------------------------------------------------
!!!XXX
!!!XXX  !------------------------------------------------------------------------
!!!XXX  !NEX_UTest
!!!XXX  write(name, *) "Testing LocStream IsCreated for created object"
!!!XXX  write(failMsg, *) "Did not return .true."
!!!XXX  isCreated = ESMF_LocStreamIsCreated(locstream)
!!!XXX  call ESMF_Test((isCreated .eqv. .true.), name, failMsg, result, ESMF_SRCLINE)
!!!XXX  !------------------------------------------------------------------------
!!!XXX
!!!XXX  !------------------------------------------------------------------------
!!!XXX  !NEX_UTest
!!!XXX  write(name, *) "Testing LocStream IsCreated for created object"
!!!XXX  write(failMsg, *) "Did not return ESMF_SUCCESS"
!!!XXX  isCreated = ESMF_LocStreamIsCreated(locstream, rc=rc)
!!!XXX  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!!!XXX  !------------------------------------------------------------------------
!!!XXX
!!!XXX  !------------------------------------------------------------------------
!!!XXX  !NEX_UTest
!!!XXX  write(name, *) "Destroy test LocStream for IsCreated"
!!!XXX  write(failMsg, *) "Did not return ESMF_SUCCESS"
!!!XXX  call ESMF_LocStreamDestroy(locstream, rc=rc)
!!!XXX  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!!!XXX  !------------------------------------------------------------------------
!!!XXX
!!!XXX  !------------------------------------------------------------------------
!!!XXX  !NEX_UTest
!!!XXX  write(name, *) "Testing LocStream IsCreated for destroyed object"
!!!XXX  write(failMsg, *) "Did not return .false."
!!!XXX  isCreated = ESMF_LocStreamIsCreated(locstream)
!!!XXX  call ESMF_Test((isCreated .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)
!!!XXX  !------------------------------------------------------------------------
!!!XXX
!!!XXX  !------------------------------------------------------------------------
!!!XXX  !NEX_UTest
!!!XXX  write(name, *) "Testing LocStream IsCreated for destroyed object"
!!!XXX  write(failMsg, *) "Did not return ESMF_SUCCESS"
!!!XXX  isCreated = ESMF_LocStreamIsCreated(locstream, rc=rc)
!!!XXX  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!!!XXX  !------------------------------------------------------------------------
!!!XXX
!!!XXX  !-----------------------------------------------------------------------------
!!!XXX  !NEX_UTest
!!!XXX  write(name, *) "Testing LocStream Validate"
!!!XXX  write(failMsg, *) "Did not return ESMF_SUCCESS"
!!!XXX
!!!XXX  ! initialize check variables
!!!XXX  correct=.true.
!!!XXX  rc=ESMF_SUCCESS
!!!XXX
!!!XXX  ! First make sure validate fails for an uncreated Grid
!!!XXX  call ESMF_LocStreamValidate(locstream,rc=localrc)
!!!XXX  if (localrc .eq. ESMF_SUCCESS) correct=.false.
!!!XXX
!!!XXX  ! Now make sure that a created grid validates successfully
!!!XXX  !! Create Grid
!!!XXX  locstream=ESMF_LocStreamCreate(name="ted",distgrid=distgrid, rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX     
!!!XXX  !! Check that validate returns true
!!!XXX  call ESMF_LocStreamValidate(locstream, rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) correct=.false.
!!!XXX  
!!!XXX  call ESMF_LocStreamDestroy(locstream,rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  !! Check that validate returns false
!!!XXX  call ESMF_LocStreamValidate(locstream, rc=localrc)
!!!XXX  if (localrc .eq. ESMF_SUCCESS) correct=.false.
!!!XXX
!!!XXX  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
!!!XXX  !-----------------------------------------------------------------------------
!!!XXX
!!!XXX
!!!XXX
!!!XXX  !-----------------------------------------------------------------------------
!!!XXX  !NEX_UTest
!!!XXX  write(name, *) "Test ESMF_LocStreamCreate from Regular distribution"
!!!XXX  write(failMsg, *) "Did not return ESMF_SUCCESS"
!!!XXX
!!!XXX  ! initialize check variables
!!!XXX  correct=.true.
!!!XXX  rc=ESMF_SUCCESS
!!!XXX
!!!XXX  ! Create Grid
!!!XXX  locstream=ESMF_LocStreamCreate(minIndex=2, maxIndex=20, regDecomp=4, indexflag=ESMF_INDEX_GLOBAL, &
!!!XXX             rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  if (petCount .eq. 1) then
!!!XXX     ! Check non-key bounds
!!!XXX     call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
!!!XXX             exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
!!!XXX             computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
!!!XXX             rc=localrc)
!!!XXX     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX     ! Bounds
!!!XXX     if (el .ne. 2) correct=.false.
!!!XXX     if (eu .ne. 6) correct=.false.
!!!XXX     if (ec .ne. 5) correct=.false.
!!!XXX     if (cl .ne. 2) correct=.false.
!!!XXX     if (cu .ne. 6) correct=.false.
!!!XXX     if (cc .ne. 5) correct=.false.
!!!XXX
!!!XXX     ! Check non-key bounds
!!!XXX     call  ESMF_LocStreamGetBounds(locstream, localDE=1, &
!!!XXX             exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
!!!XXX             computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
!!!XXX             rc=localrc)
!!!XXX     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX     ! Bounds
!!!XXX     if (el .ne. 7) correct=.false.
!!!XXX     if (eu .ne. 11) correct=.false.
!!!XXX     if (ec .ne. 5) correct=.false.
!!!XXX     if (cl .ne. 7) correct=.false.
!!!XXX     if (cu .ne. 11) correct=.false.
!!!XXX     if (cc .ne. 5) correct=.false.
!!!XXX
!!!XXX     ! Check non-key bounds
!!!XXX     call  ESMF_LocStreamGetBounds(locstream, localDE=2, &
!!!XXX             exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
!!!XXX             computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
!!!XXX             rc=localrc)
!!!XXX     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX     ! Bounds
!!!XXX     if (el .ne. 12) correct=.false.
!!!XXX     if (eu .ne. 16) correct=.false.
!!!XXX     if (ec .ne. 5) correct=.false.
!!!XXX     if (cl .ne. 12) correct=.false.
!!!XXX     if (cu .ne. 16) correct=.false.
!!!XXX     if (cc .ne. 5) correct=.false.
!!!XXX
!!!XXX!write(*,*) "2", correct, el, eu, ec
!!!XXX     ! Check non-key bounds
!!!XXX     call  ESMF_LocStreamGetBounds(locstream, localDE=3, &
!!!XXX             exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
!!!XXX             computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
!!!XXX             rc=localrc)
!!!XXX     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX     ! Bounds
!!!XXX     if (el .ne. 17) correct=.false.
!!!XXX     if (eu .ne. 20) correct=.false.
!!!XXX     if (ec .ne. 4) correct=.false.
!!!XXX     if (cl .ne. 17) correct=.false.
!!!XXX     if (cu .ne. 20) correct=.false.
!!!XXX     if (cc .ne. 4) correct=.false.
!!!XXX
!!!XXX!write(*,*) "3", correct, el, eu, ec
!!!XXX  else   if (petCount .eq. 4) then
!!!XXX     if (localPet .eq. 0) then
!!!XXX        ! Check non-key bounds
!!!XXX        call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
!!!XXX                exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
!!!XXX                computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
!!!XXX                rc=localrc)
!!!XXX        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX        ! Bounds
!!!XXX        if (el .ne. 2) correct=.false.
!!!XXX        if (eu .ne. 6) correct=.false.
!!!XXX        if (ec .ne. 5) correct=.false.
!!!XXX        if (cl .ne. 2) correct=.false.
!!!XXX        if (cu .ne. 6) correct=.false.
!!!XXX        if (cc .ne. 5) correct=.false.
!!!XXX     else if (localPet .eq. 1) then
!!!XXX        ! Check non-key bounds
!!!XXX        call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
!!!XXX                exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
!!!XXX                computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
!!!XXX                rc=localrc)
!!!XXX        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX        ! Bounds
!!!XXX        if (el .ne. 7) correct=.false.
!!!XXX        if (eu .ne. 11) correct=.false.
!!!XXX        if (ec .ne. 5) correct=.false.
!!!XXX        if (cl .ne. 7) correct=.false.
!!!XXX        if (cu .ne. 11) correct=.false.
!!!XXX        if (cc .ne. 5) correct=.false.
!!!XXX
!!!XXX     else if (localPet .eq. 2) then
!!!XXX        ! Check non-key bounds
!!!XXX        call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
!!!XXX               exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
!!!XXX               computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
!!!XXX               rc=localrc)
!!!XXX        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX        ! Bounds
!!!XXX        if (el .ne. 12) correct=.false.
!!!XXX        if (eu .ne. 16) correct=.false.
!!!XXX        if (ec .ne. 5) correct=.false.
!!!XXX        if (cl .ne. 12) correct=.false.
!!!XXX        if (cu .ne. 16) correct=.false.
!!!XXX        if (cc .ne. 5) correct=.false.
!!!XXX
!!!XXX     else if (localPet .eq. 3) then
!!!XXX        ! Check non-key bounds
!!!XXX        call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
!!!XXX               exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
!!!XXX               computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
!!!XXX               rc=localrc)
!!!XXX        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX        ! Bounds
!!!XXX        if (el .ne. 17) correct=.false.
!!!XXX        if (eu .ne. 20) correct=.false.
!!!XXX        if (ec .ne. 4) correct=.false.
!!!XXX        if (cl .ne. 17) correct=.false.
!!!XXX        if (cu .ne. 20) correct=.false.
!!!XXX        if (cc .ne. 4) correct=.false.
!!!XXX    endif
!!!XXX  endif
!!!XXX
!!!XXX  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
!!!XXX  !-----------------------------------------------------------------------------
!!!XXX
!!!XXX  !------------------------------------------------------------------------
!!!XXX  !NEX_UTest
!!!XXX  write(name, *) "LocStream equality before assignment Test"
!!!XXX  write(failMsg, *) "Did not return ESMF_SUCCESS"
!!!XXX  locstreamBool = (locstreamAlias.eq.locstream)
!!!XXX  call ESMF_Test(.not.locstreamBool, name, failMsg, result, ESMF_SRCLINE)
!!!XXX  
!!!XXX  !------------------------------------------------------------------------
!!!XXX  !NEX_UTest
!!!XXX  ! Testing ESMF_LocStreamAssignment(=)()
!!!XXX  write(name, *) "LocStream assignment and equality Test"
!!!XXX  write(failMsg, *) "Did not return ESMF_SUCCESS"
!!!XXX  locstreamAlias = locstream
!!!XXX  locstreamBool = (locstreamAlias.eq.locstream)
!!!XXX  call ESMF_Test(locstreamBool, name, failMsg, result, ESMF_SRCLINE)
!!!XXX  
!!!XXX  !------------------------------------------------------------------------
!!!XXX  !NEX_UTest
!!!XXX  write(name, *) "LocStreamDestroy Test"
!!!XXX  write(failMsg, *) "Did not return ESMF_SUCCESS"
!!!XXX  call ESMF_LocStreamDestroy(locstream, rc=rc)
!!!XXX  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!!!XXX  
!!!XXX  !------------------------------------------------------------------------
!!!XXX  !NEX_UTest
!!!XXX  ! Testing ESMF_LocStreamOperator(==)()
!!!XXX  write(name, *) "LocStream equality after destroy Test"
!!!XXX  write(failMsg, *) "Did not return ESMF_SUCCESS"
!!!XXX  locstreamBool = (locstreamAlias==locstream)
!!!XXX  call ESMF_Test(.not.locstreamBool, name, failMsg, result, ESMF_SRCLINE)
!!!XXX  
!!!XXX  !------------------------------------------------------------------------
!!!XXX  !NEX_UTest
!!!XXX  ! Testing ESMF_LocStreamOperator(/=)()
!!!XXX  write(name, *) "LocStream non-equality after destroy Test"
!!!XXX  write(failMsg, *) "Did not return ESMF_SUCCESS"
!!!XXX  locstreamBool = (locstreamAlias/=locstream)
!!!XXX  call ESMF_Test(locstreamBool, name, failMsg, result, ESMF_SRCLINE)
!!!XXX  
!!!XXX  !------------------------------------------------------------------------
!!!XXX  !NEX_UTest
!!!XXX  write(name, *) "Double LocStreamDestroy through alias Test"
!!!XXX  write(failMsg, *) "Did not return ESMF_SUCCESS"
!!!XXX  call ESMF_LocStreamDestroy(locstreamAlias, rc=rc)
!!!XXX  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!!!XXX  
!!!XXX
!!!XXX
!!!XXX  !-----------------------------------------------------------------------------
!!!XXX  !NEX_UTest
!!!XXX  write(name, *) "Test ESMF_LocStreamCreate from Irregular distribution"
!!!XXX  write(failMsg, *) "Did not return ESMF_SUCCESS"
!!!XXX
!!!XXX  ! initialize check variables
!!!XXX  correct=.true.
!!!XXX  rc=ESMF_SUCCESS
!!!XXX
!!!XXX  ! Create Grid
!!!XXX  locstream=ESMF_LocStreamCreate(minIndex=2, countsPerDE=(/5,5,5,4/), indexflag=ESMF_INDEX_GLOBAL, &
!!!XXX             rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  if (petCount .eq. 1) then
!!!XXX     ! Check non-key bounds
!!!XXX     call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
!!!XXX             exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
!!!XXX             computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
!!!XXX             rc=localrc)
!!!XXX     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX     ! Bounds
!!!XXX     if (el .ne. 2) correct=.false.
!!!XXX     if (eu .ne. 6) correct=.false.
!!!XXX     if (ec .ne. 5) correct=.false.
!!!XXX     if (cl .ne. 2) correct=.false.
!!!XXX     if (cu .ne. 6) correct=.false.
!!!XXX     if (cc .ne. 5) correct=.false.
!!!XXX
!!!XXX!write(*,*) "0", correct, el, eu, ec
!!!XXX     ! Check non-key bounds
!!!XXX     call  ESMF_LocStreamGetBounds(locstream, localDE=1, &
!!!XXX             exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
!!!XXX             computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
!!!XXX             rc=localrc)
!!!XXX     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX     ! Bounds
!!!XXX     if (el .ne. 7) correct=.false.
!!!XXX     if (eu .ne. 11) correct=.false.
!!!XXX     if (ec .ne. 5) correct=.false.
!!!XXX     if (cl .ne. 7) correct=.false.
!!!XXX     if (cu .ne. 11) correct=.false.
!!!XXX     if (cc .ne. 5) correct=.false.
!!!XXX
!!!XXX!write(*,*) "1", correct, el, eu, ec
!!!XXX     ! Check non-key bounds
!!!XXX     call  ESMF_LocStreamGetBounds(locstream, localDE=2, &
!!!XXX             exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
!!!XXX             computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
!!!XXX             rc=localrc)
!!!XXX     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX     ! Bounds
!!!XXX     if (el .ne. 12) correct=.false.
!!!XXX     if (eu .ne. 16) correct=.false.
!!!XXX     if (ec .ne. 5) correct=.false.
!!!XXX     if (cl .ne. 12) correct=.false.
!!!XXX     if (cu .ne. 16) correct=.false.
!!!XXX     if (cc .ne. 5) correct=.false.
!!!XXX
!!!XXX!write(*,*) "2", correct, el, eu, ec
!!!XXX     ! Check non-key bounds
!!!XXX     call  ESMF_LocStreamGetBounds(locstream, localDE=3, &
!!!XXX             exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
!!!XXX             computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
!!!XXX             rc=localrc)
!!!XXX     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX     ! Bounds
!!!XXX     if (el .ne. 17) correct=.false.
!!!XXX     if (eu .ne. 20) correct=.false.
!!!XXX     if (ec .ne. 4) correct=.false.
!!!XXX     if (cl .ne. 17) correct=.false.
!!!XXX     if (cu .ne. 20) correct=.false.
!!!XXX     if (cc .ne. 4) correct=.false.
!!!XXX
!!!XXX!write(*,*) "3", correct, el, eu, ec
!!!XXX  else   if (petCount .eq. 4) then
!!!XXX     if (localPet .eq. 0) then
!!!XXX        ! Check non-key bounds
!!!XXX        call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
!!!XXX                exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
!!!XXX                computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
!!!XXX                rc=localrc)
!!!XXX        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX        ! Bounds
!!!XXX        if (el .ne. 2) correct=.false.
!!!XXX        if (eu .ne. 6) correct=.false.
!!!XXX        if (ec .ne. 5) correct=.false.
!!!XXX        if (cl .ne. 2) correct=.false.
!!!XXX        if (cu .ne. 6) correct=.false.
!!!XXX        if (cc .ne. 5) correct=.false.
!!!XXX     else if (localPet .eq. 1) then
!!!XXX        ! Check non-key bounds
!!!XXX        call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
!!!XXX                exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
!!!XXX                computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
!!!XXX                rc=localrc)
!!!XXX        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX        ! Bounds
!!!XXX        if (el .ne. 7) correct=.false.
!!!XXX        if (eu .ne. 11) correct=.false.
!!!XXX        if (ec .ne. 5) correct=.false.
!!!XXX        if (cl .ne. 7) correct=.false.
!!!XXX        if (cu .ne. 11) correct=.false.
!!!XXX        if (cc .ne. 5) correct=.false.
!!!XXX
!!!XXX     else if (localPet .eq. 2) then
!!!XXX        ! Check non-key bounds
!!!XXX        call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
!!!XXX               exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
!!!XXX               computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
!!!XXX               rc=localrc)
!!!XXX        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX        ! Bounds
!!!XXX        if (el .ne. 12) correct=.false.
!!!XXX        if (eu .ne. 16) correct=.false.
!!!XXX        if (ec .ne. 5) correct=.false.
!!!XXX        if (cl .ne. 12) correct=.false.
!!!XXX        if (cu .ne. 16) correct=.false.
!!!XXX        if (cc .ne. 5) correct=.false.
!!!XXX
!!!XXX     else if (localPet .eq. 3) then
!!!XXX        ! Check non-key bounds
!!!XXX        call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
!!!XXX               exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
!!!XXX               computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
!!!XXX               rc=localrc)
!!!XXX        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX        ! Bounds
!!!XXX        if (el .ne. 17) correct=.false.
!!!XXX        if (eu .ne. 20) correct=.false.
!!!XXX        if (ec .ne. 4) correct=.false.
!!!XXX        if (cl .ne. 17) correct=.false.
!!!XXX        if (cu .ne. 20) correct=.false.
!!!XXX        if (cc .ne. 4) correct=.false.
!!!XXX    endif
!!!XXX  endif
!!!XXX
!!!XXX  call ESMF_LocStreamDestroy(locstream,rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
!!!XXX  !-----------------------------------------------------------------------------
!!!XXX
!!!XXX
!!!XXX  !-----------------------------------------------------------------------------
!!!XXX  !NEX_UTest
!!!XXX  write(name, *) "Testing LocStream Create from localCount and indexflag=ESMF_INDEX_DELOCAL"
!!!XXX  write(failMsg, *) "Did not return ESMF_SUCCESS"
!!!XXX
!!!XXX  ! initialize check variables
!!!XXX  correct=.true.
!!!XXX  rc=ESMF_SUCCESS
!!!XXX
!!!XXX  ! Now make sure that a created grid validates successfully
!!!XXX  !! Create Grid
!!!XXX  locstream=ESMF_LocStreamCreate(localCount=10, indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX     
!!!XXX  ! Get Bounds
!!!XXX  call ESMF_LocStreamGetBounds(locstream, localDE=0, &
!!!XXX         exclusiveCount=ec, exclusiveLBound=el, exclusiveUBound=eu, &
!!!XXX         computationalCount=cc, computationalLBound=cl, computationalUBound=cu, rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  ! Check Bounds
!!!XXX  if (ec .ne. 10) correct=.false.
!!!XXX  if (el .ne. 1) correct=.false.
!!!XXX  if (eu .ne. 10) correct=.false.
!!!XXX
!!!XXX  if (cc .ne. 10) correct=.false.
!!!XXX  if (cl .ne. 1) correct=.false.
!!!XXX  if (cu .ne. 10) correct=.false.
!!!XXX
!!!XXX  
!!!XXX  call ESMF_LocStreamDestroy(locstream,rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
!!!XXX  !-----------------------------------------------------------------------------
!!!XXX
!!!XXX
!!!XXX  !-----------------------------------------------------------------------------
!!!XXX  !NEX_UTest
!!!XXX  write(name, *) "Testing LocStream Create from localCount and indexflag=ESMF_INDEX_GLOBAL"
!!!XXX  write(failMsg, *) "Did not return ESMF_SUCCESS"
!!!XXX
!!!XXX  ! initialize check variables
!!!XXX  correct=.true.
!!!XXX  rc=ESMF_SUCCESS
!!!XXX
!!!XXX  ! Now make sure that a created grid validates successfully
!!!XXX  !! Create Grid
!!!XXX  locstream=ESMF_LocStreamCreate(localCount=10, indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX     
!!!XXX  ! Get Bounds
!!!XXX  call ESMF_LocStreamGetBounds(locstream, localDE=0, &
!!!XXX         exclusiveCount=ec, exclusiveLBound=el, exclusiveUBound=eu, &
!!!XXX         computationalCount=cc, computationalLBound=cl, computationalUBound=cu, rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  ! Check Bounds
!!!XXX  if (ec .ne. 10) correct=.false.
!!!XXX  if (el .ne. (localPet*10)+1) correct=.false.
!!!XXX  if (eu .ne. (localPet+1)*10) correct=.false.
!!!XXX
!!!XXX  if (cc .ne. 10) correct=.false.
!!!XXX  if (cl .ne. (localPet*10)+1) correct=.false.
!!!XXX  if (cu .ne. (localPet+1)*10) correct=.false.
!!!XXX
!!!XXX  call ESMF_LocStreamDestroy(locstream,rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
!!!XXX  !-----------------------------------------------------------------------------
!!!XXX
!!!XXX  !-----------------------------------------------------------------------------
!!!XXX  !NEX_UTest
!!!XXX  write(name, *) "Testing LocStream Create defaults and LocStream Get"
!!!XXX  write(failMsg, *) "Did not return ESMF_SUCCESS"
!!!XXX
!!!XXX  ! initialize check variables
!!!XXX  correct=.true.
!!!XXX  rc=ESMF_SUCCESS
!!!XXX
!!!XXX  ! Create Grid
!!!XXX  locstream=ESMF_LocStreamCreate(name="LOCSTREAMXYZ", maxIndex=20, rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX     
!!!XXX  ! Get Info about LocStream
!!!XXX  call ESMF_LocStreamGet(locstream, distgrid=distgridOut, &
!!!XXX       keyCount=keyCount, localDECount=localDECount, indexflag=indexflag, name=locstream_name, rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  ! check info
!!!XXX  !! Make sure we start with keyCount = 0
!!!XXX  if (keyCount .ne. 0) correct=.false.
!!!XXX
!!!XXX  !! make sure localDECount matches
!!!XXX  call ESMF_DistGridGet(distgridOut,delayout=delayout, rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  call ESMF_DELayoutGet(delayout,localDECount=localDECountOut, rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  if (localDECount .ne. localDECountOut) correct=.false.
!!!XXX
!!!XXX  !! Check indexflag 
!!!XXX  if (.not. (indexflag .eq. ESMF_INDEX_DELOCAL)) correct=.false.
!!!XXX
!!!XXX  !! Check name
!!!XXX  if (trim(locstream_name) .ne. "LOCSTREAMXYZ") correct=.false.
!!!XXX  
!!!XXX  call ESMF_LocStreamDestroy(locstream,rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
!!!XXX  !-----------------------------------------------------------------------------
!!!XXX
!!!XXX
!!!XXX
!!!XXX
!!!XXX  !-----------------------------------------------------------------------------
!!!XXX  !NEX_UTest
!!!XXX  write(name, *) "Test keyCount and keyNames response to ESMF_LocStreamAddKey"
!!!XXX  write(failMsg, *) "Did not return ESMF_SUCCESS"
!!!XXX
!!!XXX  ! initialize check variables
!!!XXX  correct=.true.
!!!XXX  rc=ESMF_SUCCESS
!!!XXX
!!!XXX  ! Create Grid
!!!XXX  locstream=ESMF_LocStreamCreate(maxIndex=20, rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX     
!!!XXX  ! Get keyCount
!!!XXX  call ESMF_LocStreamGet(locstream, keyCount=keyCount, keyNames=keyNames, rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  ! check info
!!!XXX  !! Make sure we start with keyCount = 0
!!!XXX  if (keyCount .ne. 0) correct=.false.
!!!XXX
!!!XXX  ! Add key
!!!XXX  call ESMF_LocStreamAddKey(locstream, keyName="A1", rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  ! Get keyCount
!!!XXX  call ESMF_LocStreamGet(locstream, keyCount=keyCount, keyNames=keyNames, rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  ! check info
!!!XXX  if (keyCount .ne. 1) correct=.false.
!!!XXX  if (trim(keyNames(1)) .ne. "A1") correct=.false.
!!!XXX
!!!XXX  ! Add key
!!!XXX  call ESMF_LocStreamAddKey(locstream, keyName="A2", rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  ! Get keyCount
!!!XXX  call ESMF_LocStreamGet(locstream, keyCount=keyCount, keyNames=keyNames, rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX
!!!XXX  ! check info
!!!XXX  if (keyCount .ne. 2) correct=.false.
!!!XXX  if (trim(keyNames(1)) .ne. "A1") correct=.false.
!!!XXX  if (trim(keyNames(2)) .ne. "A2") correct=.false.
!!!XXX
!!!XXX  call ESMF_LocStreamDestroy(locstream,rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
!!!XXX  !-----------------------------------------------------------------------------
!!!XXX
!!!XXX  !-----------------------------------------------------------------------------
!!!XXX  !NEX_UTest
!!!XXX  write(name, *) "Test ESMF_LocStreamGetKey getting an I4 pointer and bounds"
!!!XXX  write(failMsg, *) "Did not return ESMF_SUCCESS"
!!!XXX
!!!XXX  ! initialize check variables
!!!XXX  correct=.true.
!!!XXX  rc=ESMF_SUCCESS
!!!XXX
!!!XXX  ! Create Grid
!!!XXX  locstream=ESMF_LocStreamCreate(localCount=10,  rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX     
!!!XXX  ! allocate array
!!!XXX  allocate(keyDataI4(10))
!!!XXX
!!!XXX  ! Add key
!!!XXX  call ESMF_LocStreamAddKey(locstream, keyName="A1", farray=keyDataI4,  rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  ! Fill key
!!!XXX  do i=1,10
!!!XXX     keyDataI4(i)=REAL(i,ESMF_KIND_I4)
!!!XXX  enddo
!!!XXX
!!!XXX  ! Get key and bounds
!!!XXX  call  ESMF_LocStreamGetKey(locstream, keyName="A1", &
!!!XXX          exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
!!!XXX          computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
!!!XXX          totalLBound=tl, totalUBound=tu, totalCount=tc, & 
!!!XXX          farray=tmpI4, rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  ! check info
!!!XXX  !! data pointer
!!!XXX  do i=1,10
!!!XXX     if (tmpI4(i) .ne. REAL(i,ESMF_KIND_I4)) correct=.false.
!!!XXX  enddo
!!!XXX
!!!XXX  !! Bounds
!!!XXX  if (el .ne. 1) correct=.false.
!!!XXX  if (eu .ne. 10) correct=.false.
!!!XXX  if (ec .ne. 10) correct=.false.
!!!XXX  if (cl .ne. 1) correct=.false.
!!!XXX  if (cu .ne. 10) correct=.false.
!!!XXX  if (cc .ne. 10) correct=.false.
!!!XXX  if (tl .ne. 1) correct=.false.
!!!XXX  if (tu .ne. 10) correct=.false.
!!!XXX  if (tc .ne. 10) correct=.false.
!!!XXX
!!!XXX
!!!XXX  call ESMF_LocStreamDestroy(locstream,rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  ! deallocate array
!!!XXX  deallocate(keyDataI4)
!!!XXX
!!!XXX
!!!XXX  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
!!!XXX  !-----------------------------------------------------------------------------
!!!XXX
!!!XXX  !-----------------------------------------------------------------------------
!!!XXX  !NEX_UTest
!!!XXX  write(name, *) "Test ESMF_LocStreamGetKey getting an R4 pointer and bounds"
!!!XXX  write(failMsg, *) "Did not return ESMF_SUCCESS"
!!!XXX
!!!XXX  ! initialize check variables
!!!XXX  correct=.true.
!!!XXX  rc=ESMF_SUCCESS
!!!XXX
!!!XXX  ! Create Grid
!!!XXX  locstream=ESMF_LocStreamCreate(localCount=10,  rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX     
!!!XXX  ! allocate array
!!!XXX  allocate(keyDataR4(10))
!!!XXX
!!!XXX  ! Add key
!!!XXX  call ESMF_LocStreamAddKey(locstream, keyName="A1", farray=keyDataR4,  rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  ! Fill key
!!!XXX  do i=1,10
!!!XXX     keyDataR4(i)=REAL(i,ESMF_KIND_R4)
!!!XXX  enddo
!!!XXX
!!!XXX  ! Get key and bounds
!!!XXX  call  ESMF_LocStreamGetKey(locstream, keyName="A1", &
!!!XXX          exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
!!!XXX          computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
!!!XXX          totalLBound=tl, totalUBound=tu, totalCount=tc, & 
!!!XXX          farray=tmpR4, rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  ! check info
!!!XXX  !! data pointer
!!!XXX  do i=1,10
!!!XXX     if (tmpR4(i) .ne. REAL(i,ESMF_KIND_R4)) correct=.false.
!!!XXX  enddo
!!!XXX
!!!XXX  !! Bounds
!!!XXX  if (el .ne. 1) correct=.false.
!!!XXX  if (eu .ne. 10) correct=.false.
!!!XXX  if (ec .ne. 10) correct=.false.
!!!XXX  if (cl .ne. 1) correct=.false.
!!!XXX  if (cu .ne. 10) correct=.false.
!!!XXX  if (cc .ne. 10) correct=.false.
!!!XXX  if (tl .ne. 1) correct=.false.
!!!XXX  if (tu .ne. 10) correct=.false.
!!!XXX  if (tc .ne. 10) correct=.false.
!!!XXX
!!!XXX
!!!XXX  call ESMF_LocStreamDestroy(locstream,rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  ! deallocate array
!!!XXX  deallocate(keyDataR4)
!!!XXX
!!!XXX
!!!XXX  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
!!!XXX  !-----------------------------------------------------------------------------
!!!XXX
!!!XXX
!!!XXX  !-----------------------------------------------------------------------------
!!!XXX  !NEX_UTest
!!!XXX  write(name, *) "Test ESMF_LocStreamGetKey getting an R8 pointer and bounds"
!!!XXX  write(failMsg, *) "Did not return ESMF_SUCCESS"
!!!XXX
!!!XXX  ! initialize check variables
!!!XXX  correct=.true.
!!!XXX  rc=ESMF_SUCCESS
!!!XXX
!!!XXX  ! Create Grid
!!!XXX  locstream=ESMF_LocStreamCreate(localCount=10,  rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX     
!!!XXX  ! allocate array
!!!XXX  allocate(keyDataR8(10))
!!!XXX
!!!XXX  ! Add key
!!!XXX  call ESMF_LocStreamAddKey(locstream, keyName="A1", farray=keyDataR8,  rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  ! Fill key
!!!XXX  do i=1,10
!!!XXX     keyDataR8(i)=REAL(i,ESMF_KIND_R8)
!!!XXX  enddo
!!!XXX
!!!XXX  ! Get key and bounds
!!!XXX  call  ESMF_LocStreamGetKey(locstream, keyName="A1", &
!!!XXX          exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
!!!XXX          computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
!!!XXX          totalLBound=tl, totalUBound=tu, totalCount=tc, & 
!!!XXX          farray=tmpR8, rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  ! check info
!!!XXX  !! data pointer
!!!XXX  do i=1,10
!!!XXX     if (tmpR8(i) .ne. REAL(i,ESMF_KIND_R8)) correct=.false.
!!!XXX  enddo
!!!XXX
!!!XXX  !! Bounds
!!!XXX  if (el .ne. 1) correct=.false.
!!!XXX  if (eu .ne. 10) correct=.false.
!!!XXX  if (ec .ne. 10) correct=.false.
!!!XXX  if (cl .ne. 1) correct=.false.
!!!XXX  if (cu .ne. 10) correct=.false.
!!!XXX  if (cc .ne. 10) correct=.false.
!!!XXX  if (tl .ne. 1) correct=.false.
!!!XXX  if (tu .ne. 10) correct=.false.
!!!XXX  if (tc .ne. 10) correct=.false.
!!!XXX
!!!XXX
!!!XXX  call ESMF_LocStreamDestroy(locstream,rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  ! deallocate array
!!!XXX  deallocate(keyDataR8)
!!!XXX
!!!XXX
!!!XXX  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
!!!XXX  !-----------------------------------------------------------------------------
!!!XXX
!!!XXX  !-----------------------------------------------------------------------------
!!!XXX  !NEX_UTest
!!!XXX  write(name, *) "Test getting keyUnits and keyLongName"
!!!XXX  write(failMsg, *) "Did not return ESMF_SUCCESS"
!!!XXX
!!!XXX  ! initialize check variables
!!!XXX  correct=.true.
!!!XXX  rc=ESMF_SUCCESS
!!!XXX
!!!XXX  ! Create Grid
!!!XXX  locstream=ESMF_LocStreamCreate(maxIndex=20, rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX     
!!!XXX  ! Add key A1
!!!XXX  call ESMF_LocStreamAddKey(locstream, keyName="A1", keyUnits="U1", keyLongName="LN1", rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  ! Add key A2
!!!XXX  call ESMF_LocStreamAddKey(locstream, keyName="A2", keyUnits="U2", keyLongName="LN2", rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  ! Get info for A1
!!!XXX  call ESMF_LocStreamGetKey(locstream, keyName="A1", keyUnits=keyUnits, keyLongName=keyLongName, rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  ! check info
!!!XXX  if (trim(keyUnits) .ne. "U1") correct=.false.
!!!XXX  if (trim(keyLongName) .ne. "LN1") correct=.false.
!!!XXX
!!!XXX
!!!XXX  ! Get info for A2
!!!XXX  call ESMF_LocStreamGetKey(locstream, keyName="A2", keyUnits=keyUnits, keyLongName=keyLongName, rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  ! check info
!!!XXX  if (trim(keyUnits) .ne. "U2") correct=.false.
!!!XXX  if (trim(keyLongName) .ne. "LN2") correct=.false.
!!!XXX
!!!XXX  call ESMF_LocStreamDestroy(locstream,rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
!!!XXX  !-----------------------------------------------------------------------------
!!!XXX
!!!XXX  !-----------------------------------------------------------------------------
!!!XXX  ! NOTE THAT SERIALIZE/DESERIALIZE IS AN INTERNAL INTERFACE AND NOT INTENDED FOR PUBLIC USE
!!!XXX  !NEX_UTest
!!!XXX  write(name, *) "Test LocStream Serialize/Deserialize"
!!!XXX  write(failMsg, *) "Did not return ESMF_SUCCESS"
!!!XXX
!!!XXX  ! initialize check variables
!!!XXX  correct=.true.
!!!XXX  rc=ESMF_SUCCESS
!!!XXX
!!!XXX  ! Create Grid
!!!XXX  locstream=ESMF_LocStreamCreate(maxIndex=20, indexflag=ESMF_INDEX_GLOBAL, coordSys=ESMF_COORDSYS_SPH_DEG, &
!!!XXX       rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX     
!!!XXX  ! Add key A1
!!!XXX  call ESMF_LocStreamAddKey(locstream, keyName="A1", keyUnits="U1", keyLongName="LN1", rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  ! Add key A2
!!!XXX  call ESMF_LocStreamAddKey(locstream, keyName="A2", keyUnits="U2", keyLongName="LN2", rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX
!!!XXX  ! Create a buffer to put the locstream in
!!!XXX  offset=0
!!!XXX  bufCount = 1
!!!XXX  allocate (buf(bufCount))
!!!XXX  call ESMF_LocStreamSerialize(locstream, buf, bufCount, offset,  &
!!!XXX    inquireflag=ESMF_INQUIREONLY, rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE                            
!!!XXX  deallocate (buf)
!!!XXX
!!!XXX  bufCount=offset
!!!XXX  print *, 'ESMF_LocStreamUTest: serialization buffer size =', bufCount
!!!XXX  allocate(buf(bufCount))
!!!XXX
!!!XXX  ! Serialize
!!!XXX  offset=0
!!!XXX  call ESMF_LocStreamSerialize(locstream, buf, bufCount, offset, rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  ! Deserialize
!!!XXX  offset=0
!!!XXX  locstream2=ESMF_LocStreamDeserialize(buf, offset, rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX
!!!XXX  ! Check loc stream info
!!!XXX  call ESMF_LocStreamGet(locstream2, keyCount=keyCount, indexflag=indexflag, coordSys=coordSys, &
!!!XXX       rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  ! check info
!!!XXX  if (keyCount .ne. 2) correct=.false.
!!!XXX  if (.not. (indexflag .eq. ESMF_INDEX_GLOBAL)) correct=.false.
!!!XXX  if (.not. (coordSys .eq. ESMF_COORDSYS_SPH_DEG)) correct=.false.
!!!XXX
!!!XXX  ! Get info for A1
!!!XXX  call ESMF_LocStreamGetKey(locstream2, keyName="A1", keyUnits=keyUnits, keyLongName=keyLongName, rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  ! check info
!!!XXX  if (trim(keyUnits) .ne. "U1") correct=.false.
!!!XXX  if (trim(keyLongName) .ne. "LN1") correct=.false.
!!!XXX
!!!XXX  ! Get info for A2
!!!XXX  call ESMF_LocStreamGetKey(locstream2, keyName="A2", keyUnits=keyUnits, keyLongName=keyLongName, rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  ! check info
!!!XXX  if (trim(keyUnits) .ne. "U2") correct=.false.
!!!XXX  if (trim(keyLongName) .ne. "LN2") correct=.false.
!!!XXX
!!!XXX  call ESMF_LocStreamDestroy(locstream,rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  call ESMF_LocStreamDestroy(locstream2,rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  ! Get rid of buffer
!!!XXX  deallocate(buf)
!!!XXX
!!!XXX  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
!!!XXX  !-----------------------------------------------------------------------------
!!!XXX
!!!XXX
!!!XXX
!!!XXX  !-----------------------------------------------------------------------------
!!!XXX  !NEX_UTest
!!!XXX  write(name, *) "Test Print"
!!!XXX  write(failMsg, *) "Did not return ESMF_SUCCESS"
!!!XXX
!!!XXX  ! initialize check variables
!!!XXX  correct=.true.
!!!XXX  rc=ESMF_SUCCESS
!!!XXX
!!!XXX  ! Create Grid
!!!XXX  locstream=ESMF_LocStreamCreate(maxIndex=20, rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX     
!!!XXX  ! Add key A1
!!!XXX  call ESMF_LocStreamAddKey(locstream, keyName="A1", keyUnits="U1", keyLongName="LN1", rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  ! Add key A2
!!!XXX  call ESMF_LocStreamAddKey(locstream, keyName="A2", keyUnits="U2", keyLongName="LN2", rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  ! Test Print
!!!XXX  call ESMF_LocStreamPrint(locstream, rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  call ESMF_LocStreamDestroy(locstream,rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
!!!XXX  !-----------------------------------------------------------------------------
!!!XXX
!!!XXX
!!!XXX  !-----------------------------------------------------------------------------
!!!XXX  !NEX_UTest
!!!XXX  write(name, *) "Test ESMF_LocStreamCreate from Regular distribution"
!!!XXX  write(failMsg, *) "Did not return ESMF_SUCCESS"
!!!XXX
!!!XXX  ! initialize check variables
!!!XXX  correct=.true.
!!!XXX  rc=ESMF_SUCCESS
!!!XXX
!!!XXX  ! Create Grid
!!!XXX  locstream=ESMF_LocStreamCreate(minIndex=2, maxIndex=20, regDecomp=4, indexflag=ESMF_INDEX_GLOBAL, &
!!!XXX             rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  if (petCount .eq. 1) then
!!!XXX     ! Check non-key bounds
!!!XXX     call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
!!!XXX             exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
!!!XXX             computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
!!!XXX             rc=localrc)
!!!XXX     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX     ! Bounds
!!!XXX     if (el .ne. 2) correct=.false.
!!!XXX     if (eu .ne. 6) correct=.false.
!!!XXX     if (ec .ne. 5) correct=.false.
!!!XXX     if (cl .ne. 2) correct=.false.
!!!XXX     if (cu .ne. 6) correct=.false.
!!!XXX     if (cc .ne. 5) correct=.false.
!!!XXX
!!!XXX!write(*,*) "0", correct, el, eu, ec
!!!XXX     ! Check non-key bounds
!!!XXX     call  ESMF_LocStreamGetBounds(locstream, localDE=1, &
!!!XXX             exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
!!!XXX             computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
!!!XXX             rc=localrc)
!!!XXX     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX     ! Bounds
!!!XXX     if (el .ne. 7) correct=.false.
!!!XXX     if (eu .ne. 11) correct=.false.
!!!XXX     if (ec .ne. 5) correct=.false.
!!!XXX     if (cl .ne. 7) correct=.false.
!!!XXX     if (cu .ne. 11) correct=.false.
!!!XXX     if (cc .ne. 5) correct=.false.
!!!XXX
!!!XXX!write(*,*) "1", correct, el, eu, ec
!!!XXX     ! Check non-key bounds
!!!XXX     call  ESMF_LocStreamGetBounds(locstream, localDE=2, &
!!!XXX             exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
!!!XXX             computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
!!!XXX             rc=localrc)
!!!XXX     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX     ! Bounds
!!!XXX     if (el .ne. 12) correct=.false.
!!!XXX     if (eu .ne. 16) correct=.false.
!!!XXX     if (ec .ne. 5) correct=.false.
!!!XXX     if (cl .ne. 12) correct=.false.
!!!XXX     if (cu .ne. 16) correct=.false.
!!!XXX     if (cc .ne. 5) correct=.false.
!!!XXX
!!!XXX!write(*,*) "2", correct, el, eu, ec
!!!XXX     ! Check non-key bounds
!!!XXX     call  ESMF_LocStreamGetBounds(locstream, localDE=3, &
!!!XXX             exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
!!!XXX             computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
!!!XXX             rc=localrc)
!!!XXX     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX     ! Bounds
!!!XXX     if (el .ne. 17) correct=.false.
!!!XXX     if (eu .ne. 20) correct=.false.
!!!XXX     if (ec .ne. 4) correct=.false.
!!!XXX     if (cl .ne. 17) correct=.false.
!!!XXX     if (cu .ne. 20) correct=.false.
!!!XXX     if (cc .ne. 4) correct=.false.
!!!XXX
!!!XXX!write(*,*) "3", correct, el, eu, ec
!!!XXX  else   if (petCount .eq. 4) then
!!!XXX     if (localPet .eq. 0) then
!!!XXX        ! Check non-key bounds
!!!XXX        call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
!!!XXX                exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
!!!XXX                computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
!!!XXX                rc=localrc)
!!!XXX        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX        ! Bounds
!!!XXX        if (el .ne. 2) correct=.false.
!!!XXX        if (eu .ne. 6) correct=.false.
!!!XXX        if (ec .ne. 5) correct=.false.
!!!XXX        if (cl .ne. 2) correct=.false.
!!!XXX        if (cu .ne. 6) correct=.false.
!!!XXX        if (cc .ne. 5) correct=.false.
!!!XXX     else if (localPet .eq. 1) then
!!!XXX        ! Check non-key bounds
!!!XXX        call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
!!!XXX                exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
!!!XXX                computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
!!!XXX                rc=localrc)
!!!XXX        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX        ! Bounds
!!!XXX        if (el .ne. 7) correct=.false.
!!!XXX        if (eu .ne. 11) correct=.false.
!!!XXX        if (ec .ne. 5) correct=.false.
!!!XXX        if (cl .ne. 7) correct=.false.
!!!XXX        if (cu .ne. 11) correct=.false.
!!!XXX        if (cc .ne. 5) correct=.false.
!!!XXX
!!!XXX     else if (localPet .eq. 2) then
!!!XXX        ! Check non-key bounds
!!!XXX        call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
!!!XXX               exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
!!!XXX               computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
!!!XXX               rc=localrc)
!!!XXX        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX        ! Bounds
!!!XXX        if (el .ne. 12) correct=.false.
!!!XXX        if (eu .ne. 16) correct=.false.
!!!XXX        if (ec .ne. 5) correct=.false.
!!!XXX        if (cl .ne. 12) correct=.false.
!!!XXX        if (cu .ne. 16) correct=.false.
!!!XXX        if (cc .ne. 5) correct=.false.
!!!XXX
!!!XXX     else if (localPet .eq. 3) then
!!!XXX        ! Check non-key bounds
!!!XXX        call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
!!!XXX               exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
!!!XXX               computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
!!!XXX               rc=localrc)
!!!XXX        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX        ! Bounds
!!!XXX        if (el .ne. 17) correct=.false.
!!!XXX        if (eu .ne. 20) correct=.false.
!!!XXX        if (ec .ne. 4) correct=.false.
!!!XXX        if (cl .ne. 17) correct=.false.
!!!XXX        if (cu .ne. 20) correct=.false.
!!!XXX        if (cc .ne. 4) correct=.false.
!!!XXX    endif
!!!XXX  endif
!!!XXX
!!!XXX  call ESMF_LocStreamDestroy(locstream,rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
!!!XXX  !-----------------------------------------------------------------------------
!!!XXX
!!!XXX
!!!XXX
!!!XXX  !-----------------------------------------------------------------------------
!!!XXX  !NEX_UTest
!!!XXX  write(name, *) "Test ESMF_LocStreamCreate from Irregular distribution"
!!!XXX  write(failMsg, *) "Did not return ESMF_SUCCESS"
!!!XXX
!!!XXX  ! initialize check variables
!!!XXX  correct=.true.
!!!XXX  rc=ESMF_SUCCESS
!!!XXX
!!!XXX  ! Create Grid
!!!XXX  locstream=ESMF_LocStreamCreate(minIndex=2, countsPerDE=(/5,5,5,4/), indexflag=ESMF_INDEX_GLOBAL, &
!!!XXX             rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  if (petCount .eq. 1) then
!!!XXX     ! Check non-key bounds
!!!XXX     call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
!!!XXX             exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
!!!XXX             computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
!!!XXX             rc=localrc)
!!!XXX     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX     ! Bounds
!!!XXX     if (el .ne. 2) correct=.false.
!!!XXX     if (eu .ne. 6) correct=.false.
!!!XXX     if (ec .ne. 5) correct=.false.
!!!XXX     if (cl .ne. 2) correct=.false.
!!!XXX     if (cu .ne. 6) correct=.false.
!!!XXX     if (cc .ne. 5) correct=.false.
!!!XXX
!!!XXX!write(*,*) "0", correct, el, eu, ec
!!!XXX     ! Check non-key bounds
!!!XXX     call  ESMF_LocStreamGetBounds(locstream, localDE=1, &
!!!XXX             exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
!!!XXX             computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
!!!XXX             rc=localrc)
!!!XXX     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX     ! Bounds
!!!XXX     if (el .ne. 7) correct=.false.
!!!XXX     if (eu .ne. 11) correct=.false.
!!!XXX     if (ec .ne. 5) correct=.false.
!!!XXX     if (cl .ne. 7) correct=.false.
!!!XXX     if (cu .ne. 11) correct=.false.
!!!XXX     if (cc .ne. 5) correct=.false.
!!!XXX
!!!XXX!write(*,*) "1", correct, el, eu, ec
!!!XXX     ! Check non-key bounds
!!!XXX     call  ESMF_LocStreamGetBounds(locstream, localDE=2, &
!!!XXX             exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
!!!XXX             computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
!!!XXX             rc=localrc)
!!!XXX     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX     ! Bounds
!!!XXX     if (el .ne. 12) correct=.false.
!!!XXX     if (eu .ne. 16) correct=.false.
!!!XXX     if (ec .ne. 5) correct=.false.
!!!XXX     if (cl .ne. 12) correct=.false.
!!!XXX     if (cu .ne. 16) correct=.false.
!!!XXX     if (cc .ne. 5) correct=.false.
!!!XXX
!!!XXX!write(*,*) "2", correct, el, eu, ec
!!!XXX     ! Check non-key bounds
!!!XXX     call  ESMF_LocStreamGetBounds(locstream, localDE=3, &
!!!XXX             exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
!!!XXX             computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
!!!XXX             rc=localrc)
!!!XXX     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX     ! Bounds
!!!XXX     if (el .ne. 17) correct=.false.
!!!XXX     if (eu .ne. 20) correct=.false.
!!!XXX     if (ec .ne. 4) correct=.false.
!!!XXX     if (cl .ne. 17) correct=.false.
!!!XXX     if (cu .ne. 20) correct=.false.
!!!XXX     if (cc .ne. 4) correct=.false.
!!!XXX
!!!XXX!write(*,*) "3", correct, el, eu, ec
!!!XXX  else   if (petCount .eq. 4) then
!!!XXX     if (localPet .eq. 0) then
!!!XXX        ! Check non-key bounds
!!!XXX        call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
!!!XXX                exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
!!!XXX                computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
!!!XXX                rc=localrc)
!!!XXX        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX        ! Bounds
!!!XXX        if (el .ne. 2) correct=.false.
!!!XXX        if (eu .ne. 6) correct=.false.
!!!XXX        if (ec .ne. 5) correct=.false.
!!!XXX        if (cl .ne. 2) correct=.false.
!!!XXX        if (cu .ne. 6) correct=.false.
!!!XXX        if (cc .ne. 5) correct=.false.
!!!XXX     else if (localPet .eq. 1) then
!!!XXX        ! Check non-key bounds
!!!XXX        call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
!!!XXX                exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
!!!XXX                computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
!!!XXX                rc=localrc)
!!!XXX        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX        ! Bounds
!!!XXX        if (el .ne. 7) correct=.false.
!!!XXX        if (eu .ne. 11) correct=.false.
!!!XXX        if (ec .ne. 5) correct=.false.
!!!XXX        if (cl .ne. 7) correct=.false.
!!!XXX        if (cu .ne. 11) correct=.false.
!!!XXX        if (cc .ne. 5) correct=.false.
!!!XXX
!!!XXX     else if (localPet .eq. 2) then
!!!XXX        ! Check non-key bounds
!!!XXX        call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
!!!XXX               exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
!!!XXX               computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
!!!XXX               rc=localrc)
!!!XXX        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX        ! Bounds
!!!XXX        if (el .ne. 12) correct=.false.
!!!XXX        if (eu .ne. 16) correct=.false.
!!!XXX        if (ec .ne. 5) correct=.false.
!!!XXX        if (cl .ne. 12) correct=.false.
!!!XXX        if (cu .ne. 16) correct=.false.
!!!XXX        if (cc .ne. 5) correct=.false.
!!!XXX
!!!XXX     else if (localPet .eq. 3) then
!!!XXX        ! Check non-key bounds
!!!XXX        call  ESMF_LocStreamGetBounds(locstream, localDE=0, &
!!!XXX               exclusiveLBound=el, exclusiveUBound=eu, exclusiveCount=ec, & 
!!!XXX               computationalLBound=cl, computationalUBound=cu, computationalCount=cc, & 
!!!XXX               rc=localrc)
!!!XXX        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX        ! Bounds
!!!XXX        if (el .ne. 17) correct=.false.
!!!XXX        if (eu .ne. 20) correct=.false.
!!!XXX        if (ec .ne. 4) correct=.false.
!!!XXX        if (cl .ne. 17) correct=.false.
!!!XXX        if (cu .ne. 20) correct=.false.
!!!XXX        if (cc .ne. 4) correct=.false.
!!!XXX    endif
!!!XXX  endif
!!!XXX
!!!XXX  call ESMF_LocStreamDestroy(locstream,rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
!!!XXX  !-----------------------------------------------------------------------------
!!!XXX
!!!XXX
!!!XXX  !-----------------------------------------------------------------------------
!!!XXX  !NEX_UTest
!!!XXX  write(name, *) "Test ESMF_LocStream Create From Background Mesh"
!!!XXX  write(failMsg, *) "Did not return ESMF_SUCCESS"
!!!XXX
!!!XXX  ! initialize check variables
!!!XXX  correct=.true.
!!!XXX  rc=ESMF_SUCCESS
!!!XXX
!!!XXX  !!!!!!!! Create Mesh !!!!!!!!!!!
!!!XXX  ! Create Mesh
!!!XXX  ! Only do this if we have 4 PETs
!!!XXX if (petCount .eq. 4) then
!!!XXX     ! Setup mesh data depending on PET
!!!XXX     if (localPet .eq. 0) then
!!!XXX        ! Fill in node data
!!!XXX        numNodes=4
!!!XXX
!!!XXX       !! node ids
!!!XXX       allocate(nodeIds(numNodes))
!!!XXX       nodeIds=(/1,2,4,5/) 
!!!XXX
!!!XXX       !! node Coords
!!!XXX       allocate(nodeCoords(numNodes*2))
!!!XXX       nodeCoords=(/0.0,0.0, &
!!!XXX                    1.0,0.0, &
!!!XXX                    0.0,1.0, &
!!!XXX                    1.0,1.0/)
!!!XXX
!!!XXX       !! node owners
!!!XXX       allocate(nodeOwners(numNodes))
!!!XXX       nodeOwners=(/0,0,0,0/) ! everything on proc 0
!!!XXX
!!!XXX       ! Fill in elem data
!!!XXX       numElems=1
!!!XXX
!!!XXX       !! elem ids
!!!XXX       allocate(elemIds(numElems))
!!!XXX       elemIds=(/1/) 
!!!XXX
!!!XXX       !! elem type
!!!XXX       allocate(elemTypes(numElems))
!!!XXX       elemTypes=ESMF_MESHELEMTYPE_QUAD
!!!XXX
!!!XXX       !! elem conn
!!!XXX       allocate(elemConn(numElems*4))
!!!XXX       elemConn=(/1,2,4,3/)
!!!XXX     else if (localPet .eq. 1) then
!!!XXX        ! Fill in node data
!!!XXX        numNodes=4
!!!XXX
!!!XXX       !! node ids
!!!XXX       allocate(nodeIds(numNodes))
!!!XXX       nodeIds=(/2,3,5,6/) 
!!!XXX
!!!XXX       !! node Coords
!!!XXX       allocate(nodeCoords(numNodes*2))
!!!XXX       nodeCoords=(/1.0,0.0, &
!!!XXX                    2.0,0.0, &
!!!XXX                    1.0,1.0, &
!!!XXX                    2.0,1.0/)
!!!XXX
!!!XXX       !! node owners
!!!XXX       allocate(nodeOwners(numNodes))
!!!XXX       nodeOwners=(/0,1,0,1/) 
!!!XXX
!!!XXX       ! Fill in elem data
!!!XXX       numElems=1
!!!XXX
!!!XXX       !! elem ids
!!!XXX       allocate(elemIds(numElems))
!!!XXX       elemIds=(/2/) 
!!!XXX
!!!XXX       !! elem type
!!!XXX       allocate(elemTypes(numElems))
!!!XXX       elemTypes=ESMF_MESHELEMTYPE_QUAD
!!!XXX
!!!XXX       !! elem conn
!!!XXX       allocate(elemConn(numElems*4))
!!!XXX       elemConn=(/1,2,4,3/)
!!!XXX     else if (localPet .eq. 2) then
!!!XXX        ! Fill in node data
!!!XXX        numNodes=4
!!!XXX
!!!XXX       !! node ids
!!!XXX       allocate(nodeIds(numNodes))
!!!XXX       nodeIds=(/4,5,7,8/) 
!!!XXX
!!!XXX       !! node Coords
!!!XXX       allocate(nodeCoords(numNodes*2))
!!!XXX       nodeCoords=(/0.0,1.0, &
!!!XXX                    1.0,1.0, &
!!!XXX                    0.0,2.0, &
!!!XXX                    1.0,2.0/)
!!!XXX
!!!XXX       !! node owners
!!!XXX       allocate(nodeOwners(numNodes))
!!!XXX       nodeOwners=(/0,0,2,2/) 
!!!XXX
!!!XXX       ! Fill in elem data
!!!XXX       numElems=1
!!!XXX
!!!XXX       !! elem ids
!!!XXX       allocate(elemIds(numElems))
!!!XXX       elemIds=(/3/) 
!!!XXX
!!!XXX       !! elem type
!!!XXX       allocate(elemTypes(numElems))
!!!XXX       elemTypes=ESMF_MESHELEMTYPE_QUAD
!!!XXX
!!!XXX       !! elem conn
!!!XXX       allocate(elemConn(numElems*4))
!!!XXX       elemConn=(/1,2,4,3/)  
!!!XXX     else 
!!!XXX        ! Fill in node data
!!!XXX        numNodes=4
!!!XXX
!!!XXX       !! node ids
!!!XXX       allocate(nodeIds(numNodes))
!!!XXX       nodeIds=(/5,6,8,9/) 
!!!XXX
!!!XXX       !! node Coords
!!!XXX       allocate(nodeCoords(numNodes*2))
!!!XXX       nodeCoords=(/1.0,1.0, &
!!!XXX                    2.0,1.0, &
!!!XXX                    1.0,2.0, &
!!!XXX                    2.0,2.0/)
!!!XXX
!!!XXX       !! node owners
!!!XXX       allocate(nodeOwners(numNodes))
!!!XXX       nodeOwners=(/0,1,2,3/) 
!!!XXX
!!!XXX       ! Fill in elem data
!!!XXX       numElems=1
!!!XXX
!!!XXX       !! elem ids
!!!XXX       allocate(elemIds(numElems))
!!!XXX       elemIds=(/4/) 
!!!XXX
!!!XXX       !! elem type
!!!XXX       allocate(elemTypes(numElems))
!!!XXX       elemTypes=ESMF_MESHELEMTYPE_QUAD
!!!XXX
!!!XXX       !! elem conn
!!!XXX       allocate(elemConn(numElems*4))
!!!XXX       elemConn=(/1,2,4,3/)  
!!!XXX     endif
!!!XXX
!!!XXX  ! Create Mesh structure in 1 step
!!!XXX  mesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
!!!XXX         nodeIds=nodeIds, nodeCoords=nodeCoords, &
!!!XXX         coordSys=ESMF_COORDSYS_SPH_DEG, &
!!!XXX         nodeOwners=nodeOwners, elementIds=elemIds,&
!!!XXX         elementTypes=elemTypes, elementConn=elemConn, &
!!!XXX         rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!!!XXX
!!!XXX
!!!XXX  ! deallocate node data
!!!XXX  deallocate(nodeIds)
!!!XXX  deallocate(nodeCoords)
!!!XXX  deallocate(nodeOwners)
!!!XXX
!!!XXX  ! deallocate elem data
!!!XXX  deallocate(elemIds)
!!!XXX  deallocate(elemTypes)
!!!XXX  deallocate(elemConn)
!!!XXX
!!!XXX
!!!XXX  !!!!!!!! Create LocStream !!!!!!!!!!!
!!!XXX  ! Set number of points
!!!XXX  if (localPet .eq. 0) then  
!!!XXX      pntCount=2
!!!XXX  else if (localPet .eq. 1) then  
!!!XXX      pntCount=3
!!!XXX  else if (localPet .eq. 2) then  
!!!XXX      pntCount=1
!!!XXX  else if (localPet .eq. 3) then  
!!!XXX      pntCount=1
!!!XXX  endif
!!!XXX
!!!XXX
!!!XXX  ! Create LocStream
!!!XXX  locstream=ESMF_LocStreamCreate(localCount=pntCount,  rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) then
!!!XXX    rc=ESMF_FAILURE
!!!XXX    goto 100
!!!XXX  endif
!!!XXX     
!!!XXX  ! Allocate X array
!!!XXX  allocate(X(pntCount))
!!!XXX
!!!XXX  ! allocate Y array
!!!XXX  allocate(Y(pntCount))
!!!XXX
!!!XXX
!!!XXX ! Fill in points
!!!XXX  if (localPet .eq. 0) then  
!!!XXX      X(1)=1.0
!!!XXX      Y(1)=1.0
!!!XXX      X(2)=0.5
!!!XXX      Y(2)=1.5
!!!XXX  else if (localPet .eq. 1) then  
!!!XXX      X(1)=1.5
!!!XXX      Y(1)=0.5
!!!XXX      X(2)=1.5
!!!XXX      Y(2)=1.5
!!!XXX      X(3)=1.9
!!!XXX      Y(3)=1.75
!!!XXX  else if (localPet .eq. 2) then  
!!!XXX      X(1)=0.5
!!!XXX      Y(1)=0.5
!!!XXX  else if (localPet .eq. 3) then  
!!!XXX      X(1)=1.9
!!!XXX      Y(1)=0.1
!!!XXX  endif
!!!XXX
!!!XXX
!!!XXX  ! Add key X
!!!XXX  call ESMF_LocStreamAddKey(locstream, keyName="ESMF:Lon", farray=X,  rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) then
!!!XXX    rc=ESMF_FAILURE
!!!XXX    goto 100
!!!XXX  endif
!!!XXX
!!!XXX  ! Add key Y
!!!XXX  call ESMF_LocStreamAddKey(locstream, keyName="ESMF:Lat", farray=Y,  rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) then
!!!XXX    rc=ESMF_FAILURE
!!!XXX    goto 100
!!!XXX  endif
!!!XXX
!!!XXX  ! Do locStream create from background mesh
!!!XXX  newLocstream=ESMF_LocStreamCreate(locstream, &
!!!XXX                 background=mesh, rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) then
!!!XXX     rc=ESMF_FAILURE
!!!XXX     goto 100
!!!XXX  endif
!!!XXX
!!!XXX  call ESMF_LocStreamDestroy(locstream,rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) then
!!!XXX    rc=ESMF_FAILURE
!!!XXX    goto 100
!!!XXX  endif
!!!XXX
!!!XXX  ! deallocate array
!!!XXX  deallocate(X)
!!!XXX  deallocate(Y)
!!!XXX
!!!XXX
!!!XXX  ! Get rid of Mesh
!!!XXX  call ESMF_MeshDestroy(mesh, rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) then
!!!XXX    rc=ESMF_FAILURE
!!!XXX    goto 100
!!!XXX  endif
!!!XXX
!!!XXX  !!!!!!!!! Check results !!!!!!!!!!!!!!!!!
!!!XXX  call ESMF_LocStreamGetKey(newLocStream,keyName="ESMF:Lon", &
!!!XXX         farray=tstX, &
!!!XXX         exclusiveLBound=el, exclusiveUBound=eu, rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) then
!!!XXX    rc=ESMF_FAILURE
!!!XXX    goto 100
!!!XXX  endif
!!!XXX
!!!XXX  call ESMF_LocStreamGetKey(newLocStream,keyName="ESMF:Lat", &
!!!XXX         farray=tstY, rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) then
!!!XXX    rc=ESMF_FAILURE
!!!XXX    goto 100
!!!XXX  endif
!!!XXX
!!!XXX  ! Test points
!!!XXX  if (localPet .eq. 0) then  
!!!XXX      do i=el,eu
!!!XXX         if ((tstX(i) <  0.0) .or. tstX(i) >  1.0) correct=.false.
!!!XXX         if ((tstY(i) <  0.0) .or. tstY(i) >  1.0) correct=.false.
!!!XXX      enddo
!!!XXX  else if (localPet .eq. 1) then  
!!!XXX      do i=el,eu
!!!XXX         if ((tstX(i) <= 1.0) .or. tstX(i) > 2.0) correct=.false.
!!!XXX         if ((tstY(i) <  0.0) .or. tstY(i) > 1.0) correct=.false.
!!!XXX      enddo
!!!XXX  else if (localPet .eq. 2) then  
!!!XXX      do i=el,eu
!!!XXX         if ((tstX(i) <  0.0) .or. tstX(i) >  1.0) correct=.false.
!!!XXX         if ((tstY(i) <= 1.0) .or. tstY(i) >  2.0) correct=.false.
!!!XXX      enddo 
!!!XXX  else if (localPet .eq. 3) then  
!!!XXX      do i=el,eu
!!!XXX         if ((tstX(i) <= 1.0) .or. tstX(i) >  2.0) correct=.false.
!!!XXX         if ((tstY(i) <= 1.0) .or. tstY(i) >  2.0) correct=.false.
!!!XXX      enddo 
!!!XXX  endif  
!!!XXX
!!!XXX
!!!XXX  call ESMF_LocStreamDestroy(newLocstream,rc=localrc)
!!!XXX  if (localrc .ne. ESMF_SUCCESS) then
!!!XXX    rc=ESMF_FAILURE
!!!XXX    goto 100
!!!XXX  endif
!!!XXX
!!!XXX  ! endif for skip for ==4 proc
!!!XXX  endif 
!!!XXX
!!!XXX100 continue
!!!XXX  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
!!!XXX  !-----------------------------------------------------------------------------
!!!XXX
!!!XXX
!!!XXX  !------------------------------------------------------------------------
!!!XXX  !NEX_UTest
!!!XXX  write(name, *) "Test ESMF_LocStream Create From Background Grid"
!!!XXX  write(failMsg, *) "Test unsuccessful"
!!!XXX 
!!!XXX  ! initialize 
!!!XXX  rc=ESMF_SUCCESS
!!!XXX      
!!!XXX  ! do test
!!!XXX  call test_locstreambkg(rc)
!!!XXX 
!!!XXX  ! return result
!!!XXX  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!!!XXX !-----------------------------------------------------------------------------
!!!XXX
!!!XXX  !------------------------------------------------------------------------
!!!XXX  !NEX_UTest
!!!XXX  write(name, *) "Test ESMF_LocStream Create From Background Grid non-default Align"
!!!XXX  write(failMsg, *) "Test unsuccessful"
!!!XXX 
!!!XXX  ! initialize 
!!!XXX  rc=ESMF_SUCCESS
!!!XXX      
!!!XXX  ! do test
!!!XXX  call test_locstreambkgnda(rc)
!!!XXX
!!!XXX  ! return result
!!!XXX  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!!!XXX !-----------------------------------------------------------------------------
!!!XXX
!!!XXX
!!!XXX  !------------------------------------------------------------------------
!!!XXX  !NEX_UTest
!!!XXX  write(name, *) "Test ESMF_LocStream Create From Background Grid on Sphere"
!!!XXX  write(failMsg, *) "Test unsuccessful"
!!!XXX 
!!!XXX  ! initialize 
!!!XXX  rc=ESMF_SUCCESS
!!!XXX      
!!!XXX  ! do test
!!!XXX  call test_locstreambkgSph(rc)
!!!XXX
!!!XXX  ! return result
!!!XXX  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!!!XXX !-----------------------------------------------------------------------------
!!!XXX
!!!XXX
!!!XXX  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test ESMF_LocStream Create From Shapefile"
  write(failMsg, *) "Test unsuccessful"
  
  ! initialize 
  rc=ESMF_SUCCESS
  
  ! do test
  call test_locstreamshapefile(rc)
  
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


 subroutine test_locstreamshapefile(rc)
   integer, intent(out)  :: rc

!   character(ESMF_MAXSTR) :: filename = "/home/ilcentro/Work/NASA/ALI/data/infrastructure/subset.shp"
!   character(ESMF_MAXSTR) :: filename = "/home/ilcentro/Work/NASA/ALI/data/infrastructure/tl_2019_us_rails.shp"
!   character(ESMF_MAXSTR) :: filename = "/home/ilcentro/Work/NASA/ALI/data/Manufacturing/General_Manufacturing_Facilities_NAD83.shp"
   character(ESMF_MAXSTR) :: filename = "/home/ilcentro/Work/NASA/ALI/data/utilities/PowerPlants_US_EIA_NAD83.shp"
   type(ESMF_LocStream) :: locstream, locstream2
   type(ESMF_Field)     :: field, field2
!   integer(ESMF_KIND_I4), pointer :: mptr(:)
   real(ESMF_KIND_R4), pointer :: mptr(:), mptr2(:)
   type(ESMF_ArraySpec)   :: arraySpec
   type(ESMF_Mesh)        :: Mesh
   type(ESMF_Grid)        :: Grid
   character(len=*), parameter :: shapefileName = "data/test3_simple.shp"
   integer, allocatable   :: decomptile(:,:)
   
   locstream = ESMF_LocStreamCreate(filename=trim(filename),fileformat=ESMF_FILEFORMAT_SHAPEFILE, name='Coal_MW',rc=rc)
   call ESMF_ArraySpecSet(arraySpec, 1, typekind=ESMF_TYPEKIND_R4, rc=rc)

   field  = ESMF_FieldCreate(locstream, arrayspec, name='Coal_MW', rc=rc)
   field2 = ESMF_FieldCreate(locstream, arrayspec, name='Coal_EMIS', rc=rc)
   call ESMF_FieldRead( Field, &
                        fileName=fileName, &
                        iofmt=ESMF_IOFMT_SHP, &
                        rc=rc)
   
   call ESMF_FieldGet( field,  farrayPtr=mptr,  rc=rc)
   call ESMF_FieldGet( field2, farrayPtr=mptr2, rc=rc)
   mptr2 = mptr*2180.
   mptr  => null()
   mptr2 => null()

   call ESMF_FieldWrite(field2, fileName="PowerPlants_US_EIA_NAD83.shp",  &
       iofmt=ESMF_IOFMT_SHP,  &
       overwrite=.true.,  &
       status=ESMF_FILESTATUS_UNKNOWN, rc=rc)

!   call ESMF_FieldWrite(field, fileName="locstream.nc",  &
!!       iofmt=ESMF_IOFMT_SHP,  &
!       overwrite=.true.,  &
!       status=ESMF_FILESTATUS_UNKNOWN, rc=rc)

!!>>   mesh=ESMF_MeshCreate( shapefileName, &
!!>>                         fileformat=ESMF_FILEFORMAT_SHAPEFILE, &
!!>>                         coordSys=ESMF_COORDSYS_SPH_RAD, &
!!>>                         rc=rc)
!!>>   write(failMsg, *) "ESMF_MeshCreate did not return ESMF_SUCCESS"
!!>>   call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!!>>
!!>>   allocate(decomptile(2,6))
!!>>   decomptile(:,1)=(/2,2/) ! Tile 1
!!>>   decomptile(:,2)=(/2,2/) ! Tile 2
!!>>   decomptile(:,3)=(/1,2/) ! Tile 3
!!>>   decomptile(:,4)=(/1,2/) ! Tile 4
!!>>   decomptile(:,5)=(/1,2/) ! Tile 5
!!>>   decomptile(:,6)=(/1,2/) ! Tile 6
!!>>   ! Create cubed sphere grid
!!>>   write(name, *) "Creating a cubed-sphere grid"
!!>>   Grid = ESMF_GridCreateCubedSphere(               &
!!>>        tileSize=180, &
!!>>        regDecompPTile=decomptile, &
!!>>        staggerLocList=(/ESMF_STAGGERLOC_CORNER, ESMF_STAGGERLOC_CENTER/), &
!!>>        coordSys=ESMF_COORDSYS_SPH_RAD, &
!!>>        rc=rc)
!!>>   if (allocated(decomptile)) deallocate(decomptile)
!!>>
!!>>   write(failMsg, *) "ESMF_GridCreateCubedSphere did not return ESMF_SUCCESS"
!!>>   call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!!>>
!!>>   write(name, *) "Redistributing LocStream to grid"
!!>>   locstream2 = ESMF_LocStreamCreate(locstream, background=grid, rc=rc)
!!>>   write(failMsg, *) "Locstream redist did not return ESMF_SUCCESS"
!!>>   call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

   return

 end subroutine test_locstreamshapefile
 

end program ESMF_LocStreamCreateUTest
