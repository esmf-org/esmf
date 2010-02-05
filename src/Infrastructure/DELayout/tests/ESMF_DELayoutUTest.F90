! $Id: ESMF_DELayoutUTest.F90,v 1.27.2.1 2010/02/05 19:55:07 svasquez Exp $
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
      program ESMF_DELayoutUTest

!------------------------------------------------------------------------------
 
#include "ESMF.h"
#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_DELayoutTest - This unit test file verifies DELayout methods.
!
! !DESCRIPTION:
!
! The code in this file drives F90 DELayout unit tests.
! The companion file ESMF\_DELayout.F90 contains the definitions for the
! DELayout methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod

      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_DELayoutUTest.F90,v 1.27.2.1 2010/02/05 19:55:07 svasquez Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc = 1

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

!     !LOCAL VARIABLES:
      type(ESMF_VM):: vm
      type(ESMF_DELayout):: delayout, delayoutAssigned
      integer:: petCount

#ifdef ESMF_TESTEXHAUSTIVE
      type(ESMF_VM):: vm1
      integer, allocatable:: deGrouping(:)
      integer, allocatable:: list(:)
      integer, allocatable:: petMap(:)
      integer:: i, ndes, n, nsum, isum, rc_loop
      type(ESMF_DELayout):: delayout1, delayout2

      character, pointer :: buffer(:)
      integer :: buff_len, offset
      integer :: alloc_err
      type(ESMF_InquireFlag) :: inquireflag
#endif

!-------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then 
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------- 
      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

      call ESMF_VMGetGlobal(vm, rc)
      call ESMF_VMGet(vm, petCount=petCount, rc=rc)

!-------------------------------------------------------------------------------
! Test the OLDSTYLE DELayout while it is still available...
!-------------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Default OLDSTYLE DELayout Create Test"
      delayout = ESMF_DELayoutCreate(vm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_TESTEXHAUSTIVE
      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "DELayout validate an un-created delayout Test"
      call ESMF_DELayoutValidate(delayout2, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

     !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Calling DELayoutGetDEMatchDE with uncreated DELayout Test"
      call ESMF_DELayoutGetDEMatchDE(delayout=delayout, de=1, &
			delayoutMatch=delayout1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "DELayout Create Test"
      delayout2 = ESMF_DELayoutCreate(vm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "DELayout validate a delayout Test"
      call ESMF_DELayoutValidate(delayout2, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) " DELayout Destroy Test"
      call ESMF_DELayoutDestroy(delayout2, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "DELayout validate an destroyed delayout Test"
      call ESMF_DELayoutValidate(delayout2, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "DELayout Get Test"
      call ESMF_DELayoutGet(delayout, deCount=ndes, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Returned wrong results"
      write(name, *) "Verify deCount against petCount"
      call ESMF_Test((ndes.eq.petCount), name, failMsg, result, ESMF_SRCLINE)
      
      allocate(list(petCount)) ! cannot find more PETs than there are!
      
      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "DELayout Get DEMatchPET Test"
      call ESMF_DELayoutGetDEMatchPET(delayout, 0, vm, n, list, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      nsum = 0
      isum = 0
      rc=ESMF_SUCCESS
      do i=0, ndes-1

        call ESMF_DELayoutGetDEMatchPET(delayout, i, vm, n, list, rc=rc_loop)
        if (rc_loop.ne.ESMF_SUCCESS) rc=rc_loop

        nsum = nsum + n
        isum = isum + (i - list(1))
        
      enddo  

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "DELayout Get DEMatchPET Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      deallocate(list)
      nsum = nsum / ndes

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Returned wrong results"
      write(name, *) "Verify matches"
      call ESMF_Test(((nsum.eq.1).and.(isum.eq.0)), name, failMsg, result, ESMF_SRCLINE)
      

      call ESMF_VMPrint(vm1)
      call ESMF_VMPrint(vm)

#endif

      !------------------------------------------------------------------------
      !NEX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Default OLDSTYLE DELayout Destroy Test"
      call ESMF_DELayoutDestroy(delayout, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
! Test the NEWSTYLE DELayout ...
!-------------------------------------------------------------------------------


      !------------------------------------------------------------------------
      !NEX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Default DELayout Create Test"
      delayout = ESMF_DELayoutCreate(rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!      call ESMF_DELayoutPrint(delayout)

      delayoutAssigned = delayout  ! shallow copy

      !------------------------------------------------------------------------
      !NEX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Default DELayout Destroy Test"
      call ESMF_DELayoutDestroy(delayout, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      write(failMsg, *) "Did return ESMF_SUCCESS"
      write(name, *) "Default DELayout Destroy Twice Test"
      call ESMF_DELayoutDestroy(delayout, rc=rc)
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "DELayout Destroy again through shallow copy Test"
      call ESMF_DELayoutDestroy(delayoutAssigned, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      write(failMsg, *) "Did return ESMF_SUCCESS"
      write(name, *) "DELayout Destroy again through shallow copy Test"
      call ESMF_DELayoutDestroy(delayoutAssigned, rc=rc)
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_TESTEXHAUSTIVE
      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "DELayout with petList Create Test"
      delayout = ESMF_DELayoutCreate(petList=(/0,3,1,2/), rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!      call ESMF_DELayoutPrint(delayout)

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "DELayout with petList Destroy Test"
      call ESMF_DELayoutDestroy(delayout, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "DELayout with deCount Create Test"
      delayout = ESMF_DELayoutCreate(deCount=2*petCount, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!      call ESMF_DELayoutPrint(delayout)

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "DELayout with deCount Destroy Test"
      call ESMF_DELayoutDestroy(delayout, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "DELayout with deCount and deGrouping Create Test"
      allocate(deGrouping(2*petCount))
      do i=1, 2*petCount
        deGrouping(i) = real(petCount) * sin(3.1416*real(i)/real(2*petCount))
      enddo
      print *, "deGrouping: ", deGrouping
      delayout = ESMF_DELayoutCreate(deCount=2*petCount, deGrouping=deGrouping,&
        dePinFlag=ESMF_DE_PIN_VAS, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      deallocate(deGrouping)

!      call ESMF_DELayoutPrint(delayout)

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "DELayoutGetNEW() "
      
      allocate(petMap(2*petCount))
      
      call ESMF_DELayoutGet(delayout, vm=vm1, petMap=petMap, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  
      print *, "petMap: ", petMap
      call ESMF_VMPrint(vm1)
      call ESMF_VMPrint(vm)

      deallocate(petMap)

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "DELayout with deCount and deGrouping Destroy Test"
      call ESMF_DELayoutDestroy(delayout, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Destroy a destroyed DELayout
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Destroy a destroyed DELayout Test"
      call ESMF_DELayoutDestroy(delayout, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Destroy a non-created  DELayout
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Destroy a non-created DELayout Test"
      call ESMF_DELayoutDestroy(delayout1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get from a destroyed DELayout
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "DELayoutGet from destroyed DELayout Test"
      call ESMF_DELayoutGet(delayout, vm=vm1, petMap=petMap, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get from a non-created DELayout
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "DELayoutGet from non-created DELayout Test"
      call ESMF_DELayoutGet(delayout1, vm=vm1, petMap=petMap, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Print a destroyed DELayout
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Print a destroyed DELayout Test"
      call ESMF_DELayoutPrint(delayout, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Print a non-created DELayout
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Print a non-created DELayout Test"
      call ESMF_DELayoutPrint(delayout1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  !EX_UTest
  ! test the serialize inquire-only option
  ! WARNING: This is testing an INTERNAL method.  It is NOT
  ! part of the supported ESMF user API!
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Default DELayout Create Test for serialization"
  delayout = ESMF_DELayoutCreate(rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  !EX_UTest
  write(name, *) "Computing space for serialization buffer"
  write(failMsg, *) "Size could not be determined"
  buff_len = 1
  allocate (buffer(buff_len))
  offset = 0
  inquireflag  = ESMF_INQUIREONLY
  call ESMF_DELayoutSerialize (delayout, buffer, buff_len, offset,  &
      inquireflag, rc)
  print *, 'computed serialization buffer length =', offset, ' bytes'
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  deallocate (buffer)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !EX_UTest
  write(name, *) "Allocate serialization buffer"
  write(failMsg, *) "Size was illegal"
  buff_len = offset
  allocate (buffer(buff_len), stat=alloc_err)
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, alloc_err == 0)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !EX_UTest
  ! test actually doing the serialization
  ! WARNING: This is testing an INTERNAL method.  It is NOT
  ! part of the supported ESMF user API!
  write(name, *) "Serialization DELayout data"
  write(failMsg, *) "Serialization failed"
  buff_len = size (buffer)
  offset = 0
  inquireflag  = ESMF_NOINQUIRE
  call ESMF_DELayoutSerialize (delayout, buffer, buff_len, offset,  &
      inquireflag, rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

#endif
      

10    continue
      call ESMF_TestEnd(result, ESMF_SRCLINE)


      end program ESMF_DELayoutUTest
