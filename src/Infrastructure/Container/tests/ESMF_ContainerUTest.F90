! $Id: ESMF_ContainerUTest.F90,v 1.3 2011/04/18 18:25:59 theurich Exp $
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
!
program ESMF_ContainerUTest

!------------------------------------------------------------------------------
 
#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_ContainerUTest - This unit test file tests ESMF_Container
!  methods
!
! !DESCRIPTION:
!
! The ESMF_Container class is an internally used utility class
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_Mod
  use ESMF_TestMod      ! test methods
  use ESMF_ContainerMod ! internal class to be tested

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id: ESMF_ContainerUTest.F90,v 1.3 2011/04/18 18:25:59 theurich Exp $'
!------------------------------------------------------------------------------

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: rc

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  !LOCAL VARIABLES:
  type(ESMF_Container)            :: container
  type(ESMF_Field), allocatable   :: fieldList(:)
  type(ESMF_Field)                :: field
  character(ESMF_MAXSTR)          :: iString
  character(ESMF_MAXSTR)          :: fieldName
  integer, parameter              :: fieldCount = 5
  integer                         :: i
  
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
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Container Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  container = ESMF_ContainerCreate(rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !- prepare fieldList
  allocate(fieldList(fieldCount))
  do i=1, fieldCount
    write(iString, *) i
    fieldList(i) = ESMF_FieldCreateEmpty(name="testField"//&
      trim(adjustl(iString)), rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  enddo

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Container Add Field Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerAdd(container, fieldList=fieldList, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Container Print Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerPrint(container, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Container Get item Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerGet(container, fieldName="testField3", field=field, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !- get name out of queried Field object in preparation of verify test
  call ESMF_FieldGet(field, name=fieldName, rc=rc)
  if (rc/=ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Verify Container Get item Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_Test((trim(fieldName)=="testField3"), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Container Destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerDestroy(container, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !- fieldList garbage collection
  do i=1, fieldCount
    call ESMF_FieldDestroy(fieldList(i), rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  enddo
  deallocate(fieldList)

10 continue
  !------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------

end program ESMF_ContainerUTest
