! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

#define ESMF_FILENAME "ESMF_InfoProfileUTest.F90"

#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!==============================================================================
!==============================================================================

program ESMF_InfoProfileUTest

  !============================================================================
  !BOP
  ! !PROGRAM: ESMF_InfoProfileUTest -  Profiles Info code
  !
  ! !DESCRIPTION:
  !
  !----------------------------------------------------------------------------
  ! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF
  use ESMF_InfoMod

  implicit none

  !----------------------------------------------------------------------------
  ! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = '$Id$'
  !----------------------------------------------------------------------------

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name
  ! cumulative result: count failures; no failures equals "all pass"
  integer               :: result = 0

  character(ESMF_MAXSTR) :: key
  integer               :: rc, ii, idx, rsize
  integer, allocatable, dimension(:) :: seed
  real                  :: r
  integer(ESMF_KIND_I4) :: value
  type(ESMF_Info) :: attrs, attrs2
  integer, parameter    :: nkeys = 1000
  integer, parameter    :: ntests = 100000
  logical :: is_present
  type(ESMF_DistGrid) :: distgrid
  type(ESMF_Array) :: array, array2
  character(len=7) :: set_value = "Maximum"
  character(len=ESMF_MAXSTR) :: get_value
  type(ESMF_TypeKind_Flag) :: tk
  integer(ESMF_KIND_I4), dimension(10) :: arr

  !----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !EX_disabled_UTest
  ! Test setting and getting a bunch of attributes.

  rc = ESMF_FAILURE
  write(name, *) "ESMF_Info Profile Loop"
  write(failMsg, *) "Failure during profile loop test"

  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/5,5/), &
    regDecomp=(/2,3/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  array = ESMF_ArrayCreate(distgrid=distgrid, typekind=ESMF_TYPEKIND_I4, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  array2 = ESMF_ArrayCreate(distgrid=distgrid, typekind=ESMF_TYPEKIND_I4, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !----------------------------------------------------------------------------

  call ESMF_InfoGetFromHost(array, attrs, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Set nkeys count of attributes key/value pairs.
  do ii=1, nkeys

    write(key, *) ii

    call ESMF_TraceRegionEnter("Info::Set", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_InfoSet(attrs, "/NUOPC/Instance/"//adjustl(trim(key)), set_value, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TraceRegionExit("Info::Set", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  end do

  !----------------------------------------------------------------------------

  ! For the ntests test count, retrieve the attribute value.
  call random_seed(size=rsize)
  allocate(seed(rsize))
  seed(:) = 0
  call random_seed(put=seed)

  do ii=1, ntests
    call random_number(r)
    idx = ceiling(r*nkeys)

    write(key, *) idx

    call ESMF_TraceRegionEnter("Info::Get", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TraceRegionEnter("Info::Get::InfoGetFromHost", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_InfoGetFromHost(array, attrs, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TraceRegionExit("Info::Get::InfoGetFromHost", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_InfoGet(attrs, "/NUOPC/Instance/"//adjustl(trim(key)), get_value, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TraceRegionExit("Info::Get", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  end do

  !----------------------------------------------------------------------------

!  call ESMF_InfoPrint(attrs)

!  deallocate(seed)

  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !EX_disabled_UTest

  ! Profile checking for attribute presence.

  write(name, *) "ESMF_Info Presence Check Profile Test"
  write(failMsg, *) "Failure during presence profile loop test"

  do ii=1, ntests

    call ESMF_TraceRegionEnter("Info::IsPresent False", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    is_present = ESMF_InfoIsPresent(attrs, "this", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TraceRegionExit("Info::IsPresent False", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  end do

  do ii=1, ntests

    call ESMF_TraceRegionEnter("Info::IsPresent True", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    is_present = ESMF_InfoIsPresent(attrs, "999", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TraceRegionExit("Info::IsPresent True", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  end do

  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------------------
  !EX_disabled_UTest

  ! Profile checking Info inquire capabilities.

  write(name, *) "ESMF_Info Inquire Profile Test"
  write(failMsg, *) "Failure during inquire profile loop test"

  do ii=1, ntests
    call random_number(r)
    idx = ceiling(r*nkeys)

    write(key, *) idx

    call ESMF_TraceRegionEnter("Info::Inquire", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_InfoGet(attrs, key="/NUOPC/Instance/"//adjustl(trim(key)), typekind=tk, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TraceRegionExit("Info::Inquire", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  end do

  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------------------
  !EX_disabled_UTest

  write(name, *) "ESMF_Info Set Array Profile Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  call ESMF_InfoGetFromHost(array2, attrs, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Set nkeys count of attributes key/value pairs.
  do ii=1, nkeys

    write(key, *) ii

    call ESMF_TraceRegionEnter("Info::Set::Array", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_InfoSet(attrs, "/NUOPC/Instance/"//adjustl(trim(key)), arr, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TraceRegionExit("Info::Set::Array", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  end do

  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------------------
  !EX_disabled_UTest

  write(name, *) "ESMF_Info Get Array Profile Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  call ESMF_InfoGetFromHost(array2, attrs, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Set nkeys count of attributes key/value pairs.
  do ii=1, ntests
    call random_number(r)
    idx = ceiling(r*nkeys)

    write(key, *) idx

    call ESMF_TraceRegionEnter("Info::Get::Array", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_InfoGet(attrs, "/NUOPC/Instance/"//adjustl(trim(key)), arr, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TraceRegionExit("Info::Get::Array", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  end do

  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------------------

  call ESMF_ArrayDestroy(array, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  call ESMF_ArrayDestroy(array2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !----------------------------------------------------------------------------

end program ESMF_InfoProfileUTest
