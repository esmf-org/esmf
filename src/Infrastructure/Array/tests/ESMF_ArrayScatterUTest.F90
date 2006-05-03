! $Id: ESMF_ArrayScatterUTest.F90,v 1.1 2006/05/03 21:54:33 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!
program ESMF_ArrayScatterUTest

!------------------------------------------------------------------------------
 
#include <ESMF_Macros.inc>

!==============================================================================
!BOP
! !PROGRAM: ESMF_ArrayScatterUTest - This unit test file tests ArrayScatter()
! !DESCRIPTION:
!
! The code in this file drives F90 ArrayScatter unit tests.
! The companion file ESMF\_Array.F90 contains the definitions for the
! Array methods.
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF_Mod

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id: ESMF_ArrayScatterUTest.F90,v 1.1 2006/05/03 21:54:33 theurich Exp $'
!------------------------------------------------------------------------------

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: rc = 1

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  !LOCAL VARIABLES:
  type(ESMF_VM):: vm
  integer:: petCount, localPet, i, j
  type(ESMF_ArraySpec)  :: arrayspec
  type(ESMF_DistGrid)   :: distgrid
  type(ESMF_Array)      :: array
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:)     ! matching F90 array pointer
  real(ESMF_KIND_R8), allocatable :: srcfarray(:,:)

!-------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then 
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------- 

  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  
  ! preparations

  call ESMF_VMGetGlobal(vm, rc)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      
  call ESMF_ArraySpecSet(arrayspec, type=ESMF_DATA_REAL, kind=ESMF_R8, rank=2, &
    rc=rc)
  distgrid = ESMF_DistGridCreate(minCorner=(/1,1/), maxCorner=(/15,23/), &
    regDecomp=(/2,2/), rc=rc)
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    indexflag=ESMF_INDEX_GLOBAL, rc=rc)

!  call ESMF_ArrayPrint(array)

  call ESMF_ArrayGet(array, farrayPtr=farrayPtr, rc=rc)
  
  farrayPtr = real(localPet)  ! initialize each DE-local data chunk of Array
  print *, "farrayPtr:", farrayPtr
  
  ! prepare srcfarray on all PETs -> serves as ref. in comparison after scatter
  allocate(srcfarray(1:15, 1:23))
  do j=1, 23
    do i=1, 15
      srcfarray(i,j) = 123. * sin(real(i)) + 321. * cos(real(j))
    enddo
  enddo
  
  print *, "srcfarray:", srcfarray


  !------------------------------------------------------------------------
  !NEX_UTest
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "ArrayScatter() Test"
  call ESMF_ArrayScatter(array, srcfarray, rootPet=0, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  ! Verify srcfarray data after scatter
  write(failMsg, *) "Source data was modified."
  write(name, *) "Verifying srcfarray data after scatter Test"
  rc = ESMF_SUCCESS
  do j=1, 23
    do i=1, 15
      if (srcfarray(i,j)/=123. * sin(real(i)) + 321. * cos(real(j))) &
        rc = ESMF_FAILURE
    enddo
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Verify Array data after scatter
  write(failMsg, *) "Array data wrong."
  write(name, *) "Verifying Array data after scatter Test"
  rc = ESMF_SUCCESS
  do j=lbound(farrayPtr,2), ubound(farrayPtr,2)
    do i=lbound(farrayPtr,1), ubound(farrayPtr,1)
      if (farrayPtr(i,j) /= srcfarray(i,j)) rc = ESMF_FAILURE
    enddo
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


  
  call ESMF_TestEnd(result, ESMF_SRCLINE) ! calls ESMF_Finalize() internally


end program ESMF_ArrayScatterUTest
