! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_ArrayDataUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_ArrayDataTest - Check Array data storage integrity
!
! !DESCRIPTION:
!
! The code in this file drives Fortran Array data unit tests.
! The companion file ESMF\_Array.F90 contains the definitions for the
! LocalArray methods.
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
  integer :: rc

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  integer :: i, j, petCount
  logical :: looptest, isProxy

  ! Fortran array pointer of 4-byte integers
  integer (ESMF_KIND_I4),dimension(:), pointer :: fdata
  integer (ESMF_KIND_I4),dimension(:), pointer :: fdataSlice
  integer (ESMF_KIND_I4),dimension(:), pointer :: fptr, fptrOut
  integer (ESMF_KIND_I4),dimension(:,:), pointer  :: fptrOutWrongRank
  real (ESMF_KIND_R4),dimension(:), pointer       :: fptrOutWrongTK

  type(ESMF_DistGrid) :: distgrid
  type(ESMF_Array)    :: array, arrayOut, array_new
  type(ESMF_VM)       :: vm

  character, allocatable :: buffer(:)
  integer :: buff_len, offset1, offset2, offset3
  integer :: alloc_err
  type(ESMF_AttReconcileFlag) :: attreconflag
  type(ESMF_InquireFlag) :: inquireflag

  !-----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !-----------------------------------------------------------------------------

  ! get global VM
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! prepare for Fortran allocatable array "data"
  allocate(fdata(-12:-6), stat=rc)
  do i = -12, -6
    fdata(i) = i*1000
  enddo

  ! prepare DistGrid
  distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/7*petCount/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating an Array from allocated Fortran array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(distgrid, fdata, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Determine Array proxy status"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isProxy = ESMF_ArrayIsProxy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Checking that Array is marked as not proxy"
  write(failMsg, *) "Incorrect isProxy status"
  call ESMF_Test((.not.isProxy), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Printing Array created from allocated Fortran array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Obtaining access to data in Array via Fortran array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, farrayPtr=fptr, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying data in Array via Fortran array pointer access"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do i = -12, -6
    j = i + 12 + lbound(fptr, 1)
    print *, fptr(j), fdata(i)
    if (fptr(j) /= fdata(i)) looptest = .false.
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  ! Fill in different values
  do i = -12, -6
    fdata(i) = fdata(i) + 57
  enddo
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Printing Array created from allocated Fortran array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying data in Array via Fortran array pointer access"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do i = -12, -6
    j = i + 12 + lbound(fptr, 1)
    print *, fptr(j), fdata(i)
    if (fptr(j) /= fdata(i)) looptest = .false.
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroying Array created from an allocated Fortran ",&
    "array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating an Array from allocated Fortran array pointer using ESMF_DATACOPY_VALUE"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(distgrid, fdata, datacopyflag=ESMF_DATACOPY_VALUE, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Printing Array created from allocated Fortran array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Obtaining access to data in Array via Fortran array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, farrayPtr=fptr, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying data in Array via Fortran array pointer access"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do i = -12, -6
    j = i + 12 + lbound(fptr, 1)
    print *, fptr(j), fdata(i)
    if (fptr(j) /= fdata(i)) looptest = .false.
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  ! Fill in different values
  do i = -12, -6
    fdata(i) = fdata(i) + 57
  enddo
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Printing Array created from allocated Fortran array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying data in Array via Fortran array pointer access"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do i = -12, -6
    j = i + 12 + lbound(fptr, 1)
    print *, fptr(j), fdata(i)
    if (fptr(j) /= fdata(i)-57) looptest = .false.
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroying Array created from an allocated Fortran ",&
    "array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  ! test with array slice
  fdataSlice => fdata(:)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating an Array from allocated Fortran array pointer slice"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(distgrid, fdataSlice, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Printing Array created from allocated Fortran array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Obtaining access to data in Array via Fortran array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, farrayPtr=fptr, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying data in Array via Fortran array pointer access"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do i = -12, -6
    j = i + 12 + lbound(fptr, 1)
    print *, fptr(j), fdata(i)
    if (fptr(j) /= fdata(i)) looptest = .false.
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  ! Fill in different values
  do i = -12, -6
    fdata(i) = fdata(i) + 57
  enddo
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Printing Array created from allocated Fortran array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying data in Array via Fortran array pointer access"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do i = -12, -6
    j = i + 12 + lbound(fptr, 1)
    print *, fptr(j), fdata(i)
    if (fptr(j) /= fdata(i)) looptest = .false.
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroying Array created from an allocated Fortran ",&
    "array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  ! test with array slice
  deallocate(fdata)
  allocate(fdata(-20:10))
  do i = -20, 10
    fdata(i) = i*1000
  enddo
  fdataSlice => fdata(-12:-6)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating an Array from allocated Fortran array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(distgrid, fdataSlice, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Printing Array created from allocated Fortran array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Obtaining access to data in Array via Fortran array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, farrayPtr=fptr, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying data in Array via Fortran array pointer access"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do i = -12, -6
    j = i + 12 + lbound(fptr, 1)
    print *, fptr(j), fdata(i)
    if (fptr(j) /= fdata(i)) looptest = .false.
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  ! Fill in different values
  do i = -12, -6
    fdata(i) = fdata(i) + 57
  enddo
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Printing Array created from allocated Fortran array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying data in Array via Fortran array pointer access"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do i = -12, -6
    j = i + 12 + lbound(fptr, 1)
    print *, fptr(j), fdata(i)
    if (fptr(j) /= fdata(i)) looptest = .false.
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroying Array created from an allocated Fortran ",&
    "array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating an Array from allocated Fortran array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(distgrid, fdataSlice, datacopyflag=ESMF_DATACOPY_VALUE, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Printing Array created from allocated Fortran array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Obtaining access to data in Array via Fortran array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, farrayPtr=fptr, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying data in Array via Fortran array pointer access"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do i = -12, -6
    j = i + 12 + lbound(fptr, 1)
    print *, fptr(j), fdata(i)
    if (fptr(j) /= fdata(i)) looptest = .false.
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  ! Fill in different values
  do i = -12, -6
    fdata(i) = fdata(i) + 57
  enddo
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Printing Array created from allocated Fortran array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying data in Array via Fortran array pointer access"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do i = -12, -6
    j = i + 12 + lbound(fptr, 1)
    print *, fptr(j), fdata(i)
    if (fptr(j) /= fdata(i)-57) looptest = .false.
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  ! BEGIN tests of certain INTERNAL methods.  They are subject
  ! to change and are NOT part of the ESMF user API.
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  ! test the serialize inquire-only option
  ! WARNING: This is testing an INTERNAL method.  It is NOT
  ! part of the supported ESMF user API!
  write(name, *) "Computing space for serialization buffer"
  write(failMsg, *) "Size could not be determined"
  buff_len = 1
  allocate (buffer(buff_len))
  offset1 = 0
  attreconflag = ESMF_ATTRECONCILE_OFF
  inquireflag  = ESMF_INQUIREONLY
  call c_esmc_arrayserialize (array, buffer, buff_len, offset1,  &
      attreconflag, inquireflag, rc)
  print *, 'computed serialization buffer length =', offset1, ' bytes'
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  deallocate (buffer)
  !-----------------------------------------------------------------------------
 
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Allocate serialization buffer"
  write(failMsg, *) "Size was illegal"
  buff_len = offset1
  allocate (buffer(buff_len), stat=alloc_err)
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, alloc_err == 0)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  !NEX_UTest
  ! test doing the serialization for real
  ! WARNING: This is testing an INTERNAL method.  It is NOT 
  ! part of the supported ESMF user API!
  write(name, *) "Serialization Array data"
  write(failMsg, *) "Serialization failed"
  buff_len = size (buffer)
  offset2 = 0
  attreconflag = ESMF_ATTRECONCILE_OFF
  inquireflag  = ESMF_NOINQUIRE
  call c_esmc_arrayserialize (array, buffer, buff_len, offset2,  &
      attreconflag, inquireflag, rc)
  print *, 'actual serialization buffer length =', offset2, ' bytes'
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
 
  !-----------------------------------------------------------------------------
  !NEX_UTest
  ! compare offset estimate with offset actual
  write(name, *) "Compare serialization offset actual vs estimate"
  write(failMsg, *) "Actual serialization offset > estimate"
  call ESMF_Test(offset1 >= offset2, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  !NEX_UTest
  ! test doing the deserialization
  ! WARNING: This is testing an INTERNAL method.  It is NOT 
  ! part of the supported ESMF user API!
  write(name, *) "Deserialize Array data"
  write(failMsg, *) "Deserialization failed"
  buff_len = size (buffer)
  offset3 = 0
  attreconflag = ESMF_ATTRECONCILE_OFF
  inquireflag  = ESMF_NOINQUIRE
  call c_esmc_arraydeserialize (array_new, buffer, offset3, attreconflag, rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  deallocate (buffer)
  call ESMF_ArraySetInitCreated(array_new, rc=rc) ! set init code for Fortran
  
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Printing Array created by deserialize"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array_new, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Determine Array created by deserialize proxy status"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isProxy = ESMF_ArrayIsProxy(array_new, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Checking that Array created by deserialize is marked as proxy"
  write(failMsg, *) "Incorrect isProxy status"
  call ESMF_Test((isProxy), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroying Array created by deserialize."
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array_new, noGarbage=.true., rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  ! compare serialize/deserialize offsets
  write(name, *) "Compare serialization offset to deserialization offset"
  write(failMsg, *) "deserialization offset /= serialization offset"
  call ESMF_Test(offset3 == offset2, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating an Array that matches the existing Array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  arrayOut = ESMF_ArrayCreate(distgrid, ESMF_TYPEKIND_I4, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Trying to access data with pointer of incorrect rank."
  write(failMsg, *) "Did return ESMF_SUCCESS"
  call ESMF_ArrayGet(arrayOut, farrayPtr=fptrOutWrongRank, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Trying to access data with pointer of incorrect typekind."
  write(failMsg, *) "Did return ESMF_SUCCESS"
  call ESMF_ArrayGet(arrayOut, farrayPtr=fptrOutWrongTK, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Obtaining access to data in arrayOut via Fortran array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(arrayOut, farrayPtr=fptrOut, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  ! initialize data in arrayOut to something obvious
  fptrOut = -999

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Copy data from one Array to another"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayCopy(arrayOut=arrayOut, arrayIn=array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying data match between array and arrayOut"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do i = lbound(fptr,1), ubound(fptr,1)
    print *, fptrOut(i), fptr(i)
    if (fptrOut(i) /= fptr(i)) looptest = .false.
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  
! TODO -> move the internal methods here

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroying Array created from an allocated Fortran ",&
    "array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  ! garbage collection
  deallocate(fdata, stat=rc)

  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

end program ESMF_ArrayDataUTest
