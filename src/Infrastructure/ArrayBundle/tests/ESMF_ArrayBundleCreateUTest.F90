! $Id: ESMF_ArrayBundleCreateUTest.F90,v 1.27 2012/05/16 22:19:18 svasquez Exp $
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
!
program ESMF_ArrayBundleCreateUTest

!------------------------------------------------------------------------------
 
#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_ArrayBundleCreateUTest 
! !DESCRIPTION:
!
! The code in this file drives F90 ArrayBundleCreate() unit tests.
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id: ESMF_ArrayBundleCreateUTest.F90,v 1.27 2012/05/16 22:19:18 svasquez Exp $'
!------------------------------------------------------------------------------

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: rc

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  !LOCAL VARIABLES:
  type(ESMF_VM):: vm
  integer:: petCount, localPet
  type(ESMF_ArraySpec):: arrayspec
  type(ESMF_DistGrid):: distgrid
  type(ESMF_Array):: array(10), arrayOut(5), arraySingle
  type(ESMF_Array), pointer :: arrays(:)
  type(ESMF_Array), allocatable :: arrayList(:)
  character(len=ESMF_MAXSTR):: arrayNameList(5)
  integer:: arrayCount, i
  type(ESMF_ArrayBundle):: arraybundle, arraybundleAlias
  character (len=80)      :: arrayName
  logical:: arraybundleBool, isPresent, loopResult
  
  character, allocatable :: buffer(:)
  integer :: buff_len, offset
  integer :: alloc_err
  type(ESMF_AttReconcileFlag) :: attreconflag
  type(ESMF_InquireFlag) :: inquireflag

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
  
  !------------------------------------------------------------------------
  ! preparations
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  !------------------------------------------------------------------------
  ! this unit test requires to be run on exactly 4 PETs
  if (petCount /= 4) goto 10
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "empty ArrayBundleCreate Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  arraybundle = ESMF_ArrayBundleCreate(name="MyEmptyArrayBundle", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundlePrint Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayBundlePrint(arraybundle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayBundleDestroy(arraybundle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  ! preparations
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/15,23/), &
    regDecomp=(/2,2/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate Allocate 2D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array(1) = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    indexflag=ESMF_INDEX_GLOBAL, name="MyArray", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate Allocate 2D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array(2) = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    indexflag=ESMF_INDEX_GLOBAL, name="MyArray2", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleCreate Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  arraybundle = ESMF_ArrayBundleCreate(arrayList=array(1:2), &
    name="MyArrayBundle", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundle equality before assignment Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  arraybundleBool = (arraybundleAlias.eq.arraybundle)
  call ESMF_Test(.not.arraybundleBool, name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Testing ESMF_ArrayBundleAssignment(=)()
  write(name, *) "ArrayBundle assignment and equality Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  arraybundleAlias = arraybundle
  arraybundleBool = (arraybundleAlias.eq.arraybundle)
  call ESMF_Test(arraybundleBool, name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayBundleDestroy(arraybundle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Testing ESMF_ArrayBundleOperator(==)()
  write(name, *) "ArrayBundle equality after destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  arraybundleBool = (arraybundleAlias==arraybundle)
  call ESMF_Test(.not.arraybundleBool, name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Testing ESMF_ArrayBundleOperator(/=)()
  write(name, *) "ArrayBundle non-equality after destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  arraybundleBool = (arraybundleAlias/=arraybundle)
  call ESMF_Test(arraybundleBool, name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Double ArrayBundleDestroy through alias Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayBundleDestroy(arraybundleAlias, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleCreate Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  arraybundle = ESMF_ArrayBundleCreate(arrayList=array(1:2), &
    name="MyArrayBundle", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundlePrint Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayBundlePrint(arraybundle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleGet with arrayList Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayBundleGet(arraybundle, arrayCount=arrayCount, &
    arrayList=arrayOut, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  print *,"arrayCount=", arrayCount

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleGet with arrayNameList Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayBundleGet(arraybundle, arrayCount=arrayCount, &
    arrayNameList=arrayNameList, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  do i=1, arrayCount
    print *,"arrayNameList(",i,")=", trim(arrayNameList(i))
  enddo

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleAdd with existing Array Test"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  call ESMF_ArrayBundleAdd(arraybundle, arrayList=(/array(1)/), rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleAdd with existing Array but relaxed Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayBundleAdd(arraybundle, arrayList=(/array(1)/), &
    relaxedflag=.true., rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleAdd with zero size arrayList Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(arrays(0))
  call ESMF_ArrayBundleAdd(arraybundle, arrayList=arrays, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  if (associated(arrays)) deallocate(arrays)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleGet with arrayName not exist Test"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  call ESMF_ArrayBundleGet(arraybundle, arrayName="MyArray3", &
    array=arraySingle, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleGet with arrayName not exist isPresent Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayBundleGet(arraybundle, arrayName="MyArray3", &
    isPresent=isPresent, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Validate isPresent flag"
  call ESMF_Test((isPresent.eqv. .false.), name, failMsg, result, ESMF_SRCLINE)

  allocate(arrays(1))
  arrays(1) = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    indexflag=ESMF_INDEX_DELOCAL, name="MyArray3", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleAdd with arrayList size 1 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"  
  call ESMF_ArrayBundleAdd(arraybundle, arrayList=arrays, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  if (associated(arrays)) deallocate(arrays)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleGet with arrayName Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayBundleGet(arraybundle, arrayName="MyArray3", &
    array=arraySingle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  call ESMF_ArrayPrint(arraySingle, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleRemove with arrayNameList size 1 exising Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"  
  call ESMF_ArrayBundleRemove(arraybundle, arrayNameList=(/"MyArray"/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleRemove with arrayNameList size 1 not-existing Test"
  write(failMsg, *) "Did return ESMF_SUCCESS"  
  call ESMF_ArrayBundleRemove(arraybundle, arrayNameList=(/"MyArray"/), rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleRemove with arrayNameList size 1 not-existing but relaxed Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"  
  call ESMF_ArrayBundleRemove(arraybundle, arrayNameList=(/"MyArray"/), &
    relaxedflag=.true., rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  allocate(arrays(3))
  arrays(1) = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    indexflag=ESMF_INDEX_DELOCAL, name="MyArray4", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  arrays(2) = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    indexflag=ESMF_INDEX_DELOCAL, name="MyArray6", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  arrays(3) = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    indexflag=ESMF_INDEX_DELOCAL, name="MyArray5", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleAdd with arrayList size 3 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"  
  call ESMF_ArrayBundleAdd(arraybundle, arrayList=arrays, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleRemove with arrayNameList size 2 existing Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"  
  call ESMF_ArrayBundleRemove(arraybundle, arrayNameList=(/"MyArray4", &
    "MyArray5"/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleReplace with arrayList 1exist+2notexist relaxed Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"  
  call ESMF_ArrayBundleReplace(arraybundle, arrayList=arrays, &
    relaxedflag=.true., rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleReplace with arrayList 1exist+2notexist strict Test"
  write(failMsg, *) "Did return ESMF_SUCCESS"  
  call ESMF_ArrayBundleReplace(arraybundle, arrayList=arrays, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleAddReplace with arrayList 1exist+2notexist Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"  
  call ESMF_ArrayBundleAddReplace(arraybundle, arrayList=arrays, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleRemove with arrayNameList size 2 existing Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"  
  call ESMF_ArrayBundleRemove(arraybundle, arrayNameList=(/"MyArray3", &
    "MyArray5"/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleAdd with multiflag #1 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"  
  call ESMF_ArrayBundleAdd(arraybundle, arrayList=arrays, multiflag=.true., &
    rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleAdd with multiflag #2 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"  
  call ESMF_ArrayBundleAdd(arraybundle, arrayList=arrays, multiflag=.true., &
    rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  call ESMF_ArrayBundlePrint(arraybundle, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleGet with arrayName to get count Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayBundleGet(arraybundle, arrayName="MyArray6", &
    arrayCount=arrayCount, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  allocate(arrayList(arrayCount))
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleGet arrayList for arrayName Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayBundleGet(arraybundle, arrayName="MyArray6", &
    arrayList=arrayList, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Verify Container Get arrayList for arrayname Test"
  write(failMsg, *) "arrayList contains incorrect Arrays"
  loopResult = .false. ! initialize
  if (allocated(arrayList)) then
    loopResult = .true. ! initialize
    do i=1, arrayCount
      call ESMF_ArrayGet(arrayList(i), name=arrayName, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      print *, "name of arrayList(",i,") is: ", arrayName
      if (trim(arrayName)/="MyArray6") loopResult = .false.
    enddo
  endif
  call ESMF_Test(loopResult, name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !------------------------------------------------------------------------
  !------------------------------------------------------------------------

  ! BEGIN tests of INTERNAL serialization methods.  They are subject
  ! to change and are NOT part of the ESMF user API.

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! test the serialize inquire-only option
  ! WARNING: This is testing an INTERNAL method.  It is NOT
  ! part of the supported ESMF user API!
  write(name, *) "Computing space for serialization buffer"
  write(failMsg, *) "Size could not be determined"
  buff_len = 1
  allocate (buffer(buff_len))
  offset = 0
  attreconflag = ESMF_ATTRECONCILE_OFF
  inquireflag  = ESMF_INQUIREONLY
  call c_esmc_arraybundleserialize (arraybundle, buffer, buff_len, offset,  &
      attreconflag, inquireflag, rc)
  print *, 'computed serialization buffer length =', offset, ' bytes'
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  deallocate (buffer)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Allocate serialization buffer"
  write(failMsg, *) "Size was illegal"
  buff_len = offset
  allocate (buffer(buff_len), stat=alloc_err)
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, alloc_err == 0)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! test actually doing the serialization
  ! WARNING: This is testing an INTERNAL method.  It is NOT
  ! part of the supported ESMF user API!
  write(name, *) "Serialization Arraybundle data"
  write(failMsg, *) "Serialization failed"
  buff_len = size (buffer)
  offset = 0
  attreconflag = ESMF_ATTRECONCILE_OFF
  inquireflag  = ESMF_NOINQUIRE
  call c_esmc_arraybundleserialize (arraybundle, buffer, buff_len, offset,  &
      attreconflag, inquireflag, rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  ! END tests of INTERNAL serialization methods.  They are subject
  ! to change and are NOT part of the ESMF user API.

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayBundleDestroy(arraybundle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  ! cleanup
  
  do i=1, 2
    call ESMF_ArrayDestroy(array(i), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  enddo
  
  do i=1, 3
    call ESMF_ArrayDestroy(arrays(i), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  enddo
  if (associated(arrays)) deallocate(arrays)

  call ESMF_ArrayDestroy(arraySingle, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
10 continue
  !------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------

end program ESMF_ArrayBundleCreateUTest
