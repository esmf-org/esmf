! $Id: ESMF_ArrayCreateGetUTest.F90,v 1.12.2.12 2009/01/21 21:25:19 cdeluca Exp $
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
program ESMF_ArrayCreateGetUTest

!------------------------------------------------------------------------------
 
#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_ArrayCreateGetUTest - This unit test file tests ArrayCreate()
!   and ArrayGet() methods.
! !DESCRIPTION:
!
! The code in this file drives F90 ArrayCreate(), ArrayGet() unit tests.
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
    '$Id: ESMF_ArrayCreateGetUTest.F90,v 1.12.2.12 2009/01/21 21:25:19 cdeluca Exp $'
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
  type(ESMF_Array):: array, arrayCpy
  type(ESMF_DistGrid):: distgrid, distgrid2
  real(ESMF_KIND_R8)      :: farray1D(10)
  real(ESMF_KIND_R8)      :: farray2D(10,10)
  real(ESMF_KIND_R4)      :: farray3D(10,10,10)
  integer(ESMF_KIND_I4)   :: farray4D(10,10,10,10)
  real(ESMF_KIND_R8), pointer     :: farrayPtr1D(:)
  real(ESMF_KIND_R8), pointer     :: farrayPtr2D(:,:)
  real(ESMF_KIND_R4), pointer     :: farrayPtr3D(:,:,:)
  real(ESMF_KIND_R4), pointer     :: farrayPtr3Dx(:,:,:)
  integer(ESMF_KIND_I4), pointer  :: farrayPtr4D(:,:,:,:)
  character (len=80)      :: arrayName
  integer, allocatable:: totalLWidth(:,:), totalUWidth(:,:)
  integer, allocatable:: totalLBound(:,:), totalUBound(:,:)
  
  integer:: count, i
  integer(ESMF_KIND_I4) :: intattr, intattr2
  integer(ESMF_KIND_I4) :: intattrlist(6)
  real(ESMF_KIND_R8) :: rattr, rattrlist(2)
  character (len=32) :: lattrstr
  type(ESMF_Logical) :: lattr, lattrlist(6)
  character (len=512) :: cattr, cattr2
  
  

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
  ! preparations
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  !------------------------------------------------------------------------
  ! this unit test requires to be run on exactly 4 PETs
  if (petCount /= 4) goto 10
  
  !------------------------------------------------------------------------
  ! preparations
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/15,23/), &
    regDecomp=(/2,2/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate Allocate 2D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    indexflag=ESMF_INDEX_GLOBAL, name="MyArray", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayPrint 2D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet name, 2D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, name=arrayName, rc=rc)
  print *, "Array name: ", arrayname
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet F90 array pointer, 2D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr2D, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Getting Attribute count from an Array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_AttributeGet(array, count, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Verify Attribute count from an Array"
  write(failMsg, *) "Incorrect count"
  call ESMF_Test((count.eq.0), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate from Copy, 2D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  arrayCpy = ESMF_ArrayCreate(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayPrint from Copy after original destroy, 2D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(arrayCpy, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy of Copy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(arrayCpy, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate from Ptr with 3D farray on 2D DistGrid Test as Ptr"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(farrayPtr3D(-2:6,12,3:10))
  array = ESMF_ArrayCreate(farrayPtr=farrayPtr3D, distgrid=distgrid, &
    name="MyArray", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayPrint for ArrayCreate from Ptr Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr3Dx, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Deallocate returned pointer Test"
  write(failMsg, *) "Did not return success"
  deallocate(farrayPtr3Dx, stat=rc)
  call ESMF_Test((rc.eq.0), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate with 3D farray on 2D DistGrid Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(farrayPtr3D(8,12,10))
  array = ESMF_ArrayCreate(farray=farrayPtr3D, distgrid=distgrid, &
    indexflag=ESMF_INDEX_GLOBAL, name="MyArray", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  deallocate(farrayPtr3D)
  nullify(farrayPtr3D)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate with 3D farray on 2D DistGrid Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(farrayPtr3D(12,13,10))
  array = ESMF_ArrayCreate(farray=farrayPtr3D, distgrid=distgrid, &
    distgridToArrayMap=(/2,1/), computationalLWidth=(/0,5/), &
    indexflag=ESMF_INDEX_GLOBAL, name="MyArray", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  deallocate(farrayPtr3D)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate Allocate 2D ESMF_TYPEKIND_R8 w/ negative computational widths Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    indexflag=ESMF_INDEX_GLOBAL, computationalLWidth=(/-1,-1/), &
    computationalUWidth=(/-2,-3/), name="MyArray Negative", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayPrint 2D ESMF_TYPEKIND_R8 w/ computational widths Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate Allocate 2D ESMF_TYPEKIND_R8 w/ computationalEdge widths Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    indexflag=ESMF_INDEX_GLOBAL, computationalEdgeLWidth=(/0,-1/), &
    computationalEdgeUWidth=(/-2,+1/), name="MyArray Negative Edge", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayPrint 2D ESMF_TYPEKIND_R8 w/ computationalEdge widths Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate Allocate 2D ESMF_TYPEKIND_R8 w/ computationalEdge and total widths Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    indexflag=ESMF_INDEX_GLOBAL, computationalEdgeLWidth=(/0,-1/), &
    computationalEdgeUWidth=(/-2,+1/), totalLWidth=(/1,2/), totalUWidth=(/3,4/),&
    name="MyArray Negative Edge", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayPrint 2D ESMF_TYPEKIND_R8 w/ computationalEdge and total widths Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet 2D ESMF_TYPEKIND_R8 w/ computationalEdge and total widths Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(totalLWidth(2,1))
  allocate(totalUWidth(2,1))
  allocate(totalLBound(2,1))
  allocate(totalUBound(2,1))
  call ESMF_ArrayGet(array, totalLWidth=totalLWidth, totalUWidth=totalUWidth, &
    totalLBound=totalLBound, totalUBound=totalUBound, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Check total widths for 2D ESMF_TYPEKIND_R8 w/ computationalEdge and total widths Test"
  write(failMsg, *) "Total widths are wrong"
  call ESMF_Test((totalLWidth(1,1)==1.and.totalLWidth(2,1)==2.and.&
    totalUWidth(1,1)==3.and.totalUWidth(2,1)==4), &
    name, failMsg, result, ESMF_SRCLINE)
  deallocate(totalLWidth, totalUWidth)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Check total bounds for 2D ESMF_TYPEKIND_R8 w/ computationalEdge and total widths Test"
  write(failMsg, *) "Total bounds are wrong"
  if (localPet==0) then
    call ESMF_Test((totalLBound(1,1)==0.and.totalLBound(2,1)==0.and.&
    totalUBound(1,1)==11.and.totalUBound(2,1)==16), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet==1) then
    call ESMF_Test((totalLBound(1,1)==8.and.totalLBound(2,1)==0.and.&
    totalUBound(1,1)==16.and.totalUBound(2,1)==16), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet==2) then
    call ESMF_Test((totalLBound(1,1)==0.and.totalLBound(2,1)==11.and.&
    totalUBound(1,1)==11.and.totalUBound(2,1)==28), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet==3) then
    call ESMF_Test((totalLBound(1,1)==8.and.totalLBound(2,1)==11.and.&
    totalUBound(1,1)==16.and.totalUBound(2,1)==28), &
      name, failMsg, result, ESMF_SRCLINE)
  endif  
  deallocate(totalLBound, totalUBound)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  ! cleanup  
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  ! test validate code
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "DistGrid Validate of non-created DistGrid Test"
  write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
  call ESMF_DistGridValidate(distgrid2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "DistGrid create DistGrid Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  distgrid2 = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/40/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "DistGrid Validate of created DistGrid Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridValidate(distgrid2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "DistGrid destroy DistGrid Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridDestroy(distgrid2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "DistGrid Validate of destroyed DistGrid Test"
  write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
  call ESMF_DistGridValidate(distgrid2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  ! preparations
  distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/40/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate AssmdShape 1D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(farray=farray1D, distgrid=distgrid, &
    indexflag=ESMF_INDEX_DELOCAL, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet F90 array pointer, 1D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr1D, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet w/ incompatible F90 array pointer, 1D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr2D, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayPrint AssmdShape 1D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate AssmdShape 1D ESMF_TYPEKIND_R8 w/ negative computationalEdge widths Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(farray=farray1D, distgrid=distgrid, &
    indexflag=ESMF_INDEX_DELOCAL, computationalEdgeLWidth=(/-1/), &
    computationalEdgeUWidth=(/-1/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayPrint AssmdShape 1D ESMF_TYPEKIND_R8 w/ negative computationalEdge widths Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  ! cleanup  
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  !------------------------------------------------------------------------
  ! preparations
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/40,10/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate AssmdShape 2D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(farray=farray2D, distgrid=distgrid, &
    indexflag=ESMF_INDEX_DELOCAL, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet F90 array pointer, 2D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr2D, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet w/ incompatible F90 array pointer, 2D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr3D, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  ! cleanup  
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  !------------------------------------------------------------------------
  ! preparations
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1,1/), maxIndex=(/40,10,10/), &
    rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate AssmdShape 3D ESMF_TYPEKIND_R4 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(farray=farray3D, distgrid=distgrid, &
    indexflag=ESMF_INDEX_DELOCAL, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet F90 array pointer, 3D ESMF_TYPEKIND_R4 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr3D, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  ! cleanup  
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  !------------------------------------------------------------------------
  ! preparations
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1,1,1/), &
    maxIndex=(/40,10,10,10/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate AssmdShape 4D ESMF_TYPEKIND_I4 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(farray=farray4D, distgrid=distgrid, &
    indexflag=ESMF_INDEX_DELOCAL, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet F90 array pointer, 4D ESMF_TYPEKIND_I4 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr4D, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  



  
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Adding Attribute to an Array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_AttributeSet(array, "Scale Factor", 4, rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Getting Attribute count from an Array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_AttributeGet(array, count, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Verify Attribute count from an Array"
  write(failMsg, *) "Incorrect count"
  call ESMF_Test((count.eq.1), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Getting Attribute info from an Array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_AttributeGet(array, name="Scale Factor", count=count, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Verify Attribute count from an Array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_Test((count.eq.1), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  intattr = 0
  write(name, *) "Getting an Integer Attribute back from an Array"
  write(failMsg, *) "Incorrect result"
  call ESMF_AttributeGet(array, "Scale Factor", intattr, rc)
  call ESMF_Test((intattr.eq.4), name, failMsg, result, ESMF_SRCLINE)
 
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Getting a second Integer Attribute back from an Array"
  write(failMsg, *) "Incorrect result"
  call ESMF_AttributeSet(array, "Invalid Data Tag", -999, rc)
  intattr2 = 0
  call ESMF_AttributeGet(array, "Invalid Data Tag", intattr2, rc)
  print *, "Invalid Data Tag should be -999, is: ", intattr2
  call ESMF_Test((intattr2.eq.-999), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Getting an non-existant Integer Attribute from an Array"
  write(failMsg, *) "Incorrect result"
  print *, "ready to call no attr"
  call ESMF_AttributeGet(array, "No such attribute", intattr, rc)
  print *, "back from no attr"
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  print *, "done with test"

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Getting an Integer List Attribute back from an Array"
  write(failMsg, *) "Incorrect result"
  call ESMF_AttributeSet(array, "Multiple Scale Factors", 4, (/4,3,2,1/), rc)
  intattr = 0
  count = 4   ! expected number of values
  call ESMF_AttributeGet(array, "Multiple Scale Factors", count, intattrlist, rc)
  print *, count, "attributes found in list"
  call ESMF_Test((intattrlist(1).eq.4), name, failMsg, result, ESMF_SRCLINE)
 
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Getting a real Attribute back from an Array"
  write(failMsg, *) "Incorrect result"
  rattr = 3.14159
  call ESMF_AttributeSet(array, "Pi", 3.14159_ESMF_KIND_R8, rc)
  rattr = 0.0
  call ESMF_AttributeGet(array, "Pi", rattr, rc)
  print *, "Pi should be 3.14159, is: ", rattr
  call ESMF_Test((rattr-3.14159.lt.0.00001), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Getting a real list Attribute back from an Array"
  write(failMsg, *) "Incorrect result"
  rattrlist = (/ 1.1, 2.2 /)
  call ESMF_AttributeSet(array, "Vertices", 2, rattrlist, rc)
  rattr = 0.0
  count = 2   ! expected count
  call ESMF_AttributeGet(array, "Vertices", count, rattrlist, rc)
  print *, count, "attributes found in list"
  print *, "Vertices should be 1.1 and 2.2, are: ", rattrlist
  call ESMF_Test((rattrlist(1).eq.1.1), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Getting a logical Attribute back from an Array"
  write(failMsg, *) "Incorrect result"
  call ESMF_AttributeSet(array, "Sky is Blue", ESMF_TRUE, rc)
  lattr = ESMF_FALSE
  call ESMF_AttributeGet(array, "Sky is Blue", lattr, rc)
  call ESMF_LogicalString(lattr, lattrstr, rc)
  print *, "Sky is Blue  should be true, is: ", lattrstr
  call ESMF_Test((lattr.eq.ESMF_TRUE), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Getting a logical Attribute back from an Array"
  write(failMsg, *) "Incorrect result"
  call ESMF_AttributeSet(array, "FlipFlop", 3, (/ESMF_TRUE,ESMF_FALSE,ESMF_TRUE/), rc)
  lattr = ESMF_FALSE
  count = 3   ! expected count
  call ESMF_AttributeGet(array, "FlipFlop", count, lattrlist, rc)
  print *, count, "attributes found in list"
  print *, "FlipFlop should be alternate, are: " 
  do i=1, 3
    call ESMF_LogicalString(lattrlist(i), lattrstr, rc)
    print *, i, lattrstr
  enddo
  call ESMF_Test((lattrlist(1).eq.ESMF_TRUE), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Getting a character Attribute back from an Array"
  write(failMsg, *) "Incorrect result"
  cattr = "It was a dark and stormy night"
  call ESMF_AttributeSet(array, "Book", cattr, rc)
  call ESMF_AttributeGet(array, "Book", cattr2, rc)
  print *, "Book  should be drivel, is: ", trim(cattr2)
  call ESMF_Test((cattr.eq.cattr2), name, failMsg, result, ESMF_SRCLINE)
  
  
  
  
  
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  ! cleanup  
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
10 continue
  !------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------

end program ESMF_ArrayCreateGetUTest
