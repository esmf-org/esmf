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

#define ESMF_FILENAME "ESMF_AttributeUtilUTest.F90"

#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!==============================================================================
!==============================================================================

program ESMF_AttributeUtilUTest

!==============================================================================
!BOP
! !PROGRAM: ESMF_AttributeUtilUTest
!
! !DESCRIPTION:
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod ! test methods
  use ESMF
  use ESMF_AttributeMod, only : ESMF_InfoFormatKey

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id$'
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!==============================================================================

  character(ESMF_MAXSTR)      :: failMsg
  character(ESMF_MAXSTR)      :: name
  integer                     :: rc
  integer                     :: result = 0

  character(:), allocatable :: key, name_key, convention, purpose

  type(ESMF_FieldBundle) :: src_fb, dst_fb, src_fb2, dst_fb2, src_fb3, dst_fb3, &
                            fb_idx
  type(ESMF_Info) :: infoh, infoh2
  character(:), allocatable :: actual
  type(ESMF_Grid) :: grid
  type(ESMF_Field) :: field,field2,field3,field4
  integer :: i,j,count,ii,n
  character(len=ESMF_MAXSTR) :: attname
  type(ESMF_TYPEKIND_FLAG) :: tk, actual_tk
  integer, pointer :: iptr(:)
  integer, dimension(2) :: atos
  real(ESMF_KIND_R4), dimension(4) :: arr
  real(ESMF_KIND_R8), dimension(3) :: arr2
  integer :: not_there, actual_value, itemCount

!------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoFormatKey w/ name"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  name_key = "AttributeName"
  call ESMF_InfoFormatKey(key, name_key, rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((rc .eq. ESMF_SUCCESS .and. &
                  key .eq. "/ESMF/General/AttributeName"), &
                  name, failMsg, result, ESMF_SRCLINE)
  ! Must abort to prevent possible hanging due to communications.
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Attribute copy by reference"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  src_fb = ESMF_FieldBundleCreate(name="src_fb", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  dst_fb = ESMF_FieldBundleCreate(name="dst_fb", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_AttributeCopy(src_fb, dst_fb, attcopy=ESMF_ATTCOPY_REFERENCE, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoGetFromHost(src_fb, infoh, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSet(infoh, "/NetCDF/FV3/output_file", "out.nc", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoGetFromHost(dst_fb, infoh2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoGetCharAlloc(infoh2, "/NetCDF/FV3/output_file", actual, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((actual .eq. "out.nc"), name, failMsg, result, ESMF_SRCLINE)
  ! Must abort to prevent possible hanging due to communications.
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Setting scalars and arrays in loop"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  grid = ESMF_GridCreate(countsPerDEDim1=[180], &
             & countsPerDEDim2=[91], &
             & indexFlag=ESMF_INDEX_DELOCAL, &
             & gridEdgeLWidth=[0,0], &
             & gridEdgeUWidth=[1,1], &
             & coordDep1=[1,2], &
             & coordDep2=[1,2], &
             & coordSys=ESMF_COORDSYS_SPH_RAD, &
             & rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field=ESMF_FieldCreate(grid,typekind=ESMF_TYPEKIND_R4,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field2=ESMF_FieldCreate(grid,typekind=ESMF_TYPEKIND_R4,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  i=17
  count=1
  call ESMF_AttributeSet(field,name="bob",value=i,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_AttributeGet(field,count=n,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  do ii=1,n
     call ESMF_AttributeGet(field,attributeIndex=ii,name=attname, &
      typekind=tk, itemcount=count, rc=rc)
     if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

     allocate(iptr(count))

     call ESMF_AttributeGet(field,  NAME=attname, itemcount=count, VALUELIST=iptr, RC=rc)
     if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

     call ESMF_AttributeSet(field2 , NAME=attname, itemcount=count, VALUELIST=iptr, RC=rc)
     if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

     deallocate(iptr)
  enddo

  call ESMF_AttributeSet(field2,name="bob",value=i,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_AttributeGet(field2,name="bob",value=j,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((i==j), name, failMsg, result, ESMF_SRCLINE)
  ! Must abort to prevent possible hanging due to communications.
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Array to scalar"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  field3=ESMF_FieldCreate(grid,typekind=ESMF_TYPEKIND_R4,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field4=ESMF_FieldCreate(grid,typekind=ESMF_TYPEKIND_R4,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  i=17
  count=1
  call ESMF_AttributeSet(field3,name="bob",value=i,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_AttributeGet(field3,count=n,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  do ii=1,n
     call ESMF_AttributeGet(field3,attributeIndex=ii,name=attname, &
      typekind=tk, itemcount=count, rc=rc)
     if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

     allocate(iptr(count))

     call ESMF_AttributeGet(field3,  NAME=attname, itemcount=count, VALUELIST=iptr, RC=rc)
     if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

     call ESMF_AttributeSet(field4 , NAME=attname, itemcount=count, VALUELIST=iptr, RC=rc)
     if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

     deallocate(iptr)
  enddo

  call ESMF_AttributeGet(field4,name="bob",value=j,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((i==j), name, failMsg, result, ESMF_SRCLINE)
  ! Must abort to prevent possible hanging due to communications.
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Scalar from multi-valued array should error out"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  call ESMF_AttributeSet(field3,name="mv_array",valueList=(/ 1, 2, 3, 4, 5 /),rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_AttributeGet(field3,name="mv_array",value=j,rc=rc)
  if (rc==ESMF_RC_ATTR_WRONGTYPE) rc = ESMF_SUCCESS

  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  ! Must abort to prevent possible hanging due to communications.
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Default value when getting an attribute that does not exist"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  call ESMF_AttributeGet(field3, name="not_there", value=not_there, defaultvalue=55, &
    rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((not_there==55), name, failMsg, result, ESMF_SRCLINE)
  ! Must abort to prevent possible hanging due to communications.
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Multi-valued array from scalar should fail"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  call ESMF_AttributeSet(field3,name="a_scalar",value=999,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_AttributeGet(field3,name="a_scalar",valueList=atos,rc=rc)
  if (rc==ESMF_RC_ATTR_ITEMSOFF) rc = ESMF_SUCCESS

  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  ! Must abort to prevent possible hanging due to communications.
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Set array and retrieve a type R4"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  arr(1) = HUGE(arr)
  arr(2) = HUGE(arr)
  arr(3) = HUGE(arr)
  arr(4) = HUGE(arr)

  call ESMF_AttributeSet(field3,name="arr",valueList=arr,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_AttributeGet(field3,name="arr",typekind=tk,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((tk==ESMF_TYPEKIND_R4), name, failMsg, result, ESMF_SRCLINE)
  ! Must abort to prevent possible hanging due to communications.
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Set array and retrieve a type R8"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  arr2(1) = HUGE(arr2)
  arr2(2) = HUGE(arr2) - 2.0

  call ESMF_AttributeSet(field3,name="arr2",valueList=arr2,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_AttributeGet(field3,name="arr2",typekind=tk,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((tk==ESMF_TYPEKIND_R8), name, failMsg, result, ESMF_SRCLINE)
  ! Must abort to prevent possible hanging due to communications.
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Mix R4 and R8 and return R8"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  arr2(1) = TINY(arr)
  arr2(2) = HUGE(arr2)
  ! Leave the third element uninitialized.

  call ESMF_AttributeSet(field3,name="arr_mix",valueList=arr2,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_AttributeGet(field3,name="arr_mix",typekind=tk,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((tk==ESMF_TYPEKIND_R8), name, failMsg, result, ESMF_SRCLINE)
  ! Must abort to prevent possible hanging due to communications.
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Attribute copy preserves 32-bit"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  src_fb2 = ESMF_FieldBundleCreate(name="src_fb2", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  dst_fb2 = ESMF_FieldBundleCreate(name="dst_fb2", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_AttributeSet(src_fb2, "is_32bit", not_there, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_AttributeCopy(src_fb2, dst_fb2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_AttributeGet(dst_fb2, "is_32bit", typekind=actual_tk, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((actual_tk == ESMF_TYPEKIND_I4), name, failMsg, result, ESMF_SRCLINE)
  ! Must abort to prevent possible hanging due to communications.
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Attribute copy by reference preserves 32-bit"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  src_fb3 = ESMF_FieldBundleCreate(name="src_fb3", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  dst_fb3 = ESMF_FieldBundleCreate(name="dst_fb3", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_AttributeSet(src_fb3, "is_32bit", actual_value, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_AttributeCopy(src_fb3, dst_fb3, attcopy=ESMF_ATTCOPY_REFERENCE, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  actual_tk = ESMF_TYPEKIND_I8
  call ESMF_AttributeGet(dst_fb3, "is_32bit", typekind=actual_tk, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((actual_tk == ESMF_TYPEKIND_I4), name, failMsg, result, ESMF_SRCLINE)
  ! Must abort to prevent possible hanging due to communications.
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Attribute get by index preserves 32-bit"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  fb_idx = ESMF_FieldBundleCreate(name="fb_idx", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_AttributeSet(fb_idx, "is_32bit", actual_value, convention="NetCDF", &
                         purpose="FV3", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  actual_tk = ESMF_TYPEKIND_I4
  call ESMF_AttributeGet(fb_idx, convention="NetCDF", purpose="FV3", &
                         attnestflag=ESMF_ATTNEST_OFF, attributeIndex=1, &
                         name=attname, typekind=actual_tk, itemCount=itemCount, &
                         rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((actual_tk == ESMF_TYPEKIND_I4), name, failMsg, result, ESMF_SRCLINE)
  ! Must abort to prevent possible hanging due to communications.
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  call ESMF_FieldBundleDestroy(src_fb, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldBundleDestroy(dst_fb, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldBundleDestroy(src_fb2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldBundleDestroy(dst_fb2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldBundleDestroy(src_fb3, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldBundleDestroy(dst_fb3, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldDestroy(field, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldDestroy(field2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldDestroy(field3, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldDestroy(field4, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridDestroy(grid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally

end program ESMF_AttributeUtilUTest
