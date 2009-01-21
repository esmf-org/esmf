! $Id: ESMF_InternArrayUTest.F90,v 1.15.2.4 2009/01/21 21:25:22 cdeluca Exp $
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
      program ESMF_ArrayUTest

!------------------------------------------------------------------------------
!

#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_ArrayTest - Test Array functionalities
!
! !DESCRIPTION:
!
! The code in this file drives F90 Array unit tests.
!
!EOP
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod      ! test methods
      use ESMF_Mod
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_InternArrayUTest.F90,v 1.15.2.4 2009/01/21 21:25:22 cdeluca Exp $'
!------------------------------------------------------------------------------

!   ! Local variables
    type(ESMF_InternArray) :: array1, array2
    real(ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr1
    integer :: width, attribute
    integer :: counts(2), lbounds(2), ubounds(2)
    real(ESMF_KIND_R4) :: attribute4
    logical :: tf_result
    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_TypeKind) :: att_typekind


    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name, array_name
    integer :: rc, status, att_count, result = 0

    
    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)



!-------------------------------------------------------------------------------
!   !  Create an Array Test
 
    !NEX_removeUTest
    allocate(f90ptr1(10,20))
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Create Array Test"
    array1 =  ESMF_InternArrayCreate(f90ptr1, ESMF_DATA_REF, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!   !  Destroy an Array Test

    !NEX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Destroy Array Test"
    call ESMF_InternArrayDestroy(array1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!   !  Check memory status test - this memory should not be deallocated 
!   !  by the array destroy call, so the associated() call should return true.

    !NEX_removeUTest
    write(failMsg, *) "Data area should not be deallocated"
    write(name, *) "Data not deallocated at destroy time"
    tf_result = associated(f90ptr1)
    call ESMF_Test((tf_result), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!   !  Deallocate the user-allocated memory (framework should *not* have
!   !   deallocated it at array destroy time).

    !NEX_removeUTest
    write(failMsg, *) "Did not return Fortran status code = 0" 
    write(name, *) "Deallocate memory"
    deallocate(f90ptr1, stat=status)
    call ESMF_Test((status.eq.0), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!   !  Check memory status test - this memory should now be deallocated,
!   !  so the associated() call should return false.

    !NEX_removeUTest
    write(failMsg, *) "Data area should be deallocated"
    write(name, *) "Data not deallocated"
    tf_result = associated(f90ptr1)
    call ESMF_Test((.not. tf_result), name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_TESTEXHAUSTIVE

!-------------------------------------------------------------------------------
!   !  Destroy destroyed  Array Test

    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
    write(name, *) "Destroy a destroyed Array Test"
    call ESMF_InternArrayDestroy(array1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!   !  Destroy a non-created Array Test

    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
    write(name, *) "Destroy a non-created Array Test"
    call ESMF_InternArrayDestroy(array2, rc=rc)
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!   !  Get Array Name  of deleted Array Test
 
    !EX_____UTest
!    write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
!    write(name, *) "Get Array Name of destroyed Array Test"
!    call ESMF_InternArrayGet(array1, name=array_name, rc=rc)
!    call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!   !  Get Array Name  of non-created Array Test
 
    !EX_____UTest
!    write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
!    write(name, *) "Get Array Name of non-created Array Test"
!    call ESMF_InternArrayGet(array2, name=array_name, rc=rc)
!    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!  !  Print a destroyed Array

    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED" 
    write(name, *) "Print a destroyed Array Test"
    call ESMF_InternArrayPrint(array1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!  !  Print a non-created Array

    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED" 
    write(name, *) "Print a non-created Array Test"
    call ESMF_InternArrayPrint(array2, rc=rc)
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!  !  Get Attribute count from a destroyed Array

    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED" 
    write(name, *) "Set an Attribute from a destroyed Array Test"
    call ESMF_IArrayGetAttributeCount(array1, attribute, rc=rc)
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!  !  Get Attribute count from a non-created Array

    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED" 
    write(name, *) "Set an Attribute from a non-created Array Test"
    call ESMF_IArrayGetAttributeCount(array2, attribute, rc=rc)
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!  !  Set an Attribute in a destroyed Array

    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
    write(name, *) "Set an Attribute in a destroyed Array Test"
    call ESMF_IArraySetAttribute(array1, "test_attribute", 123456789, rc=rc)
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!  !  Set an Attribute in a non-created Array

    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
    write(name, *) "Set an Attribute in a non-created Array Test"
    call ESMF_IArraySetAttribute(array2, "test_attribute", 123456789, rc=rc)
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!  !  Get Attribute Info from a deleted Array

    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED" 
    write(name, *) "Get Attribute Info from a deleted Array Test"
    call ESMF_IArrayGetAttributeInfo(array1, "test_attribute", &
      typekind=att_typekind, count=att_count, rc=rc)
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!  !  Get Attribute Info from a non-created Array

    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED" 
    write(name, *) "Get Attribute Info from a non-created Array Test"
    call ESMF_IArrayGetAttributeInfo(array2, "test_attribute", &
      typekind=att_typekind, count=att_count, rc=rc)
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!   !  Create an Array Test with data copy

    !EX_removeUTest
    allocate(f90ptr1(10,20))
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Create Array Test"
    array1 = ESMF_InternArrayCreate(f90ptr1, ESMF_DATA_COPY, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!   !  Set Array Name Test
 
    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Set Array Name Test"
    call ESMF_InternArraySet(array1, name="SAM", rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!   !  Get Array Name Test
 
    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS or returned wrong name" 
    write(name, *) "Get Array Name Test"
    call ESMF_InternArrayGet(array1, name=array_name, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS.and.array_name.eq."SAM"), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!   !  Destroy an Array Test

    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Destroy Array Test"
    call ESMF_InternArrayDestroy(array1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!   !  Deallocate the user-allocated memory (framework should *not* have
!   !   deallocated it at array destroy time).

    !EX_removeUTest
    write(failMsg, *) "Did not return Fortran status code = 0" 
    write(name, *) "Deallocate memory"
    deallocate(f90ptr1, stat=status)
    call ESMF_Test((status.eq.0), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!   !  Create an Array with a halo width
 
    !EX_removeUTest
    allocate(f90ptr1(-5:5,20:40))
    f90ptr1(:,:) = 1.0
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Create Array with HaloWidth Test"
    array1 = ESMF_InternArrayCreate(f90ptr1, ESMF_DATA_COPY, haloWidth=2, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!   !  Get Halo Width back
 
    !EX_removeUTest
    write(name, *) "Get Array HaloWidth Test"
    call ESMF_InternArrayGet(array1, haloWidth=width, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !EX_removeUTest
    write(name, *) "Verify Array HaloWidth Test"
    write(failMsg, *) "Halo Width not 2" 
    call ESMF_Test((width.eq.2), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!  !  Print an Array

    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Print an Array Test"
    call ESMF_InternArrayPrint(array1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!  !  Get Attribute count from an Array

    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Set an Attribute in an Array Test"
    call ESMF_IArrayGetAttributeCount(array1, attribute, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!  !  Verify Attribute count from an Array

    !EX_removeUTest
    write(failMsg, *) "Attribute count is incorrect" 
    write(name, *) "Verify Attribute count from an Array Test"
    call ESMF_Test((attribute.eq.0), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!  !  Set an Attribute in an Array

    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Set an Attribute in an Array Test"
    call ESMF_IArraySetAttribute(array1, "test_attribute", 123456789, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!  !  Set an Attribute in an Array

    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Set an Attribute in an Array Test"
    call ESMF_IArraySetAttribute(array1, "test_attribute1", 0, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!
   !  Set an Attribute in an Array
    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Set an Attribute in an Array Test"
    call ESMF_IArraySetAttribute(array1, "test_attribute2", 0.0, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!  !  Set an Attribute in an Array

    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Set an Attribute in an Array Test"
    call ESMF_IArraySetAttribute(array1, "test_attribute3", 6789, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!  !  Set an Attribute in an Array

    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Set an Attribute in an Array Test"
    call ESMF_IArraySetAttribute(array1, "test_attribute4", 5.87, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!  !  Get Attribute count from an Array

    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Get Attribute Count from an Array Test"
    call ESMF_IArrayGetAttributeCount(array1, attribute, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!  !  Verify Attribute count from an Array

    !EX_removeUTest
    write(failMsg, *) "Attribute count is incorrect" 
    write(name, *) "Verify Attribute count from an Array Test"
    call ESMF_Test((attribute.eq.5), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!  !  Get an Attribute from an Array

    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Get an Attribute from an Array Test"
    call ESMF_IArrayGetAttribute(array1, "test_attribute", attribute, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!  !  Verify the value of the Attribute

    !EX_removeUTest
    write(failMsg, *) "Attribute value is wrong" 
    write(name, *) "Verify Attribute value from an Array Test"
    call ESMF_Test((attribute.eq.123456789), name, failMsg, result, ESMF_SRCLINE)
!-------------------------------------------------------------------------------
!  !  Get Attribute Info from an Array

    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Get Attribute Info from an Array Test"
    call ESMF_IArrayGetAttributeInfo(array1, "test_attribute", &
      typekind=att_typekind, count=att_count, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!  !  Verify typekind of Attribute

    !EX_removeUTest
    write(failMsg, *) "Attribute typekind is wrong" 
    write(name, *) "Verify Attribute typekind from an Array Test"
    call ESMF_Test((att_typekind.eq.ESMF_TYPEKIND_I4), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!  !  Verify count of Attribute

    !EX_removeUTest
    write(failMsg, *) "Attribute count is wrong" 
    write(name, *) "Verify Attribute count from an Array Test"
    call ESMF_Test((att_count.eq.1), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!  !  Get an Attribute from an Array with wrong typekind

    !EX_removeUTest
    write(failMsg, *) "Should not return ESMF_SUCCESS" 
    write(name, *) "Get a Wrong typekind Attribute from an Array Test"
    call ESMF_IArrayGetAttribute(array1, "test_attribute4", attribute, rc=rc)
    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!  !  Get an Attribute from an Array

    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Get an Attribute from an Array Test"
    call ESMF_IArrayGetAttribute(array1, "test_attribute4", attribute4, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!  !  Verify the value of the Attribute

    !EX_removeUTest
    write(failMsg, *) "Attribute value is wrong" 
    write(name, *) "Verify Attribute value from an Array Test"
    call ESMF_Test((attribute4.eq.5.87), name, failMsg, result, ESMF_SRCLINE)
    print *, "test_attribute4 = ", attribute4

!-------------------------------------------------------------------------------
!  !  Get Attribute Info from an Array

    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Get Attribute Info from an Array Test"
    call ESMF_IArrayGetAttributeInfo(array1, "test_attribute4", &
      typekind=att_typekind, count=att_count, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!  !  Verify typekind of Attribute

    !EX_removeUTest
    write(failMsg, *) "Attribute typekind is wrong" 
    write(name, *) "Verify Attribute typekind from an Array Test"
    call ESMF_Test((att_typekind.eq.ESMF_TYPEKIND_R4), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!  !  Verify count of Attribute

    !EX_removeUTest
    write(failMsg, *) "Attribute count is wrong" 
    write(name, *) "Verify Attribute count from an Array Test"
    call ESMF_Test((att_count.eq.1), name, failMsg, result, ESMF_SRCLINE)
    print *, "Attribute count =", att_count

!-------------------------------------------------------------------------------
!   !  Destroy an Array Test

    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Destroy Array Test"
    call ESMF_InternArrayDestroy(array1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!   !  Deallocate the user-allocated memory (framework should *not* have
!   !   deallocated it at array destroy time).

    !EX_removeUTest
    write(failMsg, *) "Did not return Fortran status code = 0" 
    write(name, *) "Deallocate memory"
    deallocate(f90ptr1, stat=status)
    call ESMF_Test((status.eq.0), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!   !  Create an ArraySpec for use below
 
    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Create ArraySpec for use in ArrayCreate"
    call ESMF_ArraySpecSet(arrayspec, rank=2, &
                           typekind=ESMF_TYPEKIND_R8, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!   !  Create an Array with data allocation inside the framework
 
    !EX_removeUTest
    nullify(f90ptr1)
    counts(1) = 10
    counts(2) = 20
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Create Array with data allocated by framework"
    array1 = ESMF_InternArrayCreate(f90ptr1, counts, haloWidth=2, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!  !  Print an Array

    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Print an Array Test"
    call ESMF_InternArrayPrint(array1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!   !  Destroy an Array Test

    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Destroy Array Test"
    call ESMF_InternArrayDestroy(array1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!   !  Check memory status test - this memory should be deallocated 
!   !  by the array destroy call, so the associated() call should return false.

    !EX_removeUTest
    write(failMsg, *) "Data area should be deallocated"
    write(name, *) "Data not deallocated at destroy time"
    tf_result = associated(f90ptr1)
    call ESMF_Test((.not. tf_result), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!   !  Create an Array with data allocation inside the framework
 
    !EX_removeUTest
    nullify(f90ptr1)
    counts(1) = 11
    counts(2) = 36 
    lbounds(1) = -3
    lbounds(2) = -3
    ubounds(1) = 8
    ubounds(2) = 33
    width = 3
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Create Array with data allocated by framework"
    array1 = ESMF_InternArrayCreate(f90ptr1, counts, width, lbounds, ubounds, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!  !  Print an Array

    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Print an Array Test"
    call ESMF_InternArrayPrint(array1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!   !  Destroy an Array Test

    !EX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Destroy Array Test"
    call ESMF_InternArrayDestroy(array1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!   !  Check memory status test - this memory should be deallocated 
!   !  by the array destroy call, so the associated() call should return false.

    !EX_removeUTest
    write(failMsg, *) "Data area should be deallocated"
    write(name, *) "Data not deallocated at destroy time"
    tf_result = associated(f90ptr1)
    call ESMF_Test((.not. tf_result), name, failMsg, result, ESMF_SRCLINE)

#endif

    call ESMF_TestEnd(result, ESMF_SRCLINE)

    end program ESMF_ArrayUTest
    
!\end{verbatim}
    
