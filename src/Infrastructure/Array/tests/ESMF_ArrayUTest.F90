! $Id: ESMF_ArrayUTest.F90,v 1.9 2004/10/05 15:20:11 svasquez Exp $
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
      program ESMF_ArrayUTest

!------------------------------------------------------------------------------
!

#include <ESMF_Macros.inc>

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
      '$Id: ESMF_ArrayUTest.F90,v 1.9 2004/10/05 15:20:11 svasquez Exp $'
!------------------------------------------------------------------------------

!   ! Local variables
    type(ESMF_Array) :: array1
    !character(ESMF_MAXSTR) :: filename
    real, dimension(:,:), pointer :: f90ptr1
    integer :: width, attribute, attribute1, attribute2
    real :: attribute3, attribute4
    type(ESMF_DataType) :: att_datatype
    type(ESMF_DataKind) :: att_datakind
    type(ESMF_VM):: vm


    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name, array_name
    integer :: rc, att_count, result = 0, npets

    
     call ESMF_Initialize(vm=vm, rc=rc)
     call ESMF_VMGet(vm, petCount=npets, rc=rc)
     print '(/, a, i3)' , "NUMBER_OF_PROCESSORS", npets



!-------------------------------------------------------------------------------
!   !  Create an Array Test
 
    !NEX_UTest
    allocate(f90ptr1(10,20))
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Create Array Test"
    array1 =  ESMF_ArrayCreate(f90ptr1, ESMF_DATA_REF, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!   !  Destroy an Array Test

    !NEX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Destroy Array Test"
    call ESMF_ArrayDestroy(array1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_EXHAUSTIVE
!-------------------------------------------------------------------------------

!   !  Create an Array Test with data copy
    !EX_UTest
    allocate(f90ptr1(10,20))
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Create Array Test"
    array1 = ESMF_ArrayCreate(f90ptr1, ESMF_DATA_COPY, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 1 create returned"

!-------------------------------------------------------------------------------
!   !  Set Array Name Test
 
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Set Array Name Test"
    call ESMF_ArraySet(array1, name="SAM", rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 1 create returned"

!-------------------------------------------------------------------------------
!   !  Get Array Name Test
 
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS or returned wrong name" 
    write(name, *) "Get Array Name Test"
    call ESMF_ArrayGet(array1, name=array_name, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS.and.array_name.eq."SAM"), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 1 is called ", array_name

!-------------------------------------------------------------------------------
!   !  Destroy an Array Test

    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Destroy Array Test"
    call ESMF_ArrayDestroy(array1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 1 destroy returned"

!-------------------------------------------------------------------------------
!
!   !  Create an Array with a halo width
 
    !EX_UTest
    allocate(f90ptr1(10,20))
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Create Array with HaloWidth Test"
    array1 = ESMF_ArrayCreate(f90ptr1, ESMF_DATA_COPY, haloWidth=2, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 1 create returned"

!-------------------------------------------------------------------------------
!   !  Get Halo Width back
 
    !EX_UTest
    write(name, *) "Get Array HaloWidth Test"
    call ESMF_ArrayGet(array1, haloWidth=width, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !EX_UTest
    write(name, *) "Verify Array HaloWidth Test"
    write(failMsg, *) "Halo Width not 2" 
    call ESMF_Test((width.eq.2), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 1 get halowidth returned"

!-------------------------------------------------------------------------------
!
   !  Get Attribute count from an Array
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Set an Attribute in an Array Test"
    call ESMF_ArrayGetAttributeCount(array1, attribute, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!
   !  Verify Attribute count from an Array
    !EX_UTest
    write(failMsg, *) "Attribute count is incorrect" 
    write(name, *) "Verify Attribute count from an Array Test"
    call ESMF_Test((attribute.eq.0), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!
   !  Set an Attribute in an Array
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Set an Attribute in an Array Test"
    call ESMF_ArraySetAttribute(array1, "test_attribute", 123456789, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!
   !  Set an Attribute in an Array
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Set an Attribute in an Array Test"
    call ESMF_ArraySetAttribute(array1, "test_attribute1", 0, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


!-------------------------------------------------------------------------------
!
   !  Set an Attribute in an Array
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Set an Attribute in an Array Test"
    call ESMF_ArraySetAttribute(array1, "test_attribute2", 0.0, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!
   !  Set an Attribute in an Array
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Set an Attribute in an Array Test"
    call ESMF_ArraySetAttribute(array1, "test_attribute3", 6789, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


!-------------------------------------------------------------------------------
!
   !  Set an Attribute in an Array
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Set an Attribute in an Array Test"
    call ESMF_ArraySetAttribute(array1, "test_attribute4", 5.87, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!
   !  Get Attribute count from an Array
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Get an Attribute from an Array Test"
    call ESMF_ArrayGetAttributeCount(array1, attribute, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!
   !  Verify Attribute count from an Array
    !EX_UTest
    write(failMsg, *) "Attribute count is incorrect" 
    write(name, *) "Verify Attribute count from an Array Test"
    call ESMF_Test((attribute.eq.5), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!
   !  Get an Attribute from an Array
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Get an Attribute from an Array Test"
    call ESMF_ArrayGetAttribute(array1, "test_attribute", attribute, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!
   !  Verify the value of the Attribute
    !EX_UTest
    write(failMsg, *) "Attribute value is wrong" 
    write(name, *) "Verify Attribute value from an Array Test"
    call ESMF_Test((attribute.eq.123456789), name, failMsg, result, ESMF_SRCLINE)
!-------------------------------------------------------------------------------
!
   !  Get Attribute Info from an Array
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Get Attribute Info from an Array Test"
    call ESMF_ArrayGetAttributeInfo(array1, "test_attribute", datatype=att_datatype, &
                                     datakind=att_datakind, count=att_count, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!
   !  Verify datatype of Attribute
    !EX_UTest
    write(failMsg, *) "Attribute datatype is wrong" 
    write(name, *) "Verify Attribute datatype from an Array Test"
    call ESMF_Test((att_datatype.eq.ESMF_DATA_INTEGER), name, failMsg, result, ESMF_SRCLINE)
!-------------------------------------------------------------------------------
!
   !  Verify datakind of Attribute
    !EX_UTest
    write(failMsg, *) "Attribute datakind is wrong" 
    write(name, *) "Verify Attribute datakind from an Array Test"
    call ESMF_Test((att_datakind.eq.ESMF_I4), name, failMsg, result, ESMF_SRCLINE)
!-------------------------------------------------------------------------------
!
   !  Verify count of Attribute
    !EX_UTest
    write(failMsg, *) "Attribute count is wrong" 
    write(name, *) "Verify Attribute count from an Array Test"
    call ESMF_Test((att_count.eq.1), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!
   !  Get an Attribute from an Array with wrong data type
    !EX_UTest
    write(failMsg, *) "Should not return ESMF_SUCCESS" 
    write(name, *) "Get a Wrong Data type Attribute from an Array Test"
    call ESMF_ArrayGetAttribute(array1, "test_attribute4", attribute, rc=rc)
    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!
   !  Get an Attribute from an Array
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Get an Attribute from an Array Test"
    call ESMF_ArrayGetAttribute(array1, "test_attribute4", attribute4, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!
   !  Verify the value of the Attribute
    !EX_UTest
    write(failMsg, *) "Attribute value is wrong" 
    write(name, *) "Verify Attribute value from an Array Test"
    call ESMF_Test((attribute4.eq.5.87), name, failMsg, result, ESMF_SRCLINE)
    print *, "test_attribute4 = ", attribute4
!-------------------------------------------------------------------------------
!
   !  Get Attribute Info from an Array
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Get Attribute Info from an Array Test"
    call ESMF_ArrayGetAttributeInfo(array1, "test_attribute4", datatype=att_datatype, &
                                     datakind=att_datakind, count=att_count, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!
   !  Verify datatype of Attribute
    !EX_UTest
    write(failMsg, *) "Attribute datatype is wrong" 
    write(name, *) "Verify Attribute datatype from an Array Test"
    call ESMF_Test((att_datatype.eq.ESMF_DATA_REAL), name, failMsg, result, ESMF_SRCLINE)
!-------------------------------------------------------------------------------
!
   !  Verify datakind of Attribute
    !EX_UTest
    write(failMsg, *) "Attribute datakind is wrong" 
    write(name, *) "Verify Attribute datakind from an Array Test"
    call ESMF_Test((att_datakind.eq.ESMF_R4), name, failMsg, result, ESMF_SRCLINE)
!-------------------------------------------------------------------------------
!
   !  Verify count of Attribute
    !EX_UTest
    write(failMsg, *) "Attribute count is wrong" 
    write(name, *) "Verify Attribute count from an Array Test"
    call ESMF_Test((att_count.eq.1), name, failMsg, result, ESMF_SRCLINE)

    print *, "Attribute count =", att_count
!-------------------------------------------------------------------------------
!   !  Destroy an Array Test

    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Destroy Array Test"
    call ESMF_ArrayDestroy(array1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 1 destroy returned"

!-------------------------------------------------------------------------------

#endif


    call ESMF_Finalize()


    end program ESMF_ArrayUTest
    
!\end{verbatim}
    
