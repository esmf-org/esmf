! $Id: ESMF_ArrayUTest.F90,v 1.4 2004/06/15 22:45:07 jwolfe Exp $
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
      '$Id: ESMF_ArrayUTest.F90,v 1.4 2004/06/15 22:45:07 jwolfe Exp $'
!------------------------------------------------------------------------------

!   ! Local variables
    type(ESMF_Array) :: array1
    character(ESMF_MAXSTR) :: filename
    real, dimension(:,:), pointer :: f90ptr1


    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name, array_name
    integer :: rc, result = 0

    
    call ESMF_Initialize()


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

!   !  Create an Array Test
    ! The following code core dumps it will be uncommented
    ! when bug 932018 is fixed.
    !EX_UTest
    allocate(f90ptr1(10,20))
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Create Array Test"
    array1 =  ESMF_ArrayCreate(f90ptr1, ESMF_DATA_COPY, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 1 create returned"

!-------------------------------------------------------------------------------
!   !  Create an Array Test
 
    !EX_UTest
    allocate(f90ptr1(10,20))
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Create Array Test"
    array1 =  ESMF_ArrayCreate(f90ptr1, ESMF_DATA_REF, rc=rc)
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

#endif


    call ESMF_Finalize()


    end program ESMF_ArrayUTest
    
!\end{verbatim}
    
