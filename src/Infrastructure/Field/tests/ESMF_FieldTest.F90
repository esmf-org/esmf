! $Id: ESMF_FieldTest.F90,v 1.5 2003/03/12 23:59:11 svasquez Exp $
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
      program ESMF_FieldTest

!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_FieldTest - One line general statement about this test
!
! !DESCRIPTION:
!
! The code in this file drives F90 Field unit tests.
! The companion file ESMF\_Field.F90 contains the definitions for the
! Field methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_BaseMod
      use ESMF_IOMod
      use ESMF_ArrayMod
      use ESMF_DataMapMod
      use ESMF_GridMod
      use ESMF_FieldMod

      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_FieldTest.F90,v 1.5 2003/03/12 23:59:11 svasquez Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc = 1

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

!     !LOCAL VARIABLES:
      integer :: x, y
      type(ESMF_Grid) :: grid
      type(ESMF_Array) :: arr
      real, dimension(:,:), pointer :: f90ptr1
      type(ESMF_DataMap) :: dm
      type(ESMF_RelLoc) :: rl
      character (len = 20) :: fname
      type(ESMF_IOSpec) :: ios
      type(ESMF_Field) :: f1, f2, f3, f4, f5

      print *, "******************FIELDS UNIT TESTS****************************"
      print *

     ! Verifing that printing an uninitialized Field is handled properly.
      call ESMF_FieldPrint(f1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing an uninitialized Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! Verifing that a Field can be created with no data
      f1 = ESMF_FieldCreateNoData()
      write(failMsg, *) ""
      write(name, *) "Creating a Field with no data Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! Verifing that an initialized  Field can be printed
      call ESMF_FieldPrint(f1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing an initialized Field with no data Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! Verifing that the Field name can be queried from a no data Field
      call ESMF_FieldGetName(f1, fname, rc=rc)
      write(failMsg, *) "returned name not 'default_name'"
      write(name, *) "Getting name of Field with no data Test"
      call ESMF_Test((fname.eq."default_name"), name, failMsg, result, ESMF_SRCLINE)

      ! Verifing that a Field can be destroyed
      call ESMF_FieldDestroy(f1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying initialized Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

       ! verify that querying the name of a destroyed Field is handled properly.
!      The following code is commented out because it crashes the program.
!      It will be uncommented when the bug is fixed.
!      print *
!      call ESMF_FieldGetName(f1, fname, rc=rc)
!      write(failMsg, *) ""
!      write(name, *) "Getting name of a destroyed Field Test"
!      call ESMF_Test((rc.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)
!

      ! Verifing that printing a destroyed Field is handled properly.
      call ESMF_FieldPrint(f1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing destroyed Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! Verifing that a Field can be created with a name
      f2 = ESMF_FieldCreateNoData("pressure", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating Field with name Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f2)

      ! Verifing that the Field name can be queried.
      Call ESMF_FieldGetName(f2, fname, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting a Field name Test"
      call ESMF_Test((fname.eq."pressure"), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f2)

      ! Verifing that recreating a Field is allowed.
      f2 = ESMF_FieldCreateNoData("temperature", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Recreate a created Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f2)
      call ESMF_FieldDestroy(f2)

      ! Verifing that a Field can be created after it has been destroyed
      f2 = ESMF_FieldCreateNoData("precipitation", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Recreate a destroyed Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f2)

      ! Verifing that a Grid can be created
      grid =  ESMF_GridCreate("atmgrid", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     
      ! Verifing that a Grid can be printed
      call ESMF_GridPrint(grid, "", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! Verifing that recreating a created Grid is allowed.
      grid =  ESMF_GridCreate("landgrid", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Recreating a created Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! Verifing that an Array can be created
      allocate(f90ptr1(10,20))
      arr = ESMF_ArrayCreate(f90ptr1, ESMF_NO_COPY, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! Verifing that an Array can be printed
      call ESMF_ArrayPrint(arr, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing an Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! Verifing that recreating a created Array is allowd
      allocate(f90ptr1(10,20))
      arr = ESMF_ArrayCreate(f90ptr1, ESMF_NO_COPY, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Recreating a created Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! Verifing that a Field can be created with a Grid and Array
      f3 = ESMF_FieldCreate(grid, arr, ESMF_NO_COPY, ESMF_CELL_CENTER, &
                                   dm, "Field 0", ios, rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Field with a Grid and Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f3)

      ! Verifing that a Field with a Grid and Array can be destroyed
      call ESMF_FieldDestroy(f3, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying a Field with a Grid and Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f3)

      ! Verifing that a Field with no data can be destroyed
      call ESMF_FieldDestroy(f2, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying a Field with no data Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f2)


      ! Verifing that a destroying a destroyed  Field is handled properly.
      call ESMF_FieldDestroy(f2, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying a destroyed Field Test"
      call ESMF_Test((rc.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f2)

      end program ESMF_FieldTest
