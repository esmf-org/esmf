! $Id: ESMF_FieldUTest.F90,v 1.26 2003/10/20 20:13:56 cdeluca Exp $
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
      program ESMF_FieldUTest

!------------------------------------------------------------------------------
 
#include <ESMF_Macros.inc>

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
      use ESMF_Mod

      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_FieldUTest.F90,v 1.26 2003/10/20 20:13:56 cdeluca Exp $'
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
      type(ESMF_Grid) :: grid, grid2, grid3, grid4
      type(ESMF_Array) :: arr, arr2
      type(ESMF_ArraySpec) :: arrayspec
      real, dimension(:,:), pointer :: f90ptr1
      type(ESMF_DataMap) :: dm
      type(ESMF_RelLoc) :: rl
      character (len = 20) :: fname, fname1, fname2
      character (len = 20) :: gname, gname3
      type(ESMF_IOSpec) :: ios
      type(ESMF_Mask) :: mask
      type(ESMF_Field) :: f1, f2, f3, f4, f5

!-------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then 
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------- 
      call ESMF_Initialize(rc)

      !------------------------------------------------------------------------
      
      !NEX_UTest
      ! Test Requirement FLD1.1.3 Creation without data 
      ! Fields may be created as in FLD1.1.1 without allocating data or specifying 
      ! an associated data array. In this case specifying the grid parameters and 
      ! data array dimensions may be deferred until data is attached.
      f1 = ESMF_FieldCreateNoData(rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Field with no data Test Req. FLD1.1.3"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

#ifdef ESMF_EXHAUSTIVE

      !EX_UTest
      ! Verifing that an initialized Field can be printed
      call ESMF_FieldPrint(f1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing an initialized Field with no data Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Verifing that the Field name can be queried from a no data Field
      call ESMF_FieldGetName(f1, fname, rc=rc)
      write(failMsg, *) "default name not generated"
      write(name, *) "Getting name of Field with no data Test"
      call ESMF_Test((fname.ne.""), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      !EX_UTest
      ! default names unique
      f2 = ESMF_FieldCreateNoData(rc=rc)
      call ESMF_FieldGetName(f1, fname1, rc=rc)
      call ESMF_FieldGetName(f2, fname2, rc=rc)
      call ESMF_FieldPrint(f1)
      call ESMF_FieldPrint(f2)
      write(failMsg, *) "default name not unique"
      write(name, *) "Getting name of field created with default name"
      call ESMF_Test((fname1.ne.fname2), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldDestroy(f2, rc=rc)

#endif

      !NEX_UTest
      ! Test Requirement FLD1.4 Deletion 
      ! Fields may be deleted.
      call ESMF_FieldDestroy(f1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying initialized Field Test Req. FLD1.4"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

#ifdef ESMF_EXHAUSTIVE

     !EX_UTest
     ! Verifing that printing an uninitialized Field is handled properly.
     ! This code is commented out until bug 747699 is fixed.
     call ESMF_FieldPrint(f2, rc=rc)
     write(failMsg, *) ""
     write(name, *) "Printing an uninitialized Field Test"
     call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     !------------------------------------------------------------------------

     !EX_UTest
     ! verify that querying the name of a destroyed Field is handled properly.
     call ESMF_FieldGetName(f1, fname, rc=rc)
     write(failMsg, *) ""
     write(name, *) "Getting name of a destroyed Field Test"
     call ESMF_Test((rc.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)
     !------------------------------------------------------------------------

      !EX_UTest
      ! Verifing that printing a destroyed Field is handled properly.
      call ESMF_FieldPrint(f1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing destroyed Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

#endif
      !NEX_UTest
      ! Verifing that a Field can be created with a name
      f2 = ESMF_FieldCreateNoData("pressure", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating Field with name Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f2)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! Verifing that the Field name can be queried.
      Call ESMF_FieldGetName(f2, fname, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting a Field name Test"
      call ESMF_Test((fname.eq."pressure"), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f2)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! Verifing that recreating a Field is allowed.
      f2 = ESMF_FieldCreateNoData("temperature", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Recreate a created Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f2)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! Verifing that a Field can be created after it has been destroyed
      call ESMF_FieldDestroy(f2)
      f2 = ESMF_FieldCreateNoData("precipitation", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Recreate a destroyed Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f2)
      !------------------------------------------------------------------------

#ifdef ESMF_EXHAUSTIVE

      !EX_UTest
      ! Verifing that an uninitialized Grid can be printed
      call ESMF_GridPrint(grid, "", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing an uninitialized Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

#endif

      !NEX_UTest
      ! Verifing that a Grid can be created
      grid =  ESMF_GridCreate("atmgrid", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
     
#ifdef ESMF_EXHAUSTIVE

      !EX_UTest
      ! Test requirement FLD1.5.1. Default name attribute 
      ! The only default attribute of a field will be a name. A unique name will 
      ! be generated if not supplied by the user.
      ! Test Requirement FLD1.7.1 Query name
      ! A field shall be able to easily return its name. If the user does not provide 
      ! a field name one will be created. Field names must be unique within an address 
      ! space and it shall be possible to check this.
      ! Bug 705087 "Default Field names not unique"
      f1 = ESMF_FieldCreateNoData(rc=rc)
      f2 = ESMF_FieldCreateNoData(rc=rc)
      Call ESMF_FieldGetName(f1, fname1, rc=rc)
      Call ESMF_FieldGetName(f2, fname2, rc=rc)
      write(failMsg, *) "Field names not unique"
      write(name, *) "Unique default Field names Test, FLD1.5.1 & 1.7.1"
      call ESMF_Test((fname1.ne.fname2), name, failMsg, result, ESMF_SRCLINE)
      print *, "Field (f1) name = ", trim(fname1)
      print *, "Field (f2) name = ", trim(fname2)
      call ESMF_FieldPrint(f1)
      call ESMF_FieldPrint(f2)
      call ESMF_FieldDestroy(f1)
      call ESMF_FieldDestroy(f2)
      !------------------------------------------------------------------------


      !EX_UTest
      ! Verifing that a Grid can be printed
      call ESMF_GridPrint(grid, "", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

#endif

      !NEX_UTest
      ! Verifing that recreating a created Grid is allowed.
      grid =  ESMF_GridCreate("landgrid", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Recreating a created Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! Verifing that an Array can be created
      allocate(f90ptr1(10,20))
      arr = ESMF_ArrayCreate(f90ptr1, ESMF_DATA_REF, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

#ifdef ESMF_EXHAUSTIVE

      !EX_UTest
      ! Verifing that an Array can be printed
      call ESMF_ArrayPrint(arr, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing an Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

#endif

      !NEX_UTest
      ! Verifing that recreating a created Array is allowed
      allocate(f90ptr1(10,20))
      arr = ESMF_ArrayCreate(f90ptr1, ESMF_DATA_REF, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Recreating a created Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! Test requirement FLD1.1.1
      ! Fields may be created by specifying attributes, a grid, data array dimensions 
      ! and descriptors, optional masks (e.g. for active cells), and an optional I/O 
      ! specification. In this case a field will allocate its own data. The grid passed 
      ! into the argument list is referenced and not copied.
      call ESMF_ArraySpecInit(arrayspec, 2, ESMF_DATA_REAL, ESMF_R4)
      f2 = ESMF_FieldCreate(grid, arrayspec, relloc=ESMF_CELL_CENTER, &
                                          name="rh", rc=rc)
      write(failMsg, *) "ArraySpec has not been created"
      write(name, *) "Creating a Field with a Grid and ArraySpec Test FLD1.1.1"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! Requirement FLD1.1.2 Creation with external data 
      ! Fields may be created as in FLD1.1.1 with a data array passed into 
      ! the argument list. The data array is referenced and not copied.
      ! Verifing that a Field can be created with a Grid and Array
      f3 = ESMF_FieldCreate(grid, arr, ESMF_DATA_REF, ESMF_CELL_CENTER, &
                                   2, dm, "Field 0", ios, rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Field with a Grid and Array Test FLD1.1.2"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f3)
      !------------------------------------------------------------------------

#ifdef ESMF_EXHAUSTIVE

      ! Verifing that destroying a Grid in a Field is not allowed
      ! call ESMF_GridDestroy(grid, rc=rc)
      ! write(failMsg, *) ""
      ! write(name, *) "Destroying a Grid in a Field Test"
      ! call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Verifing that a Field with a Grid and Array can be destroyed
      call ESMF_FieldDestroy(f3, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying a Field with a Grid and Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f3)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Verifing that a Field cannot be created with an uninitialized Grid and Array
      f3 = ESMF_FieldCreate(grid2, arr2, ESMF_DATA_REF, ESMF_CELL_CENTER, &
                                   3, dm, "Field 0", ios, rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Field with an uninitialized Grid and Array Test"
      call ESMF_Test((rc.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f3)
      !------------------------------------------------------------------------
#endif

      !NEX_UTest
      ! Verifing that a Field with no data can be destroyed
      call ESMF_FieldDestroy(f2, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying a Field with no data Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f2)
      !------------------------------------------------------------------------

#ifdef ESMF_EXHAUSTIVE

      !EX_UTest
      ! Verifing that a destroying a destroyed  Field is handled properly.
      call ESMF_FieldDestroy(f2, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying a destroyed Field Test"
      call ESMF_Test((rc.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f2)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Verify that a Grid cannot be gotten from a Field created with no data
      f5 = ESMF_FieldCreateNoData(rc=rc)
      call ESMF_FieldGetGrid(f5, grid3, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting a Grid from a Field created with no data Test"
      call ESMF_Test((rc.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldDestroy(f5, rc=rc)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Req. 1.6.2 Return grid 
      ! A field shall be able to return a reference to its grid.
      ! The following code is commented out because there is
      ! no way to query the name of a Grid.
      ! It will be uncommented when the query function is written.
      ! Bug 705196 "Unable to query Grid name"
      gname="oceangrid"
      grid =  ESMF_GridCreate(name=gname, rc=rc)
      arr = ESMF_ArrayCreate(f90ptr1, ESMF_DATA_REF, rc=rc)
      f3 = ESMF_FieldCreate(grid, arr, ESMF_DATA_REF, ESMF_CELL_CENTER, &
                                   1, dm, "Field 0", ios, rc)
      call ESMF_FieldGetGrid(f3, grid3, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting a Grid from a Field created with no data Test"
      call ESMF_GridGet(grid3, name=gname3, rc=rc)
      print *, "Grid (grid3) name = ", trim(gname3)
      call ESMF_GridPrint(grid, "", rc=rc)
      call ESMF_GridPrint(grid3, "", rc=rc)
      call ESMF_Test((gname.eq.gname3), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! Requirement 1.2 Local memory layout 
      ! It shall be possible to specify whether the field data is row major or column 
      ! major at field creation and to rearrange it (assumes local copy).
      ! Cannot be tested until Bug 705247 "Unable to query Data Map from Field" 
      ! is fixed.
      !EX_UTest
      call ESMF_FieldGetDataMap(f3, dm, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting a DataMap from a Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_DataMapPrint(dm, "", rc=rc)

      ! Requirement 1.3 Index Order
      ! It shall be possible to specify the index order of field data and also to rearrange it.
      ! Cannot be tested until Bug 705308 "ESMF_FieldGetLocalDataInfo not implemented"
      ! is fixed.

      ! Requirement 1.7.2 Query number of dimensions and index order
      ! A Field shall be able to return the number of dimensions and index order it has.
      ! Cannot be tested until Bug 705308 "ESMF_FieldGetLocalDataInfo not implemented"
      ! is fixed.

      ! Requirement 1.7.3 Query attributes
      ! A Field can return its list of attributes.
      ! Cannot be tested until Bug 705716 "Field Query attributes not implemented"
      ! is fixed.

#endif

      call ESMF_Finalize(rc)

      print *, "******  End of FieldUTest  ******"

      end program ESMF_FieldUTest
