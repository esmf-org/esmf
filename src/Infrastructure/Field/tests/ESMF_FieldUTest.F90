! $Id: ESMF_FieldUTest.F90,v 1.75 2004/10/14 22:59:30 nscollins Exp $
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
      '$Id: ESMF_FieldUTest.F90,v 1.75 2004/10/14 22:59:30 nscollins Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc = 1

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

!     !LOCAL VARIABLES:
      integer :: i, count, npets
      integer, dimension(2) :: cellCounts
      type(ESMF_DELayout) :: delayout
      type(ESMF_VM) :: vm
      type(ESMF_Grid) :: grid, grid2, grid3, grid4
      type(ESMF_Array) :: arr, arr2
      type(ESMF_ArraySpec) :: arrayspec
      real, dimension(:,:), pointer :: f90ptr1, f90ptr2, f90ptr3, f90ptr4, f90ptr5
      real(ESMF_KIND_R8) :: minCoord(2)
      type(ESMF_FieldDataMap) :: dm, dm1
      !type(ESMF_RelLoc) :: rl
      character (len = 20) :: fname, fname1, fname2
      character (len = 20) :: gname, gname3
      type(ESMF_IOSpec) :: ios
      !type(ESMF_Mask) :: mask
      type(ESMF_Field) :: f1, f2, f3, f4, f5, f6, f7
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
      print *, "Starting job"

      call ESMF_Initialize(vm=vm, rc=rc)
      call ESMF_VMGet(vm, petCount=npets, rc=rc)
      call ESMF_TestStart(npets, ESMF_SRCLINE)


      !------------------------------------------------------------------------
      ! several calls to field need a valid grid.  these will be used in
      ! the grid create calls.
      delayout = ESMF_DELayoutCreate(vm, rc=rc)
      minCoord(:) = (/ 0.0, 0.0 /)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Test Requirement FLD1.1.3 Creation without data 
      ! Fields may be created as in FLD1.1.1 without allocating data or 
      ! specifying an associated data array. In this case specifying the 
      ! grid parameters and data array dimensions may be deferred until 
      ! data is attached.
      f1 = ESMF_FieldCreateNoData(rc=rc) 
      write(failMsg, *) ""
      write(name, *) "Creating a Field with no data Test Req. FLD1.1.3"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! Verifing that a Field with no data can be destroyed
      call ESMF_FieldDestroy(f1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying a Field with no data Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_EXHAUSTIVE

      !------------------------------------------------------------------------

      !EX_UTest
      f1 = ESMF_FieldCreateNoData(rc=rc) 
      write(failMsg, *) ""
      write(name, *) "Creating a Field with no data Test Req. FLD1.1.3"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Verifing that an initialized Field can be printed
      call ESMF_FieldPrint(f1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing an initialized Field with no data Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Verifing that the Field name can be queried from a no data Field
      call ESMF_FieldGet(f1, name=fname, rc=rc)
      write(failMsg, *) "default name not generated"
      write(name, *) "Getting name of Field with no data Test"
      call ESMF_Test((fname.ne.""), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Getting Attrubute count from a Field
      call ESMF_FieldGetAttributeCount(f1, count, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Attribute count from a Field "
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      ! Verify Attribute Count Test
      write(failMsg, *) "Incorrect count"
      write(name, *) "Verify Attribute count from a Field "
      call ESMF_Test((count.eq.0), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! default names unique
      f2 = ESMF_FieldCreateNoData(rc=rc)
      call ESMF_FieldGet(f1, name=fname1, rc=rc)
      call ESMF_FieldGet(f2, name=fname2, rc=rc)
      call ESMF_FieldPrint(f1)
      call ESMF_FieldPrint(f2)
      write(failMsg, *) "default name not unique"
      write(name, *) "Verifing uniqueness of fields created default name"
      call ESMF_Test((fname1.ne.fname2), name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! Verifing that a Field with no data can be destroyed
      call ESMF_FieldDestroy(f2, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying a Field with no data Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f2)
      !------------------------------------------------------------------------


      !EX_UTest
      ! Test Requirement FLD1.4 Deletion 
      ! Fields may be deleted.
      call ESMF_FieldDestroy(f1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying initialized Field Test Req. FLD1.4"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Verifing that printing an uninitialized Field is handled properly.
      call ESMF_FieldPrint(f6, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing an uninitialized Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! verify that querying the name of a destroyed Field is handled properly.
      call ESMF_FieldGet(f1, name=fname, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting name of a destroyed Field Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Verifing that printing a destroyed Field is handled properly.
      call ESMF_FieldPrint(f1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing destroyed Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Verifing that a Field can be created with a name
      f2 = ESMF_FieldCreateNoData("pressure", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating Field with name Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f2)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Verifing that the Field name can be queried.
      Call ESMF_FieldGet(f2, name=fname, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting a Field name Test"
      call ESMF_Test((fname.eq."pressure"), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f2)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Verifing that recreating a Field is allowed.
      f2 = ESMF_FieldCreateNoData("temperature", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Recreate a created Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f2)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Verifing that a Field can be created after it has been destroyed
      call ESMF_FieldDestroy(f2)
      f2 = ESMF_FieldCreateNoData("precipitation", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Recreate a destroyed Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f2)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Verifing that a Grid can be created
      grid =  ESMF_GridCreateHorzXYUni((/ 10, 20 /), minCoord, &
                                     name="landgrid", rc=rc)
      call ESMF_GridDistribute(grid, delayout=delayout, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Grid to use in Field Tests"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
     
      !EX_UTest
      ! Get local Grid counts now for use later
      call ESMF_GridGetDELocalInfo(grid, ESMF_CELL_CENTER, &
                                    localCellCountPerDim=cellCounts, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting cell counts for each DE"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test requirement FLD1.5.1. Default name attribute 
      ! The only default attribute of a field will be a name. A unique 
      ! name will be generated if not supplied by the user.
      ! Test Requirement FLD1.7.1 Query name
      ! A field shall be able to easily return its name. 
      ! If the user does not provide a field name one will be created. 
      ! Field names must be unique within an address 
      ! space and it shall be possible to check this.
      ! Bug 705087 "Default Field names not unique"
      f1 = ESMF_FieldCreateNoData(rc=rc)
      f2 = ESMF_FieldCreateNoData(rc=rc)
      call ESMF_FieldGet(f1, name=fname1, rc=rc)
      call ESMF_FieldGet(f2, name=fname2, rc=rc)
      write(failMsg, *) "Field names not unique"
      write(name, *) "Unique default Field names Test, FLD1.5.1 & 1.7.1"
      call ESMF_Test((fname1.ne.fname2), name, failMsg, result, ESMF_SRCLINE)
      !print *, "Field (f1) name = ", trim(fname1)
      print *, "Field (f2) name = ", trim(fname2)
      call ESMF_FieldPrint(f1)
      call ESMF_FieldPrint(f2)
      call ESMF_FieldDestroy(f1)
      call ESMF_FieldDestroy(f2)
      !------------------------------------------------------------------------



      !EX_UTest
      ! Verifing that an Array can be created
      allocate(f90ptr1(cellCounts(1),cellCounts(2)))
      arr = ESMF_ArrayCreate(f90ptr1, ESMF_DATA_REF, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an Array for use in Field Tests"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      f4 = ESMF_FieldCreateNoData(rc=rc) 
      write(failMsg, *) ""
      write(name, *) "Creating a Field with no data Test Req. FLD1.1.3"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      ! Setting a data Array associated with Field
      call ESMF_FieldSetArray(f4, arr, rc=rc) 
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting a data Array associated with Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      ! Setting a Grid associated with Field
      call ESMF_FieldSetGrid(f4, grid, rc=rc) 
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting a Grid associated with Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      call ESMF_FieldValidate(f4, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Testing to see if Field is Valid"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test requirement FLD1.1.1
      ! Fields may be created by specifying attributes, a grid, data array 
      ! dimensions and descriptors, optional masks (e.g. for active cells), 
      ! and an optional I/O specification. In this case a field will 
      ! allocate its own data. The grid passed into the argument list 
      ! is referenced and not copied.
      call ESMF_ArraySpecSet(arrayspec, 2, ESMF_DATA_REAL, ESMF_R4, rc=rc)
      write(name, *) "Creating an ArraySpec Test "
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest
      f2 = ESMF_FieldCreate(grid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                                          name="rh", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a Field with a Grid and ArraySpec Test FLD1.1.1"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Verifing that a destroying a destroyed  Field is handled properly.
      call ESMF_FieldDestroy(f2, rc=rc)  ! should succeed, f2 exists
      call ESMF_FieldDestroy(f2, rc=rc)  ! should fail
      write(failMsg, *) ""
      write(name, *) "Destroying a destroyed Field Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! verify we can create a 3d data on a 2d grid
      call ESMF_ArraySpecSet(arrayspec, 3, ESMF_DATA_REAL, ESMF_R4, rc=rc)
      f2 = ESMF_FieldCreate(grid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                                          name="rh", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a Field with a 2d Grid and 3d ArraySpec"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Requirement FLD1.1.2 Creation with external data 
      ! Fields may be created as in FLD1.1.1 with a data array passed into 
      ! the argument list. The data array is referenced and not copied.
      ! Verifing that a Field can be created with a Grid and Array
      call ESMF_FieldDataMapSetDefault(dm, ESMF_INDEX_IJ)
      f3 = ESMF_FieldCreate(grid, arr, ESMF_DATA_REF, ESMF_CELL_CENTER, &
                            ESMF_CELL_CELL, 2, dm, "Field 0", ios, rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Field with a Grid and Array Test FLD1.1.2"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_FieldValidate(f3, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Testing to see if Field is Valid"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Create a different size Grid for testing with an incompatible Array
      grid4 =  ESMF_GridCreateHorzXYUni((/ 100, 20 /), minCoord, &
                                     name="biglandgrid", rc=rc)
      call ESMF_GridDistribute(grid4, delayout=delayout, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Grid to use in Field Tests"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
     
      !EX_UTest
      ! Try to create a Field with a Grid and Array of the wrong sizes
      f6 = ESMF_FieldCreate(grid4, arr, ESMF_DATA_REF, ESMF_CELL_CENTER, &
                            ESMF_CELL_CELL, 2, dm, "Field 1", ios, rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Field with a mismatched Grid/Array"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Verifing the ESMF_FieldSetDataMap
      !  setting a different datamap in an existing field which already has
      !  data is not implemented yet (it is interpreted as a request to 
      !  reorder the data).  this will work if the field has no data yet.
      call ESMF_FieldDataMapSetDefault(dm1, 2, (/ 1, 0 /), rc=rc)
      f1 = ESMF_FieldCreateNoData(rc=rc) 
      call ESMF_FieldSetDataMap(f1, datamap=dm1, rc=rc)
      write(failMsg, *) "Did return ESMF_SUCCESS"
      write(name, *) "Setting a Field Data Map Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldDestroy(f1)
      !------------------------------------------------------------------------

      !E-X-_-U-T-e-s-t
      ! Verifing that destroying a Grid in a Field is not allowed
      ! TODO: we cannot tell that this has happened!  there is no way to
      ! prevent the user from doing this!!  comment this test back in after
      ! we have agreed as a group on the strategy for validation vs. the
      ! cost of doing it each time you call into the framework.
     !!call ESMF_GridDestroy(grid, rc=rc)
     !!write(failMsg, *) ""
     !!write(name, *) "Destroying a Grid in a Field Test"
     !!call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Verifing that a Field with a Grid and Array can be destroyed
      call ESMF_FieldDestroy(f3, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying a Field with a Grid and Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Verifing that destroying the Field did not destroy the Grid
       call ESMF_GridValidate(grid, rc=rc)
       write(failMsg, *) ""
       write(name, *) "Using a Grid after the Field is destroyed"
       call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Verifing that a Field cannot be created with an uninitialized Grid 
      ! and Array.  f3 is *not* created here and should be invalid.
      f3 = ESMF_FieldCreate(grid2, arr2, ESMF_DATA_REF, ESMF_CELL_CENTER, &
                            ESMF_CELL_CELL, 3, dm, "Field 0", ios, rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Field with an uninitialized Grid and Array Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Verify that a Grid cannot be gotten from a Field created with no data
      f5 = ESMF_FieldCreateNoData(rc=rc)
      call ESMF_FieldGet(f5, grid=grid3, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting a Grid from a Field created with no data Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldDestroy(f5, rc=rc)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Req. 1.6.2 Return grid 
      ! A field shall be able to return a reference to its grid.
      ! f3 gets created here and used thru the rest of the tests.
      gname="oceangrid"
      f3 = ESMF_FieldCreate(grid, arr, ESMF_DATA_REF, ESMF_CELL_CENTER, &
                            ESMF_CELL_CELL, 1, dm, "Field 0", ios, rc)
      call ESMF_FieldGet(f3, grid=grid3, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting a Grid from a Field created with no data Test"
      call ESMF_GridGet(grid, name=gname, rc=rc)
      call ESMF_GridGet(grid3, name=gname3, rc=rc)
      print *, "Grid (grid3) name = ", trim(gname3)
      call ESMF_Test((gname.eq.gname3), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Req. xxx - getting a data pointer directly from a field
      ! f3 exists and is valid at this point.
      nullify(f90ptr2)
      call ESMF_FieldGetDataPointer(f3, f90ptr2, rc=rc)
      print *, "data = ", f90ptr2(1,1)
      write(failMsg, *) ""
      write(name, *) "Getting an F90 pointer directly back from a Field"
      call ESMF_Test((associated(f90ptr2)), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Req. xxx - setting an array with the wrong halo size.
      ! this should fail to create.
      allocate(f90ptr3(cellCounts(1),cellCounts(2)))
      arr = ESMF_ArrayCreate(f90ptr3, ESMF_DATA_REF, haloWidth=3, rc=rc)
      f7 = ESMF_FieldCreate(grid, arr, ESMF_DATA_REF, ESMF_CELL_CENTER, &
                            ESMF_CELL_CELL, 3, dm, "Field 0", ios, rc)
      write(failMsg, *) ""
      write(name, *) "Setting the wrong size Array (no halo space) in Field"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      deallocate(f90ptr3, stat=rc)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Req. xxx - Creating a Field with halo regions, this should be ok.
      allocate(f90ptr3(cellCounts(1)+(3*2),cellcounts(2)+(3*2)))
      arr = ESMF_ArrayCreate(f90ptr3, ESMF_DATA_REF, haloWidth=3, rc=rc)
      f7 = ESMF_FieldCreate(grid, arr, ESMF_DATA_REF, ESMF_CELL_CENTER, &
                            ESMF_CELL_CELL, 3, dm, "Field 0", ios, rc)
      write(failMsg, *) ""
      write(name, *) "Creating Field with non-zero haloWidth"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Req. xxx - getting the haloWidth back from a field
      call ESMF_FieldGet(f7, haloWidth=i, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting haloWidth back from a Field"
      call ESMF_Test((i .eq. 3), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Verifing that a Field can be created with a name
      f2 = ESMF_FieldCreateNoData("pressure", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating Field with name Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f2)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Setting a (bad) data pointer directly in an empty Field
      nullify(f90ptr4)
      call ESMF_FieldSetDataPointer(f2, f90ptr4, rc=rc)
      write(failMsg, *) "Did not return ESMF_FAILURE"
      write(name, *) "Setting a null F90 pointer directly in a Field"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Setting a (good) data pointer directly in an empty Field
      allocate(f90ptr4(4,4))
      f90ptr4(:,:) = 3.14159
      call ESMF_FieldSetDataPointer(f2, f90ptr4, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an F90 pointer directly in a Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      ! Getting the data pointer back from a Field
      call ESMF_FieldGetDataPointer(f2, f90ptr4, rc=rc)
      print *, "data = ", f90ptr4(1,1)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting an F90 pointer directly back from a Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Bug 986852 opened
      ! Verify the pointers are equal
      call ESMF_FieldGetDataPointer(f2, f90ptr5, rc=rc)
      print *, "data = ", f90ptr5(1,1)
      write(failMsg, *) "The pointers are not equal"
      write(name, *) "Compare F90 pointers Test"
      call ESMF_Test((associated(f90ptr4,f90ptr5)), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Adding Attributes to a Field
      ! f3 exists and is valid at this point.
      call ESMF_FieldSetAttribute(f3, "Scale Factor", 4, rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding Attribute to a Field "
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Getting Attribute count from a Field
      call ESMF_FieldGetAttributeCount(f3, count, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Attribute count from a Field "
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      ! Verify Attribute Count Test
      write(failMsg, *) "Incorrect count"
      write(name, *) "Verify Attribute count from a Field "
      call ESMF_Test((count.eq.1), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      ! Getting Attrubute Info from a Field
      call ESMF_FieldGetAttributeInfo(f3, name="Scale Factor", count=count, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Attribute info from a Field "
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      ! Verify Attribute Count Test
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Verify Attribute count from a Field "
      call ESMF_Test((count.eq.1), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get Field Attribute Test
      intattr = 0
      call ESMF_FieldGetAttribute(f3, "Scale Factor", intattr, rc)
      write(failMsg, *) ""
      write(name, *) "Getting an Integer Attribute back from a Field"
      call ESMF_Test((intattr.eq.4), name, failMsg, result, ESMF_SRCLINE)
 
      !------------------------------------------------------------------------
      !EX_UTest
      ! test setting a second attribute
      call ESMF_FieldSetAttribute(f3, "Invalid Data Tag", -999, rc)
      call ESMF_FieldPrint(f3, rc=rc)
      intattr2 = 0
      call ESMF_FieldGetAttribute(f3, "Invalid Data Tag", intattr2, rc)
      print *, "Invalid Data Tag should be -999, is: ", intattr2
      write(failMsg, *) ""
      write(name, *) "Getting a second Integer Attribute back from a Field"
      call ESMF_Test((intattr2.eq.-999), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! getting a non-existant attribute
      call ESMF_FieldGetAttribute(f3, "No such attribute", intattr, rc)
      write(failMsg, *) ""
      write(name, *) "Getting an non-existant Integer Attribute from a Field"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! getting a non-existant attribute
      ! setting an integer list
      call ESMF_FieldSetAttribute(f3, "Multiple Scale Factors", 4, (/4,3,2,1/), rc)
      call ESMF_FieldPrint(f3, rc=rc)
      intattr = 0
      count = 4   ! expected number of values
      call ESMF_FieldGetAttribute(f3, "Multiple Scale Factors", count, intattrlist, rc)
      print *, count, "attributes found in list"
      write(failMsg, *) ""
      write(name, *) "Getting an Integer List Attribute back from a Field"
      call ESMF_Test((intattrlist(1).eq.4), name, failMsg, result, ESMF_SRCLINE)
 
      !------------------------------------------------------------------------
      !EX_UTest
      ! test setting a real attribute
      rattr = 3.14159
      call ESMF_FieldSetAttribute(f3, "Pi", 3.14159_ESMF_KIND_R8, rc)
      call ESMF_FieldPrint(f3, rc=rc)
      rattr = 0.0
      call ESMF_FieldGetAttribute(f3, "Pi", rattr, rc)
      print *, "Pi should be 3.14159, is: ", rattr
      write(failMsg, *) ""
      write(name, *) "Getting a real Attribute back from a Field"
      call ESMF_Test((rattr-3.14159.lt.0.00001), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! test setting a real list
      rattrlist = (/ 1.1, 2.2 /)
      call ESMF_FieldSetAttribute(f3, "Vertices", 2, rattrlist, rc)
      call ESMF_FieldPrint(f3, rc=rc)
      rattr = 0.0
      count = 2   ! expected count
      call ESMF_FieldGetAttribute(f3, "Vertices", count, rattrlist, rc)
      print *, count, "attributes found in list"
      print *, "Vertices should be 1.1 and 2.2, are: ", rattrlist
      write(failMsg, *) ""
      write(name, *) "Getting a real list Attribute back from a Field"
      call ESMF_Test((rattrlist(1).eq.1.1), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! test setting a logical attribute
      call ESMF_FieldSetAttribute(f3, "Sky is Blue", ESMF_TRUE, rc)
      call ESMF_FieldPrint(f3, rc=rc)
      lattr = ESMF_FALSE
      call ESMF_FieldGetAttribute(f3, "Sky is Blue", lattr, rc)
      call ESMF_LogicalString(lattr, lattrstr, rc)
      print *, "Sky is Blue  should be true, is: ", lattrstr
      write(failMsg, *) ""
      write(name, *) "Getting a logical Attribute back from a Field"
      call ESMF_Test((lattr.eq.ESMF_TRUE), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! test setting a logical list
      call ESMF_FieldSetAttribute(f3, "FlipFlop", 3, (/ESMF_TRUE,ESMF_FALSE,ESMF_TRUE/), rc)
      call ESMF_FieldPrint(f3, rc=rc)
      lattr = ESMF_FALSE
      count = 3   ! expected count
      call ESMF_FieldGetAttribute(f3, "FlipFlop", count, lattrlist, rc)
      print *, count, "attributes found in list"
      print *, "FlipFlop should be alternate, are: " 
      do i=1, 3
        call ESMF_LogicalString(lattrlist(i), lattrstr, rc)
        print *, i, lattrstr
      enddo
      write(failMsg, *) ""
      write(name, *) "Getting a logical Attribute back from a Field"
      call ESMF_Test((lattrlist(1).eq.ESMF_TRUE), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! test setting a character attribute
      cattr = "It was a dark and stormy night"
      call ESMF_FieldSetAttribute(f3, "Book", cattr, rc)
      call ESMF_FieldPrint(f3, rc=rc)
      call ESMF_FieldGetAttribute(f3, "Book", cattr2, rc)
      print *, "Book  should be drivel, is: ", trim(cattr2)
      write(failMsg, *) ""
      write(name, *) "Getting a character Attribute back from a Field"
      call ESMF_Test((cattr.eq.cattr2), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Requirement 1.2 Local memory layout 
      ! It shall be possible to specify whether the field data is row major 
      ! or column major at field creation and to rearrange it (assumes 
      ! local copy).
      ! Cannot be tested until Bug 705247 "Unable to query Data Map from Field" 
      ! is fixed.
      !EX_UTest
      call ESMF_FieldGet(f3, datamap=dm, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting a DataMap from a Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldDataMapPrint(dm, "", rc=rc)

      ! Requirement 1.3 Index Order
      ! It shall be possible to specify the index order of field data and 
      !  also to rearrange it.
      ! Cannot be tested until Bug 705308 "ESMF_FieldGetLocalDataInfo not 
      !  implemented" is fixed.

      ! Requirement 1.7.2 Query number of dimensions and index order
      ! A Field shall be able to return the number of dimensions and index 
      !  order it has.
      ! Cannot be tested until Bug 705308 "ESMF_FieldGetLocalDataInfo not 
      !  implemented" is fixed.

      ! Requirement 1.7.3 Query attributes
      ! A Field can return its list of attributes.
      ! Cannot be tested until Bug 705716 "Field Query attributes not 
      !  implemented" is fixed.

#endif

      call ESMF_TestEnd(result, ESMF_SRCLINE)
      call ESMF_Finalize(rc)

      print *, "******  End of FieldUTest  ******"

      end program ESMF_FieldUTest
