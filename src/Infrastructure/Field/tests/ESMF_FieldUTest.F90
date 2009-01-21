! $Id: ESMF_FieldUTest.F90,v 1.106.2.21 2009/01/21 21:25:20 cdeluca Exp $
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
      program ESMF_FieldUTest

!------------------------------------------------------------------------------
 
#include "ESMF.h"

!==============================================================================
!BOPI
! !PROGRAM: ESMF_FieldTest - One line general statement about this test
!
! !DESCRIPTION:
!
! The code in this file drives F90 Field unit tests.
! The companion file ESMF\_Field.F90 contains the definitions for the
! Field methods.
!EOPI
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod
      use ESMF_FieldGetMod

      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_FieldUTest.F90,v 1.106.2.21 2009/01/21 21:25:20 cdeluca Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc = 1

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

!     !LOCAL VARIABLES:
      integer :: i, count
      integer, dimension(3) :: cellCounts
      type(ESMF_DELayout) :: delayout
      type(ESMF_VM) :: vm
      type(ESMF_Grid) :: grid, grid2, grid3, grid4, grid5
      type(ESMF_ArraySpec) :: arrayspec
      real, dimension(:,:), pointer :: f90ptr2, f90ptr4, f90ptr5
      real(ESMF_KIND_R8) :: minCoord(2), deltas(10)
      !type(ESMF_RelLoc) :: rl
      character (len = 20) :: fname, fname1, fname2
      character (len = 20) :: gname, gname3
      type(ESMF_IOSpec) :: ios
      !type(ESMF_Mask) :: mask
      type(ESMF_Field) :: f1, f2, f3, f4, f5, f6, f7, f8, f9
      integer(ESMF_KIND_I4) :: intattr, intattr2
      integer(ESMF_KIND_I4) :: intattrlist(6)
      real(ESMF_KIND_R8) :: rattr, rattrlist(2)
      character (len=32) :: lattrstr
      type(ESMF_Logical) :: lattr, lattrlist(6)
      character (len=512) :: cattr, cattr2
      real(ESMF_KIND_R8), dimension(:,:), pointer :: farray
      real(ESMF_KIND_R8), dimension(:,:), pointer :: farray1, farray2
      type(ESMF_Array)                            :: array8
      type(ESMF_ArraySpec)                        :: arrayspec8
      type(ESMF_DistGrid)                         :: distgrid
      type(ESMF_StaggerLoc)                       :: staggerloc8

      integer :: im, jm, km, ldecount
      real(ESMF_KIND_R8) :: xmin,xmax,ymin,ymax
      real(ESMF_KIND_R8), dimension(:), allocatable :: delta
      real(ESMF_KIND_R8), dimension(:,:,:), pointer :: fptr

!-------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then 
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------- 
      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)


      !------------------------------------------------------------------------
      ! several calls to field need a valid grid.  these will be used in
      ! the grid create calls.
      call ESMF_VMGetGlobal(vm, rc=rc)
      delayout = ESMF_DELayoutCreate(vm, rc=rc)
      minCoord(:) = (/ 0.0, 0.0 /)


      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Test Requirement FLD1.1.3 Creation without data 
      ! Fields may be created as in FLD1.1.1 without allocating data or 
      ! specifying an associated data array. In this case specifying the 
      ! grid parameters and data array dimensions may be deferred until 
      ! data is attached.
      f1 = ESMF_FieldCreateEmpty(rc=rc) 
      write(failMsg, *) ""
      write(name, *) "Creating a Field with no data"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Verifying that a Field with no data can be destroyed
      call ESMF_FieldDestroy(f1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying a Field with no data Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_TESTEXHAUSTIVE

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verifying that a Field that has not been created doesn't
      ! crash when it is destroyed 
      call ESMF_FieldDestroy(f2, rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Destroying a Field not created"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_remove_UTest_Multi_Proc_Only
      ! Get a name from a deleted created Field
!      call ESMF_FieldGet(f1, name=fname, rc=rc)
!      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED "
!      write(name, *) "Getting name of deleted Field"
!      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Get a name from a non-created Field
      call ESMF_FieldGet(f2, name=fname, rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Getting name of uncreated Field"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      f1 = ESMF_FieldCreateEmpty(rc=rc) 
      write(failMsg, *) ""
      write(name, *) "Creating a Field with no data Test Req. FLD1.1.3"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!      !------------------------------------------------------------------------
!      !E-X_UTest_Multi_Proc_Only
!      ! Verifying that an initialized Field can be printed
!      call ESMF_FieldPrint(f1, rc=rc)
!      write(failMsg, *) ""
!      write(name, *) "Printing an initialized Field with no data Test"
!      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verifying that the Field name can be queried from a no data Field
      call ESMF_FieldGet(f1, name=fname, rc=rc)
      write(failMsg, *) "default name not generated"
      write(name, *) "Getting name of Field with no data Test"
      call ESMF_Test((fname.ne.""), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Getting Attribute count from a Field
      call ESMF_AttributeGet(f1, count, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Attribute count from a Field "
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verify attribute count test
      write(failMsg, *) "Incorrect count"
      write(name, *) "Verify Attribute count from a Field "
      call ESMF_Test((count.eq.0), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! default names unique
      f2 = ESMF_FieldCreateEmpty(rc=rc)
      call ESMF_FieldGet(f1, name=fname1, rc=rc)
      call ESMF_FieldGet(f2, name=fname2, rc=rc)
      call ESMF_FieldPrint(f1)
      call ESMF_FieldPrint(f2)
      write(failMsg, *) "default name not unique"
      write(name, *) "Verifying uniqueness of fields created default name"
      call ESMF_Test((fname1.ne.fname2), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verifying that a Field with no data can be destroyed
      call ESMF_FieldDestroy(f2, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying a Field with no data Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Test Requirement  
      ! Fields may be deleted.
      call ESMF_FieldDestroy(f1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying initialized Field Test Req. FLD1.4"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verifying that printing an uninitialized Field is handled properly.
      call ESMF_FieldPrint(f6, rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Printing an uninitialized Field Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! verify that querying the name of a destroyed Field is handled properly.
      call ESMF_FieldGet(f1, name=fname, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting name of a destroyed Field Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verifying that printing a destroyed Field is handled properly.
      call ESMF_FieldPrint(f1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing destroyed Field Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verifying that a Field can be created with a name
      f2 = ESMF_FieldCreateEmpty("pressure", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating Field with name Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verifying that the Field name can be queried.
      Call ESMF_FieldGet(f2, name=fname, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting a Field name Test"
      call ESMF_Test((fname.eq."pressure"), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f2)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verifying that recreating a Field is allowed.
      f2 = ESMF_FieldCreateEmpty("temperature", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Recreate a created Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f2)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verifying that a Field can be created after it has been destroyed
      call ESMF_FieldDestroy(f2)
      f2 = ESMF_FieldCreateEmpty("precipitation", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Recreate a destroyed Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldPrint(f2)
      call ESMF_FieldDestroy(f2)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verifying that a Grid can be created
      grid = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=(/10,20/), &
                                  regDecomp=(/2,2/), name="landgrid", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Grid to use in Field Tests"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     
      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Test requirement Default name attribute 
      ! The only default attribute of a field will be a name. A unique 
      ! name will be generated if not supplied by the user.
      ! Test Requirement Query name
      ! A field shall be able to easily return its name. 
      ! If the user does not provide a field name one will be created. 
      ! Field names must be unique within an address 
      ! space and it shall be possible to check this.
      ! Bug 705087 "Default Field names not unique"
      f1 = ESMF_FieldCreateEmpty(rc=rc)
      f2 = ESMF_FieldCreateEmpty(rc=rc)
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
      !EX_UTest_Multi_Proc_Only
      f4 = ESMF_FieldCreateEmpty(rc=rc) 
      write(failMsg, *) ""
      write(name, *) "Creating a Field with no data"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!      !------------------------------------------------------------------------
!      !EremoveX_UTest_Multi_Proc_Only
!      ! Setting a Grid associated with Field
!      call ESMF_FieldSet(f4, grid, rc=rc) 
!      write(failMsg, *) "Did not return ESMF_SUCCESS"
!      write(name, *) "Setting a Grid associated with Field Test"
!      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      call ESMF_FieldValidate(f4, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Testing to see if Field is Valid"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verifying that a Field with a Grid and Array can be destroyed
      call ESMF_FieldDestroy(f4, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying a Field with a Grid and Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Fields may be created by specifying attributes, a grid, data array 
      ! dimensions and descriptors, optional masks (e.g. for active cells), 
      ! and an optional I/O specification. In this case a field will 
      ! allocate its own data. The grid passed into the argument list 
      ! is referenced and not copied.
      call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R4, rc=rc)
      write(name, *) "Creating an ArraySpec Test "
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      f2 = ESMF_FieldCreate(grid, arrayspec, ESMF_INDEX_DELOCAL, &
                                          staggerloc=ESMF_STAGGERLOC_CENTER, &
                                          name="rh", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a Field with a Grid and ArraySpec Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verifying that a destroying a destroyed  Field is handled properly.
      call ESMF_FieldDestroy(f2, rc=rc)  ! should succeed, f2 exists
      call ESMF_FieldDestroy(f2, rc=rc)  ! should fail
      write(failMsg, *) ""
      write(name, *) "Destroying a destroyed Field Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! verify we can create a 3d data on a 2d grid
      call ESMF_ArraySpecSet(arrayspec, 3, ESMF_TYPEKIND_R4, rc=rc)
      f2 = ESMF_FieldCreate(grid, arrayspec, ESMF_INDEX_DELOCAL, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            ungriddedLBound=(/1/), ungriddedUBound=(/10/), &
            name="rh", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a Field with a 2d Grid and 3d ArraySpec"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldDestroy(f2)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Creation with external data 
      ! Fields may be created as in FLD1.1.1 with a data array passed into 
      ! the argument list. The data array is referenced and not copied.
      ! Verifying that a Field can be created with a Grid
      call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R4, rc=rc)
      f3 = ESMF_FieldCreate(grid, arrayspec, ESMF_INDEX_DELOCAL, &
                        staggerloc=ESMF_STAGGERLOC_CENTER, &
                        name="Field 0", iospec=ios, rc=rc)

      write(failMsg, *) ""
      write(name, *) "Creating a Field with a Grid and Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      call ESMF_FieldValidate(f3, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Testing to see if Field is Valid"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldDestroy(f3)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verifying that destroying the Field did not destroy the Grid
       call ESMF_GridValidate(grid, rc=rc)
       write(failMsg, *) ""
       write(name, *) "Using a Grid after the Field is destroyed"
       call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verifying that a Field cannot be created with an uninitialized Grid 
      ! and Array.  f6 is *not* created here and should be invalid.
      grid2 = ESMF_GridCreateEmpty(rc=rc)
      call ESMF_GridDestroy(grid2, rc=rc)
      f6 = ESMF_FieldCreate(grid2, arrayspec, ESMF_INDEX_DELOCAL, &
                        staggerloc=ESMF_STAGGERLOC_CENTER, &
                        name="Field 0", iospec=ios, rc=rc)

      write(failMsg, *) ""
      write(name, *) "Creating a Field with an uninitialized Grid and Array"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verify that a Grid cannot be gotten from a Field created with no data
      f5 = ESMF_FieldCreateEmpty(rc=rc)
      call ESMF_FieldGet(f5, grid=grid3, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting a Grid from a Field created with no data Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldDestroy(f5, rc=rc)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Return grid 
      ! A field shall be able to return a reference to its grid.
      ! f3 gets created here and used thru the rest of the tests.
      gname="oceangrid"
      call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R4, rc=rc)
      f3 = ESMF_FieldCreate(grid, arrayspec, ESMF_INDEX_DELOCAL, &
                        staggerloc=ESMF_STAGGERLOC_CENTER, &
                        name="Field 0", iospec=ios, rc=rc)
      call ESMF_FieldGet(f3, grid=grid3, localDeCount=ldecount, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting a Grid from a Field created with no data Test"
      call ESMF_GridGet(grid, name=gname, rc=rc)
      call ESMF_GridGet(grid3, name=gname3, rc=rc)
      print *, "Grid (grid3) name = ", trim(gname3)
      call ESMF_Test((gname.eq.gname3), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only 
      ! ESMF_ArraySpecPrint test ArraySpecPrint public interface
      call ESMF_ArraySpecSet(arrayspec8, 2, ESMF_TYPEKIND_R8, rc=rc)
      call ESMF_ArraySpecPrint(arrayspec8, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Test ESMF_ArraySpecPrint public interface"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only 
      ! ESMF_StaggerLocPrint test StaggerLoc public interface
      call ESMF_StaggerLocPrint(staggerloc8, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Test ESMF_StaggerLocPrint public interface"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Adding Attributes to a Field
      ! f3 exists and is valid at this point.
      call ESMF_AttributeSet(f3, "Scale Factor", 4, rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding Attribute to a Field "
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Getting Attribute count from a Field
      call ESMF_AttributeGet(f3, count, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Attribute count from a Field "
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verify Attribute Count Test
      write(failMsg, *) "Incorrect count"
      write(name, *) "Verify Attribute count from a Field "
      call ESMF_Test((count.eq.1), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Getting Attribute Info from a Field
      call ESMF_AttributeGet(f3, name="Scale Factor", count=count, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Attribute info from a Field "
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Verify Attribute Count Test
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Verify Attribute count from a Field "
      call ESMF_Test((count.eq.1), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! Get Field Attribute Test
      intattr = 0
      call ESMF_AttributeGet(f3, "Scale Factor", intattr, rc)
      write(failMsg, *) ""
      write(name, *) "Getting an Integer Attribute back from a Field"
      call ESMF_Test((intattr.eq.4), name, failMsg, result, ESMF_SRCLINE)
 
      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! test setting a second attribute
      call ESMF_AttributeSet(f3, "Invalid Data Tag", -999, rc)
      call ESMF_FieldPrint(f3, rc=rc)
      intattr2 = 0
      call ESMF_AttributeGet(f3, "Invalid Data Tag", intattr2, rc)
      print *, "Invalid Data Tag should be -999, is: ", intattr2
      write(failMsg, *) ""
      write(name, *) "Getting a second Integer Attribute back from a Field"
      call ESMF_Test((intattr2.eq.-999), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! getting a non-existent attribute
      print *, "ready to call no attr"
      call ESMF_AttributeGet(f3, "No such attribute", intattr, rc)
      print *, "back from no attr"
      write(failMsg, *) ""
      write(name, *) "Getting an non-existant Integer Attribute from a Field"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, "done with test"

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! getting a non-existent attribute
      ! setting an integer list
      call ESMF_AttributeSet(f3, "Multiple Scale Factors", 4, (/4,3,2,1/), rc)
      call ESMF_FieldPrint(f3, rc=rc)
      intattr = 0
      count = 4   ! expected number of values
      call ESMF_AttributeGet(f3, "Multiple Scale Factors", count, intattrlist, rc)
      print *, count, "attributes found in list"
      write(failMsg, *) ""
      write(name, *) "Getting an Integer List Attribute back from a Field"
      call ESMF_Test((intattrlist(1).eq.4), name, failMsg, result, ESMF_SRCLINE)
 
      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! test setting a real attribute
      rattr = 3.14159
      call ESMF_AttributeSet(f3, "Pi", 3.14159_ESMF_KIND_R8, rc)
      call ESMF_FieldPrint(f3, rc=rc)
      rattr = 0.0
      call ESMF_AttributeGet(f3, "Pi", rattr, rc)
      print *, "Pi should be 3.14159, is: ", rattr
      write(failMsg, *) ""
      write(name, *) "Getting a real Attribute back from a Field"
      call ESMF_Test((rattr-3.14159.lt.0.00001), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! test setting a real list
      rattrlist = (/ 1.1, 2.2 /)
      call ESMF_AttributeSet(f3, "Vertices", 2, rattrlist, rc)
      call ESMF_FieldPrint(f3, rc=rc)
      rattr = 0.0
      count = 2   ! expected count
      call ESMF_AttributeGet(f3, "Vertices", count, rattrlist, rc)
      print *, count, "attributes found in list"
      print *, "Vertices should be 1.1 and 2.2, are: ", rattrlist
      write(failMsg, *) ""
      write(name, *) "Getting a real list Attribute back from a Field"
      call ESMF_Test((rattrlist(1).eq.1.1), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! test setting a logical attribute
      call ESMF_AttributeSet(f3, "Sky is Blue", ESMF_TRUE, rc)
      call ESMF_FieldPrint(f3, rc=rc)
      lattr = ESMF_FALSE
      call ESMF_AttributeGet(f3, "Sky is Blue", lattr, rc)
      call ESMF_LogicalString(lattr, lattrstr, rc)
      print *, "Sky is Blue  should be true, is: ", lattrstr
      write(failMsg, *) ""
      write(name, *) "Getting a logical Attribute back from a Field"
      call ESMF_Test((lattr.eq.ESMF_TRUE), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest_Multi_Proc_Only
      ! test setting a logical list
      call ESMF_AttributeSet(f3, "FlipFlop", 3, (/ESMF_TRUE,ESMF_FALSE,ESMF_TRUE/), rc)
      call ESMF_FieldPrint(f3, rc=rc)
      lattr = ESMF_FALSE
      count = 3   ! expected count
      call ESMF_AttributeGet(f3, "FlipFlop", count, lattrlist, rc)
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
      !EX_UTest_Multi_Proc_Only
      ! test setting a character attribute
      cattr = "It was a dark and stormy night"
      call ESMF_AttributeSet(f3, "Book", cattr, rc)
      call ESMF_FieldPrint(f3, rc=rc)
      call ESMF_AttributeGet(f3, "Book", cattr2, rc)
      print *, "Book  should be drivel, is: ", trim(cattr2)
      write(failMsg, *) ""
      write(name, *) "Getting a character Attribute back from a Field"
      call ESMF_Test((cattr.eq.cattr2), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldDestroy(f3)

      !------------------------------------------------------------------------
      ! Local memory layout 
      ! It shall be possible to specify whether the field data is row major 
      ! or column major at field creation and to rearrange it (assumes 
      ! local copy).
      !EX_remove_UTest_Multi_Proc_Only

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

! grid destroy to clear previous grids.
       call ESMF_GridDestroy(grid, rc=rc)

#if 0
      !------------------------------------------------------------------------
      ! ESMF 3D Field Validate test to accommodate the single point mismatch 
      ! between center and edge staggerings. This Bug is actually a igrid design 
      ! issue having to do with the igrid being specified by vertex locations.

      ! define igrid dimensions
       im = 18
       jm = 36
       km = 72
      ! build uniform global igrid in degrees
       xmin = 0.0
       xmax = 360.0
       ymin =-90.0
       ymax = 90.0
       igrid = ESMF_IGridCreateHorzLatLonUni(counts=(/im,jm/),         &
                     minGlobalCoordPerDim=(/xmin,ymin/),             &
                     maxGlobalCoordPerDim=(/xmax,ymax/),             &
                     horzStagger=ESMF_IGRID_HORZ_STAGGER_A,           &
                     periodic=(/ ESMF_TRUE, ESMF_FALSE /),           &
                     name="A-igrid", rc=rc)

      ! construct vertical coordinate and add to height to igrid
       allocate( delta(km) )
       delta(1:km) = 1.0
       call ESMF_IGridAddVertHeight( igrid, delta=delta,               &
              vertStagger=ESMF_IGRID_VERT_STAGGER_TOP, rc=rc)
       deallocate( delta )

      ! distribute first two dimensions of the igrid
       call ESMF_IGridDistribute(igrid, delayout=delayout,             &
                                decompIds=(/1,2,0/), rc=rc)

      ! Get local IGrid counts now for use later
       call ESMF_IGridGetDELocalInfo(igrid, ESMF_STAGGERLOC_CENTER,          &
                 Vertrelloc=ESMF_CELL_TOPFACE,                       &
                 localCellCountPerDim=cellCounts, rc=rc)
      !EX_remove_UTest_Multi_Proc_Only
       write(failMsg, *) ""
       write(name, *) "Getting cell counts for each DE"
       call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! Create the test field from a fortran pointer  
       allocate( fptr(cellCounts(1),cellCounts(2),cellCounts(3)+1) )  
       print*,'cellcounts',cellCounts
       f1 = ESMF_FieldCreate(igrid, fptr, ESMF_DATA_REF,              &
                 horzRelloc=ESMF_STAGGERLOC_CENTER,                   &
                 vertRelloc=ESMF_CELL_TOPFACE,                       &
                 name="field", rc=rc)
       print*,'field create',rc
      !EX_remove_UTest_Multi_Proc_Only
       write(failMsg, *) ""
       write(name, *) "Create Field with vertical axis longer than igrid"
       call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! validate field
       call ESMF_FieldValidate(f1, rc=rc)
       print*,'field create',rc,ESMF_SUCCESS
      !EX_remove_UTest_Multi_Proc_Only
       write(failMsg, *) ""
       write(name, *) "Field Validated "
       call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! clean up
       deallocate( fptr )
       call ESMF_FieldDestroy(f1, rc)

#endif

#endif

      call ESMF_TestEnd(result, ESMF_SRCLINE)

      end program ESMF_FieldUTest
