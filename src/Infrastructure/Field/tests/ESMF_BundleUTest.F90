! $Id: ESMF_BundleUTest.F90,v 1.14 2003/04/22 20:58:51 svasquez Exp $
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
      program ESMF_BundleUTest

!------------------------------------------------------------------------------
!
#include <ESMF_Macros.inc>

!==============================================================================
!BOP
! !PROGRAM: ESMF_BundleTest - Bundle Unit Tests
!
! !DESCRIPTION:
!
! The code in this file drives F90 Bundle unit tests.
! The companion file ESMF\_Bundle.F90 contains the definitions for the
! Bundle methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod

      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_BundleUTest.F90,v 1.14 2003/04/22 20:58:51 svasquez Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0
!     ! Local variables
      integer :: i, x, y, rc, mycell, fieldcount
      type(ESMF_Grid) :: grid, grid2
      type(ESMF_ArraySpec) :: arrayspec
      type(ESMF_Array) :: arraya, arrayb
      type(ESMF_DataMap) :: datamap
      type(ESMF_RelLoc) :: relativelocation
      character (len = ESMF_MAXSTR) :: bname1, bname2, fname1, fname2, fname3
      type(ESMF_IOspec) :: iospec
      type(ESMF_Field) :: field(10), returnedfield1, returnedfield2, returnedfield3, simplefield
      type(ESMF_Bundle) :: bundle1, bundle2, bundle3, bundle4
      real (selected_real_kind(6,45)), dimension(:,:), pointer :: f90ptr1, f90ptr2



      ! individual test result code

      ! individual test name
      character(ESMF_MAXSTR) :: named


      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name


      ! instantiate a Bundle 
      type(ESMF_Bundle) :: bundle

!--------------------------------------------------------------------------------
!     The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!     always run. When the environment variable, EXHAUSTIVE, is set to ON then
!     the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!     to OFF, then only the sanity unit tests.
!--------------------------------------------------------------------------------

      !------------------------------------------------------------------------

      ! Verify that getting the name of an uninitialized Bundle is handled properly.
      call ESMF_BundleGetName(bundle1, bname1, rc)
      write(failMsg, *) "Subroutine should have returned ESMF_FAILURE"
      write(name, *) "Getting name of uninitalized Bundle Test"
      call ESMF_Test((rc.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
     
#ifdef ESMF_EXHAUSTIVE

      ! The following code is commented out because it crashes the program
      ! It will be uncommented when the bug is fixed
      !  Verify that the Field count query from an uninitialized Bundle is handled properly
      !call ESMF_BundleGetFieldCount(bundle1, fieldcount, rc);
      !write(failMsg, *) ""
      !write(name, *) "Getting Field count from an uninitialized Bundle Test"
      !call ESMF_Test((rc.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

#endif
      ! Test Requirement FLD2.1.1 Creation using Field list
      ! It shall be possible to create a bundle with a field list, an optional 
      ! I/O specification, and an identifier that specifies whether the bundle 
      ! is to be packed (contiguous data) or loose (noncontiguous data). 
      !  Create several empty Fields and add them to a new Bundle.
      field(1) = ESMF_FieldCreateNoData(name="pressure", rc=rc)
      field(2) = ESMF_FieldCreateNoData(name="temperature", rc=rc)
      field(3) = ESMF_FieldCreateNoData(name="heat flux", rc=rc)
      bundle1 = ESMF_BundleCreate(3, field, name="atmosphere data", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating Bundle with 3 No Data Fields Test Req. FLD2.1.1"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! Test Requirement FLD2.1.1 Creating a Bundle with ESMF_PACK_FIELD_DATA option
      bundle1 = ESMF_BundleCreate(3, field, ESMF_PACK_FIELD_DATA, &
				name="atmosphere data", rc=rc)
      write(name, *) "Creating Bundle with ESMF_PACK_FIELD_DATA Req. FLD2.1.1"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

#ifdef ESMF_EXHAUSTIVE

      ! Verify that adding a Field to an uninitialized Bundle is handled correctly
      ! This code is commented out because it crashes the program.
      !It will be uncommented when the bug is fixed
      !call ESMF_BundleAddFields(bundle2, simplefield, rc=rc);
      !write(failMsg, *) "Add Field to uncreated Bundle failed"
      !write(name, *) "Adding a Field to an uncreated Bundle Test"
      !call ESMF_Test((rc.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !  Verify that a Field can be added to a Bundle
      ! The following code s commented out because it crashes. Bug 703872
      ! simplefield = ESMF_FieldCreate(grid, arrayspec, relloc=ESMF_CELL_CENTER, &
                                    ! name="rh", rc=rc)

      !  Verify that an empty Bundle can be created
      ! bundle2 = ESMF_BundleCreate(name="time step 1", rc=rc);
      ! write(failMsg, *) ""
      ! write(name, *) "Creating Empty Bundle Test"
      ! call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

#endif

      !  Verify that recreating a created Bundle is handled properly
      bundle2 = ESMF_BundleCreate(name="time step 1", rc=rc);
      write(failMsg, *) ""
      write(name, *) "Creating a Bundle that has already been created Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

#ifdef ESMF_EXHAUSTIVE

      !  Verify that the Field count query from an empty Bundle is handled properly
      call ESMF_BundleGetFieldCount(bundle2, fieldcount, rc);
      write(failMsg, *) "Returned ESMF_FAILURE or field count not equal to zero"
      write(name, *) "Getting Field count from an empty Bundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(fieldcount.eq.0), name, failMsg, result, ESMF_SRCLINE)
      print *, "Field count of empty Bundle = ", fieldcount
      !------------------------------------------------------------------------

#endif

      !  Verify that a Field can be added to an empty Bundle
      call ESMF_BundleAddFields(bundle2, simplefield, rc=rc);
      write(failMsg, *) ""
      write(name, *) "Adding a Field to an Empty Bundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !  Verify that the Field count can be queried from a Bundle
      call ESMF_BundleGetFieldCount(bundle2, fieldcount, rc);
      write(failMsg, *) ""
      write(name, *) "Getting Field count from a Bundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(fieldcount.eq.1), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

#ifdef ESMF_EXHAUSTIVE

      !  Test Requirement FLD2.5.7 Return Grid
      ! Commented out until Bug 707065 is fixed 
      !call ESMF_BundleGetGrid(bundle2, grid2, rc);
      !write(failMsg, *) ""
      !write(name, *) "Getting a Grid from a Bundle Test Req. FLD2.5.7"
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

#endif

      ! Create an empty Bundle and then add multiple fields to it.
      bundle3 = ESMF_BundleCreate(name="southern hemisphere", rc=rc);
      bundle3 = ESMF_BundleCreate(name="northern hemisphere", rc=rc);
   
      !  Verify that multiple Fields can be added to a Bundle 
      call ESMF_BundleAddFields(bundle3, 3, field, rc);
      write(failMsg, *) ""
      write(name, *) "Adding multiple Fields to a Bundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !  Verify that Fields count can be queried from a Bundle 
      call ESMF_BundleGetFieldCount(bundle3, fieldcount, rc);
      write(failMsg, *) "Returned ESMF_FAILURE or field count not equal to three"
      write(name, *) "Getting Field count from a Bundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(fieldcount.eq.3), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! Verify that the first Field names can be queried fron a Bundle
      call ESMF_BundleGetFields(bundle1, "pressure", returnedfield1, rc)
      write(failMsg, *) ""
      write(name, *) "Getting first Field by name from a Bundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldGetName(returnedfield1, fname1, rc)
      write(failMsg, *) "Subroutine returned ESMF_FAILURE or incorrect name returned"
      write(name, *) "Getting first Field from a Bundle Test continued"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(fname1.eq."pressure"), name, &
							failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! Verify that the second Field names can be queried fron a Bundle
      call ESMF_BundleGetFields(bundle1, 2, returnedfield2, rc)
      write(failMsg, *) ""
      write(name, *) "Getting a second Field by index from a Bundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldGetName(returnedfield2, fname2, rc)
      write(failMsg, *) "Subroutine returned ESMF_FAILURE or incorrect name returned"
      write(name, *) "Getting a second Field from a Bundle Test continued"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(fname2.eq."temperature"), name, &
						failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------


      ! Verify that the third Field names can be queried fron a Bundle
      call ESMF_BundleGetFields(bundle1, 3, returnedfield3, rc)
      write(failMsg, *) ""
      write(name, *) "Getting a third Field by index from a Bundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldGetName(returnedfield3, fname3, rc)
      write(failMsg, *) "Subroutine returned ESMF_FAILURE or incorrect name returned"
      write(name, *) "Getting a third Field from a Bundle Test continued"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(fname3.eq."heat flux"), name, &
						failMsg, result, ESMF_SRCLINE)
      print *, "Bundle returned, field names = ", &
                    trim(fname1), ", ", trim(fname2), ", ", trim(fname3)
      !------------------------------------------------------------------------

      ! Verify that the fourth Field names cannot be queried fron a Bundle
      ! because there are ony three Fields in the Bundle
      call ESMF_BundleGetFields(bundle1, 4, returnedfield3, rc)
      write(failMsg, *) ""
      write(name, *) "Getting a non-existent Field from a Bundle Test"
      call ESMF_Test((rc.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

#ifdef ESMF_EXHAUSTIVE

      ! Verify that the zeroth Field names cannot be queried fron a Bundle
      call ESMF_BundleGetFields(bundle1, 0, returnedfield3, rc)
      write(failMsg, *) ""
      write(name, *) "Getting a zeroth non-existent Field from a Bundle Test"
      call ESMF_Test((rc.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! Verify that the querying Field with wrong name from a Bundle returns FAILURE
      call ESMF_BundleGetFields(bundle1, "nressure", returnedfield1, rc)
      write(failMsg, *) ""
      write(name, *) "Getting wrong Field name from a Bundle Test"
      call ESMF_Test((rc.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

#endif

      ! Verify that the Bundle name can be queried 
      call ESMF_BundleGetName(bundle1, bname1, rc)
      write(failMsg, *) ""
      write(name, *) "Query Bundle Name Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bname1.eq."atmosphere data"), &
				name, failMsg, result, ESMF_SRCLINE)
      print *, "Bundle name = ", trim(bname1)
      !------------------------------------------------------------------------

      ! Test Requirement FLD2.4 Deletion
      ! Bundles may be deleted. Data allocated by and included in packed bundles 
      ! is deleted along with the bundle. Pointers to field data in unpacked 
      ! bundles are returned at deletion. 
      call ESMF_BundleDestroy(bundle1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Bundle Destroy Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, "rc = ", (rc)
      !------------------------------------------------------------------------

#ifdef ESMF_EXHAUSTIVE

      ! Verify that destroying a destroyed Bundle is handled correctly
      call ESMF_BundleDestroy(bundle1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying a Destroyed Bundle Test"
      call ESMF_Test((rc.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)
      print *, "rc = ", (rc)
      !------------------------------------------------------------------------

      ! Requirement 2.5.2 Insert and remove Field
      ! A Field can be inserted into or removed from a Bundle
      ! The remove portion of this requirement cannot be tested until Bug 705849
      ! ESMF_BundleDeleteField not implemented" is fixed.
      !------------------------------------------------------------------------

#endif


      end program ESMF_BundleUTest
