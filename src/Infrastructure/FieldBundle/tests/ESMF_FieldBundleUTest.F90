! $Id: ESMF_FieldBundleUTest.F90,v 1.1.2.10 2009/01/21 21:25:21 cdeluca Exp $
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
      program ESMF_FieldBundleUTest

!------------------------------------------------------------------------------
!
#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_FieldBundleTest - FieldBundle Unit Tests
!
! !DESCRIPTION:
!
! The code in this file drives F90 FieldBundle unit tests.
! The companion file ESMF\_FieldBundle.F90 contains the definitions for the
! FieldBundle methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod

      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_FieldBundleUTest.F90,v 1.1.2.10 2009/01/21 21:25:21 cdeluca Exp $'
!------------------------------------------------------------------------------

!     ! Local variables
      integer :: rc, fieldcount, count, countlist(2)
      integer :: number, i
      type(ESMF_Grid) :: grid, grid2
      type(ESMF_VM) :: vm
      character (len = ESMF_MAXSTR) :: bname1, fname1, fname2, fname3
      character(len = ESMF_MAXSTR), dimension(10) :: fieldNameList
      type(ESMF_Field) :: fields(10)
      type(ESMF_Field) :: returnedfield1, returnedfield2, returnedfield3
      type(ESMF_Field) :: simplefield
      type(ESMF_FieldBundle) :: bundle1, bundle2, bundle3, bundle4
      real (ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr2
      real (ESMF_KIND_R8) :: mincoord(2)


      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name


!-------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------

      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

      !NEX_UTest
      !  Verify that an empty FieldBundle can be created
      bundle2 = ESMF_FieldBundleCreate(name="time step 1", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating Empty FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! Test Requirement - Deletion
      ! FieldBundles may be deleted. Data allocated by and included in packed bundles
      ! is deleted along with the bundle. Pointers to field data in unpacked 
      ! bundles are returned at deletion. 
      call ESMF_FieldBundleDestroy(bundle2, rc=rc)
      write(failMsg, *) ""
      write(name, *) "FieldBundle Destroy Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_TESTEXHAUSTIVE

      !------------------------------------------------------------------------
      !EX_UTest
      ! FieldBundle destroy of destroyed FieldBundle
      call ESMF_FieldBundleDestroy(bundle2, rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Destroy of destroyed FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! FieldBundle destroy of non-created FieldBundle
      call ESMF_FieldBundleDestroy(bundle1, rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Destroy of non-created FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Verify getting the name of a non-created  FieldBundle
      call ESMF_FieldBundleGet(bundle2, name=bname1, rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Getting name of deleted FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Verify getting the name of a non-created FieldBundle
      call ESMF_FieldBundleGet(bundle1, name=bname1, rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Getting name of non-create FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
! Think these next two should really be called with a valid grid
      !EX_removeUTest
      ! Set Grid in deleted FieldBundle Test
!      call ESMF_FieldBundleSetGrid(bundle2, grid, rc=rc)
!      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
!      write(name, *) "Set Grid in deleted FieldBundle  Test"
!      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Set Grid in non-created FieldBundle Test
!      call ESMF_FieldBundleSetGrid(bundle1, grid, rc=rc)
!      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
!      write(name, *) "Set Grid in non-created FieldBundle  Test"
!      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Getting Attribute count from a deleted FieldBundle
      call ESMF_AttributeGet(bundle2, count, rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Getting Attribute Count from a deleted FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Getting Attribute count from a non-created FieldBundle
      call ESMF_AttributeGet(bundle1, count, rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Getting Attribute Count from a non-created FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Getting a third Field name from a deleted FieldBundle
      call ESMF_FieldBundleGet(bundle2, 3, returnedfield3, rc)
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Getting a third Field by index from a deleted FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Getting a third Field name from a non-created FieldBundle
      call ESMF_FieldBundleGet(bundle1, 3, returnedfield3, rc)
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Getting a third Field by index from a non-create FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Validate a deleted FieldBundle Test
      call ESMF_FieldBundleValidate(bundle2, rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Validating a deleted FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------
      !EX_UTest
      ! Validate a non-created FieldBundle Test
      call ESMF_FieldBundleValidate(bundle1, rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Validating a non-created FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Requirement Deletion
      ! Verify getting the name of an uninitialized FieldBundle is handled properly.
      call ESMF_FieldBundleGet(bundle1, name=bname1, rc=rc)
      write(failMsg, *) "Subroutine should have returned ESMF_FAILURE"
      write(name, *) "Getting name of uninitalized FieldBundle Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
     
      !EX_UTest
      !  Verify that Field count query from an uninitialized FieldBundle is handled
      !  properly
      call ESMF_FieldBundleGet(bundle1, fieldCount=fieldcount, rc=rc)
      write(failMsg, *) "Returned ESMF_SUCCESS"
      write(name, *) "Getting Field count from an uninitialized FieldBundle Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
     
      !EX_UTest
      !  Verify the Field count query from an uninitialized FieldBundle is 0
      write(failMsg, *) "Field count not zero"
      write(name, *) "Verify Field count from an uninitialized FieldBundle is zero Test"
      call ESMF_Test((fieldCount.eq.0), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      !This test crashes, bug 1169299 created, commented out
      !  Verify the getting Field names query from an uninitialized FieldBundle is handled
      ! (I think its fixed - Bob 2/12/2007)
      call ESMF_FieldBundleGet(bundle1, nameList=fieldNameList, nameCount=fieldcount, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting Field names from an uninitialized FieldBundle Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !print *, "Field count of uninitialized FieldBundle = ", fieldcount
      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Requirement Creation using Field list
      ! It shall be possible to create a bundle with a field list, an optional 
      ! I/O specification, and an identifier that specifies whether the bundle 
      ! is to be packed (contiguous data) or loose (noncontiguous data). 
      !  Create several empty Fields and add them to a new FieldBundle.
      fields(1) = ESMF_FieldCreateEmpty(name="pressure", rc=rc)
      fields(2) = ESMF_FieldCreateEmpty(name="temperature", rc=rc)
      fields(3) = ESMF_FieldCreateEmpty(name="heat flux", rc=rc)
      bundle1 = ESMF_FieldBundleCreate(3, fields, name="atmosphere data", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating FieldBundle with 3 No Data Fields Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test Requirement Creating a FieldBundle with ESMF_PACKED_DATA option
      ! The ESMF_PACKED_DATA option is not implemented and until it is, it
      ! is correct for the method to return ESMF_RC_NOT_IMPL when it is used.
      write(failMsg, *) "Did not return ESMF_RC_NOT_IMPL"
      bundle4 = ESMF_FieldBundleCreate(3, fields, ESMF_PACKED_DATA, &
      			name="atmosphere data", rc=rc)
      write(name, *) "Creating FieldBundle with ESMF_PACKED_DATA"
      call ESMF_Test((rc.eq.ESMF_RC_NOT_IMPL), name, failMsg, result, ESMF_SRCLINE)
      print *, "rc = ", rc
      !------------------------------------------------------------------------

      !EX_UTest
      !  Verify the getting Field names query from FieldBundle returns ESMF_SUCCESS
      call ESMF_FieldBundleGet(bundle1, nameList=fieldNameList, nameCount=fieldcount, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Field names from a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      !  Verify the Field names query count is correct
      write(failMsg, *) "Field count not 3"
      write(name, *) "Verifying Field count from a FieldBundle Test"
      call ESMF_Test((fieldcount.eq.3), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      !  Verify the Field names are correct
      write(failMsg, *) "Field names are wrong"
      write(name, *) "Verifying Field names from a FieldBundle Test"
      call ESMF_Test((fieldNameList(1).eq."pressure".and.fieldNameList(2).eq."temperature" &
		.and.fieldNameList(3).eq."heat flux"), name, failMsg, result, ESMF_SRCLINE)
		
      print *, "The Field names are:"
      do i = 1 , fieldcount
	print *, fieldNameList(i)
      end do

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_FieldBundleAdd(bundle2, field=simplefield, rc=rc)
      write(failMsg, *) "Add uninitialized Field to uncreated FieldBundle failed"
      write(name, *) "Adding an uninitialized Field to an uncreated FieldBundle Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !Verify that an empty FieldBundle can be created
      !EX_UTest
      bundle2 = ESMF_FieldBundleCreate(name="time step 1", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating Empty FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Creating a Grid
      grid=ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=(/180,90/), &
                                regDecomp=(/2,2/), name="Grid", rc=rc)

      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Create a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      ! Creating a Field Test
      simplefield = ESMF_FieldCreateEmpty(name="rh", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Create a Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      ! FieldBundle Set Grid Test
      call ESMF_FieldBundleSetGrid(bundle2, grid, rc=rc)
      write(failMsg, *) "Did not Return ESMF_SUCCESS"
      write(name, *) "FieldBundle Set Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      !  Verify the getting Field names query from FieldBundle returns ESMF_SUCCESS
      call ESMF_FieldBundleGet(bundle2, nameList=fieldNameList, nameCount=fieldcount, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Field names from a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      
      !EX_UTest
      !  Verify the Field names query count is correct
      write(failMsg, *) "Field count not 0"
      write(name, *) "Verifying Field count from a FieldBundle Test"
      call ESMF_Test((fieldcount.eq.0), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      ! Add a field to an empty FieldBundle
      call ESMF_FieldBundleAdd(bundle2, field=simplefield, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Adding a field to an Empty FieldBundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      !  Verify the getting Field names query from FieldBundle returns ESMF_SUCCESS
      call ESMF_FieldBundleGet(bundle2, nameList=fieldNameList, nameCount=fieldcount, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Field names from a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      
      !EX_UTest
      !  Verify the Field names query count is correct
      write(failMsg, *) "Field count not 1"
      write(name, *) "Verifying Field count from a FieldBundle Test"
      call ESMF_Test((fieldcount.eq.1), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------


      !EX_UTest
      !  Verify the Field names are correct
      write(failMsg, *) "Field name is wrong"
      write(name, *) "Verifying Field name from a FieldBundle Test"
      call ESMF_Test((fieldNameList(1).eq."rh"), name, failMsg, result, ESMF_SRCLINE)
		
      print *, "The Field names is:"
      do i = 1 , fieldcount
	print *, fieldNameList(i)
      end do

      !------------------------------------------------------------------------

      !EX_UTest
      ! Getting Attribute count from a FieldBundle
      call ESMF_AttributeGet(bundle2, count, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Attribute Count from a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      ! Verify Attribute count Test
      write(failMsg, *) "Incorrect count"
      write(name, *) "Verify Attribute count from a FieldBundle "
      call ESMF_Test((count.eq.0), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      !  Verify that recreating a created FieldBundle is handled properly
      bundle2 = ESMF_FieldBundleCreate(name="time step 1", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a FieldBundle that has already been created Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! FieldBundle Set Grid Test
      call ESMF_FieldBundleSetGrid(bundle2, grid, rc=rc)
      write(failMsg, *) "Did not Return ESMF_SUCCESS"
      write(name, *) "FieldBundle Set Grid Test after recreating bundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      !  Verify that the Field count query from an empty FieldBundle is handled properly
      call ESMF_FieldBundleGet(bundle2, fieldCount=fieldcount, rc=rc)
      write(failMsg, *) "Returned ESMF_FAILURE or field count not equal to zero"
      write(name, *) "Getting Field count from an empty FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(fieldcount.eq.0), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "Field count of empty FieldBundle = ", fieldcount
      !------------------------------------------------------------------------

      !EX_UTest
      !  Verify that a Field can be added to an empty FieldBundle
      call ESMF_FieldBundleAdd(bundle2, field=simplefield, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Adding a Field to an Empty FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      !  Verify that the Field count can be queried from a FieldBundle
      call ESMF_FieldBundleGet(bundle2, fieldCount=fieldcount, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting Field count from a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(fieldcount.eq.1), &
                       name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      !  Test Requirement Return Grid
      call ESMF_FieldBundleGet(bundle2, grid=grid2, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting a Grid from a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldBundleDestroy(bundle2)
      !------------------------------------------------------------------------

      ! Create an empty FieldBundle and then add multiple fields to it.
      bundle3 = ESMF_FieldBundleCreate(name="southern hemisphere", rc=rc)
      !bundle3 = ESMF_FieldBundleCreate(name="northern hemisphere", rc=rc)
   
      !EX_UTest
      !  Verify that multiple Fields can be added to a FieldBundle 
      call ESMF_FieldBundleAdd(bundle3, 3, fields, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Adding multiple Fields to a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      !  Verify that Fields count can be queried from a FieldBundle 
      call ESMF_FieldBundleGet(bundle3, fieldCount=fieldcount, rc=rc)
      write(failMsg, *) "Returned ESMF_FAILURE or field count not equal to three"
      write(name, *) "Getting Field count from a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(fieldcount.eq.3), &
                     name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldBundleDestroy(bundle3)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Verify that the first Field name can be queried fron a FieldBundle
      call ESMF_FieldBundleGet(bundle1, "pressure", returnedfield1, rc)
      write(failMsg, *) ""
      write(name, *) "Getting first Field by name from a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldGet(returnedfield1, name=fname1, rc=rc)
      !------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS or incorrect name returned"
      write(name, *) "Getting first Field from a FieldBundle Test continued"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(fname1.eq."pressure"), name, &
					failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Verify that the second Field name can be queried from a FieldBundle
      call ESMF_FieldBundleGet(bundle1, 2, returnedfield2, rc)
      write(failMsg, *) ""
      write(name, *) "Getting a second Field by index from a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldGet(returnedfield2, name=fname2, rc=rc)

      !EX_UTest
      write(failMsg, *) "Subroutine returned ESMF_FAILURE or incorrect name returned"
      write(name, *) "Getting a second Field from a FieldBundle Test continued"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(fname2.eq."temperature"), name, &
						failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Verify that the third Field name can be queried fron a FieldBundle
      call ESMF_FieldBundleGet(bundle1, 3, returnedfield3, rc)
      write(failMsg, *) ""
      write(name, *) "Getting a third Field by index from a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldGet(returnedfield3, name=fname3, rc=rc)

      !------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Subroutine returned ESMF_FAILURE or incorrect name returned"
      write(name, *) "Getting a third Field from a FieldBundle Test continued"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(fname3.eq."heat flux"), name, &
						failMsg, result, ESMF_SRCLINE)
      print *, "FieldBundle returned, field names = ", &
                    trim(fname1), ", ", trim(fname2), ", ", trim(fname3)
      !------------------------------------------------------------------------


      !EX_UTest
      ! Verify that the fourth Field name cannot be queried from a FieldBundle
      ! because there are only three Fields in the FieldBundle
      call ESMF_FieldBundleGet(bundle1, 4, returnedfield3, rc)
      write(failMsg, *) ""
      write(name, *) "Getting a non-existent Field from a FieldBundle Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------


      !EX_UTest
      ! Add an integer attribute to a FieldBundle Test
      call ESMF_AttributeSet(bundle1, name="Sides", value=65, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an integer attribute to a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------


      !EX_UTest
      ! Get an integer attribute from a FieldBundle Test
      call ESMF_AttributeGet(bundle1, name="Sides", value=number, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an integer attribute from a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(number.eq.65), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an integer attribute from a FieldBundle Test
      call ESMF_AttributeGet(bundle1, name="Sides", count=number, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an attribute info from a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(number.eq.1), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Getting Attribute count from a FieldBundle
      call ESMF_AttributeGet(bundle1, count, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Attribute Count from a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      
      !EX_UTest
      ! Verify Attribute count Test
      write(failMsg, *) "Incorrect count"
      write(name, *) "Verify Attribute count from a FieldBundle "
      call ESMF_Test((count.eq.1), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------

      !EX_UTest
      ! Verify that the third Field names can be queried fron a FieldBundle
      call ESMF_FieldBundleGet(bundle1, 3, returnedfield3, rc)
      write(failMsg, *) ""
      write(name, *) "Getting a third Field by index from a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldGet(returnedfield3, name=fname3, rc=rc)

! TODO:FIELDINTEGRATE Restore once FieldBundleGetDataPointer is implemented
      !------------------------------------------------------------------------
      ! Get a FieldBundle Data Pointer from a field with no data - should fail
      !EX_removeUTest
      ! write(failMsg, *) "Returned ESMF_SUCCESS incorrectly"
      ! write(name, *) "Get a FieldBundle Data Pointer Test from empty Field"
      ! call ESMF_FieldBundleGetDataPointer(bundle1, fieldName="heat flux", dataPointer=f90ptr2, rc=rc)
      ! call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      ! print *, "rc =",  rc
      !------------------------------------------------------------------------

      !EX_UTest
      ! Print a FieldBundle Test
      call ESMF_FieldBundlePrint(bundle1, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Printing a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      ! Validate a FieldBundle Test
      call ESMF_FieldBundleValidate(bundle1, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Validating a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      ! Verify that the zeroth Field names cannot be queried fron a FieldBundle
      call ESMF_FieldBundleGet(bundle1, 0, returnedfield3, rc)
      write(failMsg, *) ""
      write(name, *) "Getting a zeroth non-existent Field from a FieldBundle Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------


      !EX_UTest
      ! Verify that the querying Field with wrong name from a FieldBundle returns FAILURE
      call ESMF_FieldBundleGet(bundle1, "nressure", returnedfield1, rc)
      write(failMsg, *) ""
      write(name, *) "Getting wrong Field name from a FieldBundle Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------


      !EX_UTest
      ! Verify that the FieldBundle name can be queried 
      call ESMF_FieldBundleGet(bundle1, name=bname1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Query FieldBundle Name Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bname1.eq."atmosphere data"), &
				name, failMsg, result, ESMF_SRCLINE)
      print *, "FieldBundle name = ", trim(bname1)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test Requirement Deletion
      ! FieldBundles may be deleted. Data allocated by and included in packed bundles 
      ! is deleted along with the bundle. Pointers to field data in unpacked 
      ! bundles are returned at deletion. 
      call ESMF_FieldBundleDestroy(bundle1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "FieldBundle Destroy Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, "rc = ", (rc)
      !------------------------------------------------------------------------


      !EX_UTest
      ! Verify that destroying a destroyed FieldBundle is handled correctly
      call ESMF_FieldBundleDestroy(bundle1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying a Destroyed FieldBundle Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, "rc = ", (rc)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Destroying Fields
      call ESMF_FieldDestroy(fields(1), rc=rc)
      write(failMsg, *) "Destroying a Field"
      write(name, *) "Destroying a Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Destroying Fields
      call ESMF_FieldDestroy(fields(2), rc=rc)
      write(failMsg, *) "Destroying a Field"
      write(name, *) "Destroying a Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Destroying Fields
      call ESMF_FieldDestroy(fields(3), rc=rc)
      write(failMsg, *) "Destroying a Field"
      write(name, *) "Destroying a Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Destroying fields
      call ESMF_FieldDestroy(simplefield, rc=rc)
      write(failMsg, *) "Destroying a Field"
      write(name, *) "Destroying a Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Destroying a grid
      call ESMF_GridDestroy(grid, rc=rc)
      write(failMsg, *) "Destroying a Grid"
      write(name, *) "Destroying a Grid"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Requirement Insert and remove Field
      ! A Field can be inserted into or removed from a FieldBundle
      ! The remove portion of this requirement cannot be tested until Bug 705849
      ! ESMF_FieldBundleDeleteField not implemented" is fixed.
      !------------------------------------------------------------------------

#endif

      call ESMF_TestEnd(result, ESMF_SRCLINE)

      end program ESMF_FieldBundleUTest
