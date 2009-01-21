! $Id: ESMF_FieldBundleRedistUTest.F90,v 1.1.2.6 2009/01/21 21:25:21 cdeluca Exp $
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
      program ESMF_RedistUTest

#include "ESMF_Macros.inc"

!------------------------------------------------------------------------------
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_RedistUTest - Data redistribution tests
!
! !DESCRIPTION:
!
! The code in this file drives F90 Redist unit tests, using the Route code.
!
!  "Redist" is sending data from one field to another, where the igrids 
!   themselves are identical, but the decompositions (which subsets of the
!   igrid are located on each processor) are different.  Redist sends data
!   from one processor to another with no interpolation.  See Regrid for
!   routines which do data interpolation from one igrid to another.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod
      use ESMF_FieldBundleRedistHelpers
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_FieldBundleRedistUTest.F90,v 1.1.2.6 2009/01/21 21:25:21 cdeluca Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure message
      character(ESMF_MAXSTR*2) :: failMsg
      character(ESMF_MAXSTR) :: validate_options = "full"
      character(ESMF_MAXSTR) :: print_options = "all"

      ! local args needed to create/construct objects
      type(ESMF_RouteHandle) :: redist_rh
      type(ESMF_IGrid) :: igrid1, igrid2
      type(ESMF_Field) :: field1, field2, field3, field4
      type(ESMF_FieldBundle) :: bundle1, bundle2
      type(ESMF_VM) :: vm

      integer :: combined_rc
      real(ESMF_KIND_R8), parameter :: val_one = 1.0
      real(ESMF_KIND_R8), parameter :: val_two = 2.0
      real(ESMF_KIND_R8), parameter :: val_neg_one = -1.0
      real(ESMF_KIND_R8), parameter :: val_neg_two = -2.0


      ! -------- beginning of executable code below here -------

      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! create igrids for later on
      call Create2DIGrids(igrid1, igrid2, rc=rc)
      write(name, *) "Creating src and dest igrids"
      write(failMsg, *) "Unable to create src and/or dst igrids"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! create fields for later on
      call CreateFields(igrid1, field1, field3, halo1=2, halo2=4, rc=rc)
      write(name, *) "Creating src and dest fields"
      write(failMsg, *) "Unable to create src and/or dst fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! create fields for later on
      call CreateFields(igrid2, field2, field4, rc=rc)
      write(name, *) "Creating second src and dest fields"
      write(failMsg, *) "Unable to create second src and/or dst fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! create src bundle
      call CreateFieldBundle(bundle1, field1, field3, rc=rc)
      write(name, *) "Creating src bundle"
      write(failMsg, *) "Unable to create src bundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! create dst bundle
      call CreateFieldBundle(bundle2, field2, field4, rc=rc)
      write(name, *) "Creating dst bundle"
      write(failMsg, *) "Unable to create dstsrc bundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! fill source field with known data
      call FillConstantR8Field(field1, val_one, rc)
      write(name, *) "Filling src field with constant data values"
      write(failMsg, *) "Filling src field with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! fill source field with known data
      call FillConstantR8Field(field3, val_two, rc)
      write(name, *) "Filling src field with constant data values"
      write(failMsg, *) "Filling src field with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! fill destination field with known data
      call FillConstantR8Field(field2, val_neg_one, rc)
      write(name, *) "Filling dst field with constant data values"
      write(failMsg, *) "Filling dst field with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! fill destination field with known data
      call FillConstantR8Field(field4, val_neg_two, rc)
      write(name, *) "Filling dst field with constant data values"
      write(failMsg, *) "Filling dst field with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! store
      call ESMF_VMGetGlobal(vm, rc=rc)
      call ESMF_FieldBundleRedistStore(bundle1, bundle2, vm, &
                                                routehandle=redist_rh, rc=rc)
      write(name, *) "Computing route for redist"
      write(failMsg, *) "Computing route for redist"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! run
      call ESMF_FieldBundleRedist(bundle1, bundle2, routehandle=redist_rh, rc=rc)
      write(name, *) "Executing redist"
      write(failMsg, *) "Executing redist"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! validate destination field
      call ValidateConstantR8Field(field2, val_one, rc=rc)
      write(name, *) "Validating constant data in dest fields"
      write(failMsg, *) "Validating constant data in dest fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! validate destination field
      call ValidateConstantR8Field(field4, val_two, rc=rc)
      write(name, *) "Validating constant data in dest fields"
      write(failMsg, *) "Validating constant data in dest fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#if ESMF_TESTEXHAUSTIVE
      !------------------------------------------------------------------------
      !EX_removeUTest
      ! fill source field with known data
      call FillIndexField(field1, rc)
      write(name, *) "Filling src field with indexed data values"
      write(failMsg, *) "Filling src field with indexed data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! fill source field with known data
      call FillIndexField(field3, rc)
      write(name, *) "Filling src field with indexed data values"
      write(failMsg, *) "Filling src field with indexed data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! fill destination field with known data
      call FillConstantR8Field(field2, val_neg_one, rc)
      write(name, *) "Filling dst field with constant data values"
      write(failMsg, *) "Filling dst field with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! fill destination field with known data
      call FillConstantR8Field(field4, val_neg_one, rc)
      write(name, *) "Filling dst field with constant data values"
      write(failMsg, *) "Filling dst field with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! run
      call ESMF_FieldBundleRedist(bundle1, bundle2, routehandle=redist_rh, rc=rc)
      write(name, *) "Executing redist 2"
      write(failMsg, *) "Executing redist 2"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! validate destination field, data regions only
      call ValidateIndexField(field2, rc)
      write(name, *) "Validating indexed data in dest fields"
      write(failMsg, *) "Validating indexed data in dest fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! validate destination field, data regions only
      call ValidateIndexField(field4, rc)
      write(name, *) "Validating indexed data in dest fields"
      write(failMsg, *) "Validating indexed data in dest fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! validate halo regions in destination field - should be unchanged
      call ValidateConstantHalo(field2, val_neg_one, rc=rc)
      write(name, *) "Validating halo area in dest fields"
      write(failMsg, *) "Validating indexed data in dest fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! validate halo regions in destination field - should be unchanged
      call ValidateConstantHalo(field4, val_neg_one, rc=rc)
      write(name, *) "Validating halo area in dest fields"
      write(failMsg, *) "Validating indexed data in dest fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! release first route handle, compute another below
      call ESMF_FieldBundleRedistRelease(redist_rh, rc=rc)
      write(name, *) "Releasing route"
      write(failMsg, *) "Releasing route"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! fill source field with known data
      call FillConstantR8Field(field2, val_one, rc)
      write(name, *) "Filling src field with constant data values"
      write(failMsg, *) "Filling src field with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! fill destination field with known data
      call FillConstantR8Field(field1, val_neg_one, rc)
      write(name, *) "Filling dst field with constant data values"
      write(failMsg, *) "Filling dst field with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! store
      call ESMF_VMGetGlobal(vm, rc=rc)
      call ESMF_FieldBundleRedistStore(bundle2, bundle1, vm, &
                                                routehandle=redist_rh, rc=rc)
      write(name, *) "Computing route for redist, 2 to 1"
      write(failMsg, *) "Computing route for redist, 2 to 1"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! run
      call ESMF_FieldBundleRedist(bundle2, bundle1, routehandle=redist_rh, rc=rc)
      write(name, *) "Executing redist"
      write(failMsg, *) "Executing redist"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! validate destination field
      call ValidateConstantR8Field(field1, val_one, rc=rc)
      write(name, *) "Validating constant data in dest fields"
      write(failMsg, *) "Validating constant data in dest fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! fill source field with known data
      call FillIndexField(field2, rc)
      write(name, *) "Filling src field with indexed data values"
      write(failMsg, *) "Filling src field with indexed data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! fill destination field with known data
      call FillConstantR8Field(field3, val_neg_one, rc)
      write(name, *) "Filling dst field with constant data values"
      write(failMsg, *) "Filling dst field with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! run
      call ESMF_FieldBundleRedist(bundle2, bundle1, routehandle=redist_rh, rc=rc)
      write(name, *) "Executing redist 2 -> 1"
      write(failMsg, *) "Executing redist 2 -> 1"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! validate destination field
      call ValidateIndexField(field3, rc)
      write(name, *) "Validating indexed data in dest fields"
      write(failMsg, *) "Validating indexed data in dest fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! validate halo regions in destination field - should be unchanged
      call ValidateConstantHalo(field3, val_neg_one, rc=rc)
      write(name, *) "Validating halo area in dest fields"
      write(failMsg, *) "Validating indexed data in dest fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#endif

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! release
      call ESMF_FieldBundleRedistRelease(redist_rh, rc=rc)
      write(name, *) "Releasing route"
      write(failMsg, *) "Releasing route"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! cleanup
      combined_rc = ESMF_SUCCESS
      call FieldBundleCleanup(bundle1, rc=rc)
      if (rc .ne. ESMF_SUCCESS) combined_rc = rc
      call FieldBundleCleanup(bundle2, rc=rc)
      if (rc .ne. ESMF_SUCCESS) combined_rc = rc

      write(name, *) "Deleting bundles at cleanup time"
      write(failMsg, *) "Deleting bundles at cleanup time"
      call ESMF_Test((combined_rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! cleanup
      combined_rc = ESMF_SUCCESS
      call FieldCleanup(field1, field3, rc=rc)
      if (rc .ne. ESMF_SUCCESS) combined_rc = rc
      call FieldCleanup(field2, field4, rc=rc)
      if (rc .ne. ESMF_SUCCESS) combined_rc = rc
      write(name, *) "Deleting fields at cleanup time"
      write(failMsg, *) "Deleting fields at cleanup time"
      call ESMF_Test((combined_rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


10    continue

      call ESMF_TestEnd(result, ESMF_SRCLINE)
  
      ! -------- end of unit test code ------------------------

      end program ESMF_RedistUTest



