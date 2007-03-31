! $Id: ESMF_BundleCommUTest.F90,v 1.18 2007/03/31 05:50:54 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_BundleCommUTest

!------------------------------------------------------------------------------
#define ESMF_FILENAME "ESMF_BundleCommUTest.F90"
!
!
#include <ESMF.h>

!==============================================================================
!BOP
! !PROGRAM: ESMF_BundleCommUTest - Bundle Communication Unit Tests
!
! !DESCRIPTION:
!
! The code in this file drives F90 Bundle Communication unit tests.
! These include calls like Halo, Gather, Regrid, and Redistribution.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod
      use ESMF_BundleRedistHelpers

      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_BundleCommUTest.F90,v 1.18 2007/03/31 05:50:54 cdeluca Exp $'
!------------------------------------------------------------------------------

!     ! Local variables
      integer :: rc
      type(ESMF_VM) :: vm
      type(ESMF_Grid) :: grid
      type(ESMF_Field) :: fields(8)
      type(ESMF_Bundle) :: bundle1, bundle2
      type(ESMF_DELayout) :: layout 
      type(ESMF_RouteHandle) :: rh
      integer :: nroutes

#ifdef ESMF_EXHAUSTIVE
      ! try to avoid "unused variable" warnings from some of our compilers.
      type(ESMF_Bundle) :: nobundle
      type(ESMF_Field) :: nofield
      type(ESMF_RouteHandle) :: rh1, rh2
#endif

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

      ! all the following tests are checking rc for success.   the failMsg
      ! can be the same for all.
      write(failMsg, *) "rc not ESMF_SUCCESS"

      !------------------------------------------------------------------------
      !NEX_UTest
      call ESMF_VMGetGlobal(vm, rc)
      write(name, *) "Getting global vm"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      ! Test redistribution of bundle when the datatypes match exactly,
      !  so the route code can use a single route table for all fields.
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !NEX_UTest
      !  Create 4 data field for use below. fields 1 and 2 share a grid, 
      !  fields 3 and 4 share a different grid.  fields 1 and 2 go into 
      !  bundle 1; fields 3 and 4 go into bundle 2.  
      fields(1) = CreateDataField("humidity", r8value=1.1_ESMF_KIND_R8, rc=rc)
      write(name, *) "Creating Data Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !NEX_UTest
      !  Get grid from previous field and reuse it below.
      call ESMF_FieldGet(fields(1), grid=grid, rc=rc)
      write(name, *) "Getting Grid from Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !NEX_UTest
      fields(2) = CreateDataField("pressure", grid=grid, &
                                   r8value=2.2_ESMF_KIND_R8, rc=rc)
      write(name, *) "Creating Data Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !NEX_UTest
      ! Create a non-default layout of type "2".  See the helper routines
      ! for the various options in layout types.
      layout = CreateLayout(2, rc)
      write(name, *) "Creating non-default layout"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !NEX_UTest
      fields(3) = CreateDataField("humidity", r8value=-88.88_ESMF_KIND_R8, &
                                   layout=layout, rc=rc)
      write(name, *) "Creating Data Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !NEX_UTest
      !  Get grid from previous field and reuse it below.
      call ESMF_FieldGet(fields(3), grid=grid, rc=rc)
      write(name, *) "Getting Grid from Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !NEX_UTest
      fields(4) = CreateDataField("pressure", grid=grid, &
                                  r8value=-99.99_ESMF_KIND_R8, rc=rc)
      write(name, *) "Creating Data Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !NEX_UTest
      !  Create 2 bundles with the already created fields.
      bundle1 = ESMF_BundleCreate(2, fields(1:2), name="ocn export", rc=rc)
      write(name, *) "Creating source Bundle with 2 fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     
      !------------------------------------------------------------------------
      !NEX_UTest
      bundle2 = ESMF_BundleCreate(2, fields(3:4), name="atm import", rc=rc)
      write(name, *) "Creating destination Bundle with 2 fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     
      !------------------------------------------------------------------------
      !NEX_UTest
      ! redistribute data from bundle1 to bundle2
      call ESMF_BundleRedistStore(bundle1, bundle2, vm, rh, rc=rc)
      write(name, *) "Precompute Redist Communication"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !NEX_UTest
      !  This route table should have only 1 internal route.
      call ESMF_RouteHandleGet(rh, route_count=nroutes, rc=rc)
      write(name, *) "Single route inside handle"
      write(failMsg, *) "Multiple routes inside handle, should be 1"
      call ESMF_Test((nroutes.eq.1), name, failMsg, result, ESMF_SRCLINE)

      ! put the default message back
      write(failMsg, *) "rc not ESMF_SUCCESS"
      
      !------------------------------------------------------------------------
      !NEX_UTest
      !  Redist both fields in a single Bundle call.
      call ESMF_BundleRedist(bundle1, bundle2, rh, rc=rc)
      write(name, *) "Redistributing data from all Fields in a Bundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !NEX_UTest
      call ValidateConstantR8Field(fields(3), val=1.1_ESMF_KIND_R8, rc=rc)
      write(name, *) "Validating field 3 results"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !NEX_UTest
      call ValidateConstantR8Field(fields(4), val=2.2_ESMF_KIND_R8, rc=rc)
      write(name, *) "Validating field 4 results"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !NEX_UTest
      !  Release the routehandle
      call ESMF_BundleRedistRelease(rh, rc=rc)
      write(name, *) "Release the RouteHandle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      ! clean up the objects
      !------------------------------------------------------------------------
      !NEX_UTest
      ! Clean up by deleting the Bundles.
      call ESMF_BundleDestroy(bundle1, rc=rc)
      write(name, *) "Bundle Destroy 1"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      call ESMF_BundleDestroy(bundle2, rc=rc)
      write(name, *) "Bundle Destroy 2"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Clean up by deleting the Fields.
      call FieldCleanup(fields(1), fields(2), rc=rc)
      write(name, *) "Field Destroy 1,2"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Clean up by deleting the Fields.
      call FieldCleanup(fields(3), fields(4), rc=rc)
      write(name, *) "Field Destroy 3,4"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------

#ifdef ESMF_EXHAUSTIVE

      !------------------------------------------------------------------------

      ! set these up for use later
      nobundle = ESMF_BundleCreate()
      call ESMF_BundleDestroy(nobundle)
      nofield = ESMF_FieldCreateNoData()
      call ESMF_FieldDestroy(nofield)

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      ! Test redistribution of bundle when the datatypes do not match,
      !  so the route code must compute multiple routes and loop internally.
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !EX_UTest
      !  Create 4 data field for use below. fields 1 and 2 share a grid, 
      !  fields 3 and 4 share a different grid.  fields 1 and 2 go into 
      !  bundle 1; fields 3 and 4 go into bundle 2.
      fields(1) = CreateDataField("humidity", r8value=1.1_ESMF_KIND_R8, rc=rc)
      write(name, *) "Creating Data Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Get grid from previous field and reuse it below.
      call ESMF_FieldGet(fields(1), grid=grid, rc=rc)
      write(name, *) "Getting Grid from Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      fields(2) = CreateDataField("pressure", grid=grid, &
                                   r4value=2.2_ESMF_KIND_R4, rc=rc)
      write(name, *) "Creating Data Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      ! Create a non-default layout of type "2".  See the helper routines
      ! for the various options in layout types.
      layout = CreateLayout(2, rc)
      write(name, *) "Creating non-default layout"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      fields(3) = CreateDataField("humidity", r8value=-88.88_ESMF_KIND_R8, &
                                   layout=layout, rc=rc)
      write(name, *) "Creating Data Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Get grid from previous field and reuse it below.
      call ESMF_FieldGet(fields(3), grid=grid, rc=rc)
      write(name, *) "Getting Grid from Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      fields(4) = CreateDataField("pressure", grid=grid, &
                                  r4value=-99.99_ESMF_KIND_R4, rc=rc)
      write(name, *) "Creating Data Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Create 2 bundles with the already created fields.
      bundle1 = ESMF_BundleCreate(2, fields(1:2), name="ocn export", rc=rc)
      write(name, *) "Creating source Bundle with 2 fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     
      !------------------------------------------------------------------------
      !EX_UTest
      bundle2 = ESMF_BundleCreate(2, fields(3:4), name="atm import", rc=rc)
      write(name, *) "Creating destination Bundle with 2 fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     
      !------------------------------------------------------------------------
      !EX_UTest
      ! redistribute data from bundle1 to bundle2
      call ESMF_BundleRedistStore(bundle1, bundle2, vm, rh, rc=rc)
      write(name, *) "Precompute Redist Communication"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  This route table should have 2 internal routes.
      call ESMF_RouteHandleGet(rh, route_count=nroutes, rc=rc)
      write(name, *) "Multiple routes inside handle"
      write(failMsg, *) "Number of routes inside handle not 2"
      call ESMF_Test((nroutes.eq.2), name, failMsg, result, ESMF_SRCLINE)

      ! put the default message back
      write(failMsg, *) "rc not ESMF_SUCCESS"
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Redist both fields in a single Bundle call.
      call ESMF_BundleRedist(bundle1, bundle2, rh, rc=rc)
      write(name, *) "Redistributing data from all Fields in a Bundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      call ValidateConstantR8Field(fields(3), val=1.1_ESMF_KIND_R8, rc=rc)
      write(name, *) "Validating field 3 results"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      call ValidateConstantR4Field(fields(4), val=2.2_ESMF_KIND_R4, rc=rc)
      write(name, *) "Validating field 4 results"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Release the routehandle
      call ESMF_BundleRedistRelease(rh, rc=rc)
      write(name, *) "Release the RouteHandle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      ! clean up the objects
      !------------------------------------------------------------------------
      !EX_UTest
      ! Clean up by deleting the Bundles.
      call ESMF_BundleDestroy(bundle1, rc=rc)
      write(name, *) "Bundle Destroy 1"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_BundleDestroy(bundle2, rc=rc)
      write(name, *) "Bundle Destroy 2"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Clean up by deleting the Fields.
      call FieldCleanup(fields(1), fields(2), rc=rc)
      write(name, *) "Field Destroy 1,2"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Clean up by deleting the Fields.
      call FieldCleanup(fields(3), fields(4), rc=rc)
      write(name, *) "Field Destroy 3,4"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      ! Test halo of bundle both when the fields match and when they do not.
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !EX_UTest
      !  Create 4 data field for use below. fields 1 and 2 share a grid, 
      !  fields 3 and 4 share a different grid.  fields 1 and 2 go into 
      !  bundle 1; fields 3 and 4 go into bundle 2.  fields 1 and 2 match;
      !  fields 3 and 4 do not.
      fields(1) = CreateDataField("humidity", r8value=1.1_ESMF_KIND_R8, rc=rc)
      write(name, *) "Creating Data Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      ! set the halo to another value.
      call FillConstantR8Halo(fields(1), val=-4.4_ESMF_KIND_R8, rc=rc)
      write(name, *) "Setting halo region to another value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Get grid from previous field and reuse it below.
      call ESMF_FieldGet(fields(1), grid=grid, rc=rc)
      write(name, *) "Getting Grid from Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      fields(2) = CreateDataField("pressure", grid=grid, &
                                   r8value=2.2_ESMF_KIND_R8, rc=rc)
      write(name, *) "Creating Data Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      ! set the halo to another value.
      call FillConstantR8Halo(fields(2), val=-6.6_ESMF_KIND_R8, rc=rc)
      write(name, *) "Setting halo region to another value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      ! Create a non-default layout of type "2".  See the helper routines
      ! for the various options in layout types.
      layout = CreateLayout(2, rc)
      write(name, *) "Creating non-default layout"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      fields(3) = CreateDataField("humidity", r8value=8.8_ESMF_KIND_R8, &
                                   layout=layout, rc=rc)
      write(name, *) "Creating Data Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      ! set the halo to another value.
      call FillConstantR8Halo(fields(3), val=-1.1_ESMF_KIND_R8, rc=rc)
      write(name, *) "Setting halo region to another value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Get grid from previous field and reuse it below.
      call ESMF_FieldGet(fields(3), grid=grid, rc=rc)
      write(name, *) "Getting Grid from Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      fields(4) = CreateDataField("pressure", grid=grid, &
                                  r4value=9.9_ESMF_KIND_R4, rc=rc)
      write(name, *) "Creating Data Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      ! set the halo to another value.
      call FillConstantR4Halo(fields(4), val=-2.2_ESMF_KIND_R4, rc=rc)
      write(name, *) "Setting halo region to another value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Create 2 bundles with the already created fields.
      bundle1 = ESMF_BundleCreate(2, fields(1:2), name="ocn export", rc=rc)
      write(name, *) "Creating source Bundle with 2 fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     
      !------------------------------------------------------------------------
      !EX_UTest
      bundle2 = ESMF_BundleCreate(2, fields(3:4), name="atm import", rc=rc)
      write(name, *) "Creating destination Bundle with 2 fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     
      !------------------------------------------------------------------------
      !EX_UTest
      ! compute halo routehandles for both bundle1 and bundle2
      call ESMF_BundleHaloStore(bundle1, rh1, rc=rc)
      write(name, *) "Precompute Halo Communication 1"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_BundleHaloStore(bundle2, rh2, rc=rc)
      write(name, *) "Precompute Halo Communication 2"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Halo both fields in a single Bundle call.
      call ESMF_BundleHalo(bundle1, rh1, rc=rc)
      write(name, *) "Halo data from all Fields in a Bundle 1"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_BundleHalo(bundle2, rh2, rc=rc)
      write(name, *) "Halo data from all Fields in a Bundle 2"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      call ValidateConstantR8Field(fields(1), val=1.1_ESMF_KIND_R8, rc=rc)
      write(name, *) "Validating field 1 results"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      call ValidateConstantR8Field(fields(2), val=2.2_ESMF_KIND_R8, rc=rc)
      write(name, *) "Validating field 2 results"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      call ValidateConstantR8Field(fields(3), val=8.8_ESMF_KIND_R8, rc=rc)
      write(name, *) "Validating field 3 results"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      call ValidateConstantR4Field(fields(4), val=9.9_ESMF_KIND_R4, rc=rc)
      write(name, *) "Validating field 4 results"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Release the routehandles
      call ESMF_BundleRedistRelease(rh1, rc=rc)
      write(name, *) "Release RouteHandle 1"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Release the routehandles
      call ESMF_BundleRedistRelease(rh2, rc=rc)
      write(name, *) "Release RouteHandle 2"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      ! clean up the data used by the previous tests.
      !------------------------------------------------------------------------
      !EX_UTest
      ! Clean up by deleting the Bundles.
      call ESMF_BundleDestroy(bundle1, rc=rc)
      write(name, *) "Bundle Destroy 1"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_BundleDestroy(bundle2, rc=rc)
      write(name, *) "Bundle Destroy 2"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Clean up by deleting the Fields.
      call FieldCleanup(fields(1), fields(2), rc=rc)
      write(name, *) "Field Destroy 1,2"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Clean up by deleting the Fields.
      call FieldCleanup(fields(3), fields(4), rc=rc)
      write(name, *) "Field Destroy 3,4"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      ! Test regridding of bundle when the datatypes match, so a single
      !  precomputed set of weights and route table apply to all.
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !EX_UTest
      !  Create 4 data field for use below. fields 1 and 2 share a grid, 
      !  fields 3 and 4 share a different grid.  fields 1 and 2 go into 
      !  bundle 1; fields 3 and 4 go into bundle 2.
      fields(1) = CreateDataField("humidity", r8value=1.1_ESMF_KIND_R8, rc=rc)
      write(name, *) "Creating Data Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Get grid from previous field and reuse it below.
      call ESMF_FieldGet(fields(1), grid=grid, rc=rc)
      write(name, *) "Getting Grid from Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      fields(2) = CreateDataField("pressure", grid=grid, &
                                   r8value=2.2_ESMF_KIND_R8, rc=rc)
      write(name, *) "Creating Data Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      ! Create a non-default layout of type "3".  See the helper routines
      ! for the various options in layout types.
      layout = CreateLayout(3, rc)
      write(name, *) "Creating non-default layout"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      ! Create a different grid based on the above layout.
      ! See the helper routines for the different grid options.
      grid = CreateGrid(3, layout, rc)
      write(name, *) "Creating non-default grid"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      fields(3) = CreateDataField("humidity", r8value=-88.88_ESMF_KIND_R8, &
                                  grid=grid, rc=rc)
      write(name, *) "Creating Data Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      fields(4) = CreateDataField("pressure", grid=grid, &
                                  r8value=-99.99_ESMF_KIND_R8, rc=rc)
      write(name, *) "Creating Data Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Create 2 bundles with the already created fields.
      bundle1 = ESMF_BundleCreate(2, fields(1:2), name="ocn export", rc=rc)
      write(name, *) "Creating source Bundle with 2 fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     
      !------------------------------------------------------------------------
      !EX_UTest
      bundle2 = ESMF_BundleCreate(2, fields(3:4), name="atm import", rc=rc)
      write(name, *) "Creating destination Bundle with 2 fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     
      !------------------------------------------------------------------------
      !EX_UTest
      ! regrid data from bundle1 to bundle2
      call ESMF_BundleRegridStore(bundle1, bundle2, vm, rh, &
                                  ESMF_REGRID_METHOD_BILINEAR, rc=rc)
      write(name, *) "Precompute Regrid Communication"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Regrid both fields in a single Bundle call.
      call ESMF_BundleRegrid(bundle1, bundle2, rh, rc=rc)
      write(name, *) "Regridding data from all Fields in a Bundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      call ValidateConstantR8Field(fields(3), val=1.1_ESMF_KIND_R8, rc=rc)
      write(name, *) "Validating field 3 results"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      call ValidateConstantR8Field(fields(4), val=2.2_ESMF_KIND_R8, rc=rc)
      write(name, *) "Validating field 4 results"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Release the routehandle
      call ESMF_BundleRegridRelease(rh, rc=rc)
      write(name, *) "Release the RouteHandle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      ! clean up the objects
      !------------------------------------------------------------------------
      !EX_UTest
      ! Clean up by deleting the Bundles.
      call ESMF_BundleDestroy(bundle1, rc=rc)
      write(name, *) "Bundle Destroy 1"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_BundleDestroy(bundle2, rc=rc)
      write(name, *) "Bundle Destroy 2"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Clean up by deleting the Fields.
      call FieldCleanup(fields(1), fields(2), rc=rc)
      write(name, *) "Field Destroy 1,2"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Clean up by deleting the Fields.
      call FieldCleanup(fields(3), fields(4), rc=rc)
      write(name, *) "Field Destroy 3,4"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      ! Test regridding of bundle when the rellocs do *not* match, so the
      !  regrid code must loop internally, computing weights and routes for
      !  each field individually.
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !EX_UTest
      !  Create 4 data field for use below. fields 1 and 2 share a grid, 
      !  fields 3 and 4 share a different grid.  fields 1 and 2 go into 
      !  bundle 1; fields 3 and 4 go into bundle 2.
      fields(1) = CreateDataField("humidity", r8value=1.1_ESMF_KIND_R8, &
                                   relloc=ESMF_CELL_NECORNER, rc=rc)
      write(name, *) "Creating Data Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Get grid from previous field and reuse it below.
      call ESMF_FieldGet(fields(1), grid=grid, rc=rc)
      write(name, *) "Getting Grid from Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      fields(2) = CreateDataField("pressure", grid=grid, &
                                   r8value=2.2_ESMF_KIND_R8, &
                                   relloc=ESMF_CELL_CENTER, rc=rc)
      write(name, *) "Creating Data Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      ! Create a non-default layout of type "3".  See the helper routines
      ! for the various options in layout types.
      layout = CreateLayout(3, rc)
      write(name, *) "Creating non-default layout"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      ! Create a different grid based on the above layout.
      ! See the helper routines for the different grid options.
      grid = CreateGrid(3, layout, rc)
      write(name, *) "Creating non-default grid"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      fields(3) = CreateDataField("humidity", grid=grid, &
                                   r8value=-88.88_ESMF_KIND_R8, &
                                   relloc=ESMF_CELL_NECORNER, rc=rc)
      write(name, *) "Creating Data Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      fields(4) = CreateDataField("pressure", grid=grid, &
                                  r8value=-99.99_ESMF_KIND_R8, &
                                  relloc=ESMF_CELL_CENTER, rc=rc)
      write(name, *) "Creating Data Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Create 2 bundles with the already created fields.
      bundle1 = ESMF_BundleCreate(2, fields(1:2), name="ocn export", rc=rc)
      write(name, *) "Creating source Bundle with 2 fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     
      !------------------------------------------------------------------------
      !EX_UTest
      bundle2 = ESMF_BundleCreate(2, fields(3:4), name="atm import", rc=rc)
      write(name, *) "Creating destination Bundle with 2 fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     
      !------------------------------------------------------------------------
      !EX_UTest
      ! regrid data from bundle1 to bundle2
      call ESMF_BundleRegridStore(bundle1, bundle2, vm, rh, &
                                  ESMF_REGRID_METHOD_BILINEAR, rc=rc)
                                  !ESMF_REGRID_METHOD_CONSERV1, rc=rc)
      write(name, *) "Precompute Regrid Communication"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Regrid both fields in a single Bundle call.
      call ESMF_BundleRegrid(bundle1, bundle2, rh, rc=rc)
      write(name, *) "Regridding data from all Fields in a Bundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      call ValidateConstantR8Field(fields(3), val=1.1_ESMF_KIND_R8, rc=rc)
      write(name, *) "Validating field 3 results"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      call ValidateConstantR8Field(fields(4), val=2.2_ESMF_KIND_R8, rc=rc)
      write(name, *) "Validating field 4 results"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Release the routehandle
      call ESMF_BundleRegridRelease(rh, rc=rc)
      write(name, *) "Release the RouteHandle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      ! clean up the objects
      !------------------------------------------------------------------------
      !EX_UTest
      ! Clean up by deleting the Bundles.
      call ESMF_BundleDestroy(bundle1, rc=rc)
      write(name, *) "Bundle Destroy 1"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_BundleDestroy(bundle2, rc=rc)
      write(name, *) "Bundle Destroy 2"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Clean up by deleting the Fields.
      call FieldCleanup(fields(1), fields(2), rc=rc)
      write(name, *) "Field Destroy 1,2"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Clean up by deleting the Fields.
      call FieldCleanup(fields(3), fields(4), rc=rc)
      write(name, *) "Field Destroy 3,4"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      ! Test regridding of bundle when the datatypes do *not* match, so the
      !  regrid code must loop internally, computing weights and routes for
      !  each field individually.
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !EX_UTest
      !  Create 4 data field for use below. fields 1 and 2 share a grid, 
      !  fields 3 and 4 share a different grid.  fields 1 and 2 go into 
      !  bundle 1; fields 3 and 4 go into bundle 2.
      fields(1) = CreateDataField("humidity", r4value=1.1_ESMF_KIND_R4, rc=rc)
      write(name, *) "Creating Data Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Get grid from previous field and reuse it below.
      call ESMF_FieldGet(fields(1), grid=grid, rc=rc)
      write(name, *) "Getting Grid from Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      fields(2) = CreateDataField("pressure", grid=grid, &
                                   r8value=2.2_ESMF_KIND_R8, rc=rc)
      write(name, *) "Creating Data Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      ! Create a non-default layout of type "3".  See the helper routines
      ! for the various options in layout types.
      layout = CreateLayout(3, rc)
      write(name, *) "Creating non-default layout"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      ! Create a different grid based on the above layout.
      ! See the helper routines for the different grid options.
      grid = CreateGrid(3, layout, rc)
      write(name, *) "Creating non-default grid"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      fields(3) = CreateDataField("humidity", r4value=-88.88_ESMF_KIND_R4, &
                                  grid=grid, rc=rc)
      write(name, *) "Creating Data Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      fields(4) = CreateDataField("pressure", grid=grid, &
                                  r8value=-99.99_ESMF_KIND_R8, rc=rc)
      write(name, *) "Creating Data Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Create 2 bundles with the already created fields.
      bundle1 = ESMF_BundleCreate(2, fields(1:2), name="ocn export", rc=rc)
      write(name, *) "Creating source Bundle with 2 fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     
      !------------------------------------------------------------------------
      !EX_UTest
      bundle2 = ESMF_BundleCreate(2, fields(3:4), name="atm import", rc=rc)
      write(name, *) "Creating destination Bundle with 2 fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     
      !------------------------------------------------------------------------
      !EX_UTest
      ! regrid data from bundle1 to bundle2
      call ESMF_BundleRegridStore(bundle1, bundle2, vm, rh, &
                                  ESMF_REGRID_METHOD_BILINEAR, rc=rc)
                                  !ESMF_REGRID_METHOD_CONSERV1, rc=rc)
      write(name, *) "Precompute Regrid Communication"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Regrid both fields in a single Bundle call.
      call ESMF_BundleRegrid(bundle1, bundle2, rh, rc=rc)
      write(name, *) "Regridding data from all Fields in a Bundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      call ValidateConstantR4Field(fields(3), val=1.1_ESMF_KIND_R4, rc=rc)
      write(name, *) "Validating field 3 results"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      call ValidateConstantR8Field(fields(4), val=2.2_ESMF_KIND_R8, rc=rc)
      write(name, *) "Validating field 4 results"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Release the routehandle
      call ESMF_BundleRegridRelease(rh, rc=rc)
      write(name, *) "Release the RouteHandle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      ! clean up the objects
      !------------------------------------------------------------------------
      !EX_UTest
      ! Clean up by deleting the Bundles.
      call ESMF_BundleDestroy(bundle1, rc=rc)
      write(name, *) "Bundle Destroy 1"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_BundleDestroy(bundle2, rc=rc)
      write(name, *) "Bundle Destroy 2"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Clean up by deleting the Fields.
      call FieldCleanup(fields(1), fields(2), rc=rc)
      write(name, *) "Field Destroy 1,2"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Clean up by deleting the Fields.
      call FieldCleanup(fields(3), fields(4), rc=rc)
      write(name, *) "Field Destroy 3,4"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      ! Test redistribution of bundles when things should *not* succeed:
      !  number of fields do not match; corresponding fields are incompatible
      !  data types, fields are empty (no data), and source == destination.
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !EX_UTest
      !  Create 5 data field for use below. fields 1, 2, and 3 share a grid, 
      !  fields 4 and 5 share a different grid.  fields 1, 2 and 3 go into 
      !  bundle 1; fields 4 and 5 go into bundle 2.
      fields(1) = CreateDataField("humidity", r8value=1.1_ESMF_KIND_R8, rc=rc)
      write(name, *) "Creating Data Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Get grid from previous field and reuse it below.
      call ESMF_FieldGet(fields(1), grid=grid, rc=rc)
      write(name, *) "Getting Grid from Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      fields(2) = CreateDataField("pressure", grid=grid, &
                                   r4value=2.2_ESMF_KIND_R4, rc=rc)
      write(name, *) "Creating Data Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      fields(3) = CreateDataField("density", grid=grid, &
                                   r8value=4.4_ESMF_KIND_R8, rc=rc)
      write(name, *) "Creating Data Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      ! Create a non-default layout of type "2".  See the helper routines
      ! for the various options in layout types.
      layout = CreateLayout(2, rc)
      write(name, *) "Creating non-default layout"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      fields(4) = CreateDataField("humidity", r8value=-88.88_ESMF_KIND_R8, &
                                   layout=layout, rc=rc)
      write(name, *) "Creating Data Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Get grid from previous field and reuse it below.
      call ESMF_FieldGet(fields(4), grid=grid, rc=rc)
      write(name, *) "Getting Grid from Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      fields(5) = CreateDataField("pressure", grid=grid, &
                                  r4value=-99.99_ESMF_KIND_R4, rc=rc)
      write(name, *) "Creating Data Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      !  Create 2 bundles with the already created fields.
      bundle1 = ESMF_BundleCreate(3, fields(1:3), name="ocn export", rc=rc)
      write(name, *) "Creating source Bundle with 3 fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     
      !------------------------------------------------------------------------
      !EX_UTest
      bundle2 = ESMF_BundleCreate(2, fields(4:5), name="atm import", rc=rc)
      write(name, *) "Creating destination Bundle with 2 fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     
      !------------------------------------------------------------------------
      !EX_UTest
      ! redistribute data from bundle1 to bundle2 - this *should* fail
      ! because count of fields in source and dest bundles not same.
      call ESMF_BundleRedistStore(bundle1, bundle2, vm, rh, rc=rc)
      write(name, *) "Precompute Redist with mismatching field counts"
      write(failMsg, *) "rc should not be ESMF_SUCCESS"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !!EX_UTest
      ! redistribute data from bundle1 to bundle2 - this *should* fail
      ! because data types do not line up between src and dst bundles.
      call ESMF_BundleRedistStore(bundle1, bundle2, vm, rh, rc=rc)
      write(name, *) "Precompute Redist with mismatched data types"
      write(failMsg, *) "rc should not be ESMF_SUCCESS"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      ! redistribute data from bundle1 to bundle2 - this *should* fail
      ! because empty fields do not line up.
      call ESMF_BundleRedistStore(bundle1, bundle2, vm, rh, rc=rc)
      write(name, *) "Precompute Redist with mismatched empty fields"
      write(failMsg, *) "rc should not be ESMF_SUCCESS"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      ! redistribute data from bundle1 to bundle2 - this *should* fail
      ! because both bundles have no fields.
      call ESMF_BundleRedistStore(bundle1, bundle2, vm, rh, rc=rc)
      write(name, *) "Precompute Redist with empty bundles"
      write(failMsg, *) "rc should not be ESMF_SUCCESS"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      ! redistribute data from bundle1 to bundle2 - this *should* fail
      ! because both bundles have fields but the fields have no data.
      call ESMF_BundleRedistStore(bundle1, bundle2, vm, rh, rc=rc)
      write(name, *) "Precompute Redist with only empty fields"
      write(failMsg, *) "rc should not be ESMF_SUCCESS"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      
      !------------------------------------------------------------------------
      ! put default fail message back back.
      write(failMsg, *) "rc not ESMF_SUCCESS"
      !------------------------------------------------------------------------
      
      !------------------------------------------------------------------------
      ! clean up the objects
      !------------------------------------------------------------------------
      !EX_UTest
      ! Clean up by deleting the Bundles.
      call ESMF_BundleDestroy(bundle1, rc=rc)
      write(name, *) "Bundle Destroy 1"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_BundleDestroy(bundle2, rc=rc)
      write(name, *) "Bundle Destroy 2"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Clean up by deleting the Fields.
      call FieldCleanup(fields(1), fields(2), fields(3), rc=rc)
      write(name, *) "Field Destroy 1,2,3"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Clean up by deleting the Fields.
      call FieldCleanup(fields(4), fields(5), rc=rc)
      write(name, *) "Field Destroy 4,5"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------

#endif

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------

      call ESMF_TestEnd(result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      end program ESMF_BundleCommUTest
