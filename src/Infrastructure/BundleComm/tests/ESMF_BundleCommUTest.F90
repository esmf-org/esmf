! $Id: ESMF_BundleCommUTest.F90,v 1.8 2005/10/12 19:06:16 nscollins Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2005, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
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
      '$Id: ESMF_BundleCommUTest.F90,v 1.8 2005/10/12 19:06:16 nscollins Exp $'
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
#if 0
      type(ESMF_Grid) :: grid2
      type(ESMF_Bundle) :: bundle3, bundle4, nobundle
      type(ESMF_Array) :: arrayList(2)
#endif
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
      call ValidateConstantField(fields(3), val=1.1_ESMF_KIND_R8, rc=rc)
      write(name, *) "Validating field 3 results"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !NEX_UTest
      call ValidateConstantField(fields(4), val=2.2_ESMF_KIND_R8, rc=rc)
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
      !NEX_UTest
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
      call ValidateConstantField(fields(3), val=1.1_ESMF_KIND_R8, rc=rc)
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
      call FillConstantHalo(fields(1), val=-4.4_ESMF_KIND_R8, rc=rc)
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
      call FillConstantHalo(fields(2), val=-6.6_ESMF_KIND_R8, rc=rc)
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
      call FillConstantHalo(fields(3), val=-1.1_ESMF_KIND_R8, rc=rc)
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
      call ValidateConstantField(fields(1), val=1.1_ESMF_KIND_R8, rc=rc)
      write(name, *) "Validating field 1 results"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      call ValidateConstantField(fields(2), val=2.2_ESMF_KIND_R8, rc=rc)
      write(name, *) "Validating field 2 results"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      call ValidateConstantField(fields(3), val=8.8_ESMF_KIND_R8, rc=rc)
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
      call ValidateConstantField(fields(3), val=1.1_ESMF_KIND_R8, rc=rc)
      write(name, *) "Validating field 3 results"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      call ValidateConstantField(fields(4), val=2.2_ESMF_KIND_R8, rc=rc)
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
      call ValidateConstantField(fields(3), val=1.1_ESMF_KIND_R8, rc=rc)
      write(name, *) "Validating field 3 results"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      call ValidateConstantField(fields(4), val=2.2_ESMF_KIND_R8, rc=rc)
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
      call ValidateConstantField(fields(4), val=2.2_ESMF_KIND_R8, rc=rc)
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
! TODO: ok to here
#if 0
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Create an empty bundle for use below.
      bundle1 = ESMF_BundleCreate(name="time step 1", rc=rc)
      write(name, *) "Creating Empty Bundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Create an empty bundle for use below.
      bundle2 = ESMF_BundleCreate(name="time step 2", rc=rc)
      write(name, *) "Creating Empty Bundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Create a data Field for use below.
      fields(1) = CreateDataField("humidity", rc=rc)
      write(name, *) "Creating Data Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Create another data Field for use below.
      !  Get grid from previous field and reuse it.
      call ESMF_FieldGet(fields(1), grid=grid, rc=rc)
      write(name, *) "Getting Grid from Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Create another data Field for use below.
      !  Get grid from previous field and reuse it.
      fields(2) = CreateDataField("pressure", grid=grid, rc=rc)
      write(name, *) "Creating Data Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Add fields to bundle
      call ESMF_BundleAddField(bundle1, 2, fields(1:2), rc=rc)
      write(name, *) "Adding 2 Fields to Bundle 1"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Add fields to bundle
      call ESMF_BundleAddField(bundle2, 2, fields(1:2), rc=rc)
      write(name, *) "Adding 2 Fields to Bundle 2"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !E-X_UTest
  ! FIX ME - don't try a redist to yourself as the first thing.
      call ESMF_BundleRedistStore(bundle1, bundle2, vm, rh, rc=rc)
      write(name, *) "Precompute Redist Communication"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Print out precomputed Route Table
      call ESMF_RouteHandlePrint(rh, rc=rc)
      write(name, *) "Printing out the contents of a Route Table"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Redist both fields in a single Bundle call.
      call ESMF_BundleRedist(bundle1, bundle2, rh, rc=rc)
      write(name, *) "Redisting all Fields in a Bundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Release the routehandle
      call ESMF_BundleRedistRelease(rh, rc=rc)
      write(name, *) "Release the RouteHandle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Create another data Field for use below.
      !  Get grid from previous field and reuse it.
      fields(3) = CreateDataField2("pressure", rvalue=9.9_ESMF_KIND_R8, rc=rc)
      write(name, *) "Creating Data Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Create another data Field for use below.
      call ESMF_FieldGet(fields(3), grid=grid2, rc=rc)
      write(name, *) "Getting Grid from Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Create another data Field for use below.
      !  Get grid from previous field and reuse it.
      fields(4) = CreateDataField2("pressure", grid2, -1.0_ESMF_KIND_R8, rc)
      write(name, *) "Creating Data Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Create a bundle for use below.
      bundle3 = ESMF_BundleCreate(2, fields(3:4), name="time step 3", rc=rc)
      write(name, *) "Creating Bundle w/ 2 Fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Validate fields before going into Regrid call.
      call ESMF_FieldValidate(fields(1), rc=rc)
      write(name, *) "Validate Fields before regridding"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !E-X_UTest
      !  Validate fields before going into Regrid call.
      call ESMF_FieldValidate(fields(3), rc=rc)
      write(name, *) "Validate Fields before regridding"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     
      !------------------------------------------------------------------------
      !!E-X_UTest
      !!  Print fields before going into Regrid call.
      !call ESMF_FieldPrint(fields(1), rc=rc)
      !write(name, *) "Print Fields before regridding"
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !!E-X_UTest
      !!  Print fields before going into Regrid call.
      !call ESMF_FieldPrint(fields(3), rc=rc)
      !write(name, *) "Print Fields before regridding"
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     
      !------------------------------------------------------------------------
      !E-X_UTest
      ! Regrid a single Field first before regridding a Bundle.
      call ESMF_FieldRegridStore(fields(1), fields(3), vm, rh, &
                                  ESMF_REGRID_METHOD_BILINEAR, rc=rc)
      write(name, *) "Precompute Regrid Communication"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Print out precomputed Route Table
      call ESMF_RouteHandlePrint(rh, rc=rc)
      write(name, *) "Printing out the contents of a Route Table"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Regrid a single Field.
      call ESMF_FieldRegrid(fields(1), fields(3), rh, rc=rc)
      write(name, *) "Regriding 1 Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Release the routehandle
      call ESMF_FieldRegridRelease(rh, rc=rc)
      write(name, *) "Release the RouteHandle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !E-X_UTest
      call ESMF_BundleRegridStore(bundle1, bundle3, vm, rh, &
                                  ESMF_REGRID_METHOD_BILINEAR, rc=rc)
      write(name, *) "Precompute Regrid Communication"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Print out precomputed Route Table
      call ESMF_RouteHandlePrint(rh, rc=rc)
      write(name, *) "Printing out the contents of a Route Table"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Regrid both fields in a single Bundle call.
      call ESMF_BundleRegrid(bundle1, bundle3, rh, rc=rc)
      write(name, *) "Regriding all Fields in a Bundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Release the routehandle
      call ESMF_BundleRegridRelease(rh, rc=rc)
      write(name, *) "Release the RouteHandle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Create another data Field for use below.
      !  Get grid from previous field and reuse it.
      fields(5) = CreateDataField3("pressure", rvalue=3.3_ESMF_KIND_R4, rc=rc)
      write(name, *) "Creating Data Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Create another data Field for use below.
      call ESMF_FieldGet(fields(5), grid=grid2, rc=rc)
      write(name, *) "Getting Grid from Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Create another data Field for use below.
      !  Get grid from previous field and reuse it.
      fields(6) = CreateDataField3("partial pressure", grid2, -1.0_ESMF_KIND_R4, rc)
      write(name, *) "Creating Data Field"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Create a bundle for use below.
      bundle4 = ESMF_BundleCreate(2, fields(5:6), name="time step 4", rc=rc)
      write(name, *) "Creating Bundle w/ 2 non-compatible Fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     
      !------------------------------------------------------------------------
      !E-X_UTest
      ! should succeed now, new code added to bundle to handle this.
      call ESMF_BundleRegridStore(bundle1, bundle4, vm, rh, &
                                  ESMF_REGRID_METHOD_BILINEAR, rc=rc)
      write(name, *) "Precompute Regrid Communication"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Print out precomputed Route Table
      call ESMF_RouteHandlePrint(rh, rc=rc)
      write(name, *) "Printing out the contents of a Route Table"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Regrid both fields in a single Bundle call.
      call ESMF_BundleRegrid(bundle1, bundle4, rh, rc=rc)
      write(name, *) "Regriding all Fields in a Bundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Release the routehandle
      call ESMF_BundleRegridRelease(rh, rc=rc)
      write(name, *) "Release the RouteHandle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !E-X_UTest
      ! should succeed now, new code added to bundle to handle this.
      call ESMF_BundleRedistStore(bundle1, bundle4, vm, rh, rc=rc)
      write(name, *) "Precompute Redist Communication"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Print out precomputed Route Table
      call ESMF_RouteHandlePrint(rh, rc=rc)
      write(name, *) "Printing out the contents of a Route Table"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Redist both fields in a single Bundle call, but looping inside
      !  because the fields are not the same data types.
      call ESMF_BundleRedist(bundle1, bundle4, rh, rc=rc)
      write(name, *) "Redist all Fields in a Bundle with a loop"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Release the routehandle
      call ESMF_BundleRegridRelease(rh, rc=rc)
      write(name, *) "Release the RouteHandle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !E-X_UTest
      ! should succeed now, new code added to bundle to handle this.
      call ESMF_BundleHaloStore(bundle4, rh, rc=rc)
      write(name, *) "Precompute Halo Communication"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Print out precomputed Route Table
      call ESMF_RouteHandlePrint(rh, rc=rc)
      write(name, *) "Printing out the contents of a Route Table"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Halo both fields in a single Bundle call, but looping inside
      !  because the fields are not the same data types.
      call ESMF_BundleHalo(bundle4, rh, rc=rc)
      write(name, *) "Halo all Fields in a Bundle with a loop"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !E-X_UTest
      !  Release the routehandle
      call ESMF_BundleRegridRelease(rh, rc=rc)
      write(name, *) "Release the RouteHandle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !!E-X_UTest
      !!  BUG - if fields are empty this dies.
      !call ESMF_BundleHaloStore(bundle2, rh, rc=rc)
      !write(name, *) "Precompute Halo Communication"
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !!E-X_UTest
      !!  Print out precomputed Route Table
      !call ESMF_RouteHandlePrint(rh, rc=rc)
      !write(name, *) "Printing out the contents of a Route Table"
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !!E-X_UTest
      !!  Halo both fields in a single Bundle call.
      !call ESMF_BundleHalo(bundle2, rh, rc=rc)
      !write(name, *) "Haloing all Fields in a Bundle"
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !!E-X_UTest
      !!  Release the routehandle
      !call ESMF_BundleHaloRelease(rh, rc=rc)
      !write(name, *) "Release the RouteHandle"
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      ! TODO: need bundle scatter test (when scatter code exists)

      !------------------------------------------------------------------------
      !E-X_UTest
      call ESMF_BundleGather(bundle1, 1, arrayList, rc=rc)
      write(name, *) "Gathering Bundle Array data to a single processor"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !E-X_UTest
      ! Clean up by deleting the Fields.
      call ESMF_FieldDestroy(fields(1), rc=rc)
      write(name, *) "Field 1 Destroy"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !E-X_UTest
      ! Clean up by deleting the Fields.
      call ESMF_FieldDestroy(fields(2), rc=rc)
      write(name, *) "Field 2 Destroy"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !E-X_UTest
      ! Clean up by deleting the Fields.
      call ESMF_FieldDestroy(fields(3), rc=rc)
      write(name, *) "Field 3 Destroy"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !E-X_UTest
      ! Clean up by deleting the Fields.
      call ESMF_FieldDestroy(fields(4), rc=rc)
      write(name, *) "Field 4 Destroy"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !E-X_UTest
      ! Clean up by deleting the Bundle.
      call ESMF_BundleDestroy(bundle1, rc=rc)
      write(name, *) "Bundle 1 Destroy"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !E-X_UTest
      ! Clean up by deleting the Bundle.
      call ESMF_BundleDestroy(bundle2, rc=rc)
      write(name, *) "Bundle 2 Destroy"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !E-X_UTest
      ! Clean up by deleting the Bundle.
      call ESMF_BundleDestroy(bundle3, rc=rc)
      write(name, *) "Bundle 3 Destroy"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !E-X_UTest
      ! Clean up by deleting the Bundle.
      call ESMF_BundleDestroy(bundle4, rc=rc)
      write(name, *) "Bundle 4 Destroy"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#endif

#endif

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------

      call ESMF_TestEnd(result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      end program ESMF_BundleCommUTest
