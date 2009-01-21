! $Id: ESMF_FieldBundleRedistUseUTest.F90,v 1.1.2.5 2009/01/21 21:25:21 cdeluca Exp $
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
      program ESMF_FieldBundleRedistUseUTest

#include "ESMF_Macros.inc"

!------------------------------------------------------------------------------
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_FieldBundleRedistUseUTest - Data redistribution tests
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
      '$Id: ESMF_FieldBundleRedistUseUTest.F90,v 1.1.2.5 2009/01/21 21:25:21 cdeluca Exp $'
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
      type(ESMF_IGrid) :: igrid(2)
      type(ESMF_Field) :: sfield(10), dfield(10)
      type(ESMF_FieldBundle) :: bundle(2)
      type(ESMF_VM) :: vm

      integer :: combined_rc
      integer :: nx, ny, nz, m, n, mprime, nprime

      real(ESMF_KIND_R8), parameter :: val_one   =  1.0
      real(ESMF_KIND_R8), parameter :: val_two   =  2.0
      real(ESMF_KIND_R8), parameter :: val_three =  3.0
      real(ESMF_KIND_R8), parameter :: val_four  =  4.0
      real(ESMF_KIND_R8), parameter :: val_five  =  5.0
      real(ESMF_KIND_R8), parameter :: val_six   =  6.0
      real(ESMF_KIND_R8), parameter :: val_seven =  7.0
      real(ESMF_KIND_R8), parameter :: val_eight =  8.0
      real(ESMF_KIND_R8), parameter :: val_nine  =  9.0
      real(ESMF_KIND_R8), parameter :: val_ten   = 10.0

      real(ESMF_KIND_R8), parameter :: val_neg_one   =  -1.0
      real(ESMF_KIND_R8), parameter :: val_neg_two   =  -2.0
      real(ESMF_KIND_R8), parameter :: val_neg_three =  -3.0
      real(ESMF_KIND_R8), parameter :: val_neg_four  =  -4.0
      real(ESMF_KIND_R8), parameter :: val_neg_five  =  -5.0
      real(ESMF_KIND_R8), parameter :: val_neg_six   =  -6.0
      real(ESMF_KIND_R8), parameter :: val_neg_seven =  -7.0
      real(ESMF_KIND_R8), parameter :: val_neg_eight =  -8.0
      real(ESMF_KIND_R8), parameter :: val_neg_nine  =  -9.0
      real(ESMF_KIND_R8), parameter :: val_neg_ten   = -10.0


      ! -------- beginning of executable code below here -------

      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
!---   
!---   
!---   
!---   Case 1:
!---   
!---   A FieldBundle with a 3D IGrid.  The IGrid corresponds to a (long, lat)
!---   igrid which is periodic in X, non-periodic in Y.    The community
!---   describes igrid sizes in (lat, long) order, confusingly enough,
!---   so a 2 x 2.5 degree igrid is 2 degrees in latitude, 2.5 in longitude,
!---   giving an overall igrid size of (144, 90).  There are 72 vertical levels.
!---   
!---   The FieldBundle contains roughly 10 Fields, each with data type Real*8,
!---   scalar, cell-centered.
!---   
!---   The redistribution goes from source to destination and back
!---   again in a ping-pong pattern. Decomposition 1 is (M by N),
!---   and decomposition 2 is (M' by N'), where (M x N) == (M' x N') + 1
!---   e.g. M = 8, N = 8, M' = 7, N' = 9.   Processor counts around 64
!---   and 256 are of interest.
!---   
!---   
!---   
!---   Case 2:
!---   
!---   A single Field, 3D IGrid, 4D data. The 4D data is Real*8,
!---   cell-centered, and sized: (long, lat, elevation, species), where
!---   long, lat, and elevation are as above (144, 90, 72), and
!---   and species is dimensioned 100.
!---   
!---   The first 3 dimensions correspond to the igrid, and there are
!---   100 data points per igrid cell.  The last fortran dimension is used
!---   for the species, so all data values for species 1 are stored together
!---   in memory, then species 2, etc.  The 2 decompositions are the
!---   same as described above.
!---   

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
 
      ! set up values to use below
      nx = 144
      ny = 90
      nz = 72

      ! these depend on npets - use these for testing
      m = 2
      n = 2
      mprime = 1
      nprime = 4
      
      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! create first igrid
      igrid(1) = CreateLatLonIGrid(nx, ny, nz, m, n, "MxN", rc=rc)
      write(name, *) "Creating igrid 1"
      write(failMsg, *) "Unable to create igrid 1"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! create fields
      call CreateFields(igrid(1), &
                        sfield(1), sfield(2), sfield(3), sfield(4), sfield(5), &
                        dim1=3, dim2=3, dim3=3, dim4=3, dim5=3, &
		        vrelloc1=ESMF_CELL_CELL, &
		        vrelloc2=ESMF_CELL_CELL, &
		        vrelloc3=ESMF_CELL_CELL, &
		        vrelloc4=ESMF_CELL_CELL, &
		        vrelloc5=ESMF_CELL_CELL, &
                        rc=rc)
      write(name, *) "Creating first 5 src fields"
      write(failMsg, *) "Unable to create first 5 src fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! create fields, cont.
      call CreateFields(igrid(1), &
                        sfield(6), sfield(7), sfield(8), sfield(9), sfield(10),&
                        dim1=3, dim2=3, dim3=3, dim4=3, dim5=3, &
		        vrelloc1=ESMF_CELL_CELL, &
		        vrelloc2=ESMF_CELL_CELL, &
		        vrelloc3=ESMF_CELL_CELL, &
		        vrelloc4=ESMF_CELL_CELL, &
		        vrelloc5=ESMF_CELL_CELL, &
                        rc=rc)
      write(name, *) "Creating last 5 src fields"
      write(failMsg, *) "Unable to create last 5 src fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! create src bundle
      call CreateFieldBundle(bundle(1), sfield(1), sfield(2), sfield(3), &
                                   sfield(4), sfield(5), rc=rc)
      write(name, *) "Creating src bundle"
      write(failMsg, *) "Unable to create src bundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! add more fields
      call AddFieldBundle(bundle(1), sfield(6), sfield(7), sfield(8), &
                                sfield(9), sfield(10), rc=rc)
      write(name, *) "Adding to src bundle"
      write(failMsg, *) "Unable to add to src bundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !NEX_removeUTest
      igrid(2) = CreateLatLonIGrid(nx, ny, nz, mprime, nprime, "M'xN'", mprime, nprime-1, rc)
      write(name, *) "Creating igrid 2"
      write(failMsg, *) "Unable to create igrid 2"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! create fields
      call CreateFields(igrid(2), &
                        dfield(1), dfield(2), dfield(3), dfield(4), dfield(5),&
                        dim1=3, dim2=3, dim3=3, dim4=3, dim5=3, &
		        vrelloc1=ESMF_CELL_CELL, &
		        vrelloc2=ESMF_CELL_CELL, &
		        vrelloc3=ESMF_CELL_CELL, &
		        vrelloc4=ESMF_CELL_CELL, &
		        vrelloc5=ESMF_CELL_CELL, &
                        rc=rc)
      write(name, *) "Creating first 5 dst fields"
      write(failMsg, *) "Unable to create first 5 dst fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! create fields, cont.
      call CreateFields(igrid(2), &
                        dfield(6), dfield(7), dfield(8), dfield(9), dfield(10),&
                        dim1=3, dim2=3, dim3=3, dim4=3, dim5=3, &
		        vrelloc1=ESMF_CELL_CELL, &
		        vrelloc2=ESMF_CELL_CELL, &
		        vrelloc3=ESMF_CELL_CELL, &
		        vrelloc4=ESMF_CELL_CELL, &
		        vrelloc5=ESMF_CELL_CELL, &
                        rc=rc)
      write(name, *) "Creating last 5 dst fields"
      write(failMsg, *) "Unable to create last 5 dst fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! create dst bundle
      call CreateFieldBundle(bundle(2), dfield(1), dfield(2), dfield(3), &
                                   dfield(4), dfield(5), rc=rc)
      write(name, *) "Creating dst bundle"
      write(failMsg, *) "Unable to create dst bundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! add more fields
      call AddFieldBundle(bundle(2), dfield(6), dfield(7), dfield(8), &
                                dfield(9), dfield(10), rc=rc)
      write(name, *) "Adding to dst bundle"
      write(failMsg, *) "Unable to add to dst bundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! fill source fields with known data
      call FillConstantR8Field(sfield(1), val_one, rc)
      write(name, *) "Filling src field 1 with constant data values"
      write(failMsg, *) "Filling src field 1 with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! fill source fields with known data
      call FillConstantR8Field(sfield(2), val_two, rc)
      write(name, *) "Filling src field 2 with constant data values"
      write(failMsg, *) "Filling src field 2 with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! fill source fields with known data
      call FillConstantR8Field(sfield(3), val_three, rc)
      write(name, *) "Filling src field 3 with constant data values"
      write(failMsg, *) "Filling src field 3 with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! fill source fields with known data
      call FillConstantR8Field(sfield(4), val_four, rc)
      write(name, *) "Filling src field 4 with constant data values"
      write(failMsg, *) "Filling src field 4 with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! fill source fields with known data
      call FillConstantR8Field(sfield(5), val_five, rc)
      write(name, *) "Filling src field 5 with constant data values"
      write(failMsg, *) "Filling src field 5 with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! fill source fields with known data
      call FillConstantR8Field(sfield(6), val_six, rc)
      write(name, *) "Filling src field 6 with constant data values"
      write(failMsg, *) "Filling src field 6 with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! fill source fields with known data
      call FillConstantR8Field(sfield(7), val_seven, rc)
      write(name, *) "Filling src field 7 with constant data values"
      write(failMsg, *) "Filling src field 7 with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! fill source fields with known data
      call FillConstantR8Field(sfield(8), val_eight, rc)
      write(name, *) "Filling src field 8 with constant data values"
      write(failMsg, *) "Filling src field 8 with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! fill source fields with known data
      call FillConstantR8Field(sfield(9), val_nine, rc)
      write(name, *) "Filling src field 9 with constant data values"
      write(failMsg, *) "Filling src field 9 with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! fill source fields with known data
      call FillConstantR8Field(sfield(10), val_ten, rc)
      write(name, *) "Filling src field 10 with constant data values"
      write(failMsg, *) "Filling src field 10 with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! fill dest fields with known data
      call FillConstantR8Field(dfield(1), val_neg_one, rc)
      write(name, *) "Filling dst field 1 with constant data values"
      write(failMsg, *) "Filling dst field 1 with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! fill dest fields with known data
      call FillConstantR8Field(dfield(2), val_neg_two, rc)
      write(name, *) "Filling dst field 2 with constant data values"
      write(failMsg, *) "Filling dst field 2 with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! fill dest fields with known data
      call FillConstantR8Field(dfield(3), val_neg_three, rc)
      write(name, *) "Filling dst field 3 with constant data values"
      write(failMsg, *) "Filling dst field 3 with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! fill dest fields with known data
      call FillConstantR8Field(dfield(4), val_neg_four, rc)
      write(name, *) "Filling dst field 4 with constant data values"
      write(failMsg, *) "Filling dst field 4 with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! fill dest fields with known data
      call FillConstantR8Field(dfield(5), val_neg_five, rc)
      write(name, *) "Filling dst field 5 with constant data values"
      write(failMsg, *) "Filling dst field 5 with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! fill dest fields with known data
      call FillConstantR8Field(dfield(6), val_neg_six, rc)
      write(name, *) "Filling dst field 6 with constant data values"
      write(failMsg, *) "Filling dst field 6 with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! fill dest fields with known data
      call FillConstantR8Field(dfield(7), val_neg_seven, rc)
      write(name, *) "Filling dst field 7 with constant data values"
      write(failMsg, *) "Filling dst field 7 with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! fill dest fields with known data
      call FillConstantR8Field(dfield(8), val_neg_eight, rc)
      write(name, *) "Filling dst field 8 with constant data values"
      write(failMsg, *) "Filling dst field 8 with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! fill dest fields with known data
      call FillConstantR8Field(dfield(9), val_neg_nine, rc)
      write(name, *) "Filling dst field 9 with constant data values"
      write(failMsg, *) "Filling dst field 9 with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! fill dest fields with known data
      call FillConstantR8Field(dfield(10), val_neg_ten, rc)
      write(name, *) "Filling dst field 10 with constant data values"
      write(failMsg, *) "Filling dst field 10 with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! store
      call ESMF_VMGetGlobal(vm, rc=rc)
      call ESMF_FieldBundleRedistStore(bundle(1), bundle(2), vm, &
        routehandle=redist_rh, routeOptions=ESMF_ROUTE_OPTION_PACK_PET, rc=rc)
      write(name, *) "Computing route for redist"
      write(failMsg, *) "Computing route for redist"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! run
      call ESMF_FieldBundleRedist(bundle(1), bundle(2), routehandle=redist_rh, rc=rc)
      write(name, *) "Executing redist"
      write(failMsg, *) "Executing redist"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! validate dest fields 
      call ValidateConstantR8Field(dfield(1), val_one, rc=rc)
      write(name, *) "Validating dst field 1"
      write(failMsg, *) "Validating dst field 1"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! validate dest fields 
      call ValidateConstantR8Field(dfield(2), val_two, rc=rc)
      write(name, *) "Validating dst field 2"
      write(failMsg, *) "Validating dst field 2"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! validate dest fields 
      call ValidateConstantR8Field(dfield(3), val_three, rc=rc)
      write(name, *) "Validating dst field 3"
      write(failMsg, *) "Validating dst field 3"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! validate dest fields 
      call ValidateConstantR8Field(dfield(4), val_four, rc=rc)
      write(name, *) "Validating dst field 4"
      write(failMsg, *) "Validating dst field 4"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! validate dest fields 
      call ValidateConstantR8Field(dfield(5), val_five, rc=rc)
      write(name, *) "Validating dst field 5"
      write(failMsg, *) "Validating dst field 5"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! validate dest fields 
      call ValidateConstantR8Field(dfield(6), val_six, rc=rc)
      write(name, *) "Validating dst field 6"
      write(failMsg, *) "Validating dst field 6"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! validate dest fields 
      call ValidateConstantR8Field(dfield(7), val_seven, rc=rc)
      write(name, *) "Validating dst field 7"
      write(failMsg, *) "Validating dst field 7"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! validate dest fields 
      call ValidateConstantR8Field(dfield(8), val_eight, rc=rc)
      write(name, *) "Validating dst field 8"
      write(failMsg, *) "Validating dst field 8"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! validate dest fields 
      call ValidateConstantR8Field(dfield(9), val_nine, rc=rc)
      write(name, *) "Validating dst field 9"
      write(failMsg, *) "Validating dst field 9"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! validate dest fields 
      call ValidateConstantR8Field(dfield(10), val_ten, rc=rc)
      write(name, *) "Validating dst field 10"
      write(failMsg, *) "Validating dst field 10"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------

#if ESMF_TESTEXHAUSTIVE

      !------------------------------------------------------------------------
      ! create a igrid with data only on DE (1,1)
      ! 
      ! redistribute it to a igrid on all DEs
      !
      !
      ! fill source fields with index data  (previous tests used constant data)
      !
      ! fill destination fields with dummy data
      !
      ! run redist (store needed first?)
      !
      ! validate dest fields for index data
      !
      ! validate halo regions are unchanged 
      !
      ! redist back from dest to src 
      !
      ! validate src for index data
      !
      ! validate halo regions are unchanged 
      !------------------------------------------------------------------------

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
      call FieldBundleCleanup(bundle(1), bundle(2), rc=rc)
      write(name, *) "Deleting bundles at cleanup time"
      write(failMsg, *) "Deleting bundles at cleanup time"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! cleanup
      combined_rc = ESMF_SUCCESS
      call FieldCleanup(sfield(1), sfield(2), sfield(3), sfield(4), &
                        sfield(5), .FALSE., rc=rc)
      if (rc .ne. ESMF_SUCCESS) combined_rc = rc
      call FieldCleanup(sfield(6), sfield(7), sfield(8), sfield(9), &
                        sfield(10), rc=rc)
      if (rc .ne. ESMF_SUCCESS) combined_rc = rc
      call FieldCleanup(dfield(1), dfield(2), dfield(3), dfield(4), &
                        dfield(5), .FALSE., rc=rc)
      if (rc .ne. ESMF_SUCCESS) combined_rc = rc
      call FieldCleanup(dfield(6), dfield(7), dfield(8), dfield(9), &
                        dfield(10), rc=rc)
      if (rc .ne. ESMF_SUCCESS) combined_rc = rc
      write(name, *) "Deleting fields at cleanup time"
      write(failMsg, *) "Deleting fields at cleanup time"
      call ESMF_Test((combined_rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


10    continue

      call ESMF_TestEnd(result, ESMF_SRCLINE)
  
      ! -------- end of unit test code ------------------------

      end program ESMF_FieldBundleRedistUseUTest



