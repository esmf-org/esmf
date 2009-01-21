! $Id: ESMF_HaloUTest.F90,v 1.6.2.3 2009/01/21 21:25:23 cdeluca Exp $
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
      program ESMF_HaloUTest

#include "ESMF_Macros.inc"

!------------------------------------------------------------------------------
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_HaloUTest - Data halo tests
!
! !DESCRIPTION:
!
! The code in this file drives F90 Halo unit tests, using the Route code.
!
!  "Halo" is sending boundary data from one field to another, where the 
!   overlapping cells are treated as read-only by the receiver, and are
!   then available for computations involving boundary cells in the receiver.
!   These cells are known variously as ghost zones or halo regions.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod
      use ESMF_HaloHelpers
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_HaloUTest.F90,v 1.6.2.3 2009/01/21 21:25:23 cdeluca Exp $'
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
      type(ESMF_RouteHandle) :: halo_rh
      type(ESMF_Field) :: field1, field2
      real(ESMF_KIND_R8) :: val1, val2


      ! -------- beginning of executable code below here -------

      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! create fields for later on
      call CreateFields(field1, field2, rc)
      write(name, *) "Creating src and dest fields"
      write(failMsg, *) "Unable to create src and/or dst fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! fill data region of source field with known data
      val1 = 1.0
      call FillConstantField(field1, val1, rc)
      write(name, *) "Filling field with constant data values"
      write(failMsg, *) "Filling field with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! fill halo region of source field with known data
      val2 = -1.0
      call FillConstantHalo(field1, val2, rc)
      write(name, *) "Filling halo with constant data values"
      write(failMsg, *) "Filling halo with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! store
      call ESMF_FieldHaloStore(field1, routehandle=halo_rh, rc=rc)
      write(name, *) "Computing route for halo"
      write(failMsg, *) "Computing route for halo"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! run
      call ESMF_FieldHalo(field1, routehandle=halo_rh, rc=rc)
      write(name, *) "Executing halo"
      write(failMsg, *) "Executing halo"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !call ESMF_FieldPrint(field1, rc=rc)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! validate resulting field
      call ValidateConstantField(field1, val1, rc)
      write(name, *) "Validating constant data in dest fields"
      write(failMsg, *) "Validating constant data in dest fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#if ESMF_TESTEXHAUSTIVE
      !------------------------------------------------------------------------
      !EX_removeUTest
      ! fill computational area of field with known data
      call FillIndexField(field1, rc)
      write(name, *) "Filling src field with indexed data values"
      write(failMsg, *) "Filling src field with indexed data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! fill halo region with constant value
      val2 = -1.0
      call FillConstantHalo(field1, val2, rc)
      write(name, *) "Filling halo with constant data values"
      write(failMsg, *) "Filling halo with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! run
      call ESMF_FieldHalo(field1, routehandle=halo_rh, rc=rc)
      write(name, *) "Executing halo 2"
      write(failMsg, *) "Executing halo 2"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !call ESMF_FieldPrint(field1, rc=rc)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! validate field, data regions only
      call ValidateIndexField(field1, rc)
      write(name, *) "Validating indexed data in dest fields"
      write(failMsg, *) "Validating indexed data in dest fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      call ValidateIndexHalo(field1, val2, rc)
      write(name, *) "Validating halo area in dest fields"
      write(failMsg, *) "Validating indexed data in dest fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! release first route handle, compute another below
      call ESMF_FieldHaloRelease(halo_rh, rc=rc)
      write(name, *) "Releasing route"
      write(failMsg, *) "Releasing route"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! fill source field with known data
      val1 = 1.0
      call FillConstantField(field2, val1, rc)
      write(name, *) "Filling src field with constant data values"
      write(failMsg, *) "Filling src field with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! fill destination field with known data
      val2 = -1.0
      call FillConstantHalo(field2, val2, rc)
      write(name, *) "Filling halo with constant data values"
      write(failMsg, *) "Filling halo with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! store
      call ESMF_FieldHaloStore(field2, routehandle=halo_rh, rc=rc)
      write(name, *) "Computing route for halo, 2 to 1"
      write(failMsg, *) "Computing route for halo, 2 to 1"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! run
      call ESMF_FieldHalo(field2, routehandle=halo_rh, rc=rc)
      write(name, *) "Executing halo"
      write(failMsg, *) "Executing halo"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !call ESMF_FieldPrint(field2, rc=rc)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! validate destination field
      call ValidateConstantField(field2, val1, rc)
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
      ! fill halo area with known data
      val2 = -1.0
      call FillConstantHalo(field2, val2, rc)
      write(name, *) "Filling dst field with constant data values"
      write(failMsg, *) "Filling dst field with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! run
      call ESMF_FieldHalo(field2, routehandle=halo_rh, rc=rc)
      write(name, *) "Executing halo 2 -> 1"
      write(failMsg, *) "Executing halo 2 -> 1"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !call ESMF_FieldPrint(field2, rc=rc)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! validate destination field
      call ValidateIndexField(field2, rc)
      write(name, *) "Validating indexed data in dest fields"
      write(failMsg, *) "Validating indexed data in dest fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      call ValidateIndexHalo(field2, val2, rc)
      write(name, *) "Validating halo area in dest fields"
      write(failMsg, *) "Validating indexed data in dest fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#endif

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! release
      call ESMF_FieldHaloRelease(halo_rh, rc=rc)
      write(name, *) "Releasing route"
      write(failMsg, *) "Releasing route"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! cleanup
      call Cleanup(field1, field2, rc)
      write(name, *) "Deleting fields at cleanup time"
      write(failMsg, *) "Deleting fields at cleanup time"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


10    continue

      call ESMF_TestEnd(result, ESMF_SRCLINE)
  
      ! -------- end of unit test code ------------------------

      end program ESMF_HaloUTest
