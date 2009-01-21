! $Id: ESMF_RedistUTest.F90,v 1.12.2.4 2009/01/21 21:25:23 cdeluca Exp $
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
#include "ESMF.h"

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
      use ESMF_RedistHelpers   ! helper subroutines
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_RedistUTest.F90,v 1.12.2.4 2009/01/21 21:25:23 cdeluca Exp $'
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
      type(ESMF_RouteHandle) :: redist_rh, redist_rh1
      type(ESMF_Field) :: field1, field2
      type(ESMF_VM) :: vm
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
      ! fill source field with known data
      val1 = 1.0
      call FillConstantField(field1, val1, rc)
      write(name, *) "Filling src field with constant data values"
      write(failMsg, *) "Filling src field with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! fill destination field with known data
      val2 = -1.0
      call FillConstantField(field2, val2, rc)
      write(name, *) "Filling dst field with constant data values"
      write(failMsg, *) "Filling dst field with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! store
      call ESMF_VMGetGlobal(vm, rc=rc)
      call ESMF_FieldRedistStore(field1, field2, vm, &
                                                routehandle=redist_rh, rc=rc)
      write(name, *) "Computing route for redist"
      write(failMsg, *) "Computing route for redist"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! run
      call ESMF_FieldRedist(field1, field2, routehandle=redist_rh, rc=rc)
      write(name, *) "Executing redist"
      write(failMsg, *) "Executing redist"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! validate destination field
      call ValidateConstantField(field2, val1, rc)
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
      ! fill destination field with known data
      val2 = -1.0
      call FillConstantField(field2, val2, rc)
      write(name, *) "Filling dst field with constant data values"
      write(failMsg, *) "Filling dst field with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! run
      call ESMF_FieldRedist(field1, field2, routehandle=redist_rh, rc=rc)
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
      ! validate halo regions in destination field - should be unchanged
      call ValidateConstantHalo(field2, val2, rc)
      write(name, *) "Validating halo area in dest fields"
      write(failMsg, *) "Validating indexed data in dest fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! release first route handle, compute another below
      call ESMF_FieldRedistRelease(redist_rh, rc=rc)
      write(name, *) "Releasing route"
      write(failMsg, *) "Releasing route"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! release previously released routehandle
      call ESMF_FieldRedistRelease(redist_rh, rc=rc)
      write(name, *) "Releasing previously released route test"
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !EX_removeUTest
      ! release uncreated route handle
      call ESMF_FieldRedistRelease(redist_rh1, rc=rc)
      write(name, *) "Releasing uncreated Routehandle"
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

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
      call FillConstantField(field1, val2, rc)
      write(name, *) "Filling dst field with constant data values"
      write(failMsg, *) "Filling dst field with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! store
      call ESMF_VMGetGlobal(vm, rc=rc)
      call ESMF_FieldRedistStore(field2, field1, vm, &
                                                routehandle=redist_rh, rc=rc)
      write(name, *) "Computing route for redist, 2 to 1"
      write(failMsg, *) "Computing route for redist, 2 to 1"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! run
      call ESMF_FieldRedist(field2, field1, routehandle=redist_rh, rc=rc)
      write(name, *) "Executing redist"
      write(failMsg, *) "Executing redist"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! validate destination field
      call ValidateConstantField(field1, val1, rc)
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
      val2 = -1.0
      call FillConstantField(field1, val2, rc)
      write(name, *) "Filling dst field with constant data values"
      write(failMsg, *) "Filling dst field with constant data values"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! run
      call ESMF_FieldRedist(field2, field1, routehandle=redist_rh, rc=rc)
      write(name, *) "Executing redist 2 -> 1"
      write(failMsg, *) "Executing redist 2 -> 1"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! validate destination field
      call ValidateIndexField(field1, rc)
      write(name, *) "Validating indexed data in dest fields"
      write(failMsg, *) "Validating indexed data in dest fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! validate halo regions in destination field - should be unchanged
      call ValidateConstantHalo(field1, val2, rc)
      write(name, *) "Validating halo area in dest fields"
      write(failMsg, *) "Validating indexed data in dest fields"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#endif

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! release
      call ESMF_FieldRedistRelease(redist_rh, rc=rc)
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

      end program ESMF_RedistUTest



