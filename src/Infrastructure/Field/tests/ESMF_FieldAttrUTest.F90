! $Id: ESMF_FieldAttrUTest.F90,v 1.3 2008/03/26 03:50:27 rokuingh Exp $
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
program ESMF_FieldAttrUTest

!------------------------------------------------------------------------------
!
#include <ESMF.h>

!==============================================================================
!BOP
! !PROGRAM: ESMF_FieldAttrTest - Field Attribute Unit Tests
!
! !DESCRIPTION:
!
! The code in this file drives F90 Field Attribute unit tests.
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
      '$Id: ESMF_FieldAttrUTest.F90,v 1.3 2008/03/26 03:50:27 rokuingh Exp $'
!------------------------------------------------------------------------------

!-------------------------------------------------------------------------
!=========================================================================

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

      type(ESMF_Field) :: field
      character(ESMF_MAXSTR) :: conv, purp, attrname, attrvalue
      character(ESMF_MAXSTR), dimension(3) :: attrList
      integer :: rc, count, number, defaultvalue

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

!-------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  !-----------------------------------------------------------------------------

#ifdef ESMF_EXHAUSTIVE

      !------------------------------------------------------------------------
      ! preparations
      field = ESMF_FieldCreateEmpty(name="field 1", rc=rc)
      if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

      !EX_UTest
      ! Add an integer attribute to a Field Test
      call ESMF_FieldAttributeSet(field, name="Sides", value=65, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an integer attribute to a Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an integer attribute from a Field Test
      call ESMF_FieldAttributeGet(field, name="Sides", value=number, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an integer attribute from a Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(number.eq.65), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      defaultvalue = 7;

      !EX_UTest
      ! Get an integer attribute from a Field Test
      call ESMF_FieldAttributeGet(field, name="NotSides", value=number, &
        defaultvalue=defaultvalue, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default integer attribute from a Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(number.eq.7), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an integer attribute from a Field Test
      call ESMF_FieldAttributeGetInfo(field, name="Sides", count=number, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an attribute info from a Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(number.eq.1), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      conv = "defaultconvention"
      purp = "defaultpurpose"

      !EX_UTest
      ! Create an attribute package on a Field Test
      call ESMF_FieldAttPackCreate(field, convention=conv, &
        purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating an Attpack on a Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      attrList(1) = "Custom1"
      attrList(2) = "Custom2"
      attrList(3) = "Custom3"
      count = 3
      conv = "customconvention"
      purp = "custompurpose"
      
      !EX_UTest
      ! Create a custom attribute package on a Field Test
      call ESMF_FieldAttPackCreate(field, convention=conv, &
        purpose=purp, attrList=attrList, count=count, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a custom Attpack on a Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      attrname = "units"
      attrvalue = "m/s"
      conv = "defaultconvention"
      purp = "defaultpurpose"

      !EX_UTest
      ! Set an attribute in an attribute package on a Field Test
      call ESMF_FieldAttPackSet(field, name=attrname, value=attrvalue, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an Attribute in an Attpack from a Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Write the attribute package from a Field Test
      call ESMF_FieldAttPackWrite(field, convention=conv, &
        purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Writing an Attpack from a Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Getting Attribute count from a Field
      call ESMF_FieldAttributeGetCount(field, count, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Attribute Count from a Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      !------------------------------------------------------------------------
      ! clean up
      call ESMF_FieldDestroy(field, rc=rc)
      if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

#endif

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

end program ESMF_FieldAttrUTest

