! $Id: ESMF_AttReadFieldUTest.F90,v 1.4.2.1 2010/02/05 20:03:39 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_AttReadFieldUTest

!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_AttReadFieldUTest - AttributeRead Field Unit Tests
!
! !DESCRIPTION:
!
! The code in this file drives F90 AttributeRead Field unit tests.
! The companion file ESMF\_Attribute.F90 contains the definitions for the
! AttributeRead methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod         ! the ESMF Framework

      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_AttReadFieldUTest.F90,v 1.4.2.1 2010/02/05 20:03:39 svasquez Exp $'
!------------------------------------------------------------------------------

!-------------------------------------------------------------------------
!=========================================================================

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

      ! local variables
      type(ESMF_Field)       :: field
      logical                :: xercesNotPresent
      integer                :: rc

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0
#ifdef ESMF_TESTEXHAUSTIVE
      character(ESMF_MAXSTR) :: attrname, attrvalue, outChar
      character(ESMF_MAXSTR) :: conv, purp

#endif

!-------------------------------------------------------------------------------
!  The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!  always run. When the environment variable, EXHAUSTIVE, is set to ON then
!  the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!  to OFF, then only the sanity unit tests.
!  Special strings (Non-exhaustive and exhaustive) have been
!  added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  !-----------------------------------------------------------------------------

      !------------------------------------------------------------------------
      ! Preparations
      !------------------------------------------------------------------------
      
      ! create a field
      field = ESMF_FieldCreateEmpty(name="field", rc=rc)

! TODO:  resolve
print *, "this print statement prevents mpi abort!"

      if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

      ! assume Xerces XML C++ API library present until proven otherwise
      xercesNotPresent = .false.


    !-------------------------------------------------------------------------
    !  Read an XML file containing Attributes into a field
    !-------------------------------------------------------------------------

      !NEX_UTest
      ! Read an XML file to populate the Attribute package of a Field Test
      call ESMF_AttributeRead(field=field, fileName="esmf_field.xml", rc=rc)
      if (rc==ESMF_RC_LIB_NOT_PRESENT) then
        xercesNotPresent = .true.
      endif
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Reading an XML Attribute file for a Field Test"
      call ESMF_Test((rc==ESMF_SUCCESS.or.xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc

#ifdef ESMF_TESTEXHAUSTIVE
    !-------------------------------------------------------------------------
    !  Check read-in Attributes
    !-------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get CF "Name" Attribute from a Field Test
      attrname = 'Name'
      attrvalue = 'DPEDT'
      conv = 'CF'
      purp = 'General'
      call ESMF_AttributeGet(field, name=attrname, value=outChar, &
                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting CF 'Name' Attribute from a Field Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', trim(outChar)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get CF "StandardName" Attribute from a Field Test
      attrname = 'StandardName'
      attrvalue = 'tendency_of_air_pressure'
      conv = 'CF'
      purp = 'Extended'
      call ESMF_AttributeGet(field, name=attrname, value=outChar, &
                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting CF 'StandardName' Attribute from a Field Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get CF "LongName" Attribute from a Field Test
      attrname = 'LongName'
      attrvalue = 'Edge pressure tendency'
      conv = 'CF'
      purp = 'General'
      call ESMF_AttributeGet(field, name=attrname, value=outChar, &
                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting CF 'LongName' Attribute from a Field Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get CF "Units" Attribute from a Field Test
      attrname = 'Units'
      attrvalue = 'Pa s-1'
      conv = 'CF'
      purp = 'General'
      call ESMF_AttributeGet(field, name=attrname, value=outChar, &
                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting CF 'Units' Attribute from a Field Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
#endif

      !------------------------------------------------------------------------
      ! clean up      
      !------------------------------------------------------------------------
      call ESMF_FieldDestroy(field, rc=rc)

      if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
end program ESMF_AttReadFieldUTest
