! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_AttReadCplCompUTest

!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_AttReadCplCompUTest - AttributeRead CplComp Unit Tests
!
! !DESCRIPTION:
!
! The code in this file drives F90 AttributeRead CplComp unit tests.
! The companion file ESMF\_Attribute.F90 contains the definitions for the
! AttributeRead methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF         ! the ESMF Framework

      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id$'
!------------------------------------------------------------------------------

!-------------------------------------------------------------------------
!=========================================================================

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

      ! local variables
      type(ESMF_CplComp)     :: cplcomp
      type(ESMF_AttPack)     :: attpack
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
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !-----------------------------------------------------------------------------

      !------------------------------------------------------------------------
      ! Preparations
      !------------------------------------------------------------------------
      
      ! coupler component
      cplcomp = ESMF_CplCompCreate(name="cplcomp", petList=(/0/), rc=rc)

      if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      ! assume Xerces XML C++ API library present until proven otherwise
      xercesNotPresent = .false.

!-------------------------------------------------------------------------
!  GRIDCOMP
!-------------------------------------------------------------------------


    !-------------------------------------------------------------------------
    !  Read an XML file containing Attributes
    !-------------------------------------------------------------------------

      !NEX_UTest
      ! Read an XML file to populate the Attribute package of a CplComp Test
      call ESMF_AttributeRead(comp=cplcomp, fileName="esmf_cplcomp.xml", rc=rc)
      if (rc==ESMF_RC_LIB_NOT_PRESENT) then
        xercesNotPresent = .true.
      endif
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Reading an XML Attribute file for a CplComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS.or.xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc

#ifdef ESMF_TESTEXHAUSTIVE
    !-------------------------------------------------------------------------
    !  Check read-in Attributes
    !-------------------------------------------------------------------------

      conv = 'ESG'
      purp = 'General'

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get ESG General Attribute package from a CplComp Test
      call ESMF_AttributeGetAttPack(cplcomp, attpack=attpack, &
                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Get ESG General Attribute package from a CplComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS &
                     .or. xercesNotPresent), &
                     name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get ESG "ComponentShortName" Attribute from a CplComp Test
      attrname = 'ComponentShortName'
      attrvalue = 'ESMF Example Coupler'
      call ESMF_AttributeGet(cplcomp, name=attrname, value=outChar, &
                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'ComponentShortName' Attribute from a CplComp Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', trim(adjustL(outChar))

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get ESG "ComponentLongName" Attribute from a CplComp Test
      attrname = 'ComponentLongName'
      attrvalue = 'Earth System Modeling Framework Example Coupler'
      call ESMF_AttributeGet(cplcomp, name=attrname, value=outChar, &
                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'ComponentLongName' Attribute from a CplComp Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get ESG "Agency" Attribute from a CplComp Test
      attrname = 'Agency'
      attrvalue = 'UCAR'
      call ESMF_AttributeGet(cplcomp, name=attrname, value=outChar, &
                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'Agency' Attribute from a CplComp Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get ESG "Institution" Attribute from a CplComp Test
      attrname = 'Institution'
      attrvalue = 'National Center for Atmospheric Research (NCAR)'
      call ESMF_AttributeGet(cplcomp, name=attrname, value=outChar, &
                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'Institution' Attribute from a CplComp Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get ESG "Version" Attribute from a CplComp Test
      attrname = 'Version'
      attrvalue = '4'
      call ESMF_AttributeGet(cplcomp, name=attrname, value=outChar, &
                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'Version' Attribute from a CplComp Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get ESG "Author" Attribute from a CplComp Test
      attrname = 'Author'
      attrvalue = 'Cecelia Deluca, et al.'
      call ESMF_AttributeGet(cplcomp, name=attrname, value=outChar, &
                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'Author' Attribute from a CplComp Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get ESG "Discipline" Attribute from a CplComp Test
      attrname = 'Discipline'
      attrvalue = 'Atmosphere, Ocean, and Land'
      call ESMF_AttributeGet(cplcomp, name=attrname, value=outChar, &
                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'Discipline' Attribute from a CplComp Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get ESG "PhysicalDomain" Attribute from a CplComp Test
      attrname = 'PhysicalDomain'
      attrvalue = 'Earth System'
      call ESMF_AttributeGet(cplcomp, name=attrname, value=outChar, &
                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'PhysicalDomain' Attribute from a CplComp Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get ESG "CodingLanguage" Attribute from a CplComp Test
      attrname = 'CodingLanguage'
      attrvalue = 'Fortran 90'
      call ESMF_AttributeGet(cplcomp, name=attrname, value=outChar, &
                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'CodingLanguage' Attribute from a CplComp Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get ESG "ModelComponentFramework" Attribute from a CplComp Test
      attrname = 'ModelComponentFramework'
      attrvalue = 'ESMF (Earth System Modeling Framework)'
      call ESMF_AttributeGet(cplcomp, name=attrname, value=outChar, &
                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'ModelComponentFramework' Attribute from a CplComp Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

#endif

      !------------------------------------------------------------------------
      ! clean up      
      !------------------------------------------------------------------------
      call ESMF_CplCompDestroy(cplcomp, rc=rc)

      if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
end program ESMF_AttReadCplCompUTest
