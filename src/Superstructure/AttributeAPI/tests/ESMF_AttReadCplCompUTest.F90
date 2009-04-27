! $Id: ESMF_AttReadCplCompUTest.F90,v 1.2 2009/04/27 05:55:47 eschwab Exp $
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
      use ESMF_Mod         ! the ESMF Framework

      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_AttReadCplCompUTest.F90,v 1.2 2009/04/27 05:55:47 eschwab Exp $'
!------------------------------------------------------------------------------

!-------------------------------------------------------------------------
!=========================================================================

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

      ! local variables
      type(ESMF_CplComp)    :: cplcomp
      character(ESMF_MAXSTR) :: attrname, attrvalue, outChar
      character(ESMF_MAXSTR) :: conv, purp
      logical                :: xercesNotPresent
      integer                :: rc

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

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
      
      ! coupler component
      cplcomp = ESMF_CplCompCreate(name="cplcomp", petList=(/0/), rc=rc)

! TODO:  resolve
print *, "this print statement prevents mpi abort!"

      if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

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
      purp = 'general'

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get ESG "name" Attribute from a CplComp Test
      attrname = 'name'
      attrvalue = 'ESMF Example Coupler'
      call ESMF_AttributeGet(cplcomp, name=attrname, value=outChar, &
                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'name' Attribute from a CplComp Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', trim(adjustL(outChar))

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get ESG "full_name" Attribute from a CplComp Test
      attrname = 'full_name'
      attrvalue = 'Earth System Modeling Framework Example Coupler'
      call ESMF_AttributeGet(cplcomp, name=attrname, value=outChar, &
                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'full_name' Attribute from a CplComp Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get ESG "agency" Attribute from a CplComp Test
      attrname = 'agency'
      attrvalue = 'UCAR'
      call ESMF_AttributeGet(cplcomp, name=attrname, value=outChar, &
                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'agency' Attribute from a CplComp Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get ESG "institution" Attribute from a CplComp Test
      attrname = 'institution'
      attrvalue = 'National Center for Atmospheric Research (NCAR)'
      call ESMF_AttributeGet(cplcomp, name=attrname, value=outChar, &
                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'institution' Attribute from a CplComp Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get ESG "version" Attribute from a CplComp Test
      attrname = 'version'
      attrvalue = '4'
      call ESMF_AttributeGet(cplcomp, name=attrname, value=outChar, &
                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'version' Attribute from a CplComp Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get ESG "author" Attribute from a CplComp Test
      attrname = 'author'
      attrvalue = 'Cecelia Deluca, et al.'
      call ESMF_AttributeGet(cplcomp, name=attrname, value=outChar, &
                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'author' Attribute from a CplComp Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get ESG "discipline" Attribute from a CplComp Test
      attrname = 'discipline'
      attrvalue = 'Atmosphere, Ocean, and Land'
      call ESMF_AttributeGet(cplcomp, name=attrname, value=outChar, &
                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'discipline' Attribute from a CplComp Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get ESG "physical_domain" Attribute from a CplComp Test
      attrname = 'physical_domain'
      attrvalue = 'Earth System'
      call ESMF_AttributeGet(cplcomp, name=attrname, value=outChar, &
                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'physical_domain' Attribute from a CplComp Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get ESG "coding_language" Attribute from a CplComp Test
      attrname = 'coding_language'
      attrvalue = 'Fortran 90'
      call ESMF_AttributeGet(cplcomp, name=attrname, value=outChar, &
                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'coding_language' Attribute from a CplComp Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get ESG "model_component_framework" Attribute from a CplComp Test
      attrname = 'model_component_framework'
      attrvalue = 'ESMF (Earth System Modeling Framework)'
      call ESMF_AttributeGet(cplcomp, name=attrname, value=outChar, &
                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'model_component_framework' Attribute from a CplComp Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get ESG "comment" Attribute from a CplComp Test
      attrname = 'comment'
      attrvalue = 'ESMF CplComp Attribute IO Test'
      conv = 'CF'
      call ESMF_AttributeGet(cplcomp, name=attrname, value=outChar, &
                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting CF 'comment' Attribute from a CplComp Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get ESG "references" Attribute from a CplComp Test
      attrname = 'references'
      attrvalue = 'http://www.esmf.ucar.edu'
      conv = 'CF'
      call ESMF_AttributeGet(cplcomp, name=attrname, value=outChar, &
                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting CF 'references' Attribute from a CplComp Test"
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

      if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
end program ESMF_AttReadCplCompUTest
