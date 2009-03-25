! $Id: ESMF_AttReadGridCompUTest.F90,v 1.1 2009/03/25 05:53:54 eschwab Exp $
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
program ESMF_AttReadGridCompUTest

!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_AttReadGridCompUTest - AttributeRead GridComp Unit Tests
!
! !DESCRIPTION:
!
! The code in this file drives F90 AttributeRead GridComp unit tests.
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
      '$Id: ESMF_AttReadGridCompUTest.F90,v 1.1 2009/03/25 05:53:54 eschwab Exp $'
!------------------------------------------------------------------------------

!-------------------------------------------------------------------------
!=========================================================================

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

      ! local variables
      type(ESMF_GridComp)    :: gridcomp
      character(ESMF_MAXSTR) :: attrname, attrvalue, outChar
!      character(ESMF_MAXSTR) :: conv, purp
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
      
      ! gridded component
      gridcomp = ESMF_GridCompCreate(name="gridcomp", petList=(/0/), rc=rc)

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
      ! Read an XML file to populate the Attribute package of a GridComp Test
      call ESMF_AttributeRead(comp=gridcomp, fileName="esmf_gridcomp.xml", rc=rc)
      if (rc==ESMF_RC_LIB_NOT_PRESENT) then
        xercesNotPresent = .true.
      endif
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Reading an XML Attribute file for a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS.or.xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc

#ifdef ESMF_TESTEXHAUSTIVE
    !-------------------------------------------------------------------------
    !  Check read-in Attributes
    !-------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get ESG "name" Attribute from a GridComp Test
      attrname = 'name'
      attrvalue = 'GEOS'
      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, rc=rc)
!      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, &
!                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'name' Attribute from a GridComp Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get ESG "full_name" Attribute from a GridComp Test
      attrname = 'full_name'
      attrvalue = 'Goddard Earth Observing System Model'
      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, rc=rc)
!      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, &
!                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'full_name' Attribute from a GridComp Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get ESG "agency" Attribute from a GridComp Test
      attrname = 'agency'
      attrvalue = 'NASA'
      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, rc=rc)
!      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, &
!                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'agency' Attribute from a GridComp Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get ESG "institution" Attribute from a GridComp Test
      attrname = 'institution'
      attrvalue = 'Global Modeling and Assimilation Office (GMAO)'
      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, rc=rc)
!      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, &
!                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'institution' Attribute from a GridComp Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get ESG "version" Attribute from a GridComp Test
      attrname = 'version'
      attrvalue = '5'
      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, rc=rc)
!      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, &
!                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'version' Attribute from a GridComp Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get ESG "author" Attribute from a GridComp Test
      attrname = 'author'
      attrvalue = 'Max Suarez'
      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, rc=rc)
!      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, &
!                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'author' Attribute from a GridComp Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get ESG "discipline" Attribute from a GridComp Test
      attrname = 'discipline'
      attrvalue = 'Atmosphere'
      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, rc=rc)
!      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, &
!                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'discipline' Attribute from a GridComp Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get ESG "physical_domain" Attribute from a GridComp Test
      attrname = 'physical_domain'
      attrvalue = 'Earth System'
      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, rc=rc)
!      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, &
!                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'physical_domain' Attribute from a GridComp Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get ESG "coding_language" Attribute from a GridComp Test
      attrname = 'coding_language'
      attrvalue = 'Fortran 90'
      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, rc=rc)
!      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, &
!                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'coding_language' Attribute from a GridComp Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get ESG "model_component_framework" Attribute from a GridComp Test
      attrname = 'model_component_framework'
      attrvalue = 'ESMF (Earth System Modeling Framework)'
      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, rc=rc)
!      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, &
!                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'model_component_framework' Attribute from a GridComp Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get ESG "comment" Attribute from a GridComp Test
      attrname = 'comment'
      attrvalue = 'ESMF GridComp Attribute IO Test'
      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, rc=rc)
!      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, &
!                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'comment' Attribute from a GridComp Test"
      call ESMF_Test(((rc==ESMF_SUCCESS .and. outChar==attrvalue) &
                      .or. xercesNotPresent), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get ESG "references" Attribute from a GridComp Test
      attrname = 'references'
      attrvalue = 'http://gmao.gsfc.nasa.gov/systems/geos5'
      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, rc=rc)
!      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, &
!                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'references' Attribute from a GridComp Test"
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
      call ESMF_GridCompDestroy(gridcomp, rc=rc)

      if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
end program ESMF_AttReadGridCompUTest
