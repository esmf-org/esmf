! $Id: ESMF_IOGridCompUTest.F90,v 1.2 2009/03/10 05:48:08 eschwab Exp $
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
program ESMF_IOGridCompUTest

!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_IOGridCompUTest - IO GridComp Unit Tests
!
! !DESCRIPTION:
!
! The code in this file drives F90 IO GridComp unit tests.
! The companion file ESMF\_IO.F90 contains the definitions for the
! IO methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod         ! the ESMF Framework
      use ESMF_IOMod       ! TODO: integrate into ESMF_Mod when build
                           !   dependency is resolved
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_IOGridCompUTest.F90,v 1.2 2009/03/10 05:48:08 eschwab Exp $'
!------------------------------------------------------------------------------

!-------------------------------------------------------------------------
!=========================================================================

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

      ! local variables
      type(ESMF_GridComp)    :: gridcomp
      type(ESMF_IO)          :: io
      character(ESMF_MAXSTR) :: attrname, attrvalue, outChar
!      character(ESMF_MAXSTR) :: conv, purp
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

      ! io for gridded component
      io = ESMF_IOCreate(name="io", gridComp=gridComp, rc=rc)
      
      if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!-------------------------------------------------------------------------
!  GRIDCOMP
!-------------------------------------------------------------------------

#ifdef ESMF_TESTEXHAUSTIVE

#if 0
    !-------------------------------------------------------------------------
    !  Add Attribute package - standard
    !-------------------------------------------------------------------------
      conv = 'ESG'
      purp = 'general'
      
      !EX__UTest
      ! Create an Attribute package on a GridComp Test
      call ESMF_AttributeAdd(gridcomp, convention=conv, purpose=purp, &
        attpacknestflag=ESMF_ATTPACKNEST_OFF, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a standard Attribute package on a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
#endif

    !-------------------------------------------------------------------------
    !  Read an XML file containing Attributes
    !-------------------------------------------------------------------------

      !EX__UTest
      ! Read an XML file to populate the Attribute package of a GridComp Test
      call ESMF_IORead(io, "esmf_gridcomp.xml", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Reading an XML Attribute file for a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !  Check read-in Attributes
    !-------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !EX__UTest
      ! Get ESG "name" Attribute from a GridComp Test
      attrname = 'name'
      attrvalue = 'GEOS'
      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, rc=rc)
!      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, &
!                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'name' Attribute from a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(outChar==attrvalue), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX__UTest
      ! Get ESG "full_name" Attribute from a GridComp Test
      attrname = 'full_name'
      attrvalue = 'Goddard Earth Observing System Model'
      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, rc=rc)
!      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, &
!                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'full_name' Attribute from a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(outChar==attrvalue), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX__UTest
      ! Get ESG "agency" Attribute from a GridComp Test
      attrname = 'agency'
      attrvalue = 'NASA'
      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, rc=rc)
!      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, &
!                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'agency' Attribute from a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(outChar==attrvalue), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX__UTest
      ! Get ESG "institution" Attribute from a GridComp Test
      attrname = 'institution'
      attrvalue = 'Global Modeling and Assimilation Office (GMAO)'
      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, rc=rc)
!      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, &
!                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'institution' Attribute from a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(outChar==attrvalue), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX__UTest
      ! Get ESG "version" Attribute from a GridComp Test
      attrname = 'version'
      attrvalue = '5'
      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, rc=rc)
!      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, &
!                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'version' Attribute from a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(outChar==attrvalue), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX__UTest
      ! Get ESG "author" Attribute from a GridComp Test
      attrname = 'author'
      attrvalue = 'Max Suarez'
      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, rc=rc)
!      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, &
!                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'author' Attribute from a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(outChar==attrvalue), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX__UTest
      ! Get ESG "discipline" Attribute from a GridComp Test
      attrname = 'discipline'
      attrvalue = 'Atmosphere'
      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, rc=rc)
!      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, &
!                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'discipline' Attribute from a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(outChar==attrvalue), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX__UTest
      ! Get ESG "physical_domain" Attribute from a GridComp Test
      attrname = 'physical_domain'
      attrvalue = 'Earth System'
      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, rc=rc)
!      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, &
!                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'physical_domain' Attribute from a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(outChar==attrvalue), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX__UTest
      ! Get ESG "coding_language" Attribute from a GridComp Test
      attrname = 'coding_language'
      attrvalue = 'Fortran 90'
      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, rc=rc)
!      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, &
!                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'coding_language' Attribute from a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(outChar==attrvalue), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX__UTest
      ! Get ESG "model_component_framework" Attribute from a GridComp Test
      attrname = 'model_component_framework'
      attrvalue = 'ESMF (Earth System Modeling Framework)'
      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, rc=rc)
!      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, &
!                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'model_component_framework' Attribute from a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(outChar==attrvalue), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX__UTest
      ! Get ESG "comment" Attribute from a GridComp Test
      attrname = 'comment'
      attrvalue = 'ESMF GridComp Attribute IO Test'
      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, rc=rc)
!      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, &
!                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'comment' Attribute from a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(outChar==attrvalue), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

      !------------------------------------------------------------------------
      !EX__UTest
      ! Get ESG "references" Attribute from a GridComp Test
      attrname = 'references'
      attrvalue = 'http://gmao.gsfc.nasa.gov/systems/geos5'
      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, rc=rc)
!      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, &
!                             convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting ESG 'references' Attribute from a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(outChar==attrvalue), &
                      name, failMsg, result, ESMF_SRCLINE)
print *, 'rc = ', rc
print *, 'attrname = ', attrname
print *, 'attrvalue = ', attrvalue
print *, 'outChar = ', outChar

#endif

      !------------------------------------------------------------------------
      ! clean up      
      !------------------------------------------------------------------------
      call ESMF_IODestroy(io, rc=rc)
      call ESMF_GridCompDestroy(gridcomp, rc=rc)

      if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
end program ESMF_IOGridCompUTest
