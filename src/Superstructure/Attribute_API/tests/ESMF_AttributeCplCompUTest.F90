! $Id: ESMF_AttributeCplCompUTest.F90,v 1.1 2008/08/01 19:24:19 rokuingh Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2008, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_AttributeCplCompUTest

!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_AttributeCplCompUTest - Attribute CplComp Unit Tests
!
! !DESCRIPTION:
!
! The code in this file drives F90 Attribute CplComp unit tests.
! The companion file ESMF\_Attribute.F90 contains the definitions for the
! Attribute methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod         ! the ESMF Framework
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_AttributeCplCompUTest.F90,v 1.1 2008/08/01 19:24:19 rokuingh Exp $'
!------------------------------------------------------------------------------

!-------------------------------------------------------------------------
!=========================================================================

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

      ! local variables needed to pass into function/subroutine calls
      type(ESMF_CplComp)     :: cplcomp, cfc
      type(ESMF_GridComp)    :: gfc
      type(ESMF_State)       :: sfc
      character(ESMF_MAXSTR) :: conv, purp, attrname, attrvalue
      character(ESMF_MAXSTR), dimension(3) :: attrList
      character(ESMF_MAXSTR), dimension(4) :: attrListIn, defvalList
      character(ESMF_MAXSTR), dimension(4) :: attrListOut1, attrListOut2
      logical                :: lattrList(3), lattrResult(3)
      integer                :: rc, count, defout, defaultvalue, inval, outval
  
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

#ifdef ESMF_TESTEXHAUSTIVE

      !------------------------------------------------------------------------
      ! preparations
      ! states
      sfc = ESMF_StateCreate("stateforcplcomp", ESMF_STATE_EXPORT, rc=rc)
      
      ! coupler components
      cplcomp = ESMF_CplCompCreate(name="cplcomp", petList=(/0/), rc=rc)
      cfc = ESMF_CplCompCreate(name="cplcompforcplcomp", petList=(/0/), rc=rc)
      
      ! gridded components
      gfc = ESMF_GridCompCreate(name="gridcompforcplcomp", petList=(/0/), rc=rc)
      
      if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!-------------------------------------------------------------------------
!  CPLCOMP
!-------------------------------------------------------------------------

      !EX_UTest
      ! Add an integer attribute to a CplComp Test
      call ESMF_AttributeSet(cplcomp, name="Sides", value=inval, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an integer attribute to a CplComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an integer attribute from a CplComp Test
      call ESMF_AttributeGet(cplcomp, name="Sides", value=outval, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an integer attribute from a CplComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(inval==outval), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      defaultvalue = 7;

      !EX_UTest
      ! Get an integer attribute from a Field Test
      call ESMF_AttributeGet(cplcomp, name="NotSides", value=defout, &
        defaultvalue=defaultvalue, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default integer attribute from a CplComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(defaultvalue==defout), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get an integer attribute from a CplComp Test
      call ESMF_AttributeGet(cplcomp, name="Sides", count=count, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an attribute info from a CplComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(count==1), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      conv = "defaultconvention"
      purp = "defaultpurpose"

      !EX_UTest
      ! Create an attribute package on a CplComp Test
      call ESMF_AttributeAdd(cplcomp, convention=conv, &
        purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating an Attpack on a CplComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      attrList(1) = "Custom1"
      attrList(2) = "Custom2"
      attrList(3) = "Custom3"
      count = 3
      conv = "customconvention"
      purp = "custompurpose"
      
      !EX_UTest
      ! Create a custom attribute package on a CplComp Test
      call ESMF_AttributeAdd(cplcomp, convention=conv, &
        purpose=purp, attrList=attrList, count=count, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a custom Attpack on a CplComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      attrname = "discipline"
      attrvalue = "atmosphere"
      conv = "defaultconvention"
      purp = "defaultpurpose"

      !EX_UTest
      ! Set an attribute in an attribute package on a CplComp Test
      call ESMF_AttributeSet(cplcomp, name=attrname, value=attrvalue, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an Attribute in an Attpack from a CplComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------- Attribute character list ---------------------------
      attrListIn(1) = "Character String 1"
      attrListIn(2) = "Character String 2"
      attrListIn(3) = "Character String 3"
      attrListIn(4) = "Character String 4"
      defvalList(1) = "Character String 5"
      defvalList(2) = "Character String 6"
      defvalList(3) = "Character String 7"
      defvalList(4) = "Character String 8"
      count = 4

      !EX_UTest
      ! Set a char list Attribute on a CplComp Test
      call ESMF_AttributeSet(cplcomp, name="list", count=count, &
        valueList=attrListIn, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an Attribute character list on an CplComp test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
  
      !EX_UTest
      ! Get a char list Attribute on a CplComp Test
      call ESMF_AttributeGet(cplcomp, name="list", count=count, &
        valueList=attrListOut1, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting an Attribute character list from an CplComp test"
      call ESMF_Test((rc==ESMF_SUCCESS) .and. all (attrListIn==attrListOut1), &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get a char list default Attribute on a CplComp Test
      call ESMF_AttributeGet(cplcomp, name="notlist", count=count, &
        valueList=attrListOut1, defaultvalueList=defvalList, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting a default Attribute character list from a CplComp test"
      call ESMF_Test((rc==ESMF_SUCCESS) .and. all (attrListOut1 == defvalList), &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      conv = "customconvention"
      purp = "custompurpose"
      attrname = "Custom1"
      count = 4

      !EX_UTest
      ! Set a char list attribute in an attribute package on a CplComp Test
      call ESMF_AttributeSet(cplcomp, name=attrname, count=count, &
        valueList=attrListIn, convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting a char list Attribute in an Attpack from a CplComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get a char list attribute in an attribute package on a CplComp Test
      call ESMF_AttributeGet(cplcomp, name=attrname, count=count, &
        valueList=attrListOut2, convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting a char list Attribute in an Attpack from a CplComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS) .and. all (attrListIn == attrListOut2), &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get a char list default Attribute in an Attribute package on a CplComp Test
      call ESMF_AttributeGet(cplcomp, name="notlist", count=count, &
        valueList=attrListOut2, defaultvalueList=defvalList, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting a default Attribute character list in an Attpack from a CplComp test"
      call ESMF_Test((rc==ESMF_SUCCESS) .and. all (attrListOut2 == defvalList), &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Write the attribute package from a CplComp Test
      call ESMF_AttributeWrite(cplcomp, convention=conv, &
        purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Writing an Attpack from a CplComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Link a cplcomp attribute hierarchy to a state attribute hierarchy CplComp Test
      call ESMF_AttributeSet(cplcomp, sfc, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Linking a CplComp hierarchy to a State hierarchy Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Link a cplcomp attribute hierarchy to a cplcomp attribute hierarchy CplComp Test
      call ESMF_AttributeSet(cplcomp, cfc, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Linking a CplComp hierarchy to a CplComp hierarchy Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Link a cplcomp attribute hierarchy to a gridcomp attribute hierarchy CplComp Test
      call ESMF_AttributeSet(cplcomp, gfc, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Linking a CplComp hierarchy to a GridComp hierarchy Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Getting Attribute count from a CplComp
      call ESMF_AttributeGet(cplcomp, count, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Attribute Count from a CplComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Set a logical attribute - scalar version
      attrname = "flag"
      lattrList = (/ .true., .false., .true. /)

      call ESMF_AttributeSet(cplcomp, name=attrname, value=lattrList(1), &
        rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCESS"
      write(name, *) "Setting CplComp attribute (type Fortran logical scalar)"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! Get a logical attribute - scalar version
      lattrResult = .false.

      call ESMF_AttributeGet(cplcomp, name=attrname, value=lattrResult(1), &
        rc=rc)
      write(failMsg, *) "Did not return logical .TRUE."
      write(name, *) "Getting CplComp attribute (type Fortran logical scalar)"
      call ESMF_Test((rc == ESMF_SUCCESS .and. lattrResult(1) .eqv. lattrList(1)),   &
        name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! Set a logical attribute - array version
      attrname = "flag array"
      lattrList = (/ .true., .false., .true. /)

      call ESMF_AttributeSet(cplcomp, name=attrname,  &
        count=size (lattrList), valueList=lattrList, &
        rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCESS"
      write(name, *) "Setting CplComp attribute (type Fortran logical array)"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! Get a logical attribute - array version
      lattrResult = .false.

      call ESMF_AttributeGet(cplcomp, name=attrname,  &
        count=size (lattrResult), valueList=lattrResult, &
        rc=rc)
      write(failMsg, *) "Did not return logical .TRUE."
      write(name, *) "Getting CplComp attribute (type Fortran logical array)"
      call ESMF_Test((rc == ESMF_SUCCESS .and. all (lattrResult .eqv. lattrList)),   &
        name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! clean up
      call ESMF_CplCompDestroy(cplcomp, rc=rc)
      call ESMF_CplCompDestroy(cfc, rc=rc)
      
      call ESMF_GridCompDestroy(gfc, rc=rc)

      call ESMF_StateDestroy(sfc, rc=rc)
      
      if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
#endif

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
end program ESMF_AttributeCplCompUTest
