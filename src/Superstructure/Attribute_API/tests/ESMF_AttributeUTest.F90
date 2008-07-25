! $Id: ESMF_AttributeUTest.F90,v 1.9 2008/07/25 02:35:16 rokuingh Exp $
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
program ESMF_AttributeUTest

!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_AttributeUTest - Attribute Unit Tests
!
! !DESCRIPTION:
!
! The code in this file drives F90 Attribute unit tests.
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
      '$Id: ESMF_AttributeUTest.F90,v 1.9 2008/07/25 02:35:16 rokuingh Exp $'
!------------------------------------------------------------------------------

!-------------------------------------------------------------------------
!=========================================================================

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

      ! local variables needed to pass into function/subroutine calls
      type(ESMF_Array)       :: array
      type(ESMF_ArraySpec)   :: arrayspec
      type(ESMF_DistGrid)    :: distgrid
      type(ESMF_Grid)        :: grid
      type(ESMF_CplComp)     :: cplcomp, cfc, cfg
      type(ESMF_GridComp)    :: grdcomp, gfc, gfg
      type(ESMF_Field)       :: field, ffb, ffs 
      type(ESMF_State)       :: state, state2, sfc, sfg
      type(ESMF_FieldBundle) :: fieldbundle, fbfs
      character(ESMF_MAXSTR) :: conv, purp, attrname, attrvalue
      character(ESMF_MAXSTR), dimension(3) :: attrList
      logical                :: lattrList(3), lattrResult(3)
      integer                :: rc, count, number, defaultvalue, inval, outval
  
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
      ! array and grid
      call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
      distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/5,5/), &
        regDecomp=(/2,3/), rc=rc)
      array = ESMF_ArrayCreate(arrayspec, distgrid, rc=rc)
      grid = ESMF_GridCreateEmpty(rc=rc)
      
      ! fields
      field = ESMF_FieldCreateEmpty(name="original field", rc=rc)
      ffs = ESMF_FieldCreateEmpty(name="fieldforstate", rc=rc)
      ffb = ESMF_FieldCreateEmpty(name="fieldforbundle", rc=rc)
      
      ! field bundles
      fieldbundle = ESMF_FieldBundleCreate(name="original field bundle", rc=rc)
      fbfs = ESMF_FieldBundleCreate(name="fieldbundleforstate", rc=rc)
      
      ! states
      state = ESMF_StateCreate("original state", ESMF_STATE_IMPORT, rc=rc)
      state2 = ESMF_StateCreate("state copy", ESMF_STATE_EXPORT, rc=rc)
      sfc = ESMF_StateCreate("stateforcplcomp", ESMF_STATE_EXPORT, rc=rc)
      sfg = ESMF_StateCreate("stateforgridcomp", ESMF_STATE_EXPORT, rc=rc)
      
      ! coupler components
      cplcomp = ESMF_CplCompCreate(name="cplcomp", petList=(/0/), rc=rc)
      cfc = ESMF_CplCompCreate(name="cplcompforcplcomp", petList=(/0/), rc=rc)
      cfg = ESMF_CplCompCreate(name="cplcompforgridcomp", petList=(/0/), rc=rc)
      
      ! gridded components
      grdcomp = ESMF_GridCompCreate(name="gridcomp", petList=(/0/), rc=rc)
      gfc = ESMF_GridCompCreate(name="gridcompforcplcomp", petList=(/0/), rc=rc)
      gfg = ESMF_GridCompCreate(name="gridcompforgridcomp", petList=(/0/), rc=rc)
      
      if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!-------------------------------------------------------------------------
!  ARRAY
!-------------------------------------------------------------------------

      !EX_UTest
      ! Add an integer attribute to a Array Test
      call ESMF_AttributeSet(array, name="Sides", value=65, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an integer attribute to a Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an integer attribute from a Array Test
      call ESMF_AttributeGet(array, name="Sides", value=number, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an integer attribute from a Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(number.eq.65), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

defaultvalue = 7;

      !EX_UTest
      ! Get an integer attribute from a Field Test
      call ESMF_AttributeGet(array, name="NotSides", value=number, &
        defaultvalue=defaultvalue, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default integer attribute from an Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(number.eq.7), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an integer attribute from a Array Test
      call ESMF_AttributeGet(array, name="Sides", count=number, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an attribute info from a Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(number.eq.1), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      conv = "defaultconvention"
      purp = "defaultpurpose"

      !EX_UTest
      ! Create an attribute package on a Array Test
      call ESMF_AttributeAdd(array, convention=conv, &
        purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating an Attpack on a Array Test"
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
      call ESMF_AttributeAdd(array, convention=conv, &
        purpose=purp, attrList=attrList, count=count, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a custom Attpack on an Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      attrname = "units"
      attrvalue = "m/s"
      conv = "defaultconvention"
      purp = "defaultpurpose"
      
      !EX_UTest
      ! Set an attribute in an attribute package on a Array Test
      call ESMF_AttributeSet(array, name=attrname, value=attrvalue, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an Attribute in an Attpack from a Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Write the attribute package from a Array Test
      call ESMF_AttributeWrite(array, convention=conv, &
        purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Writing an Attpack from a Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Getting Attribute count from a Array
      call ESMF_AttributeGet(array, count, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Attribute Count from a Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Set a logical attribute - scalar version
      attrname = "flag"
      lattrList = (/ .true., .false., .true. /)

      call ESMF_AttributeSet(array, name=attrname, value=lattrList(1), &
        rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCESS"
      write(name, *) "Setting Array attribute (type Fortran logical scalar)"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! Get a logical attribute - scalar version
      lattrResult = .false.

      call ESMF_AttributeGet(array, name=attrname, value=lattrResult(1), &
        rc=rc)
      write(failMsg, *) "Did not return logical .TRUE."
      write(name, *) "Getting Array attribute (type Fortran logical scalar)"
      call ESMF_Test((rc == ESMF_SUCCESS .and. lattrResult(1) == lattrList(1)),   &
        name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! Set a logical attribute - array version
      attrname = "flag array"
      lattrList = (/ .true., .false., .true. /)

      call ESMF_AttributeSet(array, name=attrname,  &
        count=size (lattrList), valueList=lattrList, &
        rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCESS"
      write(name, *) "Setting Array attribute (type Fortran logical array)"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! Get a logical attribute - array version
      lattrResult = .false.

      call ESMF_AttributeGet(array, name=attrname,  &
        count=size (lattrResult), valueList=lattrResult, &
        rc=rc)
      write(failMsg, *) "Did not return logical .TRUE."
      write(name, *) "Getting Array attribute (type Fortran logical array)"
      call ESMF_Test((rc == ESMF_SUCCESS .and. all (lattrResult == lattrList)),   &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

!-------------------------------------------------------------------------
!  CPLCOMP
!-------------------------------------------------------------------------

      !EX_UTest
      ! Add an integer attribute to a CplComp Test
      call ESMF_AttributeSet(cplcomp, name="Sides", value=65, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an integer attribute to a CplComp Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an integer attribute from a CplComp Test
      call ESMF_AttributeGet(cplcomp, name="Sides", value=number, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an integer attribute from a CplComp Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(number.eq.65), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      defaultvalue = 7;

      !EX_UTest
      ! Get an integer attribute from a Field Test
      call ESMF_AttributeGet(cplcomp, name="NotSides", value=number, &
        defaultvalue=defaultvalue, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default integer attribute from a CplComp Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(number.eq.7), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get an integer attribute from a CplComp Test
      call ESMF_AttributeGet(cplcomp, name="Sides", count=number, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an attribute info from a CplComp Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(number.eq.1), &
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
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
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
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
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
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Write the attribute package from a CplComp Test
      call ESMF_AttributeWrite(cplcomp, convention=conv, &
        purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Writing an Attpack from a CplComp Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Link a cplcomp attribute hierarchy to a state attribute hierarchy CplComp Test
      call ESMF_AttributeSet(cplcomp, sfc, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Linking a CplComp hierarchy to a State hierarchy Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Link a cplcomp attribute hierarchy to a cplcomp attribute hierarchy CplComp Test
      call ESMF_AttributeSet(cplcomp, cfc, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Linking a CplComp hierarchy to a CplComp hierarchy Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Link a cplcomp attribute hierarchy to a gridcomp attribute hierarchy CplComp Test
      call ESMF_AttributeSet(cplcomp, gfc, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Linking a CplComp hierarchy to a GridComp hierarchy Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Getting Attribute count from a CplComp
      call ESMF_AttributeGet(cplcomp, count, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Attribute Count from a CplComp Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Set a logical attribute - scalar version
      attrname = "flag"
      lattrList = (/ .true., .false., .true. /)

      call ESMF_AttributeSet(cplcomp, name=attrname, value=lattrList(1), &
        rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCESS"
      write(name, *) "Setting CplComp attribute (type Fortran logical scalar)"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! Get a logical attribute - scalar version
      lattrResult = .false.

      call ESMF_AttributeGet(cplcomp, name=attrname, value=lattrResult(1), &
        rc=rc)
      write(failMsg, *) "Did not return logical .TRUE."
      write(name, *) "Getting CplComp attribute (type Fortran logical scalar)"
      call ESMF_Test((rc == ESMF_SUCCESS .and. lattrResult(1) == lattrList(1)),   &
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
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! Get a logical attribute - array version
      lattrResult = .false.

      call ESMF_AttributeGet(cplcomp, name=attrname,  &
        count=size (lattrResult), valueList=lattrResult, &
        rc=rc)
      write(failMsg, *) "Did not return logical .TRUE."
      write(name, *) "Getting CplComp attribute (type Fortran logical array)"
      call ESMF_Test((rc == ESMF_SUCCESS .and. all (lattrResult == lattrList)),   &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

!-------------------------------------------------------------------------
!  GRIDCOMP
!-------------------------------------------------------------------------

      !EX_UTest
      ! Add an integer attribute to a GridComp Test
      call ESMF_AttributeSet(grdcomp, name="Sides", value=65, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an integer attribute to a GridComp Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an integer attribute from a GridComp Test
      call ESMF_AttributeGet(grdcomp, name="Sides", value=number, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an integer attribute from a GridComp Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(number.eq.65), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      defaultvalue = 7;

      !EX_UTest
      ! Get an integer attribute from a Field Test
      call ESMF_AttributeGet(grdcomp, name="NotSides", value=number, &
        defaultvalue=defaultvalue, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default integer attribute from a GridComp Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(number.eq.7), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get an integer attribute from a GridComp Test
      call ESMF_AttributeGet(grdcomp, name="Sides", count=number, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an attribute info from a GridComp Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(number.eq.1), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      conv = "defaultconvention"
      purp = "defaultpurpose"

      !EX_UTest
      ! Create an attribute package on a GridComp Test
      call ESMF_AttributeAdd(grdcomp, convention=conv, &
        purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating an Attpack on a GridComp Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      attrList(1) = "Custom1"
      attrList(2) = "Custom2"
      attrList(3) = "Custom3"
      count = 3
      conv = "customconvention"
      purp = "custompurpose"
      
      !EX_UTest
      ! Create a custom attribute package on a GridComp Test
      call ESMF_AttributeAdd(grdcomp, convention=conv, &
        purpose=purp, attrList=attrList, count=count, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a custom Attpack on a GridComp Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      attrname = "discipline"
      attrvalue = "atmosphere"
      conv = "defaultconvention"
      purp = "defaultpurpose"

      !EX_UTest
      ! Set an attribute in an attribute package on a GridComp Test
      call ESMF_AttributeSet(grdcomp, name=attrname, value=attrvalue, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an Attribute in an Attpack from a GridComp Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Write the attribute package from a GridComp Test
      call ESMF_AttributeWrite(grdcomp, convention=conv, &
        purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Writing an Attpack from a GridComp Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Link a grdcomp attribute hierarchy to a state attribute hierarchy GridComp Test
      call ESMF_AttributeSet(grdcomp, sfg, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Linking a GridComp hierarchy to a State hierarchy Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Link a grdcomp attribute hierarchy to a cplcomp attribute hierarchy GridComp Test
      call ESMF_AttributeSet(grdcomp, cfg, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Linking a GridComp hierarchy to a CplComp hierarchy Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Link a grdcomp attribute hierarchy to a gridcomp attribute hierarchy GridComp Test
      call ESMF_AttributeSet(grdcomp, gfg, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Linking a GridComp hierarchy to a GridComp hierarchy Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Getting Attribute count from a GridComp
      call ESMF_AttributeGet(grdcomp, count, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Attribute Count from a GridComp Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Set a logical attribute - scalar version
      attrname = "flag"
      lattrList = (/ .true., .false., .true. /)

      call ESMF_AttributeSet(grdcomp, name=attrname, value=lattrList(1), &
        rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCESS"
      write(name, *) "Setting GrdComp attribute (type Fortran logical scalar)"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! Get a logical attribute - scalar version
      lattrResult = .false.

      call ESMF_AttributeGet(grdcomp, name=attrname, value=lattrResult(1), &
        rc=rc)
      write(failMsg, *) "Did not return logical .TRUE."
      write(name, *) "Getting GrdComp attribute (type Fortran logical scalar)"
      call ESMF_Test((rc == ESMF_SUCCESS .and. lattrResult(1) == lattrList(1)),   &
        name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! Set a logical attribute - array version
      attrname = "flag array"
      lattrList = (/ .true., .false., .true. /)

      call ESMF_AttributeSet(grdcomp, name=attrname,  &
        count=size (lattrList), valueList=lattrList, &
        rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCESS"
      write(name, *) "Setting GrdComp attribute (type Fortran logical array)"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! Get a logical attribute - array version
      lattrResult = .false.

      call ESMF_AttributeGet(grdcomp, name=attrname,  &
        count=size (lattrResult), valueList=lattrResult, &
        rc=rc)
      write(failMsg, *) "Did not return logical .TRUE."
      write(name, *) "Getting GrdComp attribute (type Fortran logical array)"
      call ESMF_Test((rc == ESMF_SUCCESS .and. all (lattrResult == lattrList)),   &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

!-------------------------------------------------------------------------
!  FIELD
!-------------------------------------------------------------------------

      !EX_UTest
      ! Add an integer attribute to a Field Test
      call ESMF_AttributeSet(field, name="Sides", value=65, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an integer attribute to a Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an integer attribute from a Field Test
      call ESMF_AttributeGet(field, name="Sides", value=number, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an integer attribute from a Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(number.eq.65), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      defaultvalue = 7;

      !EX_UTest
      ! Get an integer attribute from a Field Test
      call ESMF_AttributeGet(field, name="NotSides", value=number, &
        defaultvalue=defaultvalue, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default integer attribute from a Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(number.eq.7), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an integer attribute from a Field Test
      call ESMF_AttributeGet(field, name="Sides", count=number, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an attribute info from a Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(number.eq.1), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      conv = "defaultconvention"
      purp = "defaultpurpose"

      !EX_UTest
      ! Create an attribute package on a Field Test
      call ESMF_AttributeAdd(field, convention=conv, &
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
      call ESMF_AttributeAdd(field, convention=conv, &
        purpose=purp, attrList=attrList, count=count, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a custom Attpack on a Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      attrname = "Custom1"
      inval = 4

      !EX_UTest
      ! Set an attribute in an attribute package on a Field Test
      call ESMF_AttributeSet(field, name=attrname, value=inval, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting an Attribute in an Attpack from a Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------    

      !EX_UTest
      ! Set an attribute in an attribute package on a Field Test
      call ESMF_AttributeGet(field, name=attrname, value=outval, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting an Attribute in an Attpack from a Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(outval.eq.4), name, failMsg, &
                     result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      attrname = "units"
      attrvalue = "m/s"
      conv = "defaultconvention"
      purp = "defaultpurpose"

      !EX_UTest
      ! Set an attribute in an attribute package on a Field Test
      call ESMF_AttributeSet(field, name=attrname, value=attrvalue, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an Attribute in an Attpack from a Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Write the attribute package from a Field Test
      call ESMF_AttributeWrite(field, convention=conv, &
        purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Writing an Attpack from a Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Getting Attribute count from a Field
      call ESMF_AttributeGet(field, count, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Attribute Count from a Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Set a logical attribute - scalar version
      attrname = "flag"
      lattrList = (/ .true., .false., .true. /)

      call ESMF_AttributeSet(field, name=attrname, value=lattrList(1), &
        rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCESS"
      write(name, *) "Setting Field attribute (type Fortran logical scalar)"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! Get a logical attribute - scalar version
      lattrResult = .false.

      call ESMF_AttributeGet(field, name=attrname, value=lattrResult(1), &
        rc=rc)
      write(failMsg, *) "Did not return logical .TRUE."
      write(name, *) "Getting Field attribute (type Fortran logical scalar)"
      call ESMF_Test((rc == ESMF_SUCCESS .and. lattrResult(1) == lattrList(1)),   &
        name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! Set a logical attribute - array version
      attrname = "flag array"
      lattrList = (/ .true., .false., .true. /)

      call ESMF_AttributeSet(field, name=attrname,  &
        count=size (lattrList), valueList=lattrList, &
        rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCESS"
      write(name, *) "Setting Field attribute (type Fortran logical array)"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! Get a logical attribute - array version
      lattrResult = .false.

      call ESMF_AttributeGet(field, name=attrname,  &
        count=size (lattrResult), valueList=lattrResult, &
        rc=rc)
      write(failMsg, *) "Did not return logical .TRUE."
      write(name, *) "Getting Field attribute (type Fortran logical array)"
      call ESMF_Test((rc == ESMF_SUCCESS .and. all (lattrResult == lattrList)),   &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

!-------------------------------------------------------------------------
!  FIELDBUNDLE
!-------------------------------------------------------------------------

      !EX_UTest
      ! Add an integer attribute to a FieldBundle Test
      call ESMF_AttributeSet(fieldbundle, name="Sides", value=65, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an integer attribute to a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an integer attribute from a FieldBundle Test
      call ESMF_AttributeGet(fieldbundle, name="Sides", value=number, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an integer attribute from a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(number.eq.65), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      defaultvalue = 7;

      !EX_UTest
      ! Get an integer attribute from a Field Test
      call ESMF_AttributeGet(fieldbundle, name="NotSides", value=number, &
        defaultvalue=defaultvalue, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default integer attribute from a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(number.eq.7), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get an integer attribute from a FieldBundle Test
      call ESMF_AttributeGet(fieldbundle, name="Sides", count=number, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an attribute info from a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(number.eq.1), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      conv = "defaultconvention"
      purp = "defaultpurpose"

      !EX_UTest
      ! Create an attribute package on a FieldBundle Test
      call ESMF_AttributeAdd(fieldbundle, convention=conv, &
        purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating an Attpack on a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      attrList(1) = "Custom1"
      attrList(2) = "Custom2"
      attrList(3) = "Custom3"
      count = 3
      conv = "customconvention"
      purp = "custompurpose"
      
      !EX_UTest
      ! Create a custom attribute package on a FieldBundle Test
      call ESMF_AttributeAdd(fieldbundle, convention=conv, &
        purpose=purp, attrList=attrList, count=count, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a custom Attpack on a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      attrname = "units"
      attrvalue = "m/s"
      conv = "defaultconvention"
      purp = "defaultpurpose"

      !EX_UTest
      ! Set an attribute in an attribute package on a FieldBundle Test
      call ESMF_AttributeSet(fieldbundle, name=attrname, value=attrvalue, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an Attribute in an Attpack from a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Write the attribute package from a FieldBundle Test
      call ESMF_AttributeWrite(fieldbundle, convention=conv, &
        purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Writing an Attpack from a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Link a bundle attribute hierarchy to a field attribute hierarchy FieldBundle Test
      call ESMF_AttributeSet(fieldbundle, ffb, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Linking a FieldBundle hierarchy to a Field hierarchy Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Getting Attribute count from a FieldBundle
      call ESMF_AttributeGet(fieldbundle, count, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Attribute Count from a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Set a logical attribute - scalar version
      attrname = "flag"
      lattrList = (/ .true., .false., .true. /)

      call ESMF_AttributeSet(fieldbundle, name=attrname, value=lattrList(1), &
        rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCESS"
      write(name, *) "Setting FieldBundle attribute (type Fortran logical scalar)"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! Get a logical attribute - scalar version
      lattrResult = .false.

      call ESMF_AttributeGet(fieldbundle, name=attrname, value=lattrResult(1), &
        rc=rc)
      write(failMsg, *) "Did not return logical .TRUE."
      write(name, *) "Getting FieldBundle attribute (type Fortran logical scalar)"
      call ESMF_Test((rc == ESMF_SUCCESS .and. lattrResult(1) == lattrList(1)),   &
        name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! Set a logical attribute - array version
      attrname = "flag array"
      lattrList = (/ .true., .false., .true. /)

      call ESMF_AttributeSet(fieldbundle, name=attrname,  &
        count=size (lattrList), valueList=lattrList, &
        rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCESS"
      write(name, *) "Setting FieldBundle attribute (type Fortran logical array)"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! Get a logical attribute - array version
      lattrResult = .false.

      call ESMF_AttributeGet(fieldbundle, name=attrname,  &
        count=size (lattrResult), valueList=lattrResult, &
        rc=rc)
      write(failMsg, *) "Did not return logical .TRUE."
      write(name, *) "Getting FieldBundle attribute (type Fortran logical array)"
      call ESMF_Test((rc == ESMF_SUCCESS .and. all (lattrResult == lattrList)),   &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

!-------------------------------------------------------------------------
!  GRID
!-------------------------------------------------------------------------

      !EX_UTest
      ! Add an integer attribute to a Grid Test
      call ESMF_AttributeSet(grid, name="Sides", value=65, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an integer attribute to a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an integer attribute from a Grid Test
      call ESMF_AttributeGet(grid, name="Sides", value=number, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an integer attribute from a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(number.eq.65), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      defaultvalue = 7;

      !EX_UTest
      ! Get an integer attribute from a Field Test
      call ESMF_AttributeGet(grid, name="NotSides", value=number, &
        defaultvalue=defaultvalue, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default integer attribute from a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(number.eq.7), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an integer attribute from a Grid Test
      call ESMF_AttributeGet(grid, name="Sides", count=number, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an attribute info from a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(number.eq.1), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      conv = "defaultconvention"
      purp = "defaultpurpose"

      !EX_UTest
      ! Create an attribute package on a Grid Test
      call ESMF_AttributeAdd(grid, convention=conv, &
        purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating an Attpack on a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      attrList(1) = "Custom1"
      attrList(2) = "Custom2"
      attrList(3) = "Custom3"
      count = 3
      conv = "customconvention"
      purp = "custompurpose"
      
      !EX_UTest
      ! Create a custom attribute package on a Grid Test
      call ESMF_AttributeAdd(grid, convention=conv, &
        purpose=purp, attrList=attrList, count=count, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a custom Attpack on a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      attrname = "units"
      attrvalue = "m/s"
      conv = "defaultconvention"
      purp = "defaultpurpose"
      
      !EX_UTest
      ! Set an attribute in an attribute package on a Grid Test
      call ESMF_AttributeSet(grid, name=attrname, value=attrvalue, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an Attribute in an Attpack from a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Write the attribute package from a Grid Test
      call ESMF_AttributeWrite(grid, convention=conv, &
        purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Writing an Attpack from a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Getting Attribute count from a Grid
      call ESMF_AttributeGet(grid, count, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Attribute Count from a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Set a logical attribute - scalar version
      attrname = "flag"
      lattrList = (/ .true., .false., .true. /)

      call ESMF_AttributeSet(grid, name=attrname, value=lattrList(1), &
        rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCESS"
      write(name, *) "Setting Grid attribute (type Fortran logical scalar)"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! Get a logical attribute - scalar version
      lattrResult = .false.

      call ESMF_AttributeGet(grid, name=attrname, value=lattrResult(1), &
        rc=rc)
      write(failMsg, *) "Did not return logical .TRUE."
      write(name, *) "Getting Grid attribute (type Fortran logical scalar)"
      call ESMF_Test((rc == ESMF_SUCCESS .and. lattrResult(1) == lattrList(1)),   &
        name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! Set a logical attribute - array version
      attrname = "flag array"
      lattrList = (/ .true., .false., .true. /)

      call ESMF_AttributeSet(grid, name=attrname,  &
        count=size (lattrList), valueList=lattrList, &
        rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCESS"
      write(name, *) "Setting Grid attribute (type Fortran logical array)"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! Get a logical attribute - array version
      lattrResult = .false.

      call ESMF_AttributeGet(grid, name=attrname,  &
        count=size (lattrResult), valueList=lattrResult, &
        rc=rc)
      write(failMsg, *) "Did not return logical .TRUE."
      write(name, *) "Getting Grid attribute (type Fortran logical array)"
      call ESMF_Test((rc == ESMF_SUCCESS .and. all (lattrResult == lattrList)),   &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

!-------------------------------------------------------------------------
!  STATE
!-------------------------------------------------------------------------

      !EX_UTest
      ! Add an integer attribute to a State Test
      call ESMF_AttributeSet(state, name="Sides", value=65, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an integer attribute to a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an integer attribute from a State Test
      call ESMF_AttributeGet(state, name="Sides", value=number, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an integer attribute from a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(number.eq.65), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

       defaultvalue = 7;

      !EX_UTest
      ! Get an integer attribute from a Field Test
      call ESMF_AttributeGet(state, name="NotSides", value=number, &
        defaultvalue=defaultvalue, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default integer attribute from a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(number.eq.7), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an integer attribute from a State Test
      call ESMF_AttributeGet(state, name="Sides", count=number, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an attribute info from a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(number.eq.1), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      conv = "defaultconvention"
      purp = "defaultpurpose"

      !EX_UTest
      ! Create an attribute package on a State Test
      call ESMF_AttributeAdd(state, convention=conv, &
        purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating an Attpack on a State Test"
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
      call ESMF_AttributeAdd(state, convention=conv, &
        purpose=purp, attrList=attrList, count=count, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a custom Attpack on a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      attrname = "organization"
      attrvalue = "NCAR"
      conv = "defaultconvention"
      purp = "defaultpurpose"

      !EX_UTest
      ! Set an attribute in an attribute package on a State Test
      call ESMF_AttributeSet(state, name=attrname, value=attrvalue, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an Attribute in an Attpack from a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Write the attribute package from a State Test
      call ESMF_AttributeWrite(state, convention=conv, &
        purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Writing an Attpack from a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Link a state attribute hierarchy to a bundle attribute hierarchy State Test
      call ESMF_AttributeSet(state, fbfs, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Linking a State hierarchy to a FieldBundle hierarchy Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Link a state attribute hierarchy to a field attribute hierarchy State Test
      call ESMF_AttributeSet(state, ffs, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Linking a State hierarchy to a Field hierarchy Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

			!EX_UTest
      ! Getting Attribute count from a State
      call ESMF_AttributeGet(state, count, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Attribute Count from a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      !EX_UTest
      ! Copy an attribute hierarchy from state1 to state2
      call ESMF_AttributeCopy(sfc, state2, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Copy an attribute hierarchy from state to state2"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Set a logical attribute - scalar version
      attrname = "flag"
      lattrList = (/ .true., .false., .true. /)

      call ESMF_AttributeSet(state, name=attrname, value=lattrList(1), &
        rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCESS"
      write(name, *) "Setting State attribute (type Fortran logical scalar)"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! Get a logical attribute - scalar version
      lattrResult = .false.

      call ESMF_AttributeGet(state, name=attrname, value=lattrResult(1), &
        rc=rc)
      write(failMsg, *) "Did not return logical .TRUE."
      write(name, *) "Getting State attribute (type Fortran logical scalar)"
      call ESMF_Test((rc == ESMF_SUCCESS .and. lattrResult(1) == lattrList(1)),   &
        name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! Set a logical attribute - array version
      attrname = "flag array"
      lattrList = (/ .true., .false., .true. /)

      call ESMF_AttributeSet(state, name=attrname,  &
        count=size (lattrList), valueList=lattrList, &
        rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCESS"
      write(name, *) "Setting State attribute (type Fortran logical array)"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! Get a logical attribute - array version
      lattrResult = .false.

      call ESMF_AttributeGet(state, name=attrname,  &
        count=size (lattrResult), valueList=lattrResult, &
        rc=rc)
      write(failMsg, *) "Did not return logical .TRUE."
      write(name, *) "Getting State attribute (type Fortran logical array)"
      call ESMF_Test((rc == ESMF_SUCCESS .and. all (lattrResult == lattrList)),   &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      ! clean up
      call ESMF_ArrayDestroy(array, rc=rc)
      call ESMF_DistGridDestroy(distGrid, rc=rc)
      call ESMF_GridDestroy(grid, rc=rc)
      
      call ESMF_FieldBundleDestroy(fieldbundle, rc=rc)
      call ESMF_FieldBundleDestroy(fbfs, rc=rc)
      
      call ESMF_FieldDestroy(field, rc=rc)
      call ESMF_FieldDestroy(ffb, rc=rc)
      call ESMF_FieldDestroy(ffs, rc=rc)
      
      call ESMF_CplCompDestroy(cplcomp, rc=rc)
      call ESMF_CplCompDestroy(cfc, rc=rc)
      call ESMF_CplCompDestroy(cfg, rc=rc)
      
      call ESMF_GridCompDestroy(grdcomp, rc=rc)
      call ESMF_GridCompDestroy(gfc, rc=rc)
      call ESMF_GridCompDestroy(gfg, rc=rc)

      call ESMF_StateDestroy(state2, rc=rc)
      call ESMF_StateDestroy(state, rc=rc)
      call ESMF_StateDestroy(sfc, rc=rc)
      call ESMF_StateDestroy(sfg, rc=rc)
      
      if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
#endif

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
end program ESMF_AttributeUTest
