! $Id: ESMF_AttributeGridCompUTest.F90,v 1.2 2008/08/08 15:25:27 rokuingh Exp $
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
program ESMF_AttributeGridCompUTest

!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_AttributeGridCompUTest - Attribute GridComp Unit Tests
!
! !DESCRIPTION:
!
! The code in this file drives F90 Attribute GridComp unit tests.
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
      '$Id: ESMF_AttributeGridCompUTest.F90,v 1.2 2008/08/08 15:25:27 rokuingh Exp $'
!------------------------------------------------------------------------------

!-------------------------------------------------------------------------
!=========================================================================

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

      ! local variables
      type(ESMF_CplComp)     :: cfg
      type(ESMF_GridComp)    :: gridcomp, gfg
      type(ESMF_State)       :: sfg
      character(ESMF_MAXSTR) :: conv, purp, attrname, attrnameOut, attrvalue
      integer                :: rc, count, items
      type(ESMF_TypeKind)    :: attrTK

      integer(kind=4)                     :: inI4, outI4, defaultI4, dfltoutI4
      integer(kind=4), dimension(3)       :: inI4l, outI4l, defaultI4l, dfltoutI4l
      integer(kind=8)                     :: inI8, outI8, defaultI8, dfltoutI8
      integer(kind=8), dimension(3)       :: inI8l, outI8l, defaultI8l, dfltoutI8l
      real(kind=4)                     :: inR4, outR4, defaultR4, dfltoutR4
      real(kind=4), dimension(3)       :: inR4l, outR4l, defaultR4l, dfltoutR4l
      real(kind=8)                     :: inR8, outR8, defaultR8, dfltoutR8
      real(kind=8), dimension(3)       :: inR8l, outR8l, defaultR8l, dfltoutR8l
      character(ESMF_MAXSTR)                     :: inChar, outChar, defaultChar, dfltoutChar
      character(ESMF_MAXSTR), dimension(3)       :: inCharl, outCharl, defaultCharl, dfltoutCharl
      logical                          :: inLog, outLog, defaultLog, dfltoutLog
      logical, dimension(3)            :: inLogl, outLogl, defaultLogl, dfltoutLogl
  
      character(ESMF_MAXSTR), dimension(3)  :: attpackList, attpackListOut, &
                                               attpackListOut2, attpackDfltList, &
                                               attpackListOut3, attpackListOut4, &
                                               attpackDfltList2   
      character(ESMF_MAXSTR), dimension(12) :: attpackListTNames

  
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
      sfg = ESMF_StateCreate("stateforgridcomp", ESMF_STATE_EXPORT, rc=rc)
      
      ! coupler components
      cfg = ESMF_CplCompCreate(name="cplcompforgridcomp", petList=(/0/), rc=rc)
      
      ! gridded components
      gridcomp = ESMF_GridCompCreate(name="gridcomp", petList=(/0/), rc=rc)
      gfg = ESMF_GridCompCreate(name="gridcompforgridcomp", petList=(/0/), rc=rc)
      
      if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!-------------------------------------------------------------------------
!  GRIDCOMP
!-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  ESMF_I4
    !-------------------------------------------------------------------------
      inI4 = 4
      !EX_UTest
      ! Add an ESMF_I4 Attribute to a GridComp Test
      call ESMF_AttributeSet(gridcomp, name="AttrI4", value=inI4, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an ESMF_I4 Attribute to a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an ESMF_I4 Attribute from a GridComp Test
      call ESMF_AttributeGet(gridcomp, name="AttrI4", value=outI4, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an ESMF_I4 Attribute from a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(inI4==outI4), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Destroy an Attribute on a GridComp Test
      call ESMF_AttributeDestroy(gridcomp, name="AttrI4", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Destroying an ESMF_I4 Attribute on a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      defaultI4 = 7
      !EX_UTest
      ! Get an ESMF_I4 Attribute from a GridComp Test
      call ESMF_AttributeGet(gridcomp, name="AttrI4", value=dfltoutI4, &
        defaultvalue=defaultI4, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default ESMF_I4 Attribute from a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(defaultI4==dfltoutI4), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  ESMF_I4 list
    !-------------------------------------------------------------------------
      inI4l = (/1,2,3/)
      count = 3
      !EX_UTest
      ! Add an ESMF_I4 list Attribute to a GridComp Test
      call ESMF_AttributeSet(gridcomp, name="AttrI4l", count=count, &
        valueList=inI4l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an ESMF_I4l Attribute to a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an ESMF_I4 list Attribute from a GridComp Test
      call ESMF_AttributeGet(gridcomp, name="AttrI4l", count=count, &
        valueList=outI4l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an ESMF_I4l Attribute from a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and. all (inI4l==outI4l), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Destroy an Attribute on a GridComp Test
      call ESMF_AttributeDestroy(gridcomp, name="AttrI4l", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Destroying an ESMF_I4l Attribute on a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      defaultI4l = (/4,2,7/)
      !EX_UTest
      ! Get an ESMF_I4 list Attribute from a GridComp Test
      call ESMF_AttributeGet(gridcomp, name="AttrI4l", count=count, &
        valueList=dfltoutI4l, defaultvalue=defaultI4l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default ESMF_I4l Attribute from a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and. all (defaultI4l==dfltoutI4l), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  ESMF_I8
    !-------------------------------------------------------------------------
      inI8 = 4
      !EX_UTest
      ! Add an ESMF_I8 Attribute to a GridComp Test
      call ESMF_AttributeSet(gridcomp, name="AttrI8", value=inI8, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an ESMF_I8 Attribute to a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an ESMF_I8 Attribute from a GridComp Test
      call ESMF_AttributeGet(gridcomp, name="AttrI8", value=outI8, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an ESMF_I8 Attribute from a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and. (inI8==outI8), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Destroy an Attribute on a GridComp Test
      call ESMF_AttributeDestroy(gridcomp, name="AttrI8", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Destroying an ESMF_I8 Attribute on a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      defaultI8 = 7
      !EX_UTest
      ! Get an ESMF_I8 Attribute from a GridComp Test
      call ESMF_AttributeGet(gridcomp, name="AttrI8", value=dfltoutI8, &
        defaultvalue=defaultI8, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default ESMF_I8 Attribute from a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(defaultI8==dfltoutI8), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  ESMF_I8 list
    !-------------------------------------------------------------------------
      inI8l = (/1,2,3/)
      count = 3
      !EX_UTest
      ! Add an ESMF_I8 list Attribute to a GridComp Test
      call ESMF_AttributeSet(gridcomp, name="AttrI8l", count=count, &
        valueList=inI8l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an ESMF_I8l Attribute to a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an ESMF_I8 list Attribute from a GridComp Test
      call ESMF_AttributeGet(gridcomp, name="AttrI8l", count=count, &
        valueList=outI8l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an ESMF_I8l Attribute from a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and. all (inI8l==outI8l), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Destroy an Attribute on a GridComp Test
      call ESMF_AttributeDestroy(gridcomp, name="AttrI8l", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Destroying an ESMF_I8l Attribute on a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      defaultI8l = (/4,2,7/)
      !EX_UTest
      ! Get an ESMF_I8 list Attribute from a GridComp Test
      call ESMF_AttributeGet(gridcomp, name="AttrI8l", count=count, &
        valueList=dfltoutI8l, defaultvalue=defaultI8l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default ESMF_I8l Attribute from a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and. all (defaultI8l==dfltoutI8l), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  ESMF_R4
    !-------------------------------------------------------------------------
      inR4 = 4
      !EX_UTest
      ! Add an ESMF_R4 Attribute to a GridComp Test
      call ESMF_AttributeSet(gridcomp, name="AttrR4", value=inR4, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an ESMF_R4 Attribute to a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an ESMF_R4 Attribute from a GridComp Test
      call ESMF_AttributeGet(gridcomp, name="AttrR4", value=outR4, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an ESMF_R4 Attribute from a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(inR4==outR4), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Destroy an Attribute on a GridComp Test
      call ESMF_AttributeDestroy(gridcomp, name="AttrR4", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Destroying an ESMF_R4 Attribute on a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      defaultR4 = 7
      !EX_UTest
      ! Get an ESMF_R4 Attribute from a GridComp Test
      call ESMF_AttributeGet(gridcomp, name="AttrR4", value=dfltoutR4, &
        defaultvalue=defaultR4, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default ESMF_R4 Attribute from a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(defaultR4==dfltoutR4), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  ESMF_R4 list
    !-------------------------------------------------------------------------
      inR4l = (/1,2,3/)
      count = 3
      !EX_UTest
      ! Add an ESMF_R4 list Attribute to a GridComp Test
      call ESMF_AttributeSet(gridcomp, name="AttrR4l", count=count, &
        valueList=inR4l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an ESMF_R4l Attribute to a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an ESMF_R4 list Attribute from a GridComp Test
      call ESMF_AttributeGet(gridcomp, name="AttrR4l", count=count, &
        valueList=outR4l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an ESMF_R4l Attribute from a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and. all (inR4l==outR4l), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Destroy an Attribute on a GridComp Test
      call ESMF_AttributeDestroy(gridcomp, name="AttrR4l", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Destroying an ESMF_R4l Attribute on a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      defaultR4l = (/7,8,9/)
      !EX_UTest
      ! Get an ESMF_R4 list Attribute from a GridComp Test
      call ESMF_AttributeGet(gridcomp, name="AttrR4l", count=count, &
        valueList=dfltoutR4l, defaultvalue=defaultR4l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default ESMF_R4l Attribute from a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and. all (defaultR4l==dfltoutR4l), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  ESMF_R8
    !-------------------------------------------------------------------------
      inR8 = 4
      !EX_UTest
      ! Add an ESMF_R8 Attribute to a GridComp Test
      call ESMF_AttributeSet(gridcomp, name="AttrR8", value=inR8, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an ESMF_R8 Attribute to a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an ESMF_R8 Attribute from a GridComp Test
      call ESMF_AttributeGet(gridcomp, name="AttrR8", value=outR8, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an ESMF_R8 Attribute from a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(inR8==outR8), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Destroy an Attribute on a GridComp Test
      call ESMF_AttributeDestroy(gridcomp, name="AttrR8", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Destroying an ESMF_R8 Attribute on a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      defaultR8 = 7
      !EX_UTest
      ! Get an ESMF_R8 Attribute from a GridComp Test
      call ESMF_AttributeGet(gridcomp, name="AttrR8", value=dfltoutR8, &
        defaultvalue=defaultR8, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default ESMF_R8 Attribute from a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(defaultR8==dfltoutR8), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  ESMF_R8 list
    !-------------------------------------------------------------------------
      inR8l = (/1,2,3/)
      count = 3
      !EX_UTest
      ! Add an ESMF_R8 list Attribute to a GridComp Test
      call ESMF_AttributeSet(gridcomp, name="AttrR8l", count=count, &
        valueList=inR8l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an ESMF_R8l Attribute to a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an ESMF_R8 list Attribute from a GridComp Test
      call ESMF_AttributeGet(gridcomp, name="AttrR8l", count=count, &
        valueList=outR8l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an ESMF_R8l Attribute from a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and. all (inR8l==outR8l), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Destroy an Attribute on a GridComp Test
      call ESMF_AttributeDestroy(gridcomp, name="AttrR8l", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Destroying an ESMF_R8l Attribute on a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      defaultR8l = (/7,8,9/)
      !EX_UTest
      ! Get an ESMF_R8 list Attribute from a GridComp Test
      call ESMF_AttributeGet(gridcomp, name="AttrR8l", count=count, &
        valueList=dfltoutR8l, defaultvalue=defaultR8l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default ESMF_R8l Attribute from a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and. all (defaultR8l==dfltoutR8l), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
    !-------------------------------------------------------------------------
    !  Character
    !-------------------------------------------------------------------------
      inChar = "charAttribute"
      attrName = "char_"
      !EX_UTest
      ! Add a char Attribute to a GridComp Test
      call ESMF_AttributeSet(gridcomp, name=attrname, value=inChar, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding a char Attribute to a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get a char Attribute from a GridComp Test
      call ESMF_AttributeGet(gridcomp, name=attrname, value=outChar, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a char Attribute from a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and. (inChar==outChar), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Destroy an Attribute on a GridComp Test
      call ESMF_AttributeDestroy(gridcomp, name=attrname, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Destroying a char Attribute on a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      defaultChar = "charAttributeDefault"
      !EX_UTest
      ! Get a default char Attribute from a GridComp Test
      call ESMF_AttributeGet(gridcomp, name=attrname, value=dfltoutChar, &
        defaultvalue=defaultChar, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default char Attribute from a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and. (defaultChar==dfltoutChar), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
    !-------------------------------------------------------------------------
    !  Character list
    !-------------------------------------------------------------------------
      InCharl(1) = "Character String 1"
      InCharl(2) = "Character String 2"
      InCharl(3) = "Character String 3"
      defaultCharl(1) = "Character String 5"
      defaultCharl(2) = "Character String 6"
      defaultCharl(3) = "Character String 7"
      count = 3

      !EX_UTest
      ! Set a char list Attribute on a GridComp Test
      call ESMF_AttributeSet(gridcomp, name="Charl", count=count, &
        valueList=InCharl, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an Attribute char list on a GridComp test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
  
      !EX_UTest
      ! Get a char list Attribute on a GridComp Test
      call ESMF_AttributeGet(gridcomp, name="Charl", count=count, &
        valueList=OutCharl, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting an Attribute char list from a GridComp test"
      call ESMF_Test((rc==ESMF_SUCCESS) .and. all (InCharl==OutCharl), &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Destroy an Attribute on a GridComp Test
      call ESMF_AttributeDestroy(gridcomp, name="Charl", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Destroying a Character list Attribute on a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Destroy an Attribute on a GridComp Test, again
      call ESMF_AttributeDestroy(gridcomp, name="Charl", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Destroying a Character list Attribute on a GridComp Test, again"
      call ESMF_Test((rc/=ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get a char list default Attribute on a GridComp Test
      call ESMF_AttributeGet(gridcomp, name="Charl", count=count, &
        valueList=DfltOutCharl, defaultvalueList=defaultCharl, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting a default Attribute char list from a GridComp test"
      call ESMF_Test((rc==ESMF_SUCCESS) .and. all (DfltOutCharl == defaultCharl), &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Logical
    !-------------------------------------------------------------------------
      attrname = "flag"
      inLog = .true.

      !EX_UTest
      ! Set a logical attribute - scalar version
      call ESMF_AttributeSet(gridcomp, name=attrname, value=inLog, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCESS"
      write(name, *) "Setting GridComp Attribute (type Fortran logical scalar)"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      outLog = .false.
      !EX_UTest
      ! Get a logical attribute - scalar version
      call ESMF_AttributeGet(gridcomp, name=attrname, value=outLog, rc=rc)
      write(failMsg, *) "Did not return logical .TRUE."
      write(name, *) "Getting GridComp Attribute (type Fortran logical scalar)"
      call ESMF_Test((rc == ESMF_SUCCESS).and.(inLog .eqv. outLog),   &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Destroy an Attribute on a GridComp Test
      call ESMF_AttributeDestroy(gridcomp, name=attrname, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Destroying a logical Attribute on a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      dfltoutLog = .false.
      defaultLog = .true.
      !EX_UTest
      ! Get a logical attribute - scalar version
      call ESMF_AttributeGet(gridcomp, name=attrname, value=dfltoutLog, &
        defaultvalue=defaultLog, rc=rc)
      write(failMsg, *) "Did not return logical .TRUE."
      write(name, *) "Getting GridComp default Attribute (type Fortran logical scalar)"
      call ESMF_Test((rc == ESMF_SUCCESS).and.(defaultLog .eqv. dfltoutLog),   &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Logical list
    !-------------------------------------------------------------------------
      attrname = "flag gridcomp"
      inLogl = (/ .true., .false., .true. /)
      count = 3

      !EX_UTest
      ! Set a logical attribute - gridcomp version
      call ESMF_AttributeSet(gridcomp, name=attrname, count=count, &
        valueList=inLogl, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCESS"
      write(name, *) "Setting GridComp Attribute (type Fortran logical gridcomp)"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      outLogl = .false.
      !EX_UTest
      ! Get a logical attribute - gridcomp version
      call ESMF_AttributeGet(gridcomp, name=attrname,  count=count, &
        valueList=outLogl, rc=rc)
      write(failMsg, *) "Did not return logical .TRUE."
      write(name, *) "Getting GridComp Attribute (type Fortran logical gridcomp)"
      call ESMF_Test((rc == ESMF_SUCCESS) .and. all (inLogl .eqv. outLogl),   &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Destroy an Attribute on a GridComp Test
      call ESMF_AttributeDestroy(gridcomp, name=attrname, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Destroying a logical list Attribute on a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
  
      dfltoutLogl = .false.
      defaultLogl = .true.
      !EX_UTest
      ! Get a logical attribute - gridcomp version
      call ESMF_AttributeGet(gridcomp, name=attrname, count=count, &
        valueList=dfltoutLogl, defaultvalue=defaultLogl, rc=rc)
      write(failMsg, *) "Did not return logical .TRUE."
      write(name, *) "Getting GridComp default Attribute (type Fortran logical gridcomp)"
      call ESMF_Test((rc == ESMF_SUCCESS) .and. all (defaultLogl .eqv. dfltoutLogl),   &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Attribute package - custom
    !-------------------------------------------------------------------------
      attpackList(1) = "Custom1"
      attpackList(2) = "Custom2"
      attpackList(3) = "Custom3"
      count = 3
      conv = "customconvention"
      purp = "custompurpose"
      
      !EX_UTest
      ! Create a custom Attribute package on a GridComp Test
      call ESMF_AttributeAdd(gridcomp, convention=conv, &
        purpose=purp, attrList=attpackList, count=count, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a custom Attribute package on a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      attrname = "Custom1"
      attrvalue = "m/s"
      
      !EX_UTest
      ! Set an Attribute in an Attribute package on a GridComp Test
      call ESMF_AttributeSet(gridcomp, name=attrname, value=attrvalue, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an Attribute in an Attribute package on a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      attrname = "Custom2"
      count = 3

      !EX_UTest
      ! Set a char list Attribute in an Attribute package on a GridComp Test
      call ESMF_AttributeSet(gridcomp, name=attrname, count=count, &
        valueList=attpackList, convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting a char list Attribute in an Attribute package on a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get a char list Attribute in an Attribute package on a GridComp Test
      call ESMF_AttributeGet(gridcomp, name=attrname, count=count, &
        valueList=attpackListOut, convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting a char list Attribute in an Attribute package on a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS) .and. all (attpackList == attpackListOut), &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Destroy an Attribute in an Attribute package on a GridComp Test
      call ESMF_AttributeDestroy(gridcomp, name=attrname, convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Destroying an Attribute in an Attribute package on a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      attpackDfltList(1) = "Custom4"
      attpackDfltList(2) = "Custom5"
      attpackDfltList(3) = "Custom6"

      !EX_UTest
      ! Get a char list default Attribute in an Attribute package on a GridComp Test
      call ESMF_AttributeGet(gridcomp, name=attrname, count=count, &
        valueList=attpackListOut2, defaultvalueList=attpackDfltList, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting a default Attribute character list in an Attribute package on a GridComp test"
      call ESMF_Test((rc==ESMF_SUCCESS) .and. all (attpackListOut2 == attpackDfltList), &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Write the Attribute package from a GridComp Test
      call ESMF_AttributeWrite(gridcomp, convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Writing an Attribute package from a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Attribute package - standard
    !-------------------------------------------------------------------------
      conv = 'CF'
      purp = 'general'
      
      !EX_UTest
      ! Create an Attribute package on a GridComp Test
      call ESMF_AttributeAdd(gridcomp, convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a standard Attribute package on a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      attrname = "discipline"
      attrvalue = "atmosphere"
      
      !EX_UTest
      ! Set an Attribute in an Attribute package on a GridComp Test
      call ESMF_AttributeSet(gridcomp, name=attrname, value=attrvalue, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an Attribute in an Attribute package on a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      attpackListTNames(1) = "ESMF_I4"
      attpackListTNames(2) = "ESMF_I4list"
      attpackListTNames(3) = "ESMF_I8"
      attpackListTNames(4) = "ESMF_I8list"
      attpackListTNames(5) = "ESMF_R4"
      attpackListTNames(6) = "ESMF_R4list"
      attpackListTNames(7) = "ESMF_R8"
      attpackListTNames(8) = "ESMF_R8list"
      attpackListTNames(9) = "Logical_"
      attpackListTNames(10) = "Logical_list"
      attpackListTNames(11) = "Character_"
      attpackListTNames(12) = "Character_list"
      count=12
    
      !EX_UTest
      ! Add multiple Attributes to an Attribute package on a GridComp Test
      call ESMF_AttributeAdd(gridcomp, convention=conv, purpose=purp, &
        attrList=attpackListTNames, count=count, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding multiple Attributes to a standard Attribute package on a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Set an ESMF_I4 Attribute in an Attribute package on a GridComp Test
      call ESMF_AttributeSet(gridcomp, name="ESMF_I4", value=inI4, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an ESMF_I4 Attribute in an Attribute package on a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      count = 3
      !EX_UTest
      ! Set an ESMF_I4list Attribute in an Attribute package on a GridComp Test
      call ESMF_AttributeSet(gridcomp, name="ESMF_I4list", count=count, valueList=inI4l, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an ESMF_I4list Attribute in an Attribute package on a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Set an ESMF_I8 Attribute in an Attribute package on a GridComp Test
      call ESMF_AttributeSet(gridcomp, name="ESMF_I8", value=inI8, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an ESMF_I8 Attribute in an Attribute package on a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      count = 3
      !EX_UTest
      ! Set an ESMF_I8list Attribute in an Attribute package on a GridComp Test
      call ESMF_AttributeSet(gridcomp, name="ESMF_I8list", count=count, valueList=inI8l, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an ESMF_I8list Attribute in an Attribute package on a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Set an ESMF_R4 Attribute in an Attribute package on a GridComp Test
      call ESMF_AttributeSet(gridcomp, name="ESMF_R4", value=inR4, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an ESMF_R4 Attribute in an Attribute package on a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      count = 3
      !EX_UTest
      ! Set an ESMF_R4list Attribute in an Attribute package on a GridComp Test
      call ESMF_AttributeSet(gridcomp, name="ESMF_R4list", count=count, valueList=inR4l, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an ESMF_R4list Attribute in an Attribute package on a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Set an ESMF_R8 Attribute in an Attribute package on a GridComp Test
      call ESMF_AttributeSet(gridcomp, name="ESMF_R8", value=inR8, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an ESMF_R8 Attribute in an Attribute package on a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      count = 3
      !EX_UTest
      ! Set an ESMF_R8list Attribute in an Attribute package on a GridComp Test
      call ESMF_AttributeSet(gridcomp, name="ESMF_R8list", count=count, valueList=inR8l, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an ESMF_R8list Attribute in an Attribute package on a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Set an ESMF_R8 Attribute in an Attribute package on a GridComp Test
      call ESMF_AttributeSet(gridcomp, name="Character_", value=attrvalue, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting a Character Attribute in an Attribute package on a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      count = 3
      attrname = "Character_list"
      !EX_UTest
      ! Set a char list Attribute in an Attribute package on a GridComp Test
      call ESMF_AttributeSet(gridcomp, name=attrname, count=count, &
        valueList=attpackList, convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting a char list Attribute in an Attribute package on a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get a char list attribute in an Attribute package on a GridComp Test
      call ESMF_AttributeGet(gridcomp, name=attrname, count=count, &
        valueList=attpackListOut3, convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting a char list Attribute in an Attribute package on a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS) .and. all (attpackList == attpackListOut), &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Destroy an Attribute in an Attribute package on a GridComp Test
      call ESMF_AttributeDestroy(gridcomp, name=attrname, convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Destroying an Attribute in an Attribute package on a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Destroy an Attribute in an Attribute package on a GridComp Test, again
      call ESMF_AttributeDestroy(gridcomp, name=attrname, convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Destroying an Attribute in an Attribute package on a GridComp Test, again"
      call ESMF_Test((rc/=ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      attpackDfltList(1) = "Custom4"
      attpackDfltList(2) = "Custom5"
      attpackDfltList(3) = "Custom6"

      !EX_UTest
      ! Get a char list default Attribute in an Attribute package on a GridComp Test
      call ESMF_AttributeGet(gridcomp, name=attrname, count=count, &
        valueList=attpackListOut4, defaultvalueList=attpackDfltList2, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting a default Attribute character list in an Attribute package on a GridComp test"
      call ESMF_Test((rc==ESMF_SUCCESS) .and. all (attpackListOut2 == attpackDfltList), &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Write the Attribute package from a GridComp Test
      call ESMF_AttributeWrite(gridcomp, convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Writing an Attribute package from a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Attribute hierarchy linking
    !-------------------------------------------------------------------------

      !EX_UTest
      ! Link a GridComp Attribute hierarchy to a State Attribute hierarchy GridComp Test
      call ESMF_AttributeSet(gridcomp, sfg, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Linking a GridComp hierarchy to a State hierarchy Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Link a GridComp Attribute hierarchy to a CplComp Attribute hierarchy GridComp Test
      call ESMF_AttributeSet(gridcomp, cfg, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Linking a GridComp hierarchy to a CplComp hierarchy Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Link a GridComp Attribute hierarchy to a GridComp Attribute hierarchy GridComp Test
      call ESMF_AttributeSet(gridcomp, gfg, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Linking a GridComp hierarchy to a GridComp hierarchy Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Attribute Info
    !-------------------------------------------------------------------------

      attrname="Character_"
      attrvalue="stuff"
      ! Set a Character Attribute on a GridComp to test the get info calls
      call ESMF_AttributeSet(gridcomp, name=attrname, value=attrvalue, rc=rc)

      !EX_UTest
      ! Get the Attribute count from a GridComp Test
      call ESMF_AttributeGet(gridcomp, count=count, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting the Attribute count from a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(count.ge.0), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get Attribute info by name from a GridComp Test
      call ESMF_AttributeGet(gridcomp, name=attrname, typekind=attrTK, &
        count=items, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting Attribute info by name from a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS) &
                                       .and.(attrTK==ESMF_TYPEKIND_CHARACTER) &
                                       .and.(items==1), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get Attribute info by num from a GridComp Test
      call ESMF_AttributeGet(gridcomp, attributeIndex=count, name=attrnameOut, &
        typekind=attrTK, count=count, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting Attribute info by num from a GridComp Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(attrname==attrnameOut) &
                                       .and.(attrTK==ESMF_TYPEKIND_CHARACTER) &
                                       .and.(items==1), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      ! clean up
      call ESMF_CplCompDestroy(cfg, rc=rc)
      
      call ESMF_GridCompDestroy(gridcomp, rc=rc)
      call ESMF_GridCompDestroy(gfg, rc=rc)

      call ESMF_StateDestroy(sfg, rc=rc)
      
      if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
#endif

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
end program ESMF_AttributeGridCompUTest
