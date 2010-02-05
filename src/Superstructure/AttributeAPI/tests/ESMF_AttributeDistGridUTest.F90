! $Id: ESMF_AttributeDistGridUTest.F90,v 1.22.2.1 2010/02/05 20:03:39 svasquez Exp $
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
program ESMF_AttributeDistGridUTest

!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_AttributeDistGridUTest - Attribute DistGrid Unit Tests
!
! !DESCRIPTION:
!
! The code in this file drives F90 Attribute DistGrid unit tests.
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
      '$Id: ESMF_AttributeDistGridUTest.F90,v 1.22.2.1 2010/02/05 20:03:39 svasquez Exp $'
!------------------------------------------------------------------------------

!-------------------------------------------------------------------------
!=========================================================================

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

      ! local variables
      type(ESMF_DistGrid)    :: distgrid
      character(ESMF_MAXSTR) :: attrname, attrnameOut, attrvalue
      integer                :: rc, count, items
      type(ESMF_TypeKind)    :: attrTK

      real(ESMF_KIND_I8)                     :: inR8, outR8, defaultR8, dfltoutR8
      real(ESMF_KIND_I8), dimension(3)       :: inR8l, defaultR8l, dfltoutR8l, outR8l
      character(ESMF_MAXSTR)           :: inChar, outChar, defaultChar, dfltoutChar
      real(ESMF_KIND_I8), dimension(4)       :: defaultR8lWrong
  
  
      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

#ifdef ESMF_TESTEXHAUSTIVE

      ! local variables
      character(ESMF_MAXSTR) :: conv, nestconv, purp, nestpurp
      integer                :: itemCount

      integer(ESMF_KIND_I4)                     :: inI4, outI4, defaultI4, dfltoutI4
      integer(ESMF_KIND_I4), dimension(3)       :: inI4l, outI4l, defaultI4l, dfltoutI4l
      integer(ESMF_KIND_I8)                     :: inI8, outI8, defaultI8, dfltoutI8
      integer(ESMF_KIND_I8), dimension(3)       :: inI8l, outI8l, defaultI8l, dfltoutI8l
      real(ESMF_KIND_I4)                     :: inR4, outR4, defaultR4, dfltoutR4
      real(ESMF_KIND_I4), dimension(3)       :: inR4l, outR4l, defaultR4l, dfltoutR4l
      real(ESMF_KIND_I8), dimension(10)       :: outR8lLong
      character(ESMF_MAXSTR)                     :: inEmpty, outEmpty
      character(ESMF_MAXSTR), dimension(3)       :: inCharl, defaultCharl, dfltoutCharl, outCharl
      character(ESMF_MAXSTR), dimension(4)       :: defaultCharlWrong
      character(ESMF_MAXSTR), dimension(10)       :: outCharlLong
      logical                          :: inLog, outLog, defaultLog, dfltoutLog
      logical, dimension(3)            :: inLogl, defaultLogl, dfltoutLogl, outLogl
      logical, dimension(4)       :: defaultLoglWrong
      logical, dimension(10)            :: outLoglLong

      character(ESMF_MAXSTR), dimension(3)  :: attpackList, attpackListOut, &
                                               attpackListOut2, attpackDfltList, &
                                               attpackListOut3, attpackListOut4, &
                                               attpackDfltList2
      character(ESMF_MAXSTR), dimension(12) :: attpackListTNames, attpackListTNames2



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
      ! preparations
      ! distgrid and grid
      distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/5,5/), &
        regDecomp=(/2,3/), rc=rc)

!-------------------------------------------------------------------------
!  DISTGRID
!-------------------------------------------------------------------------
    
#ifdef ESMF_TESTEXHAUSTIVE

    !-------------------------------------------------------------------------
    !  Empty value
    !-------------------------------------------------------------------------
      !EX_UTest
      ! Add an empty value character Attribute to a DistGrid Test
      call ESMF_AttributeSet(distgrid, name="EmptyValue", value="", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an empty value character Attribute to a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an empty valDistGridue character from a DistGrid Test
      call ESMF_AttributeGet(distgrid, name="EmptyValue", value=outEmpty, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an empty value character Attribute from a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(""==outEmpty), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Empty value from variable
    !-------------------------------------------------------------------------
      inEmpty = ""
      !EX_UTest
      ! Add an empty value character to a DistGrid Test
      call ESMF_AttributeSet(distgrid, name="EmptyValue", value=inEmpty, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an empty value character Attribute to a DistGrid Test 2"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an empty value character from a DistGrid Test
      call ESMF_AttributeGet(distgrid, name="EmptyValue", value=outEmpty, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an empty value character Attribute from a DistGrid Test 2"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(inEmpty==outEmpty), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Long value
    !-------------------------------------------------------------------------
      !EX_UTest
      ! Add a long value character to a DistGrid Test
      call ESMF_AttributeSet(distgrid, name="LongValue", value= &
        "This is a really long line " // & 
        "that's broken into multiple lines " // & 
        "to compile, and it is also a runon " // & 
        "sentence, which is bad grammar but a good" // & 
        " test of how the Attributes behave with long" // & 
        " values, yada yada yada!!!", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding a long value character Attribute to a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Get an Attribute which was not set
    !-------------------------------------------------------------------------

      !EX_UTest
      ! Get an ESMF_I4 Attribute from a DistGrid Test
      call ESMF_AttributeGet(distgrid, name="NotHere", value=outI4, rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_ATTR_NOTSET"
      write(name, *) "Getting a nonexistent Attribute from a DistGrid Test"
      call ESMF_Test((rc==ESMF_RC_ATTR_NOTSET), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
    !-------------------------------------------------------------------------
    !  ESMF_I4
    !-------------------------------------------------------------------------
      inI4 = 4
      !EX_UTest
      ! Add an ESMF_I4 Attribute to a DistGrid Test
      call ESMF_AttributeSet(distgrid, name="AttrI4", value=inI4, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an ESMF_I4 Attribute to a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an ESMF_I4 Attribute from a DistGrid Test
      call ESMF_AttributeGet(distgrid, name="AttrI4", value=outI4, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an ESMF_I4 Attribute from a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(inI4==outI4), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Remove an Attribute on a DistGrid Test
      call ESMF_AttributeRemove(distgrid, name="AttrI4", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Removing an ESMF_I4 Attribute on a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      defaultI4 = 7
      !EX_UTest
      ! Get an ESMF_I4 Attribute from a Field Test
      call ESMF_AttributeGet(distgrid, name="AttrI4", value=dfltoutI4, &
        defaultvalue=defaultI4, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default ESMF_I4 Attribute from a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(defaultI4==dfltoutI4), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  ESMF_I4 list
    !-------------------------------------------------------------------------
      inI4l = (/1,2,3/)
      !EX_UTest
      ! Add an ESMF_I4 list Attribute to a DistGrid Test
      call ESMF_AttributeSet(distgrid, name="AttrI4l", &
        valueList=inI4l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an ESMF_I4l Attribute to a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an ESMF_I4 list Attribute from a DistGrid Test
      call ESMF_AttributeGet(distgrid, name="AttrI4l", &
        valueList=outI4l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an ESMF_I4l Attribute from a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and. all (inI4l==outI4l), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Remove an Attribute on a DistGrid Test
      call ESMF_AttributeRemove(distgrid, name="AttrI4l", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Removeing an ESMF_I4l Attribute on a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      defaultI4l = (/4,2,7/)
      !EX_UTest
      ! Get an ESMF_I4 list Attribute from a Field Test
      call ESMF_AttributeGet(distgrid, name="AttrI4l", &
        valueList=dfltoutI4l, defaultvalueList=defaultI4l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default ESMF_I4l Attribute from a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and. all (defaultI4l==dfltoutI4l), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  ESMF_I8
    !-------------------------------------------------------------------------
      inI8 = 4
      !EX_UTest
      ! Add an ESMF_I8 Attribute to a DistGrid Test
      call ESMF_AttributeSet(distgrid, name="AttrI8", value=inI8, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an ESMF_I8 Attribute to a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an ESMF_I8 Attribute from a DistGrid Test
      call ESMF_AttributeGet(distgrid, name="AttrI8", value=outI8, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an ESMF_I8 Attribute from a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and. (inI8==outI8), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Remove an Attribute on a DistGrid Test
      call ESMF_AttributeRemove(distgrid, name="AttrI8", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Removeing an ESMF_I8 Attribute on a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      defaultI8 = 7
      !EX_UTest
      ! Get an ESMF_I8 Attribute from a Field Test
      call ESMF_AttributeGet(distgrid, name="AttrI8", value=dfltoutI8, &
        defaultvalue=defaultI8, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default ESMF_I8 Attribute from a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(defaultI8==dfltoutI8), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  ESMF_I8 list
    !-------------------------------------------------------------------------
      inI8l = (/1,2,3/)
      !EX_UTest
      ! Add an ESMF_I8 list Attribute to a DistGrid Test
      call ESMF_AttributeSet(distgrid, name="AttrI8l", &
        valueList=inI8l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an ESMF_I8l Attribute to a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an ESMF_I8 list Attribute from a DistGrid Test
      call ESMF_AttributeGet(distgrid, name="AttrI8l", &
        valueList=outI8l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an ESMF_I8l Attribute from a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and. all (inI8l==outI8l), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Remove an Attribute on a DistGrid Test
      call ESMF_AttributeRemove(distgrid, name="AttrI8l", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Removeing an ESMF_I8l Attribute on a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      defaultI8l = (/4,2,7/)
      !EX_UTest
      ! Get an ESMF_I8 list Attribute from a Field Test
      call ESMF_AttributeGet(distgrid, name="AttrI8l", &
        valueList=dfltoutI8l, defaultvalueList=defaultI8l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default ESMF_I8l Attribute from a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and. all (defaultI8l==dfltoutI8l), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  ESMF_R4
    !-------------------------------------------------------------------------
      inR4 = 4
      !EX_UTest
      ! Add an ESMF_R4 Attribute to a DistGrid Test
      call ESMF_AttributeSet(distgrid, name="AttrR4", value=inR4, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an ESMF_R4 Attribute to a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an ESMF_R4 Attribute from a DistGrid Test
      call ESMF_AttributeGet(distgrid, name="AttrR4", value=outR4, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an ESMF_R4 Attribute from a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(inR4==outR4), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Remove an Attribute on a DistGrid Test
      call ESMF_AttributeRemove(distgrid, name="AttrR4", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Removeing an ESMF_R4 Attribute on a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      defaultR4 = 7
      !EX_UTest
      ! Get an ESMF_R4 Attribute from a Field Test
      call ESMF_AttributeGet(distgrid, name="AttrR4", value=dfltoutR4, &
        defaultvalue=defaultR4, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default ESMF_R4 Attribute from a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(defaultR4==dfltoutR4), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  ESMF_R4 list
    !-------------------------------------------------------------------------
      inR4l = (/1,2,3/)
      !EX_UTest
      ! Add an ESMF_R4 list Attribute to a DistGrid Test
      call ESMF_AttributeSet(distgrid, name="AttrR4l", &
        valueList=inR4l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an ESMF_R4l Attribute to a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an ESMF_R4 list Attribute from a DistGrid Test
      call ESMF_AttributeGet(distgrid, name="AttrR4l", &
        valueList=outR4l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an ESMF_R4l Attribute from a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and. all (inR4l==outR4l), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Remove an Attribute on a DistGrid Test
      call ESMF_AttributeRemove(distgrid, name="AttrR4l", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Removeing an ESMF_R4l Attribute on a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      defaultR4l = (/7,8,9/)
      !EX_UTest
      ! Get an ESMF_R4 list Attribute from a Field Test
      call ESMF_AttributeGet(distgrid, name="AttrR4l", &
        valueList=dfltoutR4l, defaultvalueList=defaultR4l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default ESMF_R4l Attribute from a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and. all (defaultR4l==dfltoutR4l), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

#endif
      
    !-------------------------------------------------------------------------
    !  ESMF_R8
    !-------------------------------------------------------------------------
      inR8 = 4
      !NEX_UTest
      ! Add an ESMF_R8 Attribute to a DistGrid Test
      call ESMF_AttributeSet(distgrid, name="AttrR8", value=inR8, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an ESMF_R8 Attribute to a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! Get an ESMF_R8 Attribute from a DistGrid Test
      call ESMF_AttributeGet(distgrid, name="AttrR8", value=outR8, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an ESMF_R8 Attribute from a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(inR8==outR8), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! Remove an Attribute on a DistGrid Test
      call ESMF_AttributeRemove(distgrid, name="AttrR8", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Removeing an ESMF_R8 Attribute on a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      defaultR8 = 7
      !NEX_UTest
      ! Get an ESMF_R8 Attribute from a Field Test
      call ESMF_AttributeGet(distgrid, name="AttrR8", value=dfltoutR8, &
        defaultvalue=defaultR8, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default ESMF_R8 Attribute from a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(defaultR8==dfltoutR8), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  ESMF_R8 list
    !-------------------------------------------------------------------------
      inR8l = (/1,2,3/)
      !NEX_UTest
      ! Add an ESMF_R8 list Attribute to a DistGrid Test
      call ESMF_AttributeSet(distgrid, name="AttrR8l", &
        valueList=inR8l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an ESMF_R8l Attribute to a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! Get an ESMF_R8 list Attribute from a DistGrid Test
      call ESMF_AttributeGet(distgrid, name="AttrR8l", &
        valueList=outR8l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an ESMF_R8l Attribute from a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS) .and. all (inR8l==outR8l), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! Remove an Attribute on a DistGrid Test
      call ESMF_AttributeRemove(distgrid, name="AttrR8l", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Removeing an ESMF_R8l Attribute on a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      defaultR8l = (/7,8,9/)
      !NEX_UTest
      ! Get an ESMF_R8 list Attribute from a Field Test
      call ESMF_AttributeGet(distgrid, name="AttrR8l", &
        valueList=dfltoutR8l, defaultvalueList=defaultR8l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default ESMF_R8l Attribute from a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and. all (defaultR8l==dfltoutR8l), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      defaultR8lWrong = (/6,7,8,9/)
      !NEX_UTest
      ! Get a R8 list default Attribute on a DistGrid Test
      call ESMF_AttributeGet(distgrid, name="AttrR8l", &
        valueList=dfltOutR8l, defaultvalueList=defaultR8lWrong, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting a wrong sized default Attribute R8 list from a DistGrid test"
      call ESMF_Test((rc==ESMF_SUCCESS) .and. &
        all (dfltOutR8l==defaultR8lWrong(1:size(DfltOutR8l))), &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Character
    !-------------------------------------------------------------------------
      inChar = "charAttribute"
      attrName = "char_"
      !NEX_UTest
      ! Add a char Attribute to a DistGrid Test
      call ESMF_AttributeSet(distgrid, name=attrname, value=inChar, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding a char Attribute to a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! Get a char Attribute from a DistGrid Test
      call ESMF_AttributeGet(distgrid, name=attrname, value=outChar, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a char Attribute from a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and. (inChar==outChar), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! Remove an Attribute on a DistGrid Test
      call ESMF_AttributeRemove(distgrid, name=attrname, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Removeing a char Attribute on a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      defaultChar = "charAttributeDefault"
      !NEX_UTest
      ! Get a default char Attribute from a Field Test
      call ESMF_AttributeGet(distgrid, name=attrname, value=dfltoutChar, &
        defaultvalue=defaultChar, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default char Attribute from a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and. (defaultChar==dfltoutChar), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
#ifdef ESMF_TESTEXHAUSTIVE

    !-------------------------------------------------------------------------
    !  Character list
    !-------------------------------------------------------------------------
      InCharl(1) = "Character String 1"
      InCharl(2) = "Character String 2"
      InCharl(3) = "Character String 3"
      defaultCharl(1) = "Character String 5"
      defaultCharl(2) = "Character String 6"
      defaultCharl(3) = "Character String 7"
      defaultCharlWrong(1) = "Character String 5"
      defaultCharlWrong(2) = "Character String 6"
      defaultCharlWrong(3) = "Character String 7"
      defaultCharlWrong(4) = "Character String 8"

      !EX_UTest
      ! Set a char list Attribute on a DistGrid Test
      call ESMF_AttributeSet(distgrid, name="Charl", &
        valueList=InCharl, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an Attribute char list on a DistGrid test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
  
      !EX_UTest
      ! Get a char list Attribute on a DistGrid Test
      call ESMF_AttributeGet(distgrid, name="Charl", &
        valueList=OutCharl, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting an Attribute char list from a DistGrid test"
      call ESMF_Test((rc==ESMF_SUCCESS) .and. all (InCharl==OutCharl), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Remove an Attribute on a DistGrid Test
      call ESMF_AttributeRemove(distgrid, name="Charl", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Removeing a Character list Attribute on a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Remove an Attribute on a DistGrid Test, again
      call ESMF_AttributeRemove(distgrid, name="Charl", rc=rc)
      write(failMsg, *) "Did not return ESMC_RC_NOT_FOUND"
      write(name, *) "Removeing a Character list Attribute on a DistGrid Test, again"
      call ESMF_Test((rc==ESMC_RC_NOT_FOUND), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get a char list default Attribute on a DistGrid Test
      call ESMF_AttributeGet(distgrid, name="Charl", &
        valueList=DfltOutCharl, defaultvalueList=defaultCharl, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting a default Attribute char list from a DistGrid test"
      call ESMF_Test((rc==ESMF_SUCCESS) .and. all (DfltOutCharl == defaultCharl), &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get a char list default Attribute on a DistGrid Test
      call ESMF_AttributeGet(distgrid, name="Charl", &
        valueList=DfltOutCharl, defaultvalueList=defaultCharlWrong, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting a wrong sized default Attribute char list from a DistGrid test"
      call ESMF_Test((rc==ESMF_SUCCESS) .and. &
        all (DfltOutCharl==defaultCharlWrong(1:size(DfltOutCharl))), &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Logical
    !-------------------------------------------------------------------------
      attrname = "flag"
      inLog = .true.

      !EX_UTest
      ! Set a logical attribute - scalar version
      call ESMF_AttributeSet(distgrid, name=attrname, value=inLog, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCESS"
      write(name, *) "Setting DistGrid Attribute (type Fortran logical scalar)"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      outLog = .false.
      !EX_UTest
      ! Get a logical attribute - scalar version
      call ESMF_AttributeGet(distgrid, name=attrname, value=outLog, rc=rc)
      write(failMsg, *) "Did not return logical .TRUE."
      write(name, *) "Getting DistGrid Attribute (type Fortran logical scalar)"
      call ESMF_Test((rc == ESMF_SUCCESS).and.(inLog .eqv. outLog),   &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Remove an Attribute on a DistGrid Test
      call ESMF_AttributeRemove(distgrid, name=attrname, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Removeing a logical Attribute on a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      dfltoutLog = .false.
      defaultLog = .true.
      !EX_UTest
      ! Get a logical attribute - scalar version
      call ESMF_AttributeGet(distgrid, name=attrname, value=dfltoutLog, &
        defaultvalue=defaultLog, rc=rc)
      write(failMsg, *) "Did not return logical .TRUE."
      write(name, *) "Getting DistGrid default Attribute (type Fortran logical scalar)"
      call ESMF_Test((rc == ESMF_SUCCESS).and.(defaultLog .eqv. dfltoutLog),   &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Logical list
    !-------------------------------------------------------------------------
      attrname = "flag distgrid"
      inLogl = (/ .true., .false., .true. /)

      !EX_UTest
      ! Set a logical attribute - distgrid version
      call ESMF_AttributeSet(distgrid, name=attrname, &
        valueList=inLogl, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCESS"
      write(name, *) "Setting DistGrid Attribute (type Fortran logical distgrid)"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      outLogl = .false.
      !EX_UTest
      ! Get a logical attribute - distgrid version
      call ESMF_AttributeGet(distgrid, name=attrname,  &
        valueList=outLogl, rc=rc)
      write(failMsg, *) "Did not return logical .TRUE."
      write(name, *) "Getting DistGrid Attribute (type Fortran logical distgrid)"
      call ESMF_Test((rc == ESMF_SUCCESS) .and. all (inLogl .eqv. outLogl), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Remove an Attribute on a DistGrid Test
      call ESMF_AttributeRemove(distgrid, name=attrname, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Removeing a logical list Attribute on a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
  
      dfltoutLogl = (/.false.,.false.,.false./)
      defaultLogl = (/.true.,.true.,.true./)
      !EX_UTest
      ! Get a logical attribute - distgrid version
      call ESMF_AttributeGet(distgrid, name=attrname, &
        valueList=dfltoutLogl, defaultvalueList=defaultLogl, rc=rc)
      write(failMsg, *) "Did not return logical .TRUE."
      write(name, *) "Getting DistGrid default Attribute (type Fortran logical distgrid)"
      call ESMF_Test((rc == ESMF_SUCCESS) .and. all (defaultLogl .eqv. dfltoutLogl),   &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      defaultLoglWrong = (/.true.,.true.,.true.,.true./)
      !EX_UTest
      ! Get a logical list default Attribute on a DistGrid Test
      call ESMF_AttributeGet(distgrid, name="Logl", &
        valueList=dfltOutLogl, defaultvalueList=defaultLoglWrong, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting a wrong sized default Attribute logical list from a DistGrid test"
      call ESMF_Test((rc==ESMF_SUCCESS) .and. &
        all (dfltOutLogl.eqv.defaultLoglWrong(1:size(DfltOutLogl))), &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  WRONG SIZE ARRAY TESTS
    !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  ESMF_R8 list  -  wrong size distgrid
    !-------------------------------------------------------------------------

      !EX_UTest
      ! Add an ESMF_R8 list Attribute to a DistGrid Test
      call ESMF_AttributeSet(distgrid, name="AttrR8l", &
        valueList=inR8l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an ESMF_R8l Attribute to a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      itemCount = 4
      !EX_UTest
      ! Too Short Get an ESMF_R8 list Attribute from a DistGrid Test
      call ESMF_AttributeGet(distgrid, name="AttrR8l", &
        valueList=outR8lLong(1:2), itemCount=itemCount, rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_ARG_BAD or wrong value"
      write(name, *) "Getting an ESMF_R8l Attribute from a DistGrid Test with short valueList"
      call ESMF_Test(rc==ESMF_RC_ARG_BAD, name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      itemCount = 3
      !EX_UTest
      ! Too Long Get an ESMF_R8 list Attribute from a DistGrid Test
      call ESMF_AttributeGet(distgrid, name="AttrR8l", &
        valueList=outR8lLong(4:8), itemCount=itemCount, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an ESMF_R8l Attribute from a DistGrid Test with long valueList"
      call ESMF_Test((rc==ESMF_SUCCESS).and. all (inR8l==outR8lLong(4:6) .and. &
                    itemCount==3), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !-------------------------------------------------------------------------
      !  Character list wrong size distgrid
      !-------------------------------------------------------------------------
      !EX_UTest
      ! Set a char list Attribute on a DistGrid Test
      call ESMF_AttributeSet(distgrid, name="Charl", &
        valueList=InCharl, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an Attribute char list on a DistGrid test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
  
      itemCount = 4
      !EX_UTest
      ! Too Short Get a char list Attribute from a DistGrid Test
      call ESMF_AttributeGet(distgrid, name="Charl", &
        valueList=outCharlLong(1:2),itemCount=itemCount, rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_ARG_BAD"
      write(name, *) "Getting an Attribute char list from a DistGrid test with short valueList"
      call ESMF_Test((rc==ESMF_RC_ARG_BAD), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      itemCount = 3
      !EX_UTest
      ! Too Long Get a char list Attribute from a DistGrid Test
      call ESMF_AttributeGet(distgrid, name="Charl", &
        valueList=outCharlLong(4:8), itemCount=itemCount, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting a default Attribute char list from a DistGrid test with long valueList"
      call ESMF_Test((rc==ESMF_SUCCESS).and. all (inCharl==outCharlLong(4:6) .and. &
                    itemCount==3), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Logical wrong size distgrid
    !-------------------------------------------------------------------------
      !EX_UTest
      ! Set a logical attribute - distgrid version
      call ESMF_AttributeSet(distgrid, name=attrname, &
        valueList=inLogl, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCESS"
      write(name, *) "Setting an Attribute logical list on a DistGrid test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      outLogl = .false.
      !EX_UTest
      ! Too Short Get an ESMF_R8 list Attribute from a DistGrid Test
      call ESMF_AttributeGet(distgrid, name=attrname,  &
        valueList=outLoglLong(1:2), rc=rc)
      write(failMsg, *) "Did not return logical .TRUE."
      write(name, *) "Getting an logical list Attribute from a DistGrid Test with short valueList"
      call ESMF_Test((rc /= ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      itemCount = 3
      !EX_UTest
      ! Too Long Get an ESMF_R8 list Attribute from a DistGrid Test
      call ESMF_AttributeGet(distgrid, name=attrname, &
        valueList=outLoglLong(4:8), itemCount=itemCount, rc=rc)
      write(failMsg, *) "Did not return logical .TRUE."
      write(name, *) "Getting an logical list Attribute from a DistGrid Test with long valueList"
      call ESMF_Test((rc == ESMF_SUCCESS).and. all (inLogl .eqv. outLoglLong(4:6)) .and. &
                    itemCount==3, name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Attribute package - custom
    !-------------------------------------------------------------------------
      conv = "customconvention"
      purp = "custompurpose"
      attpackList(1) = "Custom1"
      attpackList(2) = "Custom2"
      attpackList(3) = "Custom3"
      
      !EX_UTest
      ! Create a custom Attribute package on a DistGrid Test
      call ESMF_AttributeAdd(distgrid, convention=conv, &
        purpose=purp, attrList=attpackList, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a custom Attribute package on a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Get an Attribute which was not set from an Attribute package
    !-------------------------------------------------------------------------

      !EX_UTest
      ! Get an ESMF_I4 Attribute from a DistGrid Test
      call ESMF_AttributeGet(distgrid, name="NotHere", value=outI4, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_ATTR_NOTSET"
      write(name, *) "Getting a nonexistent Attribute from a DistGrid Test"
      call ESMF_Test((rc==ESMF_RC_ATTR_NOTSET), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      attrname = "Custom1"
      attrvalue = "m/s"
      
      !EX_UTest
      ! Set an Attribute in an Attribute package on a DistGrid Test
      call ESMF_AttributeSet(distgrid, name=attrname, value=attrvalue, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an Attribute in an Attribute package on a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      attrname = "Custom2"

      !EX_UTest
      ! Set a char list Attribute in an Attribute package on a DistGrid Test
      call ESMF_AttributeSet(distgrid, name=attrname, &
        valueList=attpackList, convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting a char list Attribute in an Attribute package on a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get a char list Attribute in an Attribute package on a DistGrid Test
      call ESMF_AttributeGet(distgrid, name=attrname, &
        valueList=attpackListOut, convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting a char list Attribute in an Attribute package on a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS) .and. all (attpackList == attpackListOut), &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Remove an Attribute in an Attribute package on a DistGrid Test
      call ESMF_AttributeRemove(distgrid, name=attrname, convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Removeing an Attribute in an Attribute package on a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      attpackDfltList(1) = "Custom4"
      attpackDfltList(2) = "Custom5"
      attpackDfltList(3) = "Custom6"

      !EX_UTest
      ! Get a char list default Attribute in an Attribute package on a DistGrid Test
      call ESMF_AttributeGet(distgrid, name=attrname, &
        valueList=attpackListOut2, defaultvalueList=attpackDfltList, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting a default Attribute character list in an Attribute package on a DistGrid test"
      call ESMF_Test((rc==ESMF_SUCCESS) .and. all (attpackListOut2 == attpackDfltList), &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Remove the entire Attribute package from a DistGrid Test
      call ESMF_AttributeRemove(distgrid, convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Removing the entire Attribute package from a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      attpackListTNames(1) = "ESMF_I4name"
      attpackListTNames(2) = "ESMF_I4namelist"
      attpackListTNames(3) = "ESMF_I8name"
      attpackListTNames(4) = "ESMF_I8namelist"
      attpackListTNames(5) = "ESMF_R4name"
      attpackListTNames(6) = "ESMF_R4namelist"
      attpackListTNames(7) = "ESMF_R8name"
      attpackListTNames(8) = "ESMF_R8namelist"
      attpackListTNames(9) = "Logical_name"
      attpackListTNames(10) = "Logical_namelist"
      attpackListTNames(11) = "Character_name"
      attpackListTNames(12) = "Character_namelist"
    
      !EX_UTest
      ! Add multiple Attributes to an Attribute package on a DistGrid Test
      call ESMF_AttributeAdd(distgrid, convention=conv, purpose=purp, &
        attrList=attpackListTNames, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding multiple Attributes to a standard Attribute package on a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Set an ESMF_I4name Attribute in an Attribute package on a DistGrid Test
      call ESMF_AttributeSet(distgrid, name="ESMF_I4name", value=inI4, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an ESMF_I4name Attribute in an Attribute package on a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Set an ESMF_I4namelist Attribute in an Attribute package on a DistGrid Test
      call ESMF_AttributeSet(distgrid, name="ESMF_I4namelist", valueList=inI4l, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an ESMF_I4namelist Attribute in an Attribute package on a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Set an ESMF_I8name Attribute in an Attribute package on a DistGrid Test
      call ESMF_AttributeSet(distgrid, name="ESMF_I8name", value=inI8, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an ESMF_I8name Attribute in an Attribute package on a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Set an ESMF_I8namelist Attribute in an Attribute package on a DistGrid Test
      call ESMF_AttributeSet(distgrid, name="ESMF_I8namelist", valueList=inI8l, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an ESMF_I8namelist Attribute in an Attribute package on a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Set an ESMF_R4name Attribute in an Attribute package on a DistGrid Test
      call ESMF_AttributeSet(distgrid, name="ESMF_R4name", value=inR4, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an ESMF_R4name Attribute in an Attribute package on a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Set an ESMF_R4namelist Attribute in an Attribute package on a DistGrid Test
      call ESMF_AttributeSet(distgrid, name="ESMF_R4namelist", valueList=inR4l, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an ESMF_R4namelist Attribute in an Attribute package on a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Set an ESMF_R8name Attribute in an Attribute package on a DistGrid Test
      call ESMF_AttributeSet(distgrid, name="ESMF_R8name", value=inR8, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an ESMF_R8name Attribute in an Attribute package on a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Set an ESMF_R8namelist Attribute in an Attribute package on a DistGrid Test
      call ESMF_AttributeSet(distgrid, name="ESMF_R8namelist", valueList=inR8l, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an ESMF_R8namelist Attribute in an Attribute package on a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Set a Logical Attribute in an Attribute package on a DistGrid Test
      call ESMF_AttributeSet(distgrid, name="Logical_name", value=inLog, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting a logical Attribute in an Attribute package on a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Set a Logical list Attribute in an Attribute package on a DistGrid Test
      call ESMF_AttributeSet(distgrid, name="Logical_namelist", valueList=inLogl, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting a logical list Attribute in an Attribute package on a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Set a character Attribute in an Attribute package on a DistGrid Test
      call ESMF_AttributeSet(distgrid, name="Character_name", value=attrvalue, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting a Character Attribute in an Attribute package on a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      attrname = "Character_namelist"
      !EX_UTest
      ! Set a char list Attribute in an Attribute package on a DistGrid Test
      call ESMF_AttributeSet(distgrid, name=attrname, &
        valueList=attpackList, convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting a char list Attribute in an Attribute package on a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Attribute package - custom nested
    !-------------------------------------------------------------------------
      nestconv = "customconvention_top"
      nestpurp = "custompurpose_top"
      attpackListTNames2(1) = "ESMF_I4name2"
      attpackListTNames2(2) = "ESMF_I4namelist2"
      attpackListTNames2(3) = "ESMF_I8name2"
      attpackListTNames2(4) = "ESMF_I8namelist2"
      attpackListTNames2(5) = "ESMF_R4name2"
      attpackListTNames2(6) = "ESMF_R4namelist2"
      attpackListTNames2(7) = "ESMF_R8name2"
      attpackListTNames2(8) = "ESMF_R8namelist2"
      attpackListTNames2(9) = "Logical_name2"
      attpackListTNames2(10) = "Logical_namelist2"
      attpackListTNames2(11) = "Character_name2"
      attpackListTNames2(12) = "Character_namelist2"
      attpackDfltList2(1) = "Custom4"
      attpackDfltList2(2) = "Custom5"
      attpackDfltList2(3) = "Custom6"
      attrname = "Character_namelist2"
      !EX_UTest
      ! Add multiple Attributes to an Attribute package on a DistGrid Test
      call ESMF_AttributeAdd(distgrid, convention=nestconv, purpose=nestpurp, &
        attrList=attpackListTNames2, nestConvention=conv, nestPurpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding multiple Attributes to a nested Attribute package on a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Remove an Attribute in an Attribute package on a DistGrid Test
      call ESMF_AttributeRemove(distgrid, name=attrname, convention=nestconv, &
        purpose=nestpurp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Removeing an Attribute in an Attribute package on a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Remove an Attribute in an Attribute package on a DistGrid Test, again
      call ESMF_AttributeRemove(distgrid, name=attrname, convention=nestconv, &
        purpose=nestpurp, rc=rc)
      write(failMsg, *) "Did not return ESMC_RC_NOT_FOUND"
      write(name, *) "Removeing an Attribute in an Attribute package on a DistGrid Test, again"
      call ESMF_Test((rc==ESMC_RC_NOT_FOUND), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get a char list default Attribute in an Attribute package on a DistGrid Test
      call ESMF_AttributeGet(distgrid, name=attrname, &
        valueList=attpackListOut4, defaultvalueList=attpackDfltList2, &
        convention=nestconv, purpose=nestpurp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting a default Attribute character list in an Attribute package on a DistGrid test"
      call ESMF_Test((rc==ESMF_SUCCESS) .and. all (attpackListOut4 == attpackDfltList2), &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      attrname = "Character_namelist"
      !EX_UTest
      ! Get a char list attribute in an Attribute package on a DistGrid Test
      call ESMF_AttributeGet(distgrid, name=attrname, &
        valueList=attpackListOut3, convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting a char list Attribute in an Attribute package on a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS) .and. all (attpackList == attpackListOut3), &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

#endif

    !-------------------------------------------------------------------------
    !  Attribute Info
    !-------------------------------------------------------------------------

      attrname="Character_name"
      attrvalue="stuff"
      ! Set a Character Attribute on a DistGrid to test the get info calls
      call ESMF_AttributeSet(distgrid, name=attrname, value=attrvalue, rc=rc)

      !NEX_UTest
      ! Get the Attribute count from a DistGrid Test
      call ESMF_AttributeGet(distgrid, count=count, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting the Attribute count from a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(count.ge.0), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! Get Attribute info by name from a DistGrid Test
      call ESMF_AttributeGet(distgrid, name=attrname, typekind=attrTK, &
        itemcount=items, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting Attribute info by name from a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS) &
                                       .and.(attrTK==ESMF_TYPEKIND_CHARACTER) &
                                       .and.(items==1), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! Get Attribute info by num from a DistGrid Test
      call ESMF_AttributeGet(distgrid, attributeIndex=count, name=attrnameOut, &
        typekind=attrTK, itemcount=items, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting Attribute info by num from a DistGrid Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(attrname==attrnameOut) &
                                       .and.(attrTK==ESMF_TYPEKIND_CHARACTER) &
                                       .and.(items==1), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

     !------------------------------------------------------------------------
      ! clean up
      call ESMF_DistGridDestroy(distgrid, rc=rc)
      
      if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
end program ESMF_AttributeDistGridUTest
