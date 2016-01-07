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
program ESMF_AttributeArrayUTest

!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_AttributeArrayUTest - Attribute Array Unit Tests
!
! !DESCRIPTION:
!
! The code in this file drives F90 Attribute Array unit tests.
! The companion file ESMF\_Attribute.F90 contains the definitions for the
! Attribute methods.
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
      type(ESMF_Array)       :: array
      type(ESMF_ArraySpec)   :: arrayspec
      type(ESMF_DistGrid)    :: distgrid
      character(ESMF_MAXSTR) :: attrname, attrnameOut, attrvalue
      integer                :: rc, count, items
      type(ESMF_TypeKind_Flag)    :: attrTK

      real(ESMF_KIND_R8)                     :: inR8, outR8, defaultR8, dfltoutR8
      real(ESMF_KIND_R8), dimension(3)       :: inR8l, defaultR8l, dfltoutR8l, outR8l
      character(ESMF_MAXSTR)           :: inChar, outChar, defaultChar, dfltoutChar
      real(ESMF_KIND_R8), dimension(4)       :: defaultR8lWrong

      ! non exhaustive constant value variables
      real(ESMF_KIND_R8)                        :: outConstantR8
      real(ESMF_KIND_R8), dimension(3)          :: outConstantR8l
      character(ESMF_MAXSTR)                    :: outConstantChar
      character(ESMF_MAXSTR), dimension(3)      :: outConstantCharl
  
      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0
#ifdef ESMF_TESTEXHAUSTIVE
      ! individual test failure message

      ! local variables
      integer                :: itemCount
      character(ESMF_MAXSTR) :: conv, purp

      integer(ESMF_KIND_I4)                     :: inI4, outI4, defaultI4, dfltoutI4
      integer(ESMF_KIND_I4), dimension(3)       :: inI4l, outI4l, defaultI4l, dfltoutI4l
      integer(ESMF_KIND_I8)                     :: inI8, outI8, defaultI8, dfltoutI8
      integer(ESMF_KIND_I8), dimension(3)       :: inI8l, outI8l, defaultI8l, dfltoutI8l
      real(ESMF_KIND_R4)                     :: inR4, outR4, defaultR4, dfltoutR4
      real(ESMF_KIND_R4), dimension(3)       :: inR4l, outR4l, defaultR4l, dfltoutR4l
      real(ESMF_KIND_R8), dimension(10)       :: outR8lLong
      character(ESMF_MAXSTR)                     :: inEmpty, outEmpty
      character(ESMF_MAXSTR), dimension(3)       :: inCharl, defaultCharl, dfltoutCharl, outCharl
      character(ESMF_MAXSTR), dimension(4)       :: defaultCharlWrong
      character(ESMF_MAXSTR), dimension(10)       :: outCharlLong
      logical                          :: inLog, outLog, defaultLog, dfltoutLog
      logical, dimension(3)            :: inLogl, defaultLogl, dfltoutLogl, outLogl
      logical, dimension(4)             :: defaultLoglWrong
      logical, dimension(10)            :: outLoglLong

      ! exhaustive constant value variables
      integer(ESMF_KIND_I4)                     :: outConstantI4
      integer(ESMF_KIND_I4), dimension(3)       :: outConstantI4l
      integer(ESMF_KIND_I8)                     :: outConstantI8
      integer(ESMF_KIND_I8), dimension(3)       :: outConstantI8l
      real(ESMF_KIND_R4)                        :: outConstantR4
      real(ESMF_KIND_R4), dimension(3)          :: outConstantR4l
      logical                                   :: outConstantLogical
      logical, dimension(3)                     :: outConstantLogicall

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
      ! preparations
      ! array and grid
      call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
      distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/5,5/), &
        regDecomp=(/2,3/), rc=rc)
      array = ESMF_ArrayCreate(distgrid, arrayspec, rc=rc)
      if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------
!  ARRAY
!-------------------------------------------------------------------------
    
#ifdef ESMF_TESTEXHAUSTIVE

    !-------------------------------------------------------------------------
    !  Empty value
    !-------------------------------------------------------------------------
      !EX_UTest
      ! Add an empty value character Attribute to an Array Test
      call ESMF_AttributeSet(array, name="EmptyValue", value="", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an empty value character Attribute to an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an empty valArrayue character from an Array Test
      call ESMF_AttributeGet(array, name="EmptyValue", value=outEmpty, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an empty value character Attribute from an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(""==outEmpty), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Empty value from variable
    !-------------------------------------------------------------------------
      inEmpty = ""
      !EX_UTest
      ! Add an empty value character to an Array Test
      call ESMF_AttributeSet(array, name="EmptyValue", value=inEmpty, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an empty value character Attribute to an Array Test 2"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an empty value character from an Array Test
      call ESMF_AttributeGet(array, name="EmptyValue", value=outEmpty, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an empty value character Attribute from an Array Test 2"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(inEmpty==outEmpty), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Long value
    !-------------------------------------------------------------------------
      !EX_UTest
      ! Add a long value character to an Array Test
      call ESMF_AttributeSet(array, name="LongValue", value= &
        "This is a really long line " // &
        "that's broken into multiple lines " // &
        "to compile, and it is also a runon " // &
        "sentence, which is bad grammar but a good" // &
        " test of how the Attributes behave with long" // &
        " values, yada yada yada!!!", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding a long value character Attribute to an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Get an Attribute which was not set
    !-------------------------------------------------------------------------

      !EX_UTest
      ! Get an ESMF_I4 Attribute from an Array Test
      call ESMF_AttributeGet(array, name="NotHere", value=outI4, rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_ATTR_NOTSET"
      write(name, *) "Getting a nonexistent Attribute from an Array Test"
      call ESMF_Test((rc==ESMF_RC_ATTR_NOTSET), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
    !-------------------------------------------------------------------------
    !  ESMF_I4
    !-------------------------------------------------------------------------
      inI4 = 4
      !EX_UTest
      ! Add an ESMF_I4 Attribute to an Array Test
      call ESMF_AttributeSet(array, name="AttrI4", value=inI4, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an ESMF_I4 Attribute to an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an ESMF_I4 Attribute from an Array Test
      call ESMF_AttributeGet(array, name="AttrI4", value=outI4, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an ESMF_I4 Attribute from an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(inI4==outI4), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Remove an Attribute on an Array Test
      call ESMF_AttributeRemove(array, name="AttrI4", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Removing an ESMF_I4 Attribute on an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      defaultI4 = 7
      !EX_UTest
      ! Get an ESMF_I4 Attribute from a Field Test
      call ESMF_AttributeGet(array, name="AttrI4", value=dfltoutI4, &
        defaultvalue=defaultI4, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default ESMF_I4 Attribute from an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(defaultI4==dfltoutI4), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Constant value from variable
    !-------------------------------------------------------------------------
      !EX_UTest
      ! Add a constant numerical Attribute to an Array Test
      call ESMF_AttributeSet(array, name="ConstantI4", value=42, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding a constant I4 Attribute to an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get a constant numerical Attribute from an Array Test
      call ESMF_AttributeGet(array, name="ConstantI4", value=outConstantI4, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a constant I4 Attribute from an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(42==outConstantI4), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  ESMF_I4 list
    !-------------------------------------------------------------------------
      inI4l = (/1,2,3/)
      !EX_UTest
      ! Add an ESMF_I4 list Attribute to an Array Test
      call ESMF_AttributeSet(array, name="AttrI4l", &
        valueList=inI4l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an ESMF_I4l Attribute to an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an ESMF_I4 list Attribute from an Array Test
      call ESMF_AttributeGet(array, name="AttrI4l", &
        valueList=outI4l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an ESMF_I4l Attribute from an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and. all (inI4l==outI4l), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Remove an Attribute on an Array Test
      call ESMF_AttributeRemove(array, name="AttrI4l", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Removeing an ESMF_I4l Attribute on an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      defaultI4l = (/4,2,7/)
      !EX_UTest
      ! Get an ESMF_I4 list Attribute from a Field Test
      call ESMF_AttributeGet(array, name="AttrI4l", &
        valueList=dfltoutI4l, defaultvalueList=defaultI4l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default ESMF_I4l Attribute from an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and. all (defaultI4l==dfltoutI4l), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Constant value from variable
    !-------------------------------------------------------------------------
      !EX_UTest
      ! Add a constant numerical Attribute to an Array Test
      call ESMF_AttributeSet(array, name="ConstantI4l", &
        valueList=(/1,2,3/), rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding a constant I4 list Attribute to an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get a constant numerical Attribute from an Array Test
      call ESMF_AttributeGet(array, name="ConstantI4l", &
        valueList=outConstantI4l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a constant I4 list Attribute from an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and. all((/1,2,3/)==outConstantI4l), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  ESMF_I8
    !-------------------------------------------------------------------------
      inI8 = 4
      !EX_UTest
      ! Add an ESMF_I8 Attribute to an Array Test
      call ESMF_AttributeSet(array, name="AttrI8", value=inI8, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an ESMF_I8 Attribute to an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an ESMF_I8 Attribute from an Array Test
      call ESMF_AttributeGet(array, name="AttrI8", value=outI8, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an ESMF_I8 Attribute from an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and. (inI8==outI8), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Remove an Attribute on an Array Test
      call ESMF_AttributeRemove(array, name="AttrI8", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Removeing an ESMF_I8 Attribute on an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      defaultI8 = 7
      !EX_UTest
      ! Get an ESMF_I8 Attribute from a Field Test
      call ESMF_AttributeGet(array, name="AttrI8", value=dfltoutI8, &
        defaultvalue=defaultI8, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default ESMF_I8 Attribute from an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(defaultI8==dfltoutI8), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Constant value from variable
    !-------------------------------------------------------------------------
      !EX_UTest
      ! Add a constant numerical Attribute to an Array Test
      call ESMF_AttributeSet(array, name="ConstantI8", value=42, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding a constant I8 Attribute to an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get a constant numerical Attribute from an Array Test
      ! expect fail because this will default to I4
      call ESMF_AttributeGet(array, name="ConstantI8", value=outConstantI8, rc=rc)
      write(failMsg, *) "Did not return ESMC_RC_ATTR_WRONGTYPE"
      write(name, *) "Getting a constant I8 Attribute from an Array Test"
      call ESMF_Test((rc==ESMC_RC_ATTR_WRONGTYPE), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  ESMF_I8 list
    !-------------------------------------------------------------------------
      inI8l = (/1,2,3/)
      !EX_UTest
      ! Add an ESMF_I8 list Attribute to an Array Test
      call ESMF_AttributeSet(array, name="AttrI8l", &
        valueList=inI8l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an ESMF_I8l Attribute to an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an ESMF_I8 list Attribute from an Array Test
      call ESMF_AttributeGet(array, name="AttrI8l", &
        valueList=outI8l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an ESMF_I8l Attribute from an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and. all (inI8l==outI8l), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Remove an Attribute on an Array Test
      call ESMF_AttributeRemove(array, name="AttrI8l", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Removeing an ESMF_I8l Attribute on an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      defaultI8l = (/4,2,7/)
      !EX_UTest
      ! Get an ESMF_I8 list Attribute from a Field Test
      call ESMF_AttributeGet(array, name="AttrI8l", &
        valueList=dfltoutI8l, defaultvalueList=defaultI8l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default ESMF_I8l Attribute from an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and. all (defaultI8l==dfltoutI8l), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Constant value from variable
    !-------------------------------------------------------------------------
      !EX_UTest
      ! Add a constant numerical Attribute to an Array Test
      call ESMF_AttributeSet(array, name="ConstantI8l", &
        valueList=(/1,2,3/), rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding a constant I8 list Attribute to an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get a constant numerical Attribute from an Array Test
      ! expect fail because this will default to I4
      call ESMF_AttributeGet(array, name="ConstantI8l", &
        valueList=outConstantI8l, rc=rc)
      write(failMsg, *) "Did not return ESMC_RC_ATTR_WRONGTYPE"
      write(name, *) "Getting a constant I8 list Attribute from an Array Test"
      call ESMF_Test((rc==ESMC_RC_ATTR_WRONGTYPE), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  ESMF_R4
    !-------------------------------------------------------------------------
      inR4 = 4
      !EX_UTest
      ! Add an ESMF_R4 Attribute to an Array Test
      call ESMF_AttributeSet(array, name="AttrR4", value=inR4, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an ESMF_R4 Attribute to an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an ESMF_R4 Attribute from an Array Test
      call ESMF_AttributeGet(array, name="AttrR4", value=outR4, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an ESMF_R4 Attribute from an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(inR4==outR4), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Remove an Attribute on an Array Test
      call ESMF_AttributeRemove(array, name="AttrR4", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Removeing an ESMF_R4 Attribute on an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      defaultR4 = 7
      !EX_UTest
      ! Get an ESMF_R4 Attribute from a Field Test
      call ESMF_AttributeGet(array, name="AttrR4", value=dfltoutR4, &
        defaultvalue=defaultR4, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default ESMF_R4 Attribute from an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(defaultR4==dfltoutR4), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Constant value from variable
    !-------------------------------------------------------------------------
      !EX_UTest
      ! Add a constant numerical Attribute to an Array Test
      call ESMF_AttributeSet(array, name="ConstantR4", value=4.2, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding a constant R4 Attribute to an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get a constant numerical Attribute from an Array Test
      call ESMF_AttributeGet(array, name="ConstantR4", value=outConstantR4, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a constant R4 Attribute from an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(4.2==outConstantR4), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  ESMF_R4 list
    !-------------------------------------------------------------------------
      inR4l = (/1,2,3/)
      !EX_UTest
      ! Add an ESMF_R4 list Attribute to an Array Test
      call ESMF_AttributeSet(array, name="AttrR4l", &
        valueList=inR4l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an ESMF_R4l Attribute to an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get an ESMF_R4 list Attribute from an Array Test
      call ESMF_AttributeGet(array, name="AttrR4l", &
        valueList=outR4l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an ESMF_R4l Attribute from an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and. all (inR4l==outR4l), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Remove an Attribute on an Array Test
      call ESMF_AttributeRemove(array, name="AttrR4l", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Removeing an ESMF_R4l Attribute on an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      defaultR4l = (/7,8,9/)
      !EX_UTest
      ! Get an ESMF_R4 list Attribute from a Field Test
      call ESMF_AttributeGet(array, name="AttrR4l", &
        valueList=dfltoutR4l, defaultvalueList=defaultR4l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default ESMF_R4l Attribute from an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and. all (defaultR4l==dfltoutR4l), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Constant value from variable
    !-------------------------------------------------------------------------
      !EX_UTest
      ! Add a constant numerical Attribute to an Array Test
      call ESMF_AttributeSet(array, name="ConstantR4l", &
        valueList=(/1.1,2.2,3.3/), rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding a constant R4 list Attribute to an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get a constant numerical Attribute from an Array Test
      call ESMF_AttributeGet(array, name="ConstantR4l", &
        valueList=outConstantR4l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a constant R4 list Attribute from an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and. all((/1.1,2.2,3.3/)==outConstantR4l), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

#endif
      
    !-------------------------------------------------------------------------
    !  ESMF_R8
    !-------------------------------------------------------------------------
      inR8 = 4
      !NEX_UTest
      ! Add an ESMF_R8 Attribute to an Array Test
      call ESMF_AttributeSet(array, name="AttrR8", value=inR8, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an ESMF_R8 Attribute to an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! Get an ESMF_R8 Attribute from an Array Test
      call ESMF_AttributeGet(array, name="AttrR8", value=outR8, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an ESMF_R8 Attribute from an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(inR8==outR8), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! Remove an Attribute on an Array Test
      call ESMF_AttributeRemove(array, name="AttrR8", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Removeing an ESMF_R8 Attribute on an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      defaultR8 = 7
      !NEX_UTest
      ! Get an ESMF_R8 Attribute from a Field Test
      call ESMF_AttributeGet(array, name="AttrR8", value=dfltoutR8, &
        defaultvalue=defaultR8, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default ESMF_R8 Attribute from an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(defaultR8==dfltoutR8), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Constant value from variable
    !-------------------------------------------------------------------------
      !NEX_UTest
      ! Add a constant numerical Attribute to an Array Test
      call ESMF_AttributeSet(array, name="ConstantR8", value=4.2, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding a constant R8 Attribute to an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! Get a constant numerical Attribute from an Array Test
      ! expect fail because this will default to R4
      call ESMF_AttributeGet(array, name="ConstantR8", value=outConstantR8, rc=rc)
      write(failMsg, *) "Did not return ESMC_RC_ATTR_WRONGTYPE"
      write(name, *) "Getting a constant R8 Attribute from an Array Test"
      call ESMF_Test((rc==ESMC_RC_ATTR_WRONGTYPE), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  ESMF_R8 list
    !-------------------------------------------------------------------------
      inR8l = (/1,2,3/)
      !NEX_UTest
      ! Add an ESMF_R8 list Attribute to an Array Test
      call ESMF_AttributeSet(array, name="AttrR8l", &
        valueList=inR8l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an ESMF_R8l Attribute to an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! Get an ESMF_R8 list Attribute from an Array Test
      call ESMF_AttributeGet(array, name="AttrR8l", &
        valueList=outR8l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an ESMF_R8l Attribute from an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS) .and. all (inR8l==outR8l), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! Remove an Attribute on an Array Test
      call ESMF_AttributeRemove(array, name="AttrR8l", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Removeing an ESMF_R8l Attribute on an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      defaultR8l = (/7,8,9/)
      !NEX_UTest
      ! Get an ESMF_R8 list Attribute from a Field Test
      call ESMF_AttributeGet(array, name="AttrR8l", &
        valueList=dfltoutR8l, defaultvalueList=defaultR8l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default ESMF_R8l Attribute from an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and. all (defaultR8l==dfltoutR8l), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      defaultR8lWrong = (/6,7,8,9/)
      !NEX_UTest
      ! Get a R8 list default Attribute on a Array Test
      call ESMF_AttributeGet(array, name="AttrR8l", &
        valueList=dfltOutR8l, defaultvalueList=defaultR8lWrong, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting a wrong sized default Attribute R8 list from a Array test"
      call ESMF_Test((rc==ESMF_SUCCESS) .and. &
        all (dfltOutR8l==defaultR8lWrong(1:size(DfltOutR8l))), &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Constant value from variable
    !-------------------------------------------------------------------------
      !NEX_UTest
      ! Add a constant numerical Attribute to an Array Test
      call ESMF_AttributeSet(array, name="ConstantR8l", &
        valueList=(/1,2,3/), rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding a constant R8 list Attribute to an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! Get a constant numerical Attribute from an Array Test
      call ESMF_AttributeGet(array, name="ConstantR8l", &
        valueList=outConstantR8l, rc=rc)
      write(failMsg, *) "Did not return ESMC_RC_ATTR_WRONGTYPE"
      write(name, *) "Getting a constant R8 list Attribute from an Array Test"
      call ESMF_Test((rc==ESMC_RC_ATTR_WRONGTYPE), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Character
    !-------------------------------------------------------------------------
      inChar = "charAttribute"
      attrName = "char_"
      !NEX_UTest
      ! Add a char Attribute to an Array Test
      call ESMF_AttributeSet(array, name=attrname, value=inChar, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding a char Attribute to an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! Get a char Attribute from an Array Test
      call ESMF_AttributeGet(array, name=attrname, value=outChar, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a char Attribute from an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and. (inChar==outChar), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! Remove an Attribute on an Array Test
      call ESMF_AttributeRemove(array, name=attrname, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Removeing a char Attribute on an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      defaultChar = "charAttributeDefault"
      !NEX_UTest
      ! Get a default char Attribute from a Field Test
      call ESMF_AttributeGet(array, name=attrname, value=dfltoutChar, &
        defaultvalue=defaultChar, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a default char Attribute from an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and. (defaultChar==dfltoutChar), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Constant value from variable
    !-------------------------------------------------------------------------
      !NEX_UTest
      ! Add a constant character Attribute to an Array Test
      call ESMF_AttributeSet(array, name="ConstantChar", value="imacharacter", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding a constant character Attribute to an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! Get a constant character Attribute from an Array Test
      call ESMF_AttributeGet(array, name="ConstantChar", value=outConstantChar, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a constant character Attribute from an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.("imacharacter"==outConstantChar), &
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
      ! Set a char list Attribute on a Array Test
      call ESMF_AttributeSet(array, name="Charl", &
        valueList=InCharl(1:1), rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an Attribute char list on an Array test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
  
      !EX_UTest
      ! Get a char list Attribute on a Array Test
      call ESMF_AttributeGet(array, name="Charl", &
        valueList=OutCharl, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting an Attribute char list from an Array test"
      call ESMF_Test((rc==ESMF_SUCCESS) .and. all (InCharl(1:1)==OutCharl(1:1)), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Remove an Attribute on an Array Test
      call ESMF_AttributeRemove(array, name="Charl", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Removeing a Character list Attribute on an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Remove an Attribute on an Array Test, again
      call ESMF_AttributeRemove(array, name="Charl", rc=rc)
      write(failMsg, *) "Did not return ESMC_RC_NOT_FOUND"
      write(name, *) "Removeing a Character list Attribute on an Array Test, again"
      call ESMF_Test((rc==ESMC_RC_NOT_FOUND), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get a char list default Attribute on a Array Test
      call ESMF_AttributeGet(array, name="Charl", &
        valueList=DfltOutCharl, defaultvalueList=defaultCharl, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting a default Attribute char list from a Array test"
      call ESMF_Test((rc==ESMF_SUCCESS) .and. all (DfltOutCharl == defaultCharl), &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get a char list default Attribute on a Array Test
      call ESMF_AttributeGet(array, name="Charl", &
        valueList=DfltOutCharl, defaultvalueList=defaultCharlWrong, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting a wrong sized default Attribute char list from a Array test"
      call ESMF_Test((rc==ESMF_SUCCESS) .and. &
        all (DfltOutCharl==defaultCharlWrong(1:size(DfltOutCharl))), &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Constant value from variable
    !-------------------------------------------------------------------------
      !EX_UTest
      ! Add a constant character list Attribute to an Array Test
      call ESMF_AttributeSet(array, name="ConstantCharList", &
        valueList=(/"imachar1","imachar2","imachar3"/), rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding a constant character list Attribute to an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get a constant character list Attribute from an Array Test
      call ESMF_AttributeGet(array, name="ConstantCharList", &
        valueList=outConstantCharl, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a constant character list Attribute from an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and. &
                      all((/"imachar1","imachar2","imachar3"/)==outConstantCharl), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Logical
    !-------------------------------------------------------------------------
      attrname = "flag"
      inLog = .true.

      !EX_UTest
      ! Set a logical attribute - scalar version
      call ESMF_AttributeSet(array, name=attrname, value=inLog, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCESS"
      write(name, *) "Setting Array Attribute (type Fortran logical scalar)"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      outLog = .false.
      !EX_UTest
      ! Get a logical attribute - scalar version
      call ESMF_AttributeGet(array, name=attrname, value=outLog, rc=rc)
      write(failMsg, *) "Did not return logical .TRUE."
      write(name, *) "Getting Array Attribute (type Fortran logical scalar)"
      call ESMF_Test((rc == ESMF_SUCCESS).and.(inLog .eqv. outLog),   &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Remove an Attribute on an Array Test
      call ESMF_AttributeRemove(array, name=attrname, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Removeing a logical Attribute on an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      dfltoutLog = .false.
      defaultLog = .true.
      !EX_UTest
      ! Get a logical attribute - scalar version
      call ESMF_AttributeGet(array, name=attrname, value=dfltoutLog, &
        defaultvalue=defaultLog, rc=rc)
      write(failMsg, *) "Did not return logical .TRUE."
      write(name, *) "Getting Array default Attribute (type Fortran logical scalar)"
      call ESMF_Test((rc == ESMF_SUCCESS).and.(defaultLog .eqv. dfltoutLog),   &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Constant value from variable
    !-------------------------------------------------------------------------
      !EX_UTest
      ! Add a constant Logical Attribute to an Array Test
      call ESMF_AttributeSet(array, name="ConstantLogical", value=.true., rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding a constant Logical Attribute to an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get a constant Logical Attribute from an Array Test
      call ESMF_AttributeGet(array, name="ConstantLogical", value=outConstantLogical, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a constant Logical Attribute from an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(.true..eqv.outConstantLogical), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Logical list
    !-------------------------------------------------------------------------
      attrname = "flag array"
      inLogl = (/ .true., .false., .true. /)

      !EX_UTest
      ! Set a logical attribute - array version
      call ESMF_AttributeSet(array, name=attrname, &
        valueList=inLogl, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCESS"
      write(name, *) "Setting Array Attribute (type Fortran logical array)"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      outLogl = .false.
      !EX_UTest
      ! Get a logical attribute - array version
      call ESMF_AttributeGet(array, name=attrname,  &
        valueList=outLogl, rc=rc)
      write(failMsg, *) "Did not return logical .TRUE."
      write(name, *) "Getting Array Attribute (type Fortran logical array)"
      call ESMF_Test((rc == ESMF_SUCCESS) .and. all (inLogl .eqv. outLogl), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Remove an Attribute on an Array Test
      call ESMF_AttributeRemove(array, name=attrname, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Removeing a logical list Attribute on an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
  
      dfltoutLogl = (/.false.,.false.,.false./)
      defaultLogl = (/.true.,.true.,.true./)
      !EX_UTest
      ! Get a logical attribute - array version
      call ESMF_AttributeGet(array, name=attrname, &
        valueList=dfltoutLogl, defaultvalueList=defaultLogl, rc=rc)
      write(failMsg, *) "Did not return logical .TRUE."
      write(name, *) "Getting Array default Attribute (type Fortran logical array)"
      call ESMF_Test((rc == ESMF_SUCCESS) .and. all (defaultLogl .eqv. dfltoutLogl),   &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      defaultLoglWrong = (/.true.,.true.,.true.,.true./)
      !EX_UTest
      ! Get a logical list default Attribute on a Array Test
      call ESMF_AttributeGet(array, name="Logl", &
        valueList=dfltOutLogl, defaultvalueList=defaultLoglWrong, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting a wrong sized default Attribute logical list from a Array test"
      call ESMF_Test((rc==ESMF_SUCCESS) .and. &
        all (dfltOutLogl.eqv.defaultLoglWrong(1:size(DfltOutLogl))), &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Constant value from variable
    !-------------------------------------------------------------------------
      !EX_UTest
      ! Add a constant Logical list Attribute to an Array Test
      call ESMF_AttributeSet(array, name="ConstantLogicalList", &
        valueList=(/.true.,.false.,.true./), rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding a constant Logical list Attribute to an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get a constant Logical list Attribute from an Array Test
      call ESMF_AttributeGet(array, name="ConstantLogicalList", &
        valueList=outConstantLogicall, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting a constant Logical list Attribute from an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and. &
                      all((/.true.,.false.,.true./).eqv.outConstantLogicall), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  WRONG SIZE ARRAY TESTS
    !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  ESMF_R8 list  -  wrong size array
    !-------------------------------------------------------------------------

      !EX_UTest
      ! Add an ESMF_R8 list Attribute to an Array Test
      call ESMF_AttributeSet(array, name="AttrR8l", &
        valueList=inR8l, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an ESMF_R8l Attribute to an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      itemCount = 4
      !EX_UTest
      ! Too Short Get an ESMF_R8 list Attribute from an Array Test
      call ESMF_AttributeGet(array, name="AttrR8l", &
        valueList=outR8lLong(1:2), itemCount=itemCount, rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_ATTR_ITEMSOFF"
      write(name, *) "Getting an ESMF_R8l Attribute from an Array Test with short valueList"
      call ESMF_Test(rc/=ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      itemCount = 3
      !EX_UTest
      ! Too Long Get an ESMF_R8 list Attribute from an Array Test
      call ESMF_AttributeGet(array, name="AttrR8l", &
        valueList=outR8lLong(4:8), itemCount=itemCount, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting an ESMF_R8l Attribute from an Array Test with long valueList"
      call ESMF_Test((rc==ESMF_SUCCESS).and. all (inR8l==outR8lLong(4:6) .and. &
                    itemCount==3), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !-------------------------------------------------------------------------
      !  Character list wrong size array
      !-------------------------------------------------------------------------
      !EX_UTest
      ! Set a char list Attribute on an Array Test
      call ESMF_AttributeSet(array, name="Charl", &
        valueList=InCharl, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an Attribute char list on an Array test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
  
      itemCount = 4
      !EX_UTest
      ! Too Short Get a char list Attribute from an Array Test
      call ESMF_AttributeGet(array, name="Charl", &
        valueList=outCharlLong(1:2),itemCount=itemCount, rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_ATTR_ITEMSOFF"
      write(name, *) "Getting an Attribute char list from an Array test with short valueList"
      call ESMF_Test((rc/=ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      itemCount = 3
      !EX_UTest
      ! Too Long Get a char list Attribute from an Array Test
      call ESMF_AttributeGet(array, name="Charl", &
        valueList=outCharlLong(4:8), itemCount=itemCount, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting a default Attribute char list from an Array test with long valueList"
      call ESMF_Test((rc==ESMF_SUCCESS).and. all (inCharl==outCharlLong(4:6) .and. &
                    itemCount==3), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Logical wrong size array
    !-------------------------------------------------------------------------
      !EX_UTest
      ! Set a logical attribute - array version
      call ESMF_AttributeSet(array, name=attrname, &
        valueList=inLogl, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCESS"
      write(name, *) "Setting an Attribute logical list on an Array test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      outLogl = .false.
      !EX_UTest
      ! Too Short Get an ESMF_R8 list Attribute from an Array Test
      call ESMF_AttributeGet(array, name=attrname,  &
        valueList=outLoglLong(1:2), rc=rc)
      write(failMsg, *) "Did not return ESMC_RC_ATTR_ITEMSOFF"
      write(name, *) "Getting an logical list Attribute from an Array Test with short valueList"
      call ESMF_Test((rc==ESMC_RC_ATTR_ITEMSOFF), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      itemCount = 3
      !EX_UTest
      ! Too Long Get an ESMF_R8 list Attribute from an Array Test
      call ESMF_AttributeGet(array, name=attrname, &
        valueList=outLoglLong(4:8), itemCount=itemCount, rc=rc)
      write(failMsg, *) "Did not return logical .TRUE."
      write(name, *) "Getting an logical list Attribute from an Array Test with long valueList"
      call ESMF_Test((rc == ESMF_SUCCESS).and. all (inLogl .eqv. outLoglLong(4:6)) .and. &
                    itemCount==3, name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

#endif

    !-------------------------------------------------------------------------
    !  Attribute Info
    !-------------------------------------------------------------------------

      attrname="Character_name"
      attrvalue="stuff"
      ! Set a Character Attribute on an Array to test the get info calls
      call ESMF_AttributeSet(array, name=attrname, value=attrvalue, rc=rc)

      !NEX_UTest
      ! Get the Attribute count from an Array Test
      call ESMF_AttributeGet(array, count=count, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting the Attribute count from an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(count.ge.0), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! Get Attribute info by name from an Array Test
      call ESMF_AttributeGet(array, name=attrname, typekind=attrTK, &
        itemcount=items, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting Attribute info by name from an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS) &
                                       .and.(attrTK==ESMF_TYPEKIND_CHARACTER) &
                                       .and.(items==1), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      ! Get Attribute info by num from an Array Test
      call ESMF_AttributeGet(array, attributeIndex=count, name=attrnameOut, &
        typekind=attrTK, itemcount=items, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
      write(name, *) "Getting Attribute info by num from an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS).and.(attrname==attrnameOut) &
                                       .and.(attrTK==ESMF_TYPEKIND_CHARACTER) &
                                       .and.(items==1), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      ! clean up
      call ESMF_ArrayDestroy(array, rc=rc)
      call ESMF_DistGridDestroy(distGrid, rc=rc)
      
      if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
end program ESMF_AttributeArrayUTest
