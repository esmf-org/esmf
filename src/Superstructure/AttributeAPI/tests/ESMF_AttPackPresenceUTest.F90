! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2013, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_AttPackPresenceUTest

!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_AttPackPresenceUTest - AttPack Presence Unit Tests
!
! !DESCRIPTION:
!
! The code in this file drives F90 Attribute ArrayBundle unit tests.
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
      type(ESMF_ArrayBundle)    :: arraybundle
      type(ESMF_Array), dimension(2) :: array
      type(ESMF_Array)               :: afb
      type(ESMF_ArraySpec) :: arrayspec
      type(ESMF_DistGrid) :: distgrid
      character(ESMF_MAXSTR) :: attrname, attrnameOut, attrvalue
      integer                :: rc, count, items
      type(ESMF_TypeKind_Flag)    :: attrTK

      real(ESMF_KIND_R8)                     :: inR8, outR8, defaultR8, dfltoutR8
      real(ESMF_KIND_R8), dimension(3)       :: inR8l, defaultR8l, dfltoutR8l, outR8l
      character(ESMF_MAXSTR)                     :: inChar, outChar, defaultChar, dfltoutChar
      real(ESMF_KIND_R8), dimension(4)       :: defaultR8lWrong

      ! non exhaustive constant value variables
      real(ESMF_KIND_R8)                        :: outConstantR8
      real(ESMF_KIND_R8), dimension(3)          :: outConstantR8l
      character(ESMF_MAXSTR)                    :: outConstantChar
      character(ESMF_MAXSTR), dimension(3)      :: outConstantCharl
   
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
      real(ESMF_KIND_R4)                     :: inR4, outR4, defaultR4, dfltoutR4
      real(ESMF_KIND_R4), dimension(3)       :: inR4l, outR4l, defaultR4l, dfltoutR4l
      real(ESMF_KIND_R8), dimension(10)       :: outR8lLong
      character(ESMF_MAXSTR)                     :: inEmpty, outEmpty
      character(ESMF_MAXSTR), dimension(3)       :: inCharl, defaultCharl, dfltoutCharl, outCharl
      character(ESMF_MAXSTR), dimension(4)       :: defaultCharlWrong
      character(ESMF_MAXSTR), dimension(10)       :: outCharlLong
      logical                          :: inLog, outLog, defaultLog, dfltoutLog
      logical, dimension(3)            :: inLogl, defaultLogl, dfltoutLogl, outLogl
      logical, dimension(4)       :: defaultLoglWrong
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

     character(ESMF_MAXSTR), dimension(3)  :: attpackList, &
                                               attpackListOut2, attpackDfltList, &
                                               attpackListOut3, attpackListOut4, &
                                               attpackDfltList2
      character(ESMF_MAXSTR), dimension(12) :: attpackListTNames, attpackListTNames2

      logical :: rc_logical
      character(ESMF_MAXSTR), dimension(1) :: exclusions

      logical :: isPresent
      type(ESMF_AttPack) :: attpack

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
      call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
      distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/5,5/), &
        regDecomp=(/2,3/), rc=rc)
      array(1) = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, rc=rc)
      array(2) = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, rc=rc)
      afb = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, rc=rc)
      arraybundle = ESMF_ArrayBundleCreate(arrayList=array, &
        name="MyArrayBundle", rc=rc)

!-------------------------------------------------------------------------
!  ARRAYBUNDLE
!-------------------------------------------------------------------------
    
#ifdef ESMF_TESTEXHAUSTIVE

    !-------------------------------------------------------------------------
    !  Attribute package - custom
    !-------------------------------------------------------------------------
      conv = "customconvention"
      purp = "custompurpose"
      attpackList(1) = "Custom1"
      attpackList(2) = "Custom2"
      attpackList(3) = "Custom3"
      
      !EX_UTest
      ! Create a custom Attribute package on an ArrayBundle Test
      call ESMF_AttributeAdd(arraybundle, convention=conv, &
        purpose=purp, attrList=attpackList, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a custom Attribute package on an ArrayBundle Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Get an Attribute which was not set from an Attribute package
    !-------------------------------------------------------------------------

      !EX_UTest
      ! Get an ESMF_I4 Attribute from an ArrayBundle Test
      call ESMF_AttributeGet(arraybundle, name="NotHere", value=outI4, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_ATTR_NOTSET"
      write(name, *) "Getting a nonexistent Attribute from an ArrayBundle Test"
      call ESMF_Test((rc==ESMF_RC_ATTR_NOTSET), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      attrname = "Custom1"
      attrvalue = "m/s"
  
      !EX_UTest
      ! Set an Attribute in an Attribute package on an ArrayBundle Test
      call ESMF_AttributeSet(arraybundle, name=attrname, value=attrvalue, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an Attribute in an Attribute package on an ArrayBundle Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      attrname = "Custom2"

      !EX_UTest
      ! Set a char list Attribute in an Attribute package on an ArrayBundle Test
      call ESMF_AttributeSet(arraybundle, name=attrname, &
        valueList=attpackList, convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting a char list Attribute in an Attribute package on an ArrayBundle Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get a char list Attribute in an Attribute package on an ArrayBundle Test
      call ESMF_AttributeGet(arraybundle, name=attrname, &
        valueList=attpackListOut3, convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting a char list Attribute in an Attribute package on an ArrayBundle Test"
      call ESMF_Test((rc==ESMF_SUCCESS) .and. all (attpackList == attpackListOut3), &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Remove an Attribute in an Attribute package on an ArrayBundle Test
      call ESMF_AttributeRemove(arraybundle, name=attrname, convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Removeing an Attribute in an Attribute package on an ArrayBundle Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      attpackDfltList(1) = "Custom4"
      attpackDfltList(2) = "Custom5"
      attpackDfltList(3) = "Custom6"
      !EX_UTest
      ! Get a char list default Attribute in an Attribute package on an ArrayBundle Test
      call ESMF_AttributeGet(arraybundle, name=attrname, &
        valueList=attpackListOut2, defaultvalueList=attpackDfltList, &
        convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting a default Attribute character list in an Attribute package on an ArrayBundle test"
      call ESMF_Test((rc==ESMF_SUCCESS) .and. all (attpackListOut2 == attpackDfltList), &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Remove the entire Attribute package from an ArrayBundle Test
      call ESMF_AttributeRemove(arraybundle, convention=conv, purpose=purp, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Removing the entire Attribute package from an ArrayBundle Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get ispresent from Attribute from an Array Test
      call ESMF_AttributeGet(arraybundle, name="NotHere", value=outI4, &
        convention=conv, purpose=purp, isPresent=isPresent, rc=rc)
      write(failMsg, *) "Did not return isPresent=False"
      write(name, *) "Getting an Attribute from a non-existent Attpack"
      call ESMF_Test((rc==ESMF_SUCCESS .and. isPresent.eqv..false.), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "isPresent = ", isPresent
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Attribute package - custom
    !-------------------------------------------------------------------------
      conv = "customconvention2"
      purp = "custompurpose2"
      attpackList(1) = "Custom1"
      attpackList(2) = "Custom2"
      attpackList(3) = "Custom3"
      
      !EX_UTest
      ! Create a custom Attribute package on an Array Test
      call ESMF_AttributeAdd(arraybundle, attrList=attpackList, convention=conv, &
        purpose=purp, attpack=attpack, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a custom Attribute package on an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !  Get an Attribute which was not set from an Attribute package
    !-------------------------------------------------------------------------

      !EX_UTest
      ! Get an ESMF_I4 Attribute from an Array Test
      call ESMF_AttributeGet(arraybundle, name="NotHere", value=outI4, &
        attpack=attpack, rc=rc)
      write(failMsg, *) "Did not return ESMF_RC_ATTR_NOTSET"
      write(name, *) "Getting a nonexistent Attribute from an Array Test"
      call ESMF_Test((rc==ESMF_RC_ATTR_NOTSET), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      attrname = "Custom1"
      attrvalue = "m/s"
      
      !EX_UTest
      ! Set an Attribute in an Attribute package on an Array Test
      call ESMF_AttributeSet(arraybundle, name=attrname, value=attrvalue, &
        attpack=attpack, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting an Attribute in an Attribute package on an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      attrname = "Custom2"

      !EX_UTest
      ! Set a char list Attribute in an Attribute package on an Array Test
      call ESMF_AttributeSet(arraybundle, name=attrname, &
        valueList=attpackList, attpack=attpack, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting a char list Attribute in an Attribute package on an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get a char list Attribute in an Attribute package on an Array Test
      call ESMF_AttributeGet(arraybundle, name=attrname, &
        valueList=attpackListOut3, attpack=attpack, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting a char list Attribute in an Attribute package on an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS) .and. all (attpackList == attpackListOut3), &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Remove an Attribute in an Attribute package on an Array Test
      call ESMF_AttributeRemove(arraybundle, name=attrname, attpack=attpack, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Removeing an Attribute in an Attribute package on an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      attpackDfltList(1) = "Custom4"
      attpackDfltList(2) = "Custom5"
      attpackDfltList(3) = "Custom6"

      !EX_UTest
      ! Get a char list default Attribute in an Attribute package on an Array Test
      call ESMF_AttributeGet(arraybundle, name=attrname, &
        valueList=attpackListOut2, defaultvalueList=attpackDfltList, &
        attpack=attpack, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting a default Attribute character list in an Attribute package on an Array test"
      call ESMF_Test((rc==ESMF_SUCCESS) .and. all (attpackListOut2 == attpackDfltList), &
        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Validate an Attribute package on an Array Test
      call ESMF_AttributeValidate(arraybundle, attpack, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Validate an Attribute package on an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Remove the entire Attribute package from an Array Test
      call ESMF_AttributeRemove(arraybundle, attpack=attpack, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Removing the entire Attribute package from an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Get ispresent from Attribute from an Array Test
      call ESMF_AttributeGet(arraybundle, name="NotHere", value=outI4, &
        attpack=attpack, isPresent=isPresent, rc=rc)
      write(failMsg, *) "Did not return isPresent=False"
      write(name, *) "Getting a nonexistent Attribute from an Array Test"
      call ESMF_Test((rc==ESMF_SUCCESS .and. isPresent.eqv..false.), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

#endif

      !------------------------------------------------------------------------
      ! clean up
      call ESMF_ArrayBundleDestroy(arraybundle, rc=rc)
      call ESMF_ArrayDestroy(afb, rc=rc)
      call ESMF_ArrayDestroy(array(1), rc=rc)
      call ESMF_ArrayDestroy(array(2), rc=rc)
      call ESMF_DistGridDestroy(distgrid, rc=rc)

      if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
end program ESMF_AttPackPresenceUTest
