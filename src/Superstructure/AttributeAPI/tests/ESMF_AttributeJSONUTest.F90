! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_AttributeJSONUTest

!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_AttributeJSONUTest - Attribute JSON tests
!
! !DESCRIPTION:
!
! The code in this file drives F90 Attribute unit tests.
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
  integer                :: rc

  ! cumulative result: count failures; no failures equals "all pass"
  integer                :: result = 0

#ifdef ESMF_TESTEXHAUSTIVE

  type(ESMF_GridComp)     :: gridcomp
  type(ESMF_Field)        :: field1, field2
  type(ESMF_State)        :: state

  type(ESMF_AttPack)      :: attpack
  character(ESMF_MAXSTR)  :: conv, purp
  character(ESMF_MAXSTR), dimension(3)  :: attpackNames

  character(1024)         :: output, check

  integer                 :: i
  logical                 :: passed

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

#ifdef ESMF_TESTEXHAUSTIVE

  !-------------------------------------------------------------------------
  !   Set up object hierarchy for JSON test
  !-------------------------------------------------------------------------

    ! Construct a gridded component ESMF object that will be decorated with
    ! Attributes to output to JSON
    gridcomp = ESMF_GridCompCreate(name="gridcomp_json", petList=(/0/), rc=rc)

    ! Construct a field ESMF object that will be decorated with
    ! Attributes to output to JSON
    field1 = ESMF_FieldEmptyCreate(name="sst", rc=rc)

    ! Construct a field ESMF object that will be decorated with
    ! Attributes to output to JSON
    field2 = ESMF_FieldEmptyCreate(name="icefrac", rc=rc)

    ! Construct an import state ESMF object that will contain a fieldbundle
    state = ESMF_StateCreate(name="importState",  &
                             stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)

    ! Add a fieldbundle to the import state (links attributes also)
    call ESMF_StateAdd(state, fieldList=(/field1, field2/), rc=rc)

    ! Link import state attributes to the gridded component
    !call ESMF_AttributeLink(gridcomp, state, rc=rc)

    !-------------------------------------------------------------------------
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set up object hierarchy for JSON test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !-------------------------------------------------------------------------


  !-------------------------------------------------------------------------
  !-------------------------------------------------------------------------
  ! Add Attribute packages to the objects
  !-------------------------------------------------------------------------
  !-------------------------------------------------------------------------

    ! Add the ESMF/General package (wrapper to CF/Extended package)
    ! and then nest it inside of our own json/general package
    conv = "json"
    purp = "general"
    attpackNames(1) = "connected"
    attpackNames(2) = "timestamp"
    attpackNames(3) = "transferOfferGeomObject"

    call ESMF_AttributeAdd(field1, "json", "extended", &
                           attrList=attpackNames, &
                           attpack=attpack, rc=rc)
    call ESMF_AttributeAdd(field1, convention=conv, purpose=purp, &
                           attrList=(/"name         ", "standard_name", "units        " /), &
                           nestConvention="json", nestPurpose="extended", &
                           attpack=attpack, rc=rc)

    call ESMF_AttributeSet(field1, name="connected", value="false", &
                           attpack=attpack, rc=rc)
    call ESMF_AttributeSet(field1, name="timestamp", value="20091201000000", &
                           attpack=attpack, rc=rc)
    call ESMF_AttributeSet(field1, name="transferOfferGeomObject", &
                           value="will provide", attpack=attpack, rc=rc)

    call ESMF_AttributeSet(field1, name="name", value="sst", &
                           attpack=attpack, rc=rc)
    call ESMF_AttributeSet(field1, name="standard_name", &
                           value="sea_surface_temp", attpack=attpack, rc=rc)
    call ESMF_AttributeSet(field1, name="units", value="C", &
                           attpack=attpack, rc=rc)

    call ESMF_AttributeAdd(field2, "json", "extended", &
                           attrList=attpackNames, &
                           attpack=attpack, rc=rc)
    call ESMF_AttributeAdd(field2, convention=conv, purpose=purp, &
                           attrList=(/"name         ", "standard_name", "units        " /), &
                           nestConvention="json", nestPurpose="extended", &
                           attpack=attpack, rc=rc)

    call ESMF_AttributeSet(field2, name="connected", value="false", &
                           attpack=attpack, rc=rc)
    call ESMF_AttributeSet(field2, name="timestamp", value="20091201000000", &
                           attpack=attpack, rc=rc)
    call ESMF_AttributeSet(field2, name="transferOfferGeomObject", &
                           value="will provide", attpack=attpack, rc=rc)


    call ESMF_AttributeSet(field2, name="name", value="ssaf", &
                           attpack=attpack, rc=rc)
    call ESMF_AttributeSet(field2, name="standard_name", &
                           value="sea_ice_area_fraction", attpack=attpack, rc=rc)
    call ESMF_AttributeSet(field2, name="units", value="C", &
                           attpack=attpack, rc=rc)

    call ESMF_AttributeAdd(state, "json", "general", &
                           attrList=(/ "intent", "name  " /), &
                           attpack=attpack, rc=rc)

    call ESMF_AttributeSet(state, name="intent", value="import", &
                           attpack=attpack, rc=rc)
    call ESMF_AttributeSet(state, name="name", value="myImport", &
                           attpack=attpack, rc=rc)

    ! Add nested
    call ESMF_AttributeAdd(state, "json", "not-so-general", &
                           attrList=(/ "nestAttr1", "nestAttr2" /), &
                           nestConvention=(/ "json" /), &
                           nestPurpose=(/ "general" /), &
                           attpack=attpack, rc=rc)

    call ESMF_AttributeSet(state, name="nestAttr1", value="nestAttr1Val", &
                           attpack=attpack, rc=rc)
    call ESMF_AttributeSet(state, name="nestAttr2", value="nestAttr2Val", &
                           attpack=attpack, rc=rc)



    !-------------------------------------------------------------------------
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set up Attribute packages to write to JSON"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !-------------------------------------------------------------------------


    !-------------------------------------------------------------------------
    !EX_UTest
    ! Write JSON stream for the State
    call ESMF_AttPackStreamJSON(attpack, flattenPackList=.true., &
        includeUnset=.true., includeLinks=.true., output=output, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Write JSON stream for the State"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Write JSON stream to the default log
    call ESMF_LogWrite(output, ESMF_LOGMSG_JSON, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Write JSON stream to the default log"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !-------------------------------------------------------------------------

    check = '{"state":{"nestAttr1$json$not-so-general":"nestAttr1Val","nestAttr2$json$not-so-general":"nestAttr2Val"&
    &,"intent$json$general":"import","name$json$general":"myImport","linkList":[{"field":{"name$json$general":"sst",&
    &"standard_name$json$general":"sea_surface_temp","units$json$general":"C","connected$json$extended":"false",&
    &"timestamp$json$extended":"20091201000000","transferOfferGeomObject$json$extended":"will provide"}},{"field":&
    &{"name$json$general":"ssaf","standard_name$json$general":"sea_ice_area_fraction","units$json$general":"C",&
    &"connected$json$extended":"false","timestamp$json$extended":"20091201000000",&
    &"transferOfferGeomObject$json$extended":"will provide"}}]}}'

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Validate JSON string
    write(failMsg, *) "JSON string is no longer the same"
    write(name, *) "Validate JSON string"
    do, i=1, len (output)
      if (output(i:i) /= check(i:i)) exit
    end do
    passed = i > len (output)
    if (.not. passed) then
      write(failMsg, *) "JSON string mismatch at character", i,  &
        ' (', ichar (output(i:i)), ' vs', ichar (check(i:i)), ')'
    endif
    call ESMF_Test(passed, name, failMsg, result, ESMF_SRCLINE)
    !-------------------------------------------------------------------------


    !-------------------------------------------------------------------------
    !EX_UTest
    ! Write JSON stream for the State
    call ESMF_AttPackStreamJSON(attpack, flattenPackList=.false., &
        includeUnset=.true., includeLinks=.true., output=output, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Write JSON stream for the State"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Write JSON stream to the default log
    call ESMF_LogWrite(output, ESMF_LOGMSG_JSON, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Write JSON stream to the default log"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !-------------------------------------------------------------------------

    check = '{"state":{"packList":[{"attrList":{"nestAttr1$json$not-so-general":"nestAttr1Val",&
    &"nestAttr2$json$not-so-general":"nestAttr2Val"},"packList":[{"attrList":{"intent$json$general":"import",&
    &"name$json$general":"myImport"}}]}],"linkList":[{"field":{"packList":[{"attrList":{"name$json$general":&
    &"sst","standard_name$json$general":"sea_surface_temp","units$json$general":"C"},"packList":[{"attrList":&
    &{"connected$json$extended":"false","timestamp$json$extended":"20091201000000",&
    &"transferOfferGeomObject$json$extended":"will provide"}}]}]}},{"field":{"packList":[{"attrList":&
    &{"name$json$general":"ssaf","standard_name$json$general":"sea_ice_area_fraction",&
    &"units$json$general":"C"},"packList":[{"attrList":{"connected$json$extended":"false",&
    &"timestamp$json$extended":"20091201000000","transferOfferGeomObject$json$extended":"will provide"}}]}]}}]}}'

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Validate JSON string
    write(failMsg, *) "JSON string is no longer the same"
    write(name, *) "Validate JSON string"
    do, i=1, len (output)
      if (output(i:i) /= check(i:i)) exit
    end do
    passed = i > len (output)
    if (.not. passed) then
      write(failMsg, *) "JSON string mismatch at character", i,  &
        ' (', ichar (output(i:i)), ' vs', ichar (check(i:i)), ')'
    endif
    call ESMF_Test(passed, name, failMsg, result, ESMF_SRCLINE)
    !-------------------------------------------------------------------------

    call ESMF_AttributeAdd(gridcomp, convention=conv, purpose=purp, &
                           attrList=(/"event     ", "phaseLabel", "phase     " /), &
                           attpack=attpack, rc=rc)

    call ESMF_AttributeSet(gridcomp, name="event", value="start_phase", &
                           attpack=attpack, rc=rc)
    call ESMF_AttributeSet(gridcomp, name="phaseLabel", value="IPDv01p2", &
                           attpack=attpack, rc=rc)
    call ESMF_AttributeSet(gridcomp, name="phase", value="0", &
                           attpack=attpack, rc=rc)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Write JSON stream for the GridComp
    call ESMF_AttPackStreamJSON(attpack, flattenPackList=.true., &
        includeUnset=.true., includeLinks=.true., output=output, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Write JSON stream for the GridComp"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Write JSON stream to the default log
    call ESMF_LogWrite(output, ESMF_LOGMSG_JSON, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Write JSON stream to the default log"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !-------------------------------------------------------------------------

    check = '{"comp":{"event$json$general":"start_phase","phaseLabel$json$general":"IPDv01p2","phase$json$general":"0"}}'

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Validate JSON string
    write(failMsg, *) "JSON string is no longer the same"
    write(name, *) "Validate JSON string"
    do, i=1, len (output)
      if (output(i:i) /= check(i:i)) exit
    end do
    passed = i > len (output)
    if (.not. passed) then
      write(failMsg, *) "JSON string mismatch at character", i,  &
        ' (', ichar (output(i:i)), ' vs', ichar (check(i:i)), ')'
    endif
    call ESMF_Test(passed, name, failMsg, result, ESMF_SRCLINE)
    !-------------------------------------------------------------------------

  !------------------------------------------------------------------------
  ! clean up
  call ESMF_FieldDestroy(field1, rc=rc)
  if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_FieldDestroy(field2, rc=rc)
  if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_StateDestroy(state, rc=rc)
  if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_GridCompDestroy(gridcomp, rc=rc)
  if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

#endif
  
  if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
end program ESMF_AttributeJSONUTest
