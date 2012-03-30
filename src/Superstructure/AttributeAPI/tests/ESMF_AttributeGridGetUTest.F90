! $Id: ESMF_AttributeGridGetUTest.F90,v 1.4 2012/03/30 16:58:49 rokuingh Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2011, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_AttributeGridGetUTest

!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_AttributeGridGetUTest - Attribute GridGet Unit Tests
!
! !DESCRIPTION:
!
! The code in this file drives F90 Attribute GridGetInternal unit tests.
! The companion file ESMF\_AttributeInternals.F90 contains the definitions 
! for the Attribute methods.
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF         ! the ESMF Framework
  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
  '$Id: ESMF_AttributeGridGetUTest.F90,v 1.4 2012/03/30 16:58:49 rokuingh Exp $'
!------------------------------------------------------------------------------

!-------------------------------------------------------------------------
!=========================================================================

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name
  
  ! local variables
  type(ESMF_Grid)        :: grid
  type(ESMF_DistGrid)    :: distgrid
  integer                :: rc
  
  ! cumulative result: count failures; no failures equals "all pass"
  integer                :: result = 0

#ifdef ESMF_TESTEXHAUSTIVE

  integer(ESMF_KIND_I4)  :: outI4
  real(ESMF_KIND_R4)     :: outR4
  character(ESMF_MAXSTR) :: outCoordTypeKind, outIndexFlag, outStatus, outName

  integer(ESMF_KIND_I4)  :: distgridToGridMap(2), coordDimCount(2), coordDimMap(2,2)
  integer(ESMF_KIND_I4)  :: gridEdgeLWidth(2), gridEdgeUWidth(2), gridAlign(2)
  integer(ESMF_KIND_I4)  :: minIndex(2), maxIndex(2)
  integer(ESMF_KIND_I4)  :: exclusiveLBound(2), exclusiveUBound(2), exclusiveCount(2)
  integer(ESMF_KIND_I4)  :: computationalLBound(2), computationalUBound(2), computationalCount(2)
  integer(ESMF_KIND_I4)  :: totalLBound(2), totalUBound(2), totalCount(2)
  logical                :: isLBound(2), isUBound(2)

  character(ESMF_MAXSTR),dimension(3) :: inputList 
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

  distgrid=ESMF_DistGridCreate(minIndex=(/1,1/),maxIndex=(/10,10/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  grid=ESMF_GridCreate(distgrid=distgrid, coordTypeKind=ESMF_TYPEKIND_I4, &
                       name="AttributeTestGrid", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

#if 0

  integer :: minIndex(3)
  character(len=ESMF_MAXSTR) :: minIndexInput(2)
  minIndexInput(1) = '1'
  minIndexInput(2) = 'ESMF_STAGGERLOC_CENTER'
  call ESMF_AttributeGet(grid, name="minIndex", value=minIndex, &
                         auxInput=minIndexInput, rc=rc)

#endif

#ifdef ESMF_TESTEXHAUSTIVE

  !------------------------------------------------------------------------
  !------------------- Integer types --------------------------------------
  !------------------------------------------------------------------------

  !EX_UTest
  outI4 = 42
  call ESMF_AttributeGet(grid, name="ESMF:dimCount", value=outI4, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'dimCount' from a Grid via Attribute Test"
  call ESMF_Test((rc==ESMF_SUCCESS).and.(outI4==2), &
                  name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "dimCount= ", outI4

  !EX_UTest
  outR4 = 42
  call ESMF_AttributeGet(grid, name="ESMF:dimCount", value=outR4, rc=rc)
  write(failMsg, *) "Did not return ESMF_RC_ATTR_NOTSET"
  write(name, *) "Incorrectly getting internal info from a Grid via Attribute Test"
  call ESMF_Test((rc==ESMF_RC_ATTR_NOTSET).and.(outR4==42), &
                  name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !EX_UTest
  outI4 = 42
  call ESMF_AttributeGet(grid, name="dimCount", value=outI4, rc=rc)
  write(failMsg, *) "Did not return ESMF_RC_ATTR_NOTSET"
  write(name, *) "Getting 'dimCount' from a Grid via Attribute Test"
  call ESMF_Test((rc==ESMF_RC_ATTR_NOTSET).and.(outI4==42), &
                  name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "dimCount= ", outI4

  ! An attempt is made to retrieve a misspelled piece of Grid information.
  !EX_UTest
  outI4 = 42
  call ESMF_AttributeGet(grid, name="ESMF:dmiCount", value=outI4, rc=rc)
  write(failMsg, *) "Did not return ESMF_RC_ATTR_NOTSET"
  write(name, *) "Getting 'dimCount' from a Grid via Attribute Test"
  call ESMF_Test((rc==ESMF_RC_NOT_VALID).and.(outI4==42), &
                  name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "dimCount= ", outI4

  ! An attempt is made to retrieve a piece of internal Grid info masked by an
  ! Attribute, the Attribute is retrieved, not the internal Grid info.  This
  ! happens because the attribute name does not have 'ESMF:' prepended.
  !EX_UTest
  call ESMF_AttributeSet(grid, name="dimCount", value=4, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_TestEnd(result, ESMF_SRCLINE)
  outI4=42
  call ESMF_AttributeGet(grid, name="dimCount", value=outI4, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Attempted masking of Attribute internal Grid info Test"
  call ESMF_Test((rc==ESMF_SUCCESS).and.(outI4==4), &
                  name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "dimCount= ", outI4

  ! An attempt is made to retrieve a piece of internal Grid info masked by an
  ! Attribute.  This is different from the previous test because the user
  ! manually prepends 'ESMF:' to the Attribute name.  
  ! The AttributeSet should fail for this call..
  !EX_UTest
  call ESMF_AttributeSet(grid, name="ESMF:dimCount", value=4, rc=rc)
  write(failMsg, *) "Did not return ESMF_RC_NOT_VALID"
  write(name, *) "Attempted masking 2 of Attribute internal Grid info Test"
  call ESMF_Test((rc==ESMF_RC_NOT_VALID), name, failMsg, result, ESMF_SRCLINE)

  !EX_UTest
  outI4 = 42
  call ESMF_AttributeGet(grid, name="ESMF:tileCount", value=outI4, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'tileCount' from a Grid via Attribute Test"
  call ESMF_Test((rc==ESMF_SUCCESS).and.(outI4==1), &
                  name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "tileCount= ", outI4

  !EX_UTest
  outI4 = 42
  call ESMF_AttributeGet(grid, name="ESMF:staggerlocCount", value=outI4, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'staggerlocCount' from a Grid via Attribute Test"
  call ESMF_Test((rc==ESMF_SUCCESS).and.(outI4==4), &
                  name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "staggerlocCount= ", outI4

  !EX_UTest
  outI4 = 42
  call ESMF_AttributeGet(grid, name="ESMF:localDECount", value=outI4, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'localDECount' from a Grid via Attribute Test"
  call ESMF_Test((rc==ESMF_SUCCESS).and.(outI4==1), &
                  name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "localDECount= ", outI4

  ! THIS ARGUMENT IS NOT YET IMPLEMENTED IN ESMF_GRIDGET()
  !EX_UTest
  outI4 = 42
  call ESMF_AttributeGet(grid, name="ESMF:arbDim", value=outI4, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'arbDim' from a Grid via Attribute Test"
  call ESMF_Test((rc/=ESMF_SUCCESS), &
                  name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "arbDim= ", outI4

  !EX_UTest
  outI4 = 42
  call ESMF_AttributeGet(grid, name="ESMF:rank", value=outI4, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'rank' from a Grid via Attribute Test"
  call ESMF_Test((rc==ESMF_SUCCESS).and.(outI4==2), &
                  name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "rank = ", outI4

  !EX_UTest
  outI4 = 42
  call ESMF_AttributeGet(grid, name="ESMF:arbDimCount", value=outI4, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'arbDimCount' from a Grid via Attribute Test"
  call ESMF_Test((rc==ESMF_SUCCESS).and.(outI4==0), &
                  name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "arbDimCount= ", outI4

  !------------------------------------------------------------------------
  !------------------- Integer types ---(additional input info) -----------
  !------------------------------------------------------------------------

  !EX_UTest
  ! this should fail because arbIndexCount is not available for non-arbitrarily
  !  distributed grids
  inputList(:) = ''
  inputList(1) = 'localde:0'
  call ESMF_AttributeGet(grid, name="ESMF:arbIndexCount", &
                         value=outI4, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'arbIndexCount' from a Grid via Attribute Test"
  call ESMF_Test(rc/=ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "arbIndexCount=", outI4

  !------------------------------------------------------------------------
  !------------------- Character types ------------------------------------
  !------------------------------------------------------------------------

  !EX_UTest
  call ESMF_AttributeGet(grid, name="ESMF:name", value=outName, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'name' from a Grid via Attribute Test"
  call ESMF_Test((rc==ESMF_SUCCESS).and.(trim(outName)=='AttributeTestGrid'), &
                  name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "name=", trim(outName)

  !------------------------------------------------------------------------
  !------------------- Named constants ------------------------------------
  !------------------------------------------------------------------------

  !EX_UTest
  call ESMF_AttributeGet(grid, name="ESMF:coordTypeKind", value=outCoordTypeKind, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'coordTypeKind' from a Grid via Attribute Test"
  call ESMF_Test((rc==ESMF_SUCCESS).and.(trim(outCoordTypeKind)=='ESMF_TYPEKIND_I4'), &
                  name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "coordTypeKind=", trim(outCoordTypeKind)

  !EX_UTest
  call ESMF_AttributeGet(grid, name="ESMF:indexflag", value=outIndexFlag, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'indexflag' from a Grid via Attribute Test"
  call ESMF_Test((rc==ESMF_SUCCESS).and.(trim(outIndexFlag)=='ESMF_INDEX_DELOCAL'), &
                  name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "indexflag=", trim(outIndexFlag)

  !EX_UTest
  call ESMF_AttributeGet(grid, name="ESMF:status", value=outStatus, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'status' from a Grid via Attribute Test"
  call ESMF_Test((rc==ESMF_SUCCESS).and.(trim(outStatus)=='ESMF_GRIDSTATUS_COMPLETE'), &
                  name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "status=", trim(outStatus)


  !------------------------------------------------------------------------
  !------------------- Fortran list types ---------------------------------
  !------------------------------------------------------------------------

  !EX_UTest
  call ESMF_AttributeGet(grid, name="ESMF:distgridToGridMap", &
                         valueList=distgridToGridMap, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'distgridToGridMap' from a Grid via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "distgridToGridMap=", distgridToGridMap

  !EX_UTest
  call ESMF_AttributeGet(grid, name="ESMF:coordDimCount", &
                         valueList=coordDimCount, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'coordDimCount' from a Grid via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "coordDimCount=", coordDimCount

#if 0
  !!!!!!!!!!!!!! NO SUPPORT FOR 2D ARRAYS IN ATTRIBUTE YET !!!!!!!!!!!!!!!!!!!
  !EX_disable_UTest
  call ESMF_AttributeGet(grid, name="ESMF:coordDimMap", &
                         valueList=coordDimMap, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'coordDimMap' from a Grid via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "coordDimMap=", coordDimMap
#endif

  !EX_UTest
  call ESMF_AttributeGet(grid, name="ESMF:gridEdgeLWidth", &
                         valueList=gridEdgeLWidth, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'gridEdgeLWidth' from a Grid via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "gridEdgeLWidth=", gridEdgeLWidth

  !EX_UTest
  call ESMF_AttributeGet(grid, name="ESMF:gridEdgeUWidth", &
                         valueList=gridEdgeUWidth, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'gridEdgeUWidth' from a Grid via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "gridEdgeUWidth=", gridEdgeUWidth

  !EX_UTest
  call ESMF_AttributeGet(grid, name="ESMF:gridAlign", &
                         valueList=gridAlign, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'gridAlign' from a Grid via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "gridAlign=", gridAlign

  
  !------------------------------------------------------------------------
  !------------------- Fortran list types ---(additional input info) ------
  !------------------------------------------------------------------------

  !EX_UTest
  call ESMF_AttributeGet(grid, name="ESMF:coord", valueList=minIndex, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting unimplemented 'coord' from a Grid via Attribute Test"
  call ESMF_Test(rc==ESMF_RC_NOT_VAlID, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  ! Test that Fortran sublisting works
  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'junk:blah'
  inputList(2) = 'junk:blah'
  inputList(3) = 'tile:1'
  call ESMF_AttributeGet(grid, name="ESMF:minIndex", &
                         valueList=minIndex, inputList=inputList(3:3), rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'minIndex' from a Grid via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "minIndex=", minIndex

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'tile:1'
  call ESMF_AttributeGet(grid, name="ESMF:maxIndex", &
                         valueList=maxIndex, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'maxIndex' from a Grid via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "maxIndex=", maxIndex

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe:0'
  call ESMF_AttributeGet(grid, name="ESMF:exclusiveLBound", &
                         valueList=exclusiveLBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'exclusiveLBound' from a Grid via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "exclusiveLBound=", exclusiveLBound

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe:0'
  inputList(2) = 'coorddim:1'
  call ESMF_AttributeGet(grid, name="ESMF:exclusiveLBound", &
                         valueList=exclusiveLBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'exclusiveLBound' from a Grid with coords via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "exclusiveLBound=", exclusiveLBound

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe:0'
  inputList(2) = 'itemflag:ESMF_GRIDITEM_MASK'
  inputList(3) = 'staggerloc:ESMF_STAGGERLOC_CENTER'
  call ESMF_AttributeGet(grid, name="ESMF:exclusiveLBound", &
                         valueList=exclusiveLBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'exclusiveLBound' from a Grid with itemflag via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "exclusiveLBound=", exclusiveLBound

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe:0'
  call ESMF_AttributeGet(grid, name="ESMF:exclusiveUBound", &
                         valueList=exclusiveUBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'exclusiveUBound' from a Grid via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "exclusiveUBound=", exclusiveUBound

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe:0'
  inputList(1) = 'coorddim:1'
  call ESMF_AttributeGet(grid, name="ESMF:exclusiveUBound", &
                         valueList=exclusiveUBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'exclusiveUBound' from a Grid with coords via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "exclusiveUBound=", exclusiveUBound

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe:0'
  inputList(2) = 'itemflag:ESMF_GRIDITEM_MASK'
  inputList(3) = 'staggerloc:ESMF_STAGGERLOC_CENTER'
  call ESMF_AttributeGet(grid, name="ESMF:exclusiveUBound", &
                         valueList=exclusiveUBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'exclusiveUBound' from a Grid with itemflag via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "exclusiveUBound=", exclusiveUBound

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe:0'
  call ESMF_AttributeGet(grid, name="ESMF:exclusiveCount", &
                         valueList=exclusiveCount, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'exclusiveCount' from a Grid via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "exclusiveCount=", exclusiveCount

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe:0'
  inputList(2) = 'coorddim:1'
  call ESMF_AttributeGet(grid, name="ESMF:exclusiveCount", &
                         valueList=exclusiveCount, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'exclusiveCount' from a Grid with coords via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "exclusiveCount=", exclusiveCount

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe:0'
  inputList(2) = 'itemflag:ESMF_GRIDITEM_MASK'
  inputList(3) = 'staggerloc:ESMF_STAGGERLOC_CENTER'
  call ESMF_AttributeGet(grid, name="ESMF:exclusiveCount", &
                         valueList=exclusiveCount, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'exclusiveCount' from a Grid with itemflag via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "exclusiveCount=", exclusiveCount

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe:0'
  call ESMF_AttributeGet(grid, name="ESMF:computationalLBound", &
                         valueList=computationalLBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'computationalLBound' from a Grid via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "computationalLBound=", computationalLBound

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe:0'
  inputList(2) = 'coorddim:1'
  call ESMF_AttributeGet(grid, name="ESMF:computationalLBound", &
                         valueList=computationalLBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'computationalLBound' from a Grid with coords via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "computationalLBound=", computationalLBound

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe:0'
  inputList(2) = 'itemflag:ESMF_GRIDITEM_MASK'
  inputList(3) = 'staggerloc:ESMF_STAGGERLOC_CENTER'
  call ESMF_AttributeGet(grid, name="ESMF:computationalLBound", &
                         valueList=computationalLBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'computationalLBound' from a Grid with itemflag via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "computationalLBound=", computationalLBound

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe:0'
  call ESMF_AttributeGet(grid, name="ESMF:computationalUBound", &
                         valueList=computationalUBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'computationalUBound' from a Grid via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "computationalUBound=", computationalUBound

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe:0'
  inputList(1) = 'coorddim:1'
  call ESMF_AttributeGet(grid, name="ESMF:computationalUBound", &
                         valueList=computationalUBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'computationalUBound' from a Grid with coords via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "computationalUBound=", computationalUBound

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe:0'
  inputList(2) = 'itemflag:ESMF_GRIDITEM_MASK'
  inputList(3) = 'staggerloc:ESMF_STAGGERLOC_CENTER'
  call ESMF_AttributeGet(grid, name="ESMF:computationalUBound", &
                         valueList=computationalUBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'computationalUBound' from a Grid with itemflag via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "computationalUBound=", computationalUBound

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe:0'
  call ESMF_AttributeGet(grid, name="ESMF:computationalCount", &
                         valueList=computationalCount, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'computationalCount' from a Grid via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "computationalCount=", computationalCount

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe:0'
  inputList(2) = 'coorddim:1'
  call ESMF_AttributeGet(grid, name="ESMF:computationalCount", &
                         valueList=computationalCount, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'computationalCount' from a Grid with coords via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "computationalCount=", computationalCount

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe:0'
  inputList(2) = 'itemflag:ESMF_GRIDITEM_MASK'
  inputList(3) = 'staggerloc:ESMF_STAGGERLOC_CENTER'
  call ESMF_AttributeGet(grid, name="ESMF:computationalCount", &
                         valueList=computationalCount, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'computationalCount' from a Grid with itemflag via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "computationalCount=", computationalCount

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe:0'
  inputList(2) = 'coorddim:1'
  call ESMF_AttributeGet(grid, name="ESMF:totalLBound", &
                         valueList=totalLBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'totalLBound' from a Grid with coords via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "totalLBound=", totalLBound

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe:0'
  inputList(2) = 'itemflag:ESMF_GRIDITEM_MASK'
  inputList(3) = 'staggerloc:ESMF_STAGGERLOC_CENTER'
  call ESMF_AttributeGet(grid, name="ESMF:totalLBound", &
                         valueList=totalLBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'totalLBound' from a Grid with itemflag via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE) 
  !------------------------------------------------------------------------
print *, "totalLBound=", totalLBound
  
  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe:0'
  inputList(1) = 'coorddim:1'
  call ESMF_AttributeGet(grid, name="ESMF:totalUBound", &
                         valueList=totalUBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'totalUBound' from a Grid with coords via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "totalUBound=", totalUBound

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe:0'
  inputList(2) = 'itemflag:ESMF_GRIDITEM_MASK'
  inputList(3) = 'staggerloc:ESMF_STAGGERLOC_CENTER'
  call ESMF_AttributeGet(grid, name="ESMF:totalUBound", &
                         valueList=totalUBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'totalUBound' from a Grid with itemflag via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------ 
print *, "totalUBound=", totalUBound
  
  
  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe:0'
  inputList(2) = 'coorddim:1'                                              
  call ESMF_AttributeGet(grid, name="ESMF:totalCount", &
                         valueList=totalCount, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'totalCount' from a Grid with coords via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "totalCount=", totalCount

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe:0'
  inputList(2) = 'itemflag:ESMF_GRIDITEM_MASK'
  inputList(3) = 'staggerloc:ESMF_STAGGERLOC_CENTER'
  call ESMF_AttributeGet(grid, name="ESMF:totalCount", &
                         valueList=totalCount, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'totalCount' from a Grid with itemflag via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "totalCount=", totalCount

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe:0'
  call ESMF_AttributeGet(grid, name="ESMF:isLBound", &
                         valueList=isLBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'isLBound' from a Grid with coords via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "isLBound=", isLBound

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe:0'
  call ESMF_AttributeGet(grid, name="ESMF:isUBound", &
                         valueList=isUBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'isUBound' from a Grid with itemflag via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "isUBound=", isUBound

#endif

  !------------------------------------------------------------------------
  ! clean up
  call ESMF_GridDestroy(grid, rc=rc)
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  
  if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
end program ESMF_AttributeGridGetUTest
