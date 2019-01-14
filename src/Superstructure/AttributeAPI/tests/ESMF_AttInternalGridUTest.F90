! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
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
  '$Id$'
!------------------------------------------------------------------------------

!-------------------------------------------------------------------------
!=========================================================================

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name
  
  ! local variables
  type(ESMF_Grid)        :: grid, gridR4
  integer                :: rc
  
  ! grid coordinates
  integer                :: localDECount, lDE
  type(ESMF_StaggerLoc)  :: staggerloc
  real(ESMF_KIND_R8), pointer :: farrayPtrX(:,:),farrayPtrY(:,:)
  real(ESMF_KIND_R4), pointer :: farrayPtrXR4(:,:),farrayPtrYR4(:,:)
  integer                :: i1, i2, clbnd(2), cubnd(2)

  character(*), parameter :: apConv = 'Attribute_IO'
  character(*), parameter :: apPurp = 'attributes'

  ! cumulative result: count failures; no failures equals "all pass"
  integer                :: result = 0

  integer                :: nx, ny
  real(ESMF_KIND_R8)     :: dx, dy
  real(ESMF_KIND_R8)     :: xcoords(100), ycoords(100)

  character(ESMF_MAXSTR),dimension(3) :: inputList 

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
  real(ESMF_KIND_R4)     :: xcoordsR4(100), ycoordsR4(100)
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

  !------------------------------------------------------------------------
  ! R8 Grid

  nx = 10
  ny = 10
  dx = 360./nx
  dy = 180./ny

  ! Create Grid with coordinates, Attributes should work on an empty Grid as well
  grid=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/nx,ny/), &
                                indexflag=ESMF_INDEX_GLOBAL,         &
                                name="AttributeTestGrid", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Get number of local DEs
  call ESMF_GridGet(grid, localDECount=localDECount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Allocate Center (e.g. Center) stagger
  call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Loop through DEs and set Centers as the average of the corners
  do lDE=0,localDECount-1  

    ! get and fill first coord array
    call ESMF_GridGetCoord(grid, localDE=lDE,  staggerloc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                           computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrX, &
                           rc=rc)           
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! get and fill second coord array
    call ESMF_GridGetCoord(grid, localDE=lDE, staggerloc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                           farrayPtr=farrayPtrY, rc=rc)           
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    do i1=clbnd(1),cubnd(1)
    do i2=clbnd(2),cubnd(2)
      farrayPtrX(i1,i2)=REAL(i1-1)*dx
      farrayPtrY(i1,i2)=-90. + (REAL(i2-1)*dy + 0.5*dy)
    enddo
    enddo

  enddo

#ifdef ESMF_TESTEXHAUSTIVE

  !------------------------------------------------------------------------
  ! R4 Grid

  nx = 10
  ny = 10
  dx = 360./nx
  dy = 180./ny

  ! Create Grid with coordinates, Attributes should work on an empty Grid as well
  gridR4=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/nx,ny/), &
                                coordTypeKind=ESMF_TYPEKIND_R4, &
                                indexflag=ESMF_INDEX_GLOBAL,         &
                                name="AttributeTestGridR4", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Get number of local DEs
  call ESMF_GridGet(gridR4, localDECount=localDECount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Allocate Center (e.g. Center) stagger
  call ESMF_GridAddCoord(gridR4, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Loop through DEs and set Centers as the average of the corners
  do lDE=0,localDECount-1  

    ! get and fill first coord array
    call ESMF_GridGetCoord(gridR4, localDE=lDE,  staggerloc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                           computationalLBound=clbnd, computationalUBound=cubnd, &
                           farrayPtr=farrayPtrXR4, rc=rc)           
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! get and fill second coord array
    call ESMF_GridGetCoord(gridR4, localDE=lDE, staggerloc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                           farrayPtr=farrayPtrYR4, rc=rc)           
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    do i1=clbnd(1),cubnd(1)
    do i2=clbnd(2),cubnd(2)
      farrayPtrXR4(i1,i2)=REAL ((i1-1)*dx)
      farrayPtrYR4(i1,i2)=REAL (-90. + (i2-1)*dy + 0.5*dy)
    enddo
    enddo

  enddo


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
  ! This routine should return in error, the output value is undefined
  outR4 = 42
  call ESMF_AttributeGet(grid, name="ESMF:dimCount", value=outR4, rc=rc)
  write(failMsg, *) "Did not return ESMF_RC_ATTR_NOTSET"
  write(name, *) "Incorrectly getting internal info from a Grid via Attribute Test"
  call ESMF_Test((rc==ESMF_RC_ATTR_NOTSET), &
                  name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "rc = ", rc
print *, "outR4 = ", outR4

  !EX_UTest
  ! This routine should return in error, the output value is undefined
  outI4 = 42
  call ESMF_AttributeGet(grid, name="dimCount", value=outI4, rc=rc)
  write(failMsg, *) "Did not return ESMF_RC_ATTR_NOTSET"
  write(name, *) "Getting 'dimCount' from a Grid via Attribute Test"
  call ESMF_Test((rc==ESMF_RC_ATTR_NOTSET), &
                  name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "rc = ", rc
print *, "outI4 = ", outI4

  !EX_UTest
  ! An attempt is made to retrieve a misspelled piece of Grid information.
  ! This routine should return in error, the output value is undefined
  outI4 = 42
  call ESMF_AttributeGet(grid, name="ESMF:dmiCount", value=outI4, rc=rc)
  write(failMsg, *) "Did not return ESMF_RC_ATTR_NOTSET"
  write(name, *) "Getting 'dimCount' from a Grid via Attribute Test"
  call ESMF_Test((rc==ESMF_RC_NOT_VALID), &
                  name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "rc = ", rc
print *, "outI4 = ", outI4

  !EX_UTest
  ! An attempt is made to retrieve a piece of internal Grid info masked by an
  ! Attribute, the Attribute is retrieved, not the internal Grid info.  This
  ! happens because the attribute name does not have 'ESMF:' prepended.
  call ESMF_AttributeSet(grid, name="dimCount", value=4, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_TestEnd(ESMF_SRCLINE)
  outI4=42
  call ESMF_AttributeGet(grid, name="dimCount", value=outI4, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Attempted masking of Attribute internal Grid info Test"
  call ESMF_Test((rc==ESMF_SUCCESS).and.(outI4==4), &
                  name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "dimCount= ", outI4

  !EX_UTest
  ! An attempt is made to retrieve a piece of internal Grid info masked by an
  ! Attribute.  This is different from the previous test because the user
  ! manually prepends 'ESMF:' to the Attribute name.  
  ! The AttributeSet should fail for this call..
  call ESMF_AttributeSet(grid, name="ESMF:dimCount", value=4, rc=rc)
  write(failMsg, *) "Did not return ESMF_RC_NOT_VALID"
  write(name, *) "Attempted masking 2 of Attribute internal Grid info Test"
  call ESMF_Test((rc==ESMF_RC_NOT_VALID), name, failMsg, result, ESMF_SRCLINE)
print *, "!!!!!!!!!!!!!!!!!!!!!!!!rc = ", rc


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
  inputList(1) = 'localde=0'
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
  outCoordTypeKind = ""
  call ESMF_AttributeGet(grid, name="ESMF:coordTypeKind", value=outCoordTypeKind, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'coordTypeKind' from a Grid via Attribute Test"
  call ESMF_Test((rc==ESMF_SUCCESS).and.(trim(outCoordTypeKind)=='ESMF_TYPEKIND_R8'), &
                  name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "coordTypeKind=", trim(outCoordTypeKind)

  !EX_UTest
  call ESMF_AttributeGet(grid, name="ESMF:indexflag", value=outIndexFlag, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'indexflag' from a Grid via Attribute Test"
  call ESMF_Test((rc==ESMF_SUCCESS).and.(trim(outIndexFlag)=='ESMF_INDEX_GLOBAL'), &
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
  inputList(1) = 'junk=blah'
  inputList(2) = 'junk=blah'
  inputList(3) = 'tile=1'
  call ESMF_AttributeGet(grid, name="ESMF:minIndex", &
                         valueList=minIndex, inputList=inputList(3:3), rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'minIndex' from a Grid via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "minIndex=", minIndex

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'tile=1'
  call ESMF_AttributeGet(grid, name="ESMF:maxIndex", &
                         valueList=maxIndex, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'maxIndex' from a Grid via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "maxIndex=", maxIndex

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe=0'
  call ESMF_AttributeGet(grid, name="ESMF:exclusiveLBound", &
                         valueList=exclusiveLBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'exclusiveLBound' from a Grid via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "exclusiveLBound=", exclusiveLBound

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe=0'
  inputList(2) = 'coordDim=1'
  call ESMF_AttributeGet(grid, name="ESMF:exclusiveLBound", &
                         valueList=exclusiveLBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'exclusiveLBound' from a Grid with coords via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "exclusiveLBound=", exclusiveLBound

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe=0'
  inputList(2) = 'itemflag=ESMF_GRIDITEM_MASK'
  inputList(3) = 'staggerloc=ESMF_STAGGERLOC_CENTER'
  call ESMF_AttributeGet(grid, name="ESMF:exclusiveLBound", &
                         valueList=exclusiveLBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'exclusiveLBound' from a Grid with itemflag via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "exclusiveLBound=", exclusiveLBound

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe=0'
  call ESMF_AttributeGet(grid, name="ESMF:exclusiveUBound", &
                         valueList=exclusiveUBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'exclusiveUBound' from a Grid via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "exclusiveUBound=", exclusiveUBound

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe=0'
  inputList(1) = 'coordDim=1'
  call ESMF_AttributeGet(grid, name="ESMF:exclusiveUBound", &
                         valueList=exclusiveUBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'exclusiveUBound' from a Grid with coords via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "exclusiveUBound=", exclusiveUBound

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe=0'
  inputList(2) = 'itemflag=ESMF_GRIDITEM_MASK'
  inputList(3) = 'staggerloc=ESMF_STAGGERLOC_CENTER'
  call ESMF_AttributeGet(grid, name="ESMF:exclusiveUBound", &
                         valueList=exclusiveUBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'exclusiveUBound' from a Grid with itemflag via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "exclusiveUBound=", exclusiveUBound

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe=0'
  call ESMF_AttributeGet(grid, name="ESMF:exclusiveCount", &
                         valueList=exclusiveCount, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'exclusiveCount' from a Grid via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "exclusiveCount=", exclusiveCount

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe=0'
  inputList(2) = 'coordDim=1'
  call ESMF_AttributeGet(grid, name="ESMF:exclusiveCount", &
                         valueList=exclusiveCount, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'exclusiveCount' from a Grid with coords via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "exclusiveCount=", exclusiveCount

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe=0'
  inputList(2) = 'itemflag=ESMF_GRIDITEM_MASK'
  inputList(3) = 'staggerloc=ESMF_STAGGERLOC_CENTER'
  call ESMF_AttributeGet(grid, name="ESMF:exclusiveCount", &
                         valueList=exclusiveCount, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'exclusiveCount' from a Grid with itemflag via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "exclusiveCount=", exclusiveCount

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe=0'
  call ESMF_AttributeGet(grid, name="ESMF:computationalLBound", &
                         valueList=computationalLBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'computationalLBound' from a Grid via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "computationalLBound=", computationalLBound

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe=0'
  inputList(2) = 'coordDim=1'
  call ESMF_AttributeGet(grid, name="ESMF:computationalLBound", &
                         valueList=computationalLBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'computationalLBound' from a Grid with coords via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "computationalLBound=", computationalLBound

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe=0'
  inputList(2) = 'itemflag=ESMF_GRIDITEM_MASK'
  inputList(3) = 'staggerloc=ESMF_STAGGERLOC_CENTER'
  call ESMF_AttributeGet(grid, name="ESMF:computationalLBound", &
                         valueList=computationalLBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'computationalLBound' from a Grid with itemflag via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "computationalLBound=", computationalLBound

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe=0'
  call ESMF_AttributeGet(grid, name="ESMF:computationalUBound", &
                         valueList=computationalUBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'computationalUBound' from a Grid via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "computationalUBound=", computationalUBound

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe=0'
  inputList(1) = 'coordDim=1'
  call ESMF_AttributeGet(grid, name="ESMF:computationalUBound", &
                         valueList=computationalUBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'computationalUBound' from a Grid with coords via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "computationalUBound=", computationalUBound

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe=0'
  inputList(2) = 'itemflag=ESMF_GRIDITEM_MASK'
  inputList(3) = 'staggerloc=ESMF_STAGGERLOC_CENTER'
  call ESMF_AttributeGet(grid, name="ESMF:computationalUBound", &
                         valueList=computationalUBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'computationalUBound' from a Grid with itemflag via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "computationalUBound=", computationalUBound

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe=0'
  call ESMF_AttributeGet(grid, name="ESMF:computationalCount", &
                         valueList=computationalCount, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'computationalCount' from a Grid via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "computationalCount=", computationalCount

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe=0'
  inputList(2) = 'coordDim=1'
  call ESMF_AttributeGet(grid, name="ESMF:computationalCount", &
                         valueList=computationalCount, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'computationalCount' from a Grid with coords via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "computationalCount=", computationalCount

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe=0'
  inputList(2) = 'itemflag=ESMF_GRIDITEM_MASK'
  inputList(3) = 'staggerloc=ESMF_STAGGERLOC_CENTER'
  call ESMF_AttributeGet(grid, name="ESMF:computationalCount", &
                         valueList=computationalCount, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'computationalCount' from a Grid with itemflag via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "computationalCount=", computationalCount

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe=0'
  inputList(2) = 'coordDim=1'
  call ESMF_AttributeGet(grid, name="ESMF:totalLBound", &
                         valueList=totalLBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'totalLBound' from a Grid with coords via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "totalLBound=", totalLBound

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe=0'
  inputList(2) = 'itemflag=ESMF_GRIDITEM_MASK'
  inputList(3) = 'staggerloc=ESMF_STAGGERLOC_CENTER'
  call ESMF_AttributeGet(grid, name="ESMF:totalLBound", &
                         valueList=totalLBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'totalLBound' from a Grid with itemflag via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE) 
  !------------------------------------------------------------------------
print *, "totalLBound=", totalLBound
  
  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe=0'
  inputList(1) = 'coordDim=1'
  call ESMF_AttributeGet(grid, name="ESMF:totalUBound", &
                         valueList=totalUBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'totalUBound' from a Grid with coords via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "totalUBound=", totalUBound

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe=0'
  inputList(2) = 'itemflag=ESMF_GRIDITEM_MASK'
  inputList(3) = 'staggerloc=ESMF_STAGGERLOC_CENTER'
  call ESMF_AttributeGet(grid, name="ESMF:totalUBound", &
                         valueList=totalUBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'totalUBound' from a Grid with itemflag via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------ 
print *, "totalUBound=", totalUBound
  
  
  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe=0'
  inputList(2) = 'coordDim=1'                                              
  call ESMF_AttributeGet(grid, name="ESMF:totalCount", &
                         valueList=totalCount, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'totalCount' from a Grid with coords via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "totalCount=", totalCount

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe=0'
  inputList(2) = 'itemflag=ESMF_GRIDITEM_MASK'
  inputList(3) = 'staggerloc=ESMF_STAGGERLOC_CENTER'
  call ESMF_AttributeGet(grid, name="ESMF:totalCount", &
                         valueList=totalCount, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'totalCount' from a Grid with itemflag via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "totalCount=", totalCount

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe=0'
  call ESMF_AttributeGet(grid, name="ESMF:isLBound", &
                         valueList=isLBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'isLBound' from a Grid with coords via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "isLBound=", isLBound

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'localDe=0'
  call ESMF_AttributeGet(grid, name="ESMF:isUBound", &
                         valueList=isUBound, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'isUBound' from a Grid with itemflag via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "isUBound=", isUBound

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'coordDim=1'
  call ESMF_AttributeGet(gridR4, name="ESMF:farrayPtr", &
                         valueList=xcoordsR4, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'farrayPtr' (x coordinates) from a R4 Grid via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "xcoords=", xcoordsR4

  !EX_UTest
  inputList(:) = ''
  inputList(1) = 'coordDim=2'
  call ESMF_AttributeGet(gridR4, name="ESMF:farrayPtr", &
                         valueList=ycoordsR4, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'farrayPtr' (y coordinates) from a R4 Grid via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "ycoords=", ycoordsR4

  !------------------------------------------------------------------------
  ! clean up
  call ESMF_GridDestroy(gridR4, rc=rc)
#endif

  !NEX_UTest
  inputList(:) = ''
  inputList(1) = 'coordDim=1'
  call ESMF_AttributeGet(grid, name="ESMF:farrayPtr", &
                         valueList=xcoords, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'farrayPtr' (x coordinates) from a Grid via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "xcoords=", xcoords

  !NEX_UTest
  inputList(:) = ''
  inputList(1) = 'coordDim=2'
  call ESMF_AttributeGet(grid, name="ESMF:farrayPtr", &
                         valueList=ycoords, inputList=inputList, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
  write(name, *) "Getting 'farrayPtr' (y coordinates) from a Grid via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
print *, "ycoords=", ycoords


  !------------------------------------------------------------------------
  !---------- Attribute Add (User additions to ESMF: namespace) -----------
  !------------------------------------------------------------------------

  !NEX_UTest
  call ESMF_AttributeAdd (grid,  &
      convention=apConv, purpose=apPurp,  &
      attrList=(/ ESMF_ATT_GRIDDED_DIM_LABELS /),  &
      rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Adding gridded dimension labels via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !NEX_UTest
  call ESMF_AttributeSet(grid,  &
      name=ESMF_ATT_GRIDDED_DIM_LABELS,  &
      valueList=(/ "x_axis", "y_axis" /), &
      convention=apConv, purpose=apPurp,  &
      rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Adding gridded dimension label values via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !NEX_UTest
  ! Note: Normally ESMF_ATT_UNGRIDDED_DIM_LABELS would only be used at the
  ! Field level.  It is included here simply for unit testing.
  call ESMF_AttributeAdd (grid,  &
      convention=apConv, purpose=apPurp,  &
      attrList=(/ ESMF_ATT_UNGRIDDED_DIM_LABELS /),  &
      rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Adding ungridded dimension labels via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !NEX_UTest
  call ESMF_AttributeSet(grid,  &
      name=ESMF_ATT_UNGRIDDED_DIM_LABELS,  &
      valueList=(/ "x_axis_ug", "y_axis_ug" /), &
      convention=apConv, purpose=apPurp,  &
      rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Adding ungridded dimension label values via Attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !NEX_UTest
  call ESMF_AttributeAdd (grid,  &
      convention=apConv, purpose=apPurp,  &
      attrList=(/ "ESMF:Something_unknown" /),  &
      rc=rc)
  write(failMsg, *) "Did not return failure rc"
  write(name, *) "Adding unknown ESMF: attribute Test"
  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !NEX_UTest
  call ESMF_AttributeSet(grid,  &
      name="ESMF:Something_unknown",  &
      valueList=(/ "some value" /), &
      convention=apConv, purpose=apPurp,  &
      rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Adding values to unknown ESMF: Attribute Test"
  call ESMF_Test(rc/=ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  ! clean up
  call ESMF_GridDestroy(grid, rc=rc)
  
  if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
end program ESMF_AttributeGridGetUTest
