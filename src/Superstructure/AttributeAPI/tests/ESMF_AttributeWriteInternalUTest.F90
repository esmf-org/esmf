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
program ESMF_AttributeWriteIntrnalUTest

!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_AttributeWriteIntrnalUTest - Attribute write internal info
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
  type(ESMF_AttPack)   :: attpack   
  type(ESMF_Grid)        :: grid
  integer                :: rc
  
  ! grid coordinates
  integer                :: localDECount, lDE
  type(ESMF_StaggerLoc)  :: staggerloc
  real(ESMF_KIND_R8), pointer :: farrayPtrX(:,:),farrayPtrY(:,:)
  integer                :: i1, i2, clbnd(2), cubnd(2)

  ! cumulative result: count failures; no failures equals "all pass"
  integer                :: result = 0

  integer                :: nx, ny
  real(ESMF_KIND_R8)     :: dx, dy

#ifdef ESMF_TESTEXHAUSTIVE

  type(ESMF_GridComp)     :: gridcomp
  type(ESMF_Field)        :: field
  type(ESMF_FieldBundle)  :: fieldBundle
  type(ESMF_State)        :: importState
  character(ESMF_MAXSTR)  :: conv, purp
   
  character(ESMF_MAXSTR),dimension(11)   :: attrList         
  character(ESMF_MAXSTR),dimension(3)   :: inputList 
  character(ESMF_MAXSTR),dimension(4)   :: outValList

  integer(ESMF_KIND_I4), dimension(2) :: exclusiveCount
  real(ESMF_KIND_R8), pointer :: farrayPtr(:)
  integer :: coorddim = 1

  logical :: rc_logical
  character(ESMF_MAXSTR), dimension(4) :: exclusions

#if 0
  ! for code to prove that internal info can be retrieved from a Grid
  integer(ESMF_KIND_I4)  :: outI4
  character(ESMF_MAXSTR) :: outCoordTypeKind, outName
  integer(ESMF_KIND_I4)  :: distgridToGridMap(2)
  integer(ESMF_KIND_I4)  :: minIndex(2)
  character(ESMF_MAXSTR),dimension(3) :: inputList 
#endif

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

  nx = 10
  ny = 10
  dx = 360./nx
  dy = 180./ny

  ! Create Grid with coordinates, Attributes should work on an empty Grid as well
  grid=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/10,10/), &
                                indexflag=ESMF_INDEX_GLOBAL,         &
                                rc=rc)
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

  !-------------------------------------------------------------------------
  !   <CFDocument> attribute representation and output test for
  !   <modelComponent>, <simulationRun>, including <input>s (fields) and
  !   <platform>. Uses built-in, standard CF packages.
  !-------------------------------------------------------------------------


    !-------------------------------------------------------------------------
    !EX_UTest
    ! Construct a gridded component ESMF object that will be decorated with
    ! Attributes to output <CFDocument>s
    gridcomp = ESMF_GridCompCreate(name="gridcomp_cim151grids", petList=(/0/), &
                 rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a gridded component to decorate with Attributes test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


    !-------------------------------------------------------------------------
    ! <input>
    !-------------------------------------------------------------------------
    !EX_UTest
    ! Construct a field ESMF object that will be decorated with
    ! Attributes to output within <input>s in a <simulationRun>
    field = ESMF_FieldEmptyCreate(name="DMS_emi", rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a field to decorate with Attributes test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Construct a fieldbundle ESMF object that will contain fields
    fieldBundle = ESMF_FieldBundleCreate(name="Field Bundle", rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a fieldbundle to contain fields test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Add the field to the field bundle (links attributes also)
    call ESMF_FieldBundleAdd(fieldBundle, (/ field /), rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Adding a field to a fieldbundle test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Construct an import state ESMF object that will contain a fieldbundle
    importState = ESMF_StateCreate(name="importState",  &
                                   stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating an import state test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Add a fieldbundle to the import state (links attributes also)
    call ESMF_StateAdd(importState, fieldbundleList=(/fieldBundle/), rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Adding a field bundle to an import state test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Link import state attributes to the gridded component
    call ESMF_AttributeLink(gridcomp, importState, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Linking import state attributes to gridded component"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


  !-------------------------------------------------------------------------
  !-------------------------------------------------------------------------
  ! Add Attribute package to the Grid
  !-------------------------------------------------------------------------
  !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Create standard CF attribute package on the grid
    call ESMF_AttributeAdd(grid, &
                           convention='CIM 1.5.1', &
                           purpose='grids', &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating custom Grid CF attribute package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#if 0
    !-------------------------------------------------------------------------
    !EX_disable_UTest
    ! Create package nested inside the CF attribute package on the grid
    attrList(1) = 'nestPack1'
    attrList(2) = 'nestPack2'
    attrList(3) = 'nestPack3'
    call ESMF_AttributeAdd(grid, &
                           convention='CF-nested', &
                           purpose='grids-nested', &
                           attrList=attrList(1:3), &
                           count = 3, &
                           nestConvention='CF', &
                           nestPurpose='grids', &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a package nested inside the CF package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_disable_UTest
    ! Set an attribute value within the CF Grid package
    call ESMF_AttributeSet(grid, 'nestPack1', &
                           'nestPack1', &
                           convention='CF-nested', &
                           purpose='grids-nested', &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set an attribute value in CF Grid package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_disable_UTest
    ! Set an attribute value within the CF Grid package
    call ESMF_AttributeSet(grid, 'nestPack2', &
                           'nestPack2', &
                           convention='CF-nested', &
                           purpose='grids-nested', &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set an attribute value in CF Grid package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_disable_UTest
    ! Set an attribute value within the CF Grid package
    call ESMF_AttributeSet(grid, 'nestPack3', &
                           'nestPack3', &
                           convention='CF-nested', &
                           purpose='grids-nested', &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set an attribute value in CF Grid package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#endif

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Add a Grid to the Field, this should link the Attribute hierarchies
    call ESMF_FieldEmptySet(field, grid, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Add a Grid to the Field"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !------------------------------------------------------------------------
  !------------------------------------------------------------------------


    !-------------------------------------------------------------------------
    !EX_UTest
    ! Write out the attribute tree as a CF-formatted XML file
    call ESMF_AttributeWrite(gridcomp, 'CIM 1.5.1', &
                                 'grids', &
      attwriteflag=ESMF_ATTWRITE_XML,rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Write out CF XML file test"
    call ESMF_Test((rc==ESMF_SUCCESS .or. rc==ESMF_RC_LIB_NOT_PRESENT), &
                    name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! compare the output file to the baseline file
    exclusions(1) = "ESMF Version"
    exclusions(2) = "documentID"
    exclusions(3) = "documentVersion"
    exclusions(4) = "documentCreationDate"
    rc_logical = ESMF_TestFileCompare('gridcomp_cim151grids.xml', &
      'baseline_gridcomp_cim151grids.xml', exclusionList=exclusions)
    write(failMsg, *) "Did not return True"
    write(name, *) "Compare the XML output file to the baseline file"
    call ESMF_Test(rc_logical.eqv..true., name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  ! clean up
  call ESMF_FieldDestroy(field, rc=rc)
  if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_FieldBundleDestroy(fieldbundle, rc=rc)
  if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_StateDestroy(importState, rc=rc)
  if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_GridCompDestroy(gridcomp, rc=rc)
  if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

#endif

  !------------------------------------------------------------------------
  ! clean up
  call ESMF_GridDestroy(grid, rc=rc)
  
  if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
end program ESMF_AttributeWriteIntrnalUTest
