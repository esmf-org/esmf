! $Id$
!
! Earth System Modeling Framework
! Copyright (c) 2002-2024, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_GridCompCreateUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_GridCompCreateUTest - Unit test for Components.
!
! !DESCRIPTION:
! Tests, cursory and exahustive, for Component Create code.
!
!-------------------------------------------------------------------------
!
! !USES:
    use ESMF_TestMod     ! test methods
    use ESMF
    implicit none
    
!   ! Local variables
    integer :: rc
    character(ESMF_MAXSTR) :: cname
    type(ESMF_GridComp) :: gcomp, gridcompAlias
    logical:: gridcompBool
    logical:: isCreated

    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name
    integer :: result = 0

    ! Internal State Variables
    type testData
    sequence
        integer :: testNumber
    end type

    type dataWrapper
    sequence
        type(testData), pointer :: p
    end type

#ifdef ESMF_TESTEXHAUSTIVE
    character(ESMF_MAXSTR)        :: bname
    type(dataWrapper)             :: wrap1, wrap2, wrap3, wrap4, wrap5, wrap6
    type(dataWrapper)             :: wrap7, wrap8, wrap9, wrap10
    type(ESMF_Grid)               :: grid, gridInA, gridInB
    type(ESMF_Grid), allocatable  :: gridList(:)
    type(ESMF_Mesh)               :: mesh, meshInA, meshInB
    type(ESMF_Mesh), allocatable  :: meshList(:)
    logical                       :: isPresent
    type(ESMF_Config)             :: config
    type(ESMF_HConfig)            :: hconfig
    integer                       :: fred, i
    character(len=:), allocatable :: labelList(:)
    integer, allocatable          :: petList(:)
    character(160)                :: msgStr
    type(ESMF_GridComp)           :: gcomp2
#endif

!-------------------------------------------------------------------------------
!   The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!   always run. When the environment variable, EXHAUSTIVE, is set to ON then
!   the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!   to OFF, then only the sanity unit tests.
!   Special strings (Non-exhaustive and exhaustive) have been
!   added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------
        
    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    !-----------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Testing GridComp IsCreated w/o keyword for uncreated object"
    write(failMsg, *) "Did not return .false."
    isCreated = ESMF_GridCompIsCreated(gcomp)
    call ESMF_Test((isCreated .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)

    !-----------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Testing GridComp IsCreated for uncreated object"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    isCreated = ESMF_GridCompIsCreated(gcomp, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-----------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Testing GridComp IsCreated value for uncreated object"
    write(failMsg, *) "Did not return .false."
    call ESMF_Test((isCreated .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest
    cname = "Atmosphere"
    gcomp = ESMF_GridCompCreate(name=cname, configFile="comp.yaml", rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Component Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-----------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Testing GridComp IsCreated w/o keyword for created object"
    write(failMsg, *) "Did not return .true."
    isCreated = ESMF_GridCompIsCreated(gcomp)
    call ESMF_Test((isCreated .eqv. .true.), name, failMsg, result, ESMF_SRCLINE)

    !-----------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Testing GridComp IsCreated for created object"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    isCreated = ESMF_GridCompIsCreated(gcomp, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-----------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Testing GridComp IsCreated value for created object"
    write(failMsg, *) "Did not return .true."
    call ESMF_Test((isCreated .eqv. .true.), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "GridComp equality before assignment Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    gridcompBool = (gridcompAlias.eq.gcomp)
    call ESMF_Test(.not.gridcompBool, name, failMsg, result, ESMF_SRCLINE)
    
    !------------------------------------------------------------------------
    !NEX_UTest
    ! Testing ESMF_GridCompAssignment(=)()
    write(name, *) "GridComp assignment and equality Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    gridcompAlias = gcomp
    gridcompBool = (gridcompAlias.eq.gcomp)
    call ESMF_Test(gridcompBool, name, failMsg, result, ESMF_SRCLINE)
    
    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "GridCompDestroy Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_GridCompDestroy(gcomp, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-----------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Testing GridComp IsCreated w/o keyword for destroyed object"
    write(failMsg, *) "Did not return .false."
    isCreated = ESMF_GridCompIsCreated(gcomp)
    call ESMF_Test((isCreated .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)

    !-----------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Testing GridComp IsCreated for destroyed object"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    isCreated = ESMF_GridCompIsCreated(gcomp, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    
    !-----------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Testing GridComp IsCreated value for destroyed object"
    write(failMsg, *) "Did not return .false."
    call ESMF_Test((isCreated .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)
    
    !------------------------------------------------------------------------
    !NEX_UTest
    ! Testing ESMF_GridCompOperator(==)()
    write(name, *) "GridComp equality after destroy Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    gridcompBool = (gridcompAlias==gcomp)
    call ESMF_Test(.not.gridcompBool, name, failMsg, result, ESMF_SRCLINE)
    
    !------------------------------------------------------------------------
    !NEX_UTest
    ! Testing ESMF_GridCompOperator(/=)()
    write(name, *) "GridComp non-equality after destroy Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    gridcompBool = (gridcompAlias/=gcomp)
    call ESMF_Test(gridcompBool, name, failMsg, result, ESMF_SRCLINE)
    
    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Double GridCompDestroy through alias Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_GridCompDestroy(gridcompAlias, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest
    cname = "Atmosphere"
    gcomp = ESMF_GridCompCreate(name=cname, configFile="comp.yaml", rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Component Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "GridCompDestroy Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_GridCompDestroy(gcomp, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_TESTEXHAUSTIVE

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test get a Component name from a destroyed component

    call ESMF_GridCompGet(gcomp, name=bname, rc=rc)

    write(failMsg, *) "Did return ESMF_SUCCESS"
    write(name, *) "Getting a Component name Test"
    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test creation of a Component
    gcomp = ESMF_GridCompCreate(rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Component Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!--- Grid handling

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Query gridIsPresent

    call ESMF_GridCompGet(gcomp, gridIsPresent=isPresent, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Query gridIsPresent for Grid that was not set Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Verify gridIsPresent

    write(failMsg, *) "Did not verify"
    write(name, *) "Verify gridIsPresent for Grid that was not set Test"
    call ESMF_Test((.not.isPresent), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test get a Grid that was not set

    call ESMF_GridCompGet(gcomp, grid=grid, rc=rc)

    write(failMsg, *) "Did return ESMF_SUCCESS"
    write(name, *) "Getting a Grid that was not set Test"
    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------

    gridInA = ESMF_GridEmptyCreate(rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Set a Grid

    call ESMF_GridCompSet(gcomp, grid=gridInA, rc=rc)

    write(failMsg, *) "Did return ESMF_SUCCESS"
    write(name, *) "Setting a Grid that was not set Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Query gridIsPresent

    call ESMF_GridCompGet(gcomp, gridIsPresent=isPresent, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Query gridIsPresent for Grid that was set Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Verify gridIsPresent

    write(failMsg, *) "Did not verify"
    write(name, *) "Verify gridIsPresent for Grid that was set Test"
    call ESMF_Test((isPresent), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test get a Grid that was set

    call ESMF_GridCompGet(gcomp, grid=grid, rc=rc)

    write(failMsg, *) "Did return ESMF_SUCCESS"
    write(name, *) "Getting a Grid that was set Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Verify Grid

    write(failMsg, *) "Did not verify"
    write(name, *) "Verify Grid that was set Test"
    call ESMF_Test((grid==gridInA), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------

    gridInB = ESMF_GridEmptyCreate(rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Set a Grid

    call ESMF_GridCompSet(gcomp, gridList=(/gridInB, gridInA/), rc=rc)

    write(failMsg, *) "Did return ESMF_SUCCESS"
    write(name, *) "Re-setting a list of Grids Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Query gridIsPresent

    call ESMF_GridCompGet(gcomp, gridIsPresent=isPresent, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Query gridIsPresent for Grid that was set Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Verify gridIsPresent

    write(failMsg, *) "Did not verify"
    write(name, *) "Verify gridIsPresent for Grid that was set Test"
    call ESMF_Test((isPresent), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test get a Grid that was set

    call ESMF_GridCompGet(gcomp, grid=grid, rc=rc)

    write(failMsg, *) "Did return ESMF_SUCCESS"
    write(name, *) "Getting a Grid that was set Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Verify Grid

    write(failMsg, *) "Did not verify"
    write(name, *) "Verify Grid that was set Test"
    call ESMF_Test((grid/=gridInA), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Verify Grid

    write(failMsg, *) "Did not verify"
    write(name, *) "Verify Grid that was set Test"
    call ESMF_Test((grid==gridInB), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test get a gridList that was set

    call ESMF_GridCompGet(gcomp, gridList=gridList, rc=rc)

    write(failMsg, *) "Did return ESMF_SUCCESS"
    write(name, *) "Getting a gridList that was set Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Verify gridList

    write(failMsg, *) "Did not verify"
    write(name, *) "Verify gridList that was set Test"
    call ESMF_Test((size(gridList)==2), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Verify gridList

    write(failMsg, *) "Did not verify"
    write(name, *) "Verify gridList(1) that was set Test"
    call ESMF_Test((gridList(1)==gridInB), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Verify gridList

    write(failMsg, *) "Did not verify"
    write(name, *) "Verify gridList(2) that was set Test"
    call ESMF_Test((gridList(2)==gridInA), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------

    call ESMF_GridDestroy(gridInA, rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    call ESMF_GridDestroy(gridInB, rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
!-------------------------------------------------------------------------
!--- Mesh handling

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Query meshIsPresent

    call ESMF_GridCompGet(gcomp, meshIsPresent=isPresent, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Query meshIsPresent for Mesh that was not set Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Verify meshIsPresent

    write(failMsg, *) "Did not verify"
    write(name, *) "Verify meshIsPresent for Mesh that was not set Test"
    call ESMF_Test((.not.isPresent), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test get a Mesh that was not set

    call ESMF_GridCompGet(gcomp, mesh=mesh, rc=rc)

    write(failMsg, *) "Did return ESMF_SUCCESS"
    write(name, *) "Getting a Mesh that was not set Test"
    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------

    meshInA = ESMF_MeshCreateCubedSphere(tileSize=45, nx=2,ny=2, rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Set a Mesh

    call ESMF_GridCompSet(gcomp, mesh=meshInA, rc=rc)

    write(failMsg, *) "Did return ESMF_SUCCESS"
    write(name, *) "Setting a Mesh that was not set Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Query meshIsPresent

    call ESMF_GridCompGet(gcomp, meshIsPresent=isPresent, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Query meshIsPresent for Mesh that was set Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Verify meshIsPresent

    write(failMsg, *) "Did not verify"
    write(name, *) "Verify meshIsPresent for Mesh that was set Test"
    call ESMF_Test((isPresent), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test get a Mesh that was set

    call ESMF_GridCompGet(gcomp, mesh=mesh, rc=rc)

    write(failMsg, *) "Did return ESMF_SUCCESS"
    write(name, *) "Getting a Mesh that was set Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Verify Mesh

    write(failMsg, *) "Did not verify"
    write(name, *) "Verify Mesh that was set Test"
    call ESMF_Test((mesh==meshInA), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------

    meshInB = ESMF_MeshCreateCubedSphere(tileSize=20, nx=2,ny=2, rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Set a mesh

    call ESMF_GridCompSet(gcomp, meshList=(/meshInB, meshInA/), rc=rc)

    write(failMsg, *) "Did return ESMF_SUCCESS"
    write(name, *) "Re-setting a list of Meshes Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Query meshIsPresent

    call ESMF_GridCompGet(gcomp, meshIsPresent=isPresent, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Query meshIsPresent for Mesh that was set Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Verify meshIsPresent

    write(failMsg, *) "Did not verify"
    write(name, *) "Verify meshIsPresent for Mesh that was set Test"
    call ESMF_Test((isPresent), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test get a Mesh that was set

    call ESMF_GridCompGet(gcomp, mesh=mesh, rc=rc)

    write(failMsg, *) "Did return ESMF_SUCCESS"
    write(name, *) "Getting a Mesh that was set Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Verify Mesh

    write(failMsg, *) "Did not verify"
    write(name, *) "Verify Mesh that was set Test"
    call ESMF_Test((mesh/=meshInA), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Verify Mesh

    write(failMsg, *) "Did not verify"
    write(name, *) "Verify Mesh that was set Test"
    call ESMF_Test((mesh==meshInB), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test get a meshList that was set

    call ESMF_GridCompGet(gcomp, meshList=meshList, rc=rc)

    write(failMsg, *) "Did return ESMF_SUCCESS"
    write(name, *) "Getting a meshList that was set Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Verify meshList

    write(failMsg, *) "Did not verify"
    write(name, *) "Verify meshList that was set Test"
    call ESMF_Test((size(meshList)==2), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Verify meshList

    write(failMsg, *) "Did not verify"
    write(name, *) "Verify meshList(1) that was set Test"
    call ESMF_Test((meshList(1)==meshInB), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Verify meshList

    write(failMsg, *) "Did not verify"
    write(name, *) "Verify meshList(2) that was set Test"
    call ESMF_Test((meshList(2)==meshInA), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------

    call ESMF_MeshDestroy(meshInA, rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    call ESMF_MeshDestroy(meshInB, rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
!-------------------------------------------------------------------------
!--- Config handling

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test correct config handling

    call ESMF_GridCompGet(gcomp, configIsPresent=isPresent, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Config handling Test - configIsPresent before setting"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    
!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test correct config handling
    write(failMsg, *) "Did not return correct isPresent status"
    write(name, *) "Config handling Test - configIsPresent value before setting"
    call ESMF_Test((.not.isPresent), name, failMsg, result, ESMF_SRCLINE)
    
!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test correct config handling

    call ESMF_GridCompGet(gcomp, configFileIsPresent=isPresent, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Config handling Test - configFileIsPresent before setting"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    
!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test correct config handling
    write(failMsg, *) "Did not return correct isPresent status"
    write(name, *) "Config handling Test - configFileIsPresent value before setting"
    call ESMF_Test((.not.isPresent), name, failMsg, result, ESMF_SRCLINE)
    
!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Set a configFile

    call ESMF_GridCompSet(gcomp, configFile="comp.yaml", rc=rc)

    write(failMsg, *) "Did return ESMF_SUCCESS"
    write(name, *) "Setting a ConfigFile Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    
!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test correct config handling

    call ESMF_GridCompGet(gcomp, configFileIsPresent=isPresent, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Config handling Test - configFileIsPresent"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    
!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test correct config handling
    write(failMsg, *) "Did not return correct isPresent status"
    write(name, *) "Config handling Test - configFileIsPresent value"
    call ESMF_Test((isPresent), name, failMsg, result, ESMF_SRCLINE)
    
!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test correct config handling

    call ESMF_GridCompGet(gcomp, configIsPresent=isPresent, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Config handling Test - configIsPresent"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test correct config handling
    write(failMsg, *) "Did not return correct isPresent status"
    write(name, *) "Config handling Test - configIsPresent value"
    call ESMF_Test((isPresent), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test correct config handling

    call ESMF_GridCompGet(gcomp, config=config, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Config handling Test - get config"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test correct config handling

    fred = 0
    call ESMF_ConfigGetAttribute(config, fred, label="fred:", rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Config handling Test - access attribute through config"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test correct config handling

    write(msgStr,*) "fred = ", fred
    call ESMF_LogWrite(msgStr, ESMF_LOGMSG_INFO, rc=rc)

    write(failMsg, *) "Did not return correct value in fred"
    write(name, *) "Config handling Test - validate attribute value"
    call ESMF_Test((fred==1), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test correct hconfig handling

    call ESMF_GridCompGet(gcomp, hconfigIsPresent=isPresent, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "HConfig handling Test - hconfigIsPresent"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test correct hconfig handling
    write(failMsg, *) "Did not return correct isPresent status"
    write(name, *) "HConfig handling Test - hconfigIsPresent value"
    call ESMF_Test((isPresent), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test correct hconfig handling

    call ESMF_GridCompGet(gcomp, hconfig=hconfig, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "HConfig handling Test - get hconfig"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test correct hconfig handling

    fred = 0
    fred = ESMF_HConfigAsI4(hconfig, keyString="fred", rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "HConfig handling Test - access map through hconfig"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test correct hconfig handling

    write(msgStr,*) "fred = ", fred
    call ESMF_LogWrite(msgStr, ESMF_LOGMSG_INFO, rc=rc)

    write(failMsg, *) "Did not return correct value in fred"
    write(name, *) "HConfig handling Test - validate attribute value"
    call ESMF_Test((fred==1), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test creation of a Component
    write(name, *) "Creating a Component with Config Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"

    gcomp2 = ESMF_GridCompCreate(name="TestComp", config=config, rc=rc)

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
    write(name, *) "GridCompDestroy Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_GridCompDestroy(gcomp2, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test creation of a Component
    write(name, *) "Creating a Component with HConfig Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"

    gcomp2 = ESMF_GridCompCreate(name="TestComp", hconfig=hconfig, rc=rc)

    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
    write(name, *) "GridCompDestroy Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_GridCompDestroy(gcomp2, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test creation of a Component
    write(name, *) "Creating a Component with too many config args Test"
    write(failMsg, *) "Did not return expected return code"

    gcomp2 = ESMF_GridCompCreate(name="TestComp", hconfig=hconfig, &
      config=config, rc=rc)

    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test creation of a Component
    write(name, *) "Creating a Component with too many config args Test"
    write(failMsg, *) "Did not return expected return code"

    gcomp2 = ESMF_GridCompCreate(name="TestComp", config=config, &
      configFile="comp.yaml", rc=rc)

    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test creation of a Component
    write(name, *) "Creating a Component with too many config args Test"
    write(failMsg, *) "Did not return expected return code"

    gcomp2 = ESMF_GridCompCreate(name="TestComp", hconfig=hconfig, &
      configFile="comp.yaml", rc=rc)

    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test creation of a Component
    write(name, *) "Creating a Component with too many config args Test"
    write(failMsg, *) "Did not return expected return code"

    gcomp2 = ESMF_GridCompCreate(name="TestComp", hconfig=hconfig, &
      config=config, configFile="comp.yaml", rc=rc)

    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
    write(name, *) "GridCompDestroy Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_GridCompDestroy(gcomp, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test creation of a Component
    cname = "Atmosphere"
    gcomp = ESMF_GridCompCreate(name=cname, configFile="comp.yaml", rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Component Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test correct config handling

    call ESMF_GridCompGet(gcomp, configFileIsPresent=isPresent, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Config handling Test - configFileIsPresent"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    
!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test correct config handling
    write(failMsg, *) "Did not return correct isPresent status"
    write(name, *) "Config handling Test - configFileIsPresent value"
    call ESMF_Test((isPresent), name, failMsg, result, ESMF_SRCLINE)
    
!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test correct config handling

    call ESMF_GridCompGet(gcomp, configIsPresent=isPresent, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Config handling Test - configIsPresent"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    
!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test correct config handling
    write(failMsg, *) "Did not return correct isPresent status"
    write(name, *) "Config handling Test - configIsPresent value"
    call ESMF_Test((isPresent), name, failMsg, result, ESMF_SRCLINE)
    
!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test correct config handling

    call ESMF_GridCompGet(gcomp, config=config, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Config handling Test - get config"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    
!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test correct config handling

    call ESMF_ConfigGetAttribute(config, fred, label="fred:", rc=rc)
    
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Config handling Test - access attribute through config"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test correct config handling

    print *, "fred = ", fred

    write(failMsg, *) "Did not return correct value in fred"
    write(name, *) "Config handling Test - validate attribute value"
    call ESMF_Test((fred==1), name, failMsg, result, ESMF_SRCLINE)
    
!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test validate a component

    call ESMF_GridCompValidate(gcomp, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Validating a Component Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!-------------------------------------------------------------------------
!   !
    !EX_UTest
    ! Wait for a concurrent component to finish executing.

    call ESMF_GridCompWait(gcomp, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Waiting for a Component Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test get a Component name

    call ESMF_GridCompGet(gcomp, name=bname, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Getting a Component name Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Verify the name is correct

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Verifying the correct Component name was returned Test"
    call ESMF_Test((bname.eq.cname), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test get a Grid that was not set

    call ESMF_GridCompGet(gcomp, grid=grid, rc=rc)

    write(failMsg, *) "Did return ESMF_SUCCESS"
    write(name, *) "Getting a Grid that was not set Test"
    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
!   !  Set Internal State
    !EX_UTest
    allocate(wrap1%p)
    wrap1%p%testnumber=4567

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set Internal State Test"
    call ESMF_GridCompSetInternalState(gcomp, wrap1, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
!   !  Get Internal State
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Get Internal State Test"
    nullify(wrap2%p)
    call ESMF_GridCompGetInternalState(gcomp, wrap2, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
!   !  Verify Internal State
    !EX_UTest
    write(failMsg, *) "Did not return correct data"
    write(name, *) "Verify Internal State Test"
    if (associated(wrap2%p)) then
      call ESMF_Test((wrap2%p%testnumber.eq.4567), name, failMsg, result, ESMF_SRCLINE)
      print *, "wrap2%p%testnumber = ", wrap2%p%testnumber
    else
      call ESMF_Test(.false., name, failMsg, result, ESMF_SRCLINE)
    endif

!-------------------------------------------------------------------------
!   !
!   !  Set Internal State
    !EX_UTest
    allocate(wrap3%p)
    wrap3%p%testnumber=1234

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set Internal State 2nd time Test"
    call ESMF_GridCompSetInternalState(gcomp, wrap3, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
!   !  Get Internal State
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Get Internal State 2nd time Test"
    nullify(wrap4%p)
    call ESMF_GridCompGetInternalState(gcomp, wrap4, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
!   !  Verify Internal State
    !EX_UTest
    write(failMsg, *) "Did not return correct data"
    write(name, *) "Verify Internal State 2nd time Test"
    if (associated(wrap4%p)) then
      call ESMF_Test((wrap4%p%testnumber.eq.1234), name, failMsg, result, ESMF_SRCLINE)
      print *, "wrap4%p%testnumber = ", wrap4%p%testnumber
    else
      call ESMF_Test(.false., name, failMsg, result, ESMF_SRCLINE)
    endif

!-------------------------------------------------------------------------
!   !  Set Internal State
    !EX_UTest
    allocate(wrap5%p)
    wrap5%p%testnumber=9182

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set Internal State 3rd time Test"
    call ESMF_GridCompSetInternalState(gcomp, wrap5, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
!   !  Get Internal State
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Get Internal State 3rd time Test"
    nullify(wrap6%p)
    call ESMF_GridCompGetInternalState(gcomp, wrap6, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
!   !  Verify Internal State
    !EX_UTest
    write(failMsg, *) "Did not return correct data"
    write(name, *) "Verify Internal State 3rd time Test"
    if (associated(wrap6%p)) then
      call ESMF_Test((wrap6%p%testnumber.eq.9182), name, failMsg, result, ESMF_SRCLINE)
      print *, "wrap6%p%testnumber = ", wrap6%p%testnumber
    else
      call ESMF_Test(.false., name, failMsg, result, ESMF_SRCLINE)
    endif

!-------------------------------------------------------------------------
!   !  Set Internal State - through UserComp API
    !EX_UTest
    allocate(wrap7%p)
    wrap7%p%testnumber=192837465

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set Internal State (4th time) through UserComp API Test"
    call ESMF_UserCompSetInternalState(gcomp, "namedLabel", wrap7, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
!   !  Get Internal State - through UserComp API
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Get Internal State (4th time) through UserComp API Test"
    nullify(wrap8%p)
    call ESMF_UserCompGetInternalState(gcomp, "namedLabel", wrap8, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
!   !  Verify Internal State - through UserComp API
    !EX_UTest
    write(failMsg, *) "Did not return correct data"
    write(name, *) "Verify Internal State (4th time) through UserComp API Test"
    if (associated(wrap8%p)) then
      call ESMF_Test((wrap8%p%testnumber.eq.192837465), name, failMsg, result, ESMF_SRCLINE)
      print *, "wrap8%p%testnumber = ", wrap8%p%testnumber
    else
      call ESMF_Test(.false., name, failMsg, result, ESMF_SRCLINE)
    endif

!-------------------------------------------------------------------------
!   !  Set Internal State - through UserComp API
    !EX_UTest
    allocate(wrap9%p)
    wrap9%p%testnumber=564738291

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set Internal State (5th time) through UserComp API Test"
#ifndef ESMF_NO_F2018ASSUMEDTYPE
    call ESMF_UserCompSetInternalState(gcomp, internalState=wrap9, rc=rc)
#else
    call ESMF_UserCompSetInternalState(gcomp, "namedLabel2", wrap9, rc)
#endif
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
!   !  Get Internal State - through UserComp API
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Get Internal State (5th time) through UserComp API Test"
    nullify(wrap10%p)
#ifndef ESMF_NO_F2018ASSUMEDTYPE
    call ESMF_UserCompGetInternalState(gcomp, internalState=wrap10, rc=rc)
#else
    call ESMF_UserCompGetInternalState(gcomp, "namedLabel2", wrap10, rc)
#endif
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
!   !  Verify Internal State - through UserComp API
    !EX_UTest
    write(failMsg, *) "Did not return correct data"
    write(name, *) "Verify Internal State (5th time) through UserComp API Test"
    if (associated(wrap10%p)) then
      call ESMF_Test((wrap10%p%testnumber.eq.564738291), name, failMsg, result, ESMF_SRCLINE)
      print *, "wrap10%p%testnumber = ", wrap10%p%testnumber
    else
      call ESMF_Test(.false., name, failMsg, result, ESMF_SRCLINE)
    endif

! - clean-up
    deallocate(wrap1%p)
    deallocate(wrap3%p)
    deallocate(wrap5%p)
    deallocate(wrap7%p)
    deallocate(wrap9%p)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Access information about the currently set internal states

    call ESMF_InternalStateGet(gcomp, labelList, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Accessing information about the set internal states Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    ! print the labels
    do i=1, size(labelList)
      print *, labelList(i)
    enddo

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test printing a component

    call ESMF_GridCompPrint(gcomp, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Printing a Component Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   ! Verifing that a GridCompDestroy for a regular component catches timeout
    
    call ESMF_GridCompDestroy(gcomp, timeout=10, rc=rc)
    
    write(failMsg, *) "Did return ESMF_SUCCESS"
    write(name, *) "Destroying a Gridded Component - with timeout"
    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Destroying a component

    call ESMF_GridCompDestroy(gcomp, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Destroying a Component Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test creation of a Component with petList
    gcomp = ESMF_GridCompCreate(petList=(/0/), rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Component with petList Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Destroying a component

    call ESMF_GridCompDestroy(gcomp, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Destroying a Component created with petList Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test creation of a Component with empty petList
    allocate(petList(0))
    gcomp = ESMF_GridCompCreate(petList=petList, rc=rc)
    deallocate(petList)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Component with empty petList Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Destroying a component

    call ESMF_GridCompDestroy(gcomp, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Destroying a Component created with empty petList Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#endif

    call ESMF_TestEnd(ESMF_SRCLINE)

    end program ESMF_GridCompCreateUTest

