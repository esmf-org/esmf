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
    type(ESMF_GridComp) :: comp1, gridcompAlias
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
    type(ESMF_Grid)               :: grid, gridInA, gridInB
    type(ESMF_Grid), allocatable  :: gridList(:)
    type(ESMF_Mesh)               :: mesh, meshInA, meshInB
    type(ESMF_Mesh), allocatable  :: meshList(:)
    logical                       :: isPresent
    type(ESMF_Config)             :: config
    integer                       :: fred
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
    isCreated = ESMF_GridCompIsCreated(comp1)
    call ESMF_Test((isCreated .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)

    !-----------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Testing GridComp IsCreated for uncreated object"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    isCreated = ESMF_GridCompIsCreated(comp1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-----------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Testing GridComp IsCreated value for uncreated object"
    write(failMsg, *) "Did not return .false."
    call ESMF_Test((isCreated .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest
    cname = "Atmosphere"
    comp1 = ESMF_GridCompCreate(name=cname, configFile="comp.rc", rc=rc)  
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Component Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-----------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Testing GridComp IsCreated w/o keyword for created object"
    write(failMsg, *) "Did not return .true."
    isCreated = ESMF_GridCompIsCreated(comp1)
    call ESMF_Test((isCreated .eqv. .true.), name, failMsg, result, ESMF_SRCLINE)

    !-----------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Testing GridComp IsCreated for created object"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    isCreated = ESMF_GridCompIsCreated(comp1, rc=rc)
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
    gridcompBool = (gridcompAlias.eq.comp1)
    call ESMF_Test(.not.gridcompBool, name, failMsg, result, ESMF_SRCLINE)
    
    !------------------------------------------------------------------------
    !NEX_UTest
    ! Testing ESMF_GridCompAssignment(=)()
    write(name, *) "GridComp assignment and equality Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    gridcompAlias = comp1
    gridcompBool = (gridcompAlias.eq.comp1)
    call ESMF_Test(gridcompBool, name, failMsg, result, ESMF_SRCLINE)
    
    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "GridCompDestroy Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_GridCompDestroy(comp1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-----------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Testing GridComp IsCreated w/o keyword for destroyed object"
    write(failMsg, *) "Did not return .false."
    isCreated = ESMF_GridCompIsCreated(comp1)
    call ESMF_Test((isCreated .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)

    !-----------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Testing GridComp IsCreated for destroyed object"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    isCreated = ESMF_GridCompIsCreated(comp1, rc=rc)
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
    gridcompBool = (gridcompAlias==comp1)
    call ESMF_Test(.not.gridcompBool, name, failMsg, result, ESMF_SRCLINE)
    
    !------------------------------------------------------------------------
    !NEX_UTest
    ! Testing ESMF_GridCompOperator(/=)()
    write(name, *) "GridComp non-equality after destroy Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    gridcompBool = (gridcompAlias/=comp1)
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
    comp1 = ESMF_GridCompCreate(name=cname, configFile="comp.rc", rc=rc)  
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Component Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "GridCompDestroy Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_GridCompDestroy(comp1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_TESTEXHAUSTIVE

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test get a Component name from a destroyed component

    call ESMF_GridCompGet(comp1, name=bname, rc=rc)

    write(failMsg, *) "Did return ESMF_SUCCESS"
    write(name, *) "Getting a Component name Test"
    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test creation of a Component
    comp1 = ESMF_GridCompCreate(rc=rc)  

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Component Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!--- Grid handling

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Query gridIsPresent

    call ESMF_GridCompGet(comp1, gridIsPresent=isPresent, rc=rc)

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

    call ESMF_GridCompGet(comp1, grid=grid, rc=rc)

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

    call ESMF_GridCompSet(comp1, grid=gridInA, rc=rc)

    write(failMsg, *) "Did return ESMF_SUCCESS"
    write(name, *) "Setting a Grid that was not set Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Query gridIsPresent

    call ESMF_GridCompGet(comp1, gridIsPresent=isPresent, rc=rc)

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

    call ESMF_GridCompGet(comp1, grid=grid, rc=rc)

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

    call ESMF_GridCompSet(comp1, gridList=(/gridInB, gridInA/), rc=rc)

    write(failMsg, *) "Did return ESMF_SUCCESS"
    write(name, *) "Re-setting a list of Grids Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Query gridIsPresent

    call ESMF_GridCompGet(comp1, gridIsPresent=isPresent, rc=rc)

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

    call ESMF_GridCompGet(comp1, grid=grid, rc=rc)

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

    call ESMF_GridCompGet(comp1, gridList=gridList, rc=rc)

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

    call ESMF_GridCompGet(comp1, meshIsPresent=isPresent, rc=rc)

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

    call ESMF_GridCompGet(comp1, mesh=mesh, rc=rc)

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

    call ESMF_GridCompSet(comp1, mesh=meshInA, rc=rc)

    write(failMsg, *) "Did return ESMF_SUCCESS"
    write(name, *) "Setting a Mesh that was not set Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Query meshIsPresent

    call ESMF_GridCompGet(comp1, meshIsPresent=isPresent, rc=rc)

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

    call ESMF_GridCompGet(comp1, mesh=mesh, rc=rc)

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

    call ESMF_GridCompSet(comp1, meshList=(/meshInB, meshInA/), rc=rc)

    write(failMsg, *) "Did return ESMF_SUCCESS"
    write(name, *) "Re-setting a list of Meshes Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Query meshIsPresent

    call ESMF_GridCompGet(comp1, meshIsPresent=isPresent, rc=rc)

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

    call ESMF_GridCompGet(comp1, mesh=mesh, rc=rc)

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

    call ESMF_GridCompGet(comp1, meshList=meshList, rc=rc)

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

    call ESMF_GridCompGet(comp1, configIsPresent=isPresent, rc=rc)

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

    call ESMF_GridCompGet(comp1, configFileIsPresent=isPresent, rc=rc)

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

    call ESMF_GridCompSet(comp1, configFile="comp.rc", rc=rc)

    write(failMsg, *) "Did return ESMF_SUCCESS"
    write(name, *) "Setting a ConfigFile Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    
!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test correct config handling

    call ESMF_GridCompGet(comp1, configFileIsPresent=isPresent, rc=rc)

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

    call ESMF_GridCompGet(comp1, configIsPresent=isPresent, rc=rc)

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

    call ESMF_GridCompGet(comp1, config=config, rc=rc)

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
    write(name, *) "GridCompDestroy Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_GridCompDestroy(comp1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test creation of a Component
    cname = "Atmosphere"
    comp1 = ESMF_GridCompCreate(name=cname, configFile="comp.rc", rc=rc)  

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Component Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test correct config handling

    call ESMF_GridCompGet(comp1, configFileIsPresent=isPresent, rc=rc)

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

    call ESMF_GridCompGet(comp1, configIsPresent=isPresent, rc=rc)

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

    call ESMF_GridCompGet(comp1, config=config, rc=rc)

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

    call ESMF_GridCompValidate(comp1, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Validating a Component Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!-------------------------------------------------------------------------
!   !
    !EX_UTest
    ! Wait for a concurrent component to finish executing.

    call ESMF_GridCompWait(comp1, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Waiting for a Component Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test get a Component name

    call ESMF_GridCompGet(comp1, name=bname, rc=rc)

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

    call ESMF_GridCompGet(comp1, grid=grid, rc=rc)

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
    call ESMF_GridCompSetInternalState(comp1, wrap1, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
!   !  Get Internal State
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Get Internal State Test"
    call ESMF_GridCompGetInternalState(comp1, wrap2, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
!   !  Verify Internal State
    !EX_UTest
    write(failMsg, *) "Did not return correct data"
    write(name, *) "Verify Internal State Test"
    call ESMF_Test((wrap2%p%testnumber.eq.4567), name, failMsg, result, ESMF_SRCLINE)
    print *, "wrap2%p%testnumber = ", wrap2%p%testnumber
    
!-------------------------------------------------------------------------
!   !
!   !  Set Internal State
    !EX_UTest
    allocate(wrap3%p)
    wrap3%p%testnumber=1234

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set Internal State 2nd time Test"
    call ESMF_GridCompSetInternalState(comp1, wrap3, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
!   !  Get Internal State
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Get Internal State 2nd time Test"
    call ESMF_GridCompGetInternalState(comp1, wrap4, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
!   !  Verify Internal State
    !EX_UTest
    write(failMsg, *) "Did not return correct data"
    write(name, *) "Verify Internal State 2nd time Test"
    call ESMF_Test((wrap4%p%testnumber.eq.1234), name, failMsg, result, ESMF_SRCLINE)
    print *, "wrap4%p%testnumber = ", wrap4%p%testnumber
    
!-------------------------------------------------------------------------
!   !  Set Internal State
    !EX_UTest
    allocate(wrap5%p)
    wrap5%p%testnumber=9182

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set Internal State 3rd time Test"
    call ESMF_GridCompSetInternalState(comp1, wrap5, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
!   !  Get Internal State
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Get Internal State 3rd time Test"
    call ESMF_GridCompGetInternalState(comp1, wrap6, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
!   !  Verify Internal State
    !EX_UTest
    write(failMsg, *) "Did not return correct data"
    write(name, *) "Verify Internal State 3rd time Test"
    call ESMF_Test((wrap6%p%testnumber.eq.9182), name, failMsg, result, ESMF_SRCLINE)
    print *, "wrap6%p%testnumber = ", wrap6%p%testnumber
    
    deallocate(wrap1%p)
    deallocate(wrap3%p)
    deallocate(wrap5%p)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test printing a component

    call ESMF_GridCompPrint(comp1, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Printing a Component Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   ! Verifing that a GridCompDestroy for a regular component catches timeout
    
    call ESMF_GridCompDestroy(comp1, timeout=10, rc=rc)
    
    write(failMsg, *) "Did return ESMF_SUCCESS"
    write(name, *) "Destroying a Gridded Component - with timeout"
    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Destroying a component

    call ESMF_GridCompDestroy(comp1, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Destroying a Component Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#endif

    call ESMF_TestEnd(ESMF_SRCLINE)

    end program ESMF_GridCompCreateUTest
    
