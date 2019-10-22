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
program ESMF_GridCreateUTest

!------------------------------------------------------------------------------

#include "ESMF.h"
#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_GridCreateTest - Check Grid Create Routines
!
! !DESCRIPTION:
!
! The code in this file drives F90 Grid Create unit tests.
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id$'
!------------------------------------------------------------------------------
    
  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: localrc, rc, localPet, petCount
  logical :: correct, xercesNotPresent
  type(ESMF_TypeKind_Flag) :: typekind

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name, grid_name

  type(ESMF_Grid) :: grid, grid2, gridAlias,grid_multi
  type(ESMF_VM) :: vm
  type(ESMF_DistGrid) :: distgrid, distgrid2, distgrid_multi
  type(ESMF_Array) :: array
  integer :: coordDimMap(2,2), dimCount, undistLBound(3), undistUBound(3)
  type(ESMF_Index_Flag) :: indexflag
  integer :: distgridToGridMap(2), coordDimCount(2)
  integer :: distgridToArrayMap(3)
  integer :: coordDimCount2(3),coordDimMap2(3,3)
  integer :: gridEdgeLWidth(3),gridEdgeUWidth(3),gridAlign(3)
  integer :: exlbnd(3),exubnd(3)
  integer :: clbnd(3),cubnd(3)
  character, pointer :: buf(:)
  real(ESMF_KIND_R8), pointer :: fptr2D(:,:)
  integer :: bufCount, offset, localDECount, rank, i1,i2,lDE, i, j
  type(ESMF_StaggerLoc)          :: staggerloc8
  integer :: minIndex(3), maxIndex(3) 
  integer :: celw(3),ceuw(3)
  logical :: isLBound(2),isUBound(2)
  integer :: petMap2D(2,2,1)
  integer :: petMap2x3(2,3,1)
  real(ESMF_KIND_R8), pointer :: fptr(:,:), fptr1(:,:), fptr2(:,:)
  real(ESMF_KIND_R4), pointer :: fptr1R4(:,:), fptr2R4(:,:)
  logical:: gridBool
  logical:: isCreated
  type(ESMF_GridStatus_Flag) :: status
  ! test the AttributeGet for Grid info
  type(ESMF_TypeKind_Flag) :: attrValue
  type(ESMF_CoordSys_Flag) :: coordSys
  integer :: minIndexPTile(2,4), maxIndexPTile(2,4)
  integer :: regDecompPTile4(2,4)
  type(ESMF_Decomp_Flag) :: decompPTile(2,4)
  integer :: tile
  integer(ESMF_KIND_I4) :: regDecompPTile(2,6), deLabelList(6)
  type(ESMF_Decomp_Flag) :: decompFlagPTile(2,6)
  real(ESMF_KIND_R8), pointer :: lonPtrR8(:,:), latPtrR8(:,:)
  real(ESMF_KIND_R4), pointer :: lonPtrR4(:,:), latPtrR4(:,:)
  real(ESMF_KIND_R8), allocatable :: lonDiff(:,:), latDiff(:,:)
  real(ESMF_KIND_R8), allocatable :: mean(:)
  real(ESMF_KIND_R8) :: lonmin, latmin, lonmax, latmax, lonmean, latmean
  real(ESMF_KIND_R8) :: threshhold
  type(ESMF_DELayout) :: delayout
  integer :: total, s,  decount, localDe
  type(ESMF_Staggerloc) :: staggerLocList(2)
  type(ESMF_CubedSphereTransform_Args) :: transformArgs
 
  !-----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !-----------------------------------------------------------------------------

  ! get global VM
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! prepare DistGrid
  distgrid=ESMF_DistGridCreate(minIndex=(/1,1/),maxIndex=(/10,10/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing Grid IsCreated for uncreated object"
  write(failMsg, *) "Did not return .false."
  isCreated = ESMF_GridIsCreated(grid)
  call ESMF_Test((isCreated .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing Grid IsCreated for uncreated object"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_GridIsCreated(grid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Create test Grid for IsCreated"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  grid=ESMF_GridCreate(distgrid=distgrid, coordTypeKind=ESMF_TYPEKIND_I4, rc=localrc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing Grid IsCreated for created object"
  write(failMsg, *) "Did not return .true."
  isCreated = ESMF_GridIsCreated(grid)
  call ESMF_Test((isCreated .eqv. .true.), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing Grid IsCreated for created object"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_GridIsCreated(grid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Print Grid info"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_GridPrint(grid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroy test Grid for IsCreated"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_GridDestroy(grid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing Grid IsCreated for destroyed object"
  write(failMsg, *) "Did not return .false."
  isCreated = ESMF_GridIsCreated(grid)
  call ESMF_Test((isCreated .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing Grid IsCreated for destroyed object"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_GridIsCreated(grid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Grid creation Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  grid=ESMF_GridCreate(distgrid=distgrid, coordTypeKind=ESMF_TYPEKIND_I4, rc=localrc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Grid equality before assignment Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  gridBool = (gridAlias.eq.grid)
  call ESMF_Test(.not.gridBool, name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  ! Testing ESMF_GridAssignment(=)()
  write(name, *) "Grid assignment and equality Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  gridAlias = grid
  gridBool = (gridAlias.eq.grid)
  call ESMF_Test(gridBool, name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "GridDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_GridDestroy(grid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  ! Testing ESMF_GridOperator(==)()
  write(name, *) "Grid equality after destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  gridBool = (gridAlias==grid)
  call ESMF_Test(.not.gridBool, name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  ! Testing ESMF_GridOperator(/=)()
  write(name, *) "Grid non-equality after destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  gridBool = (gridAlias/=grid)
  call ESMF_Test(gridBool, name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Double GridDestroy through alias Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_GridDestroy(gridAlias, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing Grid Validate"
  write(failMsg, *) "Incorrect result"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! First make sure validate fails for an uncreated Grid
  call ESMF_GridValidate(grid,rc=localrc)
  if (localrc .eq. ESMF_SUCCESS) correct=.false.

  ! Now make sure that a created grid validates successfully
  !! Create Grid
  grid=ESMF_GridCreate(distgrid=distgrid, coordTypeKind=ESMF_TYPEKIND_I4,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  !! Check that validate returns true
!  call ESMF_GridValidate(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.

  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Grid creation from file ESMF_FILEFORMAT_SCRIP with default regDecomp Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  grid=ESMF_GridCreate('data/T42_grid.nc',ESMF_FILEFORMAT_SCRIP,rc=rc)
#ifdef ESMF_NETCDF

  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE) 
#endif

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Grid creation from file ESMF_FILEFORMAT_SCRIP with custom regDecomp Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  grid=ESMF_GridCreate('data/T42_grid.nc',ESMF_FILEFORMAT_SCRIP, &
    regDecomp=(/2,2/), rc=rc)
  if (petCount==1) then
    write(failMsg, *) "Did not return ESMF failure RC"
    call ESMF_Test((rc/=ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
#ifdef ESMF_NETCDF
    call ESMF_GridDestroy(grid,rc=localrc)
    if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
    write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
    call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE) 
#endif
  endif

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Grid creation from file ESMF_FILEFORMAT_SCRIP with custom regDecomp < petCount Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  grid=ESMF_GridCreate('data/T42_grid.nc',ESMF_FILEFORMAT_SCRIP, &
    regDecomp=(/2,2/), rc=rc)
  if (petCount==1) then
    write(failMsg, *) "Did not return ESMF failure RC"
    call ESMF_Test((rc/=ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
#ifdef ESMF_NETCDF
    call ESMF_GridDestroy(grid,rc=localrc)
    if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
    write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
    call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE) 
#endif
  endif

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Grid creation from a regular structured grid in GRIDSPEC format with default ESMF_TYPEKIND_R8"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  grid=ESMF_GridCreate('data/GRIDSPEC_1x1.nc', rc=rc)
#ifdef ESMF_NETCDF

  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE) 
#endif

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Grid creation from a regular structured grid in GRIDSPEC format with coordTypeKind=ESMF_TYPEKIND_R4"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  grid=ESMF_GridCreate('data/GRIDSPEC_1x1.nc', coordTypeKind=ESMF_TYPEKIND_R4, rc=rc)
#ifdef ESMF_NETCDF
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE) 
#endif

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Grid creation from file ESMF_FILEFORMAT_GRIDSPEC with default ESMF_TYPEKIND_R8"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  grid=ESMF_GridCreate('data/RCM3_CF_CART2D.nc', rc=rc)
#ifdef ESMF_NETCDF
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE) 
#endif

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Grid creation from file ESMF_FILEFORMAT_GRIDSPEC with coordTypeKind=ESMF_TYPEKIND_R4"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  grid=ESMF_GridCreate('data/RCM3_CF_CART2D.nc', coordTypeKind=ESMF_TYPEKIND_R4, rc=rc)
#ifdef ESMF_NETCDF
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE) 
#endif

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Supergrid creation from file ESMF_FILEFORMAT_GRIDSPEC with custom regDecomp Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  grid=ESMF_GridCreate('data/horizontal_grid.tile6.nc', &
    ESMF_FILEFORMAT_GRIDSPEC, regDecomp=(/2,2/), rc=rc)
  if (petCount==1) then
    write(failMsg, *) "Did not return ESMF failure RC"
    call ESMF_Test((rc/=ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
#ifdef ESMF_NETCDF
    call ESMF_GridDestroy(grid,rc=localrc)
    if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
    write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
    call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE) 
#endif
  endif

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Supergrid creation from file ESMF_FILEFORMAT_GRIDSPEC with custom regDecomp < petCount Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  grid=ESMF_GridCreate('data/horizontal_grid.tile6.nc', &
    ESMF_FILEFORMAT_GRIDSPEC, regDecomp=(/2,1/), rc=rc)
  if (petCount==1) then
    write(failMsg, *) "Did not return ESMF failure RC"
    call ESMF_Test((rc/=ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
#ifdef ESMF_NETCDF
    call ESMF_GridDestroy(grid,rc=localrc)
    if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
    write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
    call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE) 
#endif
  endif

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing Grid Match"
  write(failMsg, *) "Incorrect result"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! Check that match returns invalid
  if (ESMF_GridMatch(grid,grid2,rc=localrc) /=ESMF_GRIDMATCH_INVALID) correct=.false.
  !!  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE, Don't check because it should fail

  ! Create Grid 1
  grid=ESMF_GridCreate(distgrid=distgrid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Check that match returns alias
  if (.not.(ESMF_GridMatch(grid,grid,rc=localrc)>=ESMF_GRIDMATCH_EXACT)) correct=.false.
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Create Grid 2
  grid2=ESMF_GridCreate(distgrid=distgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Check that match returns true
  if (ESMF_GridMatch(grid,grid2,rc=localrc) /=ESMF_GRIDMATCH_EXACT) correct=.false.
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Destroy grid2 and make one that won't match
  call ESMF_GridDestroy(grid2,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  

  ! Create Grid 2
  grid2=ESMF_GridCreate(distgrid=distgrid, coordTypeKind=ESMF_TYPEKIND_I4,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Check that match returns false
  if (ESMF_GridMatch(grid,grid2,rc=localrc)==ESMF_GRIDMATCH_EXACT) correct=.false.   
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  
  ! get rid of first grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get rid of second grid
  call ESMF_GridDestroy(grid2,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing Grid copy from grid and distgrid"
  write(failMsg, *) "Incorrect result"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! Create Grid 1
  grid=ESMF_GridCreate(distgrid=distgrid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get name, so we can set name in the grid create copy
  ! to insure things are identical
  call ESMF_GridGet(grid, name=grid_name, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get local DE Count
  call ESMF_GridGet(grid, localDECount=localDECount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Add center stagger
  call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


   ! Init coordinates to 0.0 (otherwise can have problem with NAN != NAN)
  do lDE=0,localDECount-1  

     ! get and fill 1st coord array
     call ESMF_GridGetCoord(grid, localDE=lDE,  staggerloc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
          farrayPtr=fptr2D, rc=localrc)           
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     fptr2D(:,:)=0.0

     ! get and fill 2nd coord array
     call ESMF_GridGetCoord(grid, localDE=lDE,  staggerloc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
          farrayPtr=fptr2D, rc=localrc)           
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     fptr2D(:,:)=0.0
  enddo

  ! Add edge1 stagger
  call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_EDGE1, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

   ! Init coordinates to 0.0 (otherwise can have problem with NAN != NAN)
  do lDE=0,localDECount-1  

     ! get and fill 1st coord array
     call ESMF_GridGetCoord(grid, localDE=lDE,  staggerloc=ESMF_STAGGERLOC_EDGE1, coordDim=1, &
          farrayPtr=fptr2D, rc=localrc)           
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     fptr2D(:,:)=0.0

     ! get and fill 2nd coord array
     call ESMF_GridGetCoord(grid, localDE=lDE,  staggerloc=ESMF_STAGGERLOC_EDGE1, coordDim=2, &
          farrayPtr=fptr2D, rc=localrc)           
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     fptr2D(:,:)=0.0
  enddo

  ! Add edge2 stagger
  call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_EDGE2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

   ! Init coordinates to 0.0 (otherwise can have problem with NAN != NAN)
  do lDE=0,localDECount-1  

     ! get and fill 1st coord array
     call ESMF_GridGetCoord(grid, localDE=lDE,  staggerloc=ESMF_STAGGERLOC_EDGE2, coordDim=1, &
          farrayPtr=fptr2D, rc=localrc)           
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     fptr2D(:,:)=0.0

     ! get and fill 2nd coord array
     call ESMF_GridGetCoord(grid, localDE=lDE,  staggerloc=ESMF_STAGGERLOC_EDGE2, coordDim=2, &
          farrayPtr=fptr2D, rc=localrc)           
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     fptr2D(:,:)=0.0
  enddo


#if 1
  ! Add corner stagger
  call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

   ! Init coordinates to 0.0 (otherwise can have problem with NAN != NAN)
  do lDE=0,localDECount-1  

     ! get and fill 1st coord array
     call ESMF_GridGetCoord(grid, localDE=lDE,  staggerloc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
          farrayPtr=fptr2D, rc=localrc)           
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     fptr2D(:,:)=0.0

     ! get and fill 2nd coord array
     call ESMF_GridGetCoord(grid, localDE=lDE,  staggerloc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
          farrayPtr=fptr2D, rc=localrc)           
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     fptr2D(:,:)=0.0
  enddo
#endif


#if 1
  ! Add item
  call ESMF_GridAddItem(grid, staggerloc=ESMF_STAGGERLOC_CORNER, &
       itemflag=ESMF_GRIDITEM_AREA, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

   ! Init area to 0.0 (otherwise can have problem with NAN != NAN)
  do lDE=0,localDECount-1  

     ! get and fill 1st coord array
     call ESMF_GridGetItem(grid, localDE=lDE,  staggerloc=ESMF_STAGGERLOC_CORNER, &
          itemflag=ESMF_GRIDITEM_AREA, &
          farrayPtr=fptr2D, rc=localrc)           
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     fptr2D(:,:)=0.0
  enddo
#endif


  ! Create Grid 2 from the original grid and distgrid
  grid2=ESMF_GridCreate(grid, name=trim(grid_name), distgrid=distgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! If the grid create copy works, then grid2 should now be 
  ! a perfect copy of grid, so check that match returns true
  if (ESMF_GridMatch(grid,grid2,rc=localrc)/=ESMF_GRIDMATCH_EXACT) correct=.false.
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get rid of first grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get rid of second grid
  call ESMF_GridDestroy(grid2,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a Grid with only a distgrid and the rest defaults"
  write(failMsg, *) "Incorrect result"
  rc=ESMF_SUCCESS

  ! create a grid with all defaults
  grid=ESMF_GridCreate(distgrid=distgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info from Grid
  call ESMF_GridGet(grid, dimCount=dimCount, coordTypeKind=typekind, &
         distgridToGridMap=distgridToGridMap, coordSys=coordSys, &
         coordDimCount=coordDimCount, coordDimMap=coordDimMap, &
         indexflag=indexflag, &
         gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, &
         gridAlign=gridAlign, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that defaults are as expected
  correct=.true.
  if (typekind .ne. ESMF_TYPEKIND_R8) correct=.false.
  if (dimCount .ne. 2) correct=.false.
  if (coordSys .ne. ESMF_COORDSYS_SPH_DEG) correct=.false.
  if ((distgridToGridMap(1) .ne. 1) .or. (distgridToGridMap(2) .ne. 2)) correct=.false.
  !TODO: what to do about undistLBound and undistUBound
  if ((coordDimCount(1) .ne. 2) .or. (coordDimCount(2) .ne. 2)) correct=.false.
  if ((coordDimMap(1,1) .ne. 1) .or. (coordDimMap(1,2) .ne. 2) .or. & 
      (coordDimMap(2,1) .ne. 1) .or. (coordDimMap(2,2) .ne. 2)) correct=.false.
!  if (indexflag .ne. ESMF_INDEX_DELOCAL) correct=.false.
  if ((gridEdgeLWidth(1) .ne. 0) .or. (gridEdgeLWidth(2) .ne. 0)) correct=.false. 
  if ((gridEdgeUWidth(1) .ne. 1) .or. (gridEdgeUWidth(2) .ne. 1)) correct=.false. 
  if ((gridAlign(1) .ne. -1) .or. (gridAlign(2) .ne. -1)) correct=.false. 

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroying a Grid"
  write(failMsg, *) "Incorrect result"
  call ESMF_GridDestroy(grid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest 
  ! ESMF_StaggerLocPrint test StaggerLoc public interface
  call ESMF_StaggerLocPrint(staggerloc8, rc=rc)
  write(failMsg, *) ""
  write(name, *) "Test ESMF_StaggerLocPrint public interface"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  grid_name="GRID"
  write(name, *) "Creating a Grid with a non-default name"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreate(name=grid_name, distgrid=distgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  grid_name="NOT_GRID"
  call ESMF_GridGet(grid, name=grid_name, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (grid_name .ne. "GRID") correct=.false.
  
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a Grid with non-default coordTypeKind"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreate(distgrid=distgrid, coordTypeKind=ESMF_TYPEKIND_I4,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid,coordTypeKind=typekind,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (typekind .ne. ESMF_TYPEKIND_I4) correct=.false.
  
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a Grid with non-default coordSys"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreate(distgrid=distgrid, coordSys=ESMF_COORDSYS_SPH_DEG,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid,coordSys=coordSys,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (coordSys .ne. ESMF_COORDSYS_SPH_DEG) correct=.false.
  
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a Grid with non-default coordDimCount"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreate(distgrid=distgrid, coordDimCount=(/1,2/),rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, coordDimCount=coordDimCount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if ((coordDimCount(1) .ne. 1) .or. (coordDimCount(2) .ne. 2)) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a Grid with non-default coordDimMap"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  coordDimMap(1,:)=(/1,0/)
  coordDimMap(2,:)=(/2,1/)
  grid=ESMF_GridCreate(distgrid=distgrid, coordDimCount=(/1,2/), &
                                   coordDimMap=coordDimMap,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, coordDimCount=coordDimCount, coordDimMap=coordDimMap, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if ((coordDimCount(1) .ne. 1) .or. (coordDimCount(2) .ne. 2)) correct=.false.
  if (coordDimMap(1,1) .ne. 1) correct=.false.
  if ((coordDimMap(2,1) .ne. 2) .or. (coordDimMap(2,2) .ne. 1)) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a Grid with non-default distgridToGridMap"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreate(distgrid=distgrid, distgridToGridMap=(/1,2/),rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, distgridToGridMap=distgridToGridMap, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if ((distgridToGridMap(1) .ne. 1) .or. (distgridToGridMap(2) .ne. 2)) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a Grid with non-default indexflag"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreate(distgrid=distgrid, indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, indexflag=indexflag, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
!  CAN"T COMPARE INDEX FLAGS RIGHT NOW. 
!  if (indexflag .ne. ESMF_INDEX_GLOBAL) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a Grid with non-default EdgeWidths and Aligns"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreate(distgrid=distgrid, gridEdgeLWidth=(/1,0/), &
         gridEdgeUWidth=(/0,0/), gridAlign=(/1,-1/), rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, &
         gridAlign=gridAlign, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if ((gridEdgeLWidth(1) .ne. 1) .or. (gridEdgeLWidth(2) .ne. 0)) correct=.false. 
  if ((gridEdgeUWidth(1) .ne. 0) .or. (gridEdgeUWidth(2) .ne. 0)) correct=.false. 
  if ((gridAlign(1) .ne. 1) .or. (gridAlign(2) .ne. -1)) correct=.false. 

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a 2D non periodic Grid with GridCreateNoPeriDimIrreg"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2,3,4/), &
                                    countsPerDeDim2=(/3,4,5/), &
                                    rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_DistGridGet(distgrid2, dimCount=dimCount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (dimcount .ne. 2) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a 2D 1 periodic Grid with GridCreate1PeriDimIrreg"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreate1PeriDim(countsPerDEDim1=(/1,2,3,4/), &
                                    countsPerDeDim2=(/3,4,5/), &
                                    rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_DistGridGet(distgrid2, dimCount=dimCount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (dimcount .ne. 2) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a 2D 2 periodic Grid with GridCreate2PeriDimIrreg"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreate2PeriDim(countsPerDEDim1=(/1,2,3,4/), &
                                    countsPerDeDim2=(/3,4,5/), &
                                    rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_DistGridGet(distgrid2, dimCount=dimCount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (dimcount .ne. 2) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a 2D  Grid with default edge connections with GridCreateIrreg()"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreate(countsPerDEDim1=(/1,2,3,4/), &
                       countsPerDeDim2=(/3,4,5/), &
                                    rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_DistGridGet(distgrid2, dimCount=dimCount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (dimcount .ne. 2) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a 2D  Grid with sphere edge connections with GridCreateIrreg()"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreate(countsPerDEDim1=(/1,2,3,4/), &
                       countsPerDeDim2=(/3,4,5/), &
                       connflagDim1=(/ESMF_GRIDCONN_PERIODIC,ESMF_GRIDCONN_PERIODIC/), &
                       connflagDim2=(/ESMF_GRIDCONN_POLE,ESMF_GRIDCONN_POLE/), &
                                    rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_DistGridGet(distgrid2, dimCount=dimCount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (dimcount .ne. 2) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a 3D distributed Grid with CreateShapeTileIrreg"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2,3,4/), &
                                    countsPerDeDim2=(/3,4,5/), &
                                    countsPerDeDim3=(/6,8/), &
                                    rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_DistGridGet(distgrid2, dimCount=dimCount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (dimcount .ne. 3) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test getting Grid status"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  correct=.true.

  ! Create empty grid
  grid=ESMF_GridEmptyCreate(rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! make sure empty grid returns not ready
  call ESMF_GridGet(grid, status=status, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  if (status /=ESMF_GRIDSTATUS_EMPTY) correct=.false. 

  ! Commit grid
  call ESMF_GridEmptyComplete(grid, countsPerDEDim1=(/4/), &
                                    countsPerDeDim2=(/5/), &
                                    countsPerDeDim3=(/8/), &
                                    rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! make sure empty grid returns not ready
  call ESMF_GridGet(grid, status=status, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  if (status /=ESMF_GRIDSTATUS_COMPLETE) correct=.false. 

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test getting information from an empty Grid"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  correct=.true.

  ! Create empty grid
  grid=ESMF_GridEmptyCreate(rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Set some stuff
  call ESMF_GridSet(grid, &
       distgrid=distgrid, &
       name="tst grid",  &
       rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! make sure empty grid returns the right things
  call ESMF_GridGet(grid, &
       distgrid=distgrid2, &
       localDECount=localDECount, &
       name=grid_name, &
       rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Check output
  if (localDECount .ne. 1) correct=.false.
  if (grid_name .ne. "tst grid") correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a 1 DE 3D  Grid with CreateShapeTileIrreg"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/4/), &
                                    countsPerDeDim2=(/5/), &
                                    countsPerDeDim3=(/8/), &
                                    rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_DistGridGet(distgrid2, dimCount=dimCount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (dimcount .ne. 3) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a 1 DE 3D  Grid with GridEmptyCompleteIrreg"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridEmptyCreate(rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_GridEmptyComplete(grid, countsPerDEDim1=(/4/), &
                                    countsPerDeDim2=(/5/), &
                                    countsPerDeDim3=(/8/), &
                                    rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_DistGridGet(distgrid2, dimCount=dimCount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (dimcount .ne. 3) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a 2D distributed Grid with 1 non-distributed dim. with CreateShapeTileIrreg"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2,3,4/), &
                                    countsPerDeDim2=(/3,4,5/), &
                                    countsPerDeDim3=(/6/), &
                                    rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_DistGridGet(distgrid2, dimCount=dimCount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (dimcount .ne. 3) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a Grid with non-default coordDeps"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateNoPeriDim(minIndex=(/1,2,3/), countsPerDEDim1=(/1,2,3,4/), &
                                    countsPerDeDim2=(/5/), &
                                    countsPerDeDim3=(/6,8/), &
                                    coordDep1=(/1/), &
                                    coordDep2=(/3,1,2/), &
                                    coordDep3=(/2,1/), &
                                    rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, coordDimCount=coordDimCount2, coordDimMap=coordDimMap2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.

  if ((coordDimCount2(1) .ne. 1) .or. (coordDimCount2(2) .ne. 3) .or.  &
      (coordDimCount2(3) .ne. 2)) correct=.false.

  if (coordDimMap2(1,1) .ne. 1) correct=.false.
  if ((coordDimMap2(2,1) .ne. 3) .or. (coordDimMap2(2,2) .ne. 1) .or. &
      (coordDimMap2(2,3) .ne. 2)) correct=.false.
  if ((coordDimMap2(3,1) .ne. 2) .or. (coordDimMap2(3,2) .ne. 1)) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a 2D distributed Grid with 1 undistributed dim. with CreateShapeTileIrreg and no-default gridEdgeWidths"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2,3,4/), &
                                    countsPerDeDim2=(/3,4,5/), &
                                    countsPerDeDim3=(/6/), &
                                    gridEdgeLWidth=(/0,1,0/), &
                                    gridEdgeUWidth=(/1,0,0/), &
                                    gridAlign=(/-1,1,1/), &
                                    rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, &
         gridAlign=gridAlign, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if ((gridEdgeLWidth(1) .ne. 0) .or. (gridEdgeLWidth(2) .ne. 1) .or. &
      (gridEdgeLWidth(3) .ne. 0)) correct=.false. 
  if ((gridEdgeUWidth(1) .ne. 1) .or. (gridEdgeUWidth(2) .ne. 0) .or. & 
      (gridEdgeUWidth(3) .ne. 0)) correct=.false. 
  if ((gridAlign(1) .ne. -1) .or. (gridAlign(2) .ne. 1) .or. &
      (gridAlign(3) .ne. 1)) correct=.false. 

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a Grid with default coordDeps"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateNoPeriDim(minIndex=(/1,2,3/), countsPerDEDim1=(/1,2,3,4/), &
                                    countsPerDeDim2=(/5/), &
                                    countsPerDeDim3=(/6,8/), &
                                    rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, coordDimCount=coordDimCount2, coordDimMap=coordDimMap2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.

  if ((coordDimCount2(1) .ne. 3) .or. (coordDimCount2(2) .ne. 3) .or.  &
      (coordDimCount2(3) .ne. 3)) correct=.false.
  if ((coordDimMap2(1,1) .ne. 1) .or. (coordDimMap2(1,2) .ne. 2) .or. &
      (coordDimMap2(1,3) .ne. 3)) correct=.false.
  if ((coordDimMap2(2,1) .ne. 1) .or. (coordDimMap2(2,2) .ne. 2) .or. &
      (coordDimMap2(2,3) .ne. 3)) correct=.false.
  if ((coordDimMap2(3,1) .ne. 1) .or. (coordDimMap2(3,2) .ne. 2) .or. &
      (coordDimMap2(3,3) .ne. 3)) correct=.false.


  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating/Destroying an Empty Grid"
  write(failMsg, *) "Incorrect result"

  ! create empty grid
  rc=ESMF_SUCCESS
  grid=ESMF_GridEmptyCreate(rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a 2D non periodic Grid with GridCreateNoPeriDimReg"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateNoPeriDim(maxIndex=(/20,20/), &
                                    rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_DistGridGet(distgrid2, dimCount=dimCount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (dimcount .ne. 2) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a 2D 1 periodic Grid with GridCreate1PeriDimReg"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreate1PeriDim(maxIndex=(/20,20/), &
                                    rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_DistGridGet(distgrid2, dimCount=dimCount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (dimcount .ne. 2) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a 2D 2 periodic Grid with GridCreate2PeriDimReg"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreate2PeriDim(maxIndex=(/20,20/), &
                                    rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_DistGridGet(distgrid2, dimCount=dimCount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (dimcount .ne. 2) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a 2D  Grid with default edge connections with GridCreateReg()"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreate(maxIndex=(/20,20/), &
                                    rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_DistGridGet(distgrid2, dimCount=dimCount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (dimcount .ne. 2) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a 2D  Grid with sphere edge connections with GridCreateReg()"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreate(maxIndex=(/20,20/), &
                       connflagDim1=(/ESMF_GRIDCONN_PERIODIC,ESMF_GRIDCONN_PERIODIC/), &
                       connflagDim2=(/ESMF_GRIDCONN_POLE,ESMF_GRIDCONN_POLE/), &
                                    rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_DistGridGet(distgrid2, dimCount=dimCount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (dimcount .ne. 2) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a 2D  Grid with all defaults with CreateShapeTileReg"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateNoPeriDim(maxIndex=(/4,2/),rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_DistGridGet(distgrid2, dimCount=dimCount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (dimcount .ne. 2) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a 2D  Grid with non-default regDecomp with CreateShapeTileReg"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateNoPeriDim(maxIndex=(/4,2/),regDecomp=(/1,2/), &
                              rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_DistGridGet(distgrid2, dimCount=dimCount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (dimcount .ne. 2) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a 2D  Grid with non-default 2D regDecomp with CreateShapeTileReg"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateNoPeriDim(maxIndex=(/4,2/),regDecomp=(/2,2/),rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_DistGridGet(distgrid2, dimCount=dimCount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (dimcount .ne. 2) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a 2D  1 DE Grid with CreateShapeTileReg"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateNoPeriDim(maxIndex=(/4,2/), regDecomp=(/1,1/), rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_DistGridGet(distgrid2, dimCount=dimCount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (dimcount .ne. 2) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a Grid with CreateShapeTileReg and non-default gridEdgeWidths"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateNoPeriDim(minIndex=(/1,2,3/), &
                                maxIndex=(/3,4,5/), &
                                gridEdgeLWidth=(/0,1,0/), &
                                gridEdgeUWidth=(/1,0,0/), &
                                gridAlign=(/-1,1,1/), &
                                rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, &
         gridAlign=gridAlign, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if ((gridEdgeLWidth(1) .ne. 0) .or. (gridEdgeLWidth(2) .ne. 1) .or. &
      (gridEdgeLWidth(3) .ne. 0)) correct=.false. 
  if ((gridEdgeUWidth(1) .ne. 1) .or. (gridEdgeUWidth(2) .ne. 0) .or. & 
      (gridEdgeUWidth(3) .ne. 0)) correct=.false. 
  if ((gridAlign(1) .ne. -1) .or. (gridAlign(2) .ne. 1) .or. &
      (gridAlign(3) .ne. 1)) correct=.false. 

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a 3D  Grid with non-default minIndex and non-default regDecomp with CreateShapeTileReg"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateNoPeriDim(minIndex=(/1,2,3/),maxIndex=(/4,5,6/),regDecomp=(/2,1,2/), &
        rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_DistGridGet(distgrid2, dimCount=dimCount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (dimcount .ne. 3) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a Grid using EmptyCreate/Set/Commit with only a distgrid and the rest defaults"
  write(failMsg, *) "Incorrect result"
  rc=ESMF_SUCCESS

  ! create a grid with all defaults
  rc=ESMF_SUCCESS
  grid=ESMF_GridEmptyCreate(rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_GridSet(grid=grid, distgrid=distgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_GridCommit(grid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info from Grid
  call ESMF_GridGet(grid, dimCount=dimCount, coordTypeKind=typekind, &
         distgridToGridMap=distgridToGridMap, coordDimCount=coordDimCount, coordDimMap=coordDimMap, &
         indexflag=indexflag, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that defaults are as expected
  correct=.true.
  if (typekind .ne. ESMF_TYPEKIND_R8) correct=.false.
  if (dimCount .ne. 2) correct=.false.
  if ((distgridToGridMap(1) .ne. 1) .or. (distgridToGridMap(2) .ne. 2)) correct=.false.
  !TODO: what to do about undistLBound and undistUBound
  if ((coordDimCount(1) .ne. 2) .or. (coordDimCount(2) .ne. 2)) correct=.false.
  if ((coordDimMap(1,1) .ne. 1) .or. (coordDimMap(1,2) .ne. 2) .or. & 
      (coordDimMap(2,1) .ne. 1) .or. (coordDimMap(2,2) .ne. 2)) correct=.false.
!  if (indexflag .ne. ESMF_INDEX_DELOCAL) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a 2D  Grid with all defaults with GridEmptyCompleteReg"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  
  ! Create Empty Grid
  grid=ESMF_GridEmptyCreate(rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Set information
  call ESMF_GridEmptyComplete(grid, maxIndex=(/4,2/), rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_DistGridGet(distgrid2, dimCount=dimCount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (dimcount .ne. 2) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test getting minIndex and maxIndex for 2D Grid"
  write(failMsg, *) "Incorrect result"

  ! init output flags
  rc=ESMF_SUCCESS
  correct=.true.

  ! create grid 

  grid=ESMF_GridCreateNoPeriDim(minIndex=(/1,2/), maxIndex=(/4,3/), rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, tile=1, staggerloc=ESMF_STAGGERLOC_CENTER, &
           minIndex=minIndex, maxIndex=maxIndex, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  if (minIndex(1) .ne. 1)  correct=.false.
  if (minIndex(2) .ne. 2)  correct=.false.

  if (maxIndex(1) .ne. 4)  correct=.false.
  if (maxIndex(2) .ne. 3)  correct=.false.

  ! get info back from grid
  call ESMF_GridGet(grid, tile=1, staggerloc=ESMF_STAGGERLOC_CORNER, &
           minIndex=minIndex, maxIndex=maxIndex, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  if (minIndex(1) .ne. 1)  correct=.false.
  if (minIndex(2) .ne. 2)  correct=.false.

  if (maxIndex(1) .ne. 5)  correct=.false.
  if (maxIndex(2) .ne. 4)  correct=.false.

  ! get info back from grid
  call ESMF_GridGet(grid, tile=1, staggerloc=ESMF_STAGGERLOC_EDGE1, &
           minIndex=minIndex, maxIndex=maxIndex, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  if (minIndex(1) .ne. 1)  correct=.false.
  if (minIndex(2) .ne. 2)  correct=.false.

  if (maxIndex(1) .ne. 5)  correct=.false.
  if (maxIndex(2) .ne. 3)  correct=.false.

  ! get info back from grid
  call ESMF_GridGet(grid, tile=1, staggerloc=ESMF_STAGGERLOC_EDGE2, &
           minIndex=minIndex, maxIndex=maxIndex, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  if (minIndex(1) .ne. 1)  correct=.false.
  if (minIndex(2) .ne. 2)  correct=.false.

  if (maxIndex(1) .ne. 4)  correct=.false.
  if (maxIndex(2) .ne. 4)  correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test getting minIndex and maxIndex for 3D Grid"
  write(failMsg, *) "Incorrect result"

  ! init output flags
  rc=ESMF_SUCCESS
  correct=.true.

  ! create grid 

  grid=ESMF_GridCreateNoPeriDim(minIndex=(/1,2,3/), maxIndex=(/4,5,6/), &
       gridEdgeLWidth=(/1,1,1/), gridEdgeUWidth=(/0,0,0/), rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, tile=1, staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, &
           minIndex=minIndex, maxIndex=maxIndex, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  if (minIndex(1) .ne. 1)  correct=.false.
  if (minIndex(2) .ne. 2)  correct=.false.
  if (minIndex(3) .ne. 3)  correct=.false.

  if (maxIndex(1) .ne. 4)  correct=.false.
  if (maxIndex(2) .ne. 5)  correct=.false.
  if (maxIndex(3) .ne. 6)  correct=.false.

  ! get info back from grid
  call ESMF_GridGet(grid, tile=1, staggerloc=ESMF_STAGGERLOC_CENTER_VFACE, &
           minIndex=minIndex, maxIndex=maxIndex, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  if (minIndex(1) .ne. 1)  correct=.false.
  if (minIndex(2) .ne. 2)  correct=.false.
  if (minIndex(3) .ne. 2)  correct=.false.

  if (maxIndex(1) .ne. 4)  correct=.false.
  if (maxIndex(2) .ne. 5)  correct=.false.
  if (maxIndex(3) .ne. 6)  correct=.false.

  ! get info back from grid
  call ESMF_GridGet(grid, tile=1, staggerloc=ESMF_STAGGERLOC_EDGE1_VFACE, &
           minIndex=minIndex, maxIndex=maxIndex, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  if (minIndex(1) .ne. 0)  correct=.false.
  if (minIndex(2) .ne. 2)  correct=.false.
  if (minIndex(3) .ne. 2)  correct=.false.

  if (maxIndex(1) .ne. 4)  correct=.false.
  if (maxIndex(2) .ne. 5)  correct=.false.
  if (maxIndex(3) .ne. 6)  correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test ArrayCreateFromGrid with only distributed bounds"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS
  correct=.true.

  ! create grid
  grid=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2/),    &
                                countsPerDeDim2=(/3,4/),    &
                                indexflag=ESMF_INDEX_GLOBAL,&
                                rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Create an array on an unallocated stagger location
  array=ESMF_ArrayCreateFromGrid(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
          gridToArrayMap=(/2,1/), rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! make sure array is valid
  call ESMF_ArrayValidate(array,rc=localrc)  
  if (localrc .ne. ESMF_SUCCESS) correct=.false.

  ! Get array info and make sure its correct
  call ESMF_ArrayGet(array, distgridToArrayMap=distgridToArrayMap, &
                     rank=rank, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Make sure info is as expected
  if ((distgridToArrayMap(1) .ne. 2) .or. (distgridToArrayMap(2) .ne. 1)) correct=.false.
  if (rank .ne. 2) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! destroy grid
  call ESMF_ArrayDestroy(array, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
#ifndef ESMF_NO_GREATER_THAN_4D
  
  write(name, *) "Test ArrayCreateFromGrid with non-default ungriddedBounds"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS
  correct=.true.

  ! create grid
  grid=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/5/),  & 
                              countsPerDeDim3=(/3,4/),  &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Create an array on an unallocated stagger location
  array=ESMF_ArrayCreateFromGrid(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
          gridToArrayMap=(/1,4,3/), ungriddedLBound=(/2,3/), ungriddedUBound=(/7,6/), &
          rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! make sure array is valid
  call ESMF_ArrayValidate(array,rc=localrc)  
  if (localrc .ne. ESMF_SUCCESS) correct=.false.

  ! Get array info and make sure its correct
  call ESMF_ArrayGet(array,distgridToArrayMap=distgridToArrayMap, &
         undistLBound=undistLBound, undistUBound=undistUBound, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Make sure info is as expected
  if ((distgridToArrayMap(1) .ne. 1) .or. &
      (distgridToArrayMap(2) .ne. 4) .or. &
      (distgridToArrayMap(3) .ne. 3)) correct=.false.
  if ((undistLBound(1) .ne. 2) .or. (undistLBound(2) .ne. 3)) correct=.false.
  if ((undistUBound(1) .ne. 7) .or. (undistUBound(2) .ne. 6)) correct=.false.


  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! destroy grid
  call ESMF_ArrayDestroy(array, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  
#else
  rc = ESMF_SUCCESS
  write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test ArrayCreateFromGrid with replicated dims"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS
  correct=.true.

  ! create grid
  grid=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/5/),  & 
                              countsPerDeDim3=(/3,4/),  &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Create an array on an unallocated stagger location
  array=ESMF_ArrayCreateFromGrid(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
          gridToArrayMap=(/0,4,3/), ungriddedLBound=(/2,3/), ungriddedUBound=(/7,6/), &
          totalLwidth=(/1,1/), totalUwidth=(/1,1/), rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! make sure array is valid
  call ESMF_ArrayValidate(array,rc=localrc)  
  if (localrc .ne. ESMF_SUCCESS) correct=.false.

  ! Get array info and make sure its correct
  call ESMF_ArrayGet(array,rank=rank,distgridToArrayMap=distgridToArrayMap, &
         undistLBound=undistLBound, undistUBound=undistUBound, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Make sure info is as expected
  if (rank .ne. 4) correct=.false.
  if ((distgridToArrayMap(1) .ne. 0) .or. &
      (distgridToArrayMap(2) .ne. 4) .or. &
      (distgridToArrayMap(3) .ne. 3)) correct=.false.
  if ((undistLBound(1) .ne. 2) .or. (undistLBound(2) .ne. 3)) correct=.false.
  if ((undistUBound(1) .ne. 7) .or. (undistUBound(2) .ne. 6)) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! destroy grid
  call ESMF_ArrayDestroy(array, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test GridGetArrayInfo with replicated dims"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS
  correct=.true.

  ! create grid
  grid=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/5/),  & 
                              countsPerDeDim3=(/3,4/),  &
                              gridEdgeLWidth=(/1,2,3/), &
                              gridEdgeUWidth=(/4,5,6/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get array info
  call ESMF_GridGetArrayInfo(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
          gridToFieldMap=(/0,3,0/), ungriddedLBound=(/2,3/), ungriddedUBound=(/7,6/), &
          distgridToArrayMap=distgridToArrayMap, &
          undistLBound=undistLBound, undistUBound=undistUBound, &
          rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Make sure info is as expected
  if ((distgridToArrayMap(1) .ne. 0) .or. &
      (distgridToArrayMap(2) .ne. 3) .or. &
      (distgridToArrayMap(3) .ne. 0)) correct=.false. 

  if ((undistLBound(1) .ne. 2) .or. (undistLBound(2) .ne. 3)) correct=.false.
  if ((undistUBound(1) .ne. 7) .or. (undistUBound(2) .ne. 6)) correct=.false.


  ! Get array info
  call ESMF_GridGetArrayInfo(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
          gridToFieldMap=(/3,1,0/), ungriddedLBound=(/2,3/), ungriddedUBound=(/7,6/), &
          distgridToArrayMap=distgridToArrayMap, &
          undistLBound=undistLBound, undistUBound=undistUBound, &
          rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Make sure info is as expected
  if ((distgridToArrayMap(1) .ne. 3) .or. &
      (distgridToArrayMap(2) .ne. 1) .or. &
      (distgridToArrayMap(3) .ne. 0)) correct=.false. 


  if ((undistLBound(1) .ne. 2) .or. (undistLBound(2) .ne. 3)) correct=.false.
  if ((undistUBound(1) .ne. 7) .or. (undistUBound(2) .ne. 6)) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test GridGetArrayInfo with replicated dims and non-default distgridToGridMap"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS
  correct=.true.

  ! create grid
  grid=ESMF_GridCreate(distgrid=distgrid, &
                       gridEdgeLWidth=(/1,2/), &
                       gridEdgeUWidth=(/3,4/), &
                       distgridToGridMap=(/2,1/), &
                       rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get array info
  call ESMF_GridGetArrayInfo(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
          gridToFieldMap=(/0,1/), ungriddedLBound=(/2,3/), ungriddedUBound=(/7,6/), &
          distgridToArrayMap=distgridToArrayMap, &
          undistLBound=undistLBound, undistUBound=undistUBound, &
          rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Make sure info is as expected
  if ((distgridToArrayMap(1) .ne. 1) .or. &
      (distgridToArrayMap(2) .ne. 0)) correct=.false. 

  if ((undistLBound(1) .ne. 2) .or. (undistLBound(2) .ne. 3)) correct=.false.
  if ((undistUBound(1) .ne. 7) .or. (undistUBound(2) .ne. 6)) correct=.false.


  ! Get array info
  call ESMF_GridGetArrayInfo(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
          gridToFieldMap=(/1,0/), ungriddedLBound=(/2,3/), ungriddedUBound=(/7,6/), &
          distgridToArrayMap=distgridToArrayMap, &
          undistLBound=undistLBound, undistUBound=undistUBound, &
          rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Make sure info is as expected
  if ((distgridToArrayMap(1) .ne. 0) .or. &
      (distgridToArrayMap(2) .ne. 1)) correct=.false. 

  if ((undistLBound(1) .ne. 2) .or. (undistLBound(2) .ne. 3)) correct=.false.
  if ((undistUBound(1) .ne. 7) .or. (undistUBound(2) .ne. 6)) correct=.false.


  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test ArrayCreateFromGrid with replicated dims"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS
  correct=.true.

  ! create grid
  grid=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/5/),  & 
                              countsPerDeDim3=(/3,4/),  &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Create an array on an unallocated stagger location
  array=ESMF_ArrayCreateFromGrid(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
          gridToArrayMap=(/0,4,3/), ungriddedLBound=(/2,3/), ungriddedUBound=(/7,6/), &
          rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! make sure array is valid
  call ESMF_ArrayValidate(array,rc=localrc)  
  if (localrc .ne. ESMF_SUCCESS) correct=.false.

  ! Get array info and make sure its correct
  call ESMF_ArrayGet(array,rank=rank,distgridToArrayMap=distgridToArrayMap, &
         undistLBound=undistLBound, undistUBound=undistUBound, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Make sure info is as expected
  if (rank .ne. 4) correct=.false.
  if ((distgridToArrayMap(1) .ne. 0) .or. &
      (distgridToArrayMap(2) .ne. 4) .or. &
      (distgridToArrayMap(3) .ne. 3)) correct=.false.
  if ((undistLBound(1) .ne. 2) .or. (undistLBound(2) .ne. 3)) correct=.false.
  if ((undistUBound(1) .ne. 7) .or. (undistUBound(2) .ne. 6)) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! destroy grid
  call ESMF_ArrayDestroy(array, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test GridGetArrayInfo with replicated dims"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS
  correct=.true.

  ! create grid
  grid=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/5/),  & 
                              countsPerDeDim3=(/3,4/),  &
                              gridEdgeLWidth=(/1,2,3/), &
                              gridEdgeUWidth=(/4,5,6/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get array info
  call ESMF_GridGetArrayInfo(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
          gridToFieldMap=(/0,3,0/), ungriddedLBound=(/2,3/), ungriddedUBound=(/7,6/), &
          distgridToArrayMap=distgridToArrayMap, &
          undistLBound=undistLBound, undistUBound=undistUBound, &
          rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Make sure info is as expected
  if ((distgridToArrayMap(1) .ne. 0) .or. &
      (distgridToArrayMap(2) .ne. 3) .or. &
      (distgridToArrayMap(3) .ne. 0)) correct=.false. 

  if ((undistLBound(1) .ne. 2) .or. (undistLBound(2) .ne. 3)) correct=.false.
  if ((undistUBound(1) .ne. 7) .or. (undistUBound(2) .ne. 6)) correct=.false.


  ! Get array info
  call ESMF_GridGetArrayInfo(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
          gridToFieldMap=(/3,1,0/), ungriddedLBound=(/2,3/), ungriddedUBound=(/7,6/), &
          distgridToArrayMap=distgridToArrayMap, &
          undistLBound=undistLBound, undistUBound=undistUBound, &
          rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Make sure info is as expected
  if ((distgridToArrayMap(1) .ne. 3) .or. &
      (distgridToArrayMap(2) .ne. 1) .or. &
      (distgridToArrayMap(3) .ne. 0)) correct=.false. 


  if ((undistLBound(1) .ne. 2) .or. (undistLBound(2) .ne. 3)) correct=.false.
  if ((undistUBound(1) .ne. 7) .or. (undistUBound(2) .ne. 6)) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test GridGetArrayInfo with replicated dims and non-default distgridToGridMap"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS
  correct=.true.

  ! create grid
  grid=ESMF_GridCreate(distgrid=distgrid, &
                       gridEdgeLWidth=(/1,2/), &
                       gridEdgeUWidth=(/3,4/), &
                       distgridToGridMap=(/2,1/), &
                       rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get array info
  call ESMF_GridGetArrayInfo(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
          gridToFieldMap=(/0,1/), ungriddedLBound=(/2,3/), ungriddedUBound=(/7,6/), &
          distgridToArrayMap=distgridToArrayMap, &
          undistLBound=undistLBound, undistUBound=undistUBound, &
          rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Make sure info is as expected
  if ((distgridToArrayMap(1) .ne. 1) .or. &
      (distgridToArrayMap(2) .ne. 0)) correct=.false. 

  if ((undistLBound(1) .ne. 2) .or. (undistLBound(2) .ne. 3)) correct=.false.
  if ((undistUBound(1) .ne. 7) .or. (undistUBound(2) .ne. 6)) correct=.false.


  ! Get array info
  call ESMF_GridGetArrayInfo(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
          gridToFieldMap=(/1,0/), ungriddedLBound=(/2,3/), ungriddedUBound=(/7,6/), &
          distgridToArrayMap=distgridToArrayMap, &
          undistLBound=undistLBound, undistUBound=undistUBound, &
          rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Make sure info is as expected
  if ((distgridToArrayMap(1) .ne. 0) .or. &
      (distgridToArrayMap(2) .ne. 1)) correct=.false. 

  if ((undistLBound(1) .ne. 2) .or. (undistLBound(2) .ne. 3)) correct=.false.
  if ((undistUBound(1) .ne. 7) .or. (undistUBound(2) .ne. 6)) correct=.false.


  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test Serialize and Deserialize"
  write(failMsg, *) "Incorrect result"

  ! init variables
  rc=ESMF_SUCCESS
  correct=.true.

    ! create a grid with all defaults
  grid=ESMF_GridCreate(distgrid=distgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! create a buffer to put the grid in
  bufCount = 1
  allocate (buf(bufCount))
  offset=0
  call ESMF_GridSerialize(grid, buf, bufCount, offset,  &
    inquireflag=ESMF_INQUIREONLY, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  deallocate (buf)

  bufCount=offset
  print *, 'ESMF_GridCreateUTest: serialization buffer size =', bufCount
  allocate(buf(bufCount))

  ! Serialize
  offset=0
  call ESMF_GridSerialize(grid, buf, bufCount, offset, rc=localrc) 
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Deserialize
   offset=0
   grid2=ESMF_GridDeserialize(buf, offset, rc=localrc) 
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get rid of buffer
  deallocate(buf)

  ! get info from Grid
  call ESMF_GridGet(grid2, dimCount=dimCount, coordTypeKind=typekind, &
         distgridToGridMap=distgridToGridMap, coordDimCount=coordDimCount, coordDimMap=coordDimMap, &
         indexflag=indexflag, &
         gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, &
         gridAlign=gridAlign, localDECount=localDECount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! check that defaults are as expected

  if (typekind .ne. ESMF_TYPEKIND_R8) correct=.false.
  if (dimCount .ne. 2) correct=.false.
  if ((distgridToGridMap(1) .ne. 1) .or. (distgridToGridMap(2) .ne. 2)) correct=.false.
  !TODO: what to do about undistLBound and undistUBound

  if ((coordDimCount(1) .ne. 2) .or. (coordDimCount(2) .ne. 2)) correct=.false.
  if ((coordDimMap(1,1) .ne. 1) .or. (coordDimMap(1,2) .ne. 2) .or. & 
      (coordDimMap(2,1) .ne. 1) .or. (coordDimMap(2,2) .ne. 2)) correct=.false.
!  if (indexflag .ne. ESMF_INDEX_DELOCAL) correct=.false.

  if ((gridEdgeLWidth(1) .ne. 0) .or. (gridEdgeLWidth(2) .ne. 0)) correct=.false. 
  if ((gridEdgeUWidth(1) .ne. 1) .or. (gridEdgeUWidth(2) .ne. 1)) correct=.false. 
  if ((gridAlign(1) .ne. -1) .or. (gridAlign(2) .ne. -1)) correct=.false. 
  if (localDECount .ne. 0) correct=.false. 

  ! destroy grids
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_GridDestroy(grid2,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! report results
  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Checking isLBound and isUBound functionality"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  if (petCount .eq. 4) then
     petMap2D(:,1,1)=(/0,1/)
     petMap2D(:,2,1)=(/2,3/)

     grid=ESMF_GridCreateNoPeriDim(maxIndex=(/8,8/),regDecomp=(/2,2/), &
            petMap=petMap2D,rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  else
     grid=ESMF_GridCreateNoPeriDim(maxIndex=(/8,8/),regDecomp=(/2,2/), &
            rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  endif

  ! init flag
  correct=.true.

  ! get check info
  if (petCount .eq. 1) then
     call ESMF_GridGet(grid, localDE=0, & 
          isLBound=isLBound, isUBound=isUBound, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     if ((.not. isLBound(1)) .or. (.not. isLBound(2))) correct=.false. 
     if ((isUBound(1)) .or. (isUBound(2))) correct=.false. 

     call ESMF_GridGet(grid, localDE=1, & 
          isLBound=isLBound, isUBound=isUBound, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     if ((isLBound(1)) .or. (.not. isLBound(2))) correct=.false. 
     if ((.not. isUBound(1)) .or. (isUBound(2))) correct=.false. 

     call ESMF_GridGet(grid, localDE=2, & 
          isLBound=isLBound, isUBound=isUBound, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     if ((.not. isLBound(1)) .or. (isLBound(2))) correct=.false. 
     if ((isUBound(1)) .or. (.not. isUBound(2))) correct=.false. 

     call ESMF_GridGet(grid, localDE=3, & 
          isLBound=isLBound, isUBound=isUBound, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     if ((isLBound(1)) .or. (isLBound(2))) correct=.false. 
     if ((.not. isUBound(1)) .or. (.not. isUBound(2))) correct=.false. 

   else if (petCount .eq. 4) then
     if (localPet .eq. 0) then
        call ESMF_GridGet(grid, localDE=0, & 
           isLBound=isLBound, isUBound=isUBound, rc=localrc)
        if ((.not. isLBound(1)) .or. (.not. isLBound(2))) correct=.false. 
        if ((isUBound(1)) .or. (isUBound(2))) correct=.false. 

     else if (localPet .eq. 1) then
        call ESMF_GridGet(grid, localDE=0, & 
             isLBound=isLBound, isUBound=isUBound, rc=localrc)
        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

        if ((isLBound(1)) .or. (.not. isLBound(2))) correct=.false. 
        if ((.not. isUBound(1)) .or. (isUBound(2))) correct=.false. 
     else if (localPet .eq. 2) then
        call ESMF_GridGet(grid, localDE=0, & 
             isLBound=isLBound, isUBound=isUBound, rc=localrc)
        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

        if ((.not. isLBound(1)) .or. (isLBound(2))) correct=.false. 
        if ((isUBound(1)) .or. (.not. isUBound(2))) correct=.false. 
     else if (localPet .eq. 3) then

        call ESMF_GridGet(grid, localDE=0, & 
             isLBound=isLBound, isUBound=isUBound, rc=localrc)
        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

        if ((isLBound(1)) .or. (isLBound(2))) correct=.false. 
        if ((.not. isUBound(1)) .or. (.not. isUBound(2))) correct=.false. 
     endif
   endif

  ! destroy grid
!  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test ESMF_GridCreateNoPeriDimUfrm with Cartesian"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS

  ! Create Grid
  grid=ESMF_GridCreateNoPeriDimUfrm(maxIndex=(/12,12/), &
       minCornerCoord=(/0.0_ESMF_KIND_R8,0.0_ESMF_KIND_R8/), &
       maxCornerCoord=(/1.0_ESMF_KIND_R8,1.0_ESMF_KIND_R8/), &
       coordSys=ESMF_COORDSYS_CART, &
       staggerLocList=(/ESMF_STAGGERLOC_CENTER, &
       ESMF_STAGGERLOC_CORNER, &
       ESMF_STAGGERLOC_EDGE1, &
       ESMF_STAGGERLOC_EDGE2/), &
       rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

#if 0
  ! Dump grid staggers to file
  call ESMF_GridWriteVTK(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, filename="ufrmCntGrid", &
                         rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_GridWriteVTK(grid, staggerLoc=ESMF_STAGGERLOC_CORNER, filename="ufrmCnrGrid", &
                         rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

   call ESMF_GridWriteVTK(grid, staggerLoc=ESMF_STAGGERLOC_EDGE1, filename="ufrmEdge1Grid", &
                         rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_GridWriteVTK(grid, staggerLoc=ESMF_STAGGERLOC_EDGE2, filename="ufrmEdge2Grid", &
                         rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
#endif

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test ESMF_GridCreateNoPeriDimUfrm with Default CoordSys (Spherical)"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS

  ! Create Grid
  grid=ESMF_GridCreateNoPeriDimUfrm(maxIndex=(/12,12/), &
       minCornerCoord=(/0.0_ESMF_KIND_R8,0.0_ESMF_KIND_R8/), &
       maxCornerCoord=(/50.0_ESMF_KIND_R8,50.0_ESMF_KIND_R8/), &
       rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


 !call ESMF_GridWriteVTK(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, filename="ufrmGrid", &
 !                        rc=localrc)
 !if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test ESMF_GridCreate1PeriDimUfrm"
  write(failMsg, *) "Incorrect result"
 
  ! init success flag
  rc=ESMF_SUCCESS

  ! Create Grid
  grid=ESMF_GridCreate1PeriDimUfrm(maxIndex=(/12,12/), &
       minCornerCoord=(/-180.0_ESMF_KIND_R8,-80.0_ESMF_KIND_R8/), &
       maxCornerCoord=(/ 180.0_ESMF_KIND_R8,80.0_ESMF_KIND_R8/), &
       staggerLocList=(/ESMF_STAGGERLOC_CENTER, &
       ESMF_STAGGERLOC_CORNER, &
       ESMF_STAGGERLOC_EDGE1, &
       ESMF_STAGGERLOC_EDGE2/), &
       rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

#if 0
  ! Dump grid staggers to file
  call ESMF_GridWriteVTK(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, filename="ufrmCntGrid", &
                         rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_GridWriteVTK(grid, staggerLoc=ESMF_STAGGERLOC_CORNER, filename="ufrmCnrGrid", &
                         rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_GridWriteVTK(grid, staggerLoc=ESMF_STAGGERLOC_EDGE1, filename="ufrmEdge1Grid", &
                         rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_GridWriteVTK(grid, staggerLoc=ESMF_STAGGERLOC_EDGE2, filename="ufrmEdge2Grid", &
                         rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
#endif


  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test getting tile number from localDE"
  write(failMsg, *) "Incorrect result"
 
  ! init flags
  rc=ESMF_SUCCESS
  correct=.true.

  ! Setup index space
  minIndexPTile(:,1)=(/1,1/)
  maxIndexPTile(:,1)=(/20,20/)
  regDecompPTile4(:,1)=(/petCount,1/)

  minIndexPTile(:,2)=(/1,1/)
  maxIndexPTile(:,2)=(/20,20/)
  regDecompPTile4(:,2)=(/petCount,1/)

  minIndexPTile(:,3)=(/1,1/)
  maxIndexPTile(:,3)=(/20,20/)
  regDecompPTile4(:,3)=(/petCount,1/)

  minIndexPTile(:,4)=(/1,1/)
  maxIndexPTile(:,4)=(/20,20/)
  regDecompPTile4(:,4)=(/petCount,1/)

  decompPTile(:,:)=ESMF_DECOMP_BALANCED

  ! Create source distgrid
  distgrid_multi=ESMF_DistgridCreate(minIndexPTile=minIndexPTile, maxIndexPTile=maxIndexPTile, &
                              regDecompPTile=regDecompPTile4, &
                              decompflagPTile=decompPTile,indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! setup source grid
  grid_multi=ESMF_GridCreate(distgrid=distgrid_multi, indexflag=ESMF_INDEX_GLOBAL, &
                          coordSys=ESMF_COORDSYS_CART, &
                          rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get number of localDEs
  call ESMF_GridGet(grid_multi, localDECount=localDECount, rc=localrc) 
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Check number of DE
  if (localDECount .ne. 4) correct=.false.

  ! loop DEs checking tile numbers
  do lDE=0, localDECount-1

     ! Get tile number of localDE
     call ESMF_GridGet(grid_multi, localDE=lDE, tile=tile, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     ! Check tile number
     if (tile .ne. (lDE+1)) correct=.false.
  enddo  

  ! destroy grid
  call ESMF_GridDestroy(grid_multi,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! destroy distgrid
  call ESMF_DistgridDestroy(distgrid_multi,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing GridCreateCubedSphere"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS

  ! Set up decomposition for src Grid
  regDecompPTile(:,1)=(/2,2/)
  regDecompPTile(:,2)=(/2,2/)
  regDecompPTile(:,3)=(/2,2/)
  regDecompPTile(:,4)=(/2,2/)
  regDecompPTile(:,5)=(/2,2/)
  regDecompPTile(:,6)=(/2,2/)

  !decompFlagPTile(:,1)=(/ESMF_DECOMP_CYCLIC,  1/)
  !decompFlagPTile(:,2)=(/ESMF_DECOMP_BALANCED, 2/)
  !decompFlagPTile(:,3)=(/ESMF_DECOMP_RESTFIRST,3/)
  !decompFlagPTile(:,4)=(/ESMF_DECOMP_RESTLAST, 4/)
  !decompFlagPTile(:,5)=(/ESMF_DECOMP_CYCLIC,   5/)
  !decompFlagPTile(:,6)=(/ESMF_DECOMP_BALANCED,  6/)

  deLabelList(1) = 11
  deLabelList(1) = 12
  deLabelList(1) = 13
  deLabelList(1) = 14
  deLabelList(1) = 15
  deLabelList(1) = 16

  grid=ESMF_GridCreateCubedSphere(15, regDecompPTile=regDecompPTile, &
                                  !decompFlagPTile=decompFlagPTile, &
                                  !deLabelList=deLabelList, &
                                  staggerLocList = (/ESMF_STAGGERLOC_CENTER/), &
                                  rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_GridGet(grid, localDECount=localDECount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  do lde = 0, localDECount-1

    call ESMF_GridGetCoord(grid, coordDim=1, localDE=lde, farrayPtr=fptr1, &
                           exclusiveLBound=exlbnd, exclusiveUBound=exubnd, rc=localrc)
    if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
    call ESMF_GridGetCoord(grid, coordDim=2, localDE=lde, farrayPtr=fptr2, &
                           exclusiveLBound=exlbnd, exclusiveUBound=exubnd, rc=localrc)
    if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

#if 0
    print *, "coords from de", lde

    print *, "lower bounds = [", exlbnd(1), ", ", exlbnd(2), "]"
    print *, "upper bounds = [", exubnd(1), ", ", exubnd(2), "]"

    print *, "["
    do j = 1, exubnd(1)
      do i = 1, exubnd(2)
        print *, "[", fptr1(i,j), ", ", fptr2(i,j), "]"
      enddo
    enddo
    print *, "]"
#endif
  enddo

  ! destroy grid
  call ESMF_GridDestroy(grid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing GridCreateCubedSphere with SchmidtTransform"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS

  transformArgs%stretch_factor = 3.0;
  transformArgs%target_lat = 0.0; ! in radians
  transformArgs%target_lat = 1.3; ! in radians
  grid=ESMF_GridCreateCubedSphere(15, &
                                  staggerLocList = (/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
                                  coordTypeKind = ESMF_TYPEKIND_R4, &
                                  coordSys = ESMF_COORDSYS_SPH_RAD, &
                                  transformArgs=transformArgs, &
                                  rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_GridGet(grid, localDECount=localDECount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  do lde = 0, localDECount-1

    call ESMF_GridGetCoord(grid, coordDim=1, localDE=lde, farrayPtr=fptr1R4, &
                           exclusiveLBound=exlbnd, exclusiveUBound=exubnd, rc=localrc)
    if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
    call ESMF_GridGetCoord(grid, coordDim=2, localDE=lde, farrayPtr=fptr2R4, &
                           exclusiveLBound=exlbnd, exclusiveUBound=exubnd, rc=localrc)
    if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

#if 0
    print *, "coords from de", lde

    print *, "lower bounds = [", exlbnd(1), ", ", exlbnd(2), "]"
    print *, "upper bounds = [", exubnd(1), ", ", exubnd(2), "]"

    print *, "["
    do j = 1, exubnd(1)
      do i = 1, exubnd(2)
        print *, "[", fptr1(i,j), ", ", fptr2(i,j), "]"
      enddo
    enddo
    print *, "]"
#endif
  enddo

  ! destroy grid
  call ESMF_GridDestroy(grid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing PetMap with non-sequential order and >1 DE on some PETs"
  write(failMsg, *) "Incorrect result"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  if (petCount .eq. 4) then
     petMap2x3(1,1,1)=0
     petMap2x3(1,2,1)=2
     petMap2x3(1,3,1)=3
     petMap2x3(2,1,1)=0
     petMap2x3(2,2,1)=1
     petMap2x3(2,3,1)=1

     grid=ESMF_GridCreate1PeriDim(maxIndex=(/10,10/),regDecomp=(/2,3/), &
            petMap=petMap2x3,rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  else
     grid=ESMF_GridCreate1PeriDim(maxIndex=(/10,10/),regDecomp=(/2,3/), &
            rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  endif

  ! Get local DE Count
  call ESMF_GridGet(grid, localDECount=localDECount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! init flag
  correct=.true.

  ! get check info
  if (petCount .eq. 1) then
     if (localDECount .ne. 6) correct=.false. 
   else if (petCount .eq. 4) then
     if (localPet .eq. 0) then
     if (localDECount .ne. 2) correct=.false. 
     else if (localPet .eq. 1) then
     if (localDECount .ne. 2) correct=.false. 
     else if (localPet .eq. 2) then
     if (localDECount .ne. 1) correct=.false. 
     else if (localPet .eq. 3) then
     if (localDECount .ne. 1) correct=.false. 
     endif
   endif

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing GridCreateMosaic with different CoordTypeKind "
  write(failMsg, *) "Returns incorrect results"

  rc = ESMF_SUCCESS

  staggerLocList(1) = ESMF_STAGGERLOC_CENTER
  staggerLocList(2) = ESMF_STAGGERLOC_CORNER
  threshhold = 1.0E-5
  ! Create cubed sphere grid with coordTypeKind == ESMF_TYPEKIND_R4
  grid = ESMF_GridCreateMosaic(filename='data/C48_mosaic.nc', &
                staggerLocList= staggerLocList, &
                coordTypeKind = ESMF_TYPEKIND_R4, &
                tileFilePath='./data/', rc=localrc)

#ifndef ESMF_NETCDF
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((localrc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE) 
#else
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_GridGet(grid, distgrid = distgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_DistGridGet(distgrid, delayout = delayout, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
 
  call ESMF_DELayoutGet(delayout, localDeCount = decount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Create cubed sphere grid with coordTypeKind == ESMF_TYPEKIND_R8
  grid2 = ESMF_GridCreateMosaic(filename='data/C48_mosaic.nc', &
                staggerLocList= staggerLocList, &
                coordTypeKind = ESMF_TYPEKIND_R8, &
                tileFilePath='./data/', rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  do s = 1, 2
    do localDe = 0, decount-1  
      call ESMF_GridGetCoord(grid2, coordDim=1, localDe=localDe, &
         staggerloc=staggerLocList(s), farrayPtr=lonPtrR8, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
      call ESMF_GridGetCoord(grid2, coordDim=2, localDe=localDe, &
         staggerloc=staggerLocList(s), farrayPtr=latPtrR8, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
      call ESMF_GridGetCoord(grid, coordDim=1, localDe=localDe, &
       staggerloc=staggerLocList(s), farrayPtr=lonPtrR4, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
      call ESMF_GridGetCoord(grid, coordDim=2, localDe=localDe, &
         staggerloc=staggerLocList(s), farrayPtr=latPtrR4, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      allocate(lonDiff(size(lonPtrR8,1), size(lonPtrR8,2)))
      allocate(latDiff(size(lonPtrR8,1), size(lonPtrR8,2)))
      total = size(lonPtrR8,1)*size(lonPtrR8,2)
      lonDiff = abs(lonPtrR8-lonPtrR4)
      latDiff = abs(latPtrR8-latPtrR4)

      ! Find the max/min/mean errors
      ! lonmin = minval(lonDiff)
      ! latmin = minval(latDiff)
      ! lonmax = maxval(lonDiff)
      ! latmax = maxval(latDiff)
      allocate(mean(size(lonDiff,2)))
      do  j=1, size(lonDiff,2)
         mean(j) = sum(lonDiff(:,j))
      enddo
      lonmean = sum(mean)/total
      do j=1, size(latDiff,2)
         mean(j) = sum(latDiff(:,j))
      enddo
      latmean = sum(mean)/total
      
      deallocate(lonDiff, latDiff, mean)

      !print *, localPet, localDe, 'min/max/mean:', lonmin, latmin, lonmax, latmax, lonmean, latmean
      if (lonmean > threshhold .or. latmean > threshhold) rc = ESMF_FAILURE
    enddo
  enddo

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_GridDestroy(grid2,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#endif

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test ESMF_GridCreate with different coordTypeKind with GRIDSPEC supergrid tile file"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  grid=ESMF_GridCreate('data/horizontal_grid.tile6.nc', &
    ESMF_FILEFORMAT_GRIDSPEC, coordTypeKind=ESMF_TYPEKIND_R4, rc=rc)

#ifdef ESMF_NETCDF

  call ESMF_GridGet(grid, distgrid = distgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_DistGridGet(distgrid, delayout = delayout, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
 
  call ESMF_DELayoutGet(delayout, localDeCount = decount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  grid2=ESMF_GridCreate('data/horizontal_grid.tile6.nc', &
    ESMF_FILEFORMAT_GRIDSPEC, coordTypeKind=ESMF_TYPEKIND_R8, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  
  threshhold = 1.0E-5

  do localDe = 0, decount-1  
      call ESMF_GridGetCoord(grid2, coordDim=1, localDe=localDe, &
         staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=lonPtrR8, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
      call ESMF_GridGetCoord(grid2, coordDim=2, localDe=localDe, &
         staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=latPtrR8, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
      call ESMF_GridGetCoord(grid, coordDim=1, localDe=localDe, &
       staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=lonPtrR4, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
      call ESMF_GridGetCoord(grid, coordDim=2, localDe=localDe, &
         staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=latPtrR4, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      allocate(lonDiff(size(lonPtrR8,1), size(lonPtrR8,2)))
      allocate(latDiff(size(lonPtrR8,1), size(lonPtrR8,2)))
      total = size(lonPtrR8,1)*size(lonPtrR8,2)
      lonDiff = abs(lonPtrR8-lonPtrR4)
      latDiff = abs(latPtrR8-latPtrR4)

      ! Find the mean errors
      allocate(mean(size(lonDiff,2)))
      do  j=1, size(lonDiff,2)
         mean(j) = sum(lonDiff(:,j))
      enddo
      lonmean = sum(mean)/total
      do j=1, size(latDiff,2)
         mean(j) = sum(latDiff(:,j))
      enddo
      latmean = sum(mean)/total
      
      deallocate(lonDiff, latDiff, mean)

      if (lonmean > threshhold .or. latmean > threshhold) rc = ESMF_FAILURE
  enddo

  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_GridDestroy(grid2,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE) 
#endif

  call ESMF_TestEnd(ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

end program ESMF_GridCreateUTest
