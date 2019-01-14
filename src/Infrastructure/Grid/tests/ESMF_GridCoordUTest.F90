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
program ESMF_GridCoordUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_GridCoordUTest - Check Grid Coordinate manipulation routines
!
! !DESCRIPTION:
!
! The code in this file drives F90 Grid Coord unit tests.
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
  integer :: localrc, rc, petCount,localPet

  ! individual test failure message
  character(ESMF_MAXSTR) :: name, failMsg

  logical :: correct
  type(ESMF_TypeKind_Flag) :: typekind
  type(ESMF_Grid) :: grid2D,grid3D, gridA, gridB
  type(ESMF_VM) :: vm
  type(ESMF_DistGrid) :: distgrid2D, distgrid3D,tmpDistgrid
  type(ESMF_Array) :: array, array2D, array2, array1D
  type(ESMF_ArraySpec) :: arrayspec2D,arrayspec1D
  type(ESMF_StaggerLoc) :: customStagger
  real(ESMF_KIND_R8), pointer :: farrayPtrX(:,:),farrayPtrY(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:), farrayPtr3D(:,:,:)
  real(kind=ESMF_KIND_R4), parameter :: var=1.0
  integer :: petMap2D(2,2,1)
  integer :: petMapReg2D(2,1,2)
  integer :: compELWidth(3),compEUWidth(3)
  integer :: rank,clbnd(3),cubnd(3)
  integer :: i,i1,i2,i3, index(3)
  integer :: lDE, localDECount,t, loc
  real(ESMF_KIND_R8) :: coord(3)
  character(len=ESMF_MAXSTR) :: string

  INTEGER, PARAMETER :: globalXcount = 5 
  INTEGER, PARAMETER :: globalYcount = 5 
  REAL(ESMF_KIND_R8) :: cornerX(globalXcount+1)
  REAL(ESMF_KIND_R8) :: cornerY(globalYcount+1)
  REAL(ESMF_KIND_R8),pointer :: farrayPtr1D(:)
  integer :: staggerEdgeLWidth(2)
  integer :: staggerEdgeUWidth(2)
  integer :: staggerAlign(2)
  integer :: staggerLBound(2)
  logical :: isPresent

  !-----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !-----------------------------------------------------------------------------

  ! get global VM
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! prepare 2D DistGrid
  distgrid2D=ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,10/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! prepare 3D DistGrid
  distgrid3D=ESMF_DistGridCreate(minIndex=(/1,1,1/), maxIndex=(/10,10,10/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! create 3D test Grid
  grid3D=ESMF_GridCreate(distgrid=distgrid3D, coordTypeKind=ESMF_TYPEKIND_R8, &
         indexflag=ESMF_INDEX_GLOBAL, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! set arrayspec
  call ESMF_ArraySpecSet(arrayspec2D, rank=2, typekind=ESMF_TYPEKIND_R8, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! set arrayspec
  call ESMF_ArraySpecSet(arrayspec1D, rank=1, typekind=ESMF_TYPEKIND_R8, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test Grid Coord isPresent"
  write(failMsg, *) "Incorrect result"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! Create Grid 
  gridA=ESMF_GridCreateNoPeriDim(maxIndex=(/20,20/), regDecomp=(/2,2/), rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Add coords on the center
  call ESMF_GridAddCoord(gridA, staggerloc=ESMF_STAGGERLOC_CENTER, &
         rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get isPresent, for coord that is present
  call ESMF_GridGetCoord(gridA, staggerloc=ESMF_STAGGERLOC_CENTER, &
         isPresent=isPresent, rc=localrc)

  ! Check answer
  if (.not. isPresent) correct=.false.

  ! Get isPresent, for coord that is NOT present
  call ESMF_GridGetCoord(gridA, staggerloc=ESMF_STAGGERLOC_CORNER, &
         isPresent=isPresent, rc=localrc)

  ! Check answer
  if (isPresent) correct=.false.

  ! get rid of first grid
  call ESMF_GridDestroy(gridA,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test get staggerEdgeLWidth, staggerEdgeUWidth, staggerAlign, and staggerLBound"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS

  ! Create grid
  grid2D=ESMF_GridCreateNoPeriDim(maxIndex=(/20,20/), &
       gridEdgeLWidth=(/1,2/), gridEdgeUWidth=(/3,4/), &
       rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Add Coords with stagger info
  call ESMF_GridAddCoord(grid2D, staggerloc=ESMF_STAGGERLOC_CENTER, &
       staggerEdgeLWidth=(/1,2/), staggerEdgeUWidth=(/3,4/), &
       staggerAlign=(/-1,0/), staggerLBound=(/5,6/), &
       rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get stagger info
  call ESMF_GridGet(grid2D, staggerloc=ESMF_STAGGERLOC_CENTER, &
       staggerEdgeLWidth=staggerEdgeLWidth, staggerEdgeUWidth=staggerEdgeUWidth, &
       staggerAlign=staggerAlign, staggerLBound=staggerLBound, &
       rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Make sure that info matches
  correct=.true.
  if ((staggerEdgeLWidth(1) .ne. 1) .or. (staggerEdgeLWidth(2) .ne. 2)) correct=.false.
  if ((staggerEdgeUWidth(1) .ne. 3) .or. (staggerEdgeUWidth(2) .ne. 4)) correct=.false.
  if ((staggerAlign(1) .ne. -1) .or. (staggerAlign(2) .ne. 0)) correct=.false.
  if ((staggerLBound(1) .ne. 5) .or. (staggerLBound(2) .ne. 6)) correct=.false.

  ! Destroy grid
  call ESMF_GridDestroy(grid2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test ESMF_GridMatch() on Grids with the same coordinates"
  write(failMsg, *) "Incorrect result"

  ! init flags
  rc=ESMF_SUCCESS
  correct=.true.

  ! Create Grid with globalXCountxglobalYCount cells
  gridA=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/10,10/), &
                                  indexflag=ESMF_INDEX_GLOBAL,         &
                                  rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get number of local DEs
  call ESMF_GridGet(gridA, localDECount=localDECount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Allocate Center (e.g. Center) stagger
  call ESMF_GridAddCoord(gridA, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Loop through DEs and set Centers as the average of the corners
  do lDE=0,localDECount-1  

     ! get and fill first coord array
     call ESMF_GridGetCoord(gridA, localDE=lDE,  staggerloc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                         computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrX, &
                         rc=localrc)           
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

    ! get and fill second coord array
    call ESMF_GridGetCoord(gridA, localDE=lDE, staggerloc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                           farrayPtr=farrayPtrY, rc=localrc)           
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        farrayPtrX(i1,i2)=REAL(i1,ESMF_KIND_R8)
        farrayPtrY(i1,i2)=REAL(i2,ESMF_KIND_R8)
     enddo
     enddo

  enddo

  ! Create Grid with globalXCountxglobalYCount cells
  gridB=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/10,10/), &
                                  indexflag=ESMF_INDEX_GLOBAL,         &
                                  rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get number of local DEs
  call ESMF_GridGet(gridB, localDECount=localDECount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Allocate Center (e.g. Center) stagger
  call ESMF_GridAddCoord(gridB, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Loop through DEs and set Centers as the average of the corners
  do lDE=0,localDECount-1  

     ! get and fill first coord array
     call ESMF_GridGetCoord(gridB, localDE=lDE,  staggerloc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                         computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrX, &
                         rc=localrc)           
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

    ! get and fill second coord array
    call ESMF_GridGetCoord(gridB, localDE=lDE, staggerloc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                           farrayPtr=farrayPtrY, rc=localrc)           
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        farrayPtrX(i1,i2)=REAL(i1,ESMF_KIND_R8)
        farrayPtrY(i1,i2)=REAL(i2,ESMF_KIND_R8)
     enddo
     enddo

  enddo


  ! Check Grid Match
  ! (it should pass)
  if (ESMF_GridMatch(gridA, gridB, rc=localrc)/=ESMF_GRIDMATCH_EXACT) correct=.false.
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Destroy Test Grids
  call ESMF_GridDestroy(gridA, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_GridDestroy(gridB, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test ESMF_GridMatch() on Grids with different coordinates"
  write(failMsg, *) "Incorrect result"

  ! init flags
  rc=ESMF_SUCCESS
  correct=.true.

  ! Create Grid with globalXCountxglobalYCount cells
  gridA=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/10,10/), &
                                  indexflag=ESMF_INDEX_GLOBAL,         &
                                  rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get number of local DEs
  call ESMF_GridGet(gridA, localDECount=localDECount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Allocate Center (e.g. Center) stagger
  call ESMF_GridAddCoord(gridA, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Loop through DEs and set Centers as the average of the corners
  do lDE=0,localDECount-1  

     ! get and fill first coord array
     call ESMF_GridGetCoord(gridA, localDE=lDE,  staggerloc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                         computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrX, &
                         rc=localrc)           
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

    ! get and fill second coord array
    call ESMF_GridGetCoord(gridA, localDE=lDE, staggerloc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                           farrayPtr=farrayPtrY, rc=localrc)           
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        farrayPtrX(i1,i2)=REAL(i1,ESMF_KIND_R8)
        farrayPtrY(i1,i2)=REAL(i2,ESMF_KIND_R8)
     enddo
     enddo

  enddo

  ! Create Grid with globalXCountxglobalYCount cells
  gridB=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/10,10/), &
                                  indexflag=ESMF_INDEX_GLOBAL,         &
                                  rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get number of local DEs
  call ESMF_GridGet(gridB, localDECount=localDECount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Allocate Center (e.g. Center) stagger
  call ESMF_GridAddCoord(gridB, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Loop through DEs and set Centers as the average of the corners
  do lDE=0,localDECount-1  

     ! get and fill first coord array
     call ESMF_GridGetCoord(gridB, localDE=lDE,  staggerloc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                         computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrX, &
                         rc=localrc)           
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

    ! get and fill second coord array
    call ESMF_GridGetCoord(gridB, localDE=lDE, staggerloc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                           farrayPtr=farrayPtrY, rc=localrc)           
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        farrayPtrX(i1,i2)=REAL(i1,ESMF_KIND_R8)+1.0 ! Make coordinates for GridB different than GridA
        farrayPtrY(i1,i2)=REAL(i2,ESMF_KIND_R8)
     enddo
     enddo

  enddo


  ! Check Grid Match
  ! (it shouldn't pass)
  if (ESMF_GridMatch(gridA, gridB, rc=localrc)==ESMF_GRIDMATCH_EXACT) correct=.false.
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Destroy Test Grids
  call ESMF_GridDestroy(gridA, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_GridDestroy(gridB, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


#if 1
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Set/Get Coordinates from Array"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS

  ! create 2D test Grid
  grid2D=ESMF_GridCreate(distgrid=distgrid2D, coordTypeKind=ESMF_TYPEKIND_R8, &
         indexflag=ESMF_INDEX_GLOBAL, rc=rc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! get distgrid 
  call ESMF_GridGet(grid2D, staggerloc=ESMF_STAGGERLOC_CORNER, distgrid=tmpDistGrid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
 

  ! Create Array 
  array2D=ESMF_ArrayCreate(tmpdistgrid, arrayspec2D, &
                           indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Set Coord From Array
  call ESMF_GridSetCoord(grid2D,coordDim=1, &
               staggerloc=ESMF_STAGGERLOC_CORNER, array=array2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get Coord From Array
  call ESMF_GridGetCoord(grid2D,coordDim=1,&
               staggerloc=ESMF_STAGGERLOC_CORNER, array=array2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get info to do a partial sanity check that the array is the same
  call ESMF_ArrayGet(array2, rank=rank, typekind=typekind, rc=localrc)

  ! Check that array info is as expected
  correct=.true.
  if (rank .ne. 2) correct=.false.
  if (typekind .ne. ESMF_TYPEKIND_R8) correct=.false. 

  ! Destroy Test Grid
  call ESMF_GridDestroy(grid2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! destroy test array
  call ESMF_ArrayDestroy(array2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test Coordinate Storage by Creating a C-Grid"
  write(failMsg, *) "Incorrect result"

  ! init flags
  rc=ESMF_SUCCESS
  correct=.true.

  ! Set corner coordinates
  cornerX(1) = 0.0
  DO i = 2, globalXcount+1
     cornerX(i) = cornerX(i-1) + 1.0
  ENDDO

  cornerY(1) = 0.0
  DO i = 2, globalYcount+1
     cornerY(i) = cornerY(i-1) + 1.0
  ENDDO

  ! Create Grid with globalXCountxglobalYCount cells
  grid2D=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/globalXcount,globalYCount/), &
                                  coordDep1=(/1/), coordDep2=(/2/),                        &
                                  indexflag=ESMF_INDEX_GLOBAL,                             &
                                  rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get number of local DEs
  call ESMF_GridGet(grid2D, localDECount=localDECount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  !!!!!!!!!!!!!!!! Allocate and Fill Centers !!!!!!!!!!!!!!!!!!

  ! Allocate Center (e.g. Center) stagger
  call ESMF_GridAddCoord(grid2D, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Loop through DEs and set Centers as the average of the corners
  do lDE=0,localDECount-1  

     ! get and fill first coord array
     call ESMF_GridGetCoord(grid2D, localDE=lDE,  staggerloc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                         computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtr1D, &
                         rc=localrc)           
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     do i=clbnd(1),cubnd(1)
        farrayPtr1D(i)=0.5*(cornerX(i)+cornerX(i+1))
     enddo

    ! get and fill second coord array
    call ESMF_GridGetCoord(grid2D, localDE=lDE, staggerloc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                         computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtr1D, &
                         rc=localrc)           
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     do i=clbnd(1),cubnd(1)
        farrayPtr1D(i)=0.5*(cornerY(i)+cornerY(i+1))
     enddo
  enddo


  !!!!!!!!!!!!!!!! Allocate and Fill Edge 1 !!!!!!!!!!!!!!!!!!

  ! Allocate Center (e.g. Center) stagger
  call ESMF_GridAddCoord(grid2D, staggerloc=ESMF_STAGGERLOC_EDGE1, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Loop through DEs and set Edge1 computed from the corners
  do lDE=0,localDECount-1  

     ! get and fill first coord array
     call ESMF_GridGetCoord(grid2D, localDE=lDE,  staggerloc=ESMF_STAGGERLOC_EDGE1, coordDim=1, &
                           computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtr1D, &
                           rc=localrc)           
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     do i=clbnd(1),cubnd(1)
        farrayPtr1D(i)=cornerX(i)
     enddo

    ! get and fill second coord array
    call ESMF_GridGetCoord(grid2D, localDE=lDE, staggerloc=ESMF_STAGGERLOC_EDGE1, coordDim=2, &
                         computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtr1D, &
                         rc=localrc)           
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     do i=clbnd(1),cubnd(1)
        farrayPtr1D(i)=0.5*(cornerY(i)+cornerY(i+1))
     enddo
  enddo


  !!!!!!!!!!!!!!!! Allocate and Fill Edge 2 !!!!!!!!!!!!!!!!!!

  ! Allocate Center (e.g. Center) stagger
  call ESMF_GridAddCoord(grid2D, staggerloc=ESMF_STAGGERLOC_EDGE2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Loop through DEs and set Edge1 computed from the corners
  do lDE=0,localDECount-1  

     ! get and fill first coord array
     call ESMF_GridGetCoord(grid2D, localDE=lDE,  staggerloc=ESMF_STAGGERLOC_EDGE2, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtr1D, &
                            rc=localrc)           
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     do i=clbnd(1),cubnd(1)
        farrayPtr1D(i)=0.5*(cornerX(i)+cornerX(i+1))
     enddo

     ! get and fill second coord array
     call ESMF_GridGetCoord(grid2D, localDE=lDE, staggerloc=ESMF_STAGGERLOC_EDGE2, coordDim=2, &
                         computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtr1D, &
                         rc=localrc)           
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     do i=clbnd(1),cubnd(1)
        farrayPtr1D(i)=cornerY(i)
     enddo  
  enddo


  ! Validate Grid
  call ESMF_GridValidate(grid2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Destroy Test Grid
  call ESMF_GridDestroy(grid2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Set/Get Coordinates from Array check distgridToArrayMap matching"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS
  correct=.true.

  ! create 2D test Grid
  grid2D=ESMF_GridCreateNoPeriDim(coordTypeKind=ESMF_TYPEKIND_R8, regDecomp=(/2,2/), &
         maxIndex=(/10,20/), coordDep1=(/1/), coordDep2=(/2/),  &
         indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get distgrid 
  call ESMF_GridGet(grid2D, staggerloc=ESMF_STAGGERLOC_CORNER, distgrid=tmpDistGrid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
 
  ! First make bad Array and make sure it fails
  ! Create Array 
  array1D=ESMF_ArrayCreate(distgrid=tmpdistgrid, arrayspec=arrayspec1D, &
    distgridToArrayMap=(/0,1/),indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Set Coord From Array
  call ESMF_GridSetCoord(grid2D, coordDim=1, &
               staggerloc=ESMF_STAGGERLOC_CORNER, array=array1D, rc=localrc)
  if (localrc .eq. ESMF_SUCCESS) correct=.false. ! this should fail

  ! destroy test array
  call ESMF_ArrayDestroy(array1D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Then make a good Array and make sure it passes
  ! Create Array 
  array1D=ESMF_ArrayCreate(distgrid=tmpdistgrid, arrayspec=arrayspec1D, &
    distgridToArrayMap=(/1,0/),indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Set Coord From Array
  call ESMF_GridSetCoord(grid2D, coordDim=1, &
               staggerloc=ESMF_STAGGERLOC_CORNER, array=array1D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE 

  ! Get Coord From Array
  call ESMF_GridGetCoord(grid2D,coordDim=1,&
               staggerloc=ESMF_STAGGERLOC_CORNER, array=array2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get info to do a partial sanity check that the array is the same
  call ESMF_ArrayGet(array2, rank=rank, typekind=typekind, rc=localrc)

  ! Check that array info is as expected
  if (rank .ne. 1) correct=.false.
  if (typekind .ne. ESMF_TYPEKIND_R8) correct=.false. 

  ! Destroy Test Grid
  call ESMF_GridDestroy(grid2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! destroy test array
  call ESMF_ArrayDestroy(array1D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Get fortran pointer from coordinate array"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS

  ! create 2D test Grid
  grid2D=ESMF_GridCreate(distgrid=distgrid2D, coordTypeKind=ESMF_TYPEKIND_R8, &
         indexflag=ESMF_INDEX_GLOBAL, rc=rc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get distgrid 
  call ESMF_GridGet(grid2D, staggerloc=ESMF_STAGGERLOC_CORNER, distgrid=tmpDistGrid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Create Array 
  array2D=ESMF_ArrayCreate(tmpDistgrid, arrayspec2D, &
            indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Set Coord From Array
  call ESMF_GridSetCoord(grid2D,coordDim=2, &
               staggerloc=ESMF_STAGGERLOC_CORNER, array=array2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! set pointer to null
  nullify(farrayPtr)

  ! Get Coord From Grid
  call ESMF_GridGetCoord(grid2D, localDE=0, &
            staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, farrayPtr=farrayPtr, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Check that output is as expected
  correct=.true.
  if (.not. associated(farrayPtr)) correct=.false.

 ! Destroy Test Grid
  call ESMF_GridDestroy(grid2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! destroy test array
  call ESMF_ArrayDestroy(array2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test ESMF_StaggerLocSet"
  write(failMsg, *) "Incorrect result"

  ! Test StaggerLocSet by creating a custom stagger to  like a predefined and then
  ! using then interchangeably.

  ! init success flag
  rc=ESMF_SUCCESS

  ! create 2D test Grid
  grid2D=ESMF_GridCreate(distgrid=distgrid2D, coordTypeKind=ESMF_TYPEKIND_R8, &
         indexflag=ESMF_INDEX_GLOBAL, rc=rc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get distgrid 
  call ESMF_GridGet(grid2D, staggerloc=ESMF_STAGGERLOC_EDGE1, distgrid=tmpDistGrid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Create Array with extra space
  array2D=ESMF_ArrayCreate(tmpdistgrid, arrayspec2D, &
            indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! create a custom stagger to look like ESMF_STAGGERLOC_EDGE1
  call ESMF_StaggerLocSet(customStagger,loc=(/1,0/),rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Set Coord From Array
  call ESMF_GridSetCoord(grid2D,coordDim=2, &
               staggerloc=customStagger, array=array2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! set pointer to null
  nullify(farrayPtr)

  ! Get Coord From Grid
  call ESMF_GridGetCoord(grid2D, localDE=0, &
            staggerLoc=ESMF_STAGGERLOC_EDGE1, coordDim=2, farrayPtr=farrayPtr, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Check that output is as expected
  correct=.true.
  if (.not. associated(farrayPtr)) correct=.false.

 ! Destroy Test Grid
  call ESMF_GridDestroy(grid2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! destroy test array
  call ESMF_ArrayDestroy(array2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test StaggerLoc .eq. and .ne."
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS

  ! Check that output is as expected
  correct=.true.
  if (.not. (ESMF_STAGGERLOC_CORNER .eq. ESMF_STAGGERLOC_CORNER)) correct=.false.
  if (      (ESMF_STAGGERLOC_CORNER .eq. ESMF_STAGGERLOC_CENTER)) correct=.false.
  if (.not. (ESMF_STAGGERLOC_CORNER .ne. ESMF_STAGGERLOC_EDGE1)) correct=.false.
  if (      (ESMF_STAGGERLOC_EDGE1  .ne. ESMF_STAGGERLOC_EDGE1)) correct=.false.

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test ESMF_StaggerLocGet() "
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS

  ! Check that output is as expected
  correct=.true.

  ! Should be all 0's for _CENTER
  call ESMF_StaggerLocGet(ESMF_STAGGERLOC_CENTER, dim=1, loc=loc, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  if (loc .ne. 0) correct=.false.
  call ESMF_StaggerLocGet(ESMF_STAGGERLOC_CENTER, dim=2, loc=loc, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  if (loc .ne. 0) correct=.false.

  ! Should be all 1's for _CORNER
  call ESMF_StaggerLocGet(ESMF_STAGGERLOC_CORNER, dim=1, loc=loc, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  if (loc .ne. 1) correct=.false.
  call ESMF_StaggerLocGet(ESMF_STAGGERLOC_CORNER, dim=2, loc=loc, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  if (loc .ne. 1) correct=.false.

  ! Should be 1 and 0 for _EDGE1
  call ESMF_StaggerLocGet(ESMF_STAGGERLOC_EDGE1, dim=1, loc=loc, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  if (loc .ne. 1) correct=.false.
  call ESMF_StaggerLocGet(ESMF_STAGGERLOC_EDGE1, dim=2, loc=loc, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  if (loc .ne. 0) correct=.false.

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test StaggerLocString"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS

  ! Check that output is as expected
  correct=.true.
  call ESMF_StaggerLocString(ESMF_STAGGERLOC_CENTER, string, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  if (trim(string) .ne. trim("Center")) correct=.false.
  
  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Test Default 2D Bounds !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D GridAddCoord, by allocating coordinates for every stagger"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS

  ! if petCount >1, setup petMap
  if (petCount .gt. 1) then
     petMap2D(:,1,1)=(/0,1/)
     petMap2D(:,2,1)=(/2,3/)

     grid2D=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/), indexflag=ESMF_INDEX_GLOBAL, &
                              petMap=petMap2D, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  else
     grid2D=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/), indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  endif

  ! Allocate Staggers
  call ESMF_GridAddCoord(grid2D, &
               staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_GridAddCoord(grid2D, &
               staggerloc=ESMF_STAGGERLOC_EDGE1, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_GridAddCoord(grid2D, &
               staggerloc=ESMF_STAGGERLOC_EDGE2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_GridAddCoord(grid2D, &
               staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D CoordAlloc and GridGetCoord, by making sure default CENTER bounds are as expected"
  write(failMsg, *) "Incorrect result"

  ! Note that this test depends on coordinates allocated above
  ! and the fact that the grid was created with the ESMF_INDEX_GLOBAL flag

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.

  ! check coord 1
  call check2DBnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_CENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1/),ieubnd0=(/1,3/),iloff0=(/0,0/),iuoff0=(/0,0/), &
           ielbnd1=(/2,1/),ieubnd1=(/3,3/),iloff1=(/0,0/),iuoff1=(/0,0/), &
           ielbnd2=(/1,4/),ieubnd2=(/1,7/),iloff2=(/0,0/),iuoff2=(/0,0/), &
           ielbnd3=(/2,4/),ieubnd3=(/3,7/),iloff3=(/0,0/),iuoff3=(/0,0/), &
           correct=correct, rc=rc) 

  ! check coord 2
  call check2DBnds2x2(grid2D, coordDim=2, staggerloc=ESMF_STAGGERLOC_CENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1/),ieubnd0=(/1,3/),iloff0=(/0,0/),iuoff0=(/0,0/), &
           ielbnd1=(/2,1/),ieubnd1=(/3,3/),iloff1=(/0,0/),iuoff1=(/0,0/), &
           ielbnd2=(/1,4/),ieubnd2=(/1,7/),iloff2=(/0,0/),iuoff2=(/0,0/), &
           ielbnd3=(/2,4/),ieubnd3=(/3,7/),iloff3=(/0,0/),iuoff3=(/0,0/), &
           correct=correct, rc=rc) 

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D CoordAlloc and GridGetCoord, by making sure default EDGE1 bounds are as expected"
  write(failMsg, *) "Incorrect result"

  ! Note that this test depends on coordinates allocated above
  ! and the fact that the grid was created with the ESMF_INDEX_GLOBAL flag

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.

  ! check coord 1
  call check2DBnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_EDGE1, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1/),ieubnd0=(/1,3/),iloff0=(/0,0/),iuoff0=(/0,0/), &
           ielbnd1=(/2,1/),ieubnd1=(/3,3/),iloff1=(/0,0/),iuoff1=(/1,0/), &
           ielbnd2=(/1,4/),ieubnd2=(/1,7/),iloff2=(/0,0/),iuoff2=(/0,0/), &
           ielbnd3=(/2,4/),ieubnd3=(/3,7/),iloff3=(/0,0/),iuoff3=(/1,0/), &
           correct=correct, rc=rc) 

  ! check coord 2
  call check2DBnds2x2(grid2D, coordDim=2, staggerloc=ESMF_STAGGERLOC_EDGE1, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1/),ieubnd0=(/1,3/),iloff0=(/0,0/),iuoff0=(/0,0/), &
           ielbnd1=(/2,1/),ieubnd1=(/3,3/),iloff1=(/0,0/),iuoff1=(/1,0/), &
           ielbnd2=(/1,4/),ieubnd2=(/1,7/),iloff2=(/0,0/),iuoff2=(/0,0/), &
           ielbnd3=(/2,4/),ieubnd3=(/3,7/),iloff3=(/0,0/),iuoff3=(/1,0/), &
           correct=correct, rc=rc) 

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D CoordAlloc and GridGetCoord, by making sure default EDGE2 bounds are as expected"
  write(failMsg, *) "Incorrect result"

  ! Note that this test depends on coordinates allocated above
  ! and the fact that the grid was created with the ESMF_INDEX_GLOBAL flag

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.

  ! check coord 1
  call check2DBnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_EDGE2, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1/),ieubnd0=(/1,3/),iloff0=(/0,0/),iuoff0=(/0,0/), &
           ielbnd1=(/2,1/),ieubnd1=(/3,3/),iloff1=(/0,0/),iuoff1=(/0,0/), &
           ielbnd2=(/1,4/),ieubnd2=(/1,7/),iloff2=(/0,0/),iuoff2=(/0,1/), &
           ielbnd3=(/2,4/),ieubnd3=(/3,7/),iloff3=(/0,0/),iuoff3=(/0,1/), &
           correct=correct, rc=rc) 

  ! check coord 2
  call check2DBnds2x2(grid2D, coordDim=2, staggerloc=ESMF_STAGGERLOC_EDGE2, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1/),ieubnd0=(/1,3/),iloff0=(/0,0/),iuoff0=(/0,0/), &
           ielbnd1=(/2,1/),ieubnd1=(/3,3/),iloff1=(/0,0/),iuoff1=(/0,0/), &
           ielbnd2=(/1,4/),ieubnd2=(/1,7/),iloff2=(/0,0/),iuoff2=(/0,1/), &
           ielbnd3=(/2,4/),ieubnd3=(/3,7/),iloff3=(/0,0/),iuoff3=(/0,1/), &
           correct=correct, rc=rc) 

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D CoordAlloc and GridGetCoord, by making sure default CORNER bounds are as expected"
  write(failMsg, *) "Incorrect result"

  ! Note that this test depends on coordinates allocated above
  ! and the fact that the grid was created with the ESMF_INDEX_GLOBAL flag

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.

  ! check coord 1
  call check2DBnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_CORNER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1/),ieubnd0=(/1,3/),iloff0=(/0,0/),iuoff0=(/0,0/), &
           ielbnd1=(/2,1/),ieubnd1=(/3,3/),iloff1=(/0,0/),iuoff1=(/1,0/), &
           ielbnd2=(/1,4/),ieubnd2=(/1,7/),iloff2=(/0,0/),iuoff2=(/0,1/), &
           ielbnd3=(/2,4/),ieubnd3=(/3,7/),iloff3=(/0,0/),iuoff3=(/1,1/), &
           correct=correct, rc=rc) 

  ! check coord 2
  call check2DBnds2x2(grid2D, coordDim=2, staggerloc=ESMF_STAGGERLOC_CORNER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1/),ieubnd0=(/1,3/),iloff0=(/0,0/),iuoff0=(/0,0/), &
           ielbnd1=(/2,1/),ieubnd1=(/3,3/),iloff1=(/0,0/),iuoff1=(/1,0/), &
           ielbnd2=(/1,4/),ieubnd2=(/1,7/),iloff2=(/0,0/),iuoff2=(/0,1/), &
           ielbnd3=(/2,4/),ieubnd3=(/3,7/),iloff3=(/0,0/),iuoff3=(/1,1/), &
           correct=correct, rc=rc) 

  ! Destroy Test Grid
  call ESMF_GridDestroy(grid2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

#endif

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Test 2D Bounds With User Defined Widths !!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D GridAddCoord with staggerWidths, by allocating coordinates for every stagger with different widths"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS

  ! if petCount >1, setup petMap
  if (petCount .gt. 1) then
     petMap2D(:,1,1)=(/0,1/)
     petMap2D(:,2,1)=(/2,3/)

     grid2D=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/), indexflag=ESMF_INDEX_GLOBAL, &
                              gridEdgeLWidth=(/5,6/), &
                              gridEdgeUWidth=(/7,8/), &
                              petMap=petMap2D, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  else
     grid2D=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/), indexflag=ESMF_INDEX_GLOBAL, &
                              gridEdgeLWidth=(/5,6/), &
                              gridEdgeUWidth=(/7,8/), &
                              rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  endif


  ! Allocate Staggers
  call ESMF_GridAddCoord(grid2D, staggerEdgeLWidth=(/1,2/), staggerEdgeUWidth=(/3,4/), &
               staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
#if 1
  call ESMF_GridAddCoord(grid2D, staggerEdgeLWidth=(/5,6/), &
               staggerloc=ESMF_STAGGERLOC_EDGE1, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_GridAddCoord(grid2D, staggerEdgeUWidth=(/7,8/), &
               staggerloc=ESMF_STAGGERLOC_EDGE2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_GridAddCoord(grid2D, staggerEdgeUWidth=(/1,1/), &
               staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
#endif

  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D CoordAlloc and GridGetCoord, by making sure set CENTER bounds are as expected"
  write(failMsg, *) "Incorrect result"

  ! Note that this test depends on coordinates allocated above
  ! and the fact that the grid was created with the ESMF_INDEX_GLOBAL flag

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.

  ! check coord 1
  call check2DBnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_CENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1/),ieubnd0=(/1,3/),iloff0=(/1,2/),iuoff0=(/0,0/), &
           ielbnd1=(/2,1/),ieubnd1=(/3,3/),iloff1=(/0,2/),iuoff1=(/3,0/), &
           ielbnd2=(/1,4/),ieubnd2=(/1,7/),iloff2=(/1,0/),iuoff2=(/0,4/), &
           ielbnd3=(/2,4/),ieubnd3=(/3,7/),iloff3=(/0,0/),iuoff3=(/3,4/), &
           correct=correct, rc=rc) 

  ! check coord 2
  call check2DBnds2x2(grid2D, coordDim=2, staggerloc=ESMF_STAGGERLOC_CENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1/),ieubnd0=(/1,3/),iloff0=(/1,2/),iuoff0=(/0,0/), &
           ielbnd1=(/2,1/),ieubnd1=(/3,3/),iloff1=(/0,2/),iuoff1=(/3,0/), &
           ielbnd2=(/1,4/),ieubnd2=(/1,7/),iloff2=(/1,0/),iuoff2=(/0,4/), &
           ielbnd3=(/2,4/),ieubnd3=(/3,7/),iloff3=(/0,0/),iuoff3=(/3,4/), &
           correct=correct, rc=rc) 


  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

#if 1
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D CoordAlloc and GridGetCoord, by making sure set EDGE1 bounds are as expected"
  write(failMsg, *) "Incorrect result"

  ! Note that this test depends on coordinates allocated above
  ! and the fact that the grid was created with the ESMF_INDEX_GLOBAL flag

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.

  ! check coord 1
  call check2DBnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_EDGE1, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1/),ieubnd0=(/1,3/),iloff0=(/5,6/),iuoff0=(/0,0/), &
           ielbnd1=(/2,1/),ieubnd1=(/3,3/),iloff1=(/0,6/),iuoff1=(/0,0/), &
           ielbnd2=(/1,4/),ieubnd2=(/1,7/),iloff2=(/5,0/),iuoff2=(/0,0/), &
           ielbnd3=(/2,4/),ieubnd3=(/3,7/),iloff3=(/0,0/),iuoff3=(/0,0/), &
           correct=correct, rc=rc) 

  ! check coord 2
  call check2DBnds2x2(grid2D, coordDim=2, staggerloc=ESMF_STAGGERLOC_EDGE1, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1/),ieubnd0=(/1,3/),iloff0=(/5,6/),iuoff0=(/0,0/), &
           ielbnd1=(/2,1/),ieubnd1=(/3,3/),iloff1=(/0,6/),iuoff1=(/0,0/), &
           ielbnd2=(/1,4/),ieubnd2=(/1,7/),iloff2=(/5,0/),iuoff2=(/0,0/), &
           ielbnd3=(/2,4/),ieubnd3=(/3,7/),iloff3=(/0,0/),iuoff3=(/0,0/), &
           correct=correct, rc=rc) 


  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D CoordAlloc and GridGetCoord, by making sure set EDGE2 bounds are as expected"
  write(failMsg, *) "Incorrect result"

  ! Note that this test depends on coordinates allocated above
  ! and the fact that the grid was created with the ESMF_INDEX_GLOBAL flag

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.

  ! check coord 1
  call check2DBnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_EDGE2, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1/),ieubnd0=(/1,3/),iloff0=(/0,0/),iuoff0=(/0,0/), &
           ielbnd1=(/2,1/),ieubnd1=(/3,3/),iloff1=(/0,0/),iuoff1=(/7,0/), &
           ielbnd2=(/1,4/),ieubnd2=(/1,7/),iloff2=(/0,0/),iuoff2=(/0,8/), &
           ielbnd3=(/2,4/),ieubnd3=(/3,7/),iloff3=(/0,0/),iuoff3=(/7,8/), &
           correct=correct, rc=rc) 

  ! check coord 2
  call check2DBnds2x2(grid2D, coordDim=2, staggerloc=ESMF_STAGGERLOC_EDGE2, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1/),ieubnd0=(/1,3/),iloff0=(/0,0/),iuoff0=(/0,0/), &
           ielbnd1=(/2,1/),ieubnd1=(/3,3/),iloff1=(/0,0/),iuoff1=(/7,0/), &
           ielbnd2=(/1,4/),ieubnd2=(/1,7/),iloff2=(/0,0/),iuoff2=(/0,8/), &
           ielbnd3=(/2,4/),ieubnd3=(/3,7/),iloff3=(/0,0/),iuoff3=(/7,8/), &
           correct=correct, rc=rc) 


  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D CoordAlloc and GridGetCoord, by making sure set CORNER bounds are as expected"
  write(failMsg, *) "Incorrect result"

  ! Note that this test depends on coordinates allocated above
  ! and the fact that the grid was created with the ESMF_INDEX_GLOBAL flag

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.

  ! check coord 1
  call check2DBnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_CORNER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1/),ieubnd0=(/1,3/),iloff0=(/0,0/),iuoff0=(/0,0/), &
           ielbnd1=(/2,1/),ieubnd1=(/3,3/),iloff1=(/0,0/),iuoff1=(/1,0/), &
           ielbnd2=(/1,4/),ieubnd2=(/1,7/),iloff2=(/0,0/),iuoff2=(/0,1/), &
           ielbnd3=(/2,4/),ieubnd3=(/3,7/),iloff3=(/0,0/),iuoff3=(/1,1/), &
           correct=correct, rc=rc) 

  ! check coord 2
  call check2DBnds2x2(grid2D, coordDim=2, staggerloc=ESMF_STAGGERLOC_CORNER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1/),ieubnd0=(/1,3/),iloff0=(/0,0/),iuoff0=(/0,0/), &
           ielbnd1=(/2,1/),ieubnd1=(/3,3/),iloff1=(/0,0/),iuoff1=(/1,0/), &
           ielbnd2=(/1,4/),ieubnd2=(/1,7/),iloff2=(/0,0/),iuoff2=(/0,1/), &
           ielbnd3=(/2,4/),ieubnd3=(/3,7/),iloff3=(/0,0/),iuoff3=(/1,1/), &
           correct=correct, rc=rc) 

  ! Destroy Test Grid
  call ESMF_GridDestroy(grid2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D coordDep, by flipping coord2 and allocating CENTER and CORNER staggers"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS

  ! if petCount >1, setup petMap
  if (petCount .gt. 1) then
     petMap2D(:,1,1)=(/0,1/)
     petMap2D(:,2,1)=(/2,3/)

     grid2D=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/), indexflag=ESMF_INDEX_GLOBAL, &
                              coordDep2=(/2,1/), &
                              gridEdgeLWidth=(/1,2/), &
                              gridEdgeUWidth=(/3,4/), &
                              petMap=petMap2D, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  else
     grid2D=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/), &
                              gridEdgeLWidth=(/1,2/), &
                              gridEdgeUWidth=(/3,4/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              coordDep2=(/2,1/), rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  endif

  ! Allocate Staggers
  call ESMF_GridAddCoord(grid2D, staggerEdgeLWidth=(/1,2/), staggerEdgeUWidth=(/3,4/), &
               staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_GridAddCoord(grid2D, staggerEdgeUWidth=(/1,1/), &
               staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D CoordDep, by making sure set CENTER bounds are as expected"
  write(failMsg, *) "Incorrect result"

  ! Note that this test depends on coordinates allocated above
  ! and the fact that the grid was created with the ESMF_INDEX_GLOBAL flag

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.

  ! check coord 1
  call check2DBnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_CENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1/),ieubnd0=(/1,3/),iloff0=(/1,2/),iuoff0=(/0,0/), &
           ielbnd1=(/2,1/),ieubnd1=(/3,3/),iloff1=(/0,2/),iuoff1=(/3,0/), &
           ielbnd2=(/1,4/),ieubnd2=(/1,7/),iloff2=(/1,0/),iuoff2=(/0,4/), &
           ielbnd3=(/2,4/),ieubnd3=(/3,7/),iloff3=(/0,0/),iuoff3=(/3,4/), &
           correct=correct, rc=rc) 

  ! check coord 2
  call check2DBnds2x2(grid2D, coordDim=2, staggerloc=ESMF_STAGGERLOC_CENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1/),ieubnd0=(/3,1/),iloff0=(/2,1/),iuoff0=(/0,0/), &
           ielbnd1=(/1,2/),ieubnd1=(/3,3/),iloff1=(/2,0/),iuoff1=(/0,3/), &
           ielbnd2=(/4,1/),ieubnd2=(/7,1/),iloff2=(/0,1/),iuoff2=(/4,0/), &
           ielbnd3=(/4,2/),ieubnd3=(/7,3/),iloff3=(/0,0/),iuoff3=(/4,3/), &
           correct=correct, rc=rc) 



  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D CoordDep, by making sure default CORNER bounds are as expected"
  write(failMsg, *) "Incorrect result"

  ! Note that this test depends on coordinates allocated above
  ! and the fact that the grid was created with the ESMF_INDEX_GLOBAL flag

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.

  ! check coord 1
  call check2DBnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_CORNER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1/),ieubnd0=(/1,3/),iloff0=(/0,0/),iuoff0=(/0,0/), &
           ielbnd1=(/2,1/),ieubnd1=(/3,3/),iloff1=(/0,0/),iuoff1=(/1,0/), &
           ielbnd2=(/1,4/),ieubnd2=(/1,7/),iloff2=(/0,0/),iuoff2=(/0,1/), &
           ielbnd3=(/2,4/),ieubnd3=(/3,7/),iloff3=(/0,0/),iuoff3=(/1,1/), &
           correct=correct, rc=rc) 

  ! check coord 2
  call check2DBnds2x2(grid2D, coordDim=2, staggerloc=ESMF_STAGGERLOC_CORNER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1/),ieubnd0=(/3,1/),iloff0=(/0,0/),iuoff0=(/0,0/), &
           ielbnd1=(/1,2/),ieubnd1=(/3,3/),iloff1=(/0,0/),iuoff1=(/0,1/), &
           ielbnd2=(/4,1/),ieubnd2=(/7,1/),iloff2=(/0,0/),iuoff2=(/1,0/), &
           ielbnd3=(/4,2/),ieubnd3=(/7,3/),iloff3=(/0,0/),iuoff3=(/1,1/), &
           correct=correct, rc=rc) 

  ! Destroy Test Grid
  call ESMF_GridDestroy(grid2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!! Test 2D Plus 1 Default Bounds !!!!!!!!!!!!!!!! !!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D plus 1 GridAddCoord, by allocating coordinates for every stagger"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS

  ! if petCount >1, setup petMap
  if (petCount .gt. 1) then
     petMap2D(:,1,1)=(/0,1/)
     petMap2D(:,2,1)=(/2,3/)

     grid2D=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/),  &
                              countsPerDeDim3=(/5/),  &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              petMap=petMap2D, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  else
     grid2D=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/),  &
                              countsPerDeDim3=(/5/),  & 
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  endif

  ! Allocate Staggers
  call ESMF_GridAddCoord(grid2D, &
               staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_GridAddCoord(grid2D, &
               staggerloc=ESMF_STAGGERLOC_EDGE1_VCENTER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_GridAddCoord(grid2D, &
               staggerloc=ESMF_STAGGERLOC_EDGE2_VCENTER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_GridAddCoord(grid2D, &
               staggerloc=ESMF_STAGGERLOC_CORNER_VCENTER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_GridAddCoord(grid2D, &
               staggerloc=ESMF_STAGGERLOC_CENTER_VFACE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_GridAddCoord(grid2D, &
               staggerloc=ESMF_STAGGERLOC_EDGE1_VFACE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_GridAddCoord(grid2D, &
               staggerloc=ESMF_STAGGERLOC_EDGE2_VFACE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_GridAddCoord(grid2D, &
               staggerloc=ESMF_STAGGERLOC_CORNER_VFACE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D PLUS 1 CoordAlloc and GridGetCoord, by making sure default CENTER_VCENTER bounds are as expected"
  write(failMsg, *) "Incorrect result"

  ! Note that this test depends on coordinates allocated above
  ! and the fact that the grid was created with the ESMF_INDEX_GLOBAL flag

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.

  ! check coord 1
  call check2DP1Bnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,0/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/0,0,0/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,0,0/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/0,0,0/), &
           correct=correct, rc=rc) 

  ! check coord 2
  call check2DP1Bnds2x2(grid2D, coordDim=2, staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,0/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/0,0,0/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,0,0/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/0,0,0/), &
           correct=correct, rc=rc) 

  ! check coord 3
  call check2DP1Bnds2x2(grid2D, coordDim=3, staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,0/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/0,0,0/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,0,0/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/0,0,0/), &
           correct=correct, rc=rc) 


  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D PLUS 1 CoordAlloc and GridGetCoord, by making sure default EDGE1_VCENTER bounds are as expected"
  write(failMsg, *) "Incorrect result"

  ! Note that this test depends on coordinates allocated above
  ! and the fact that the grid was created with the ESMF_INDEX_GLOBAL flag

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.

  ! check coord 1
  call check2DP1Bnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_EDGE1_VCENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,0/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/1,0,0/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,0,0/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/1,0,0/), &
           correct=correct, rc=rc) 

  ! check coord 2
  call check2DP1Bnds2x2(grid2D, coordDim=2, staggerloc=ESMF_STAGGERLOC_EDGE1_VCENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,0/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/1,0,0/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,0,0/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/1,0,0/), &
           correct=correct, rc=rc) 


  ! check coord 3
  call check2DP1Bnds2x2(grid2D, coordDim=3, staggerloc=ESMF_STAGGERLOC_EDGE1_VCENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,0/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/1,0,0/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,0,0/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/1,0,0/), &
           correct=correct, rc=rc) 



  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D PLUS 1 CoordAlloc and GridGetCoord, by making sure default EDGE2_VCENTER bounds are as expected"
  write(failMsg, *) "Incorrect result"

  ! Note that this test depends on coordinates allocated above
  ! and the fact that the grid was created with the ESMF_INDEX_GLOBAL flag

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.

  ! check coord 1
  call check2DP1Bnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_EDGE2_VCENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,0/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/0,0,0/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,1,0/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/0,1,0/), &
           correct=correct, rc=rc) 

  ! check coord 2
  call check2DP1Bnds2x2(grid2D, coordDim=2, staggerloc=ESMF_STAGGERLOC_EDGE2_VCENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,0/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/0,0,0/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,1,0/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/0,1,0/), &
           correct=correct, rc=rc) 

  ! check coord 3
  call check2DP1Bnds2x2(grid2D, coordDim=3, staggerloc=ESMF_STAGGERLOC_EDGE2_VCENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,0/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/0,0,0/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,1,0/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/0,1,0/), &
           correct=correct, rc=rc) 


  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D PLUS 1 CoordAlloc and GridGetCoord, by making sure default CORNER_VCENTER bounds are as expected"
  write(failMsg, *) "Incorrect result"

  ! Note that this test depends on coordinates allocated above
  ! and the fact that the grid was created with the ESMF_INDEX_GLOBAL flag

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.

  ! check coord 1
  call check2DP1Bnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_CORNER_VCENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,0/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/1,0,0/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,1,0/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/1,1,0/), &
           correct=correct, rc=rc) 

  ! check coord 2
  call check2DP1Bnds2x2(grid2D, coordDim=2, staggerloc=ESMF_STAGGERLOC_CORNER_VCENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,0/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/1,0,0/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,1,0/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/1,1,0/), &
           correct=correct, rc=rc) 

  ! check coord 3
  call check2DP1Bnds2x2(grid2D, coordDim=3, staggerloc=ESMF_STAGGERLOC_CORNER_VCENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,0/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/1,0,0/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,1,0/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/1,1,0/), &
           correct=correct, rc=rc) 

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D PLUS 1 CoordAlloc and GridGetCoord, by making sure default CENTER_VFACE bounds are as expected"
  write(failMsg, *) "Incorrect result"

  ! Note that this test depends on coordinates allocated above
  ! and the fact that the grid was created with the ESMF_INDEX_GLOBAL flag

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.

  ! check coord 1
  call check2DP1Bnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_CENTER_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,1/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/0,0,1/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,0,1/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/0,0,1/), &
           correct=correct, rc=rc) 

  ! check coord 2
  call check2DP1Bnds2x2(grid2D, coordDim=2, staggerloc=ESMF_STAGGERLOC_CENTER_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,1/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/0,0,1/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,0,1/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/0,0,1/), &
           correct=correct, rc=rc) 

  ! check coord 3
  call check2DP1Bnds2x2(grid2D, coordDim=3, staggerloc=ESMF_STAGGERLOC_CENTER_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,1/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/0,0,1/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,0,1/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/0,0,1/), &
           correct=correct, rc=rc) 


  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D PLUS 1 CoordAlloc and GridGetCoord, by making sure default EDGE1_VFACE bounds are as expected"
  write(failMsg, *) "Incorrect result"

  ! Note that this test depends on coordinates allocated above
  ! and the fact that the grid was created with the ESMF_INDEX_GLOBAL flag

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.

  ! check coord 1
  call check2DP1Bnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_EDGE1_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,1/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/1,0,1/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,0,1/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/1,0,1/), &
           correct=correct, rc=rc) 

  ! check coord 2
  call check2DP1Bnds2x2(grid2D, coordDim=2, staggerloc=ESMF_STAGGERLOC_EDGE1_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,1/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/1,0,1/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,0,1/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/1,0,1/), &
           correct=correct, rc=rc) 


  ! check coord 3
  call check2DP1Bnds2x2(grid2D, coordDim=3, staggerloc=ESMF_STAGGERLOC_EDGE1_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,1/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/1,0,1/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,0,1/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/1,0,1/), &
           correct=correct, rc=rc) 



  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D PLUS 1 CoordAlloc and GridGetCoord, by making sure default EDGE2_VFACE bounds are as expected"
  write(failMsg, *) "Incorrect result"

  ! Note that this test depends on coordinates allocated above
  ! and the fact that the grid was created with the ESMF_INDEX_GLOBAL flag

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.

  ! check coord 1
  call check2DP1Bnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_EDGE2_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,1/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/0,0,1/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,1,1/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/0,1,1/), &
           correct=correct, rc=rc) 

  ! check coord 2
  call check2DP1Bnds2x2(grid2D, coordDim=2, staggerloc=ESMF_STAGGERLOC_EDGE2_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,1/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/0,0,1/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,1,1/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/0,1,1/), &
           correct=correct, rc=rc) 

  ! check coord 3
  call check2DP1Bnds2x2(grid2D, coordDim=3, staggerloc=ESMF_STAGGERLOC_EDGE2_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,1/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/0,0,1/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,1,1/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/0,1,1/), &
           correct=correct, rc=rc) 


  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D PLUS 1 CoordAlloc and GridGetCoord, by making sure default CORNER_VFACE bounds are as expected"
  write(failMsg, *) "Incorrect result"

  ! Note that this test depends on coordinates allocated above
  ! and the fact that the grid was created with the ESMF_INDEX_GLOBAL flag

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.

  ! check coord 1
  call check2DP1Bnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_CORNER_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,1/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/1,0,1/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,1,1/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/1,1,1/), &
           correct=correct, rc=rc) 

  ! check coord 2
  call check2DP1Bnds2x2(grid2D, coordDim=2, staggerloc=ESMF_STAGGERLOC_CORNER_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,1/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/1,0,1/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,1,1/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/1,1,1/), &
           correct=correct, rc=rc) 

  ! check coord 3
  call check2DP1Bnds2x2(grid2D, coordDim=3, staggerloc=ESMF_STAGGERLOC_CORNER_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,1/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/1,0,1/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,1,1/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/1,1,1/), &
           correct=correct, rc=rc) 


  ! Destroy Test Grid
  call ESMF_GridDestroy(grid2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!! Test 2D Plus 1  Bounds with Non-default Widths !!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D plus 1 GridAddCoord with non-default stagger widths, by allocating coordinates for every stagger"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS

  ! if petCount >1, setup petMap
  if (petCount .gt. 1) then
     petMap2D(:,1,1)=(/0,1/)
     petMap2D(:,2,1)=(/2,3/)

     grid2D=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/),  &
                              countsPerDeDim3=(/5/),  &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              gridEdgeLWidth=(/1,2,3/), &
                              gridEdgeUWidth=(/7,8,9/), &
                              petMap=petMap2D, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  else
     grid2D=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/),  &
                              countsPerDeDim3=(/5/),  & 
                              indexflag=ESMF_INDEX_GLOBAL, &
                              gridEdgeLWidth=(/1,2,3/), &
                              gridEdgeUWidth=(/7,8,9/), &
                              rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  endif

  ! Allocate Staggers
  call ESMF_GridAddCoord(grid2D, staggerEdgeLWidth=(/1,2,3/), staggerEdgeUWidth=(/4,5,6/), &
               staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_GridAddCoord(grid2D, staggerEdgeUWidth=(/7,8,9/), &
               staggerloc=ESMF_STAGGERLOC_EDGE1_VCENTER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_GridAddCoord(grid2D, staggerEdgeLWidth=(/1,2,3/), &
               staggerloc=ESMF_STAGGERLOC_EDGE2_VCENTER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_GridAddCoord(grid2D,  staggerEdgeUWidth=(/1,1,0/), &
               staggerloc=ESMF_STAGGERLOC_CORNER_VCENTER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_GridAddCoord(grid2D,  staggerEdgeUWidth=(/0,0,1/), &
               staggerloc=ESMF_STAGGERLOC_CENTER_VFACE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_GridAddCoord(grid2D,  staggerEdgeLWidth=(/1,2,3/), &
               staggerloc=ESMF_STAGGERLOC_EDGE1_VFACE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_GridAddCoord(grid2D, staggerEdgeLWidth=(/1,2,3/), staggerEdgeUWidth=(/4,5,6/), &
               staggerloc=ESMF_STAGGERLOC_EDGE2_VFACE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_GridAddCoord(grid2D, staggerEdgeUWidth=(/7,8,9/), &
               staggerloc=ESMF_STAGGERLOC_CORNER_VFACE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D PLUS 1 CoordAlloc and GridGetCoord, by making sure user set CENTER_VCENTER bounds are as expected"
  write(failMsg, *) "Incorrect result"

  ! Note that this test depends on coordinates allocated above
  ! and the fact that the grid was created with the ESMF_INDEX_GLOBAL flag

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.

  ! Adjust bounds by staggerEdgeLWidth=(/1,2,3/) staggerEdgeUWidth=(/4,5,6/)

  ! check coord 1
  call check2DP1Bnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/1,2,3/),iuoff0=(/0,0,6/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,2,3/),iuoff1=(/4,0,6/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/1,0,3/),iuoff2=(/0,5,6/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,3/),iuoff3=(/4,5,6/), &
           correct=correct, rc=rc) 


  ! check coord 2
  call check2DP1Bnds2x2(grid2D, coordDim=2, staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/1,2,3/),iuoff0=(/0,0,6/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,2,3/),iuoff1=(/4,0,6/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/1,0,3/),iuoff2=(/0,5,6/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,3/),iuoff3=(/4,5,6/), &
           correct=correct, rc=rc) 


  ! check coord 3
  call check2DP1Bnds2x2(grid2D, coordDim=3, staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/1,2,3/),iuoff0=(/0,0,6/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,2,3/),iuoff1=(/4,0,6/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/1,0,3/),iuoff2=(/0,5,6/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,3/),iuoff3=(/4,5,6/), &
           correct=correct, rc=rc) 

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D PLUS 1 CoordAlloc and GridGetCoord, by making sure user set EDGE1_VCENTER bounds are as expected"
  write(failMsg, *) "Incorrect result"

  ! Note that this test depends on coordinates allocated above
  ! and the fact that the grid was created with the ESMF_INDEX_GLOBAL flag

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.

  ! Adjust bounds by staggerEdgeUWidth=(/7,8,9/)

  ! check coord 1
  call check2DP1Bnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_EDGE1_VCENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,9/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/7,0,9/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,8,9/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/7,8,9/), &
           correct=correct, rc=rc) 


  ! check coord 2
  call check2DP1Bnds2x2(grid2D, coordDim=2, staggerloc=ESMF_STAGGERLOC_EDGE1_VCENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,9/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/7,0,9/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,8,9/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/7,8,9/), &
           correct=correct, rc=rc) 


  ! check coord 3
  call check2DP1Bnds2x2(grid2D, coordDim=3, staggerloc=ESMF_STAGGERLOC_EDGE1_VCENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,9/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/7,0,9/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,8,9/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/7,8,9/), &
           correct=correct, rc=rc) 





  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D PLUS 1 CoordAlloc and GridGetCoord, by making sure user set EDGE2_VCENTER bounds are as expected"
  write(failMsg, *) "Incorrect result"

  ! Note that this test depends on coordinates allocated above
  ! and the fact that the grid was created with the ESMF_INDEX_GLOBAL flag

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.

  ! Adjust bounds by staggerEdgeLWidth=(/1,2,3/)
  
  ! check coord 1
  call check2DP1Bnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_EDGE2_VCENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/1,2,3/),iuoff0=(/0,0,0/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,2,3/),iuoff1=(/0,0,0/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/1,0,3/),iuoff2=(/0,0,0/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,3/),iuoff3=(/0,0,0/), &
           correct=correct, rc=rc) 

  ! check coord 2
  call check2DP1Bnds2x2(grid2D, coordDim=2, staggerloc=ESMF_STAGGERLOC_EDGE2_VCENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/1,2,3/),iuoff0=(/0,0,0/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,2,3/),iuoff1=(/0,0,0/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/1,0,3/),iuoff2=(/0,0,0/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,3/),iuoff3=(/0,0,0/), &
           correct=correct, rc=rc) 


  ! check coord 3
  call check2DP1Bnds2x2(grid2D, coordDim=3, staggerloc=ESMF_STAGGERLOC_EDGE2_VCENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/1,2,3/),iuoff0=(/0,0,0/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,2,3/),iuoff1=(/0,0,0/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/1,0,3/),iuoff2=(/0,0,0/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,3/),iuoff3=(/0,0,0/), &
           correct=correct, rc=rc) 



  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D PLUS 1 CoordAlloc and GridGetCoord, by making sure user set CORNER_VCENTER bounds are as expected"
  write(failMsg, *) "Incorrect result"

  ! Note that this test depends on coordinates allocated above
  ! and the fact that the grid was created with the ESMF_INDEX_GLOBAL flag

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.

  ! Leave bounds at default

  ! check coord 1
  call check2DP1Bnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_CORNER_VCENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,0/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/1,0,0/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,1,0/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/1,1,0/), &
           correct=correct, rc=rc) 

  ! check coord 2
  call check2DP1Bnds2x2(grid2D, coordDim=2, staggerloc=ESMF_STAGGERLOC_CORNER_VCENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,0/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/1,0,0/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,1,0/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/1,1,0/), &
           correct=correct, rc=rc) 

  ! check coord 3
  call check2DP1Bnds2x2(grid2D, coordDim=3, staggerloc=ESMF_STAGGERLOC_CORNER_VCENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,0/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/1,0,0/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,1,0/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/1,1,0/), &
           correct=correct, rc=rc) 

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D PLUS 1 CoordAlloc and GridGetCoord, by making sure user set CENTER_VFACE bounds are as expected"
  write(failMsg, *) "Incorrect result"

  ! Note that this test depends on coordinates allocated above
  ! and the fact that the grid was created with the ESMF_INDEX_GLOBAL flag

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.

  ! Leave bounds at default

  ! check coord 1
  call check2DP1Bnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_CENTER_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,1/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/0,0,1/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,0,1/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/0,0,1/), &
           correct=correct, rc=rc) 

  ! check coord 2
  call check2DP1Bnds2x2(grid2D, coordDim=2, staggerloc=ESMF_STAGGERLOC_CENTER_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,1/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/0,0,1/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,0,1/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/0,0,1/), &
           correct=correct, rc=rc) 

  ! check coord 3
  call check2DP1Bnds2x2(grid2D, coordDim=3, staggerloc=ESMF_STAGGERLOC_CENTER_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,1/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/0,0,1/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,0,1/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/0,0,1/), &
           correct=correct, rc=rc) 


  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D PLUS 1 CoordAlloc and GridGetCoord, by making sure default EDGE1_VFACE bounds are as expected"
  write(failMsg, *) "Incorrect result"

  ! Note that this test depends on coordinates allocated above
  ! and the fact that the grid was created with the ESMF_INDEX_GLOBAL flag

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.


  ! Adjust bounds by staggerEdgeLWidth=(/1,2,3/)
  
  ! check coord 1
  call check2DP1Bnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_EDGE1_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/1,2,3/),iuoff0=(/0,0,0/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,2,3/),iuoff1=(/0,0,0/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/1,0,3/),iuoff2=(/0,0,0/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,3/),iuoff3=(/0,0,0/), &
           correct=correct, rc=rc) 

  ! check coord 2
  call check2DP1Bnds2x2(grid2D, coordDim=2, staggerloc=ESMF_STAGGERLOC_EDGE1_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/1,2,3/),iuoff0=(/0,0,0/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,2,3/),iuoff1=(/0,0,0/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/1,0,3/),iuoff2=(/0,0,0/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,3/),iuoff3=(/0,0,0/), &
           correct=correct, rc=rc) 


  ! check coord 3
  call check2DP1Bnds2x2(grid2D, coordDim=3, staggerloc=ESMF_STAGGERLOC_EDGE1_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/1,2,3/),iuoff0=(/0,0,0/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,2,3/),iuoff1=(/0,0,0/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/1,0,3/),iuoff2=(/0,0,0/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,3/),iuoff3=(/0,0,0/), &
           correct=correct, rc=rc) 


  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D PLUS 1 CoordAlloc and GridGetCoord, by making sure default EDGE2_VFACE bounds are as expected"
  write(failMsg, *) "Incorrect result"

  ! Note that this test depends on coordinates allocated above
  ! and the fact that the grid was created with the ESMF_INDEX_GLOBAL flag

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.


  ! Adjust bounds by staggerEdgeLWidth=(/1,2,3/) staggerEdgeUWidth=(/4,5,6/)

  ! check coord 1
  call check2DP1Bnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_EDGE2_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/1,2,3/),iuoff0=(/0,0,6/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,2,3/),iuoff1=(/4,0,6/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/1,0,3/),iuoff2=(/0,5,6/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,3/),iuoff3=(/4,5,6/), &
           correct=correct, rc=rc) 


  ! check coord 2
  call check2DP1Bnds2x2(grid2D, coordDim=2, staggerloc=ESMF_STAGGERLOC_EDGE2_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/1,2,3/),iuoff0=(/0,0,6/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,2,3/),iuoff1=(/4,0,6/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/1,0,3/),iuoff2=(/0,5,6/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,3/),iuoff3=(/4,5,6/), &
           correct=correct, rc=rc) 


  ! check coord 3
  call check2DP1Bnds2x2(grid2D, coordDim=3, staggerloc=ESMF_STAGGERLOC_EDGE2_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/1,2,3/),iuoff0=(/0,0,6/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,2,3/),iuoff1=(/4,0,6/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/1,0,3/),iuoff2=(/0,5,6/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,3/),iuoff3=(/4,5,6/), &
           correct=correct, rc=rc) 


  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D PLUS 1 CoordAlloc and GridGetCoord, by making sure default CORNER_VFACE bounds are as expected"
  write(failMsg, *) "Incorrect result"

  ! Note that this test depends on coordinates allocated above
  ! and the fact that the grid was created with the ESMF_INDEX_GLOBAL flag

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.


  ! Adjust bounds by staggerEdgeUWidth=(/7,8,9/)

  ! check coord 1
  call check2DP1Bnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_CORNER_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,9/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/7,0,9/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,8,9/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/7,8,9/), &
           correct=correct, rc=rc) 


  ! check coord 2
  call check2DP1Bnds2x2(grid2D, coordDim=2, staggerloc=ESMF_STAGGERLOC_CORNER_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,9/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/7,0,9/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,8,9/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/7,8,9/), &
           correct=correct, rc=rc) 


  ! check coord 3
  call check2DP1Bnds2x2(grid2D, coordDim=3, staggerloc=ESMF_STAGGERLOC_CORNER_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,9/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/7,0,9/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,8,9/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/7,8,9/), &
           correct=correct, rc=rc) 


  ! Destroy Test Grid
  call ESMF_GridDestroy(grid2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!! Test 2D Plus 1 Bounds with Non-default Widths and coordDep !!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D plus 1 GridAddCoord with non-default stagger widths and non-defaults coordDep"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS

  ! if petCount >1, setup petMap
  if (petCount .gt. 1) then
     petMap2D(:,1,1)=(/0,1/)
     petMap2D(:,2,1)=(/2,3/)

     grid2D=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/),  &
                              countsPerDeDim3=(/5/),  &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              coordDep1=(/2,1,3/), &
                              coordDep2=(/3,2,1/), &  ! use default for coordDep3
                              gridEdgeLWidth=(/1,2,3/), &
                              gridEdgeUWidth=(/7,8,9/), &  
                              petMap=petMap2D, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  else
     grid2D=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/),  &
                              countsPerDeDim3=(/5/),  & 
                              indexflag=ESMF_INDEX_GLOBAL, &
                              coordDep1=(/2,1,3/), &
                              coordDep2=(/3,2,1/), &  ! use default for coordDep3 
                              gridEdgeLWidth=(/1,2,3/), &
                              gridEdgeUWidth=(/7,8,9/), & 
                              rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  endif

  ! Allocate Staggers
  call ESMF_GridAddCoord(grid2D, staggerEdgeUWidth=(/7,8,9/), &
               staggerloc=ESMF_STAGGERLOC_EDGE1_VCENTER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_GridAddCoord(grid2D,  staggerEdgeLWidth=(/1,2,3/), &
               staggerloc=ESMF_STAGGERLOC_EDGE1_VFACE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_GridAddCoord(grid2D, staggerEdgeLWidth=(/1,2,3/), staggerEdgeUWidth=(/4,5,6/), &
               staggerloc=ESMF_STAGGERLOC_EDGE2_VFACE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_GridAddCoord(grid2D, staggerEdgeUWidth=(/7,8,9/), &
               staggerloc=ESMF_STAGGERLOC_CORNER_VFACE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------





  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D PLUS 1 GridGetCoord with non-default coordDep, by making sure EDGE1_VFACE bounds are as expected"
  write(failMsg, *) "Incorrect result"

  ! Note that this test depends on coordinates allocated above
  ! and the fact that the grid was created with the ESMF_INDEX_GLOBAL flag

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.


  ! Adjust bounds by staggerEdgeLWidth=(/1,2,3/)
  
  ! check coord 1
  call check2DP1Bnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_EDGE1_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/3,1,5/),iloff0=(/2,1,3/),iuoff0=(/0,0,0/), &
           ielbnd1=(/1,2,1/),ieubnd1=(/3,3,5/),iloff1=(/2,0,3/),iuoff1=(/0,0,0/), &
           ielbnd2=(/4,1,1/),ieubnd2=(/7,1,5/),iloff2=(/0,1,3/),iuoff2=(/0,0,0/), &
           ielbnd3=(/4,2,1/),ieubnd3=(/7,3,5/),iloff3=(/0,0,3/),iuoff3=(/0,0,0/), &
           correct=correct, rc=rc) 

  ! check coord 2
  call check2DP1Bnds2x2(grid2D, coordDim=2, staggerloc=ESMF_STAGGERLOC_EDGE1_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/5,3,1/),iloff0=(/3,2,1/),iuoff0=(/0,0,0/), &
           ielbnd1=(/1,1,2/),ieubnd1=(/5,3,3/),iloff1=(/3,2,0/),iuoff1=(/0,0,0/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/5,7,1/),iloff2=(/3,0,1/),iuoff2=(/0,0,0/), &
           ielbnd3=(/1,4,2/),ieubnd3=(/5,7,3/),iloff3=(/3,0,0/),iuoff3=(/0,0,0/), &
           correct=correct, rc=rc) 


  ! check coord 3
  call check2DP1Bnds2x2(grid2D, coordDim=3, staggerloc=ESMF_STAGGERLOC_EDGE1_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/1,2,3/),iuoff0=(/0,0,0/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,2,3/),iuoff1=(/0,0,0/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/1,0,3/),iuoff2=(/0,0,0/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,3/),iuoff3=(/0,0,0/), &
           correct=correct, rc=rc) 


  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D PLUS 1 GridGetCoord with non-default coordDep, by making sure EDGE2_VFACE bounds are as expected"
  write(failMsg, *) "Incorrect result"

  ! Note that this test depends on coordinates allocated above
  ! and the fact that the grid was created with the ESMF_INDEX_GLOBAL flag

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.


  ! Adjust bounds by staggerEdgeLWidth=(/1,2,3/) staggerEdgeUWidth=(/4,5,6/)

  ! check coord 1
  call check2DP1Bnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_EDGE2_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/3,1,5/),iloff0=(/2,1,3/),iuoff0=(/0,0,6/), &
           ielbnd1=(/1,2,1/),ieubnd1=(/3,3,5/),iloff1=(/2,0,3/),iuoff1=(/0,4,6/), &
           ielbnd2=(/4,1,1/),ieubnd2=(/7,1,5/),iloff2=(/0,1,3/),iuoff2=(/5,0,6/), &
           ielbnd3=(/4,2,1/),ieubnd3=(/7,3,5/),iloff3=(/0,0,3/),iuoff3=(/5,4,6/), &
           correct=correct, rc=rc) 


  ! check coord 2
  call check2DP1Bnds2x2(grid2D, coordDim=2, staggerloc=ESMF_STAGGERLOC_EDGE2_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/5,3,1/),iloff0=(/3,2,1/),iuoff0=(/6,0,0/), &
           ielbnd1=(/1,1,2/),ieubnd1=(/5,3,3/),iloff1=(/3,2,0/),iuoff1=(/6,0,4/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/5,7,1/),iloff2=(/3,0,1/),iuoff2=(/6,5,0/), &
           ielbnd3=(/1,4,2/),ieubnd3=(/5,7,3/),iloff3=(/3,0,0/),iuoff3=(/6,5,4/), &
           correct=correct, rc=rc) 


  ! check coord 3
  call check2DP1Bnds2x2(grid2D, coordDim=3, staggerloc=ESMF_STAGGERLOC_EDGE2_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/1,2,3/),iuoff0=(/0,0,6/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,2,3/),iuoff1=(/4,0,6/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/1,0,3/),iuoff2=(/0,5,6/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,3/),iuoff3=(/4,5,6/), &
           correct=correct, rc=rc) 


  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D PLUS 1 GridGetCoord with non-default coordDep, by making sure CORNER_VFACE bounds are as expected"
  write(failMsg, *) "Incorrect result"

  ! Note that this test depends on coordinates allocated above
  ! and the fact that the grid was created with the ESMF_INDEX_GLOBAL flag

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.


  ! Adjust bounds by staggerEdgeUWidth=(/7,8,9/)

  ! check coord 1
  call check2DP1Bnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_CORNER_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/3,1,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,9/), &
           ielbnd1=(/1,2,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/0,7,9/), &
           ielbnd2=(/4,1,1/),ieubnd2=(/7,1,5/),iloff2=(/0,0,0/),iuoff2=(/8,0,9/), &
           ielbnd3=(/4,2,1/),ieubnd3=(/7,3,5/),iloff3=(/0,0,0/),iuoff3=(/8,7,9/), &
           correct=correct, rc=rc) 


  ! check coord 2
  call check2DP1Bnds2x2(grid2D, coordDim=2, staggerloc=ESMF_STAGGERLOC_CORNER_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/5,3,1/),iloff0=(/0,0,0/),iuoff0=(/9,0,0/), &
           ielbnd1=(/1,1,2/),ieubnd1=(/5,3,3/),iloff1=(/0,0,0/),iuoff1=(/9,0,7/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/5,7,1/),iloff2=(/0,0,0/),iuoff2=(/9,8,0/), &
           ielbnd3=(/1,4,2/),ieubnd3=(/5,7,3/),iloff3=(/0,0,0/),iuoff3=(/9,8,7/), &
           correct=correct, rc=rc) 


  ! check coord 3
  call check2DP1Bnds2x2(grid2D, coordDim=3, staggerloc=ESMF_STAGGERLOC_CORNER_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,9/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,0,0/),iuoff1=(/7,0,9/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/0,0,0/),iuoff2=(/0,8,9/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,0/),iuoff3=(/7,8,9/), &
           correct=correct, rc=rc) 


  ! Destroy Test Grid
  call ESMF_GridDestroy(grid2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!! Test 2D Plus 1 Bounds with DELOCAL Index !!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D plus 1 GridAddCoord with DELOCAL Index"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS

  ! if petCount >1, setup petMap
  if (petCount .gt. 1) then
     petMap2D(:,1,1)=(/0,1/)
     petMap2D(:,2,1)=(/2,3/)

     grid2D=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/),  &
                              countsPerDeDim3=(/5/),  &
                              indexflag=ESMF_INDEX_DELOCAL, &
                              coordDep1=(/2,1,3/), &
                              coordDep2=(/3,2,1/), &  ! use default for coordDep3
                              gridEdgeLWidth=(/1,2,3/), &
                              gridEdgeUWidth=(/4,5,6/), &  
                               petMap=petMap2D, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  else
     grid2D=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/),  &
                              countsPerDeDim3=(/5/),  & 
                              indexflag=ESMF_INDEX_DELOCAL, &
                              coordDep1=(/2,1,3/), &
                              coordDep2=(/3,2,1/), &  ! use default for coordDep3  
                              gridEdgeLWidth=(/1,2,3/), &
                              gridEdgeUWidth=(/4,5,6/), &  
                              rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  endif

  ! Allocate Staggers
  call ESMF_GridAddCoord(grid2D, staggerEdgeLWidth=(/1,2,3/), staggerEdgeUWidth=(/4,5,6/), &
               staggerloc=ESMF_STAGGERLOC_EDGE2_VFACE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D PLUS 1 GridGetCoord with ESMF_INDEX_DELOCAL"
  write(failMsg, *) "Incorrect result"

  ! Note that this test depends on coordinates allocated above

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.


  ! Adjust bounds by staggerEdgeLWidth=(/1,2,3/) staggerEdgeUWidth=(/4,5,6/)

  ! check coord 1
  call check2DP1Bnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_EDGE2_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/3,1,5/),iloff0=(/0,0,0/),iuoff0=(/2,1,9/), &
           ielbnd1=(/1,1,1/),ieubnd1=(/3,2,5/),iloff1=(/0,0,0/),iuoff1=(/2,4,9/), &
           ielbnd2=(/1,1,1/),ieubnd2=(/4,1,5/),iloff2=(/0,0,0/),iuoff2=(/5,1,9/), &
           ielbnd3=(/1,1,1/),ieubnd3=(/4,2,5/),iloff3=(/0,0,0/),iuoff3=(/5,4,9/), &
           correct=correct, rc=rc) 


  ! check coord 2
  call check2DP1Bnds2x2(grid2D, coordDim=2, staggerloc=ESMF_STAGGERLOC_EDGE2_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/5,3,1/),iloff0=(/0,0,0/),iuoff0=(/9,2,1/), &
           ielbnd1=(/1,1,1/),ieubnd1=(/5,3,2/),iloff1=(/0,0,0/),iuoff1=(/9,2,4/), &
           ielbnd2=(/1,1,1/),ieubnd2=(/5,4,1/),iloff2=(/0,0,0/),iuoff2=(/9,5,1/), &
           ielbnd3=(/1,1,1/),ieubnd3=(/5,4,2/),iloff3=(/0,0,0/),iuoff3=(/9,5,4/), &
           correct=correct, rc=rc) 


  ! check coord 3
  call check2DP1Bnds2x2(grid2D, coordDim=3, staggerloc=ESMF_STAGGERLOC_EDGE2_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/0,0,0/),iuoff0=(/1,2,9/), &
           ielbnd1=(/1,1,1/),ieubnd1=(/2,3,5/),iloff1=(/0,0,0/),iuoff1=(/4,2,9/), &
           ielbnd2=(/1,1,1/),ieubnd2=(/1,4,5/),iloff2=(/0,0,0/),iuoff2=(/1,5,9/), &
           ielbnd3=(/1,1,1/),ieubnd3=(/2,4,5/),iloff3=(/0,0,0/),iuoff3=(/4,5,9/), &
           correct=correct, rc=rc) 

  ! Destroy Test Grid
  call ESMF_GridDestroy(grid2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!! Test 2D Plus 1 Bounds with ESMF_INDEX_USER  !!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D plus 1 GridAddCoord with ESMF_INDEX_USER"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS

  ! if petCount >1, setup petMap
  if (petCount .gt. 1) then
     petMap2D(:,1,1)=(/0,1/)
     petMap2D(:,2,1)=(/2,3/)

     grid2D=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/),  &
                              countsPerDeDim3=(/5/),  &
                              indexflag=ESMF_INDEX_USER, &
                              coordDep1=(/2,1,3/), &
                              coordDep2=(/3,2,1/), &  ! use default for coordDep3
                              gridEdgeLWidth=(/1,2,3/), &
                              gridEdgeUWidth=(/4,5,6/), &  
                              gridMemLBound=(/10,20,30/), &
                              petMap=petMap2D, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  else
     grid2D=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/),  &
                              countsPerDeDim3=(/5/),  & 
                              indexflag=ESMF_INDEX_USER, &
                              coordDep1=(/2,1,3/), &
                              coordDep2=(/3,2,1/), &  ! use default for coordDep3  
                              gridEdgeLWidth=(/1,2,3/), &
                              gridEdgeUWidth=(/4,5,6/), &  
                              gridMemLBound=(/10,20,30/), &
                              rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  endif

  ! Allocate Staggers
  call ESMF_GridAddCoord(grid2D, & 
               staggerloc=ESMF_STAGGERLOC_EDGE2_VFACE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D PLUS 1 GridGetCoord with ESMF_INDEX_USER and allocated coords"
  write(failMsg, *) "Incorrect result"

  ! Note that this test depends on coordinates allocated above

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.

  ! check coord 1
  ! check allocated staggerloc bounds
  call check2DP1Bnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_EDGE2_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/20,10,30/),ieubnd0=(/24,10,43/),iloff0=(/0,0,0/),iuoff0=(/0,0,0/), &
           ielbnd1=(/20,10,30/),ieubnd1=(/24,11,43/),iloff1=(/0,0,0/),iuoff1=(/0,0,0/), &
           ielbnd2=(/20,10,30/),ieubnd2=(/28,10,43/),iloff2=(/0,0,0/),iuoff2=(/0,0,0/), &
           ielbnd3=(/20,10,30/),ieubnd3=(/28,11,43/),iloff3=(/0,0,0/),iuoff3=(/0,0,0/), &
           correct=correct, rc=rc) 

  ! check coord 2
  ! check allocated staggerloc bounds
  call check2DP1Bnds2x2(grid2D, coordDim=2, staggerloc=ESMF_STAGGERLOC_EDGE2_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/30,20,10/),ieubnd0=(/43,24,10/),iloff0=(/0,0,0/),iuoff0=(/0,0,0/), &
           ielbnd1=(/30,20,10/),ieubnd1=(/43,24,11/),iloff1=(/0,0,0/),iuoff1=(/0,0,0/), &
           ielbnd2=(/30,20,10/),ieubnd2=(/43,28,10/),iloff2=(/0,0,0/),iuoff2=(/0,0,0/), &
           ielbnd3=(/30,20,10/),ieubnd3=(/43,28,11/),iloff3=(/0,0,0/),iuoff3=(/0,0,0/), &
           correct=correct, rc=rc) 

  ! check coord 3
  ! check allocated staggerloc bounds
  call check2DP1Bnds2x2(grid2D, coordDim=3, staggerloc=ESMF_STAGGERLOC_EDGE2_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/10,20,30/),ieubnd0=(/10,24,43/),iloff0=(/0,0,0/),iuoff0=(/0,0,0/), &
           ielbnd1=(/10,20,30/),ieubnd1=(/11,24,43/),iloff1=(/0,0,0/),iuoff1=(/0,0,0/), &
           ielbnd2=(/10,20,30/),ieubnd2=(/10,28,43/),iloff2=(/0,0,0/),iuoff2=(/0,0,0/), &
           ielbnd3=(/10,20,30/),ieubnd3=(/11,28,43/),iloff3=(/0,0,0/),iuoff3=(/0,0,0/), &
           correct=correct, rc=rc) 


  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D PLUS 1 GridGetCoord with ESMF_INDEX_USER and un-allocated coords"
  write(failMsg, *) "Incorrect result"

  ! Note that this test depends on coordinates allocated above

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.

  ! check coord 1
  ! check allocated staggerloc bounds
  call check2DP1Bnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_CORNER_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/20,10,30/),ieubnd0=(/24,11,43/),iloff0=(/0,0,0/),iuoff0=(/0,0,0/), &
           ielbnd1=(/20,10,30/),ieubnd1=(/24,15,43/),iloff1=(/0,0,0/),iuoff1=(/0,0,0/), &
           ielbnd2=(/20,10,30/),ieubnd2=(/28,11,43/),iloff2=(/0,0,0/),iuoff2=(/0,0,0/), &
           ielbnd3=(/20,10,30/),ieubnd3=(/28,15,43/),iloff3=(/0,0,0/),iuoff3=(/0,0,0/), &
           correct=correct, checkPtr=.false., rc=rc) 

  ! check coord 2
  ! check allocated staggerloc bounds
  call check2DP1Bnds2x2(grid2D, coordDim=2, staggerloc=ESMF_STAGGERLOC_CORNER_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/30,20,10/),ieubnd0=(/43,24,11/),iloff0=(/0,0,0/),iuoff0=(/0,0,0/), &
           ielbnd1=(/30,20,10/),ieubnd1=(/43,24,15/),iloff1=(/0,0,0/),iuoff1=(/0,0,0/), &
           ielbnd2=(/30,20,10/),ieubnd2=(/43,28,11/),iloff2=(/0,0,0/),iuoff2=(/0,0,0/), &
           ielbnd3=(/30,20,10/),ieubnd3=(/43,28,15/),iloff3=(/0,0,0/),iuoff3=(/0,0,0/), &
           correct=correct, checkPtr=.false., rc=rc) 

  ! check coord 3
  ! check allocated staggerloc bounds
  call check2DP1Bnds2x2(grid2D, coordDim=3, staggerloc=ESMF_STAGGERLOC_CORNER_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/10,20,30/),ieubnd0=(/11,24,43/),iloff0=(/0,0,0/),iuoff0=(/0,0,0/), &
           ielbnd1=(/10,20,30/),ieubnd1=(/15,24,43/),iloff1=(/0,0,0/),iuoff1=(/0,0,0/), &
           ielbnd2=(/10,20,30/),ieubnd2=(/11,28,43/),iloff2=(/0,0,0/),iuoff2=(/0,0,0/), &
           ielbnd3=(/10,20,30/),ieubnd3=(/15,28,43/),iloff3=(/0,0,0/),iuoff3=(/0,0,0/), &
           correct=correct, checkPtr=.false., rc=rc) 

  ! Destroy Test Grid
  call ESMF_GridDestroy(grid2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!! Test 2D Plus 1 Bounds with ESMF_GridGet  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test ESMF_GridGet getting bounds from grid"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  correct=.true.
  rc=ESMF_SUCCESS

  ! if petCount >1, setup petMap
  if (petCount .gt. 1) then
     petMap2D(:,1,1)=(/0,1/)
     petMap2D(:,2,1)=(/2,3/)

     grid2D=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/),  &
                              countsPerDeDim3=(/5/),  &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              gridEdgeLWidth=(/1,2,3/), &
                              gridEdgeUWidth=(/4,5,6/), &  
                              petMap=petMap2D, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  else
     grid2D=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/),  &
                              countsPerDeDim3=(/5/),  & 
                              indexflag=ESMF_INDEX_GLOBAL, &
                              gridEdgeLWidth=(/1,2,3/), &
                              gridEdgeUWidth=(/4,5,6/), &  
                              rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  endif

  ! Allocate coordinates
  call ESMF_GridAddCoord(grid2D, staggerEdgeLWidth=(/1,2,3/), staggerEdgeUWidth=(/4,5,6/), &
               staggerloc=ESMF_STAGGERLOC_EDGE2_VFACE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! check allocated staggerloc bounds
  call check2DP1Bnds2x2UsingSLoc(grid2D, staggerloc=ESMF_STAGGERLOC_EDGE2_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/1,2,3/),iuoff0=(/0,0,6/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,2,3/),iuoff1=(/4,0,6/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/1,0,3/),iuoff2=(/0,5,6/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,3/),iuoff3=(/4,5,6/), &
           correct=correct, rc=rc) 

  ! check unallocated staggerloc bounds
  call check2DP1Bnds2x2UsingSLoc(grid2D, staggerloc=ESMF_STAGGERLOC_CORNER_VFACE, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/1,2,3/),iuoff0=(/0,0,6/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,2,3/),iuoff1=(/4,0,6/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/1,0,3/),iuoff2=(/0,5,6/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,3/),iuoff3=(/4,5,6/), &
           correct=correct, rc=rc) 

  ! Destroy Test Grid
  call ESMF_GridDestroy(grid2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!! Test Grid Get Coord XXX !!!!!!!!!!!!!!!!!!!! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test ESMF_GridGetCoord getting coordinate from an index location in grid"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  correct=.true.
  rc=ESMF_SUCCESS

  ! if petCount >1, setup petMap
  if (petCount .gt. 1) then
     petMap2D(:,1,1)=(/0,1/)
     petMap2D(:,2,1)=(/2,3/)

     grid2D=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/),  &
                              countsPerDeDim3=(/5/),  &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              petMap=petMap2D, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  else
     grid2D=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/),  &
                              countsPerDeDim3=(/5/),  & 
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  endif

  ! Allocate coordinates
  call ESMF_GridAddCoord(grid2D, staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get number of local DEs
  call ESMF_GridGet(grid2D, localDECount=localDECount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Get memory and set coords
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(grid2D, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER_VCENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtr3D, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE    

     !! set coord 1  
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
     do i3=clbnd(3),cubnd(3)
        farrayPtr3D(i1,i2,i3)=REAL(i1,ESMF_KIND_R8)
     enddo
     enddo
     enddo

     !! get coord 2
     call ESMF_GridGetCoord(grid2D, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER_VCENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtr3D, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE    

     !! set coord 2  
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
     do i3=clbnd(3),cubnd(3)
        farrayPtr3D(i1,i2,i3)=REAL(i2,ESMF_KIND_R8)
     enddo
     enddo
     enddo

     !! get coord 3
     call ESMF_GridGetCoord(grid2D, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER_VCENTER, coordDim=3, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtr3D, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE    

     ! set coord 3  
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
     do i3=clbnd(3),cubnd(3)
        farrayPtr3D(i1,i2,i3)=REAL(i3,ESMF_KIND_R8)
     enddo
     enddo
     enddo
  enddo    

  !check coords
  do lDE=0,localDECount-1

     ! Get Bounds of DE
     call ESMF_GridGet(grid2D, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER_VCENTER, &
                            computationalLBound=clbnd, computationalUBound=cubnd, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE    

     ! check coord   
     do i1=clbnd(1),cubnd(1)
        index(1)=i1
     do i2=clbnd(2),cubnd(2)
        index(2)=i2
     do i3=clbnd(3),cubnd(3)
        index(3)=i3

        ! Do this to force absoft to not mess-up optimization of loops and index()  
        t=index(1)+index(2)+index(3)

        ! get coords for index location
        call ESMF_GridGetCoord(grid2D, localDE=lDE, staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, &
                              index=index, coord=coord, rc=localrc)
        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE    

        ! check results
        if ((coord(1) .ne. REAL(i1,ESMF_KIND_R8)) .or. &
            (coord(2) .ne. REAL(i2,ESMF_KIND_R8)) .or. &
            (coord(3) .ne. REAL(i3,ESMF_KIND_R8))) then
            write(*,*) "(",i1,",",i2,",",i3,") .ne. (",coord(1),",",coord(2),",",coord(3),")"
            correct=.false.
        endif
     enddo
     enddo
     enddo
     
  enddo

  ! Destroy Test Grid
  call ESMF_GridDestroy(grid2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!! Test 2D Plus 1 Bounds with ESMF_GridGet  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test ESMF_GridGet getting computationalEdgeWidths and bounds from grid"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS
  correct=.true.

  ! if petCount >1, setup petMap
  if (petCount .gt. 1) then
     petMap2D(:,1,1)=(/0,1/)
     petMap2D(:,2,1)=(/2,3/)

     grid2D=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/),  &
                              countsPerDeDim3=(/5/),  &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              gridEdgeLWidth=(/1,2,3/), &
                              gridEdgeUWidth=(/4,5,6/), &  
                              petMap=petMap2D, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  else
     grid2D=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/),  &
                              countsPerDeDim3=(/5/),  & 
                              gridEdgeLWidth=(/1,2,3/), &
                              gridEdgeUWidth=(/4,5,6/), &  
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  endif

  ! Allocate coordinates
  call ESMF_GridAddCoord(grid2D, staggerEdgeLWidth=(/1,2,3/), staggerEdgeUWidth=(/4,5,6/), &
               staggerloc=ESMF_STAGGERLOC_CORNER_VFACE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Destroy Test Grid
  call ESMF_GridDestroy(grid2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!! Test 2D Grid with factorized coordinate arrays !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D Grid Bounds with Factorized Coordinate Arrays"
  write(failMsg, *) "Incorrect result"

  ! init  flags
  rc=ESMF_SUCCESS
  correct=.true.

  ! if petCount >1, setup petMap
  if (petCount .gt. 1) then
     petMap2D(:,1,1)=(/0,1/)
     petMap2D(:,2,1)=(/2,3/)

     grid2D=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/),  &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              coordDep1=(/1/), &
                              coordDep2=(/2/), &
                              petMap=petMap2D, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  else
     grid2D=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/),  &
                              coordDep1=(/1/), &
                              coordDep2=(/2/), &
                               indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  endif

  ! Allocate coordinates
  call ESMF_GridAddCoord(grid2D, &
               staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check coord 1
  call check1DBnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_CENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1/),ieubnd0=(/1/),iloff0=(/0,0/),iuoff0=(/0,0/), &
           ielbnd1=(/2/),ieubnd1=(/3/),iloff1=(/0,0/),iuoff1=(/0,0/), &
           ielbnd2=(/1/),ieubnd2=(/1/),iloff2=(/0,0/),iuoff2=(/0,0/), &
           ielbnd3=(/2/),ieubnd3=(/3/),iloff3=(/0,0/),iuoff3=(/0,0/), &
           correct=correct, rc=rc) 

  ! check coord 2
  call check1DBnds2x2(grid2D, coordDim=2, staggerloc=ESMF_STAGGERLOC_CENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1/),ieubnd0=(/3/),iloff0=(/0,0/),iuoff0=(/0,0/), &
           ielbnd1=(/1/),ieubnd1=(/3/),iloff1=(/0,0/),iuoff1=(/0,0/), &
           ielbnd2=(/4/),ieubnd2=(/7/),iloff2=(/0,0/),iuoff2=(/0,0/), &
           ielbnd3=(/4/),ieubnd3=(/7/),iloff3=(/0,0/),iuoff3=(/0,0/), &
           correct=correct, rc=rc) 

  ! Destroy Test Grid
  call ESMF_GridDestroy(grid2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!! Test 3D Grid with factorized coordinate arrays !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 3D Grid Bounds with 2D plus 1 Factorized Coordinate Arrays"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS
  correct=.true.

  ! if petCount >1, setup petMap
  if (petCount .gt. 1) then
     petMap2D(:,1,1)=(/0,1/)
     petMap2D(:,2,1)=(/2,3/)

     grid2D=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/),  &
                              countsPerDeDim3=(/5/),  &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              coordDep1=(/1,2/), &
                              coordDep2=(/1,2/), &
                              coordDep3=(/3/), &
                              petMap=petMap2D, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  else
     grid2D=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/),  &
                              countsPerDeDim3=(/5/),  &
                              coordDep1=(/1,2/), &
                              coordDep2=(/1,2/), &
                              coordDep3=(/3/), &
                               indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  endif

  ! Allocate coordinates
  call ESMF_GridAddCoord(grid2D, &
               staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check coord 1
  call check2DBnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_CENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1/),ieubnd0=(/1,3/),iloff0=(/0,0/),iuoff0=(/0,0/), &
           ielbnd1=(/2,1/),ieubnd1=(/3,3/),iloff1=(/0,0/),iuoff1=(/0,0/), &
           ielbnd2=(/1,4/),ieubnd2=(/1,7/),iloff2=(/0,0/),iuoff2=(/0,0/), &
           ielbnd3=(/2,4/),ieubnd3=(/3,7/),iloff3=(/0,0/),iuoff3=(/0,0/), &
           correct=correct, rc=rc) 

  ! check coord 2
  call check2DBnds2x2(grid2D, coordDim=2, staggerloc=ESMF_STAGGERLOC_CENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1/),ieubnd0=(/1,3/),iloff0=(/0,0/),iuoff0=(/0,0/), &
           ielbnd1=(/2,1/),ieubnd1=(/3,3/),iloff1=(/0,0/),iuoff1=(/0,0/), &
           ielbnd2=(/1,4/),ieubnd2=(/1,7/),iloff2=(/0,0/),iuoff2=(/0,0/), &
           ielbnd3=(/2,4/),ieubnd3=(/3,7/),iloff3=(/0,0/),iuoff3=(/0,0/), &
           correct=correct, rc=rc) 

  ! check coord 3
  call check1DBnds2x2(grid2D, coordDim=3, staggerloc=ESMF_STAGGERLOC_CENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1/),ieubnd0=(/5/),iloff0=(/0,0/),iuoff0=(/0,0/), &
           ielbnd1=(/1/),ieubnd1=(/5/),iloff1=(/0,0/),iuoff1=(/0,0/), &
           ielbnd2=(/1/),ieubnd2=(/5/),iloff2=(/0,0/),iuoff2=(/0,0/), &
           ielbnd3=(/1/),ieubnd3=(/5/),iloff3=(/0,0/),iuoff3=(/0,0/), &
           correct=correct, rc=rc) 

  ! Destroy Test Grid
  call ESMF_GridDestroy(grid2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!! Test 2D Plus 1 Default Bounds For CreateShapeTileReg !!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D plus 1 GridCreateNoPeriDimReg Bounds"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.

  ! if petCount >1, setup petMap
  if (petCount .gt. 1) then
     petMapReg2D(:,1,1)=(/0,1/)
     petMapReg2D(:,1,2)=(/2,3/)
     grid2D=ESMF_GridCreateNoPeriDim(minIndex=(/1,2,3/),maxIndex=(/3,4,9/), &
                              regDecomp=(/2,1,2/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              petMap=petMapReg2D, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  else
     grid2D=ESMF_GridCreateNoPeriDim(minIndex=(/1,2,3/),maxIndex=(/3,4,9/), &
                              regDecomp=(/2,1,2/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  endif

  ! Allocate Staggers
  call ESMF_GridAddCoord(grid2D, &
               staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! check coord 1
  call check2DP1Bnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,2,3/),ieubnd0=(/2,4,6/),iloff0=(/0,0,0/),iuoff0=(/0,0,0/), &
           ielbnd1=(/3,2,3/),ieubnd1=(/3,4,6/),iloff1=(/0,0,0/),iuoff1=(/0,0,0/), &
           ielbnd2=(/1,2,7/),ieubnd2=(/2,4,9/),iloff2=(/0,0,0/),iuoff2=(/0,0,0/), &
           ielbnd3=(/3,2,7/),ieubnd3=(/3,4,9/),iloff3=(/0,0,0/),iuoff3=(/0,0,0/), &
           correct=correct, rc=rc) 

  ! Destroy Test Grid
  call ESMF_GridDestroy(grid2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!! Test 2D Plus 1 Default Bounds For CreateShapeTileReg !!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D plus 1 GridCreateNoPeriDimReg Bounds with even cell division"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.

  ! if petCount >1, setup petMap
  if (petCount .gt. 1) then
     petMapReg2D(:,1,1)=(/0,1/)
     petMapReg2D(:,1,2)=(/2,3/)
     grid2D=ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/),maxIndex=(/4,6,10/), &
                              regDecomp=(/2,1,2/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              petMap=petMapReg2D, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  else
     grid2D=ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/),maxIndex=(/4,6,10/), &
                              regDecomp=(/2,1,2/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  endif

  ! Allocate Staggers
  call ESMF_GridAddCoord(grid2D, &
               staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! check coord 1
  call check2DP1Bnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/2,6,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,0/), &
           ielbnd1=(/3,1,1/),ieubnd1=(/4,6,5/),iloff1=(/0,0,0/),iuoff1=(/0,0,0/), &
           ielbnd2=(/1,1,6/),ieubnd2=(/2,6,10/),iloff2=(/0,0,0/),iuoff2=(/0,0,0/), &
           ielbnd3=(/3,1,6/),ieubnd3=(/4,6,10/),iloff3=(/0,0,0/),iuoff3=(/0,0,0/), &
           correct=correct, rc=rc) 

  ! Destroy Test Grid
  call ESMF_GridDestroy(grid2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)




  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!! Test 2D Plus 1 Default Bounds For EmptyCompleteReg !!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D plus 1 EmptyCompleteReg Bounds with even cell division"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.

  ! create Empty Grid
  grid2D=ESMF_GridEmptyCreate(rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! if petCount >1, setup petMap
  if (petCount .gt. 1) then
     petMapReg2D(:,1,1)=(/0,1/)
     petMapReg2D(:,1,2)=(/2,3/)
     call ESMF_GridEmptyComplete(grid2D, minIndex=(/1,1,1/),maxIndex=(/4,6,10/), &
                              regDecomp=(/2,1,2/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              petMap=petMapReg2D, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  else
     call ESMF_GridEmptyComplete(grid2D, minIndex=(/1,1,1/),maxIndex=(/4,6,10/), &
                              regDecomp=(/2,1,2/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  endif

  ! Allocate Staggers
  call ESMF_GridAddCoord(grid2D, &
               staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! check coord 1
  call check2DP1Bnds2x2(grid2D, coordDim=1, staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/2,6,5/),iloff0=(/0,0,0/),iuoff0=(/0,0,0/), &
           ielbnd1=(/3,1,1/),ieubnd1=(/4,6,5/),iloff1=(/0,0,0/),iuoff1=(/0,0,0/), &
           ielbnd2=(/1,1,6/),ieubnd2=(/2,6,10/),iloff2=(/0,0,0/),iuoff2=(/0,0,0/), &
           ielbnd3=(/3,1,6/),ieubnd3=(/4,6,10/),iloff3=(/0,0,0/),iuoff3=(/0,0,0/), &
           correct=correct, rc=rc) 


  ! Destroy Test Grid
  call ESMF_GridDestroy(grid2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!! Test 2D Plus 1 Creating an Array at a stagger loc !!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D plus 1 GridCreateArray"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS
  correct=.true.

  ! if petCount >1, setup petMap
  if (petCount .gt. 1) then
     petMap2D(:,1,1)=(/0,1/)
     petMap2D(:,2,1)=(/2,3/)

     grid2D=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/),  &
                              countsPerDeDim3=(/5/),  &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              gridEdgeLWidth=(/1,2,3/), &
                              gridEdgeUWidth=(/4,5,6/), &  
                              petMap=petMap2D, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  else
     grid2D=ESMF_GridCreateNoPeriDim(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/),  &
                              countsPerDeDim3=(/5/),  & 
                              gridEdgeLWidth=(/1,2,3/), &
                              gridEdgeUWidth=(/4,5,6/), &  
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  endif

  ! Allocate Staggers
  call ESMF_GridAddCoord(grid2D, staggerEdgeLWidth=(/1,2,3/), staggerEdgeUWidth=(/4,5,6/), &
               staggerloc=ESMF_STAGGERLOC_EDGE2_VFACE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Create an array on an unallocated stagger location
  array=ESMF_ArrayCreateFromGrid(grid2D, staggerloc=ESMF_STAGGERLOC_CENTER, &
                             rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! make sure array is valid
  call ESMF_ArrayValidate(array,rc=localrc)  
  if (localrc .ne. ESMF_SUCCESS) correct=.false.

  ! Destroy array
  call ESMF_ArrayDestroy(array,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.

  ! Create an array on an allocated stagger location
  array=ESMF_ArrayCreateFromGrid(grid2D, staggerloc=ESMF_STAGGERLOC_EDGE2_VFACE, &
                             rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! TODO: make sure Array bounds are correct

  ! make sure array is valid
  call ESMF_ArrayValidate(array,rc=localrc)  
  if (localrc .ne. ESMF_SUCCESS) correct=.false.

  ! TODO: make sure Array bounds are correct

  ! Destroy array
  call ESMF_ArrayDestroy(array,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.

  ! Destroy Test Grid
  call ESMF_GridDestroy(grid2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Test 3D coordinate allocation !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#endif
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 3D GridAddCoord, by allocating coordinates and then getting fortran pointer"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS

  ! Set Coord From Array
  call ESMF_GridAddCoord(grid3D, &
               staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! set pointer to null
  nullify(farrayPtr3D)

  ! Get Coord From Grid
  call ESMF_GridGetCoord(grid3D, localDE=0, &
            staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, farrayPtr=farrayPtr3D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Check that output is as expected
  correct=.true.
  if (.not. associated(farrayPtr3D)) correct=.false.

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  call ESMF_GridDestroy(grid3D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Destroy Test Grid
  call ESMF_DistGridDestroy(distgrid2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Destroy Test Grid
  call ESMF_DistGridDestroy(distgrid3D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

contains


subroutine check2DBnds2x2(grid, coordDim, staggerloc, localPet, petCount, &
                          ielbnd0,ieubnd0,iloff0,iuoff0, &
                          ielbnd1,ieubnd1,iloff1,iuoff1, &
                          ielbnd2,ieubnd2,iloff2,iuoff2, &
                          ielbnd3,ieubnd3,iloff3,iuoff3, &
                          correct, rc)

  type (ESMF_Grid) :: grid
  type (ESMF_StaggerLoc),intent(in) :: staggerloc
  integer,intent(in) :: coordDim, localPet, petCount
  integer,intent(in) :: ielbnd0(:),ieubnd0(:),iloff0(:),iuoff0(:)
  integer,intent(in) :: ielbnd1(:),ieubnd1(:),iloff1(:),iuoff1(:)
  integer,intent(in) :: ielbnd2(:),ieubnd2(:),iloff2(:),iuoff2(:)
  integer,intent(in) :: ielbnd3(:),ieubnd3(:),iloff3(:),iuoff3(:)
  logical,intent(inout) :: correct
  integer,intent(inout) :: rc
  
  integer :: localrc
  integer :: elbnd(2),eubnd(2),ecnt(2)
  integer :: clbnd(2),cubnd(2),ccnt(2)
  integer :: tlbnd(2),tubnd(2),tcnt(2)
  real (ESMF_KIND_R8), pointer :: farrayPtr(:,:)

  ! Check if bounds are correct for each DE
  if (petCount .eq. 1) then
      ! Note the order of DE's here is dependant on the ordering
      ! in ESMF_GridCreateNoPeriDim, if that changes then this will
      ! probably have to change also. 

      ! check DE 0
      call ESMF_GridGetCoordBounds(grid, coordDim=coordDim, localDE=0, &
             staggerLoc=staggerloc,                  &
             exclusiveLBound=elbnd, exclusiveUBound=eubnd, exclusiveCount=ecnt,       &
             computationalLBound=clbnd, computationalUBound=cubnd, computationalCount=ccnt,     &
             totalLBound=tlbnd, totalUBound=tubnd, totalCount=tcnt, rc=localrc)
             if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!    write(*,*) "0:",clbnd,",",cubnd,correct

     !! set pointer to null
     nullify(farrayPtr)

     !! Get Coord Array From Grid
     call ESMF_GridGetCoord(grid, localDE=0, &
              staggerLoc=staggerloc, coordDim=coordDim, farrayPtr=farrayPtr, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!write(*,*) "H1", rc, correct
     !! Check that output is as expected
     if (.not. associated(farrayPtr)) correct=.false.
     if ((lbound(farrayPtr,1) .ne. tlbnd(1)) .or. (lbound(farrayPtr,2) .ne. tlbnd(2))) correct=.false.
     if ((ubound(farrayPtr,1) .ne. tubnd(1)) .or. (ubound(farrayPtr,2) .ne. tubnd(2))) correct=.false.
!write(*,*) "H2", rc, correct, elbnd(1), ielbnd0(1)-iloff0(1), ":", elbnd(2), ielbnd0(2)-iloff0(2)
     if (elbnd(1) .ne. ielbnd0(1)-iloff0(1)) correct=.false.
     if (elbnd(2) .ne. ielbnd0(2)-iloff0(2)) correct=.false.
!write(*,*) "H2.1", rc, correct
     if (eubnd(1) .ne. ieubnd0(1)+iuoff0(1)) correct=.false.
     if (eubnd(2) .ne. ieubnd0(2)+iuoff0(2)) correct=.false.
!write(*,*) "H2.2", rc, correct
     if (ecnt(1) .ne. ieubnd0(1)-ielbnd0(1)+1+iloff0(1)+iuoff0(1)) correct=.false.
     if (ecnt(2) .ne. ieubnd0(2)-ielbnd0(2)+1+iloff0(2)+iuoff0(2)) correct=.false.
!write(*,*) "H3", rc, correct
     if (clbnd(1) .ne. ielbnd0(1)-iloff0(1)) correct=.false.
     if (clbnd(2) .ne. ielbnd0(2)-iloff0(2)) correct=.false.
     if (cubnd(1) .ne. ieubnd0(1)+iuoff0(1)) correct=.false.
     if (cubnd(2) .ne. ieubnd0(2)+iuoff0(2)) correct=.false.
     if (ccnt(1) .ne. ieubnd0(1)-ielbnd0(1)+1+iloff0(1)+iuoff0(1)) correct=.false.
     if (ccnt(2) .ne. ieubnd0(2)-ielbnd0(2)+1+iloff0(2)+iuoff0(2)) correct=.false.
!write(*,*) "H4", rc, correct
     if (tlbnd(1) .ne. ielbnd0(1)-iloff0(1)) correct=.false.
     if (tlbnd(2) .ne. ielbnd0(2)-iloff0(2)) correct=.false.
     if (tubnd(1) .ne. ieubnd0(1)+iuoff0(1)) correct=.false.
     if (tubnd(2) .ne. ieubnd0(2)+iuoff0(2)) correct=.false.
     if (tcnt(1) .ne. ieubnd0(1)-ielbnd0(1)+1+iloff0(1)+iuoff0(1)) correct=.false.
     if (tcnt(2) .ne. ieubnd0(2)-ielbnd0(2)+1+iloff0(2)+iuoff0(2)) correct=.false.
!write(*,*) "H5", rc, correct


      ! check DE 1
      call ESMF_GridGetCoordBounds(grid2D, coordDim=coordDim, localDE=1, &
             staggerLoc=staggerloc,                  &
             exclusiveLBound=elbnd, exclusiveUBound=eubnd,  exclusiveCount=ecnt,      &
             computationalLBound=clbnd, computationalUBound=cubnd, computationalCount=ccnt,          &
             totalLBound=tlbnd, totalUBound=tubnd, totalCount=tcnt, rc=localrc)
             if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!  write(*,*) "1:",clbnd,",",cubnd,correct

     !! set pointer to null
     nullify(farrayPtr)

     !! Get Coord From Grid
     call ESMF_GridGetCoord(grid, localDE=1, &
              staggerLoc=staggerloc, coordDim=coordDim, farrayPtr=farrayPtr, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     !! Check that output is as expected
     if (.not. associated(farrayPtr)) correct=.false.
     if ((lbound(farrayPtr,1) .ne. tlbnd(1)) .or. (lbound(farrayPtr,2) .ne. tlbnd(2))) correct=.false.
     if ((ubound(farrayPtr,1) .ne. tubnd(1)) .or. (ubound(farrayPtr,2) .ne. tubnd(2))) correct=.false.


     if (elbnd(1) .ne. ielbnd1(1)-iloff1(1)) correct=.false.
     if (elbnd(2) .ne. ielbnd1(2)-iloff1(2)) correct=.false.
     if (eubnd(1) .ne. ieubnd1(1)+iuoff1(1)) correct=.false.
     if (eubnd(2) .ne. ieubnd1(2)+iuoff1(2)) correct=.false.
     if (ecnt(1) .ne. ieubnd1(1)-ielbnd1(1)+1+iloff1(1)+iuoff1(1)) correct=.false.
     if (ecnt(2) .ne. ieubnd1(2)-ielbnd1(2)+1+iloff1(2)+iuoff1(2)) correct=.false.

     if (clbnd(1) .ne. ielbnd1(1)-iloff1(1)) correct=.false.
     if (clbnd(2) .ne. ielbnd1(2)-iloff1(2)) correct=.false.
     if (cubnd(1) .ne. ieubnd1(1)+iuoff1(1)) correct=.false.
     if (cubnd(2) .ne. ieubnd1(2)+iuoff1(2)) correct=.false.
     if (ccnt(1) .ne. ieubnd1(1)-ielbnd1(1)+1+iloff1(1)+iuoff1(1)) correct=.false.
     if (ccnt(2) .ne. ieubnd1(2)-ielbnd1(2)+1+iloff1(2)+iuoff1(2)) correct=.false.

     if (tlbnd(1) .ne. ielbnd1(1)-iloff1(1)) correct=.false.
     if (tlbnd(2) .ne. ielbnd1(2)-iloff1(2)) correct=.false.
     if (tubnd(1) .ne. ieubnd1(1)+iuoff1(1)) correct=.false.
     if (tubnd(2) .ne. ieubnd1(2)+iuoff1(2)) correct=.false.
     if (tcnt(1) .ne. ieubnd1(1)-ielbnd1(1)+1+iloff1(1)+iuoff1(1)) correct=.false.
     if (tcnt(2) .ne. ieubnd1(2)-ielbnd1(2)+1+iloff1(2)+iuoff1(2)) correct=.false.

      ! check DE 2
      call ESMF_GridGetCoordBounds(grid, coordDim=coordDim, localDE=2, &
             staggerLoc=staggerloc,                  &
             exclusiveLBound=elbnd, exclusiveUBound=eubnd,  exclusiveCount=ecnt,      &
             computationalLBound=clbnd, computationalUBound=cubnd, computationalCount=ccnt,          &
             totalLBound=tlbnd, totalUBound=tubnd, totalCount=tcnt, rc=localrc)
             if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!  write(*,*) "2:",clbnd,",",cubnd,correct
     !! set pointer to null
     nullify(farrayPtr)

     !! Get Coord From Grid
     call ESMF_GridGetCoord(grid, localDE=2, &
              staggerLoc=staggerloc, coordDim=coordDim, farrayPtr=farrayPtr, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     !! Check that output is as expected
     if (.not. associated(farrayPtr)) correct=.false.
     if ((lbound(farrayPtr,1) .ne. tlbnd(1)) .or. (lbound(farrayPtr,2) .ne. tlbnd(2))) correct=.false.
     if ((ubound(farrayPtr,1) .ne. tubnd(1)) .or. (ubound(farrayPtr,2) .ne. tubnd(2))) correct=.false.
 
     if (elbnd(1) .ne. ielbnd2(1)-iloff2(1)) correct=.false.
     if (elbnd(2) .ne. ielbnd2(2)-iloff2(2)) correct=.false.
     if (eubnd(1) .ne. ieubnd2(1)+iuoff2(1)) correct=.false.
     if (eubnd(2) .ne. ieubnd2(2)+iuoff2(2)) correct=.false.
     if (ecnt(1) .ne. ieubnd2(1)-ielbnd2(1)+1+iloff2(1)+iuoff2(1)) correct=.false.
     if (ecnt(2) .ne. ieubnd2(2)-ielbnd2(2)+1+iloff2(2)+iuoff2(2)) correct=.false.

     if (clbnd(1) .ne. ielbnd2(1)-iloff2(1)) correct=.false.
     if (clbnd(2) .ne. ielbnd2(2)-iloff2(2)) correct=.false.
     if (cubnd(1) .ne. ieubnd2(1)+iuoff2(1)) correct=.false.
     if (cubnd(2) .ne. ieubnd2(2)+iuoff2(2)) correct=.false.
     if (ccnt(1) .ne. ieubnd2(1)-ielbnd2(1)+1+iloff2(1)+iuoff2(1)) correct=.false.
     if (ccnt(2) .ne. ieubnd2(2)-ielbnd2(2)+1+iloff2(2)+iuoff2(2)) correct=.false.

     if (tlbnd(1) .ne. ielbnd2(1)-iloff2(1)) correct=.false.
     if (tlbnd(2) .ne. ielbnd2(2)-iloff2(2)) correct=.false.
     if (tubnd(1) .ne. ieubnd2(1)+iuoff2(1)) correct=.false.
     if (tubnd(2) .ne. ieubnd2(2)+iuoff2(2)) correct=.false.
     if (tcnt(1) .ne. ieubnd2(1)-ielbnd2(1)+1+iloff2(1)+iuoff2(1)) correct=.false.
     if (tcnt(2) .ne. ieubnd2(2)-ielbnd2(2)+1+iloff2(2)+iuoff2(2)) correct=.false.

      ! check DE 3
      call ESMF_GridGetCoordBounds(grid, coordDim=coordDim, localDE=3, &
             staggerLoc=staggerloc,                  &
             exclusiveLBound=elbnd, exclusiveUBound=eubnd, exclusiveCount=ecnt,       &
             computationalLBound=clbnd, computationalUBound=cubnd, computationalCount=ccnt,          &
             totalLBound=tlbnd, totalUBound=tubnd, totalCount=tcnt, rc=localrc)
             if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!    write(*,*) "3:",clbnd,",",cubnd,correct
     !! set pointer to null
     nullify(farrayPtr)

     !! Get Coord From Grid
     call ESMF_GridGetCoord(grid, localDE=3, &
              staggerLoc=staggerloc, coordDim=coordDim, farrayPtr=farrayPtr, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     !! Check that output is as expected
     if (.not. associated(farrayPtr)) correct=.false.
     if ((lbound(farrayPtr,1) .ne. tlbnd(1)) .or. (lbound(farrayPtr,2) .ne. tlbnd(2))) correct=.false.
     if ((ubound(farrayPtr,1) .ne. tubnd(1)) .or. (ubound(farrayPtr,2) .ne. tubnd(2))) correct=.false.

     if (elbnd(1) .ne. ielbnd3(1)-iloff3(1)) correct=.false.
     if (elbnd(2) .ne. ielbnd3(2)-iloff3(2)) correct=.false.
     if (eubnd(1) .ne. ieubnd3(1)+iuoff3(1)) correct=.false.
     if (eubnd(2) .ne. ieubnd3(2)+iuoff3(2)) correct=.false.
     if (ecnt(1) .ne. ieubnd3(1)-ielbnd3(1)+1+iloff3(1)+iuoff3(1)) correct=.false.
     if (ecnt(2) .ne. ieubnd3(2)-ielbnd3(2)+1+iloff3(2)+iuoff3(2)) correct=.false.

     if (clbnd(1) .ne. ielbnd3(1)-iloff3(1)) correct=.false.
     if (clbnd(2) .ne. ielbnd3(2)-iloff3(2)) correct=.false.
     if (cubnd(1) .ne. ieubnd3(1)+iuoff3(1)) correct=.false.
     if (cubnd(2) .ne. ieubnd3(2)+iuoff3(2)) correct=.false.
     if (ccnt(1) .ne. ieubnd3(1)-ielbnd3(1)+1+iloff3(1)+iuoff3(1)) correct=.false.
     if (ccnt(2) .ne. ieubnd3(2)-ielbnd3(2)+1+iloff3(2)+iuoff3(2)) correct=.false.

     if (tlbnd(1) .ne. ielbnd3(1)-iloff3(1)) correct=.false.
     if (tlbnd(2) .ne. ielbnd3(2)-iloff3(2)) correct=.false.
     if (tubnd(1) .ne. ieubnd3(1)+iuoff3(1)) correct=.false.
     if (tubnd(2) .ne. ieubnd3(2)+iuoff3(2)) correct=.false.
     if (tcnt(1) .ne. ieubnd3(1)-ielbnd3(1)+1+iloff3(1)+iuoff3(1)) correct=.false.
     if (tcnt(2) .ne. ieubnd3(2)-ielbnd3(2)+1+iloff3(2)+iuoff3(2)) correct=.false.


  else  if (petCount .eq. 4) then
      call ESMF_GridGetCoordBounds(grid, coordDim=coordDim, localDE=0, &
             staggerLoc=staggerloc,                  &
             exclusiveLBound=elbnd, exclusiveUBound=eubnd,  exclusiveCount=ecnt,      &
             computationalLBound=clbnd, computationalUBound=cubnd, computationalCount=ccnt,          &
             totalLBound=tlbnd, totalUBound=tubnd, totalCount=tcnt, rc=localrc)
             if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      ! set pointer to null
      nullify(farrayPtr)

     ! Get Coord From Grid
     call ESMF_GridGetCoord(grid, localDE=0, &
              staggerLoc=staggerloc, coordDim=coordDim, farrayPtr=farrayPtr, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     ! Check that output is as expected
     if (.not. associated(farrayPtr)) correct=.false.
     if ((lbound(farrayPtr,1) .ne. tlbnd(1)) .or. (lbound(farrayPtr,2) .ne. tlbnd(2))) correct=.false.
     if ((ubound(farrayPtr,1) .ne. tubnd(1)) .or. (ubound(farrayPtr,2) .ne. tubnd(2))) correct=.false.

     if (localPet .eq. 0) then

        if (elbnd(1) .ne. ielbnd0(1)-iloff0(1)) correct=.false.
        if (elbnd(2) .ne. ielbnd0(2)-iloff0(2)) correct=.false.
        if (eubnd(1) .ne. ieubnd0(1)+iuoff0(1)) correct=.false.
        if (eubnd(2) .ne. ieubnd0(2)+iuoff0(2)) correct=.false.
        if (ecnt(1) .ne. ieubnd0(1)-ielbnd0(1)+1+iloff0(1)+iuoff0(1)) correct=.false.
        if (ecnt(2) .ne. ieubnd0(2)-ielbnd0(2)+1+iloff0(2)+iuoff0(2)) correct=.false.

        if (clbnd(1) .ne. ielbnd0(1)-iloff0(1)) correct=.false.
        if (clbnd(2) .ne. ielbnd0(2)-iloff0(2)) correct=.false.
        if (cubnd(1) .ne. ieubnd0(1)+iuoff0(1)) correct=.false.
        if (cubnd(2) .ne. ieubnd0(2)+iuoff0(2)) correct=.false.
        if (ccnt(1) .ne. ieubnd0(1)-ielbnd0(1)+1+iloff0(1)+iuoff0(1)) correct=.false.
        if (ccnt(2) .ne. ieubnd0(2)-ielbnd0(2)+1+iloff0(2)+iuoff0(2)) correct=.false.

        if (tlbnd(1) .ne. ielbnd0(1)-iloff0(1)) correct=.false.
        if (tlbnd(2) .ne. ielbnd0(2)-iloff0(2)) correct=.false.
        if (tubnd(1) .ne. ieubnd0(1)+iuoff0(1)) correct=.false.
        if (tubnd(2) .ne. ieubnd0(2)+iuoff0(2)) correct=.false.
        if (tcnt(1) .ne. ieubnd0(1)-ielbnd0(1)+1+iloff0(1)+iuoff0(1)) correct=.false.
        if (tcnt(2) .ne. ieubnd0(2)-ielbnd0(2)+1+iloff0(2)+iuoff0(2)) correct=.false.


     else if (localPet .eq. 1) then

        if (elbnd(1) .ne. ielbnd1(1)-iloff1(1)) correct=.false.
        if (elbnd(2) .ne. ielbnd1(2)-iloff1(2)) correct=.false.
        if (eubnd(1) .ne. ieubnd1(1)+iuoff1(1)) correct=.false.
        if (eubnd(2) .ne. ieubnd1(2)+iuoff1(2)) correct=.false.
        if (ecnt(1) .ne. ieubnd1(1)-ielbnd1(1)+1+iloff1(1)+iuoff1(1)) correct=.false.
        if (ecnt(2) .ne. ieubnd1(2)-ielbnd1(2)+1+iloff1(2)+iuoff1(2)) correct=.false.

        if (clbnd(1) .ne. ielbnd1(1)-iloff1(1)) correct=.false.
        if (clbnd(2) .ne. ielbnd1(2)-iloff1(2)) correct=.false.
        if (cubnd(1) .ne. ieubnd1(1)+iuoff1(1)) correct=.false.
        if (cubnd(2) .ne. ieubnd1(2)+iuoff1(2)) correct=.false.
        if (ccnt(1) .ne. ieubnd1(1)-ielbnd1(1)+1+iloff1(1)+iuoff1(1)) correct=.false.
        if (ccnt(2) .ne. ieubnd1(2)-ielbnd1(2)+1+iloff1(2)+iuoff1(2)) correct=.false.

        if (tlbnd(1) .ne. ielbnd1(1)-iloff1(1)) correct=.false.
        if (tlbnd(2) .ne. ielbnd1(2)-iloff1(2)) correct=.false.
        if (tubnd(1) .ne. ieubnd1(1)+iuoff1(1)) correct=.false.
        if (tubnd(2) .ne. ieubnd1(2)+iuoff1(2)) correct=.false.
        if (tcnt(1) .ne. ieubnd1(1)-ielbnd1(1)+1+iloff1(1)+iuoff1(1)) correct=.false.
        if (tcnt(2) .ne. ieubnd1(2)-ielbnd1(2)+1+iloff1(2)+iuoff1(2)) correct=.false.

     else if (localPet .eq. 2) then

        if (elbnd(1) .ne. ielbnd2(1)-iloff2(1)) correct=.false.
        if (elbnd(2) .ne. ielbnd2(2)-iloff2(2)) correct=.false.
        if (eubnd(1) .ne. ieubnd2(1)+iuoff2(1)) correct=.false.
        if (eubnd(2) .ne. ieubnd2(2)+iuoff2(2)) correct=.false.
        if (ecnt(1) .ne. ieubnd2(1)-ielbnd2(1)+1+iloff2(1)+iuoff2(1)) correct=.false.
        if (ecnt(2) .ne. ieubnd2(2)-ielbnd2(2)+1+iloff2(2)+iuoff2(2)) correct=.false.

        if (clbnd(1) .ne. ielbnd2(1)-iloff2(1)) correct=.false.
        if (clbnd(2) .ne. ielbnd2(2)-iloff2(2)) correct=.false.
        if (cubnd(1) .ne. ieubnd2(1)+iuoff2(1)) correct=.false.
        if (cubnd(2) .ne. ieubnd2(2)+iuoff2(2)) correct=.false.
        if (ccnt(1) .ne. ieubnd2(1)-ielbnd2(1)+1+iloff2(1)+iuoff2(1)) correct=.false.
        if (ccnt(2) .ne. ieubnd2(2)-ielbnd2(2)+1+iloff2(2)+iuoff2(2)) correct=.false.

        if (tlbnd(1) .ne. ielbnd2(1)-iloff2(1)) correct=.false.
        if (tlbnd(2) .ne. ielbnd2(2)-iloff2(2)) correct=.false.
        if (tubnd(1) .ne. ieubnd2(1)+iuoff2(1)) correct=.false.
        if (tubnd(2) .ne. ieubnd2(2)+iuoff2(2)) correct=.false.
        if (tcnt(1) .ne. ieubnd2(1)-ielbnd2(1)+1+iloff2(1)+iuoff2(1)) correct=.false.
        if (tcnt(2) .ne. ieubnd2(2)-ielbnd2(2)+1+iloff2(2)+iuoff2(2)) correct=.false.


     else if (localPet .eq. 3) then

        if (elbnd(1) .ne. ielbnd3(1)-iloff3(1)) correct=.false.
        if (elbnd(2) .ne. ielbnd3(2)-iloff3(2)) correct=.false.
        if (eubnd(1) .ne. ieubnd3(1)+iuoff3(1)) correct=.false.
        if (eubnd(2) .ne. ieubnd3(2)+iuoff3(2)) correct=.false.
        if (ecnt(1) .ne. ieubnd3(1)-ielbnd3(1)+1+iloff3(1)+iuoff3(1)) correct=.false.
        if (ecnt(2) .ne. ieubnd3(2)-ielbnd3(2)+1+iloff3(2)+iuoff3(2)) correct=.false.

        if (clbnd(1) .ne. ielbnd3(1)-iloff3(1)) correct=.false.
        if (clbnd(2) .ne. ielbnd3(2)-iloff3(2)) correct=.false.
        if (cubnd(1) .ne. ieubnd3(1)+iuoff3(1)) correct=.false.
        if (cubnd(2) .ne. ieubnd3(2)+iuoff3(2)) correct=.false.
        if (ccnt(1) .ne. ieubnd3(1)-ielbnd3(1)+1+iloff3(1)+iuoff3(1)) correct=.false.
        if (ccnt(2) .ne. ieubnd3(2)-ielbnd3(2)+1+iloff3(2)+iuoff3(2)) correct=.false.

        if (tlbnd(1) .ne. ielbnd3(1)-iloff3(1)) correct=.false.
        if (tlbnd(2) .ne. ielbnd3(2)-iloff3(2)) correct=.false.
        if (tubnd(1) .ne. ieubnd3(1)+iuoff3(1)) correct=.false.
        if (tubnd(2) .ne. ieubnd3(2)+iuoff3(2)) correct=.false.
        if (tcnt(1) .ne. ieubnd3(1)-ielbnd3(1)+1+iloff3(1)+iuoff3(1)) correct=.false.
        if (tcnt(2) .ne. ieubnd3(2)-ielbnd3(2)+1+iloff3(2)+iuoff3(2)) correct=.false.

     endif
  endif
end subroutine check2DBnds2x2

subroutine check1DBnds2x2(grid, coordDim, staggerloc, localPet, petCount, &
                          ielbnd0,ieubnd0,iloff0,iuoff0, &
                          ielbnd1,ieubnd1,iloff1,iuoff1, &
                          ielbnd2,ieubnd2,iloff2,iuoff2, &
                          ielbnd3,ieubnd3,iloff3,iuoff3, &
                          correct, rc)

  type (ESMF_Grid) :: grid
  type (ESMF_StaggerLoc),intent(in) :: staggerloc
  integer,intent(in) :: coordDim, localPet, petCount
  integer,intent(in) :: ielbnd0(:),ieubnd0(:),iloff0(:),iuoff0(:)
  integer,intent(in) :: ielbnd1(:),ieubnd1(:),iloff1(:),iuoff1(:)
  integer,intent(in) :: ielbnd2(:),ieubnd2(:),iloff2(:),iuoff2(:)
  integer,intent(in) :: ielbnd3(:),ieubnd3(:),iloff3(:),iuoff3(:)
  logical,intent(inout) :: correct
  integer,intent(inout) :: rc
  
  integer :: localrc
  integer :: elbnd(1),eubnd(1),ecnt(1)
  integer :: clbnd(1),cubnd(1),ccnt(1)
  integer :: tlbnd(1),tubnd(1),tcnt(1)
  real (ESMF_KIND_R8), pointer :: farrayPtr(:)


  ! Check if bounds are correct for each DE
  if (petCount .eq. 1) then
      ! Note the order of DE's here is dependant on the ordering
      ! in ESMF_GridCreateNoPeriDim, if that changes then this will
      ! probably have to change also. 

      ! check DE 0
      call ESMF_GridGetCoordBounds(grid, coordDim=coordDim, localDE=0, &
             staggerLoc=staggerloc,                  &
             exclusiveLBound=elbnd, exclusiveUBound=eubnd,  exclusiveCount=ecnt,     &
             computationalLBound=clbnd, computationalUBound=cubnd,  computationalCount=ccnt,     &
             totalLBound=tlbnd, totalUBound=tubnd, totalCount=tcnt, rc=localrc)
             if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

!    write(*,*) "0:",clbnd,",",cubnd,correct

     !! set pointer to null
     nullify(farrayPtr)
 
     !! Get Coord Array From Grid
     call ESMF_GridGetCoord(grid, localDE=0, &
              staggerLoc=staggerloc, coordDim=coordDim, farrayPtr=farrayPtr, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     !! Check that output is as expected
     if (.not. associated(farrayPtr)) correct=.false.
     if (lbound(farrayPtr,1) .ne. tlbnd(1)) correct=.false.
     if (ubound(farrayPtr,1) .ne. tubnd(1)) correct=.false.

     if (elbnd(1) .ne. ielbnd0(1)-iloff0(1)) correct=.false.
     if (eubnd(1) .ne. ieubnd0(1)+iuoff0(1)) correct=.false.
     if (ecnt(1) .ne. ieubnd0(1)-ielbnd0(1)+1+iloff0(1)+iuoff0(1)) correct=.false.

     if (clbnd(1) .ne. ielbnd0(1)-iloff0(1)) correct=.false.
     if (cubnd(1) .ne. ieubnd0(1)+iuoff0(1)) correct=.false.
     if (ccnt(1) .ne. ieubnd0(1)-ielbnd0(1)+1+iloff0(1)+iuoff0(1)) correct=.false.

     if (tlbnd(1) .ne. ielbnd0(1)-iloff0(1)) correct=.false.
     if (tubnd(1) .ne. ieubnd0(1)+iuoff0(1)) correct=.false.
     if (tcnt(1) .ne. ieubnd0(1)-ielbnd0(1)+1+iloff0(1)+iuoff0(1)) correct=.false.


      ! check DE 1
      call ESMF_GridGetCoordBounds(grid2D, coordDim=coordDim, localDE=1, &
             staggerLoc=staggerloc,                  &
             exclusiveLBound=elbnd, exclusiveUBound=eubnd,   exclusiveCount=ecnt,     &
             computationalLBound=clbnd, computationalUBound=cubnd, computationalCount=ccnt,  &
             totalLBound=tlbnd, totalUBound=tubnd, totalCount=tcnt, rc=localrc)
             if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

!    write(*,*) "1:",clbnd,",",cubnd,correct

     !! set pointer to null
     nullify(farrayPtr)

     !! Get Coord From Grid
     call ESMF_GridGetCoord(grid, localDE=1, &
              staggerLoc=staggerloc, coordDim=coordDim, farrayPtr=farrayPtr, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     !! Check that output is as expected
     if (.not. associated(farrayPtr)) correct=.false.
     if (lbound(farrayPtr,1) .ne. tlbnd(1)) correct=.false.
     if (ubound(farrayPtr,1) .ne. tubnd(1)) correct=.false.

     if (elbnd(1) .ne. ielbnd1(1)-iloff1(1)) correct=.false.
     if (eubnd(1) .ne. ieubnd1(1)+iuoff1(1)) correct=.false.
     if (ecnt(1) .ne. ieubnd1(1)-ielbnd1(1)+1+iloff1(1)+iuoff1(1)) correct=.false.

     if (clbnd(1) .ne. ielbnd1(1)-iloff1(1)) correct=.false.
     if (cubnd(1) .ne. ieubnd1(1)+iuoff1(1)) correct=.false.
     if (ccnt(1) .ne. ieubnd1(1)-ielbnd1(1)+1+iloff1(1)+iuoff1(1)) correct=.false.

     if (tlbnd(1) .ne. ielbnd1(1)-iloff1(1)) correct=.false.
     if (tubnd(1) .ne. ieubnd1(1)+iuoff1(1)) correct=.false.
     if (tcnt(1) .ne. ieubnd1(1)-ielbnd1(1)+1+iloff1(1)+iuoff1(1)) correct=.false.

      ! check DE 2
      call ESMF_GridGetCoordBounds(grid, coordDim=coordDim, localDE=2, &
             staggerLoc=staggerloc,                  &
             exclusiveLBound=elbnd, exclusiveUBound=eubnd,   exclusiveCount=ecnt,     &
             computationalLBound=clbnd, computationalUBound=cubnd, computationalCount=ccnt,   &
             totalLBound=tlbnd, totalUBound=tubnd, totalCount=tcnt, rc=localrc)
             if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
 !   write(*,*) "2:",clbnd,",",cubnd,correct
     !! set pointer to null
     nullify(farrayPtr)

     !! Get Coord From Grid
     call ESMF_GridGetCoord(grid, localDE=2, &
              staggerLoc=staggerloc, coordDim=coordDim, farrayPtr=farrayPtr, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     !! Check that output is as expected
     if (.not. associated(farrayPtr)) correct=.false.
     if (lbound(farrayPtr,1) .ne. tlbnd(1)) correct=.false.
     if (ubound(farrayPtr,1) .ne. tubnd(1)) correct=.false.
 

     if (elbnd(1) .ne. ielbnd2(1)-iloff2(1)) correct=.false.
     if (eubnd(1) .ne. ieubnd2(1)+iuoff2(1)) correct=.false.
     if (ecnt(1) .ne. ieubnd2(1)-ielbnd2(1)+1+iloff2(1)+iuoff2(1)) correct=.false.

     if (clbnd(1) .ne. ielbnd2(1)-iloff2(1)) correct=.false.
     if (cubnd(1) .ne. ieubnd2(1)+iuoff2(1)) correct=.false.
     if (ccnt(1) .ne. ieubnd2(1)-ielbnd2(1)+1+iloff2(1)+iuoff2(1)) correct=.false.

     if (tlbnd(1) .ne. ielbnd2(1)-iloff2(1)) correct=.false.
     if (tubnd(1) .ne. ieubnd2(1)+iuoff2(1)) correct=.false.
     if (tcnt(1) .ne. ieubnd2(1)-ielbnd2(1)+1+iloff2(1)+iuoff2(1)) correct=.false.


      ! check DE 3
      call ESMF_GridGetCoordBounds(grid, coordDim=coordDim, localDE=3, &
             staggerLoc=staggerloc,                  &
             exclusiveLBound=elbnd, exclusiveUBound=eubnd,   exclusiveCount=ecnt,     &
             computationalLBound=clbnd, computationalUBound=cubnd, computationalCount=ccnt,   &
             totalLBound=tlbnd, totalUBound=tubnd, totalCount=tcnt, rc=localrc)
             if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
 !    write(*,*) "3:",clbnd,",",cubnd,correct
     !! set pointer to null
     nullify(farrayPtr)

     !! Get Coord From Grid
     call ESMF_GridGetCoord(grid, localDE=3, &
              staggerLoc=staggerloc, coordDim=coordDim, farrayPtr=farrayPtr, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     !! Check that output is as expected
     if (.not. associated(farrayPtr)) correct=.false.
     if (lbound(farrayPtr,1) .ne. tlbnd(1)) correct=.false.
     if (ubound(farrayPtr,1) .ne. tubnd(1)) correct=.false.

     if (elbnd(1) .ne. ielbnd3(1)-iloff3(1)) correct=.false.
     if (eubnd(1) .ne. ieubnd3(1)+iuoff3(1)) correct=.false.
     if (ecnt(1) .ne. ieubnd3(1)-ielbnd3(1)+1+iloff3(1)+iuoff3(1)) correct=.false.

     if (clbnd(1) .ne. ielbnd3(1)-iloff3(1)) correct=.false.
     if (cubnd(1) .ne. ieubnd3(1)+iuoff3(1)) correct=.false.
     if (ccnt(1) .ne. ieubnd3(1)-ielbnd3(1)+1+iloff3(1)+iuoff3(1)) correct=.false.

     if (tlbnd(1) .ne. ielbnd3(1)-iloff3(1)) correct=.false.
     if (tubnd(1) .ne. ieubnd3(1)+iuoff3(1)) correct=.false.
     if (tcnt(1) .ne. ieubnd3(1)-ielbnd3(1)+1+iloff3(1)+iuoff3(1)) correct=.false.

  else  if (petCount .eq. 4) then
      call ESMF_GridGetCoordBounds(grid, coordDim=coordDim, localDE=0, &
             staggerLoc=staggerloc,                  &
             exclusiveLBound=elbnd, exclusiveUBound=eubnd,  exclusiveCount=ecnt,      &
             computationalLBound=clbnd, computationalUBound=cubnd, computationalCount=ccnt, &
             totalLBound=tlbnd, totalUBound=tubnd, totalCount=tcnt, rc=localrc)
             if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      ! set pointer to null
      nullify(farrayPtr)

     ! Get Coord From Grid
     call ESMF_GridGetCoord(grid, localDE=0, &
              staggerLoc=staggerloc, coordDim=coordDim, farrayPtr=farrayPtr, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     ! Check that output is as expected
     if (.not. associated(farrayPtr)) correct=.false.
     if (lbound(farrayPtr,1) .ne. tlbnd(1)) correct=.false.
     if (ubound(farrayPtr,1) .ne. tubnd(1)) correct=.false.

     if (localPet .eq. 0) then

        if (elbnd(1) .ne. ielbnd0(1)-iloff0(1)) correct=.false.
        if (eubnd(1) .ne. ieubnd0(1)+iuoff0(1)) correct=.false.
        if (ecnt(1) .ne. ieubnd0(1)-ielbnd0(1)+1+iloff0(1)+iuoff0(1)) correct=.false.

        if (clbnd(1) .ne. ielbnd0(1)-iloff0(1)) correct=.false.
        if (cubnd(1) .ne. ieubnd0(1)+iuoff0(1)) correct=.false.
        if (ccnt(1) .ne. ieubnd0(1)-ielbnd0(1)+1+iloff0(1)+iuoff0(1)) correct=.false.

        if (tlbnd(1) .ne. ielbnd0(1)-iloff0(1)) correct=.false.
        if (tubnd(1) .ne. ieubnd0(1)+iuoff0(1)) correct=.false.
        if (tcnt(1) .ne. ieubnd0(1)-ielbnd0(1)+1+iloff0(1)+iuoff0(1)) correct=.false.

     else if (localPet .eq. 1) then
        if (elbnd(1) .ne. ielbnd1(1)-iloff1(1)) correct=.false.
        if (eubnd(1) .ne. ieubnd1(1)+iuoff1(1)) correct=.false.
        if (ecnt(1) .ne. ieubnd1(1)-ielbnd1(1)+1+iloff1(1)+iuoff1(1)) correct=.false.

        if (clbnd(1) .ne. ielbnd1(1)-iloff1(1)) correct=.false.
        if (cubnd(1) .ne. ieubnd1(1)+iuoff1(1)) correct=.false.
        if (ccnt(1) .ne. ieubnd1(1)-ielbnd1(1)+1+iloff1(1)+iuoff1(1)) correct=.false.

        if (tlbnd(1) .ne. ielbnd1(1)-iloff1(1)) correct=.false.
        if (tubnd(1) .ne. ieubnd1(1)+iuoff1(1)) correct=.false.
        if (tcnt(1) .ne. ieubnd1(1)-ielbnd1(1)+1+iloff1(1)+iuoff1(1)) correct=.false.

     else if (localPet .eq. 2) then
        if (elbnd(1) .ne. ielbnd2(1)-iloff2(1)) correct=.false.
        if (eubnd(1) .ne. ieubnd2(1)+iuoff2(1)) correct=.false.
        if (ecnt(1) .ne. ieubnd2(1)-ielbnd2(1)+1+iloff2(1)+iuoff2(1)) correct=.false.

        if (clbnd(1) .ne. ielbnd2(1)-iloff2(1)) correct=.false.
        if (cubnd(1) .ne. ieubnd2(1)+iuoff2(1)) correct=.false.
        if (ccnt(1) .ne. ieubnd2(1)-ielbnd2(1)+1+iloff2(1)+iuoff2(1)) correct=.false.

        if (tlbnd(1) .ne. ielbnd2(1)-iloff2(1)) correct=.false.
        if (tubnd(1) .ne. ieubnd2(1)+iuoff2(1)) correct=.false.
        if (tcnt(1) .ne. ieubnd2(1)-ielbnd2(1)+1+iloff2(1)+iuoff2(1)) correct=.false.

     else if (localPet .eq. 3) then
        if (elbnd(1) .ne. ielbnd3(1)-iloff3(1)) correct=.false.
        if (eubnd(1) .ne. ieubnd3(1)+iuoff3(1)) correct=.false.
        if (ecnt(1) .ne. ieubnd3(1)-ielbnd3(1)+1+iloff3(1)+iuoff3(1)) correct=.false.

        if (clbnd(1) .ne. ielbnd3(1)-iloff3(1)) correct=.false.
        if (cubnd(1) .ne. ieubnd3(1)+iuoff3(1)) correct=.false.
        if (ccnt(1) .ne. ieubnd3(1)-ielbnd3(1)+1+iloff3(1)+iuoff3(1)) correct=.false.

        if (tlbnd(1) .ne. ielbnd3(1)-iloff3(1)) correct=.false.
        if (tubnd(1) .ne. ieubnd3(1)+iuoff3(1)) correct=.false.
        if (tcnt(1) .ne. ieubnd3(1)-ielbnd3(1)+1+iloff3(1)+iuoff3(1)) correct=.false.

     endif
  endif
end subroutine check1DBnds2x2

subroutine check2DP1Bnds2x2(grid, coordDim, staggerloc, localPet, petCount, &
                          ielbnd0,ieubnd0,iloff0,iuoff0, &
                          ielbnd1,ieubnd1,iloff1,iuoff1, &
                          ielbnd2,ieubnd2,iloff2,iuoff2, &
                          ielbnd3,ieubnd3,iloff3,iuoff3, &
                          correct, checkPtr,rc)

  type (ESMF_Grid) :: grid
  type (ESMF_StaggerLoc),intent(in) :: staggerloc
  integer,intent(in) :: coordDim, localPet, petCount
  integer,intent(in) :: ielbnd0(:),ieubnd0(:),iloff0(:),iuoff0(:)
  integer,intent(in) :: ielbnd1(:),ieubnd1(:),iloff1(:),iuoff1(:)
  integer,intent(in) :: ielbnd2(:),ieubnd2(:),iloff2(:),iuoff2(:)
  integer,intent(in) :: ielbnd3(:),ieubnd3(:),iloff3(:),iuoff3(:)
  logical,intent(inout) :: correct
  logical,intent(in),optional :: checkPtr
  integer,intent(inout) :: rc  
  integer :: localrc
  integer :: elbnd(3),eubnd(3),ecnt(3)
  integer :: clbnd(3),cubnd(3),ccnt(3)
  integer :: tlbnd(3),tubnd(3),tcnt(3)
  real (ESMF_KIND_R8), pointer :: farrayPtr(:,:,:)
  logical :: locCheckPtr


  if (present(checkPtr)) then
     locCheckPtr=checkPtr
  else     
     locCheckPtr=.true.
  endif


  ! Check if bounds are correct for each DE
  if (petCount .eq. 1) then
      ! Note the order of DE's here is dependant on the ordering
      ! in ESMF_GridCreateNoPeriDim, if that changes then this will
      ! probably have to change also. 

      ! check DE 0
      call ESMF_GridGetCoordBounds(grid, coordDim=coordDim, localDE=0, &
             staggerLoc=staggerloc,                  &
             exclusiveLBound=elbnd, exclusiveUBound=eubnd,       &
             exclusiveCount=ecnt, &
             computationalLBound=clbnd, computationalUBound=cubnd,           &
             computationalCount=ccnt, &
              rc=localrc)

             if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!   write(*,*) "0:",clbnd,",",cubnd, correct
!   write(*,*) "0: c=",clbnd,",",cubnd,",", ccnt," e=",elbnd,",",eubnd,",", ecnt, correct, rc

     !! set pointer to null
     nullify(farrayPtr)
 
     !! Get Coord Array From Grid
     if (locCheckPtr) then
         call ESMF_GridGetCoord(grid, localDE=0, &
                staggerLoc=staggerloc, coordDim=coordDim, farrayPtr=farrayPtr, totalLBound=tlbnd, totalUBound=tubnd, &
                totalCount=tcnt,  rc=localrc)
         if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
     endif

     !! Check that output is as expected
     if (locCheckPtr) then
        if (.not. associated(farrayPtr)) correct=.false.
        if (lbound(farrayPtr,1) .ne. tlbnd(1))  correct=.false.
        if (lbound(farrayPtr,2) .ne. tlbnd(2))  correct=.false.
        if (lbound(farrayPtr,3) .ne. tlbnd(3))  correct=.false.

        if (ubound(farrayPtr,1) .ne. tubnd(1))  correct=.false.
        if (ubound(farrayPtr,2) .ne. tubnd(2))  correct=.false.
        if (ubound(farrayPtr,3) .ne. tubnd(3))  correct=.false.
     endif

     if (elbnd(1) .ne. ielbnd0(1)-iloff0(1)) correct=.false.
     if (elbnd(2) .ne. ielbnd0(2)-iloff0(2)) correct=.false.
     if (elbnd(3) .ne. ielbnd0(3)-iloff0(3)) correct=.false.

     if (eubnd(1) .ne. ieubnd0(1)+iuoff0(1)) correct=.false.
     if (eubnd(2) .ne. ieubnd0(2)+iuoff0(2)) correct=.false.
     if (eubnd(3) .ne. ieubnd0(3)+iuoff0(3)) correct=.false.

     if (ecnt(1) .ne. ieubnd0(1)-ielbnd0(1)+1+iloff0(1)+iuoff0(1)) correct=.false.
     if (ecnt(2) .ne. ieubnd0(2)-ielbnd0(2)+1+iloff0(2)+iuoff0(2)) correct=.false.
     if (ecnt(3) .ne. ieubnd0(3)-ielbnd0(3)+1+iloff0(3)+iuoff0(3)) correct=.false.

     if (clbnd(1) .ne. ielbnd0(1)-iloff0(1)) correct=.false.
     if (clbnd(2) .ne. ielbnd0(2)-iloff0(2)) correct=.false.
     if (clbnd(3) .ne. ielbnd0(3)-iloff0(3)) correct=.false.

     if (cubnd(1) .ne. ieubnd0(1)+iuoff0(1)) correct=.false.
     if (cubnd(2) .ne. ieubnd0(2)+iuoff0(2)) correct=.false.
     if (cubnd(3) .ne. ieubnd0(3)+iuoff0(3)) correct=.false.

     if (ccnt(1) .ne. ieubnd0(1)-ielbnd0(1)+1+iloff0(1)+iuoff0(1)) correct=.false.
     if (ccnt(2) .ne. ieubnd0(2)-ielbnd0(2)+1+iloff0(2)+iuoff0(2)) correct=.false.
     if (ccnt(3) .ne. ieubnd0(3)-ielbnd0(3)+1+iloff0(3)+iuoff0(3)) correct=.false.

     if (locCheckPtr) then
        if (tlbnd(1) .ne. ielbnd0(1)-iloff0(1)) correct=.false.
        if (tlbnd(2) .ne. ielbnd0(2)-iloff0(2)) correct=.false.
        if (tlbnd(3) .ne. ielbnd0(3)-iloff0(3)) correct=.false.

        if (tubnd(1) .ne. ieubnd0(1)+iuoff0(1)) correct=.false.
        if (tubnd(2) .ne. ieubnd0(2)+iuoff0(2)) correct=.false.
        if (tubnd(3) .ne. ieubnd0(3)+iuoff0(3)) correct=.false.

        if (tcnt(1) .ne. ieubnd0(1)-ielbnd0(1)+1+iloff0(1)+iuoff0(1)) correct=.false.
        if (tcnt(2) .ne. ieubnd0(2)-ielbnd0(2)+1+iloff0(2)+iuoff0(2)) correct=.false.
        if (tcnt(3) .ne. ieubnd0(3)-ielbnd0(3)+1+iloff0(3)+iuoff0(3)) correct=.false.
     endif

      ! check DE 1
      call ESMF_GridGetCoordBounds(grid2D, coordDim=coordDim, localDE=1, &
             staggerLoc=staggerloc,                  &
             exclusiveLBound=elbnd, exclusiveUBound=eubnd,       &
             exclusiveCount=ecnt, &
             computationalLBound=clbnd, computationalUBound=cubnd,           &
             computationalCount=ccnt, &
             rc=localrc)
             if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!    write(*,*) "1:",clbnd,",",cubnd, rc,correct

     !! set pointer to null
     nullify(farrayPtr)

     !! Get Coord From Grid
     if (locCheckPtr) then
        call ESMF_GridGetCoord(grid, localDE=1, &
                 staggerLoc=staggerloc, coordDim=coordDim, farrayPtr=farrayPtr,  totalLBound=tlbnd, totalUBound=tubnd, &
                 totalCount=tcnt,rc=localrc)
        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
     endif

     !! Check that output is as expected
     if (locCheckPtr) then
        if (.not. associated(farrayPtr)) correct=.false.
        if (lbound(farrayPtr,1) .ne. tlbnd(1))  correct=.false.
        if (lbound(farrayPtr,2) .ne. tlbnd(2))  correct=.false.
        if (lbound(farrayPtr,3) .ne. tlbnd(3))  correct=.false.

        if (ubound(farrayPtr,1) .ne. tubnd(1))  correct=.false.
        if (ubound(farrayPtr,2) .ne. tubnd(2))  correct=.false.
        if (ubound(farrayPtr,3) .ne. tubnd(3))  correct=.false.
     endif


     if (elbnd(1) .ne. ielbnd1(1)-iloff1(1)) correct=.false.
     if (elbnd(2) .ne. ielbnd1(2)-iloff1(2)) correct=.false.
     if (elbnd(3) .ne. ielbnd1(3)-iloff1(3)) correct=.false.

     if (eubnd(1) .ne. ieubnd1(1)+iuoff1(1)) correct=.false.
     if (eubnd(2) .ne. ieubnd1(2)+iuoff1(2)) correct=.false.
     if (eubnd(3) .ne. ieubnd1(3)+iuoff1(3)) correct=.false.

     if (ecnt(1) .ne. ieubnd1(1)-ielbnd1(1)+1+iloff1(1)+iuoff1(1)) correct=.false.
     if (ecnt(2) .ne. ieubnd1(2)-ielbnd1(2)+1+iloff1(2)+iuoff1(2)) correct=.false.
     if (ecnt(3) .ne. ieubnd1(3)-ielbnd1(3)+1+iloff1(3)+iuoff1(3)) correct=.false.


     if (clbnd(1) .ne. ielbnd1(1)-iloff1(1)) correct=.false.
     if (clbnd(2) .ne. ielbnd1(2)-iloff1(2)) correct=.false.
     if (clbnd(3) .ne. ielbnd1(3)-iloff1(3)) correct=.false.

     if (cubnd(1) .ne. ieubnd1(1)+iuoff1(1)) correct=.false.
     if (cubnd(2) .ne. ieubnd1(2)+iuoff1(2)) correct=.false.
     if (cubnd(3) .ne. ieubnd1(3)+iuoff1(3)) correct=.false.

     if (ccnt(1) .ne. ieubnd1(1)-ielbnd1(1)+1+iloff1(1)+iuoff1(1)) correct=.false.
     if (ccnt(2) .ne. ieubnd1(2)-ielbnd1(2)+1+iloff1(2)+iuoff1(2)) correct=.false.
     if (ccnt(3) .ne. ieubnd1(3)-ielbnd1(3)+1+iloff1(3)+iuoff1(3)) correct=.false.

     if (locCheckPtr) then
        if (tlbnd(1) .ne. ielbnd1(1)-iloff1(1)) correct=.false.
        if (tlbnd(2) .ne. ielbnd1(2)-iloff1(2)) correct=.false.
        if (tlbnd(3) .ne. ielbnd1(3)-iloff1(3)) correct=.false.
        
        if (tubnd(1) .ne. ieubnd1(1)+iuoff1(1)) correct=.false.
        if (tubnd(2) .ne. ieubnd1(2)+iuoff1(2)) correct=.false.
        if (tubnd(3) .ne. ieubnd1(3)+iuoff1(3)) correct=.false.

        if (tcnt(1) .ne. ieubnd1(1)-ielbnd1(1)+1+iloff1(1)+iuoff1(1)) correct=.false.
        if (tcnt(2) .ne. ieubnd1(2)-ielbnd1(2)+1+iloff1(2)+iuoff1(2)) correct=.false.
        if (tcnt(3) .ne. ieubnd1(3)-ielbnd1(3)+1+iloff1(3)+iuoff1(3)) correct=.false.
     endif

      ! check DE 2
      call ESMF_GridGetCoordBounds(grid, coordDim=coordDim, localDE=2, &
             staggerLoc=staggerloc,                  &
             exclusiveLBound=elbnd, exclusiveUBound=eubnd,       &
             exclusiveCount=ecnt, &
             computationalLBound=clbnd, computationalUBound=cubnd,           &
             computationalCount=ccnt, &
              rc=localrc)
             if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!    write(*,*) "2:",clbnd,",",cubnd, correct

     !! set pointer to null
     nullify(farrayPtr)

     !! Get Coord From Grid
     if (locCheckPtr) then
         call ESMF_GridGetCoord(grid, localDE=2, &
                staggerLoc=staggerloc, coordDim=coordDim, farrayPtr=farrayPtr, totalLBound=tlbnd, totalUBound=tubnd, &
               totalCount=tcnt,  rc=localrc)
         if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
     endif

     !! Check that output is as expected
     if (locCheckPtr) then
        if (.not. associated(farrayPtr)) correct=.false.
        if (lbound(farrayPtr,1) .ne. tlbnd(1))  correct=.false.
        if (lbound(farrayPtr,2) .ne. tlbnd(2))  correct=.false.
        if (lbound(farrayPtr,3) .ne. tlbnd(3))  correct=.false.

        if (ubound(farrayPtr,1) .ne. tubnd(1))  correct=.false.
        if (ubound(farrayPtr,2) .ne. tubnd(2))  correct=.false.
        if (ubound(farrayPtr,3) .ne. tubnd(3))  correct=.false.
     endif


     if (elbnd(1) .ne. ielbnd2(1)-iloff2(1)) correct=.false.
     if (elbnd(2) .ne. ielbnd2(2)-iloff2(2)) correct=.false.
     if (elbnd(3) .ne. ielbnd2(3)-iloff2(3)) correct=.false.

     if (eubnd(1) .ne. ieubnd2(1)+iuoff2(1)) correct=.false.
     if (eubnd(2) .ne. ieubnd2(2)+iuoff2(2)) correct=.false.
     if (eubnd(3) .ne. ieubnd2(3)+iuoff2(3)) correct=.false.

     if (ecnt(1) .ne. ieubnd2(1)-ielbnd2(1)+1+iloff2(1)+iuoff2(1)) correct=.false.
     if (ecnt(2) .ne. ieubnd2(2)-ielbnd2(2)+1+iloff2(2)+iuoff2(2)) correct=.false.
     if (ecnt(3) .ne. ieubnd2(3)-ielbnd2(3)+1+iloff2(3)+iuoff2(3)) correct=.false.

     if (clbnd(1) .ne. ielbnd2(1)-iloff2(1)) correct=.false.
     if (clbnd(2) .ne. ielbnd2(2)-iloff2(2)) correct=.false.
     if (clbnd(3) .ne. ielbnd2(3)-iloff2(3)) correct=.false.

     if (cubnd(1) .ne. ieubnd2(1)+iuoff2(1)) correct=.false.
     if (cubnd(2) .ne. ieubnd2(2)+iuoff2(2)) correct=.false.
     if (cubnd(3) .ne. ieubnd2(3)+iuoff2(3)) correct=.false.

     if (ccnt(1) .ne. ieubnd2(1)-ielbnd2(1)+1+iloff2(1)+iuoff2(1)) correct=.false.
     if (ccnt(2) .ne. ieubnd2(2)-ielbnd2(2)+1+iloff2(2)+iuoff2(2)) correct=.false.
     if (ccnt(3) .ne. ieubnd2(3)-ielbnd2(3)+1+iloff2(3)+iuoff2(3)) correct=.false.

     if (locCheckPtr) then
        if (tlbnd(1) .ne. ielbnd2(1)-iloff2(1)) correct=.false.
        if (tlbnd(2) .ne. ielbnd2(2)-iloff2(2)) correct=.false.
        if (tlbnd(3) .ne. ielbnd2(3)-iloff2(3)) correct=.false.

        if (tubnd(1) .ne. ieubnd2(1)+iuoff2(1)) correct=.false.
        if (tubnd(2) .ne. ieubnd2(2)+iuoff2(2)) correct=.false.
        if (tubnd(3) .ne. ieubnd2(3)+iuoff2(3)) correct=.false.

        if (tcnt(1) .ne. ieubnd2(1)-ielbnd2(1)+1+iloff2(1)+iuoff2(1)) correct=.false.
        if (tcnt(2) .ne. ieubnd2(2)-ielbnd2(2)+1+iloff2(2)+iuoff2(2)) correct=.false.
        if (tcnt(3) .ne. ieubnd2(3)-ielbnd2(3)+1+iloff2(3)+iuoff2(3)) correct=.false.
     endif

      ! check DE 3
      call ESMF_GridGetCoordBounds(grid, coordDim=coordDim, localDE=3, &
             staggerLoc=staggerloc,                  &
             exclusiveLBound=elbnd, exclusiveUBound=eubnd,       &
             exclusiveCount=ecnt, &
             computationalLBound=clbnd, computationalUBound=cubnd,           &
             computationalCount=ccnt, &
             rc=localrc)
             if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

!    write(*,*) "3:",clbnd,",",cubnd, correct
     !! set pointer to null
     nullify(farrayPtr)

     !! Get Coord From Grid
     if (locCheckPtr) then
        call ESMF_GridGetCoord(grid, localDE=3, &
              staggerLoc=staggerloc, coordDim=coordDim, farrayPtr=farrayPtr,  totalLBound=tlbnd, totalUBound=tubnd, &
             totalCount=tcnt,  rc=localrc)
        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
     endif

     !! Check that output is as expected
     if (locCheckPtr) then
        if (.not. associated(farrayPtr)) correct=.false.
        if (lbound(farrayPtr,1) .ne. tlbnd(1))  correct=.false.
        if (lbound(farrayPtr,2) .ne. tlbnd(2))  correct=.false.
        if (lbound(farrayPtr,3) .ne. tlbnd(3))  correct=.false.

        if (ubound(farrayPtr,1) .ne. tubnd(1))  correct=.false.
        if (ubound(farrayPtr,2) .ne. tubnd(2))  correct=.false.
        if (ubound(farrayPtr,3) .ne. tubnd(3))  correct=.false.
     endif

     if (elbnd(1) .ne. ielbnd3(1)-iloff3(1)) correct=.false.
     if (elbnd(2) .ne. ielbnd3(2)-iloff3(2)) correct=.false.
     if (elbnd(3) .ne. ielbnd3(3)-iloff3(3)) correct=.false.

     if (eubnd(1) .ne. ieubnd3(1)+iuoff3(1)) correct=.false.
     if (eubnd(2) .ne. ieubnd3(2)+iuoff3(2)) correct=.false.
     if (eubnd(3) .ne. ieubnd3(3)+iuoff3(3)) correct=.false.

     if (ecnt(1) .ne. ieubnd3(1)-ielbnd3(1)+1+iloff3(1)+iuoff3(1)) correct=.false.
     if (ecnt(2) .ne. ieubnd3(2)-ielbnd3(2)+1+iloff3(2)+iuoff3(2)) correct=.false.
     if (ecnt(3) .ne. ieubnd3(3)-ielbnd3(3)+1+iloff3(3)+iuoff3(3)) correct=.false.

     if (clbnd(1) .ne. ielbnd3(1)-iloff3(1)) correct=.false.
     if (clbnd(2) .ne. ielbnd3(2)-iloff3(2)) correct=.false.
     if (clbnd(3) .ne. ielbnd3(3)-iloff3(3)) correct=.false.

     if (cubnd(1) .ne. ieubnd3(1)+iuoff3(1)) correct=.false.
     if (cubnd(2) .ne. ieubnd3(2)+iuoff3(2)) correct=.false.
     if (cubnd(3) .ne. ieubnd3(3)+iuoff3(3)) correct=.false.

     if (ccnt(1) .ne. ieubnd3(1)-ielbnd3(1)+1+iloff3(1)+iuoff3(1)) correct=.false.
     if (ccnt(2) .ne. ieubnd3(2)-ielbnd3(2)+1+iloff3(2)+iuoff3(2)) correct=.false.
     if (ccnt(3) .ne. ieubnd3(3)-ielbnd3(3)+1+iloff3(3)+iuoff3(3)) correct=.false.

     if (locCheckPtr) then
        if (tlbnd(1) .ne. ielbnd3(1)-iloff3(1)) correct=.false.
        if (tlbnd(2) .ne. ielbnd3(2)-iloff3(2)) correct=.false.
        if (tlbnd(3) .ne. ielbnd3(3)-iloff3(3)) correct=.false.

        if (tubnd(1) .ne. ieubnd3(1)+iuoff3(1)) correct=.false.
        if (tubnd(2) .ne. ieubnd3(2)+iuoff3(2)) correct=.false.
        if (tubnd(3) .ne. ieubnd3(3)+iuoff3(3)) correct=.false.

        if (tcnt(1) .ne. ieubnd3(1)-ielbnd3(1)+1+iloff3(1)+iuoff3(1)) correct=.false.
        if (tcnt(2) .ne. ieubnd3(2)-ielbnd3(2)+1+iloff3(2)+iuoff3(2)) correct=.false.
        if (tcnt(3) .ne. ieubnd3(3)-ielbnd3(3)+1+iloff3(3)+iuoff3(3)) correct=.false.
     endif

  else  if (petCount .eq. 4) then
      call ESMF_GridGetCoordBounds(grid, coordDim=coordDim, localDE=0, &
             staggerLoc=staggerloc,                  &
             exclusiveLBound=elbnd, exclusiveUBound=eubnd,       &
             exclusiveCount=ecnt, &
             computationalLBound=clbnd, computationalUBound=cubnd,           &
             computationalCount=ccnt, &
             rc=localrc)
             if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

!    write(*,*) localPet,":",clbnd,",",cubnd, "rc=",rc, "correct=",correct

      ! set pointer to null
      nullify(farrayPtr)

     ! Get Coord From Grid
     if (locCheckPtr) then
        call ESMF_GridGetCoord(grid, localDE=0, &
              staggerLoc=staggerloc, coordDim=coordDim, farrayPtr=farrayPtr, totalLBound=tlbnd, totalUBound=tubnd, &
             totalCount=tcnt, rc=localrc)
        if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
     endif

     ! Check that output is as expected
     if (locCheckPtr) then
        if (.not. associated(farrayPtr)) correct=.false.
        if (lbound(farrayPtr,1) .ne. tlbnd(1))  correct=.false.
        if (lbound(farrayPtr,2) .ne. tlbnd(2))  correct=.false.
        if (lbound(farrayPtr,3) .ne. tlbnd(3))  correct=.false.

        if (ubound(farrayPtr,1) .ne. tubnd(1))  correct=.false.
        if (ubound(farrayPtr,2) .ne. tubnd(2))  correct=.false.
        if (ubound(farrayPtr,3) .ne. tubnd(3))  correct=.false.
     endif

     if (localPet .eq. 0) then

        if (elbnd(1) .ne. ielbnd0(1)-iloff0(1)) correct=.false.
        if (elbnd(2) .ne. ielbnd0(2)-iloff0(2)) correct=.false.
        if (elbnd(3) .ne. ielbnd0(3)-iloff0(3)) correct=.false.

        if (eubnd(1) .ne. ieubnd0(1)+iuoff0(1)) correct=.false.
        if (eubnd(2) .ne. ieubnd0(2)+iuoff0(2)) correct=.false.
        if (eubnd(3) .ne. ieubnd0(3)+iuoff0(3)) correct=.false.

        if (ecnt(1) .ne. ieubnd0(1)-ielbnd0(1)+1+iloff0(1)+iuoff0(1)) correct=.false.
        if (ecnt(2) .ne. ieubnd0(2)-ielbnd0(2)+1+iloff0(2)+iuoff0(2)) correct=.false.
        if (ecnt(3) .ne. ieubnd0(3)-ielbnd0(3)+1+iloff0(3)+iuoff0(3)) correct=.false.

        if (clbnd(1) .ne. ielbnd0(1)-iloff0(1)) correct=.false.
        if (clbnd(2) .ne. ielbnd0(2)-iloff0(2)) correct=.false.
        if (clbnd(3) .ne. ielbnd0(3)-iloff0(3)) correct=.false.

        if (cubnd(1) .ne. ieubnd0(1)+iuoff0(1)) correct=.false.
        if (cubnd(2) .ne. ieubnd0(2)+iuoff0(2)) correct=.false.
        if (cubnd(3) .ne. ieubnd0(3)+iuoff0(3)) correct=.false.

        if (ccnt(1) .ne. ieubnd0(1)-ielbnd0(1)+1+iloff0(1)+iuoff0(1)) correct=.false.
        if (ccnt(2) .ne. ieubnd0(2)-ielbnd0(2)+1+iloff0(2)+iuoff0(2)) correct=.false.
        if (ccnt(3) .ne. ieubnd0(3)-ielbnd0(3)+1+iloff0(3)+iuoff0(3)) correct=.false.

        if (locCheckPtr) then
           if (elbnd(1) .ne. ielbnd0(1)-iloff0(1)) correct=.false.
           if (elbnd(2) .ne. ielbnd0(2)-iloff0(2)) correct=.false.
           if (elbnd(3) .ne. ielbnd0(3)-iloff0(3)) correct=.false.

           if (eubnd(1) .ne. ieubnd0(1)+iuoff0(1)) correct=.false.
           if (eubnd(2) .ne. ieubnd0(2)+iuoff0(2)) correct=.false.
           if (eubnd(3) .ne. ieubnd0(3)+iuoff0(3)) correct=.false.

           if (ecnt(1) .ne. ieubnd0(1)-ielbnd0(1)+1+iloff0(1)+iuoff0(1)) correct=.false.
           if (ecnt(2) .ne. ieubnd0(2)-ielbnd0(2)+1+iloff0(2)+iuoff0(2)) correct=.false.
           if (ecnt(3) .ne. ieubnd0(3)-ielbnd0(3)+1+iloff0(3)+iuoff0(3)) correct=.false.
        endif

     else if (localPet .eq. 1) then
        if (elbnd(1) .ne. ielbnd1(1)-iloff1(1)) correct=.false.
        if (elbnd(2) .ne. ielbnd1(2)-iloff1(2)) correct=.false.
        if (elbnd(3) .ne. ielbnd1(3)-iloff1(3)) correct=.false.

        if (eubnd(1) .ne. ieubnd1(1)+iuoff1(1)) correct=.false.
        if (eubnd(2) .ne. ieubnd1(2)+iuoff1(2)) correct=.false.
        if (eubnd(3) .ne. ieubnd1(3)+iuoff1(3)) correct=.false.

        if (ecnt(1) .ne. ieubnd1(1)-ielbnd1(1)+1+iloff1(1)+iuoff1(1)) correct=.false.
        if (ecnt(2) .ne. ieubnd1(2)-ielbnd1(2)+1+iloff1(2)+iuoff1(2)) correct=.false.
        if (ecnt(3) .ne. ieubnd1(3)-ielbnd1(3)+1+iloff1(3)+iuoff1(3)) correct=.false.

        if (clbnd(1) .ne. ielbnd1(1)-iloff1(1)) correct=.false.
        if (clbnd(2) .ne. ielbnd1(2)-iloff1(2)) correct=.false.
        if (clbnd(3) .ne. ielbnd1(3)-iloff1(3)) correct=.false.

        if (cubnd(1) .ne. ieubnd1(1)+iuoff1(1)) correct=.false.
        if (cubnd(2) .ne. ieubnd1(2)+iuoff1(2)) correct=.false.
        if (cubnd(3) .ne. ieubnd1(3)+iuoff1(3)) correct=.false.

        if (ccnt(1) .ne. ieubnd1(1)-ielbnd1(1)+1+iloff1(1)+iuoff1(1)) correct=.false.
        if (ccnt(2) .ne. ieubnd1(2)-ielbnd1(2)+1+iloff1(2)+iuoff1(2)) correct=.false.
        if (ccnt(3) .ne. ieubnd1(3)-ielbnd1(3)+1+iloff1(3)+iuoff1(3)) correct=.false.

        if (locCheckPtr) then
           if (tlbnd(1) .ne. ielbnd1(1)-iloff1(1)) correct=.false.
           if (tlbnd(2) .ne. ielbnd1(2)-iloff1(2)) correct=.false.
           if (tlbnd(3) .ne. ielbnd1(3)-iloff1(3)) correct=.false.

           if (tubnd(1) .ne. ieubnd1(1)+iuoff1(1)) correct=.false.
           if (tubnd(2) .ne. ieubnd1(2)+iuoff1(2)) correct=.false.
           if (tubnd(3) .ne. ieubnd1(3)+iuoff1(3)) correct=.false.

           if (tcnt(1) .ne. ieubnd1(1)-ielbnd1(1)+1+iloff1(1)+iuoff1(1)) correct=.false.
           if (tcnt(2) .ne. ieubnd1(2)-ielbnd1(2)+1+iloff1(2)+iuoff1(2)) correct=.false.
           if (tcnt(3) .ne. ieubnd1(3)-ielbnd1(3)+1+iloff1(3)+iuoff1(3)) correct=.false.
        endif

     else if (localPet .eq. 2) then
        if (elbnd(1) .ne. ielbnd2(1)-iloff2(1)) correct=.false.
        if (elbnd(2) .ne. ielbnd2(2)-iloff2(2)) correct=.false.
        if (elbnd(3) .ne. ielbnd2(3)-iloff2(3)) correct=.false.

        if (eubnd(1) .ne. ieubnd2(1)+iuoff2(1)) correct=.false.
        if (eubnd(2) .ne. ieubnd2(2)+iuoff2(2)) correct=.false.
        if (eubnd(3) .ne. ieubnd2(3)+iuoff2(3)) correct=.false.

        if (ecnt(1) .ne. ieubnd2(1)-ielbnd2(1)+1+iloff2(1)+iuoff2(1)) correct=.false.
        if (ecnt(2) .ne. ieubnd2(2)-ielbnd2(2)+1+iloff2(2)+iuoff2(2)) correct=.false.
        if (ecnt(3) .ne. ieubnd2(3)-ielbnd2(3)+1+iloff2(3)+iuoff2(3)) correct=.false.

        if (clbnd(1) .ne. ielbnd2(1)-iloff2(1)) correct=.false.
        if (clbnd(2) .ne. ielbnd2(2)-iloff2(2)) correct=.false.
        if (clbnd(3) .ne. ielbnd2(3)-iloff2(3)) correct=.false.

        if (cubnd(1) .ne. ieubnd2(1)+iuoff2(1)) correct=.false.
        if (cubnd(2) .ne. ieubnd2(2)+iuoff2(2)) correct=.false.
        if (cubnd(3) .ne. ieubnd2(3)+iuoff2(3)) correct=.false.

        if (ccnt(1) .ne. ieubnd2(1)-ielbnd2(1)+1+iloff2(1)+iuoff2(1)) correct=.false.
        if (ccnt(2) .ne. ieubnd2(2)-ielbnd2(2)+1+iloff2(2)+iuoff2(2)) correct=.false.
        if (ccnt(3) .ne. ieubnd2(3)-ielbnd2(3)+1+iloff2(3)+iuoff2(3)) correct=.false.

        if (locCheckPtr) then
           if (tlbnd(1) .ne. ielbnd2(1)-iloff2(1)) correct=.false.
           if (tlbnd(2) .ne. ielbnd2(2)-iloff2(2)) correct=.false.
           if (tlbnd(3) .ne. ielbnd2(3)-iloff2(3)) correct=.false.

           if (tubnd(1) .ne. ieubnd2(1)+iuoff2(1)) correct=.false.
           if (tubnd(2) .ne. ieubnd2(2)+iuoff2(2)) correct=.false.
           if (tubnd(3) .ne. ieubnd2(3)+iuoff2(3)) correct=.false.

           if (tcnt(1) .ne. ieubnd2(1)-ielbnd2(1)+1+iloff2(1)+iuoff2(1)) correct=.false.
           if (tcnt(2) .ne. ieubnd2(2)-ielbnd2(2)+1+iloff2(2)+iuoff2(2)) correct=.false.
           if (tcnt(3) .ne. ieubnd2(3)-ielbnd2(3)+1+iloff2(3)+iuoff2(3)) correct=.false.
        endif
     else if (localPet .eq. 3) then

        if (elbnd(1) .ne. ielbnd3(1)-iloff3(1)) correct=.false.
        if (elbnd(2) .ne. ielbnd3(2)-iloff3(2)) correct=.false.
        if (elbnd(3) .ne. ielbnd3(3)-iloff3(3)) correct=.false.

        if (eubnd(1) .ne. ieubnd3(1)+iuoff3(1)) correct=.false.
        if (eubnd(2) .ne. ieubnd3(2)+iuoff3(2)) correct=.false.
        if (eubnd(3) .ne. ieubnd3(3)+iuoff3(3)) correct=.false.

        if (ecnt(1) .ne. ieubnd3(1)-ielbnd3(1)+1+iloff3(1)+iuoff3(1)) correct=.false.
        if (ecnt(2) .ne. ieubnd3(2)-ielbnd3(2)+1+iloff3(2)+iuoff3(2)) correct=.false.
        if (ecnt(3) .ne. ieubnd3(3)-ielbnd3(3)+1+iloff3(3)+iuoff3(3)) correct=.false.

        if (clbnd(1) .ne. ielbnd3(1)-iloff3(1)) correct=.false.
        if (clbnd(2) .ne. ielbnd3(2)-iloff3(2)) correct=.false.
        if (clbnd(3) .ne. ielbnd3(3)-iloff3(3)) correct=.false.

        if (cubnd(1) .ne. ieubnd3(1)+iuoff3(1)) correct=.false.
        if (cubnd(2) .ne. ieubnd3(2)+iuoff3(2)) correct=.false.
        if (cubnd(3) .ne. ieubnd3(3)+iuoff3(3)) correct=.false.

        if (ccnt(1) .ne. ieubnd3(1)-ielbnd3(1)+1+iloff3(1)+iuoff3(1)) correct=.false.
        if (ccnt(2) .ne. ieubnd3(2)-ielbnd3(2)+1+iloff3(2)+iuoff3(2)) correct=.false.
        if (ccnt(3) .ne. ieubnd3(3)-ielbnd3(3)+1+iloff3(3)+iuoff3(3)) correct=.false.

        if (locCheckPtr) then
           if (tlbnd(1) .ne. ielbnd3(1)-iloff3(1)) correct=.false.
           if (tlbnd(2) .ne. ielbnd3(2)-iloff3(2)) correct=.false.
           if (tlbnd(3) .ne. ielbnd3(3)-iloff3(3)) correct=.false.

           if (tubnd(1) .ne. ieubnd3(1)+iuoff3(1)) correct=.false.
           if (tubnd(2) .ne. ieubnd3(2)+iuoff3(2)) correct=.false.
           if (tubnd(3) .ne. ieubnd3(3)+iuoff3(3)) correct=.false.

           if (tcnt(1) .ne. ieubnd3(1)-ielbnd3(1)+1+iloff3(1)+iuoff3(1)) correct=.false.
           if (tcnt(2) .ne. ieubnd3(2)-ielbnd3(2)+1+iloff3(2)+iuoff3(2)) correct=.false.
           if (tcnt(3) .ne. ieubnd3(3)-ielbnd3(3)+1+iloff3(3)+iuoff3(3)) correct=.false.
        endif
     endif
  endif
end subroutine check2DP1Bnds2x2



subroutine check2DP1Bnds2x2UsingSLoc(grid, staggerloc, localPet, petCount, &
                          ielbnd0,ieubnd0,iloff0,iuoff0, &
                          ielbnd1,ieubnd1,iloff1,iuoff1, &
                          ielbnd2,ieubnd2,iloff2,iuoff2, &
                          ielbnd3,ieubnd3,iloff3,iuoff3, &
                          correct, rc)

  type (ESMF_Grid) :: grid
  type (ESMF_StaggerLoc),intent(in) :: staggerloc
  integer,intent(in) :: localPet, petCount
  integer,intent(in) :: ielbnd0(:),ieubnd0(:),iloff0(:),iuoff0(:)
  integer,intent(in) :: ielbnd1(:),ieubnd1(:),iloff1(:),iuoff1(:)
  integer,intent(in) :: ielbnd2(:),ieubnd2(:),iloff2(:),iuoff2(:)
  integer,intent(in) :: ielbnd3(:),ieubnd3(:),iloff3(:),iuoff3(:)
  logical,intent(inout) :: correct
  integer,intent(inout) :: rc
  
  integer :: localrc
  integer :: elbnd(3),eubnd(3),ecnt(3)
  integer :: clbnd(3),cubnd(3),ccnt(3)

  ! Check if bounds are correct for each DE
  if (petCount .eq. 1) then
      ! Note the order of DE's here is dependant on the ordering
      ! in ESMF_GridCreateNoPeriDim, if that changes then this will
      ! probably have to change also. 

      ! check DE 0
      call ESMF_GridGet(grid, localDE=0, &
             staggerLoc=staggerloc,                  &
             exclusiveLBound=elbnd, exclusiveUBound=eubnd, exclusiveCount=ecnt,       &
             computationalLBound=clbnd, computationalUBound=cubnd, computationalCount=ccnt, rc=localrc)
             if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!   write(*,*) "0:",clbnd,",",cubnd, correct


     if (elbnd(1) .ne. ielbnd0(1)-iloff0(1)) correct=.false.
     if (elbnd(2) .ne. ielbnd0(2)-iloff0(2)) correct=.false.
     if (elbnd(3) .ne. ielbnd0(3)-iloff0(3)) correct=.false.
     if (eubnd(1) .ne. ieubnd0(1)+iuoff0(1)) correct=.false.
     if (eubnd(2) .ne. ieubnd0(2)+iuoff0(2)) correct=.false.
     if (eubnd(3) .ne. ieubnd0(3)+iuoff0(3)) correct=.false.
     if (ecnt(1) .ne. ieubnd0(1)-ielbnd0(1)+1+iloff0(1)+iuoff0(1)) correct=.false.
     if (ecnt(2) .ne. ieubnd0(2)-ielbnd0(2)+1+iloff0(2)+iuoff0(2)) correct=.false.
     if (ecnt(3) .ne. ieubnd0(3)-ielbnd0(3)+1+iloff0(3)+iuoff0(3)) correct=.false.

     if (clbnd(1) .ne. ielbnd0(1)-iloff0(1)) correct=.false.
     if (clbnd(2) .ne. ielbnd0(2)-iloff0(2)) correct=.false.
     if (clbnd(3) .ne. ielbnd0(3)-iloff0(3)) correct=.false.
     if (cubnd(1) .ne. ieubnd0(1)+iuoff0(1)) correct=.false.
     if (cubnd(2) .ne. ieubnd0(2)+iuoff0(2)) correct=.false.
     if (cubnd(3) .ne. ieubnd0(3)+iuoff0(3)) correct=.false.
     if (ccnt(1) .ne. ieubnd0(1)-ielbnd0(1)+1+iloff0(1)+iuoff0(1)) correct=.false.
     if (ccnt(2) .ne. ieubnd0(2)-ielbnd0(2)+1+iloff0(2)+iuoff0(2)) correct=.false.
     if (ccnt(3) .ne. ieubnd0(3)-ielbnd0(3)+1+iloff0(3)+iuoff0(3)) correct=.false.
 
      ! check DE 1
      call ESMF_GridGet(grid2D, localDE=1, &
             staggerLoc=staggerloc,                  &
             exclusiveLBound=elbnd, exclusiveUBound=eubnd,  exclusiveCount=ecnt,     &
             computationalLBound=clbnd, computationalUBound=cubnd, computationalCount=ccnt, rc=localrc)
             if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!    write(*,*) "1:",clbnd,",",cubnd, correct


     if (elbnd(1) .ne. ielbnd1(1)-iloff1(1)) correct=.false.
     if (elbnd(2) .ne. ielbnd1(2)-iloff1(2)) correct=.false.
     if (elbnd(3) .ne. ielbnd1(3)-iloff1(3)) correct=.false.
     if (eubnd(1) .ne. ieubnd1(1)+iuoff1(1)) correct=.false.
     if (eubnd(2) .ne. ieubnd1(2)+iuoff1(2)) correct=.false.
     if (eubnd(3) .ne. ieubnd1(3)+iuoff1(3)) correct=.false.
     if (ecnt(1) .ne. ieubnd1(1)-ielbnd1(1)+1+iloff1(1)+iuoff1(1)) correct=.false.
     if (ecnt(2) .ne. ieubnd1(2)-ielbnd1(2)+1+iloff1(2)+iuoff1(2)) correct=.false.
     if (ecnt(3) .ne. ieubnd1(3)-ielbnd1(3)+1+iloff1(3)+iuoff1(3)) correct=.false.

     if (clbnd(1) .ne. ielbnd1(1)-iloff1(1)) correct=.false.
     if (clbnd(2) .ne. ielbnd1(2)-iloff1(2)) correct=.false.
     if (clbnd(3) .ne. ielbnd1(3)-iloff1(3)) correct=.false.
     if (cubnd(1) .ne. ieubnd1(1)+iuoff1(1)) correct=.false.
     if (cubnd(2) .ne. ieubnd1(2)+iuoff1(2)) correct=.false.
     if (cubnd(3) .ne. ieubnd1(3)+iuoff1(3)) correct=.false.
     if (ccnt(1) .ne. ieubnd1(1)-ielbnd1(1)+1+iloff1(1)+iuoff1(1)) correct=.false.
     if (ccnt(2) .ne. ieubnd1(2)-ielbnd1(2)+1+iloff1(2)+iuoff1(2)) correct=.false.
     if (ccnt(3) .ne. ieubnd1(3)-ielbnd1(3)+1+iloff1(3)+iuoff1(3)) correct=.false.

      ! check DE 2
      call ESMF_GridGet(grid, localDE=2, &
             staggerLoc=staggerloc,                  &
             exclusiveLBound=elbnd, exclusiveUBound=eubnd, exclusiveCount=ecnt,   &
             computationalLBound=clbnd, computationalUBound=cubnd, computationalCount=ccnt, rc=localrc)
             if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!   write(*,*) "2:",clbnd,",",cubnd, correct

     if (elbnd(1) .ne. ielbnd2(1)-iloff2(1)) correct=.false.
     if (elbnd(2) .ne. ielbnd2(2)-iloff2(2)) correct=.false.
     if (elbnd(3) .ne. ielbnd2(3)-iloff2(3)) correct=.false.
     if (eubnd(1) .ne. ieubnd2(1)+iuoff2(1)) correct=.false.
     if (eubnd(2) .ne. ieubnd2(2)+iuoff2(2)) correct=.false.
     if (eubnd(3) .ne. ieubnd2(3)+iuoff2(3)) correct=.false.
     if (ecnt(1) .ne. ieubnd2(1)-ielbnd2(1)+1+iloff2(1)+iuoff2(1)) correct=.false.
     if (ecnt(2) .ne. ieubnd2(2)-ielbnd2(2)+1+iloff2(2)+iuoff2(2)) correct=.false.
     if (ecnt(3) .ne. ieubnd2(3)-ielbnd2(3)+1+iloff2(3)+iuoff2(3)) correct=.false.

     if (clbnd(1) .ne. ielbnd2(1)-iloff2(1)) correct=.false.
     if (clbnd(2) .ne. ielbnd2(2)-iloff2(2)) correct=.false.
     if (clbnd(3) .ne. ielbnd2(3)-iloff2(3)) correct=.false.
     if (cubnd(1) .ne. ieubnd2(1)+iuoff2(1)) correct=.false.
     if (cubnd(2) .ne. ieubnd2(2)+iuoff2(2)) correct=.false.
     if (cubnd(3) .ne. ieubnd2(3)+iuoff2(3)) correct=.false.
     if (ccnt(1) .ne. ieubnd2(1)-ielbnd2(1)+1+iloff2(1)+iuoff2(1)) correct=.false.
     if (ccnt(2) .ne. ieubnd2(2)-ielbnd2(2)+1+iloff2(2)+iuoff2(2)) correct=.false.
     if (ccnt(3) .ne. ieubnd2(3)-ielbnd2(3)+1+iloff2(3)+iuoff2(3)) correct=.false.

      ! check DE 3
      call ESMF_GridGet(grid, localDE=3, &
             staggerLoc=staggerloc,                  &
             exclusiveLBound=elbnd, exclusiveUBound=eubnd,  exclusiveCount=ecnt,      &
             computationalLBound=clbnd, computationalUBound=cubnd, computationalCount=ccnt, rc=localrc)
             if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!    write(*,*) "3:",clbnd,",",cubnd, correct

     if (elbnd(1) .ne. ielbnd3(1)-iloff3(1)) correct=.false.
     if (elbnd(2) .ne. ielbnd3(2)-iloff3(2)) correct=.false.
     if (elbnd(3) .ne. ielbnd3(3)-iloff3(3)) correct=.false.
     if (eubnd(1) .ne. ieubnd3(1)+iuoff3(1)) correct=.false.
     if (eubnd(2) .ne. ieubnd3(2)+iuoff3(2)) correct=.false.
     if (eubnd(3) .ne. ieubnd3(3)+iuoff3(3)) correct=.false.
     if (ecnt(1) .ne. ieubnd3(1)-ielbnd3(1)+1+iloff3(1)+iuoff3(1)) correct=.false.
     if (ecnt(2) .ne. ieubnd3(2)-ielbnd3(2)+1+iloff3(2)+iuoff3(2)) correct=.false.
     if (ecnt(3) .ne. ieubnd3(3)-ielbnd3(3)+1+iloff3(3)+iuoff3(3)) correct=.false.

     if (clbnd(1) .ne. ielbnd3(1)-iloff3(1)) correct=.false.
     if (clbnd(2) .ne. ielbnd3(2)-iloff3(2)) correct=.false.
     if (clbnd(3) .ne. ielbnd3(3)-iloff3(3)) correct=.false.
     if (cubnd(1) .ne. ieubnd3(1)+iuoff3(1)) correct=.false.
     if (cubnd(2) .ne. ieubnd3(2)+iuoff3(2)) correct=.false.
     if (cubnd(3) .ne. ieubnd3(3)+iuoff3(3)) correct=.false.
     if (ccnt(1) .ne. ieubnd3(1)-ielbnd3(1)+1+iloff3(1)+iuoff3(1)) correct=.false.
     if (ccnt(2) .ne. ieubnd3(2)-ielbnd3(2)+1+iloff3(2)+iuoff3(2)) correct=.false.
     if (ccnt(3) .ne. ieubnd3(3)-ielbnd3(3)+1+iloff3(3)+iuoff3(3)) correct=.false.

  else  if (petCount .eq. 4) then
      call ESMF_GridGet(grid, localDE=0, &
             staggerLoc=staggerloc,                  &
             exclusiveLBound=elbnd, exclusiveUBound=eubnd,  exclusiveCount=ecnt,      &
             computationalLBound=clbnd, computationalUBound=cubnd, computationalCount=ccnt, rc=localrc)
             if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     if (localPet .eq. 0) then

        if (elbnd(1) .ne. ielbnd0(1)-iloff0(1)) correct=.false.
        if (elbnd(2) .ne. ielbnd0(2)-iloff0(2)) correct=.false.
        if (elbnd(3) .ne. ielbnd0(3)-iloff0(3)) correct=.false.
        if (eubnd(1) .ne. ieubnd0(1)+iuoff0(1)) correct=.false.
        if (eubnd(2) .ne. ieubnd0(2)+iuoff0(2)) correct=.false.
        if (eubnd(3) .ne. ieubnd0(3)+iuoff0(3)) correct=.false.
        if (ecnt(1) .ne. ieubnd0(1)-ielbnd0(1)+1+iloff0(1)+iuoff0(1)) correct=.false.
        if (ecnt(2) .ne. ieubnd0(2)-ielbnd0(2)+1+iloff0(2)+iuoff0(2)) correct=.false.
        if (ecnt(3) .ne. ieubnd0(3)-ielbnd0(3)+1+iloff0(3)+iuoff0(3)) correct=.false.

        if (clbnd(1) .ne. ielbnd0(1)-iloff0(1)) correct=.false.
        if (clbnd(2) .ne. ielbnd0(2)-iloff0(2)) correct=.false.
        if (clbnd(3) .ne. ielbnd0(3)-iloff0(3)) correct=.false.
        if (cubnd(1) .ne. ieubnd0(1)+iuoff0(1)) correct=.false.
        if (cubnd(2) .ne. ieubnd0(2)+iuoff0(2)) correct=.false.
        if (cubnd(3) .ne. ieubnd0(3)+iuoff0(3)) correct=.false.
        if (ccnt(1) .ne. ieubnd0(1)-ielbnd0(1)+1+iloff0(1)+iuoff0(1)) correct=.false.
        if (ccnt(2) .ne. ieubnd0(2)-ielbnd0(2)+1+iloff0(2)+iuoff0(2)) correct=.false.
        if (ccnt(3) .ne. ieubnd0(3)-ielbnd0(3)+1+iloff0(3)+iuoff0(3)) correct=.false.
     else if (localPet .eq. 1) then

        if (elbnd(1) .ne. ielbnd1(1)-iloff1(1)) correct=.false.
        if (elbnd(2) .ne. ielbnd1(2)-iloff1(2)) correct=.false.
        if (elbnd(3) .ne. ielbnd1(3)-iloff1(3)) correct=.false.
        if (eubnd(1) .ne. ieubnd1(1)+iuoff1(1)) correct=.false.
        if (eubnd(2) .ne. ieubnd1(2)+iuoff1(2)) correct=.false.
        if (eubnd(3) .ne. ieubnd1(3)+iuoff1(3)) correct=.false.
        if (ecnt(1) .ne. ieubnd1(1)-ielbnd1(1)+1+iloff1(1)+iuoff1(1)) correct=.false.
        if (ecnt(2) .ne. ieubnd1(2)-ielbnd1(2)+1+iloff1(2)+iuoff1(2)) correct=.false.
        if (ecnt(3) .ne. ieubnd1(3)-ielbnd1(3)+1+iloff1(3)+iuoff1(3)) correct=.false.

        if (clbnd(1) .ne. ielbnd1(1)-iloff1(1)) correct=.false.
        if (clbnd(2) .ne. ielbnd1(2)-iloff1(2)) correct=.false.
        if (clbnd(3) .ne. ielbnd1(3)-iloff1(3)) correct=.false.
        if (cubnd(1) .ne. ieubnd1(1)+iuoff1(1)) correct=.false.
        if (cubnd(2) .ne. ieubnd1(2)+iuoff1(2)) correct=.false.
        if (cubnd(3) .ne. ieubnd1(3)+iuoff1(3)) correct=.false.
        if (ccnt(1) .ne. ieubnd1(1)-ielbnd1(1)+1+iloff1(1)+iuoff1(1)) correct=.false.
        if (ccnt(2) .ne. ieubnd1(2)-ielbnd1(2)+1+iloff1(2)+iuoff1(2)) correct=.false.
        if (ccnt(3) .ne. ieubnd1(3)-ielbnd1(3)+1+iloff1(3)+iuoff1(3)) correct=.false.
     else if (localPet .eq. 2) then
        if (elbnd(1) .ne. ielbnd2(1)-iloff2(1)) correct=.false.
        if (elbnd(2) .ne. ielbnd2(2)-iloff2(2)) correct=.false.
        if (elbnd(3) .ne. ielbnd2(3)-iloff2(3)) correct=.false.
        if (eubnd(1) .ne. ieubnd2(1)+iuoff2(1)) correct=.false.
        if (eubnd(2) .ne. ieubnd2(2)+iuoff2(2)) correct=.false.
        if (eubnd(3) .ne. ieubnd2(3)+iuoff2(3)) correct=.false.
        if (ecnt(1) .ne. ieubnd2(1)-ielbnd2(1)+1+iloff2(1)+iuoff2(1)) correct=.false.
        if (ecnt(2) .ne. ieubnd2(2)-ielbnd2(2)+1+iloff2(2)+iuoff2(2)) correct=.false.
        if (ecnt(3) .ne. ieubnd2(3)-ielbnd2(3)+1+iloff2(3)+iuoff2(3)) correct=.false.

        if (clbnd(1) .ne. ielbnd2(1)-iloff2(1)) correct=.false.
        if (clbnd(2) .ne. ielbnd2(2)-iloff2(2)) correct=.false.
        if (clbnd(3) .ne. ielbnd2(3)-iloff2(3)) correct=.false.
        if (cubnd(1) .ne. ieubnd2(1)+iuoff2(1)) correct=.false.
        if (cubnd(2) .ne. ieubnd2(2)+iuoff2(2)) correct=.false.
        if (cubnd(3) .ne. ieubnd2(3)+iuoff2(3)) correct=.false.
        if (ccnt(1) .ne. ieubnd2(1)-ielbnd2(1)+1+iloff2(1)+iuoff2(1)) correct=.false.
        if (ccnt(2) .ne. ieubnd2(2)-ielbnd2(2)+1+iloff2(2)+iuoff2(2)) correct=.false.
        if (ccnt(3) .ne. ieubnd2(3)-ielbnd2(3)+1+iloff2(3)+iuoff2(3)) correct=.false.
     else if (localPet .eq. 3) then
        if (elbnd(1) .ne. ielbnd3(1)-iloff3(1)) correct=.false.
        if (elbnd(2) .ne. ielbnd3(2)-iloff3(2)) correct=.false.
        if (elbnd(3) .ne. ielbnd3(3)-iloff3(3)) correct=.false.
        if (eubnd(1) .ne. ieubnd3(1)+iuoff3(1)) correct=.false.
        if (eubnd(2) .ne. ieubnd3(2)+iuoff3(2)) correct=.false.
        if (eubnd(3) .ne. ieubnd3(3)+iuoff3(3)) correct=.false.
        if (ecnt(1) .ne. ieubnd3(1)-ielbnd3(1)+1+iloff3(1)+iuoff3(1)) correct=.false.
        if (ecnt(2) .ne. ieubnd3(2)-ielbnd3(2)+1+iloff3(2)+iuoff3(2)) correct=.false.
        if (ecnt(3) .ne. ieubnd3(3)-ielbnd3(3)+1+iloff3(3)+iuoff3(3)) correct=.false.

        if (clbnd(1) .ne. ielbnd3(1)-iloff3(1)) correct=.false.
        if (clbnd(2) .ne. ielbnd3(2)-iloff3(2)) correct=.false.
        if (clbnd(3) .ne. ielbnd3(3)-iloff3(3)) correct=.false.
        if (cubnd(1) .ne. ieubnd3(1)+iuoff3(1)) correct=.false.
        if (cubnd(2) .ne. ieubnd3(2)+iuoff3(2)) correct=.false.
        if (cubnd(3) .ne. ieubnd3(3)+iuoff3(3)) correct=.false.
        if (ccnt(1) .ne. ieubnd3(1)-ielbnd3(1)+1+iloff3(1)+iuoff3(1)) correct=.false.
        if (ccnt(2) .ne. ieubnd3(2)-ielbnd3(2)+1+iloff3(2)+iuoff3(2)) correct=.false.
        if (ccnt(3) .ne. ieubnd3(3)-ielbnd3(3)+1+iloff3(3)+iuoff3(3)) correct=.false.
     endif
  endif
end subroutine check2DP1Bnds2x2UsingSLoc


end program ESMF_GridCoordUTest
