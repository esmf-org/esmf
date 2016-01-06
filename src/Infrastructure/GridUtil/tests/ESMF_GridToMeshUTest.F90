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
program ESMF_GridToMeshUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_GridToMeshUTest - Check Structured Grid to Mesh Conversion Routines
!
! !DESCRIPTION:
!
! The code in this file drives F90 GridToMesh unit tests.
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
  type(ESMF_Grid) :: grid2D
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtr2D(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: i1,i2
  integer :: lDE, localDECount
  type(ESMF_Mesh) :: mesh

  !-----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !-----------------------------------------------------------------------------

  ! get global VM
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !-----------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Test GridToMesh"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! init success flag
  correct=.true.
  rc=ESMF_SUCCESS

   grid2D=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/10,10/),regDecomp=(/4,4/), &
                              gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
   if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Allocate coordinates
  call ESMF_GridAddCoord(grid2D, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get number of local DEs
  call ESMF_GridGet(grid2D, localDECount=localDECount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get memory and set coords
  do lDE=0,localDECount-1


 
     !! get coord 1
     call ESMF_GridGetCoord(grid2D, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtr2D, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE    

    write(*,*) lDE," ::",clbnd,":",cubnd

     !! set coord 1  
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        farrayPtr2D(i1,i2)=REAL(i1,ESMF_KIND_R8)
     enddo
     enddo

     !! get coord 2
     call ESMF_GridGetCoord(grid2D, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtr2D, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE    

     !! set coord 2  
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        farrayPtr2D(i1,i2)=REAL(i2,ESMF_KIND_R8)
     enddo
     enddo
  enddo    

  ! Create a mesh from the grid
  mesh=ESMF_GridToMesh(grid=grid2D, staggerLoc=ESMF_STAGGERLOC_CENTER, &
            isSphere=0, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE    

  call ESMF_GridDestroy(grid2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE    

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
end program ESMF_GridToMeshUTest
