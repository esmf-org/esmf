!
! Earth System Modeling Framework
! Copyright 2002-2008, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#undef LOCAL_NOT_IMPL 
program ESMF_LocStreamEx

!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================


!  !PROGRAM: ESMF_LocStreamEx - LocStream examples.
!
!  !DESCRIPTION: 
!

      ! Use ESMF framework module
      use ESMF_Mod
      implicit none

      ! Parameters
      integer, parameter :: ESMF_Coord1=1, ESMF_Coord2=2, ESMF_Coord3=3
      ! Local variables  
      integer:: rc, finalrc
      type(ESMF_VM):: vm
      type(ESMF_ArraySpec) ::  arrayspec2D,arrayspec

      real(ESMF_KIND_R8), pointer :: centerX(:), centerY(:), centerZ(:)
      real(ESMF_KIND_R8), pointer :: cornerX(:), cornerY(:), cornerZ(:)
      real(ESMF_KIND_R8), pointer :: coordX2D(:,:), coordY2D(:,:)
      real(ESMF_KIND_R8), pointer :: coordX(:), coordY(:)

      integer :: elbnd(3),eubnd(3)
      integer :: clbnd(3),cubnd(3)
      integer :: celwdth(2),ceuwdth(2)
      integer :: tlbnd(3),tubnd(3)
      integer :: i,j,k
      integer :: lbnd(3), ubnd(3), lbnd_corner(3), ubnd_corner(3)
      integer, allocatable :: petMap(:,:,:), localIndices(:,:)
      integer :: distgridToGridMap(2)
      integer :: lbnd1D(1), ubnd1D(1)

      type(ESMF_Grid) :: grid2D, grid3D, grid4D
      type(ESMF_Array) :: arrayCoordX, arrayCoordY,array

      type(ESMF_distGrid) :: distgrid2D,distgrid4D,distgrid
      type(ESMF_StaggerLoc) :: staggerloc
      integer :: localPet, petCount
      integer :: lDE,localDECount

      ! initialize ESMF
      finalrc = ESMF_SUCCESS
  call ESMF_Initialize(vm=vm,  rc=rc)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!BOE
!\subsubsection{Creating a 2D Regularly Distributed Rectilinear Grid With Uniformly Spaced Coordinates}
!
! The following is an example of creating a simple rectilinear grid 
! and loading in a set of coordinates. It illustrates a straightforward use
! of the {\tt ESMF\_GridCreateShapeTile()} call described in the previous section. 
! This code creates a 10x20 2D grid with uniformly spaced coordinates varying from (10,10) to (100,200).
! The grid is partitioned using a regular distribution. The first dimension
! is divided into two pieces, and the second dimension is divided into 3.
! This example assumes that the code is being run with a 1-1 mapping between 
! PETs and DEs because we are only accessing the first DE on each PET (localDE=0).
! Because we have 6 DEs (2x3), this example would only work when run on 6 PETs. 
! The Grid is created with global indices. After Grid creation the
! local bounds and native Fortran arrays are retrieved and the
! coordinates are set by the user. 
!
!EOE

! Don't run without correct number of procs
if (petCount .le. 6) then

!BOC
   !-------------------------------------------------------------------
   ! Create the Grid:  Allocate space for the Grid object, define the
   ! topology and distribution of the Grid, and specify that it 
   ! will have global indices.  Note that aperiodic bounds are
   ! specified by default - if periodic bounds were desired they
   ! would need to be specified using an additional gridConn argument
   ! (which isn't implemented yet). In this call the minIndex hasn't 
   ! been set, so it defaults to (1,1,...). The default is to 
   ! divide the index range as equally as possible among the DEs
   ! specified in regDecomp. This behavior can be changed by 
   ! specifying decompFlag. 
   !-------------------------------------------------------------------
   grid2D=ESMF_GridCreateShapeTile(          &
            ! Define a regular distribution
            maxIndex=(/10,20/), & ! define index space
            regDecomp=(/2,3/),  & ! define how to divide among DEs
            ! Specify mapping of coords dim to Grid dim
            coordDep1=(/1/), & ! 1st coord is 1D and depends on 1st Grid dim
            coordDep2=(/2/), & ! 2nd coord is 1D and depends on 2nd Grid dim
            indexflag=ESMF_INDEX_GLOBAL, &
            rc=rc)

   !-------------------------------------------------------------------
   ! Allocate coordinate storage and associate it with the center
   ! stagger location.  Since no coordinate values are specified in
   ! this call no coordinate values are set yet.
   !-------------------------------------------------------------------
   call ESMF_GridAddCoord(grid2D,  & 
          staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)

   !-------------------------------------------------------------------
   ! Get the pointer to the first coordinate array and the bounds
   ! of its global indices on the local DE.   
   !-------------------------------------------------------------------
   call ESMF_GridGetCoord(grid2D, coordDim=1, localDE=0, &
          staggerloc=ESMF_STAGGERLOC_CENTER, &
          computationalLBound=lbnd, computationalUBound=ubnd, fptr=coordX, rc=rc)

   !-------------------------------------------------------------------
   ! Calculate and set coordinates in the first dimension [10-100].
   !-------------------------------------------------------------------
   do i=lbnd(1),ubnd(1)
        coordX(i) = i*10.0
   enddo

   !-------------------------------------------------------------------
   ! Get the pointer to the second coordinate array and the bounds of
   ! its global indices on the local DE.
   !-------------------------------------------------------------------
   call ESMF_GridGetCoord(grid2D, coordDim=2, localDE=0, &
          staggerloc=ESMF_STAGGERLOC_CENTER, &
          computationalLBound=lbnd, computationalUBound=ubnd, fptr=coordY, rc=rc)

   !-------------------------------------------------------------------
   ! Calculate and set coordinates in the second dimension [10-200]
   !-------------------------------------------------------------------
   do j=lbnd(1),ubnd(1)
        coordY(j) = j*10.0
   enddo
!EOC
   !-------------------------------------------------------------------
   ! Clean up to prepare for the next example.
   !-------------------------------------------------------------------
   call ESMF_GridDestroy(grid2D, rc=rc)
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
endif



   !-------------------------------------------------------------------
   ! Shut down and end.
   !-------------------------------------------------------------------
10 continue
  call ESMF_Finalize(rc=rc)
  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_GridUsageEx.F90"
  else
    print *, "FAIL: ESMF_GridUsageEx.F90"
  endif
  
end program
