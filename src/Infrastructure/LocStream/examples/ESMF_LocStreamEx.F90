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
!
!\subsubsection{Shortcut Creation Method for Single-Tile Grids}

! The method {\tt ESMF\_GridCreateShapeTile()} is a shortcut
! for building single tile logically rectangular Grids up to 
! three dimensions.
! It is partially implemented.  The user can specify Grid
! size, dimension and distribution, but cannot specify tile edge
! connectivities yet.  The default is that Grid edges are
! not connected.  Once completed, this method will enable users
! to create many common grid shapes, including
! rectangle, bipole sphere, and tripole sphere. 
!
! The {\tt ESMF\_GridCreateShapeTile()} method will eventually support 
! the three types of distributions described in 
! Section~\ref{sec:desc:dist}. It currently supports 
! two of these types: regular and irregular.
!
! To create a Grid 
! with a regular distribution the user specifies the global
! maximum and minimum ranges of the Grid cell index space ({\tt maxIndex} and
! {\tt minIndex}), and the number of pieces in which to partition
! each dimension (via a {\tt regDecomp} argument).
! ESMF then divides the index space as evenly as possible 
! into the specified number of pieces. If there are cells
! left over then they are distributed one per DE starting from
! the first DE until they are gone.
!
! If {\tt minIndex} is 
! not specified, then the bottom of the Grid cell index range is assumed
! to be (1,1,...,1). If {\tt regDecomp} is not specified, then
! by default ESMF creates a distribution that partitions the
! grid cells in the first dimension (e.g. NPx1x1...1) as evenly 
! as possible by  the number of processors NP.
! The remaining dimensions are not partitioned.
! The dimension of the Grid is the size of {\tt maxIndex}. 
! The following is an example of creating a 10x20x30 3D grid
! where the first dimensions is broken into 2 pieces, the second
! is broken into 4 pieces, and the third is "distributed" across only one processor. 
!EOE

!BOC
  grid3D=ESMF_GridCreateShapeTile(regDecomp=(/2,4,1/), maxIndex=(/10,20,30/), &
           rc=rc)   
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
   !-------------------------------------------------------------------
   ! Clean up to prepare for the next example.
   !-------------------------------------------------------------------
  call ESMF_GridDestroy(grid3D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
! Irregular distribution requires the user to specify the
! exact number of Grid cells per DE in each dimension.  In the
! {\tt ESMF\_GridCreateShapeTile()} call the {\tt countsPerDEDim1},
! {\tt countsPerDim2}, and {\tt countsPerDim3}
! arguments are used to specify a rectangular distribution
! containing size(countsPerDEDim1) by size(countsPerDEDim2) by
! size(countsPerDEDim3) DEs. The entries in each of these arrays
! specify the number of grid cells per DE in that dimension.
! The dimension of the grid is determined by the presence of
! {\tt countsPerDEDim3}.  If it's present the Grid
! will be 3D. If just {\tt countsPerDEDim1} and 
! {\tt countsPerDEDim2} are specified the Grid 
! will be 2D.
!
! The following call illustrates the creation of 
! a 10x20 two dimensional rectangular Grid distributed across six DEs
! that are arranged 2x3.  In the first dimension there are 3 grid
! cells on the first DE and 7 cells on the second DE.  The second 
! dimension has 3 DEs with 11,2, and 7 cells, respectively.
!EOE

!BOC
   grid2D=ESMF_GridCreateShapeTile(countsPerDEDim1=(/3,7/), &
          countsPerDEDim2=(/11,2,7/), rc=rc)   
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!!!!!!!!!!!!!!!!!!!!!!!
! Cleanup after Example
!!!!!!!!!!!!!!!!!!!!!!
call ESMF_GridDestroy(grid2D,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!BOE
! To add a distributed third dimension of size 30, broken up into
! two groups of 15, the above call would be altered as follows. 
!EOE

!BOC
   grid3d=ESMF_GridCreateShapeTile(countsPerDEDim1=(/3,7/), &
          countsPerDEDim2=(/11,2,7/), countsPerDEDim3=(/15,15/), rc=rc)   
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!!!!!!!!!!!!!!!!!!!!!!!
! Cleanup after Example
!!!!!!!!!!!!!!!!!!!!!!
call ESMF_GridDestroy(grid3D,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!BOE 
! To make a third dimension distributed across only 1 DE, then 
! {\tt countsPerDEDim3} in the call should only have a single term.
!EOE

!BOC
   grid3D=ESMF_GridCreateShapeTile(countsPerDEDim1=(/3,7/),  &
          countsPerDEDim2=(/11,2,7/), countsPerDEDim3=(/30/), rc=rc)   
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!!!!!!!!!!!!!!!!!!!!!!!
! Cleanup after Example
!!!!!!!!!!!!!!!!!!!!!!
call ESMF_GridDestroy(grid3D,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!BOE
!
! The {\tt petMap} parameter may be used to specify on to which specific PETs 
! the DEs in the Grid are assigned. Note that this parameter is only available for the 
! regular and irregular distribution types. The {\tt petMap} 
! array is a 3D array, for a 3D Grid each of its dimensions correspond to a
! Grid dimension. If the Grid is 2D, then the first two dimensions correspond
! to Grid dimensions and the last dimension should be of size 1. 
! The size of each {\tt petMap} dimension is
! the number of DE's along that dimension in the Grid. For a 
! regular Grid, the size is equal to the number in regDecomp 
! (i.e. {\tt size(petMap,d)=regDecomp(d)} for all dimensions {\tt d} in the Grid). For
! an irregular Grid the size is equal to the number of items in
! the corresponding {\tt countsPerDEDim} variable (i.e. 
! {\tt size(petMap,d)=size(countsPerDEDimd)} for all dimensions {\tt d} in the Grid).
! 
! Each entry in {\tt petMap} specifies to which PET the corresponding
! DE should be assigned. For example, {\tt petMap(3,2)=4} tells the Grid
! create call to put the DE located at column 3 row 2 on PET 4.
!
! The following example demonstrates how to specify the PET to DE association 
! for an {\tt ESMF\_GridCreateShapeTile()} call.
!EOE

! Skip if not right number of procs.
if (petcount .eq. 4) then 
!BOC
   ! allocate memory for petMap
   allocate( petMap(2,2,1) )

   ! Set petMap
   petMap(:,1,1) = (/3,2/) ! DE (1,1,1) on PET 3 and DE (2,1,1) on PET 2
   petMap(:,2,1) = (/1,0/) ! DE (1,2,1) on PET 1 and DE (2,2,1) on PET 0


   ! Let the 3D grid be be distributed only in the first two dimensions.
   grid2D=ESMF_GridCreateShapeTile(countsPerDEDim1=(/3,7/), &
           countsPerDEDim2=(/7,6/), petMap=petMap, rc=rc)   
!EOC

!!!!!!!!!!!!!!!!!!!!!!!
! Cleanup after Example
!!!!!!!!!!!!!!!!!!!!!!
   call ESMF_GridDestroy(grid2D, rc=rc)
   deallocate( petMap )
endif


!BOE
! Arbitrary distribution has not yet been implemented.
! The design is that the user specifies the global minimum and maximum
! ranges of the index space with the
! arguments {\tt minIndex} and {\tt maxIndex}, and through a {\tt localIndices}
! argument specifies the set of index space locations residing on the local PET.
! Again, if {\tt minIndex} is  not specified, then the bottom of the 
! index range is assumed to be (1,1,...). 
! The dimension of the Grid is equal to the size of {\tt maxIndex}. 
!
! The following example creates a 2D Grid of dimensions 5x5, and places
! the diagonal elements (i.e. indices (i,i) where i goes from 1 to 5)
! on the local PET. The remaining PETs would individually declare
! the remainder of the Grid locations. 
!EOE

!BOC
   ! allocate memory for local
   allocate( localIndices(2,5) )
   ! Set local indices
   localIndices(:,1)=(/1,1/)
   localIndices(:,2)=(/2,2/)
   localIndices(:,3)=(/3,3/)
   localIndices(:,4)=(/4,4/)
   localIndices(:,5)=(/5,5/)

   ! Create Grid
!  grid2D=ESMF_GridCreateShapeTile(maxIndex=(/5,5/), & ! NOT YET IMPLEMENTED
!         localIndices=localIndices, rc=rc)   
!EOC

   !-------------------------------------------------------------------
   ! Clean up to prepare for the next example.
   !-------------------------------------------------------------------
!   call ESMF_GridDestroy(grid2D, rc=rc)
   deallocate( localIndices )




!BOE
!\subsubsection{Creating a 2D Regularly Distributed Rectilinear Grid
!                  With Uniformly Spaced Coordinates}
! \label{example:2DRegUniGrid}
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


!BOE
!
! The remaining examples in this section will use the irregular 
! distribution because of its greater generality. To create code similar to these, but
! using a regular distribution, replace the {\tt countsPerDEDim} arguments
! in the Grid create with the appropriate {\tt maxIndex} and {\tt regDecomp} arguments. 
!
!\subsubsection{Creating a 2D Irregularly Distributed Rectilinear Grid
!                  With Uniformly Spaced Coordinates}
! \label{example:2DIrregUniGrid}
!
! This example serves as an illustration of the difference between using
! a regular and irregular distribution. It repeats the previous example
! except using an irregular distribution to give the user more control
! over how the cells are divided between the DEs. As before, this code
! creates a 10x20 2D Grid with uniformly spaced coordinates  varying from (10,10) to (100,200).
! In this example, the Grid is partitioned using an irregular distribution. The first dimension
! is divided into two pieces, the first with 3 Grid cells per
! DE and the second with 7 Grid cells per DE. In the second dimension,
! the Grid is divided into 3 pieces, with 11, 2, and 7 cells per DE respectively.
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
   ! will have global coordinates.  Note that aperiodic bounds are
   ! specified by default - if periodic bounds were desired they
   ! would need to be specified using an additional gridConn argument
   ! (which isn't implemented yet). In this call the minIndex hasn't 
   ! been set, so it defaults to (1,1,...).
   !-------------------------------------------------------------------
   grid2D=ESMF_GridCreateShapeTile(          &
            ! Define an irregular distribution
            countsPerDEDim1=(/3,7/),    &
            countsPerDEDim2=(/11,2,7/), &
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

!BOE
!
!\subsubsection{Creating a 2D Irregularly Distributed Grid
!                  With Curvilinear Coordinates}
! \label{example:2DIrregCurviGrid}
!
! The following is an example of creating a simple curvilinear Grid and
! loading in a set of coordinates. It creates a 10x20
! 2D Grid where the coordinates vary along every dimension. 
! The Grid is partitioned using an irregular distribution. The first dimension
! is divided into two pieces, the first with 3 Grid cells per
! DE and the second with 7 Grid cells per DE. In the second dimension,
! the Grid is divided into 3 pieces, with 11, 2, and 7 cells per DE respectively.
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
   ! distribution of the Grid, and specify that it 
   ! will have global indices.  Note that aperiodic bounds are
   ! specified by default - if periodic bounds were desired they
   ! would need to be specified using an additional gridConn argument
   ! (which isn't implemented yet). In this call the minIndex hasn't 
   ! been set, so it defaults to (1,1,...).
   !-------------------------------------------------------------------
   grid2D=ESMF_GridCreateShapeTile(      &
            ! Define an irregular distribution
            countsPerDEDim1=(/3,7/),     &
            countsPerDEDim2=(/11,2,7/),   &
            ! Specify mapping of coords dim to Grid dim
            coordDep1=(/1,2/), & ! 1st coord is 2D and depends on both Grid dim
            coordDep2=(/1,2/), & ! 2nd coord is 1D and depends on both Grid dim
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
          computationalLBound=lbnd, computationalUBound=ubnd, fptr=coordX2D, rc=rc)

   !-------------------------------------------------------------------
   ! Calculate and set coordinates in the first dimension [10-100].
   !-------------------------------------------------------------------
   do j=lbnd(2),ubnd(2)
   do i=lbnd(1),ubnd(1)
        coordX2D(i,j) = i+j
   enddo
   enddo

   !-------------------------------------------------------------------
   ! Get the pointer to the second coordinate array and the bounds of 
   ! its global indices on the local DE.
   !-------------------------------------------------------------------
   call ESMF_GridGetCoord(grid2D, coordDim=2, localDE=0, &
          staggerloc=ESMF_STAGGERLOC_CENTER, &
          computationalLBound=lbnd, computationalUBound=ubnd, fptr=coordY2D, rc=rc)

   !-------------------------------------------------------------------
   ! Calculate and set coordinates in the second dimension [10-200]
   !-------------------------------------------------------------------
   do j=lbnd(2),ubnd(2)
   do i=lbnd(1),ubnd(1)
        coordY2D(i,j) = j-i/100.0
   enddo
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
