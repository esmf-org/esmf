! $Id: ESMF_GridUsageEx.F90,v 1.8 2007/07/20 05:12:14 cdeluca Exp $
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
#undef LOCAL_NOT_IMP 
program ESMF_GridCreateEx

!==============================================================================
!EXAMPLE        String used by test script to count examples.
!==============================================================================


!  !PROGRAM: ESMF_GridCreateEx - Examples of Grid creation.
!
!  !DESCRIPTION: 
!
! This program contains a set of Grid creation examples

      ! Use ESMF framework module
      use ESMF_Mod
      implicit none

      ! Parameters
      integer, parameter :: ESMF_Coord1=1, ESMF_Coord2=2, ESMF_Coord3=3
      ! Local variables  
      integer:: rc, finalrc
      type(ESMF_VM):: vm
      type(ESMF_ArraySpec) ::  arrayspec2D

      real(ESMF_KIND_R8), pointer :: coordX(:,:), coordY(:,:), coordZ(:)
      real(ESMF_KIND_R8), pointer :: cornerX(:,:), cornerY(:,:)

      integer :: i,j,k
      integer :: lbnd(2), ubnd(2), lbnd_corner(2), ubnd_corner(2)
      integer, allocatable :: petMap(:,:,:), localIndices(:,:)

      type(ESMF_Grid) :: grid, grid2D, grid3D, grid4D
      type(ESMF_Array) :: arrayCoordX, arrayCoordY

      type(ESMF_distGrid) :: distgrid2D,distgrid4D
      type(ESMF_StaggerLoc) :: staggerloc
      integer :: localPet, petCount

      ! initialize ESMF
      finalrc = ESMF_SUCCESS
  call ESMF_Initialize(vm=vm,  rc=rc)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
!\subsubsection{Creating a 2D Irregularly Distributed Grid
!                  With Uniformly Spaced Coordinates}
! \label{example:2DIrregUniGrid}
!
! The following is an example of creating a simple rectilinear grid and
! loading in a set of coordinates. This code creates a 10x20
! 2D grid with uniformly spaced coordinates varying from (10,10) to (100,200).
! The grid is partitioned using an irregular distribution. The first dimension
! it is divided into two pieces, the first with 3 grid cells per
! DE and the second with 7 grid cells per DE. In the second dimension,
! the Grid is divided into 3 pieces, with 5, 9, and 6 cells per DE respectively.
! The Grid is created with global indices. After grid creation the
! local bounds and native Fortran arrays are retrieved and the
! coordinates are set by the user. 
!
!EOE

!BOC
   !-------------------------------------------------------------------
   ! Create the Grid:  Allocate space for the Grid object, define the
   ! topology and distribution of the grid, and specify that it 
   ! will have global coordinates.  Note that aperiodic bounds are
   ! specified by default - if periodic bounds were desired they
   ! would need to be specified using an additional gridConn argument.
   !-------------------------------------------------------------------
   grid2D=ESMF_GridCreateShape(          &
            ! Define an irregular distribution
            countsPerDEDim1=(/3,7/),     &
            countsPerDEDim2=(/4,7,6/),   &
            indexflag=ESMF_INDEX_GLOBAL,       & ! Create grid with global indices
            rc=rc)

   !-------------------------------------------------------------------
   ! Allocate coordinate storage and associate it with the center
   ! stagger location.  Since no coordinate values are specified in
   ! this call no coordinate values are set yet.
   !-------------------------------------------------------------------
   call ESMF_GridAllocCoord(grid2D,  & 
          staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)

   !-------------------------------------------------------------------
   ! Get the local bounds of the global indexing for the first 
   ! coordinate array on the local DE.
   !-------------------------------------------------------------------
   call ESMF_GridGetLocalTileInfo(grid2D, coord=1, &
          staggerloc=ESMF_STAGGERLOC_CENTER,       &
          computationalLBound=lbnd, computationalUBound=ubnd, rc=rc)

   !-------------------------------------------------------------------
   ! Get the pointer to the first coordinate array from inside
   ! the Grid object.
   !-------------------------------------------------------------------
   call ESMF_GridGetLocalTileCoord(grid2D, coord=1, &
          staggerloc=ESMF_STAGGERLOC_CENTER, fptr=coordX, rc=rc)

   !-------------------------------------------------------------------
   ! Calculate and set coordinates in the first dimension [10-100].
   !-------------------------------------------------------------------
   do j=lbnd(2),ubnd(2)
   do i=lbnd(1),ubnd(1)
        coordX(i,j) = i*10.0
   enddo
   enddo

   !-------------------------------------------------------------------
   ! Get the local bounds of the global indexing for the  second
   ! coordinate array on the local DE.
   !-------------------------------------------------------------------
   call ESMF_GridGetLocalTileInfo(grid2D, coord=2, &
          staggerloc=ESMF_STAGGERLOC_CENTER,       &
          computationalLBound=lbnd, computationalUBound=ubnd, rc=rc)

   !-------------------------------------------------------------------
   ! Get the pointer to the second coordinate array from inside
   ! the Grid object.
   !-------------------------------------------------------------------
   call ESMF_GridGetLocalTileCoord(grid2D, coord=2, &
          staggerloc=ESMF_STAGGERLOC_CENTER, fptr=coordY, rc=rc)

   !-------------------------------------------------------------------
   ! Calcuate and set coordinates in the second dimension [10-200]
   !-------------------------------------------------------------------
   do j=lbnd(2),ubnd(2)
   do i=lbnd(1),ubnd(1)
        coordY(i,j) = i*10.0
   enddo
   enddo
!EOC
   !-------------------------------------------------------------------
   ! Clean up to prepare for the next example.
   !-------------------------------------------------------------------
   call ESMF_GridDestroy(grid2D, rc=rc)
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


#ifdef LOCAL_NOT_IMPL
!BOEI
!\subsubsection{Creating a Regularly Distributed Grid with
!               Horizontal Curvilinear Coordinates
!               and multiple stagger locations, 
!               with an Undistributed Vertical Dimension}
! \label{example:CurviGridWithUndistDim}
!
! This example demonstrates how a user can build a curvilinear 
! horizontal grid with an undistributed vertical dimension. the Grid 
! contains both the center and corner stagger locations (i.e. Arakawa 
! B-Grid). This might be, for example, a displaced pole grid used in
! ocean modeling where the north pole has been relocated to a land
! mass in order to remove the pole from the computation region.
! Typically the ocean depth is placed in the undistributed third
! dimension because the size of the depth dimension is typically
! much less than that of the horizontal grid dimension.
!
!EOEI

!BOCI
   !-------------------------------------------------------------------
   ! Create the Grid:  Allocate space for the Grid object.  The
   ! grid is defined to be 180 elements in Dim1 (longitude), 90 in
   ! Dim2 (longitude), and 40 in Dim3 (depth).  The first dimension is
   ! decomposed over 10 DEs, the second over 5 DEs, and the third is 
   ! undistributed.  The connectivity of the distribution specifies that
   ! the longitude dimension is periodic, that the poles are defined at 
   ! each end of the latitude dimension, and the depth defaults to 
   ! aperiodic bounds. (See section~\ref{example:TileEdgeConn})
   !-------------------------------------------------------------------
   grid3D=ESMF_GridCreateShape( 
              maxIndex=(/180,90,40/),  &   ! upper extent of grid memory
              regDecomp=(/10, 5, 1/),  &   ! block decomposition       
              ! Specify connectivity
              connDim1=(/ESMF_GRIDCONN_PERIODIC,ESMF_GRIDCONN_PERIODIC/),  &
              connDim2=(/ESMF_GRIDCONN_POLE,ESMF_GRIDCONN_POLE/),          &
              coordDep1=(/1,2/),       &  ! Coordinate dependencies (curvilinear horizontal)
              coordDep2=(/1,2/),       &
              coordDep3=(/3/),         &  ! Coordinate dependencies (rectilinear vertical)
              indexflag=ESMF_GLOBAL,   &  ! Create grid with global indices
              rc=rc)

   !-------------------------------------------------------------------
   ! Allocate coordinate storage for both center and corner stagger
   ! location.  Since no coordinate values are specified in this
   ! call no coordinate values are set yet.
   !-------------------------------------------------------------------
   call ESMF_GridSetCoord(grid3D, ESMF_STAGGERLOC_CENTER, rc)
   call ESMF_GridSetCoord(grid3D, ESMF_STAGGERLOC_CORNER, rc)

!----------------------------------------------------------------------
! Fill in  the coordinates for the non-center stagger location first.
!----------------------------------------------------------------------
   !-------------------------------------------------------------------
   ! Get the local bounds of the global indexing for the first
   ! coordinate array on the local DE.
   !-------------------------------------------------------------------
   call ESMF_GridGetLocalTileInfo(grid3D, ESMF_Coord1,          &
          staggerLoc=ESMF_STAGGERLOC_CORNER,                    &
          computationalLBound=lbnd_corner,                      &
          computationalUBound=ubnd_corner,                      &
          rc=rc)

   !-------------------------------------------------------------------
   ! Get the pointer to the first coordinate array from inside
   ! the Grid object.
   !-------------------------------------------------------------------
   call ESMF_GridGetLocalTileCoord(grid3D, ESMF_Coord1, cornerX, rc=rc)

   !-------------------------------------------------------------------
   ! Calculate and set coordinates in the first dimension.
   !-------------------------------------------------------------------
   do j=lbnd_corner(2),ubnd_corner(2)
   do i=lbnd_corner(1),ubnd_corner(1)
        cornerX(i,j) = (i-1)*(360.0/180.0)
   enddo
   enddo

   !-------------------------------------------------------------------
   ! Get the local bounds of the global indexing for the second
   ! coordinate array on the local DE.
   !-------------------------------------------------------------------
   call ESMF_GridGetLocalTileInfo(grid3D, ESMF_Coord2, &
          staggerLoc=ESMF_STAGGERLOC_CORNER,                    &
          computationalLBound=lbnd_corner,                      &
          computationalUBound=ubnd_corner,                      &
          rc=rc)

   !-------------------------------------------------------------------
   ! Get the pointer to the second coordinate array from inside
   ! the Grid object.
   !-------------------------------------------------------------------
   call ESMF_GridLocalTileGetCoord(grid3D, ESMF_Coord2, cornerY, rc=rc)

   !-------------------------------------------------------------------
   ! Calcuate and set coordinates in the second dimension.
   !-------------------------------------------------------------------
   do j=lbnd_corner(2),ubnd_corner(2)
   do i=lbnd_corner(1),ubnd_corner(1)
        cornerY(i,j) = (i-1)*(180.0/90.0)
   enddo
   enddo

!----------------------------------------------------------------------
! Now fill the coordinates for the center stagger location by averaging 
! the corner locations.
!----------------------------------------------------------------------
   !-------------------------------------------------------------------
   ! Get the local bounds of the global indexing for the first 
   ! coordinate array on the local DE.
   !-------------------------------------------------------------------
   call ESMF_GridGetLocalTileInfo(grid3D, ESMF_Coord1, &
          computationalLBound=lbnd, computationalUBound=ubnd, rc=rc)

   !-------------------------------------------------------------------
   ! Get the pointer to the first coordinate array from inside
   ! the Grid object.
   !-------------------------------------------------------------------
   call ESMF_GridGetLocalTileCoord(grid3D, ESMF_Coord1, coordX, rc=rc)

   !-------------------------------------------------------------------
   ! Calculate and set coordinates in the first dimension.
   !-------------------------------------------------------------------
   do j=lbnd(2),ubnd(2)
   do i=lbnd(1),ubnd(1)
        coordX(i,j) = 0.25*(  cornerX(i,j) + cornerX(i+1,j) +     &
                              cornerX(i+1,j+1) + cornerX(i,j+1)  )
   enddo
   enddo

   !-------------------------------------------------------------------
   ! Get the local bounds of the global indexing for the second
   ! coordinate array on the local DE.
   !-------------------------------------------------------------------
   call ESMF_GridGetLocalTileInfo(grid3D, ESMF_Coord2, &
          computationalLBound=lbnd, computationalUBound=ubnd, rc=rc)

   !-------------------------------------------------------------------
   ! Get the pointer to the second coordinate array from inside
   ! the Grid object.
   !-------------------------------------------------------------------
   call ESMF_GridLocalTileGetCoord(grid3D, ESMF_Coord2, coordY, rc=rc)

   !-------------------------------------------------------------------
   ! Calcuate and set coordinates in the second dimension.
   !-------------------------------------------------------------------
   do j=lbnd(2),ubnd(2)
   do i=lbnd(1),ubnd(1)
        coordY(i,j) = 0.25*(  cornerY(i,j) + cornerY(i+1,j) +     &
                              cornerY(i+1,j+1) + cornerY(i,j+1)  )
   enddo
   enddo

   !-------------------------------------------------------------------
   ! Get the pointer to the third coordinate array from inside
   ! the Grid object.
   !-------------------------------------------------------------------
   call ESMF_GridLocalTileGetCoord(grid3D, ESMF_Coord3, coordZ, rc=rc)

   !-------------------------------------------------------------------
   ! Calcuate and set the vertical coordinates
   !-------------------------------------------------------------------
   do k=1, 40
        coordZ(k) = 4000.0*( (1./39.)*(i-1)  )**2
   enddo


!EOCI
   !-------------------------------------------------------------------
   ! Clean up to prepare for the next example.
   !-------------------------------------------------------------------
   call ESMF_GridDestroy(grid3D, rc=rc)
#endif


#ifdef LOCAL_NOT_IMP
!BOEI
!\subsubsection{Creating an Empty Grid in a Parent Component 
! for Completion in a Child Component}\label{sec:usage:setcommit}
!
! ESMF Grids can be created using an incremental paradigm. To do this,
! the user first calls ESMF\_GridCreateEmpty to allocate the shell of
! a Grid. Next, a series of {\tt ESMF\_GridSet()} calls are used to fill
! in the details of the grid. Here we use a convenient {\tt ESMF\_GridSetShape()}
! call that fills in the Grid via an interface much like the
! {\tt ESMF\_GridCreateShape()} call. Finally, after the set calls,
! any other grid call will internally validate and create the final, 
! usable, grid. For consistency's sake the initial {\tt ESMF\_GridCreateEmpty}
! call must occur on the same or a superset of the processors as the
! {\tt ESMF\_GridSet()} calls. The following example uses the incremental
! technique to create a rectangular 10x20 grid with coordinates at the
! center and corner stagger locations. 
!EOEI

!BOCI
!---------------------------------------------------------------------------
! IN THE PARENT COMPONENT:
! Create an empty grid in the parent component for use in a child component.
! The parent may be defined on more PETs than the child component.  
! The child's [vm or pet list] is passed into the create call so that
! the Grid is defined on the appropriate subset of the parent's PETs. 
!---------------------------------------------------------------------------
   grid2D=ESMF_GridCreateEmpty(                       &
                               petList=/1,2,3,4/,     & ! Argument NOT IMPLEMENTED
                               rc=rc)

!---------------------------------------------------------------------------
! IN THE CHILD COMPONENT:
! Set the grid topology.  Here we define an irregularly distributed 
! rectangular Grid.
!---------------------------------------------------------------------------

   call ESMF_GridSetShape(grid2D,                    &
                          countsPerDEDim1=(/5,5/),   &
                          countsPerDEDim2=(/7,7,6/), rc=rc)

!---------------------------------------------------------------------------
! Set Grid coordinates at the cell center location.
!---------------------------------------------------------------------------
   call ESMF_GridSetCoord(grid2D, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)

!---------------------------------------------------------------------------
! Set Grid coordinates at the corner stagger location.
!---------------------------------------------------------------------------
   call ESMF_GridSetCoord(grid2D, staggerLoc=ESMF_STAGGERLOC_CORNER, rc=rc)
!EOCI
   !-------------------------------------------------------------------
   ! Clean up to prepare for the next example.
   !-------------------------------------------------------------------
   call ESMF_GridDestroy(grid2D, rc=rc)

#endif

!BOE
!
!\subsubsection{Shortcut Creation Method for Single-Tile Grids}

! The method {\tt ESMF\_GridCreateShape()} is a shortcut
! for specifying single tile logically rectangular grids.
! It allows the user to specify the connections for each 
! edge for grids up to three dimensions. By employing this method 
! the user can create many common grid shapes, including
! rectangle, bipole (lat-lon) sphere, and tripole sphere. 
! The user can define more complex topologies by creating
! a DistGrid object and using a more general Grid create
! interface. See Section~\ref{sec:usage:adv:create}
! for more details.
!
! The shortcut grid creation method supports the three types of
! distributions discussed in section~\ref{sec:desc:dist}. In 
! addition to describing the partitioning of the index space,
! these methods also simultaneously describe its size and rank.
!
! Regular distribution is not yet implemented, but the design is as follows:
! To use employ a regular distribution, the user specifies the global
! maximum and minimum ranges of the index space ({\tt maxIndex} and
! {\tt minIndex}), and the number of pieces in which to partition
! each dimension (via {\tt regDecomp}).
! ESMF then divides the index space as evenly as possible 
! into the specified number of pieces. If {\tt minIndex} is 
! not specified, then the bottom of the index range is assumed
! to be (1,1,...,1). If {\tt regDecomp} is not specified, then
! by default ESMF creates a distribution that evenly partitions the
! first dimension (e.g. NPx1x1...1) by the number of processors NP.
! The remaining dimensions are not partitioned.
! The rank of the Grid is the size of {\tt maxIndex}. To create
! an undistributed dimension set that entry in {\tt regDecomp}
! to 1. The following is an example of creating a 10x20x30 3D grid
! where the first dimensions is broken into 2 pieces, the second
! is broken into 4 pieces, and the third is undistributed. 
!EOE

!BOC
!  grid3D=ESMF_GridCreateShape(maxIndex=(/10,20,30/), &
!         regDecomp=(/2,4,1/), rc=rc)   
!EOC
   !-------------------------------------------------------------------
   ! Clean up to prepare for the next example.
   !-------------------------------------------------------------------
!  call ESMF_GridDestroy(grid3D, rc=rc)

!BOE
! Irregular distribution requires the user to specify the
! exact number of cells per DE in each dimension.  For the
! {\tt ESMF\_GridCreateShape} call the {\tt countsPerDEDim1,2,3}
! arguments are used to specify a rectangular distribution
! containing size(countsPerDEDim1) by size(countsPerDEDim2) by
! size(countsPerDEDim3) DEs. The entries in each of these arrays
! specify the size of the DEs in that dimension for that row or column.
! The rank of the grid is determined by the presence of
! {\tt countsPerDEDim3}.  If it's present the Grid
! will be 3D, if just {\tt countsPerDEDim1} and 
! {\tt countsPerDEDim2} are specified the Grid 
! will be 2D.  If any of these arrays has size
! 1 then that index dimension is undistributed.
!
! The following call illustrates the creation of 
! a 10x20 two dimensional rectangular Grid distributed across six processors
! in two groups of each size 5 in the first dimension, and 
! three groups of sizes 7,7,6 in the second dimension.
!EOE

!BOC
   grid2D=ESMF_GridCreateShape(countsPerDEDim1=(/5,5/), &
          countsPerDEDim2=(/7,7,6/), rc=rc)   
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
   grid3d=ESMF_GridCreateShape(countsPerDEDim1=(/5,5/), &
          countsPerDEDim2=(/7,7,6/), countsPerDEDim3=(/15,15/), rc=rc)   
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!!!!!!!!!!!!!!!!!!!!!!!
! Cleanup after Example
!!!!!!!!!!!!!!!!!!!!!!
call ESMF_GridDestroy(grid3D,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!BOE 
! Alternatively, if the third dimension were to be undistributed, then 
! {\tt countsPerDEDim3} in the call would have only a single term.
!EOE

!BOC
   grid3D=ESMF_GridCreateShape(countsPerDEDim1=(/5,5/), &
          countsPerDEDim2=(/7,7,6/), countsPerDEDim3=(/30/), rc=rc)   
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!!!!!!!!!!!!!!!!!!!!!!!
! Cleanup after Example
!!!!!!!!!!!!!!!!!!!!!!
call ESMF_GridDestroy(grid3D,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!BOE
!
! For the regular and irregular types of distributions, the 
! {\tt petMap} parameter may be used to specify which specific PETs 
! the DEs in the Grid are assigned to. The {\tt petMap} 
! array is the same size in each dimension as the number of DEs in
! that dimension.
!
! The next example demonstrates how to specify the PET to DE association 
! for an ESMF\_GridCreateShape call.
!EOE

! Skip if not right number of procs.
if (petcount .eq. 4) then 
!BOC
   ! allocate memory for petMap
   allocate( petMap(2,2,1) )
   ! Set petMap
   petMap(:,1,1) = (/3,2/)
   petMap(:,2,1) = (/1,0/)


   ! Let the 3D grid be be distributed only in the first two dimensions.
   grid2D=ESMF_GridCreateShape(countsPerDEDim1=(/5,5/), &
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
! arguments {\tt minIndex} and {\tt maxIndex}, and through {\tt localIndices},
! specify the set of index space locations residing on the local processor.
! Again, if {\tt minIndex} is  not specified, then the bottom of the 
! index range is assumed to be (1,1,...). 
! The rank of the Grid is equal to the size of {\tt maxIndex}. 
!
! The following example of creates a 2D grid of dimensions 5x5, and places
! the diagonal elements (i.e. indices (i,i) where i goes from 1 to 5)
! on this processor. The remaining processors would individually declare
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
!  grid2D=ESMF_GridCreateShape(maxIndex=(/5,5/), &
!         localIndices=localIndices, rc=rc)   
!EOC

   !-------------------------------------------------------------------
   ! Clean up to prepare for the next example.
   !-------------------------------------------------------------------
!   call ESMF_GridDestroy(grid2D, rc=rc)
   deallocate( localIndices )

#ifdef LOCAL_NOT_IMP
!BOEI
!
!\subsubsection{Specifying Tile Edge Connections}
! \label{example:TileEdgeConn}
! The tile edge connection capability is not currently implemented, but 
! the design is as follows.
! The {\tt ESMF\_GridCreateShape} command has three arguments 
! {\tt connDim1}, {\tt connDim2}, and {\tt connDim3} which specify the
! tile connectivitay. Each of these consists of a two element array of 
! type {\tt ESMF\_GridConn}. The two elements specify the connectivity using 
! predefined tags. See section \ref{sec:opt:gridconn} for a list of valid
! values for {\tt ESMF\_GridConn}. The first entry specifies the low end of 
! the dimension, while the second specifies the high end.
!
! The following example constructs a curvilinear longitude-latitude spherical grid
! by specifying that the first dimension (longitude) connects at the ends 
! periodically, while the second dimension (latitude) has poles at each end of 
! the dimension. These poles are specified as type POLE to differentiate from
! other multiple pole grids such as northern cap Tripole grid.
!EOEI

!BOCI
   grid2D=ESMF_GridCreateShape(countsPerDEDim1=(/5,5/), &
                       countsPerDEDim2=(/7,7,6/), &
                       connDim1=(/ESMF_GRIDCONN_PERIODIC, ESMF_GRIDCONN_PERIODIC/), &
                       connDim2=(/ESMF_GRIDCONN_POLE, ESMF_GRIDCONN_POLE/), &
                       rc=rc)   
!EOCI

!!!!!!!!!!!!!!!!!!!!!!!
! Cleanup after Example
!!!!!!!!!!!!!!!!!!!!!!
call ESMF_GridDestroy(grid2D,rc=rc)

!BOEI
!
! If a pole connection is specified, the stagger location of the pole
! may be needed to precisely describe the connection. The three optional arguments
! {\tt poleStaggerLoc1}, {\tt poleStaggerLoc2}, and {\tt poleStaggerLoc3}
! allow the user set the stagger location of the pole. Again, the first argument 
! specifies the low end of the dimension, while the second the high. The following
! illustrates setting the pole location to the center stagger location.
!xEOE

!BOCI
   grid2D=ESMF_GridCreateShape(countsPerDEDim1=(/5,5/), &
                       countsPerDEDim2=(/7,7,6/), &
                       connDim1=(/ESMF_GRIDCONN_PERIODIC, ESMF_GRIDCONN_PERIODIC/), &
                       connDim2=(/ESMF_GRIDCONN_POLE, ESMF_GRIDCONN_POLE/), &
                       poleStaggerLoc2=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CENTER/), &
                       rc=rc)   
!EOCI

!!!!!!!!!!!!!!!!!!!!!!!
! Cleanup after Example
!!!!!!!!!!!!!!!!!!!!!!
call ESMF_GridDestroy(grid2D,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!BOEI
!
! If instead a bipole connection is specified, the position of the poles
! is specified by the three optional arguments {\tt bipolePos1}, {\tt bipolePos2},
! and {\tt bipolePos3}. Each of these arguments allow the user to set
! the position of one of the poles in a respective hemisphere. The position of the 
! second pole in that hemisphere is implied form the position of the first to be 
! a mirror of the first (i.e. halfway around the periodic dimension from the first pole).
! The following illustrates setting the pole position to 1.
!EOEI

!BOCI
   grid2D=ESMF_GridCreateShape(countsPerDEDim1=(/5,5/), &
           countsPerDEDim2=(/7,7,6/), &
           connDim1=(/ESMF_GRIDCONN_PERIODIC, ESMF_GRIDCONN_PERIODIC/), &
           connDim2=(/ESMF_GRIDCONN_BIPOLE, ESMF_GRIDCONN_BIPOLE/), &
           bipolePos2=(/1,1/), &
           rc=rc)   
!EOCI
#endif

!BOE
!
!\subsubsection{Specifying Coordinate Arrays and Their Relation to
! Index Space Dimensions}
!
! To specify how the coordinate arrays are mapped to the 
! index dimensions the arguments {\tt coordDep1}, 
! {\tt coordDep2}, and {\tt coordDep3} are used, each 
! of which is a Fortran array. The values of the elements
! in a {\tt coordDep} array specify which index dimension
! the corresponding coordinate dimension
! maps to.  For example, {\tt coordDep1=(/1,2/)} means that
! the first dimension of coordinate 1 maps to index
! dimension 1 and the second maps to index dimension 2. If
! the {\tt coordDep} arrays are not specified, 
! then {\tt coordDepX} defaults to {/X/}.  This default
! thus specifies a rectilinear grid.  
!
! It follows that the size of the {\tt coordDep}
! array is the rank of the corresponding coordinate array.
! For example, {\tt size(coordDep1)=2} means that coordinate
! array 1 is 2D. 

! Currently ESMF only supports coordinate dimensions 
! that are the same rank as the grid.  So, for example ....
! [explain limitation further]. 
!EOE

#ifdef LOCAL_NOT_IMP
!BOEI
! The following call demonstrates the creation of a
! 10x20 2D rectilinear grid where the first coordinate
! component is mapped to the second index dimension
! (i.e. is of size 20) and the second coordinate component
! is mapped to the first index dimension (i.e. is of size
! 10).
!EOEI

!BOCI
   grid2D=ESMF_GridCreateShape(countsPerDEDim1=(/5,5/), &
          countsPerDEDim2=(/7,7,6/),                    &
          coordDep1=(/2/),                              &
          coordDep2=(/1/), rc=rc)   
!EOCI
#endif

!BOE
! The following call demonstrates the creation of a
! 10x20 2D curvilinear grid where where both
! coordinate component arrays are 10x20 also, but
! the order of their dependency on the Grid dimensions
! is reversed. 
!EOE

!BOC
   grid2D=ESMF_GridCreateShape(countsPerDEDim1=(/5,5/), &
          countsPerDEDim2=(/7,7,6/), coordDep1=(/2,1/), &
          coordDep2=(/1,2/), rc=rc)   
!EOC 
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!!!!!!!!!!!!!!!!!!!!!!!
! Cleanup after Example
!!!!!!!!!!!!!!!!!!!!!!
call ESMF_GridDestroy(grid2D,rc=rc)

#ifdef LOCAL_NO_IMPL
!BOEI
! The following call demonstrates the creation of a
! 10x20x30 2D plus 1 curvilinear grid where 
! coordinate component 1 and 2 are still 10x20, but
! coordinate component 3 is mapped just to the 
! undistributed third index dimension.
!EOE

!BOCI
   grid2D=ESMF_GridCreateShape(countsPerDEDim1=(/5,5/), &
          countsPerDEDim2=(/7,7,6/), countsPerDEDim3=(/30/), &
          coordDep1=(/1,2/), coordDep2=(/1,2/), &
          coordDep3=(/3/), rc=rc)   
!EOCI 

!!!!!!!!!!!!!!!!!!!!!!!
! Cleanup after Example
!!!!!!!!!!!!!!!!!!!!!!
call ESMF_GridDestroy(grid2D,rc=rc)
#endif


!BOE
!
! By default the local piece of the array on each processor starts at 
! (1,1,..), however, the indexing for each grid coordinate array  
! on each DE may be shifted to the global indices by using the {\tt indexflag}.
! For example, the following call switches the grid to use global indices. 
!EOE

!BOC
   grid2D=ESMF_GridCreateShape(countsPerDEDim1=(/5,5/), &
           countsPerDEDim2=(/7,7,6/), indexflag=ESMF_INDEX_GLOBAL, rc=rc)   
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!!!!!!!!!!!!!!!!!!!!!!!
! Cleanup after Example
!!!!!!!!!!!!!!!!!!!!!!
call ESMF_GridDestroy(grid2D,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


#ifdef LOCAL_NOT_IMP
!BOEI
!
!\subsubsection{Specifying Coordinate Type and Kind}
!
! The default type and kind for Grid coordinates is ESMF\_R8. 
!  To control the data type and kind more precisely the optional 
!  {\tt coordTypeKind} parameter may be used. The following illustrates
! the creation of a Grid with 4 byte integer coordinates. 
!EOEI

!BOCI
   grid=ESMF_GridCreateShape(coordTypeKind=ESMF_TYPEKIND_I4, &
          countsPerDEDim1=(/5,5/), countsPerDEDim2=(/7,7,6/), rc=rc)   
!EOCI
#endif

!BOE
!\subsubsection{Associating Coordinates with Stagger Locations}
! \label{sec:usage:staggerloc}
!
! In this document, stagger location refers to the places in a Grid
! cell that can contain data. Typically data can is located
! at the cell center,  at the cell corner, or at the cell face, and so on into
! higher dimensions. After Grid creation the user can create a field at any of the stagger
! locations in a Grid. However, the user must specifically add coordinates
! to a stagger location if they want them. Coordinates in a Grid 
! may be neccesary for some ESMF functionality (e.g. regrid). The ESMF Grid class
! allows the user to put coordianates at multiple stagger locations per
! Grid.  When adding, or accessing
! coordinate data, the stagger location is specified to tell the Grid method 
! where in the cell to get the data. There are predefined stagger locations
! (see Section~\ref{sec:opt:staggerloc}), however, 
! should the user desire to specify their own, there
! is also a set of methods for generating custom locations. 
! (see Section~\ref{ref:stagger} for a more in depth
! description of stagger locations and stagger specification.)
!
! To indicate which stagger locations in a Grid have coordinate data, 
! the subroutine {\tt ESMF\_GridAllocCoord} is used. The following example
! adds coordinate storage to the corner stagger location in {\tt grid} using 
! one of the predefined stagger locations.
!EOE


!!!!!!!!!!!!!!!!!!!!!!
! Setup For Example
!!!!!!!!!!!!!!!!!!!!!!
   distgrid2D=ESMF_DistGridCreate(minIndex=(/1,1/),maxIndex=(/10,10/), rc=rc)
   grid2D=ESMF_GridCreate(distgrid=distgrid2D, rc=rc)


!BOC 
   call ESMF_GridAllocCoord(grid2D, staggerLoc=ESMF_STAGGERLOC_CORNER, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!!!!!!!!!!!!!!!!!!!!!!!
! Cleanup after Example
!!!!!!!!!!!!!!!!!!!!!!
   call ESMF_GridDestroy(grid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

   call ESMF_DistGridDestroy(distgrid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


#ifdef LOCAL_NOT_IMP
!BOEI
! The user may also set the coordinate data at the same time as adding
! the coordinate storage.  The following example
! adds coordinate storage to the corner stagger location in {\tt grid}, plus sets
! the associated coordinate data. Note, the input coordinate arrays (CoordX, CoordY)
! may be either F90 or ESMF Array's (The F90 arrays are restricted to single DE to
! PET mappings). They also need to be of the proper size and rank to coorespond to the Grid. 
!EOEI

!BOCI 
   call ESMF_GridSetCoord(grid, staggerLoc=ESMF_STAGGER_CORNER, &
          coord1=CoordX, coord2=CoordY, rc=rc)
!EOCI
#endif

!BOE
!\subsubsection{Accessing Grid Coordinates}
!
! Once a Grid has been created the user has several options to access
! the Grid coordinate data. The first pair of these allow the user
! to use the {\tt ESMF\_Array} class to set or get data 
! for one stagger location across the whole Grid. The
! {\tt ESMF\_GridSetCoordFromArray} allows the user to set coordinate
! data from an Array. For example, the following sets the 
! first component (e.g. x) coordinates for the center stagger location to 
! those in the array arrayCoordX.
!EOE

!!!!!!!!!!!!!!!!!!!!!!!
! Setup For Example
!!!!!!!!!!!!!!!!!!!!!!
   distgrid2D=ESMF_DistGridCreate(minIndex=(/1,1/),maxIndex=(/10,10/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
   grid2D=ESMF_GridCreate(distgrid=distgrid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
   call ESMF_ArraySpecSet(arrayspec2D,rank=2,typekind=ESMF_TYPEKIND_R8)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
   arrayCoordX=ESMF_ArrayCreate(arrayspec=arrayspec2D, distgrid=distgrid2D, &
              rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!BOC
   call ESMF_GridSetCoord(grid2D, &
          staggerLoc=ESMF_STAGGERLOC_CENTER, &
          coord=1, array=arrayCoordX, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!!!!!!!!!!!!!!!!!!!!!!!
! Cleanup after Example
!!!!!!!!!!!!!!!!!!!!!!
   call ESMF_GridDestroy(grid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
   call ESMF_ArrayDestroy(arrayCoordX, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
   call ESMF_DistGridDestroy(distgrid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!BOE
! The method {\tt ESMF\_GridGetCoord()} allows the user
! to access the Array, as a direct reference, which
! contains the coordinate data for a stagger location on a Grid. The user
! can then employ any of the standard {\tt ESMF\_Array} tools to operate
! on the data. The following copies the coordinates from the second 
! component of the corner and puts it into Array arrayCoordY. 
!EOE

!!!!!!!!!!!!!!!!!!!!!!!
! Setup For Example
!!!!!!!!!!!!!!!!!!!!!!
   distgrid2D=ESMF_DistGridCreate(minIndex=(/1,1/),maxIndex=(/10,10/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
   grid2D=ESMF_GridCreate(distgrid=distgrid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
   call ESMF_GridAllocCoord(grid2D,&
          staggerLoc=ESMF_STAGGERLOC_CORNER, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!BOC
   call ESMF_GridGetCoord(grid2D,               &
          staggerLoc=ESMF_STAGGERLOC_CORNER,    &
          coord=2,                              &
          array=arrayCoordY, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!!!!!!!!!!!!!!!!!!!!!!!
! Cleanup after Example
!!!!!!!!!!!!!!!!!!!!!!
   call ESMF_GridDestroy(grid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
   call ESMF_DistGridDestroy(distgrid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)



#ifdef LOCAL_NOT_IMP
!BOEI
! The second pair of methods enable the user to set or get data using
! a fortran pointer. These methods only work with the local piece of the 
! Grid on the DE. {\tt ESMF\_GridLocalTileSetCoord} enables the user
! to set data into the local piece of the coordinates residing on the DE.
! The following call gets a pointer (fptr) to the fortran array holding the 
! first component (e.g. x) coordinates for the corner stagger
! for the piece of tile 2 which is on this processor. It
! defaults to the first DE because it isn't specified. 
!EOEI

!BOCI
   call ESMF_GridSetLocalTileCoord(grid3D, tile=2,  &
          staggerLoc=ESMF_STAGGERLOC_CORNER,        &
          coord=1, fptr, doCopy=ESMF_DATA_REF, rc=rc)
!EOCI
#endif

!BOE
! Alternatively, the call {\tt ESMF\_GridLocalTileGetCoord()} gets a fortran pointer to 
! the coordinate data. The user can then operate on this array in the usual
! manner. The following call gets a reference to the
! fortran array which holds the data for the second coordinate (e.g. y). 
!EOE

!!!!!!!!!!!!!!!!!!!!!!!
! Setup For Example
!!!!!!!!!!!!!!!!!!!!!!
   distgrid2D=ESMF_DistGridCreate(minIndex=(/1,1/),maxIndex=(/10,10/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
   grid2D=ESMF_GridCreate(distgrid=distgrid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
   call ESMF_GridAllocCoord(grid2D,&
          staggerLoc=ESMF_STAGGERLOC_CORNER, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!BOC
   call ESMF_GridGetLocalTileCoord(grid2D, coord=2, &
          staggerloc=ESMF_STAGGERLOC_CORNER, fptr=coordY, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!!!!!!!!!!!!!!!!!!!!!!!
! Cleanup after Example
!!!!!!!!!!!!!!!!!!!!!!
   call ESMF_GridDestroy(grid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_DistGridDestroy(distgrid2D, rc=rc)



#ifdef LOCAL_NOT_IMP
!BOEI
! The last pair of methods allow the user to access coordinate data a point at
! a time given the index space coordinates. The method 
! {\tt ESMF\_GridSetLocalTileCoord}, allows for a uniform method of setting
! coordinates in the Grid no matter what the factorization of the 
! arrays. The following sets the coordinates at the index point (1,1,1) 
! in the center stagger location to (.5,.5,.5).
!EOEI

!BOCI
   call ESMF_GridSetLocalTileCoord(grid, &
          staggerLoc=ESMF_STAGGERLOC_CENTER, 
          indices=(/1,1,1/), coords=(/.5,.5,.5/), rc=rc)
!EOCI

!BOEI
! The method {\tt ESMF\_GridGetLocalTileCoord}, allows for a uniform 
! method of retrieving coordinates from the Grid no matter what the factorization of the 
! arrays. The following retrieves the coordinates (outCoords) from 
! point (1,1,1) in the center stagger location.  
!EOEI

!BOCI
   call ESMF_GridGetLocalTileCoord(grid, &
          staggerLoc=ESMF_STAGGERLOC_CENTER, 
          indices=(/1,1,1/), coords=outCoords, rc=rc)
!EOCI
#endif

#ifdef LOCAL_NOT_IMP
!BOEI
!\subsubsection{Generating Grid Coordinates}
!
! The current ESMF grid interface provides some automatic production
! of coordinate values. In the future there are plans to add more, but 
! for now the user may generate coordinates uniformly distributed across
! an index space. To do this the user specifies the coordinate values
! which coorespond to the minimum indices {\tt begCoord} and the 
! maximum indices {\tt endCoord}. The method then calculates the intermediate
! coordinates and loads them into the appropriate places in the Grid
! coordinate arrays. The following fills a 3D Grid with coordinates from 
! (11.0,10.0,100.0) to (100.0,100.0,0.0)
!
!EOEI

!BOCI
   call ESMF_GridGenCoordsUni(grid, begCoord=(/11.0,10.0,100.0/), &
          endCoord=(/100.0,100.0,0.0/), rc=rc)
!EOCI
#endif

#ifdef LOCAL_NOT_IMP
!BOEI
!\subsubsection{Calculating Grid Coordinates}
!
! In addition to the grid generate option specified above to set coordinates, 
! ESMF also provides a method to calculate coordinates based on those
! already in given a stagger location. This method current uses averaging to 
! calculate the coordianates, but in the future other methods may be added. 
! If the user doesn't specify a list of destination staggers locations, this 
! method fills in all the coordinate arrays currently allocated in the grid. 
! The following call sets the whole grid's coordinates based on the corner stagger
! location's coordinates. 
!EOEI

!BOCI
   call ESMF_GridCalcStaggerLocCoord(grid, srcStaggerLoc=ESMF_STAGGERLOC_CORNER, rc=rc)
!EOCI
#endif

#ifdef LOCAL_NOT_IMP
!BOEI
!\subsubsection{Grid Halo}
!
! The Grid halo operation allows users to update the
! halos (if present) of the Grid coordinate arrays to contain the coordinate values on the 
! neighboring processor. This would be convenient in cases like regrid
! in which having coordinate information for cells neighboring the processor
! boundary is useful. The maximum width of the halo region around each
! localTile (DE local piece of a Grid Tile) can be set with the 
! {\tt computationalLWidth} and {\tt computationalUWidth} options during 
! grid creation. The method {\tt ESMF\_GridHalo} is called to 
! perform this operation. The following call fills the computational region of the grid 
! with the coordinate values from neighboring DEs.
!EOEI

!BOCI
   call ESMF_GridHalo(grid, rc=rc)
!EOCI
#endif

#ifdef LOCAL_NOT_IMP
!BOEI
!\subsubsection{Metric Creation}
!
! There are several options for adding metric data to 
! a Grid. The first of these allows the user to 
! allocate storage for the metric in the Grid without setting 
! values. The user can then use the metric data access routines
! to set the desired values. 
! The following call adds metric "Area" to the 
! grid at the center stagger location. The metric is 
! a 4 byte real.  
!removeEOE

!removeBOC
   call ESMF_GridAddMetricNoSet(grid, name="Area", &
          metricTypeKind=ESMF_TYPEKIND_R4, &
          staggerLoc=ESMF_STAGGERLOC_CENTER, rc)
!removeEOC

!removeBOE
! The next option allows the user to add an array
! as a metric.  The following call adds metric "Length" to the 
! grid at the edge stagger location.  
!removeEOE

!removeBOC
   call ESMF_GridAddMetricFromArray(grid, "Length", &
          staggerLoc=ESMF_STAGGERLOC_EDGE1, &
          array=length, rc=rc)
!removeEOC

!removeBOE
! The final option allows the user to add a metric constructed
! out of a series of fortran arrays. Note that this option is restricted
! to situations where there is a 1-to-1 DE to PET mapping. 
! The following call adds metric "Length" to the 
! grid at the edge stagger location. It does this from 
! fortran array length.   
!removeEOE

!removeBOC
   call ESMF_GridAddMetricFromFptr(grid, "Length", &
          staggerLoc=ESMF_STAGGERLOC_EDGE1, &
          fptr=length, rc=rc)
!removeEOC
#endif

#ifdef LOCAL_NOT_IMP
!removeBOE
!\subsubsection{Metric Data Access}
!
! Once a Grid has been created the user has several options to access metric
! data. The first pair of these allow the user
! to use the {\tt ESMF\_Array} class to set or get metric data 
! across the whole Grid. {\tt ESMF\_GridSetMetricFromArray} allows the user to set metric
! data from an Array. For example, the following sets the 
! metric "CellArea" from the array Area.
!removeEOE


!removeBOC
   call ESMF_GridSetMetricFromArray(grid, name="CellArea", &
          array=Area, rc=rc)
!removeEOC


!removeBOE
! {\tt ESMF\_GridGetMetricIntoArray}, allows the user
! to get the Array (a direct reference or a copy) which
! contains the metric data on a Grid. The user
! can then employ any of the standard {\tt ESMF\_Array} tools to operate
! on the data. The following copies the area from the grid
! and puts it into Array copyOfArea. 
!removeEOE

!removeBOC
   call ESMF_GridGetMetricIntoArray(grid, &
          name="Area", array=copyOfY, docopy=ESMF_DATA_COPY, rc=rc)
!removeEOC

!removeBOE
! The second pair of methods enable the user to set or get metric data using
! a fortran pointer. These methods only work with the local piece of the 
! Grid on the DE. {\tt ESMF\_GridLocalTileSetMetric} enables the user
! to set data into the local piece of the metric residing on the DE.
! The following call gets a pointer (fptr) to the fortran array holding the 
! area for the piece of tile 2 which is on this processor. It
! defaults to the first DE because it isn't specified. 
!removeEOE

!removeBOC
   call ESMF_GridLocalTileSetMetric(grid, name="Area", tile=2, &
          fptr, doCopy=ESMF_DATA_REF, rc=rc)
!removeEOC

!removeBOE
! The call {\tt ESMF\_GridLocalTileGetMetric} gets a fortran pointer to 
! the metric data. The user can then operate on this array in the usual
! manner. The following call allocates an array (fptr) and
! makes copy of the part of tile 1's area which
! lies on the second DE. 
!removeEOE

!removeBOC
   call ESMF_GridLocalTileGetMetric(grid, name="Area", tile=1, localDE=2, &
          fptr, doCopy=ESMF_DO_COPY, rc=rc)

!removeEOC
#endif

#ifdef LOCAL_NOT_IMP
!removeBOE
!\subsubsection{Grid Attributes}
!
! As is typical for ESMF classes, the Grid class allows
! the user to attach name-value pairs to a Grid object. 
! The following adds the attribute "Size" with the value 10. 
!removeEOE

!removeBOC
   call ESMF_GridSetAttribute(grid, "Size", 10, rc=rc)
!removeEOC
#endif

#ifdef LOCAL_NOT_IMP
!BOEI
! \subsubsection{Creating a Regularly Distributed 3D Grid with Generated Coordinates}
!
! This example illustrates the creation of a 100x100x100  3D Grid distributed across
! 5 processors in each dimension. The coordinates in the Grid are uniformly distributed
! between (0.0, 0.0, 0.0) and (200.0, 200.0, 200.0).
!
!EOEI

!BOCI
   ! Use ESMF framework module
   use ESMF_Mod
   implicit none

   ! Local variables  
   integer:: rc, finalrc
   type(ESMF_Grid) :: grid
!EOCI         

      finalrc = ESMF_SUCCESS
      call ESMF_Initialize(vm=vm, rc=rc)
      call ESMF_VMGet(vm, localPet=myPet, petCount=npets, rc=rc)

!BOCI
   ! Create the Grid.
   grid=ESMF_GridCreateShape(maxIndex=(/100,100,100/), regDecomp=(/5,5,5/), rc=rc)   

   ! Add a center stagger location 
   call ESMF_GridSetCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)

   ! Put in the coordinates
   call ESMF_GridGenCoordUni(grid, begCoord=(/0.0,0.0,0.0/), &
          endCoord=(/200.0, 200.0, 200.0/), rc=rc)
 
!EOCI
#endif

#ifdef LOCAL_NOT_IMP
!removeBOE
! \subsubsection{Creating a Grid from Existing F90 Arrays}~\label{sec:example5}
!
! This example illustrates the creation of a simple 2D Grid from coordinate data
!  contained in 4 byte real fortan arrays.  The new Grid contains just the center stagger location.
!  Each processor contains a pair of 10x10 fortran 90 arrays named fptrX and fptrY. 
!  These arrays contain the coordinates for the piece of the global Grid held by each
!  processor. The final global Grid will be 20x20 and the pieces of this Grid held
! by each processor are as follows:
!
! \begin{verbatim}
!
!  
!       20  +--------------+--------------+
!           |              |              |                       
!           |              |              |                       
!           |     PET3     |     PET4     |                       
!           |              |              |                       
!           |              |              |  
!       10  +--------------+--------------+ 
!           |              |              |                       
!           |              |              |                       
!           |     PET 1    |     PET2     |                       
!           |              |              |  
!           |              |              |                      
!        1  +--------------+--------------+
!           1             10             20  
!
!
! \end{verbatim}
!
!   As illustrated by the diagram, the arrays on processor 1 hold piece (1,1)-(10,10) of the 
!   global index space. The arrays on processor 2 hold piece (11,1)-(20,10). The arrays on 
!   processor 3 hold piece (1,11)-(10,20), and the arrays on processor 4 hold piece (11,11)-(20,20).
!
!removeEOE

!removeBOC
   ! Use ESMF framework module
   use ESMF_Mod
   implicit none

   ! Local variables  
   integer:: rc, finalrc
   integer:: myPet, npets, rootPet
   type(ESMF_VM):: vm
   type(ESMF_Grid) :: grid
   real(ESMF_KIND_R4), pointer :: fptrX(:,:),fptrY(:,:)
   integer :: petMap(2,2,1)
!removeEOC         

      finalrc = ESMF_SUCCESS
      call ESMF_Initialize(vm=vm, rc=rc)
      call ESMF_VMGet(vm, localPet=myPet, petCount=npets, rc=rc)

!removeBOC
   ! Create a grid with the correct size, shape, and distribution to
   ! hold the data. 

   ! Set petMap
   petMap(:,2,1) = (/3,4/)
   petMap(:,1,1) = (/1,2/)

   ! Create Grid
   grid=ESMF_GridCreateShape(coordTypeKind=ESMF_TYPEKIND_R4, &
                           countsPerDEDim1=(/10,10/), &
                           countsPerDEDim2=(/10,10/), &
                           coordDep1=(/1,2/), &
                           coordDep2=(/1,2/), &
                           petMap=petMap, &
                           rc=rc)   


   ! Add a center stagger location and at the same time set {\tt grid} to 
   ! reference the coordinate arrays. 
   call ESMF_GridSetCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                  coord1=fptrX, coord2=fptrY, rc=rc)
 
!removeEOC


!removeBOE
! \subsubsection{Example: Grid Creation from Existing F90 Arrays Using CreateEmpty/Set}
!
!  This example illustrates the use of the CreateEmpty/Set paradigm.
!  It repeats the above example using this grid creation technique.
!removeEOE

!removeBOC
   ! Use ESMF framework module
   use ESMF_Mod
   implicit none

   ! Local variables  
   integer:: rc, finalrc
   integer:: myPet, npets, rootPet
   type(ESMF_VM):: vm
   type(ESMF_Grid) :: grid
   real(ESMF_KIND_R4), pointer :: fptrX(:,:),fptrY(:,:)
   integer :: petMap(2,2,1)
!removeEOC         

      finalrc = ESMF_SUCCESS
      call ESMF_Initialize(vm=vm, rc=rc)
      call ESMF_VMGet(vm, localPet=myPet, petCount=npets, rc=rc)

!removeBOC
   ! Create an empty grid
   grid=ESMF_GridCreateEmpty(rc=rc)


   ! Set the Grid type and kind
   grid=ESMF_GridSetShape(coordTypeKind=ESMF_TYPEKIND_R4, rc=rc)


   ! Set the grid size and distribution
   ! Init petMap
   petMap(:,2,1) = (/3,4/)
   petMap(:,1,1) = (/1,2/)

   ! Set Grid 
   grid=ESMF_GridSetShape(countsPerDEDim1=(/10,10/), &
          countsPerDEDim2=(/10,10/), petMap=petMap, rc=rc)   

   ! Set the grid coordinate array index dependency
   grid=ESMF_GridSetShape(coordDep1=(/1,2/), coordDep2=(/1,2/), &
          rc=rc)   

  ! Create the grid and add a center stagger location and at the same time set {\tt grid} to 
  ! reference the coordinate arrays. 
  call ESMF_GridSetCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                  coord1=fptrX, coord2=fptrY, rc=rc)

!removeEOC
#endif


!BOE
!\subsubsection{Using DistGrid to Create More Complex Grids}
!
! Beyond the shortcut methods for creating a grid, there is
! a set of methods which give the user more control over the
! specifics of the grid.  The following describes the more 
! general interface. 
!
!
!\subsubsection{Creation: Advanced: Size, Rank, Shape, and Distribution}
!\label{sec:usage:adv:create}
!BOPI
!% NEED TO ADD MORE HERE EXPLAINING HOW THE GENERAL DIST DIFFER FROM THE 
!% SHORTCUT AND EXPLAINING MORE ABOUT USING THE DIST GRID
!EOPI
! There are four methods of specifying the distribution in the more
! general grid creation interface. Three of them are basically the same as
! those used in the shortcut create 
! and the user is directed to Section~\ref{sec:usage:short:create} for further explanation
! of those. The fourth method is to first create an ESMF DistGrid object describing
! the distribution and shape of the Grid, and then to employ that to either directly
! create the Grid or first create Arrays and then create the Grid from those. 
! This method gives the user maximum control over the topology and distribution of the Grid. 
! Please see the DistGrid design document for an in depth description of its use. 
! Also, Example~\ref{sec:usage:ex:adv:cart} and Example~\ref{sec:usage:ex:adv:tripole}
! illustrate its use in creating two types of Grid. 
!
! A DistGrid describes only the distributed dimensions of the index
! space, so when creating a Grid from a DistGrid some arguments
! are need to describe the undistributed part, To add undistributed dimensions
! to the Grid, the arguments {\tt lbounds} and {\tt ubounds} may be used. 
! The {\tt lbounds} argument
! contains the lower bounds of the undistributed dimensions and {\tt ubounds}
! contains the upper bounds. As an example, the following call constructs
! a 10x20x30 Grid with a lower bound of (1,2,3), with the third dimension
! undistributed.
!EOE

!BOC 
   ! Create DistGrid
   distgrid2D = ESMF_DistGridCreate(minIndex=(/1,2/), maxIndex=(/11,22/), rc=rc)  

   ! Create Grid
   grid3D=ESMF_GridCreate(distGrid=distgrid2D,         &  ! explicitly specify the distGrid
                       lbounds=(/3/), ubounds=(/33/),  &  ! specify the undistributed dimension
                       rc=rc)
!EOC  

!!!!!!!!!!!!!!!!!!!!!!!
! Cleanup after Example
!!!!!!!!!!!!!!!!!!!!!!!
   call ESMF_GridDestroy(grid3D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
   call ESMF_DistGridDestroy(distgrid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!BOE
! To alter which dimensions are distributed, the {\tt dimmap} 
! argument can be used. The {\tt dimmap} is used to set
! which dimensions of the Grid are mapped to the dimensions
! described by {\tt maxIndex}. In other words, it describes how the dimensions of 
! the underlying default DistGrid are mapped to the Grid. Each entry
! in {\tt dimmap} contains the Grid dimension to which the cooresponding
! DistGrid dimension should be mapped. 
! The following example illustrates the creation of a Grid where the undistributed
! dimension is first. To accomplish this the two distributed dimensions are mapped
! to the last two Grid dimensions (i.e. 2 and 3). 
!EOE

!BOC 
   ! Create DistGrid
   distgrid2D = ESMF_DistGridCreate(minIndex=(/1,2/), maxIndex=(/11,22/), rc=rc)  

   ! Create Grid
   grid3D=ESMF_GridCreate(distGrid=distgrid2D, lbounds=(/3/), ubounds=(/33/), & 
          dimmap=(/2,3/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!!!!!!!!!!!!!!!!!!!!!!!
! Cleanup after Example
!!!!!!!!!!!!!!!!!!!!!!!
   call ESMF_GridDestroy(grid3D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
   call ESMF_DistGridDestroy(distgrid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


#ifdef LOCAL_NOT_IMP
!BOEI
!\subsubsection{Creation: Advanced: Coordinate Specification and Index Space Dependency}~\label{sec:usage:coordstore}
!
! Depending on the user's coordinate data there are many possible
! arrangements for the coordinates to be stored in memory. 
! The main option is how the coordinates are split 
! up (factored) into arrays. Obviously, if all coordinate components (e.g. x,y,z) vary over
! the entire Grid, then the coordinate arrays need to be the same dimension as the Grid.
! However, if a coordinate component stays the same over all values of an index, such 
! as rectilinear Grid, then the coordinate array can be broken into seperate arrays saving
! memory and more closely matching the structure of the coordinate data. 
!
! The Grid accepts coordinates as an array of ESMF Arrays. 
! Each Array holds one coordinate component. For example, 
! for a 3D Grid, the coordinates would be stored in an array of
! size 3. The first entry in the array would be the x coordinate
! values, the second would be the y coordinate values, and the
! third the z coordinate values. Coordinate factorization is handled 
! by the fact that the component arrays don't need to be the same dimension as
! each other or as the Grid. For example, for the 3D case, all 3 ESMF
! Arrays could be 1D, or x and y could be 2D while z is 1D.
! The parameters {\tt coordRank} and {\tt coordDimMap}
! can be used to specify the factorization of the coordinate arrays. 
!
! The default Grid has coordinate arrays the same rank as the Grid.
! To alter this, the {\tt coordRank} parameter can be used
! to set the rank of the coordinate arrays.  The size of the parameter  
! is the rank of the Grid. Each entry of {\tt coordRank}  is the rank of 
! the associated coordinate array. The following creates a 10x20
! 2D Grid where both the x and y coordinates are stored in 1D arrays. 
!EOEI

!BOCI 
   Grid2D=ESMF_GridCreate(maxIndex=(/10,20/), &
            coordRank=(/1,1/) , rc=rc)
!EOCI  

!BOEI
! The default Grid has the dimensions of the coordinate arrays
! mapped in order to the Grid dimensions (e.g. dimension 1 of the coordinate
! arrays corresponds to dimension 1 of the Grid.). If the coordinate arrays
! have a smaller rank than the Grid, then the default depends on the 
! rank of the grid and the rank of the individual coordinate Arrays. For a 2D Grid,
! if each Array has rank 1, each 1D  Array is mapped to the Grid dimension
! corresponding to its component number (e.g. component Array 1 is mapped
! to grid dimension 1, and so on). For a 3D Grid with 1D Arrays, the same
! is true. For a 3D grid with one 1D and two 2D arrays, the 2D arrays are 
! mapped to align with their corresponding components numbers and each other, 
! and the 1D array is mapped to the other dimension. For all other cases, 
! the component arrays are just mapped in order starting from the first Grid dimension. 
!
! To alter the default mapping, the {\tt coordDimMap} argument can be used. 
! The parameter is a 2D array where each dimension is the rank of the Grid.
! The first dimension indicates the component (e.g. x=1). The second dimension
! is an entry for each dimension of the coordinate array.  Each entry of 
! {\tt coordDimMap}  tells which Grid dimension the corresponding
! dimension maps to.  The following creates a 2D Grid where
! the single dimension of the x coordinate array is mapped to the second
! Grid dimension (i.e. 20) and the single dimension of the y coordinate array is
! mapped to the first Grid dimension (i.e. 10). 
!EOEI

!BOCI 
   coordDimMap(1,1)=2      ! Map X (i.e. 1) to second Grid dimension 
   coordDimMap(2,1)=1      ! Map Y (i.e. 2) to first Grid dimension 
   Grid2D=ESMF_GridCreate(maxIndex=(/10,20/), coordRank=(/1,1/) , &
               coordDimMap=coordDimMap, rc=rc)
!EOCI  
#endif

#ifdef LOCAL_NOT_IMP
!removeBOE
!\subsubsection{Creation: Advanced: Miscellaneous}
!
! There are also a couple of other grid arguments which don't
! fit into the above categories. One of these
! is {\tt gridType}. This parameter is used to indicate
! the shape of the grid to other ESMF routines, so 
! they can behave appropriately. It also
! indicates which metadata values are available 
! in the grid. The only setting currently available
! for {\tt gridType} is ESMF\_GRIDTYPE\_UNKNOWN.
!
! Another argument is {\tt noData}, this 
! parameter allows the user to create a grid without
! allocating the internal data storage. The user
! could employ this option if they wanted to set their own
! coordinate arrays in the grid after the create. 
! This option is currently unimplemented. 
!removeEOE
#endif


!BOE
!\subsubsection{Specifying Custom Stagger Locations}
!\label{sec:usage:staggerloc:adv}
!
! This section discusses the advanced options for adding data 
! (coordinates or metrics) to different stagger locations in a grid.
! The first of these supports the construction of custom stagger locations.
! To construct a custom stagger location the subroutine 
! {\it ESMF\_StaggerLocSet()} is used to specify the staggers general location,
! by specifying for each dimension whether it is located at the interior (0) 
! or on the boundary (1) of the cell. Further description of the method is
! available in Section~\ref{ref:stagger}. This method allows users
! to construct stagger locations for which
! there is no predefined value. In this example, it's used to 
! set the 4D center and 4D corner locations. 
!EOE

!!!!!!!!!!!!!!!!!!!!!!!
! Setup For Example
!!!!!!!!!!!!!!!!!!!!!!
   distgrid4D=ESMF_DistGridCreate(minIndex=(/1,1,1,1/), &
                                  maxIndex=(/10,10,10,10/), rc=rc)
   grid4D=ESMF_GridCreate(distgrid=distgrid4D, rc=rc)

!BOC 

   ! Set Center
   call ESMF_StaggerLocSet(staggerLoc,loc=(/0,0,0,0/),rc=rc)
   call ESMF_GridAllocCoord(grid4D, staggerLoc=staggerLoc, rc=rc)

   ! Set Corner
   call ESMF_StaggerLocSet(staggerLoc,loc=(/1,1,1,1/),rc=rc)
   call ESMF_GridAllocCoord(grid4D, staggerLoc=staggerLoc, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  
!!!!!!!!!!!!!!!!!!!!!!!
! Cleanup after Example
!!!!!!!!!!!!!!!!!!!!!!!
   call ESMF_GridDestroy(grid4D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
   call ESMF_DistGridDestroy(distgrid4D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!BOE
!
!There is an added complication with the data (e.g. coordinates) stored at stagger locations in 
!that they can require different amounts of storage depending
!on the underlying Grid type. 
!
!\begin{verbatim}
!
!
!  *-------*-------*-------*
!  |       |       |       |
!  |   +   |   +   |   +   |
!  |       |       |       |
!  *-------*-------*-------*
!  |       |       |       |
!  |   +   |   +   |   +   |
!  |       |       |       |
!  *-------*-------*-------*
!
!        Example Grid
!
!
!\end{verbatim}
!
! Consider the preceeding 2D grid, where the ``*'' represents the cell corners
! and the ``+'' represents the cell centers. For the corners to completely
! enclose the cell centers (symmetric stagger), the number of corners in each 
! dimension, needs to be one greater then the number of cell centers. In the above 
! figure, there are two rows and three columns of cell centers. To enclose the 
! cell centers, there must be three rows and four columns of cell corners.
! This is true in general for Grids without periodicity or
! other connections.  In fact for a symmetric stagger, given that the center
! location requires n x m storage, the corresponding corner location
! requires n+1 x m+1, and the edges, depending on the side, require n+1 x m or
! m+1 x n.  In order to add the extra storage, but also to 
! allow the the different stagger location arrays to remain on the same DistGrid,
! the capability of the ESMF Array class to have extra computational
! padding is used. By default, when the coordinate arrays are created, one extra
! layer of padding is added to the arrays to create symmetric staggers 
! (i.e. the center location is surrounded). The default is to add this padding 
! on the positive side, and to only add this padding where needed 
! (e.g. no padding for the center, padding
! on both dimensions for the corner, in only one dimension for the 
! edge in 2D.)  To change these defaults the {\tt coordWidth} arguments 
! can be used to adjust the width and placement of the padding for each
! stagger location. 
!
!EOE

!BOE
! The {\tt coordLWidth} and 
! {\tt coordUWidth} arguments are both 1D arrays of the
! same size as the Grid rank. The entries in the arrays
! give the extra offset from the outer boundary of
! the tile exclusive region for
! each stagger location. The following example shows the
! addition of two stagger locations. The
! corner location has no extra boundary and the 
! center has a single layer of extra padding on 
! the negative side and none on the positive.  This is the reverse of
! the default behavior.
!EOE

!!!!!!!!!!!!!!!!!!!!!!!
! Setup For Example
!!!!!!!!!!!!!!!!!!!!!!
   distgrid2D=ESMF_DistGridCreate(minIndex=(/1,1/), &
                                  maxIndex=(/10,10/), rc=rc)
   grid2D=ESMF_GridCreate(distgrid=distgrid2D, rc=rc)


!BOC 
   call ESMF_GridAllocCoord(grid2D, &
          staggerLoc=ESMF_STAGGERLOC_CORNER, &
          coordLWidth=(/0,0/), coordUWidth=(/0,0/), rc=rc)

   call ESMF_GridAllocCoord(grid2D, &
          staggerLoc=ESMF_STAGGERLOC_CENTER, &
          coordLWidth=(/1,1/), coordUWidth=(/0,0/), rc=rc)

!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!!!!!!!!!!!!!!!!!!!!!!!
! Cleanup after Example
!!!!!!!!!!!!!!!!!!!!!!!
   call ESMF_GridDestroy(grid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
   call ESMF_DistGridDestroy(distgrid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!BOE
! To indicate how the data at a particular stagger location is aligned with the 
! cell center, the optional {\tt coordAlign} parameter 
! may be used. This parameter indicates which stagger elements 
! in a cell shares the same index values as the cell center. 
! For example, in a 2D cell, it would indicate which of the four corners has
! the same index value as the center. To set {\tt coordAlign},  
! the values -1,+1 are used to indicate the alignment in
! each dimension. If a stagger location is 
! centered in a dimension (e.g. an edge in 2D), then that
! dimension is ignored in the alignment. This parameter is mostly 
! informational, however, if the {\tt coordWidth} parameters 
! are not set then its value determines where the default padding
! is placed. If not specified, then the default is to align all 
! staggers to the most negative, so the padding is on the positive side. 
! The following code illustrates aligning the positive (North East) 
! corner with the center. 
!EOE


!!!!!!!!!!!!!!!!!!!!!!!
! Setup For Example
!!!!!!!!!!!!!!!!!!!!!!
   distgrid2D=ESMF_DistGridCreate(minIndex=(/1,1/), &
                                  maxIndex=(/10,10/), rc=rc)
   grid2D=ESMF_GridCreate(distgrid=distgrid2D, rc=rc)

!BOC 
   call ESMF_GridAllocCoord(grid2D, &
          staggerLoc=ESMF_STAGGERLOC_CORNER, coordAlign=(/1,1/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!!!!!!!!!!!!!!!!!!!!!!!
! Cleanup after Example
!!!!!!!!!!!!!!!!!!!!!!!
   call ESMF_GridDestroy(grid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
   call ESMF_DistGridDestroy(distgrid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


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
