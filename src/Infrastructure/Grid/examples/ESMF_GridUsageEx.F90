! $Id: ESMF_GridUsageEx.F90,v 1.1 2007/06/25 18:28:09 cdeluca Exp $
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

program ESMF_GridCreateEx

!==============================================================================
!EX_NOTWORKING_AMPLE        String used by test script to count examples.
!==============================================================================


!  !PROGRAM: ESMF_GridCreateEx - Examples of Grid creation.
!
!  !DESCRIPTION: 
!
! This program contains a set of Grid creation examples

      ! Use ESMF framework module
      use ESMF_Mod
      implicit none

      ! Local variables  
      integer:: rc, finalrc
      type(ESMF_VM):: vm
      type(ESMF_ArraySpec) ::  arrayspec2D

      ! initialize ESMF
      finalrc = ESMF_SUCCESS
      call ESMF_Initialize(vm=vm, rc=rc)

!BOE
!\subsubsection{Creating a 2D Irregularly Distributed Grid
!                  With Uniformly Spaced Coordinates}
! \label{example:2DIrregUniGrid}
!
! The following is a simple example of creating a grid and
! loading in a set of coordinates.  This code creates a 10x20
! 2D grid with uniformly spaced coordinates varying from (10,10) to (100,200).
! The grid is partitioned using irregular distribution. In the first dimension
! it is divided into two pieces, the first with 3 grid cells per
! DE and the second with 7 grid cells per DE. In the second dimension
! the Grid is divided into 3 pieces, with 5, 9, and 6 cells per DE.
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
   grid2D=ESMF_GridCreateShape(countsPerDEDim1=(/3,7/),     &
            countsPerDEDim2=(/4,7,6/), indexflag=ESMF_GLOBAL, &
            rc=rc)

   !-------------------------------------------------------------------
   ! Allocate coordinate storage and associate it with the center
   ! stagger location.  Since no coordinate values are specified in
   ! this call no coordinate values are set yet.
   !-------------------------------------------------------------------

   call ESMF_GridSetCoord(grid2D, ESMF_STAGGERLOC_CENTER, rc=rc)

   !-------------------------------------------------------------------
   ! Get the bounds of the first coordinate array on the local DE.
   !-------------------------------------------------------------------
   call ESMF_GridGetLocalTileInfo(grid2D, coord=1, &
          computationalLBound=lbnd, computationalUBound=ubnd, rc=rc)

   !-------------------------------------------------------------------
   ! Get the pointer to the first coordinate array from inside
   ! the Grid object.
   !-------------------------------------------------------------------
   call ESMF_GridGetLocalTileCoord(grid2D, coord=1, coordsX, rc=rc)

   !-------------------------------------------------------------------
   ! Calculate and set coordinates in the first dimension.
   !-------------------------------------------------------------------
   do i=lbnd(1),ubnd(1)
        coordsX(i,j) = i*10.0
   enddo

   !-------------------------------------------------------------------
   ! Get the bounds of the second coordinate array on the local DE.
   !-------------------------------------------------------------------
   call ESMF_GridGetLocalTileInfo(grid2D, coord=2, &
          computationalLBound=lbnd, computationalUBound=ubnd, rc=rc)

   !-------------------------------------------------------------------
   ! Get the pointer to the second coordinate array from inside
   ! the Grid object.
   !-------------------------------------------------------------------
   call ESMF_GridGetLocalTileCoord(grid2D, coord=2, coordsY, rc=rc)

   !-------------------------------------------------------------------
   ! Calcuate and set coordinates in the second dimension.
   !-------------------------------------------------------------------
   do i=lbnd(1),ubnd(1)
        coordsY(i) = i*10.0
   enddo
!EOC

!BOE
!\subsubsection{Creating a Grid with Regular Distribution, 
                Horizontal Curvilinear Coordinates,
!               and an Undistributed Vertical Dimension}
! \label{example:CurviGridWithUndistDim}
!
! This example demonstrates how a user can build a curvilinear 
! horizontal grid with an undistributed vertical dimension.  This
! might be, for example, a displaced pole grid used in ocean modeling
! (the north pole is moved over a land mass so it can be ignored during
! computation), with depth in the vertical.  Since the grid is
! likely to be much greater in size in the horizontal, only the 
! horizontal dimension is distributed.
!
!EOE

!BOC
   !-------------------------------------------------------------------
   ! Create the Grid:  Allocate space for the Grid object.  The
   ! grid is defined to be 128 elements in Dim1 (longitude), 64 in
   ! Dim2 (longitu), and 32 in Dim3 (depth).  The longitude 
   ! dimension is periodic, poles are defined at each end of the
   ! latitude dimension, and the depth defaults to aperiodic bounds. 
   !-------------------------------------------------------------------
   grid2D=ESMF_GridCreateShape(maxIndex=(/128,64,32/), 
          connDim1=/ESMF_GRIDCONN_PERIODIC,ESMF_GRIDCONN_PERIODIC/,
          connDim2=/ESMF_GRIDCONN_POLE,ESMF_GRIDCONN_POLE/,
         indexflag=ESMF_GLOBAL, rc=rc)

   !-------------------------------------------------------------------
   ! Allocate coordinate storage and associate it with the center
   ! stagger location.  Since no coordinate values are specified in
   ! this call no coordinate values are set yet.
   !-------------------------------------------------------------------

   call ESMF_GridSetCoord(grid2D, ESMF_STAGGERLOC_CENTER, rc=rc)

   !-------------------------------------------------------------------
   ! Get the bounds of the first coordinate array on the local DE.
   !-------------------------------------------------------------------
   call ESMF_GridGetLocalTileInfo(grid2D, coord=1, &
          computationalLBound=lbnd, computationalUBound=ubnd, rc=rc)

   !-------------------------------------------------------------------
   ! Get the pointer to the first coordinate array from inside
   ! the Grid object.
   !-------------------------------------------------------------------
   call ESMF_GridGetLocalTileCoord(grid2D, coord=1, coordsX, rc=rc)

   !-------------------------------------------------------------------
   ! Calculate and set coordinates in the first dimension.
   !-------------------------------------------------------------------
   do i=lbnd(1),ubnd(1)
        coordsX(i,j) = i*10.0
   enddo

   !-------------------------------------------------------------------
   ! Get the bounds of the second coordinate array on the local DE.
   !-------------------------------------------------------------------
   call ESMF_GridGetLocalTileInfo(grid2D, coord=2, &
          computationalLBound=lbnd, computationalUBound=ubnd, rc=rc)

   !-------------------------------------------------------------------
   ! Get the pointer to the second coordinate array from inside
   ! the Grid object.
   !-------------------------------------------------------------------
   call ESMF_GridLocalTileGetCoord(grid2D, coord=2, coordsY, rc=rc)

   !-------------------------------------------------------------------
   ! Calcuate and set coordinates in the second dimension.
   !-------------------------------------------------------------------
   do i=lbnd(1),ubnd(1)
        coordsY(i) = i*10.0
   enddo
!EOC


!BOE
! \subsubsection{Creating a Spherical 2D Grid with a Third Undistributed Dimension}
!
! This example  illustrates the creation of a 2D spherical Grid with a
! 3rd undistributed dimension.  
! The Grid contains both the center stagger location and a corner
! (i.e. Arakawa B-Grid). The 2D horizontal grid is distributed across
! the processors according to  gridDist1 and gridDist2. The number
! of vertical levels is gridVertSize.
!EOE

!BOC
   ! Use ESMF framework module
   use ESMF_Mod
   implicit none

   ! Local variables  
   integer:: rc, finalrc
   integer:: myPet, npets, rootPet
   type(ESMF_VM):: vm
   type(ESMF_Grid) :: grid2D1
   integer :: gridDist1(:),gridDist2(:) gridVertSize
   integer :: lbnd(2),ubnd(2)
   integer :: lbndV(1),ubndV(1)
   real(ESMF_TYPEKIND_R8), pointer :: coordsLat(:,:),coordsLon(:,:)
   real(ESMF_TYPEKIND_R8), pointer :: coordsVert(:)
!EOC         

      finalrc = ESMF_SUCCESS
      call ESMF_Initialize(vm=vm, rc=rc)

!BOC
   ! Construct a 2D spherical grid with an undistributed third dimension. Using
   ! the 2D distribution specified in gridDist1 and gridDist2, and the vertical
   ! count specified in gridVertSize.  

   ! Create Grid
   grid2D1=ESMF_GridCreateShape(countsPerDEDim1=gridDist1, &
                              countsPerDEDim2=gridDist2, &
                              countsPerDEDim3=(/gridVertSize/), &
                              connDim1=(/ESMF_GRIDCONN_POLE,ESMF_GRIDCONN_POLE/), & 
                              connDim2=(/ESMF_GRIDCONN_PERIODIC,ESMF_GRIDCONN_PERIODIC/), &
                              coordDep1=(/1,2/), &
                              coordDep2=(/1,2/), &
                              coordDep3=(/3/), &
                              indexflag=ESMF_GLOBAL, &
                              rc=rc)   



  ! Add the center and corner stagger locations. 
  call ESMF_GridSetCoord(grid2D1,staggerLoc=ESMF_STAGGERLOC_CENTER,rc=rc)
  call ESMF_GridSetCoord(grid2D1,staggerLoc=ESMF_STAGGERLOC_CORNER,rc=rc)


  ! Set the horizontal coordinates by using the non-ESMF functions
  ! CalcHorzLat, CalcHorzLon to calculate them from the global indices. 
   call ESMF_GridGetLocalTileInfo(grid2D1, coord=1, &
          computationalLBound=lbnd, computationalUBound=ubnd, rc=rc)


   ! Get pointers to the coordinate component storage
   call ESMF_GridGetLocalTileCoord(grid2D1, coord=1, coordsLon, rc=rc)
   call ESMF_GridGetLocalTileCoord(grid2D1, coord=2, coordsLat, rc=rc)

   ! Set the coordinates
   do j=lbnd(2),ubnd(2)
   do i=lbnd(1),ubnd(1)
        coordsLon(i,j) = CalcHorzLon(i,j)
        coordsLat(i,j) = CalcHorzLat(i,j)
   enddo
   enddo


   ! Set the vertical  coordinates by using the non-ESMF function
   ! CalcVert.

   ! We actually know the bounds already, but go through the exercise anyway.
   call ESMF_GridGetLocalTileInfo(grid2D1, coord=3, &
          computationalLBound=lbndV, computationalUBound=ubndV, rc=rc)

   ! Get pointers to the coordinate component storage
   call ESMF_GridGetLocalTileCoord(grid2D1, coord=3, coordsVert, rc=rc)

   ! Set the vertical coordinates
   do i=lbndV(1),ubndV(1)
        coordsVert(i) = CalcVert(i)
   enddo

!EOC

!BOE
!\subsubsection{Creating an Empty Grid in a Parent Component 
! for Completion in a Child Component}\label{sec:usage:setcommit}
! 
! ESMF Grids can be created using an incremental paradigm.
! To do this, the user first calls ESMF\_GridCreateEmpty to 
! allocate the shell of a Grid. Next, a series of
{\tt ESMF\_GridSet()} calls are used to fill in the
! details of the grid.  Here we use a convenient {\tt ESMF\_GridSetShape()}
! call that fills in the Grid via an interface much like the
! {\tt ESMF\_GridCreateShape()} call.  Finally, after the sets,
! any other grid call 
! will internally validate and create the final, usable, grid.
! For consistency's sake the initial {\tt ESMF\_GridCreateEmpty}
! call must occur on the same or a superset of the processors as
! the {\tt ESMF\_GridSet()} calls. 
! The following example uses the incremental technique to create
! a rectangular 10x20 grid with coordinates at the center and
! corner stagger locations. 
!EOE

!BOC
   
!---------------------------------------------------------------------------
! IN THE PARENT COMPONENT:
! Create an empty grid in the parent component for use in a child component.
! The parent may be defined on more PETs than the child component.  
! The child's [vm or pet list] is passed into the create call so that
! the Grid is defined on the appropriate subset of the parent's PETs. 
!---------------------------------------------------------------------------
   grid=ESMF_GridCreateEmpty(petList=/1,2,3,4/, rc=rc)


!---------------------------------------------------------------------------
! IN THE CHILD COMPONENT:
! Set the grid topology.  Here we define an irregularly distributed 
! rectangular Grid.
!---------------------------------------------------------------------------

   call ESMF_GridSetShape(grid, countsPerDEDim1=(/5,5/), &
          countsPerDEDim2=(/7,7,6/), rc=rc)

!---------------------------------------------------------------------------
! Set Grid coordinates at the cell center location.
!---------------------------------------------------------------------------
   call ESMF_GridSetCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)

!---------------------------------------------------------------------------
! Set Grid coordinates at the corner stagger location.
!---------------------------------------------------------------------------
   call ESMF_GridSetCoord(grid, staggerLoc=ESMF_STAGGERLOC_CORNER, rc=rc)
!EOC

!BOE
!
!\subsubsection{Specifying Grid Size, Rank, and Distribution}

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
! The shortcut grid creation method supports three types of
! distribution (see Section~\ref{sec:desc:dist}). In addition to describing
! the partition of the index space, these methods also 
! simultaneously describe its size and rank. 
!
! One supported type of distribution is regular distribution. 
! To use this type the user specifies the 
! global minimum and maximum ranges of the index space
! ({\tt minIndex} and {\tt maxIndex}) and the number
! of pieces to break each dimension into (via {\tt regDecomp}).
! ESMF then breaks the index space as evenly as possible 
! into the specified number of pieces. If {\tt minIndex} is 
! not specified, then the bottom of the index range is assumed
! to be (1,1,...,1). If {\tt regDecomp} is not specified, then
! by default ESMF creates an NPx1x1...1 distribution where 
! NP is the number of processors. (i.e. The first index dimension
! is evenly divided across the processors, the rest are not divided.)
! The rank of the Grid is the size of {\tt maxIndex}. To create
! an undistributed dimension set that entry in {\tt regDecomp}
! to 1. The following is an example of creating a 10x20x30 3D grid
! where the first dimensions is broken into 2 pieces, the second
! is broken into 4 pieces, and the third is undistributed. 
!EOE

!BOC
   grid=ESMF_GridCreateShape(maxIndex=(/10,20,30/), &
          regDecomp=(/2,4,1/), rc=rc)   
!EOC

!BOE
! A second supported type of distribution is irregular distribution.
! In this type the user specified the exact number of cells per
! DE in each dimension.  For the {\tt ESMF\_GridCreateShape} call
! the {\tt countsPerDEDim1,2,3}
! arguments are used.  These specify a rectangular 
! distribution containing size(countsPerDEDim1) by
! size(countsPerDEDim2) by size(countsPerDEDim3)
! DEs.  The entries in each of these arrays specify
! he size of the DEs in that dimension for that row or column.
! The rank of the grid is determined by the presence of
! {\tt countsPerDEDim3}.  If it's present the Grid
! will be 3D, if just {\tt countsPerDEDim1} and 
! {\tt countsPerDEDim2} are specified the Grid 
! will be 2D.  If any of these arrays has size
! 1 then that index dimension is undistributed.
!
! The following call illustrates the creation of
! a 10x20 2D rectangular Grid distributed across six processors
! in two groups of 5 in the first dimension and 
! three groups of 7,7,6 in the second dimension.
!EOE

!BOC
   grid=ESMF_GridCreateShape(countsPerDEDim1=(/5,5/), &
          countsPerDEDim2=(/7,7,6/), rc=rc)   
!EOC

!BOE
! To add a distributed third dimension of size 30, broken up into
! two groups of 15, the above call would be altered as follows. 
!EOE

!BOC
   grid=ESMF_GridCreateShape(countsPerDEDim1=(/5,5/), &
          countsPerDEDim2=(/7,7,6/), countsPerDEDim3=(/15,15/), rc=rc)   
!EOC

!BOC! If the third dimension were undistributed then the call
! would look like the following. 
!EOE

!BOC
   grid=ESMF_GridCreateShape(countsPerDEDim1=(/5,5/), &
          countsPerDEDim2=(/7,7,6/), countsPerDEDim1=(/30/), rc=rc)   
!EOC

!BOE
!
! For the previous two types of distribution (regular and irregular), the 
! {\tt petMap} parameter may be used to specify which PETs 
! the DEs in the Grid are assigned to. The {\tt petMap} 
! array is the same size in each dimension as the number of DEs in
! that dimension. If the Grid is 2D then the third dimension is 
! of size 1. The following example creates the grid on a specific
! set of PETs.  
!EOE

!BOC
   ! Set petMap
   petMap(:,1,1) = (/4,5/)
   petMap(:,2,1) = (/3,2/)
   petMap(:,3,1) = (/1,0/)

   grid=ESMF_GridCreateShape(countsPerDEDim1=(/5,5/), &
           countsPerDEDim2=(/7,7,6/), petMap=petMap, rc=rc)   !EOC

!BOE
! Arbitrary distribution has not yet been implemented.
! When it is, the user will specify the 
! global minimum and maximum ranges of the index space
! ({\tt minIndex} and {\tt maxIndex}) and the set of
! index space locations residing on the local processor (via {\tt localIndices}).
! Again, if {\tt minIndex} is  not specified, then the bottom of the 
! index range is assumed to be (1,1,...). 
! The rank of the Grid is the size of {\tt maxIndex}. 
! The following is an example of creating a 5x5 2D grid
! with the diagonal (i.e. indices (i,i) where i goes from 1 to 5)
! on this processor (The rest of the processors would have to declare
! the remainder of the Grid locations.)
!removeEOE

!BOC
   ! Set local indices
   localIndices(:,1)=(/1,1/)
   localIndices(:,2)=(/2,2/)
   localIndices(:,3)=(/3,3/)
   localIndices(:,4)=(/4,4/)
   localIndices(:,5)=(/5,5/)

   ! Create Grid
   grid=ESMF_GridCreateShape(maxIndex=(/5,5/), &
          localIndices=localIndices, rc=rc)   
!EOC


!BOE
!
!\subsubsection{Specifying Tile Edge Connections}
! The {\tt ESMF\_GridCreateShape} command has three arguments
! specifying edge connections. The three 
! {\tt connDim1}, {\tt connDim2}, and {\tt connDim3} are all of type
! {\tt ESMF\_GridConn}, and can be used
! to set different types of connections at the ends of each dimension.
! Each of these arguments is a two element array. See section 
! \ref{sec:opt:gridconn} for a list of valid values of {\tt ESMF\_GridConn}.
! The following constructs a sphere with a bipole at either end of dimension 2.
!EOE

!BOC
   grid=ESMF_GridCreateShape(countsPerDEDim1=(/5,5/), &
           countsPerDEDim2=(/7,7,6/), &
           connDim1=(/ESMF_GRIDCONN_PERIODIC, ESMF_GRIDCONN_PERIODIC/), &
           connDim2=(/ESMF_GRIDCONN_BIPOLE, ESMF_GRIDCONN_BIPOLE/), &
           rc=rc)   
!EOC

!BOE
!
! If a pole connection is specified, the stagger location of the pole
! may be needed to precisely describe the connection. The three optional arguments
! {\tt poleStaggerLoc1}, {\tt poleStaggerLoc2}, and {\tt poleStaggerLoc3}
! let the user set the stagger location of the pole. The following
! illustrates setting the pole location to the center.
!EOE

!BOC
   grid=ESMF_GridCreateShape(countsPerDEDim1=(/5,5/), &
           countsPerDEDim2=(/7,7,6/), &
           connDim1=(/ESMF_GRIDCONN_PERIODIC, ESMF_GRIDCONN_PERIODIC/), &
           connDim2=(/ESMF_GRIDCONN_BIPOLE, ESMF_GRIDCONN_BIPOLE/), &
           poleStaggerLoc2=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CENTER/), &
           rc=rc)   
!EOC

!BOE
!
! If a bipole connection is specified, the position of the poles
! may be needed to correctly describe the connection. The three optional arguments
! {\tt bipolePos1}, {\tt bipolePos2}, and {\tt bipolePos3}
! let the user set the position of one of the poles. The position of the 
! second  pole is implied by the position of the first and is halfway around
! the periodic dimesnion from the first pole. The following illustrates setting the pole position to 1.
!EOE

!BOC
   grid=ESMF_GridCreateShape(countsPerDEDim1=(/5,5/), &
           countsPerDEDim2=(/7,7,6/), &
           connDim1=(/ESMF_GRIDCONN_PERIODIC, ESMF_GRIDCONN_PERIODIC/), &
           connDim2=(/ESMF_GRIDCONN_BIPOLE, ESMF_GRIDCONN_BIPOLE/), &
           bipolePos2=(/1,1/), &
           rc=rc)   
!EOC


!BOE
!
!\subsubsection{Specifying Coordinate Arrays and Their Relation to
Index Space Dimensions}
!
! To specify how the coordinate arrays are mapped to the 
! index dimensions the arguments {\tt coordDep1,2,3}
! are used. The size of the arrays specify the rank
! of the cooresponding coordinate component array.
! (e.g. size(coordDep1)=2 means that coordinate
! component 1 (e.g. x) is 2D) The entries in
! {\tt coordDep} specify which index dimension
! the cooresponding coordinate component dimension
! maps to.  (e.g. coordDep1=(/1,2/) means that
! the first dimension of component 1 maps to index dimension
! 1 and the second maps to index dimension 2.) If not set,
! then {\tt coordDepX} defaults to (/X/), in other
! words rectilinear.  
! 
! The following call demonstrates the creation of a
! 10x20 2D rectilinear grid where the first coordinate
! component is mapped to the second index dimension
! (i.e. is of size 20) and the second coordinate component
! is mapped to the first index dimension (i.e. is of size
! 10).
!EOE

!BOC
   grid=ESMF_GridCreateShape(countsPerDEDim1=(/5,5/), &
          countsPerDEDim2=(/7,7,6/), coordDep1=(/2/), &
          coordDep2=(/1/), rc=rc)   
!EOC 

!BOE
! The following call demonstrates the creation of a
! 10x20 2D curvilinear grid where where both
! coordinate component arrays are 10x20 also. 
!EOE

!BOC
   grid=ESMF_GridCreateShape(countsPerDEDim1=(/5,5/), &
          countsPerDEDim2=(/7,7,6/), coordDep1=(/1,2/), &
          coordDep2=(/1,2/), rc=rc)   
!EOC 

!BOE
! The following call demonstrates the creation of a
! 10x20x30 2D plus 1 curvilinear grid where 
! coordinate component 1 and 2 are still 10x20, but
! coordinate component 3 is mapped just to the 
! undistributed third index dimension.
!EOE

!BOC
   grid=ESMF_GridCreateShape(countsPerDEDim1=(/5,5/), &
          countsPerDEDim2=(/7,7,6/), countsPerDEDim1=(/30/), &
          coordDep1=(/1,2/), coordDep2=(/1,2/), &
          coordDep3=(/3/), rc=rc)   
!EOC 

!BOE
!
! By default the local piece of the array on each processor starts at 
! (1,1,..), however, the indexing for each grid coordinate array  
! on each DE may be shifted to the global indices by using the {\tt indexflag}.
! For example, the following call switches the grid to use global indices. 
!EOE

!BOC
   grid=ESMF_GridCreateShape(countsPerDEDim1=(/5,5/), &
           countsPerDEDim2=(/7,7,6/), indexflag=ESMF_GLOBAL, rc=rc)   
!EOC


!BOE
!
!\subsubsection{Specifying Coordinate Type and Kind}
!
! The default type and kind for Grid coordinates is ESMF\_R8. 
!  To control the data type and kind more precisely the optional 
!  {\tt coordTypeKind} parameter may be used. The following illustrates
! the creation of a Grid with 4 byte integer coordinates. 
!EOE

!BOC 
   grid=ESMF_GridCreateShape(coordTypeKind=ESMF_TYPEKIND_I4, &
          countsPerDEDim1=(/5,5/), countsPerDEDim2=(/7,7,6/), rc=rc)   
!EOC  

!BOE
!\subsubsection{Associating Coordinates with Stagger Locations}\label{sec:usage:staggerloc}
!
! In this document, stagger location refers to the places in a Grid
! cell that can contain data. For example, data can be located
! at the cell center,  at the cell corner, at the cell face, and so on into
! higher dimensions. After Grid creation the user can create a field at any of the stagger
! locations in a Grid. However, the user must specifically add coordinates
! to a stagger location if they want them. Coordinates in a Grid 
! mau be neccesary for some ESMF functionality (e.g. regrid). The ESMF Grid class
! allows the user to put coordianates at multiple stagger locations per
! Grid.  When adding, or accessing
! coordinate data the stagger location is specified to tell the Grid method 
! from where in the cell to get the data. There are predefined stagger locations
! (see Section~\ref{sec:opt:staggerloc}), however, 
! should the user find it necessary to describe their own, there
! is also a set of methods for generating custom locations. 
! (see Section~\ref{ref:stagger} for a more in depth
! description of stagger locations and stagger specification.)
!
! The following example illustrates the use of the  predefines
! in specifying a Grid's stagger locations.  
!
! To set which stagger locations in a Grid have coordinate data, the subroutine
! {\tt ESMF\_GridSetCoord} is used. The following example
! adds coordinate storage to the corner stagger location in {\tt grid}.
!EOE

!BOC 
   call ESMF_GridSetCoord(grid, staggerLoc=ESMF_STAGGER_CORNER, rc=rc)
!EOC  

!BOE
! The user may also set the coordinate data at the same time as adding
! the coordinate storage.  The following example
! adds coordinate storage to the corner stagger location in {\tt grid}, plus sets
! the associated coordinate data. Note, the input coordinate arrays (CoordX, CoordY)
! may be either F90 or ESMF Array's (The F90 arrays are restricted to single DE to
! PET mappings). They also need to be of the proper size and rank to coorespond to the Grid. 
!EOE

!BOC 
   call ESMF_GridSetCoord(grid, staggerLoc=ESMF_STAGGER_CORNER, &
          coord1=CoordX, coord2=CoordY, rc=rc)
!EOC  

!BOE
!\subsubsection{Accessing Grid Coordinates}
!
! Once a Grid has been created the user has several options to access
! the Grid coordinate data. The first pair of these allow the user
! to use the {\tt ESMF\_Array} class to set or get data 
! for one stagger location across the whole Grid.
! {\tt ESMF\_GridSetCoordFromArray} allows the user to set coordinate
! data from an Array. For example, the following sets the 
! first component (e.g. x) coordinates for the center stagger location to 
! those in the array xCoord.
! EOE

!BOC
   call ESMF_GridSetCoord(grid, &
          staggerLoc=ESMF_STAGGERLOC_CENTER, &
          coord=1, array=xCoord, rc=rc)
!EOC

!BOE
! {\tt ESMF\_GridGetCoord()}, allows the user
! to get the Array (a direct reference or a copy) which
! contains the coordinate data for a stagger location on a Grid. The user
! can then employ any of the standard {\tt ESMF\_Array} tools to operate
! on the data. The following copies the coordinates from the second 
! component of the corner and puts it into Array copyOfY. 
! EOE

!BOC
   call ESMF_GridGetCoord(grid, &
          staggerLoc=ESMF_STAGGERLOC_CORNER,coord=2, &
          array=copyOfY, docopy=ESMF_DATA_COPY, rc=rc)
!EOC

!BOE
! The second pair of methods enable the user to set or get data using
! a fortran pointer. These methods only work with the local piece of the 
! Grid on the DE. {\tt ESMF\_GridLocalTileSetCoord} enables the user
! to set data into the local piece of the coordinates residing on the DE.
! The following call gets a pointer (fptr) to the fortran array holding the 
! first component (e.g. x) coordinates for the corner stagger
! for the piece of tile 2 which is on this processor. It
! defaults to the first DE because it isn't specified. 
!EOE

!BOC
   call ESMF_GridSetLocalTileCoord(grid, tile=2, &
          staggerLoc=ESMF_STAGGERLOC_CORNER,    &
          coord=1, fptr, &
          doCopy=ESMF_DATA_REF, rc=rc)
!EOC

!BOE
! The call {\tt ESMF\_GridLocalTileGetCoord} gets a fortran pointer to 
! the coordinate data. The user can then operate on this array in the usual
! manner. The following call allocates an array (fptr) and
! makes copy of the part of tile 1's third component (e.g. z)  coordinates which
! lie on the second DE. 
!EOE

!BOC
   call ESMF_GridGetLocalTileCoord(grid, tile=1, localDE=2, &
          coord=3, fptr, doCopy=ESMF_DO_COPY, rc=rc)

!EOC

!BOE
! The last pair of methods allow the user to access coordinate data a point at
! a time given the index space coordinates. The method 
! {\tt ESMF\_GridSetLocalTileCoord}, allows for a uniform method of setting
! coordinates in the Grid no matter what the factorization of the 
! arrays. The following sets the coordinates at the index point (1,1,1) 
! in the center stagger location to (.5,.5,.5).
!EOE

!BOC
   call ESMF_GridSetLocalTileCoord(grid, &
          staggerLoc=ESMF_STAGGERLOC_CENTER, 
          indices=(/1,1,1/), coords=(/.5,.5,.5/), rc=rc)
!EOC

!BOE
! The method {\tt ESMF\_GridGetLocalTileCoord}, allows for a uniform 
! method of retrieving coordinates from the Grid no matter what the factorization of the 
! arrays. The following retrieves the coordinates (outCoords) from 
! point (1,1,1) in the center stagger location.  
!EOE

!BOC
   call ESMF_GridGetLocalTileCoord(grid, &
          staggerLoc=ESMF_STAGGERLOC_CENTER, 
          indices=(/1,1,1/), coords=outCoords, rc=rc)
!EOC


!BOE
!
!\subsubsection{Specifying Halo Width}
!
! The default halo width for a Grid is 0. 
! The shortcut Grid shape creation methods allow the user
! to set the halo, but it needs to be the same depth in all dimensions.
! The haloWidth may be defined asymmetrically by using the
!  more general grid create call. The parameter {\tt haloWidth} 
!  may be used to set the coordinate halo width. The following
!  is an example of setting the halo width to 1.
!EOE

!BOC 
   grid=ESMF_GridCreateShape(countsPerDEDim1=(/5,5/), &
           countsPerDEDim2=(/7,7,6/), haloWidth=1, rc=rc)   
!EOC  


!BOE
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
!EOE

!BOC
   call ESMF_GridGenCoordsUni(grid, begCoord=(/11.0,10.0,100.0/), &
          endCoord=(/100.0,100.0,0.0/), rc=rc)
!EOC

!removeBOE
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
!removeEOE

!removeBOC
   call ESMF_GridCalcStaggerLocCoord(grid, srcStaggerLoc=ESMF_STAGGERLOC_CORNER, rc=rc)
!removeEOC

!removeBOE
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
!removeEOE

!removeBOC
   call ESMF_GridHalo(grid, rc=rc)
!removeEOC


!removeBOE
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


!BOE
! \subsubsection{Creating a Regularly Distributed 3D Grid with Generated Coordinates}
!
! This example illustrates the creation of a 100x100x100  3D Grid distributed across
! 5 processors in each dimension. The coordinates in the Grid are uniformly distributed
! between (0.0, 0.0, 0.0) and (200.0, 200.0, 200.0).
!
!EOE

!BOC
   ! Use ESMF framework module
   use ESMF_Mod
   implicit none

   ! Local variables  
   integer:: rc, finalrc
   type(ESMF_Grid) :: grid
!EOC         

      finalrc = ESMF_SUCCESS
      call ESMF_Initialize(vm=vm, rc=rc)
      call ESMF_VMGet(vm, localPet=myPet, petCount=npets, rc=rc)

!BOC
   ! Create the Grid.
   grid=ESMF_GridCreateShape(maxIndex=(/100,100,100/), regDecomp=(/5,5,5/), rc=rc)   

   ! Add a center stagger location 
   call ESMF_GridSetCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)

   ! Put in the coordinates
   call ESMF_GridGenCoordUni(grid, begCoord=(/0.0,0.0,0.0/), &
          endCoord=(/200.0, 200.0, 200.0/), rc=rc)
 
!EOC


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



!BOE
!\subsubsection{Creation: Advanced}
!
! Beyond the shortcut methods for creating a grid, there is
! a set of methods which give the user more control over the
! specifics of the grid.  The following describes the more 
! general interface. 
!
!
!\subsubsection{Creation: Advanced: Size, Rank, Shape, and Distribution}~\label{sec:usage:adv:create}

%% NEED TO ADD MORE HERE EXPLAINING HOW THE GENERAL DIST DIFFER FROM THE 
%% SHORTCUT AND EXPLAINING MORE ABOUT USING THE DIST GRID

! There are four methods of specifying the distribution in the more
! general grid creation interface. Three of them are basically the same as
! those used in the shortcut create 
! and the user is directed to Section~\ref{sec:usage:short:create} for further explanation
! of those. The fourth method is to first create an ESMF DistGrid object describing
! the distribution and shape of the Grid and then to employ that to either directly
! create a Grid, or to create Arrays first and then to create a Grid from those. 
! This method gives the user maximum control over the topology and distribution of the Grid. 
! Please see the DistGrid design document for an in depth description of its use. 
! Also, Example~\ref{sec:usage:ex:adv:cart} and Example~\ref{sec:usage:ex:adv:tripole}
! illustrate its use in creating two types of Grid. 
!
! A DistGrid only describes the distributed dimensions of the index
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
   distgrid = ESMF_DistGridCreate(minCorner=(/1,2/), maxCorner=(/11,22/), rc=rc)  

   ! Create Grid
   grid=ESMF_GridCreate(distGrid=distgrid, lbounds=(/3/), ubounds=(/33/), rc=rc)
!EOC  

!BOE
! To alter which dimensions are distributed, the {\tt dimmap} 
! argument can be used. The {\tt dimmap} is used to set
! which dimensions of the Grid are mapped to the dimensions
! described by {\tt maxIndex}. In other words, it describes how the dimensions of 
! the underlying default DistGrid are mapped to the Grid. Each entry
! in {\tt dimmap} contains the Grid dimension to which the cooresponding
! DistGrid dimension should be mapped. 
! This example illustrates the creation of a Grid where the undistributed
! dimension is first. To accomplish this the two distributed dimensions are mapped
! to the last two Grid dimensions (i.e. 2 and 3). 
!EOE

!BOC 
   ! Create DistGrid
   distgrid = ESMF_DistGridCreate(minCorner=(/1,2/), maxCorner=(/11,22/), rc=rc)  

   ! Create Grid
   grid=ESMF_GridCreate(distGrid=distgrid, lbounds=(/3/), ubounds=(/33/), 
          dimmap=(/2,3/), rc=rc)
!EOC  

!BOE

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
! The parameters {\tt coordRanks} and {\tt coordDimMap}
! can be used to specify the factorization of the coordinate arrays. 
!
! The default Grid has coordinate arrays the same rank as the Grid.
! To alter this, the {\tt coordRanks} parameter can be used
! to set the rank of the coordinate arrays.  The size of the parameter  
! is the rank of the Grid. Each entry of {\tt coordRanks}  is the rank of 
! the associated coordinate array. The following creates a 10x20
! 2D Grid where both the x and y coordinates are stored in 1D arrays. 
!EOE

!BOC 
   Grid2D=ESMF_GridCreate(maxIndex=(/10,20/), &
            coordRanks=(/1,1/) , rc=rc)
!EOC  

!BOE
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
!EOE

!BOC 
   coordDimMap(1,1)=2      ! Map X (i.e. 1) to second Grid dimension 
   coordDimMap(2,1)=1      ! Map Y (i.e. 2) to first Grid dimension 
   Grid2D=ESMF_GridCreate(maxIndex=(/10,20/), coordRanks=(/1,1/) , &
               coordDimMap=coordDimMap, rc=rc)
!EOC  

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



!BOE
!\subsubsection{Stagger Location: Advanced}~\label{sec:usage:staggerloc:adv}
!
! This section discusses some of the more advanced options available during 
! the addition of data (coordinates or metrics) to a particular stagger locations in a Grid. The first of these is
! the construction of custom stagger locations. 
! To construct a custom stagger (method described further in Section~\ref{ref:stagger})
! the users uses the subroutine {\it ESMF\_StaggerLocSet()} to
! give a qualitative description of the location
! in each dimension (center (0) vs. side (1)).
! This method allows users
! to construct stagger locations for which
! there is no predefined value. In this example, it's used to 
! set the 4D center and 4D corner locations. 
!EOE

!BOC 

   ! Set Center
   call ESMF_StaggerLocSet(staggerLoc,where=(/0,0,0,0/),rc=rc)
   call ESMF_GridSetCoord(grid, staggerLoc=staggerLoc, rc=rc)

   ! Set Corner
   call ESMF_StaggerLocSet(staggerLoc,where=(/1,1,1,1/),rc=rc)
   call ESMF_GridSetCoord(grid, staggerLoc=staggerLoc, rc=rc)
!EOC
  
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
! For example, given the preceeding example Grid where the ``*'' represent corner 
! locations and the ``+'' represent center locations, for the corners to 
! enclose the centers (symmetric stagger) there needs to be one more corner than 
! center in each dimension. This is true in general for Grids without periodicity or
! other connections.  In fact for a symmetric stagger, given that the center
! location requires n x m storage, the corresponding corner location
! requires n+1 x m+1, and the edges require n+1 x m or
! m+1 x n.  In order to add the extra storage, but also to 
! allow the the different stagger location arrays to remain on the same DistGrid,
! a capability of the ESMF Array class to have extra computational
! padding is used. By default, when the coordinate arrays are created one extra
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
The {\tt coordLWidth} and 
{\tt coordUWidth} arguments are both 1D arrays of the
same size as the Grid rank. The entries in the arrays
give the extra offset from the outer boundary of
the tile exclusive region for
each stagger location. The following example shows the
addition of two stagger locations. The
corner location has no extra boundary and the 
center has a single layer of extra padding on 
the negative side and none on the positive. 
!EOE

!BOC 
   call ESMF_GridSetCoord(grid, staggerLoc=ESMF_STAGGERLOC_CORNER, &
          coordLWidth=(/0,0/), coordUWidth=(/0,0/), rc=rc)

   call ESMF_GridSetCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
          coordLWidth=(/1,1/), coordUWidth=(/0,0/), rc=rc)

!EOC  

!BOE
To indicate how the data at a particular stagger location is aligned with the 
cell center the optional {\tt coordAlign} parameter 
may be used. This parameter indicates which stagger elements 
in a cell shares the same index values as the cell center. 
For example, in a 2D cell, it would indicate which of the four corners has
the same index value as the center. To set {\tt coordAlign},  
the values -1,+1 are used to indicate the alignment in
each dimension. If a stagger location is 
centered in a dimension (e.g. an edge in 2D), then that
dimension is ignored in the alignment. This parameter is mostly 
informational, however, if the {\tt coordWidth} parameters 
are not set then its value determines where the default padding
is placed. If not specified, then the default is to align all 
staggers to the most negative, so the padding is on the positive side. 
The following code illustrates aligning the positive corner with the center. 
!EOE

!BOC 
   call ESMF_GridSetCoord(grid, staggerLoc=ESMF_STAGGERLOC_CORNER, &
          coordAlign=(/+1,+1/), rc=rc)
!EOC  


10 continue
  call ESMF_Finalize(rc=rc)
  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_GridEx.F90"
  else
    print *, "FAIL: ESMF_GridEx.F90"
  endif
  
end program
