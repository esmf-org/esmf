! $Id: ESMF_GridUsageEx.F90,v 1.28.2.19 2009/01/28 23:54:21 oehmke Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#undef LOCAL_NOT_IMPL 
program ESMF_GridCreateEx

!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
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



!

#ifdef LOCAL_NOT_IMPL
!BOEI
!
!\subsubsection{Specifying Tile Edge Connections}
! \label{example:TileEdgeConn}
!
! The {\tt ESMF\_GridCreateShapeTile} command has three arguments 
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
   grid2D=ESMF_GridCreateShapeTile(countsPerDEDim1=(/5,5/), &
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
   grid2D=ESMF_GridCreateShapeTile(countsPerDEDim1=(/5,5/), &
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
   grid2D=ESMF_GridCreateShapeTile(countsPerDEDim1=(/5,5/), &
           countsPerDEDim2=(/7,7,6/), &
           connDim1=(/ESMF_GRIDCONN_PERIODIC, ESMF_GRIDCONN_PERIODIC/), &
           connDim2=(/ESMF_GRIDCONN_BIPOLE, ESMF_GRIDCONN_BIPOLE/), &
           bipolePos2=(/1,1/), &
           rc=rc)   
!EOCI
#endif



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
   ! will have global coordinates.  Note that aperiodic bounds are
   ! specified by default - if periodic bounds were desired they
   ! would need to be specified using an additional gridConn argument
   ! (which isn't implemented yet). In this call the minIndex hasn't 
   ! been set, so it defaults to (1,1,...).
   !-------------------------------------------------------------------
   grid2D=ESMF_GridCreateShapeTile(          &
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

!BOE
!\subsubsection{Creating an Irregularly Distributed Rectilinear Grid with
!                a Non-Distributed Vertical Dimension}
! \label{example:CurviGridWithUndistDim}
!
! This example demonstrates how a user can build a rectilinear 
! horizontal Grid with a non-distributed vertical dimension. The Grid 
! contains both the center and corner stagger locations (i.e. Arakawa 
! B-Grid). In contrast to the previous examples, this example doesn't
! assume that the code is being run with a 1-1 mapping between 
! PETs and DEs. It should work when run on any number of PETs.
!
!EOE

!BOC
   !-------------------------------------------------------------------
   ! Create the Grid:  Allocate space for the Grid object.  The
   ! Grid is defined to be 180 Grid cells in the first dimension
   ! (e.g. longitude), 90 Grid cells in the second dimension (e.g. latitude), and
   ! 40 Grid cells in the third dimension (e.g. height).  The first dimension is
   ! decomposed over 4 DEs, the second over 3 DEs, and the third is 
   ! not distributed.  The connectivities in each dimension default 
   ! to aperiodic since they are not yet implemented. In this call 
   ! the minIndex hasn't been set, so it defaults to (1,1,...). 
   !-------------------------------------------------------------------
   grid3D=ESMF_GridCreateShapeTile( &
            ! Define an irregular distribution
            countsPerDEDim1=(/45,75,40,20/), &
            countsPerDEDim2=(/30,40,20/),    &
            countsPerDEDim3=(/40/),          &
            ! Specify mapping of coords dim to Grid dim
            coordDep1=(/1/), & ! 1st coord is 1D and depends on 1st Grid dim
            coordDep2=(/2/), & ! 2nd coord is 1D and depends on 2nd Grid dim
            coordDep3=(/3/), & ! 3rd coord is 1D and depends on 3rd Grid dim
            indexflag=ESMF_INDEX_GLOBAL,     & ! Use global indices
            rc=rc)

   !-------------------------------------------------------------------
   ! Allocate coordinate storage for both center and corner stagger
   ! locations.  Since no coordinate values are specified in this
   ! call no coordinate values are set yet.
   !-------------------------------------------------------------------
   call ESMF_GridAddCoord(grid3D, &
          staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, rc=rc)
   call ESMF_GridAddCoord(grid3D, &
          staggerloc=ESMF_STAGGERLOC_CORNER_VCENTER, rc=rc)


   !-------------------------------------------------------------------
   ! Get the number of DEs on this PET, so that the program
   ! can loop over them when accessing data.
   !-------------------------------------------------------------------
   call ESMF_GridGet(grid3D, localDECount=localDECount, rc=rc)

   !-------------------------------------------------------------------
   ! Loop over each localDE when accessing data
   !-------------------------------------------------------------------
   do lDE=0,localDECount-1

    !------------------------------------------------------------------
    ! Fill in the coordinates for the corner stagger location first.
    !------------------------------------------------------------------
      !----------------------------------------------------------------
      ! Get the local bounds of the global indexing for the first
      ! coordinate array on the local DE. If the number of PETs
      ! is less than the total number of DEs then the rest of this
      ! example would be in a loop over the local DEs.  Also get the
      ! pointer to the first coordinate array. 
      !----------------------------------------------------------------
      call ESMF_GridGetCoord(grid3D, coordDim=1, localDE=lDE, &
             staggerLoc=ESMF_STAGGERLOC_CORNER_VCENTER,       &
             computationalLBound=lbnd_corner,                 &
             computationalUBound=ubnd_corner,                 &
             fptr=cornerX, rc=rc)

      !----------------------------------------------------------------
      ! Calculate and set coordinates in the first dimension.
      !----------------------------------------------------------------
      do i=lbnd_corner(1),ubnd_corner(1)
         cornerX(i) = (i-1)*(360.0/180.0)
      enddo

      !----------------------------------------------------------------
      ! Get the local bounds of the global indexing for the second
      ! coordinate array on the local DE.  Also get the pointer to the
      ! second coordinate array.
      !----------------------------------------------------------------
      call ESMF_GridGetCoord(grid3D, coordDim=2, localDE=lDE,   &
             staggerLoc=ESMF_STAGGERLOC_CORNER_VCENTER,         &
             computationalLBound=lbnd_corner,                   &
             computationalUBound=ubnd_corner,                   &
             fptr=cornerY, rc=rc)

      !----------------------------------------------------------------
      ! Calculate and set coordinates in the second dimension.
      !----------------------------------------------------------------
      do j=lbnd_corner(1),ubnd_corner(1)
         cornerY(j) = (j-1)*(180.0/90.0)
      enddo

      !----------------------------------------------------------------
      ! Get the local bounds of the global indexing for the third
      ! coordinate array on the local DE, and the pointer to the array.
      !----------------------------------------------------------------
      call ESMF_GridGetCoord(grid3D, coordDim=3, localDE=lDE,   &
             staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER,         &
             computationalLBound=lbnd, computationalUBound=ubnd,&
             fptr=cornerZ, rc=rc)

      !----------------------------------------------------------------
      ! Calculate and set the vertical coordinates
      !----------------------------------------------------------------
      do k=lbnd(1),ubnd(1)
         cornerZ(k) = 4000.0*( (1./39.)*(k-1)  )**2
      enddo

    !------------------------------------------------------------------
    ! Now fill the coordinates for the center stagger location with
    ! the average of the corner coordinate location values.
    !------------------------------------------------------------------
      !----------------------------------------------------------------
      ! Get the local bounds of the global indexing for the first 
      ! coordinate array on the local DE, and the pointer to the array.
      !----------------------------------------------------------------
      call ESMF_GridGetCoord(grid3D, coordDim=1, localDE=lDE,    &
             staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER,          &
             computationalLBound=lbnd, computationalUBound=ubnd, &
             fptr=centerX, rc=rc)

      !----------------------------------------------------------------
      ! Calculate and set coordinates in the first dimension.
      !----------------------------------------------------------------
      do i=lbnd(1),ubnd(1)
         centerX(i) = 0.5*(i-1 + i)*(360.0/180.0) 
      enddo

      !----------------------------------------------------------------
      ! Get the local bounds of the global indexing for the second
      ! coordinate array on the local DE, and the pointer to the array.
      !----------------------------------------------------------------
       call ESMF_GridGetCoord(grid3D, coordDim=2, localDE=lDE,    &
              staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER,          &
              computationalLBound=lbnd, computationalUBound=ubnd, &
              fptr=centerY, rc=rc)

      !----------------------------------------------------------------
      ! Calculate and set coordinates in the second dimension.
      !----------------------------------------------------------------
      do j=lbnd(1),ubnd(1)
         centerY(j) = 0.5*(j-1 + j)*(180.0/90.0) 
      enddo

      !----------------------------------------------------------------
      ! Get the local bounds of the global indexing for the third
      ! coordinate array on the local DE, and the pointer to the array.
      !----------------------------------------------------------------
      call ESMF_GridGetCoord(grid3D, coordDim=3, localDE=lDE,   &
             staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER,         &
             computationalLBound=lbnd, computationalUBound=ubnd,&
             fptr=centerZ, rc=rc)

      !----------------------------------------------------------------
      ! Calculate and set the vertical coordinates
      !----------------------------------------------------------------
      do k=lbnd(1),ubnd(1)
         centerZ(k) = 4000.0*( (1./39.)*(k-1)  )**2
      enddo

   !-------------------------------------------------------------------
   ! End of loop over DEs
   !-------------------------------------------------------------------
   enddo
 

!EOC
   !-------------------------------------------------------------------
   ! Clean up to prepare for the next example.
   !-------------------------------------------------------------------
   call ESMF_GridDestroy(grid3D, rc=rc)


!BOE
!\subsubsection{Creating an Empty Grid in a Parent Component 
! for Completion in a Child Component}\label{sec:usage:setcommit}
!
! ESMF Grids can be created incrementally. To do this,
! the user first calls {\tt ESMF\_GridCreateEmpty()} to allocate the shell of
! a Grid. Next, we use the {\tt ESMF\_GridSetCommitShapeTile()}
! call that fills in the Grid and does an internal commit to make it usable.
! The following example uses this incremental technique to create a rectangular 
! 10x20 Grid with coordinates at the center and corner stagger locations. 
!EOE

!BOC
!---------------------------------------------------------------------------
! IN THE PARENT COMPONENT:
! Create an empty Grid in the parent component for use in a child component.
! The parent may be defined on more PETs than the child component.  
! The child's [vm or pet list] is passed into the create call so that
! the Grid is defined on the appropriate subset of the parent's PETs. 
!---------------------------------------------------------------------------
   grid2D=ESMF_GridCreateEmpty(rc=rc)

!---------------------------------------------------------------------------
! IN THE CHILD COMPONENT:
! Set the Grid topology.  Here we define an irregularly distributed 
! rectangular Grid.
!---------------------------------------------------------------------------

   call ESMF_GridSetCommitShapeTile(grid2D,             &
                          countsPerDEDim1=(/6,4/),      &
                          countsPerDEDim2=(/10,3,7/), rc=rc)

!---------------------------------------------------------------------------
! Add Grid coordinates at the cell center location.
!---------------------------------------------------------------------------
   call ESMF_GridAddCoord(grid2D, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)

!---------------------------------------------------------------------------
! Add Grid coordinates at the corner stagger location.
!---------------------------------------------------------------------------
   call ESMF_GridAddCoord(grid2D, staggerLoc=ESMF_STAGGERLOC_CORNER, rc=rc)

!EOC
   !-------------------------------------------------------------------
   ! Clean up to prepare for the next example.
   !-------------------------------------------------------------------
   call ESMF_GridDestroy(grid2D, rc=rc)



#ifdef LOCAL_NOT_IMPL
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
   grid=ESMF_GridCreateShapeTile(coordTypeKind=ESMF_TYPEKIND_I4, &
          countsPerDEDim1=(/5,5/), countsPerDEDim2=(/7,7,6/), rc=rc)   
!EOCI
#endif

!BOE
!\subsubsection{Associating Coordinates with Stagger Locations}
!\label{sec:usage:staggerloc}
!
! A useful finite difference technique is to place different physical 
! quantities at different locations within a grid cell. This
! {\em {staggering}} of the physical variables on the mesh is introduced so
! that the difference of a field is naturally defined at the location of 
! another variable. This method was first formalized by Mesinger and Arakawa 
! (1976).
!
! To support the staggering of variables, the Grid provides
! the idea of {\em stagger locations}. 
! Stagger locations refer to the places in a Grid cell that 
! contain coordinates and, once a Grid is associated with a 
! Field object, field data. Typically data can be located
! at the cell center, at the cell corners, or at the cell faces, in 2D, 3D, and
! higher dimensions. (Note that any Arakawa stagger can be constructed
! of a set of Grid stagger locations.) Users can put coordinates, 
! which are necessary
! for operations such as regrid, at multiple stagger
! locations in a Grid. In addition, the user can put Field data
! at any of the stagger locations in a Grid.  
!
! By default the coordinate array at the center stagger location
! starts at the bottom index of the Grid (default (1,1..,1)) and extends
! up to the maximum cell index in the Grid (e.g. given by the {\tt maxIndex} argument).
! Other stagger locations also start at the bottom index of the Grid, however, 
! they can extend to +1 element beyond the center in some dimensions to allow
! for the extra space to surround the center elements. See Section~\ref{sec:usage:staggerloc:adv}
! for a description of this extra space and how to adjust if it necessary. 
! The subroutine {\tt ESMF\_GridGetCoord} can be used
! to retrieve the stagger bounds for the piece of a coordinate
! array on a particular DE. 
!
! The user can allocate coordinate arrays without setting coordinates 
! using the {\tt ESMF\_AddCoord()} call, or allocate and 
! set coordinates in a single call with {\tt ESMF\_SetCoord()}.    
! When adding or accessing
! coordinate data, the stagger location is specified to tell the Grid method 
! where in the cell to get the data. There are predefined stagger locations
! (see Section~\ref{sec:opt:staggerloc}), or,
! should the user wish to specify their own, there
! is also a set of methods for generating custom locations 
! (See Section~\ref{sec:usage:staggerloc:adv}).
!
! The following example
! adds coordinate storage to the corner stagger location in a grid using 
! one of the predefined stagger locations.
!EOE


!!!!!!!!!!!!!!!!!!!!!!
! Setup For Example
!!!!!!!!!!!!!!!!!!!!!!
   distgrid2D=ESMF_DistGridCreate(minIndex=(/1,1/),maxIndex=(/10,10/), rc=rc)
   grid2D=ESMF_GridCreate(distgrid=distgrid2D, rc=rc)


!BOC 
   call ESMF_GridAddCoord(grid2D, staggerLoc=ESMF_STAGGERLOC_CORNER, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!!!!!!!!!!!!!!!!!!!!!!!
! Cleanup after Example
!!!!!!!!!!!!!!!!!!!!!!
   call ESMF_GridDestroy(grid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

   call ESMF_DistGridDestroy(distgrid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


#ifdef LOCAL_NOT_IMPL
!BOEI
! The user may also set the coordinate data at the same time as adding
! the coordinate storage.  The following example
! adds coordinate storage to the corner stagger location in {\tt grid}, plus sets
! the associated coordinate data. Note, the input coordinate arrays (CoordX, CoordY)
! may be either F90 or ESMF Array's (The F90 arrays are restricted to single DE to
! PET mappings). They also need to be of the proper size and dimension to correspond to the Grid. 
!EOEI

!BOCI 
   call ESMF_GridSetCoord(grid, staggerLoc=ESMF_STAGGER_CORNER, &
          coord1=CoordX, coord2=CoordY, rc=rc)
!EOCI
#endif


!BOE
!
!\subsubsection{Specifying the Relationship of Coordinate Arrays
!               to Index Space Dimensions}
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
! then {\tt coordDep1} defaults to {/1,2..,gridDimCount/}.  This default
! thus specifies a curvilinear grid.  
!
!
! The following call demonstrates the creation of a
! 10x20 2D rectilinear grid where the first coordinate
! component is mapped to the second index dimension
! (i.e. is of size 20) and the second coordinate component
! is mapped to the first index dimension (i.e. is of size
! 10).
!EOE

!BOC
   grid2D=ESMF_GridCreateShapeTile(countsPerDEDim1=(/5,5/), &
          countsPerDEDim2=(/7,7,6/),                    &
          coordDep1=(/2/),                              &
          coordDep2=(/1/), rc=rc)   
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!!!!!!!!!!!!!!!!!!!!!!!
! Cleanup after Example
!!!!!!!!!!!!!!!!!!!!!!
call ESMF_GridDestroy(grid2D,rc=rc)

!BOE
! The following call demonstrates the creation of a
! 10x20x30 2D plus 1 curvilinear grid where 
! coordinate component 1 and 2 are still 10x20, but
! coordinate component 3 is mapped just to the 
! third index dimension.
!EOE

!BOC
   grid2D=ESMF_GridCreateShapeTile(countsPerDEDim1=(/6,4/), &
          countsPerDEDim2=(/10,7,3/), countsPerDEDim3=(/30/), &
          coordDep1=(/1,2/), coordDep2=(/1,2/), &
          coordDep3=(/3/), rc=rc)   
!EOC 

!!!!!!!!!!!!!!!!!!!!!!!
! Cleanup after Example
!!!!!!!!!!!!!!!!!!!!!!
call ESMF_GridDestroy(grid2D,rc=rc)

!BOE
! By default the local piece of the array on each PET starts at 
! (1,1,..), however, the indexing for each grid coordinate array  
! on each DE may be shifted to the global indices by using the {\tt indexflag}.
! For example, the following call switches the grid to use global indices. 
!EOE

!BOC
   grid2D=ESMF_GridCreateShapeTile(countsPerDEDim1=(/6,4/), &
           countsPerDEDim2=(/10,7,3/), indexflag=ESMF_INDEX_GLOBAL, rc=rc)   
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!!!!!!!!!!!!!!!!!!!!!!!
! Cleanup after Example
!!!!!!!!!!!!!!!!!!!!!!
call ESMF_GridDestroy(grid2D,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!BOE
!\subsubsection{Accessing Grid Coordinates}
!
! Once a Grid has been created, the user has several options to access
! the Grid coordinate data. The first of these, {\tt ESMF\_GridSetCoord()}, 
! enables the user to set data 
! for one stagger location across the whole Grid, using ESMF Arrays. 
! For example, the following sets the coordinates in the first dimension 
! (e.g. x) for the center stagger location to 
! those in the ESMF Array {\tt arrayCoordX}.
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
          staggerLoc=ESMF_STAGGERLOC_CORNER, &
          coordDim=1, array=arrayCoordX, rc=rc)
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
! component of the corner and puts it into the ESMF Array {\tt arrayCoordY}. 
!EOE

!!!!!!!!!!!!!!!!!!!!!!!
! Setup For Example
!!!!!!!!!!!!!!!!!!!!!!
   distgrid2D=ESMF_DistGridCreate(minIndex=(/1,1/),maxIndex=(/10,10/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
   grid2D=ESMF_GridCreate(distgrid=distgrid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
   call ESMF_GridAddCoord(grid2D,&
          staggerLoc=ESMF_STAGGERLOC_CORNER, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!BOC
   call ESMF_GridGetCoord(grid2D,    &
          staggerLoc=ESMF_STAGGERLOC_CORNER,    &
          coordDim=2,                           &
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



!BOE
! Alternatively, the call {\tt ESMF\_GridGetCoord()} gets a Fortran pointer to 
! the coordinate data. The user can then operate on this array in the usual
! manner. The following call gets a reference to the
! Fortran array which holds the data for the second coordinate (e.g. y). 
!EOE

!!!!!!!!!!!!!!!!!!!!!!!
! Setup For Example
!!!!!!!!!!!!!!!!!!!!!!
   distgrid2D=ESMF_DistGridCreate(minIndex=(/1,1/),maxIndex=(/10,10/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
   grid2D=ESMF_GridCreate(distgrid=distgrid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
   call ESMF_GridAddCoord(grid2D,&
          staggerLoc=ESMF_STAGGERLOC_CORNER, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

! Don't run without correct number of DEs
  call ESMF_GridGet(grid2D, localDECount=localDECount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  if (localDECount .gt. 0) then
!BOC
   call ESMF_GridGetCoord(grid2D, coordDim=2, localDE=0, &
          staggerloc=ESMF_STAGGERLOC_CORNER, fptr=coordY2D, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  endif

!!!!!!!!!!!!!!!!!!!!!!!
! Cleanup after Example
!!!!!!!!!!!!!!!!!!!!!!
   call ESMF_GridDestroy(grid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_DistGridDestroy(distgrid2D, rc=rc)
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)



!BOE
!\subsubsection{Grid Regions and Bounds}
!\label{sec:grid:usage:bounds}
!
! The index space of the Grid contains three regions
! with associated bounds. This section describes these
! regions and bounds in general. The next couple of 
! sections describe getting the bound information
! for specific types of Grid information. 
!
! The exclusive region is the index space defined by the 
! distgrid of the Grid. These correspond to the
! maximum index space usable by any stagger location in the Grid.
! The size of the exclusive region is the index space for the 
! Grid cells, plus the stagger padding. 
!
! The default stagger padding depends on the topology of the Grid. 
! For an unconnected dimension the stagger padding is a width
! of 1 on the upper side (i.e. {\tt gridEdgeUWidth=(1,1,1,1...)}).
! For a periodic dimension there is no stagger padding.
! By adjusting {\tt gridEdgeLWidth} and {\tt gridEdgeUWidth}, the 
! user can set the stagger padding for the whole Grid and
! thus the exclusive region can be adjusted at will around the 
! index space corresponding to the cells. The user can
! also use {\tt staggerEdgeLWidth} and {\tt staggerEdgeUWidth} to
! adjust individual stagger location padding within the
! Grid's padding (Please see Section~\ref{sec:usage:staggerpadding:adv} for
! further discussion of customizing the stagger padding).
!
!\begin{center}
!\begin{figure}
!\center
!\scalebox{0.75}{\includegraphics{GridExclusiveReg}}
!\caption{An example of a Grid's exclusive region.}
!\label{fig:gridexreg}
!\end{figure}
!\end{center}
!
! Figure~\ref{fig:gridexreg} shows an example of a Grid exclusive region (with default
! stagger padding). This exclusive region would be for a Grid generated by either of the
! following calls:
!EOE

!BOC
  grid2D=ESMF_GridCreateShapeTile(regDecomp=(/2,4/), maxIndex=(/5,15/), &
           indexflag=ESMF_INDEX_GLOBAL, rc=rc)   
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
   !-------------------------------------------------------------------
   ! Clean up to prepare for the next example.
   !-------------------------------------------------------------------
  call ESMF_GridDestroy(grid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOC
  grid2D=ESMF_GridCreateShapeTile(countsPerDEDim1=(/4,4,4,3/), &
           countsPerDEDim2=(/3,2/), indexflag=ESMF_INDEX_GLOBAL, rc=rc)   
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
   !-------------------------------------------------------------------
   ! Clean up to prepare for the next example.
   !-------------------------------------------------------------------
  call ESMF_GridDestroy(grid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!BOE
! Each rectangle in this diagram represents a DE and the numbers along the sides
! are the index values of the locations in the DE. Note that the exclusive region 
! has one extra index location in each dimension than the number of cells
! because of the padding for the larger stagger locations (e.g. corner). 
!
! The Grid computational region is a subset of the Grid exclusive
! region.  The computational region indicates which index locations in 
! the Grid are active for a particular stagger. The computational region
! is typically the region that a user would compute over (e.g.
! would iterate over setting values for).
! 
!\begin{center}
!\begin{figure}
!\center
!\scalebox{0.75}{\includegraphics{GridCompCenterReg}}
!\caption{An example of a Grid's computational region for the center stagger location.}
!\label{fig:gridcompcntrreg}
!\end{figure}
!\end{center}
!
! Figure~\ref{fig:gridcompcntrreg} shows an example of a Grid computational region.
! This example is for the same Grid used for the exclusive region figure above (Figure~\ref{fig:gridexreg}).
! The computational region is the interior blue area. This example is 
! for the center stagger location. This would be the computational region
! for a stagger location allocated by the following call:
!EOE

   !-------------------------------------------------------------------
   ! Setup for the to prepare for the example.
   !-------------------------------------------------------------------
  grid2D=ESMF_GridCreateShapeTile(countsPerDEDim1=(/4,4,4,3/), countsPerDEDim2=(/3,2/), &
           indexflag=ESMF_INDEX_GLOBAL, rc=rc)   

!BOC
  call ESMF_GridAddCoord(grid2D, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

   !-------------------------------------------------------------------
   ! Clean up to prepare for the next example.
   !-------------------------------------------------------------------
  call ESMF_GridDestroy(grid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
!\begin{center}
!\begin{figure}
!\center
!\scalebox{0.75}{\includegraphics{GridCompEdgeReg}}
!\caption{An example of a Grid's computational region for the edge1 stagger location.}
!\label{fig:gridcompedgereg}
!\end{figure}
!\end{center}
!
! Figure~\ref{fig:gridcompedgereg} shows another example of a Grid computational region.
! This example is for the same Grid used for the exclusive region figure above (Figure~\ref{fig:gridexreg}).
! The computational region is the interior blue area. This example is
! for the edge1 stagger location. This would be the computational region
! for a stagger location allocated by the following call:
!EOE

   !-------------------------------------------------------------------
   ! Setup for the to prepare for the example.
   !-------------------------------------------------------------------
  grid2D=ESMF_GridCreateShapeTile(countsPerDEDim1=(/4,4,4,3/), countsPerDEDim2=(/3,2/), &
           indexflag=ESMF_INDEX_GLOBAL, rc=rc)   

!BOC
  call ESMF_GridAddCoord(grid2D, staggerloc=ESMF_STAGGERLOC_EDGE1, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

   !-------------------------------------------------------------------
   ! Clean up to prepare for the next example.
   !-------------------------------------------------------------------
  call ESMF_GridDestroy(grid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
! Again each rectangle in these diagrams represents a DE and the numbers along the sides
! are the index values of the locations in the DE.
!
! The total region is the outermost boundary of the memory allocated 
! on each DE to hold the data for the Grid on that DE. This region 
! can be as small as the computational region, but may be larger to 
! include space for halos, memory padding, etc. The total region is
! what is enlarged to include space for halos, and the total region 
! must be large enough to contain the maximum halo operation on the
! Grid. The total region is a property of an underlying Array and
! so information about it is only retrievable when an Array has
! been set or allocated. 
! 
!\begin{center}
!\begin{figure}
!\center
!\scalebox{0.75}{\includegraphics{GridTotalCenterReg}}
!\caption{An example of a Grid's total region for the center stagger location.}
!\label{fig:gridtotcntrreg}
!\end{figure}
!\end{center}
!
! Figure~\ref{fig:gridtotcntrreg} shows an example of a Grid total region.
! This example is for the same Grid used for the exclusive region figure above (Figure~\ref{fig:gridexreg}).
! The total region is the outer red area. This region is 
! for the center stagger location. Total width adjustment of a stagger location allocation 
! is not currently implement, but when it is, this would be the total region
! for a stagger location allocated by the following call:
!EOE

   !-------------------------------------------------------------------
   ! Setup for the to prepare for the example.
   !-------------------------------------------------------------------
  grid2D=ESMF_GridCreateShapeTile(countsPerDEDim1=(/4,4,4,3/), countsPerDEDim2=(/3,2/), &
           indexflag=ESMF_INDEX_GLOBAL, rc=rc)   

!BOC
  call ESMF_GridAddCoord(grid2D, staggerloc=ESMF_STAGGERLOC_CENTER, &
!        totalLWidth=(/1,1/), totalUWidth=(/1,1/), & ! NOT YET IMPLEMENTED
         rc=rc) 
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

   !-------------------------------------------------------------------
   ! Clean up to prepare for the next example.
   !-------------------------------------------------------------------
  call ESMF_GridDestroy(grid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
!\begin{center}
!\begin{figure}
!\center
!\scalebox{0.75}{\includegraphics{GridTotalEdgeReg}}
!\caption{An example of a Grid's total region for the edge1 stagger location.}
!\label{fig:gridtotedgereg}
!\end{figure}
!\end{center}
!
! Figure~\ref{fig:gridtotedgereg} shows an example of a Grid total region.
! This example is for the same Grid used for the exclusive region figure above (Figure~\ref{fig:gridexreg}).
! The total region is the outer red area. This region is 
! for the edge1 stagger location. Total width adjustment of a stagger location allocation 
! is not currently implement, but when it is, this would be the total region
! for a stagger location allocated by the following call:
!EOE

   !-------------------------------------------------------------------
   ! Setup for the to prepare for the example.
   !-------------------------------------------------------------------
  grid2D=ESMF_GridCreateShapeTile(countsPerDEDim1=(/4,4,4,3/), countsPerDEDim2=(/3,2/), &
           indexflag=ESMF_INDEX_GLOBAL, rc=rc)   

!BOC
  call ESMF_GridAddCoord(grid2D, staggerloc=ESMF_STAGGERLOC_CENTER, &
!        totalLWidth=(/1,1/), totalUWidth=(/1,1/), & ! NOT YET IMPLEMENTED
         rc=rc) 
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

   !-------------------------------------------------------------------
   ! Clean up to prepare for the next example.
   !-------------------------------------------------------------------
  call ESMF_GridDestroy(grid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
! Again each rectangle in these diagrams represents a DE and the numbers along the sides
! are the index values of the locations in the DE.
!
! The user can retrieve a set of bounds for each index space region 
! described above: exclusive bounds, computational bounds, 
! and total bounds. Note that although some of these are similar
! to bounds provided by ESMF\_Array subroutines 
! (see Section~\ref{Array_regions_and_default_bounds}) 
! the format here is different. The Array bounds are only for
! distributed dimensions and are ordered to correspond 
! to the dimension order in the associated DistGrid. The bounds
! provided by the Grid are ordered according to the order of dimensions of the data
! in question. This means that the bounds provided should be usable
! "as is" to access the data. 
!
! Each of the three types of bounds refers to the maximum and minimum
! per dimension of the index ranges of a particular region. The paramters
! referring to the maximums contain a 'U' for upper. The parameters referring 
! to the minimums contain an 'L' for lower. The bounds and associated
! quantities are almost always given on a per DE basis. The three types of
! bounds exclusiveBounds, computationalBounds, and totalBounds refer
! to the ranges of the exlusive region, the computational region, and the
! total region. Each of these bounds also has a corresponding count parameter
! which gives the number of items across that region (on a DE) in each dimension.
! (e.g. totalCount(d)=totallUBound(i)-totalLBound(i)+1). Width parameters
! give the spacing between two different types of region. The
! {\tt computationalWidth} argument gives the spacing between the exclusive
! region and the computational region. The {\tt totalWidth} argument gives the 
! spacing between the total region and the computational region. Like the 
! other bound information these are typically on a per DE basis, for example
! specifying {\tt totalLWidth=(1,1)} makes the bottom of the total
! region one lower in each dimension than the computational region on
! each DE. The exceptions to the per DE rule are
! {\tt computationalEdgeWidth}, {\tt staggerEdgeWidth}, and {\tt gridEdgeWidth}
! which give the spacing only on the DEs along the boundary of the Grid.
!EOE

!BOE
!\subsubsection{Getting Grid Coordinate Bounds}
!
! When operating on coordinates the user may often wish to 
! retrieve the bounds of the piece of coordinate data on
! a particular local DE. This is useful for iterating through the
! data to set coordinates, retrieve coordinates, or do calculations. 
! The method {\tt ESMF\_GridGetCoord} allows the user
! to retrieve bound information for a particular coordinate
! array. 
!
! As described in the previous section there are three types of bounds the user can 
! get: exclusive bounds, computational bounds, 
! and total bounds. The exclusive and computational bounds may 
! be retrieved from an unallocated stagger location, but
! the total bounds may only be retrieved from a stagger location 
! which contains a valid coordinate Array. The bounds
! provided by {\tt ESMF\_GridGetCoord} are for both distributed
! and undistributed dimensions and are ordered according to the
! order of dimensions in the  coordinate. This means that the bounds
!  provided should be usable
! "as is" to access data in the coordinate array. In the case
! of factorized coordinate Arrays where a coordinate may
! have a smaller dimension than its associated Grid, then
! the dimension of the coordinate's bounds are the dimension of
! the coordinate, not the Grid. 
!
! The following is an example of retrieving the bounds for localDE 0 for the first
! coordinate array from the corner stagger location.  
!EOE

!!!!!!!!!!!!!!!!!!!!!!!
! Setup For Example
!!!!!!!!!!!!!!!!!!!!!!
   distgrid2D=ESMF_DistGridCreate(minIndex=(/1,1/),maxIndex=(/10,10/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
   grid2D=ESMF_GridCreate(distgrid=distgrid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_GridAddCoord(grid2D, staggerloc=ESMF_STAGGERLOC_CORNER, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

! Don't run without correct number of DEs
  call ESMF_GridGet(grid2D, localDECount=localDECount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  if (localDECount .gt. 0) then 
!BOC
   call ESMF_GridGetCoord(grid2D, coordDim=1, localDE=0,  &
          staggerLoc=ESMF_STAGGERLOC_CORNER,                         &
          exclusiveLBound=elbnd, exclusiveUBound=eubnd,              &
          computationalLBound=clbnd, computationalUBound=cubnd,      & 
          totalLBound=tlbnd, totalUBound=tubnd, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
endif

!!!!!!!!!!!!!!!!!!!!!!!
! Cleanup after Example
!!!!!!!!!!!!!!!!!!!!!!
   call ESMF_GridDestroy(grid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
   call ESMF_DistGridDestroy(distgrid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)



!BOE
!\subsubsection{Getting Grid Stagger Location Bounds}
!
! When operating on data stored at a particular stagger
! in a Grid the user may find it useful to be able
! to retrieve the bounds of the data on a particular local DE. 
! This is useful for iterating through the
! data for computations or allocating arrays to hold the data. 
! The method {\tt ESMF\_GridGet} allows the user
! to retrieve bound information for a particular stagger location. 
!
! As described in Section~\ref{sec:grid:usage:bounds} there are three types of bounds 
! the user can typically get, however, the Grid doesn't hold data at
! a stagger location (that is the job of the Field), and so 
! no Array is contained there and so no total region exists, so the 
! user may only retrieve exclusive and computational bounds from
! a stagger location.  The bounds
! provided by {\tt ESMF\_GridGet} are ordered according to the
! order of dimensions in the Grid. 
!
! The following is an example of retrieving the bounds for localDE 0
! from the corner stagger location. 
!EOE

!!!!!!!!!!!!!!!!!!!!!!!
! Setup For Example
!!!!!!!!!!!!!!!!!!!!!!
   distgrid2D=ESMF_DistGridCreate(minIndex=(/1,1/),maxIndex=(/10,10/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
   grid2D=ESMF_GridCreate(distgrid=distgrid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_GridAddCoord(grid2D, staggerloc=ESMF_STAGGERLOC_CORNER, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

! Don't run without correct number of DEs
  call ESMF_GridGet(grid2D, localDECount=localDECount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  if (localDECount .gt. 0) then 
!BOC
   call ESMF_GridGet(grid2D, localDE=0,                         &
          staggerLoc=ESMF_STAGGERLOC_CORNER,                    &
          exclusiveLBound=elbnd, exclusiveUBound=eubnd,         &
          computationalLBound=clbnd, computationalUBound=cubnd, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
 endif

!!!!!!!!!!!!!!!!!!!!!!!
! Cleanup after Example
!!!!!!!!!!!!!!!!!!!!!!
   call ESMF_GridDestroy(grid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
   call ESMF_DistGridDestroy(distgrid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)



!BOE
!\subsubsection{Getting Grid Stagger Location Information}
!
! In addition to the per DE information that can be accessed about
! a stagger location there is some global information that can
! accessed by using {\tt ESMF\_GridGet} without specifying a
! localDE. One of the main uses of this information is to create
! an ESMF Array to hold data for a stagger location. 
!
! The information currently available from a stagger
! location is {\tt computationalEdgeLWidth}  and
! {\tt computationalEdgeUWidth} these give the difference between
! the lower and upper boundary of the exclusive region and the computational 
! region.
!
! The following is an example of retrieving information for localDE 0
! from the corner stagger location. 
!EOE


!!!!!!!!!!!!!!!!!!!!!!!
! Setup For Example
!!!!!!!!!!!!!!!!!!!!!!
   distgrid2D=ESMF_DistGridCreate(minIndex=(/1,1/),maxIndex=(/10,10/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
   grid2D=ESMF_GridCreate(distgrid=distgrid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_GridAddCoord(grid2D, staggerloc=ESMF_STAGGERLOC_CORNER, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOC
    ! Get info about staggerloc
    call ESMF_GridGet(grid2D, staggerLoc=ESMF_STAGGERLOC_CORNER,  &
           computationalEdgeLWidth=celwdth, computationalEdgeUWidth=ceuwdth, &
           rc=rc)

!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!!!!!!!!!!!!!!!!!!!!!!!
! Cleanup after Example
!!!!!!!!!!!!!!!!!!!!!!
   call ESMF_GridDestroy(grid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
   call ESMF_DistGridDestroy(distgrid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)



!BOE
!\subsubsection{Creating an Array at a Stagger Location}
!
! In order to create an Array to correspond to a Grid stagger location
! several pieces of information need to be obtained from both the 
! Grid and the stagger location in the Grid. 
!
! The information that needs to be obtained from the Grid
! is the distgrid and distgridToGridMap to ensure that the new Array
! has the correct size and distribution and that its 
! dimensions are mapped correctly to the Grid. These
! are obtained using the {\tt ESMF\_GridGet} method. 
!
! The information that needs to be obtained from the stagger
! location are the offsets from the edges of the 
! exclusive region of the distgrid. These may be obtained
! from {\tt ESMF\_GridGet} by passing in the 
! stagger location and the arguments {\tt computationalEdgeLWidth}  and
! {\tt computationalEdgeUWidth}. 
!
! The following is an example of using information from the Grid
! to create an Array corresponding to a stagger location.
!
!EOE

!!!!!!!!!!!!!!!!!!!!!!!
! Setup For Example
!!!!!!!!!!!!!!!!!!!!!!
   distgrid2D=ESMF_DistGridCreate(minIndex=(/1,1/),maxIndex=(/10,10/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
   grid2D=ESMF_GridCreate(distgrid=distgrid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_GridAddCoord(grid2D, staggerloc=ESMF_STAGGERLOC_CORNER, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
 
!BOC

    ! Get info from Grid
    call ESMF_GridGet(grid2D, distgrid=distgrid, distgridToGridMap=distgridToGridMap, rc=rc)

    ! Get info about staggerloc
    call ESMF_GridGet(grid2D, staggerLoc=ESMF_STAGGERLOC_CORNER, &
           computationalEdgeLWidth=celwdth, computationalEdgeUWidth=ceuwdth, &
           rc=rc)

    ! construct ArraySpec
    call ESMF_ArraySpecSet(arrayspec, rank=2, typekind=ESMF_TYPEKIND_R8, rc=rc)

    ! Create an Array based on the presence of distributed dimensions
    array=ESMF_ArrayCreate(arrayspec=arrayspec, &
            distgrid=distgrid, distgridToArrayMap=distgridToGridMap, &
            computationalEdgeLWidth=celwdth, &
            computationalEdgeUWidth=ceuwdth, &
            rc=rc)

!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!!!!!!!!!!!!!!!!!!!!!!!
! Cleanup after Example
!!!!!!!!!!!!!!!!!!!!!!
   call ESMF_GridDestroy(grid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
   call ESMF_DistGridDestroy(distgrid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


#ifdef LOCAL_NOT_IMPL
!BOEI
! The second pair of methods enable the user to set or get data using
! a Fortran pointer. These methods only work with the local piece of the 
! Grid on the DE. {\tt ESMF\_GridSetCoord} enables the user
! to set data into the local piece of the coordinates residing on the DE.
! The following call gets a pointer (fptr) to the Fortran array holding the 
! first component (e.g. x) coordinates for the corner stagger
! for the piece of tile 2 which is on this PET. It
! defaults to the first DE because it isn't specified. 
!EOEI

!BOCI
   call ESMF_GridSetCoord(grid3D, tile=2,  &
          staggerLoc=ESMF_STAGGERLOC_CORNER,        &
          coordDim=1, fptr, doCopy=ESMF_DATA_REF, rc=rc)
!EOCI
#endif


#ifdef LOCAL_NOT_IMPL
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
! The method {\tt ESMF\_GridGetCoord}, allows for a uniform 
! method of retrieving coordinates from the Grid no matter what the factorization of the 
! arrays. The following retrieves the coordinates (outCoords) from 
! point (1,1,1) in the center stagger location.  
!EOEI

!BOCI
   call ESMF_GridGetCoord(grid, &
          staggerLoc=ESMF_STAGGERLOC_CENTER, 
          indices=(/1,1,1/), coords=outCoords, rc=rc)
!EOCI
#endif

#ifdef LOCAL_NOT_IMPL
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

#ifdef LOCAL_NOT_IMPL
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

#ifdef LOCAL_NOT_IMPL
!BOEI
!\subsubsection{Grid Halo}
!
! The Grid halo operation allows users to update the
! halos (if present) of the Grid coordinate arrays to contain the coordinate values on the 
! neighboring PET. This would be convenient in cases like regrid
! in which having coordinate information for cells neighboring the PET
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

#ifdef LOCAL_NOT_IMPL
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
! out of a series of Fortran arrays. Note that this option is restricted
! to situations where there is a 1-to-1 DE to PET mapping. 
! The following call adds metric "Length" to the 
! grid at the edge stagger location. It does this from 
! Fortran array length.   
!removeEOE

!removeBOC
   call ESMF_GridAddMetricFromFptr(grid, "Length", &
          staggerLoc=ESMF_STAGGERLOC_EDGE1, &
          fptr=length, rc=rc)
!removeEOC
#endif

#ifdef LOCAL_NOT_IMPL
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
! a Fortran pointer. These methods only work with the local piece of the 
! Grid on the DE. {\tt ESMF\_GridLocalTileSetMetric} enables the user
! to set data into the local piece of the metric residing on the DE.
! The following call gets a pointer (fptr) to the Fortran array holding the 
! area for the piece of tile 2 which is on this PET. It
! defaults to the first DE because it isn't specified. 
!removeEOE

!removeBOC
   call ESMF_GridLocalTileSetMetric(grid, name="Area", tile=2, &
          fptr, doCopy=ESMF_DATA_REF, rc=rc)
!removeEOC

!removeBOE
! The call {\tt ESMF\_GridLocalTileGetMetric} gets a Fortran pointer to 
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

#ifdef LOCAL_NOT_IMPL
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

#ifdef LOCAL_NOT_IMPL
!BOEI
! \subsubsection{Creating a Regularly Distributed 3D Grid with Generated Coordinates}
!
! This example illustrates the creation of a 100x100x100  3D Grid distributed across
! 5 PETs in each dimension. The coordinates in the Grid are uniformly distributed
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
   grid=ESMF_GridCreateShapeTile(maxIndex=(/100,100,100/), regDecomp=(/5,5,5/), rc=rc)   

   ! Add a center stagger location 
   call ESMF_GridSetCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)

   ! Put in the coordinates
   call ESMF_GridGenCoordUni(grid, begCoord=(/0.0,0.0,0.0/), &
          endCoord=(/200.0, 200.0, 200.0/), rc=rc)
 
!EOCI
#endif

#ifdef LOCAL_NOT_IMPL
!removeBOE
! \subsubsection{Creating a Grid from Existing F90 Arrays}~\label{sec:example5}
!
! This example illustrates the creation of a simple 2D Grid from coordinate data
!  contained in 4 byte real fortan arrays.  The new Grid contains just the center stagger location.
!  Each PET contains a pair of 10x10 Fortran 90 arrays named fptrX and fptrY. 
!  These arrays contain the coordinates for the piece of the global Grid held by each
!  PET. The final global Grid will be 20x20 and the pieces of this Grid held
!  by each PET are as follows:
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
!   As illustrated by the diagram, the arrays on PET 1 hold piece (1,1)-(10,10) of the 
!   global index space. The arrays on PET 2 hold piece (11,1)-(20,10). The arrays on 
!   PET 3 hold piece (1,11)-(10,20), and the arrays on PET 4 hold piece (11,11)-(20,20).
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
   grid=ESMF_GridCreateShapeTile(coordTypeKind=ESMF_TYPEKIND_R4, &
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
   grid=ESMF_GridSetCommitShapeTile(coordTypeKind=ESMF_TYPEKIND_R4, rc=rc)


   ! Set the grid size and distribution
   ! Init petMap
   petMap(:,2,1) = (/3,4/)
   petMap(:,1,1) = (/1,2/)

   ! Set Grid 
   grid=ESMF_GridSetCommitShapeTile(countsPerDEDim1=(/10,10/), &
          countsPerDEDim2=(/10,10/), petMap=petMap, rc=rc)   

   ! Set the grid coordinate array index dependency
   grid=ESMF_GridSetCommitShapeTile(coordDep1=(/1,2/), coordDep2=(/1,2/), &
          rc=rc)   

  ! Create the grid and add a center stagger location and at the same time set {\tt grid} to 
  ! reference the coordinate arrays. 
  call ESMF_GridSetCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                  coord1=fptrX, coord2=fptrY, rc=rc)

!removeEOC
#endif


!BOE
!\subsubsection{Creating More Complex Grids Using DistGrid}
!\label{sec:usage:adv:create}
!
! Besides the shortcut methods for creating a Grid object such as
! {\tt ESMF\_GridCreateShapeTile()}, there is
! a set of methods which give the user more control over the
! specifics of the grid.  The following describes the more 
! general interface, using DistGrid.
! The basic idea is to first create an ESMF DistGrid object describing
! the distribution and shape of the Grid, and then to employ that to either directly
! create the Grid or first create Arrays and then create the Grid from those. 
! This method gives the user maximum control over the topology and distribution of the Grid. 
! See the DistGrid documentation in Section~\ref{sec:DistGrid} for an 
! in-depth description of its interface and use. 
!
! As an example, the following call constructs
! a 10x20 Grid with a lower bound of (1,2).
!EOE

!BOC 
   ! Create DistGrid
   distgrid2D = ESMF_DistGridCreate(minIndex=(/1,2/), maxIndex=(/11,22/), rc=rc)  

   ! Create Grid
   grid3D=ESMF_GridCreate(distGrid=distgrid2D, rc=rc)
!EOC  

!!!!!!!!!!!!!!!!!!!!!!!
! Cleanup after Example
!!!!!!!!!!!!!!!!!!!!!!!
   call ESMF_GridDestroy(grid3D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
   call ESMF_DistGridDestroy(distgrid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!BOE
! To alter which dimensions are distributed, the {\tt distgridToGridMap} 
! argument can be used. The {\tt distgridToGridMap} is used to set
! which dimensions of the Grid are mapped to the dimensions
! described by {\tt maxIndex}. In other words, it describes how the dimensions of 
! the underlying default DistGrid are mapped to the Grid. Each entry
! in {\tt distgridToGridMap} contains the Grid dimension to which the cooresponding
! DistGrid dimension should be mapped. 
! The following example illustrates the creation of a Grid where the largest
! dimension is first. To accomplish this the two dimensions are swapped. 
!EOE

!BOC 
   ! Create DistGrid
   distgrid2D = ESMF_DistGridCreate(minIndex=(/1,2/), maxIndex=(/11,22/), rc=rc)  

   ! Create Grid
   grid2D=ESMF_GridCreate(distGrid=distgrid2D, distgridToGridMap=(/2,1/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!!!!!!!!!!!!!!!!!!!!!!!
! Cleanup after Example
!!!!!!!!!!!!!!!!!!!!!!!
   call ESMF_GridDestroy(grid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
   call ESMF_DistGridDestroy(distgrid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


#ifdef LOCAL_NOT_IMPL
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
! The parameters {\tt coordDimCount} and {\tt coordDimMap}
! can be used to specify the factorization of the coordinate arrays. 
!
! The default Grid has coordinate arrays the same rank as the Grid.
! To alter this, the {\tt coordDimCount} parameter can be used
! to set the rank of the coordinate arrays.  The size of the parameter  
! is the dimension of the Grid. Each entry of {\tt coordDimCount}  is the rank of 
! the associated coordinate array. The following creates a 10x20
! 2D Grid where both the x and y coordinates are stored in 1D arrays. 
!EOEI

!BOCI 
   Grid2D=ESMF_GridCreate(maxIndex=(/10,20/), &
            coordDimCount=(/1,1/) , rc=rc)
!EOCI  

!BOEI
! The default Grid has the dimensions of the coordinate arrays
! mapped in order to the Grid dimensions (e.g. dimension 1 of the coordinate
! arrays corresponds to dimension 1 of the Grid.). If the coordinate arrays
! have a smaller rank than the Grid, then the default depends on the 
! dimension of the grid and the dimension of the individual coordinate Arrays. For a 2D Grid,
! if each Array has dimension 1, each 1D  Array is mapped to the Grid dimension
! corresponding to its component number (e.g. component Array 1 is mapped
! to grid dimension 1, and so on). For a 3D Grid with 1D Arrays, the same
! is true. For a 3D grid with one 1D and two 2D arrays, the 2D arrays are 
! mapped to align with their corresponding components numbers and each other, 
! and the 1D array is mapped to the other dimension. For all other cases, 
! the component arrays are just mapped in order starting from the first Grid dimension. 
!
! To alter the default mapping, the {\tt coordDimMap} argument can be used. 
! The parameter is a 2D array where each dimension is the dimension of the Grid.
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
   Grid2D=ESMF_GridCreate(maxIndex=(/10,20/), coordDimCount=(/1,1/) , &
               coordDimMap=coordDimMap, rc=rc)
!EOCI  
#endif

#ifdef LOCAL_NOT_IMPL
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
! Although ESMF provides a set of predefined stagger locations (See Section~\ref{sec:opt:staggerloc}),
! the user may need one outside this set. This section describes the construction of
! custom stagger locations. 
!
! To completely specify stagger for an arbitrary number of dimensions, we define the 
! stagger location in terms of a set of cartesian coordinates. The cell is represented
! by a n-dimensional cube with sides of length 2, and the coordinate origin located at
! the center of the cell. The geometry of the cell is for reference purposes only, 
! and does not literally represent the actual shape of the cell. Think of this method
! instead as an easy way to specify a part (e.g. center, corner, face) of a higher 
! dimensional cell which is extensible to any number of dimensions. 
!
! To illustrate this approach, consider a 2D cell. In 2 dimensions
! the cell is represented by a square. An xy axis is placed at its center, with the 
! positive x-axis oriented {\em East} and the positive y-axis oriented {\em North}.
! The resulting coordinate for the lower left corner is at $(-1,-1)$, and upper right
! corner at $(1,1)$.
! However, because our staggers are symmetric they don't need to distinguish between
! the $-1$, and the $1$, so we only need concern ourselves with the first quadrant of
! this cell. We only need to use the $1$, and the $0$, and many of the cell locations
! collapse together (e.g. we only need to represent one corner). See figure~\ref{fig:gridcuststaggerloc}
! for an illustration of these concepts.
!
!\begin{center}
!\begin{figure}
!\center
!\scalebox{0.75}{\includegraphics{GridCustStaggerLoc}}
!\caption{An example of specifying 2D stagger locations using coordinates.}
!\label{fig:gridcuststaggerloc}
!\end{figure}
!\end{center}
! 
! The cell center is represented by the coordinate pair $(0,0)$ indicating the origin.
! The cell corner is $+1$ in each direction, giving a coordinate pair of $(1,1)$.
! The edges are each $+1$ in one dimension and $0$ in the other indicating that 
! they're even with the center in one dimension and offset in the other. 
!
! For three dimensions, the vertical component of the stagger location can be added by 
! simply adding an additional coordinate. The three dimensional generalization of the
! cell center becomes $(0,0,0)$ and the cell corner becomes $(1,1,1)$. The rest of
! the 3D stagger locations are combinations of $+1$ offsets from the center. 
!
! To generalize this to $d$ dimensions, to represent a $d$ dimensional stagger
! location. A set of $d$ $0$ and $1$ is used to specify for each dimension
! whether a stagger location is aligned with the cell center in that dimension ($0$),
! or offset by $+1$ in that dimension ($1$). Using this scheme we can represent
! any symmetric stagger location.  
!
! To construct a custom stagger location in ESMF the subroutine 
! {\tt ESMF\_StaggerLocSet()} is used to specify, 
! for each dimension, whether the stagger is located at the interior (0) 
! or on the boundary (1) of the cell. This method allows users
! to construct stagger locations for which
! there is no predefined value. In this example, it's used to 
! set the 4D center and 4D corner locations. 
!
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
   call ESMF_GridAddCoord(grid4D, staggerLoc=staggerLoc, rc=rc)

   ! Set Corner
   call ESMF_StaggerLocSet(staggerLoc,loc=(/1,1,1,1/),rc=rc)
   call ESMF_GridAddCoord(grid4D, staggerLoc=staggerLoc, rc=rc)
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
!\subsubsection{Specifying Custom Stagger Padding}
!\label{sec:usage:staggerpadding:adv}
!
!There is an added complication with the data (e.g. coordinates) stored at stagger locations in 
!that they can require different amounts of storage depending
!on the underlying Grid type. 
!
!\begin{center}
!\begin{figure}
!\center
!\scalebox{0.75}{\includegraphics{GridCellsAndCorners}}
!\caption{An example 2D Grid with cell centers and corners.}
!\label{fig:gridcellsandcorners}
!\end{figure}
!\end{center}
!
! Consider the example 2D grid in figure~\ref{fig:gridcellsandcorners}, where the dots represent the cell corners
! and the ``+'' represents the cell centers. For the corners to completely
! enclose the cell centers (symmetric stagger), the number of corners in each 
! dimension needs to be one greater then the number of cell centers. In the above 
! figure, there are two rows and three columns of cell centers. To enclose the 
! cell centers, there must be three rows and four columns of cell corners.
! This is true in general for Grids without periodicity or
! other connections.  In fact, for a symmetric stagger, given that the center
! location requires n x m storage, the corresponding corner location
! requires n+1 x m+1, and the edges, depending on the side, require n+1 x m or
! m+1 x n.  In order to add the extra storage, but also to 
! allow the the different stagger location arrays to remain on the same DistGrid,
! the capability of the ESMF Array class to have computational
! bounds different from exclusive bounds is used.
! By default, when the coordinate arrays are created, one extra
! layer of padding is added to the arrays to create symmetric staggers 
! (i.e. the center location is surrounded). The default is to add this padding 
! on the positive side, and to only add this padding where needed 
! (e.g. no padding for the center, padding
! on both dimensions for the corner, in only one dimension for the 
! edge in 2D.) There are two ways for the user to change
! these defaults. 
!
! One way is to use the {\tt GridEdgeWidth} or {\tt GridAlign} arguments
! when creating a Grid. These arguments can be used to change the padding
! around the Grid cell index space. This extra padding is the extra
! space used by all the stagger locations, and no stagger location
! can extend outside of it. 
!
! The {\tt gridEdgeLWidth} and 
! {\tt gridEdgeUWidth} arguments are both 1D arrays of the
! same size as the Grid dimension. The entries in the arrays
! give the extra offset from the outer boundary of
! the grid cell index space to the exclusive region of the
! Grid. The following example shows the creation of 
! a Grid with all the extra space to hold stagger padding
! on the negative side of a Grid. This is the reverse of
! the default behavior. The resulting Grid will have
! an exclusive region which extends from $(-1,-1)$ to
! $(10,10)$, however, the cell center stagger location
! will still extend from $(1,1)$ to $(10,10)$.
!EOE

!!!!!!!!!!!!!!!!!!!!!!!
! Setup For Example
!!!!!!!!!!!!!!!!!!!!!!

!BOC 
   grid2D=ESMF_GridCreateShapeTile(minIndex=(/1,1/),maxIndex=(/10,10/), &
            gridEdgeLWidth=(/1,1/), gridEdgeUWidth=(/0,0/), rc=rc)

!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!!!!!!!!!!!!!!!!!!!!!!!
! Cleanup after Example
!!!!!!!!!!!!!!!!!!!!!!!
   call ESMF_GridDestroy(grid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!BOE
! To indicate how the data in a Grid's stagger locations are aligned with the 
! cell centers, the optional {\tt gridAlign} parameter 
! may be used. This parameter indicates which stagger elements 
! in a cell share the same index values as the cell center. 
! For example, in a 2D cell, it would indicate which of the four corners has
! the same index value as the center. To set {\tt gridAlign},  
! the values -1,+1 are used to indicate the alignment in
! each dimension. This parameter is mostly 
! informational, however, if the {\tt gridEdgeWidth} parameters 
! are not set then its value determines where the default padding
! is placed. If not specified, then the default is to align all 
! staggers to the most negative, so the padding is on the positive side. 
! The following code illustrates creating a Grid aligned to the reverse of
! default (with everything to the positive side). This creates a
! Grid identical to that created in the previous example. 
!EOE

!!!!!!!!!!!!!!!!!!!!!!!
! Setup For Example
!!!!!!!!!!!!!!!!!!!!!!

!BOC 
   grid2D=ESMF_GridCreateShapeTile(minIndex=(/1,1/),maxIndex=(/10,10/), &
            gridAlign=(/1,1/), rc=rc)

!EOC  

  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!!!!!!!!!!!!!!!!!!!!!!!
! Cleanup after Example
!!!!!!!!!!!!!!!!!!!!!!!
   call ESMF_GridDestroy(grid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
! The {\tt gridEdgeWidth} and {\tt gridAlign} arguments both
! allow the user to set the extra padding available to be used
! by stagger locations in a Grid. By default, stagger locations
! allocated in a Grid set their stagger padding based on these
! values.  A stagger location's padding in each dimension is
! equal to the value of {\tt gridEdgeWidth} (or the value implied
! by {\tt gridAlign}), unless the stagger location is centered
! in a dimension in which case the stagger padding is 0. For example,
! the cell center stagger location has 0 stagger padding in all
! dimensions, whereas the edge stagger location lower padding
! is equal to {\tt gridEdgeLWidth} and the upper padding is equal
! to {\tt gridEdgeUWidth} in one dimension, but both are 0 in the other,
! centered, dimension.  If the user wishes to set the stagger padding
! individually for each stagger location they may use the
! {\tt staggerEdgeWidth} and {\tt staggerAlign} arguments,
! however, the padding set this way must be within that specified
! by the {\tt gridEdgeWidth} and {\tt gridAlign} 
! used when the Grid was created (or the defaults if none were used).
! 
! The {\tt staggerEdgeLWidth} and 
! {\tt staggerEdgeUWidth} arguments are both 1D arrays of the
! same size as the Grid dimension. The entries in the arrays
! give the extra offset from the Grid cell index space
! for a stagger location. The following example shows the
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

!BOC 
   grid2D=ESMF_GridCreate(distgrid=distgrid2D, &
            gridEdgeLWidth=(/1,1/), gridEdgeUWidth=(/0,0/), rc=rc)

   call ESMF_GridAddCoord(grid2D, &
          staggerLoc=ESMF_STAGGERLOC_CORNER, &
          staggerEdgeLWidth=(/0,0/), staggerEdgeUWidth=(/0,0/), rc=rc)

   call ESMF_GridAddCoord(grid2D, &
          staggerLoc=ESMF_STAGGERLOC_CENTER, &
          staggerEdgeLWidth=(/1,1/), staggerEdgeUWidth=(/0,0/), rc=rc)

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
! cell center, the optional {\tt staggerAlign} parameter 
! may be used. This parameter indicates which stagger elements 
! in a cell share the same index values as the cell center. 
! For example, in a 2D cell, it would indicate which of the four corners has
! the same index value as the center. To set {\tt staggerAlign},  
! the values -1,+1 are used to indicate the alignment in
! each dimension. If a stagger location is 
! centered in a dimension (e.g. an edge in 2D), then that
! dimension is ignored in the alignment. This parameter is mostly 
! informational, however, if the {\tt staggerEdgeWidth} parameters 
! are not set then its value determines where the default padding
! is placed. If not specified, then the default is to align all 
! staggers to the most negative, so the padding is on the positive side. 
! The following code illustrates aligning the positive (northeast in 2D) 
! corner with the center. 
!EOE

!!!!!!!!!!!!!!!!!!!!!!!
! Setup For Example
!!!!!!!!!!!!!!!!!!!!!!
   distgrid2D=ESMF_DistGridCreate(minIndex=(/1,1/), &
                                  maxIndex=(/10,10/), rc=rc)
   grid2D=ESMF_GridCreate(distgrid=distgrid2D, rc=rc)

!BOC 
   call ESMF_GridAddCoord(grid2D, &
          staggerLoc=ESMF_STAGGERLOC_CORNER, staggerAlign=(/1,1/), rc=rc)
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
