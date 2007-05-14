! $Id: ESMF_GridUsageEx.F90,v 1.2 2007/05/14 17:11:03 cdeluca Exp $
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

      ! Local variables  
      integer:: rc, finalrc
      type(ESMF_VM):: vm
      type(ESMF_ArraySpec) ::  arrayspec2D

      ! initialize ESMF
      finalrc = ESMF_SUCCESS
      call ESMF_Initialize(vm=vm, rc=rc)



!BOE
!\subsubsection{Create 2D, Single-Tile, Rectilinear Grid}
!
! The following is a simple example of creating a grid and
! loading in a set of coordinates. This code creates a 10x20
! 2D grid with the coordinates varying from (10,10) to (100,200).
! The grid is distributed across 8 DEs (2 in the first
! dimension, 4 in the second). The grid is created with
! global indices to make it easier to generate the coordinates. 
! After grid creation the local bounds and f90 arrays are retrieved
! and the coordinates set.
!
!EOE

!BOC
   grid2D=ESMF_GridCreate(minIndex=(1,1/), maxIndex=(/10,20/),     &
            regDecomp=(/2,4/), indexflag=ESMF_INDEX_GLOBAL, rc=rc)

   call ESMF_GridLocalTileGet(grid2D, exclusiveLBound=lbnd, exclusiveUBound=ubnd, rc=rc)
   call ESMF_GridLocalTileGetData(grid2D, comp=1, coordsX, rc=rc)
   call ESMF_GridLocalTileGetData(g rid2D, comp=2, coordsY, rc=rc)

   do j=lbnd(2),ubnd(2)
   do i=lbnd(1),ubnd(1)
        coordsX(i,j) = 10.0*i
        coordsY(i,j) = 10.0*j
   enddo
   enddo

!EOC

!BOE
!\subsubsection{Creation: Methods}
!
! In this document, Grid creation means the allocation and setup of the 
! Grid data type. After creation the Grid object is ready to use except
! for the fact that its coordinate values are not set. This is to be
! distinguished from Grid {\it generation}. Grid generation is the 
! production of the Grid coordinate values. For example,
! the spherical Grid generation method would produce a Grid object
! with its latitude and longitude values set to locations on a sphere. 
!
! There are several methods of creating an ESMF Grid. The first of these allows
! the user to create a Grid from scratch with maximum control over the 
! specifics of the Grid to be created. The user can create a Grid and specify 
! the dimension, size, type, 
! topology (via DistGrid), distribution
! (via DistGrid), and stagger locations. This call creates a 
! Grid which is ready to be used in all respects, except its coordinate 
! values are uninitialized.
!
! The second method of Grid creation allows the user to construct a
! Grid from a set of Arrays containing 
! coordinate information. These Arrays
! need to have a coherent configuration (same dimension, sizes conforming
! to stagger locations, same distribution, same types, etc.) for the
! call to succeed. The Grid can either be created to contain the actual
! Arrays, or the Grid can be created containing copies of the orignal arrays. 
!
! The third method of creation allows the user to construct a Grid that
! would be an appropriate container for coordinates for a specific data 
! Array. Given an Array, the user can construct a Grid with exactly the 
! same shape, size and layout as the array. 
!
!\subsubsection{Creation: Size, Shape, and Distribution}
!
! Typically the first step in creating a Grid is to describe its
! size, topology, and distribution. In ESMF there is a simple option
! for easily specifying a Grid using mostly defaults, and a slightly
!  more involved option when more precise control is desired. 
!
! In the simplest method the user employs the {\tt maxIndex}
! parameters to specify the Grid size in the Grid create call. 
! This creates a Grid of upper bound  {\it maxIndex}, with the rest of the 
! options set to default values. 
!
! The following illustrates the creation of a simple 10 x 20 2D 
! Grid with all the default options. It has lower bound (1,1,1,..) with
! ESMF\_R8 coordinates and the default 
! distribution (first dimension spiit across all the processors, the rest of the dimensions
! distributed, but only across a single processor width). It contains data in 
! the center stagger location, and the coordinate data is stored in two 2D arrays. 
!EOE

!BOC 
   Grid2D=ESMF_GridCreate(maxIndex=(/10,20/), rc=rc)
!EOC  

!BOE
! For more control over the Grid index space, the lower bound, {\it minIndex}, can 
! also be specified. 
!EOE

!BOC 
   Grid2D=ESMF_GridCreate(minIndex=(/5,5/),maxIndex=(/15,25/), rc=rc)
!EOC  

!BOE
! To create a slightly more complex distribution the
! {\tt regDecomp} argument can be used to describe how many DEs to 
! break each dimension into. Here the Grid is
! distributed in the first dimension into 2 pieces and
! the second into 4. 
!EOE

!BOC 
   Grid2D=ESMF_GridCreate(maxIndex=(/10,20/), regDecomp=(/2,4/), rc=rc)
!EOC  

!BOE
! For direct control over the topology and distribution of the Grid, 
! the user can first create a DistGrid (and possibly a DElayout) to describe 
! the Grid in detail. Please see the DistGrid design document for an in depth
! description of its use. Once the DistGrid is created the user
! can either employ it directly to create the Grid or create
! Arrays and then create the Grid from those. Creation with a DistGrid is covered 
! in the examples in Sections~\ref{sec:example1}-~\ref{sec:example5}, so please
! see those for an illustration of its use. 
!EOE

!BOE
! The proceeding arguments only dealt with the disributed part
! of a Grid (e.g. {\it maxIndex} only specifies distributed dimensions).
! To add undistributed dimensions to the Grid, the parameters
! {\tt lbounds} and {\tt ubounds} may be used. The {\tt lbounds} argument
! contains the lower bounds of the undistributed dimensions and {\tt ubounds}
! contains the upper bounds. As an example, the following call constructs
! a 10x20x30 Grid with a lower bound of (1,2,3), with the third dimension
! undistributed.
!EOE

!BOC 
   Grid3D=ESMF_GridCreate(minIndex=(/1,2/), maxIndex=(/11,22/), &
            lbounds=(/3/), ubounds=(/33/), rc=rc)
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
   Grid3D=ESMF_GridCreate(minIndex=(/1,2/), maxIndex=(/11,22/), &
            lbounds=(/3/), ubounds=(/33/), dimmap=(/2,3/), rc=rc)
!EOC  

!BOE
!\subsubsection{Creation: Type, Kind, and Rank}
!
! The default type and kind for a Grid is ESMF\_R8. If the rank 
! isn't specified it's either the rank of {\it minIndex} plus the rank 
! of {\it ubounds}, or the dimension of {\it distgrid} plus the 
! rank of {\it ubounds}. If {\it ubounds} isn't specified then 
! its rank is considered to be 0.
!
!  To control the data type and kind more precisely an optional 
! arrayspec parameter may be used. The following illustrates
! the creation of a Grid with 4 byte Integer coordinates. 
!EOE

!BOC 
   call ESMF_ArraySpecSet(arrayspec, type=ESMF_DATA_INTEGER,         &
          kind=ESMF_I4, rank=2)

   Grid2D=ESMF_GridCreate(arraySpec=arrayspec,maxIndex=(/10,20/), rc=rc)
!EOC  


!BOE

!\subsubsection{Creation: Coordinate Storage}
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
! The parameters {\tt coordCompRanks} and {\tt coordCompDimMap}
! can be used to specify the factorization of the coordinate arrays. 
!
! The default Grid has coordinate arrays the same rank as the Grid.
! To alter this, the {\tt coordCompRanks} parameter can be used
! to set the rank of the coordinate arrays.  The size of the parameter  
! is the rank of the Grid. Each entry of {\tt coordCompRanks}  is the rank of 
! the associated coordinate array. The following creates a 10x20
! 2D Grid where both the x and y coordinates are stored in 1D arrays. 
!EOE

!BOC 
   Grid2D=ESMF_GridCreate(maxIndex=(/10,20/), &
            coordCompRanks=(/1,1/) , rc=rc)
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
! To alter the default mapping, the {\tt coordCompDimMap} argument can be used. 
! The parameter is a 2D array where each dimension is the rank of the Grid.
! The first dimension indicates the component (e.g. x=1). The second dimension
! is an entry for each dimension of the coordinate array.  Each entry of 
! {\tt coordCompDimMap}  tells which Grid dimension the corresponding
! dimension maps to.  The following creates a 2D Grid where
! the single dimension of the x coordinate array is mapped to the second
! Grid dimension (i.e. 20) and the single dimension of the y coordinate array is
! mapped to the first Grid dimension (i.e. 10). 
!EOE

!BOC 
   coordCompDimMap(1,1)=2      ! Map X (i.e. 1) to second Grid dimension 
   coordCompDimMap(2,1)=1      ! Map Y (i.e. 2) to first Grid dimension 
   Grid2D=ESMF_GridCreate(maxIndex=(/10,20/), coordCompRanks=(/1,1/) , &
               coordCompDimMap=coordCompDimMap, rc=rc)
!EOC  


!BOE
!\subsubsection{Creation: Stagger Location}
!
! In this document, stagger location refers to the places in a Grid
! cell that can contain data. For example, data can be located
! at the cell center,  at the cell corner, at the cell face, and so on into
! higher dimensions. The ESMF Grid class
! allows the specification of multiple stagger locations per
! Grid.  When setting or retrieving
! coordinate data the stagger location is specified to tell the Grid method 
! from where in the cell to get the data. There are predefined stagger locations, however, 
! should the user find it necessary to describe their own, there
! is also a set of methods for generating custom locations. 
! (see Section~\ref{ref:stagger} for a more in depth
! description of stagger locations and stagger specification.)
!
!\medskip
!
!\begin{verbatim}
!    
!                 2----4----2
!                 |         |
!                 |         |
!                 3    1    3
!                 |         |
!                 |         |
!                 2----4----2
! 
!      Diagram Illustrating 2D Stagger Locations
!   (Numbers correspond to bracketed numbers in list.)
!
!\end{verbatim}
!
!The 2D predefined stagger locations are:\\
!\begin{description}
!\item [ESMF\_STAGGERLOC\_CENTER:] The center of the cell [1].
!\item [ESMF\_STAGGERLOC\_CORNER:] The corners of the cell [2].
!\item [ESMF\_STAGGERLOC\_EDGE1:] The edges offset from the center in the 1st dimension [3].
!\item [ESMF\_STAGGERLOC\_EDGE2:] The edges offset from the center in the 2nd dimension [4].
!\end{description}
!
!\medskip
!
!The 2D predefined staggers are:\\
!\begin{description}
!\item [ESMF\_STAGGER\_A\_GRID:] Just the center of the cell (Arakawa A).
!\item [ESMF\_STAGGER\_B\_GRID:] The cell center and corners (Arakawa B).
!\item [ESMF\_STAGGER\_C\_GRID:] The cell center and edges (Arakawa C).
!\item [ESMF\_STAGGER\_D\_GRID:] The cell center and edges (Arakawa D).
!\end{description}
!
!\medskip
!
!\begin{verbatim}
!    
!      5----7----5        2----4----2         5----7----5
!      |         |        |         |         |         |
!      |         |        |         |         |         | 
!      6    8    6        3    1    3         6    8    6
!      |         |        |         |         |         |
!      |         |        |         |         |         |
!      5----7----5        2----4----2         5----7----5
!
!       Cell Top          Cell Middle         Cell Bottom
!         
!
!           Diagram Illustrating 3D Stagger Locations
!       (Numbers correspond to bracketed numbers in list.)
!
!\end{verbatim}
!
!
!
!The 3D predefined stagger locations are:\\
!\begin{description}
!\item [ESMF\_STAGGERLOC\_CENTER\_VCENTER:] The center of the 3D cell [1].
!\item [ESMF\_STAGGERLOC\_CORNER\_VCENTER:] Half way up the vertical edges of the cell [2].
!\item [ESMF\_STAGGERLOC\_EDGE1\_VCENTER:] The center of the face bounded by edge 1 and the vertical dimension [3].
!\item [ESMF\_STAGGERLOC\_EDGE2\_VCENTER:] The center of the face bounded by edge 2 and the vertical dimension [4]. 
!\item [ESMF\_STAGGERLOC\_CORNER\_VFACE:] The corners of the 3D cell [5].
!\item [ESMF\_STAGGERLOC\_EDGE1\_VFACE:] The center of the edges of the 3D cell parallel offset from the center in the 1st dimension [6].
!\item [ESMF\_STAGGERLOC\_EDGE2\_VFACE:] The center of the edges of the 3D cell parallel offset from the center in the 2nd dimension [7].
!\item [ESMF\_STAGGERLOC\_CENTER\_VFACE:] The center of the top and bottom face. The face bounded by the 1st and 2nd dimensions [8]. 
!\end{description}
!
!\medskip
!
!BOE
! The following  examples illustrate the use of the preceding predefines
! in specifying a Grid's stagger locations.  In addition, there is an
! example of constructing a custom stagger location. 
!
! To set which stagger locations in a Grid have coordinate data, the optional
! {\tt staggerLoc} parameter is used. The first and easiest way to do this is to
! use one of the predefined stagger parameters. This 
! sets all the stagger locations for the Grid. The following
! example creates a Grid with coordinates at both 
! the center and edge locations (C-Grid).
!EOE

!BOC 
   Grid2D=ESMF_GridCreate(maxIndex=(/10,20/), staggerLoc=ESMF_STAGGER_C_GRID, rc=rc)
!EOC  

!BOE
! Another method is to explicitly set which 
! stagger locations the Grid should contain. The following
! example creates a Grid with coordinates at 
! the center, the corners, and both edge locations.
!EOE

!BOC 
   staggerLoc(1)=ESMF_STAGGERLOC_CENTER
   staggerLoc(2)=ESMF_STAGGERLOC_CORNER
   staggerLoc(3)=ESMF_STAGGERLOC_EDGE1
   staggerLoc(4)=ESMF_STAGGERLOC_EDGE2

   Grid2D=ESMF_GridCreate(maxIndex=(/10,20/), staggerLoc=staggerLoc, rc=rc)
!EOC  


!BOE
! To construct a custom stagger (method described further in Section~\ref{ref:stagger})
! the users uses the subroutine {\it ESMF\_StaggerLocSet()} to
! give a qualitative description of the location
! in each dimension (center (0) vs. side (1)).
! This method allows users
! to construct stagger locations for which
! there is no predefined value. However, 
! in this example, for demonstration purposes,
! its used to set the 2D center and 2D corner locations. 
!EOE

!BOC 
   ! Set Center
   call ESMF_StaggerLocSet(staggerLoc(1),where=(/0,0/),rc=rc)

   ! Set Corner
   call ESMF_StaggerLocSet(staggerLoc(2),where=(/1,1/),rc=rc)
      
   Grid2D=ESMF_GridCreate(maxIndex=(/10,20/), staggerLoc=staggerLoc, rc=rc)
!EOC
  
!BOE
!
!There is an added complication with stagger locations in 
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
! (i.e. the center location is
! surrounded). The default is to add this padding on the positive side, and
! to only add this padding where needed (e.g. no padding for the center, padding
! on both dimensions for the corner, in only one dimension for the 
! edge in 2D.)  To change these defaults the {\tt staggerLocWidth} arguments 
! can be used to adjust the width and placement of the padding for each
! stagger location. 
!
!EOE

!BOE
The {\tt staggerLocLWidth} and 
{\tt staggerLocUWidth} arguments are both 2D arrays, where the first
dimension is the number of stagger locations, and the 
second is the Grid rank. The entries in the arrays
give the extra offset from the outer boundary of
the tile exclusive region for
each stagger location. The following example shows the
creation of a Grid with two stagger locations. The
center location has no extra boundary and the 
corners have a single layer of extra padding on 
the negative side and none on the positive. 
!
!EOE

!BOC 
   staggerLoc(1)=ESMF_STAGGERLOC_CENTER
   staggerLocLWidth(1,:)=(/0,0/)
   staggerLocUWidth(1,:)=(/0,0/)

   staggerLoc(2)=ESMF_STAGGERLOC_CORNER
   staggerLocLWidth(2,:)=(/-1,-1/)
   staggerLocUWidth(2,:)=(/0,0/)


   Grid2D=ESMF_GridCreate(maxIndex=(/10,20/), staggerLoc=staggerLoc, &
            staggerLocLWidth, staggerLocUWidth, rc=rc)
!EOC  

!BOE
To indicate how the stagger locations aligned with the 
cell center the optional {\tt staggerLocAlign} parameter 
may be used. This parameter indicates which stagger element 
in a cell shares the same index values as the cell center. 
For example, in a 2D cell, it would indicate which of the four corners has
the same index value as the center. To set {\tt staggerLocAlign},  
the values -1,+1 are used to indicate the alignment in
each dimension. If a stagger location is 
centered in a dimension (e.g. an edge in 2D), then that
dimension is ignored in the alignment. This parameter is mostly 
informational, however, if the {\tt staggerLocWidth} parameters 
are not set then its value determines where the default padding
is placed. If not specified, then the default is to align all 
staggers to the most negative, so the padding is on the positive side. 
The following code illustrates aligning the positive corner with the center. 
!EOE

!BOC 
   staggerLoc(1)=ESMF_STAGGERLOC_CENTER
   staggerLocAlign(1,:)=(/0,0/)  ! ignored

   staggerLoc(2)=ESMF_STAGGERLOC_CORNER
   staggerLocAlign(2,:)=(/+1,+1/) ! positive corner

   Grid2D=ESMF_GridCreate(maxIndex=(/10,20/), staggerLoc=staggerLoc, &
            staggerLocAlign=staggerLocAlign, rc=rc)
!EOC  

!BOE
!\subsubsection{Creation: Miscellaneous}
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
!EOE


!BOE
!\subsubsection{Creation: Set/Commit}
! 
!  As an alternative to the monolithic one call create, ESMF provides
! a set/commit paradigm for grid creation which allows the user
! to break up creation for clarity or convenience. 
! To do this, the user first creates an empty grid. Next, a series of 
! set/add calls are used to fill in the details of the grid. Finally, a commit
! call is used to validate and create the final, usable, grid.
! The following is an example illustrating this technique.  
! It creates a 10x20 grid with the center and corner stagger locations. 
!EOE

!BOC
   
   ! Create empty grid
   grid=ESMF_GridCreateEmpty(rc=rc)

   ! Set grid size
   call ESMF_GridSet(grid, maxIndex=(/10,20/), rc=rc)

   ! Add Center Stagger Location
   call ESMF_GridAddStaggerLoc(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)

   ! Add Corner Stagger Location
   call ESMF_GridAddStaggerLoc(grid, staggerLoc=ESMF_STAGGERLOC_CORNER, rc=rc)

   ! Turn grid into a usable ESMF Grid
   call ESMF_GridCommit(grid, rc=rc)


!EOC

!BOE
!\subsubsection{Data Access}
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
   call ESMF_GridSetCoordFromArray(grid, &
          staggerLoc=ESMF_STAGGERLOC_CENTER, &
          coordComp=1, array=xCoord, rc=rc)
!EOC

!BOE
! {\tt ESMF\_GridGetCoordIntoArray}, allows the user
! to get the Array (a direct reference or a copy) which
! contains the coordinate data for a stagger location on a Grid. The user
! can then employ any of the standard {\tt ESMF\_Array} tools to operate
! on the data. The following copies the coordinates from the second 
! component of the corner and puts it into Array copyOfY. 
! EOE

!BOC
   call ESMF_GridGetCoordIntoArray(grid, &
          staggerLoc=ESMF_STAGGERLOC_CORNER,coordComp=2, &
          array=copyOfY, docopy=ESMF_DATA_COPY, rc=rc)
!EOC

!BOE
! The second pair of methods enable the user to set or get data using
! a fortran pointer. These methods only work with the local piece of the 
! Grid on the DE. {\tt ESMF\_GridLocalTileSetData} enables the user
! to set data into the local piece of the coordinates residing on the DE.
! The following call gets a pointer (fptr) to the fortran array holding the 
! first component (e.g. x) coordinates for the corner stagger
! for the piece of tile 2 which is on this processor. It
! defaults to the first DE because it isn't specified. 
!EOE

!BOC
   call ESMF_GridLocalTileSetData(grid, tile=2, &
          staggerLoc=ESMF_STAGGERLOC_CORNER,    &
          coordComp=1, fptr, &
          doCopy=ESMF_DATA_REF, rc=rc)
!EOC

!BOE
! The call {\tt ESMF\_GridLocalTileGetData} gets a fortran pointer to 
! the coordinate data. The user can then operate on this array in the usual
! manner. The following call allocates an array (fptr) and
! makes copy of the part of tile 1's third component (e.g. z)  coordinates which
! lie on the second DE. 
!EOE

!BOC
   call ESMF_GridLocalTileGetData(grid, tile=1, localDE=2, &
          coordComp=3, fptr, doCopy=ESMF_DO_COPY, rc=rc)

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
!EOE

!BOC
   call ESMF_GridHalo(grid, rc=rc)
!EOC

!BOE
!\subsubsection{Grid Metadata}
!
! To allow the storage of data describing a particular
! type of grid, the capability to add metadata name-value
! pairs to a Grid object is provided. Metadata is 
! more strictly controlled than attributes. The valid
! metadata labels are controlled by ESMF. The 
! valid metadata for a grid will also depend on the grid's type
! set via {\tt gridType} during creation.
! Metadata can be used inside ESMF routines, so it should only 
! be set with caution by users. Metadata should 
! also be set to the same value across processors. 
! The following code is an example of setting metadata
! value "Radius" to 10.0.  
!EOE

!BOC
   call ESMF_GridSetMetaData(grid, "Radius", 10.0, rc=rc)
!EOC

!BOE
!\subsubsection{Grid Attributes}
!
! As is typical for ESMF classes, the Grid class allows
! the user to attach name-value pairs to a Grid object. 
! The following adds the attribute "Size" with the value 10. 
!EOE

!BOC
   call ESMF_GridSetAttribute(grid, "Size", 10, rc=rc)
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
