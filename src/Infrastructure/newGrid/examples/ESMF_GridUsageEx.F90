! $Id: ESMF_GridUsageEx.F90,v 1.10 2007/05/22 23:30:18 oehmke Exp $
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
!\subsubsection{Example: Create 2D, Single-Tile, Uniform Grid}
!
! The following is a simple example of creating a grid and
! loading in a set of coordinates. This code creates a 10x20
! rectilinear 2D grid with the coordinates varying from (10,10) to (100,200).
! The grid is partitioned in the first dimension into two pieces both
! of size 5. The grid is partitioned in the second dimension into 3
! pieces of size 7,7 and 6.  The grid is created with
! global indices to make it easier to generate the coordinates. 
! After grid creation the local bounds and f90 arrays are retrieved
! and the coordinates set.
!
!EOE

!BOC
   ! Create the empty grid
   grid2D=ESMF_GridCreateShape(countsPerDEDim1=(/5,5/),     &
            countsPerDEDim2=(/7,7,6/), indexflag=ESMF_GLOBAL, &
            rc=rc)

   ! Add a stagger location
   call ESMF_GridAddStaggerLoc(grid2D, ESMF_STAGGERLOC_CENTER, rc=rc)

   ! Get the X coordinate bounds
   call ESMF_GridLocalTileGet(grid2D, coordComp=1, &
          computationalLBound=lbnd, computationalUBound=ubnd, rc=rc)

   ! Get pointers to the X coordinate component storage
   call ESMF_GridLocalTileGetData(grid2D1, coordComp=1, coordsX, rc=rc)

   ! Calculate and set the X coordinates
   do i=lbnd(1),ubnd(1)
        coordsX(i,j) = i*10.0
   enddo

   ! Get the Y coordinate bounds
   call ESMF_GridLocalTileGet(grid2D, coordComp=2, &
          computationalLBound=lbnd, computationalUBound=ubnd, rc=rc)

   ! Get pointers to the X coordinate component storage
   call ESMF_GridLocalTileGetData(grid2D1, coordComp=2, coordsY, rc=rc)

   ! Calculate and set the X coordinates
   do i=lbnd(1),ubnd(1)
        coordsY(i) = i*10.0
   enddo

   ! We now have all our coordinates so commit as regrid ready.
   call ESMF_GridCommit(grid2D1, ESMF_GRIDSTATUS_REGRID_READY, rc=rc)

!EOC

!BOE
!\subsubsection{Creation: Shapes}
!
! There are several methods of creating an ESMF Grid. The first
! set of these are designed to easily allow the user to create
! the most common grids. There are specific creates for 
! several common shapes. The method {\tt ESMF\_GridCreateShape}
! creates a rectangular grid. The method 
! {\tt ESMF\_GridCreateShapeSphere} creates a Grid with a pole
! at either end of the first dimension and periodic in the second.
! The method {\tt ESMF\_GridCreateShapeTripole} creates a grid
! with a bipole at one end of the first dimension and 
! a pole at the other. The tripole grid is also periodic
! in the second dimension. The method {ESMF\_GridCreateShape}
! allows the user to specify variations on the first three
! grids, by allowing the user to specify connections
! in each of the three dimensions. Despite the fact that
! these subroutines create different shapes, the majority of
! their arguments are the same. At first we explain the
! common arguments, the shape specific arguments are covered
! at the end. 
!
!\subsubsection{Creation: Size, Shape, and Distribution of Index Space}
!
! To specify the rank, size and distribution of the index space
! for the shortcut calls, the {\tt countsPerDEDim1,2,3}
! arguments are used. These specify a rectangular
! distribution containing size(countsPerDEDim1) by
! size(countsPerDEDim2) by size(countsPerDEDim3)
! DEs. The entries in each of these arrays specifies 
! the size of the DEs in that dimension for that row or column.
! The rank of the grid is determined by the presence of 
! {\tt countsPerDEDim3}. If it's present the Grid
! will be 3D, if just {\tt countsPerDEDim1} and
! {\tt countsPerDEDim2} are specified the grid
! will be 2D. If any of these arrays has size
! 1 then that index dimension is undistributed. 
! {\bf Need picture}
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
          countsPerDEDim2=(/7,7,6/), countsPerDEDim1=(/15,15/), rc=rc)   
!EOC

!BOE
! If the third dimension were undistributed then the call
! would look like the following. 
!EOE

!BOC
   grid=ESMF_GridCreateShape(countsPerDEDim1=(/5,5/), &
          countsPerDEDim2=(/7,7,6/), countsPerDEDim1=(/30/), rc=rc)   
!EOC

!BOE
!
! The {\tt petMap} parameter may be used to specify which PETs 
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
           countsPerDEDim2=(/7,7,6/), petMap=petMap, rc=rc)   
!EOC


!BOE
!
!\subsubsection{Creation: Coordinate Specification And Index Space Dependency}
!
! To specify how the coordinate arrays are mapped to the 
! index dimensions the arguments {\tt coordCompDep1,2,3}
! are used. The size of the arrays specify the rank
! of the cooresponding coordinate component array.
! (e.g. size(coordCompDep1)=2 means that coordinate
! component 1 (e.g. x) is 2D) The entries in
! {\tt coordCompDep} specify which index dimension
! the cooresponding coordinate component dimension
! maps to.  (e.g. coordCompDep1=(/1,2/) means that
! the first dimension of component 1 maps to index dimension
! 1 and the second maps to index dimension 2.) If not set,
! then {\tt coordCompDepX} defaults to (/X/), in other
! words rectilinear.  
! 
! The following call demonstrates the creation of a
! 10x20 2D rectilinear grid where the first coordinate
! component is mapped to the second index dimension
! (i.e. is of size 10) and the second coordinate component
! is mapped to the second index dimension (i.e. is of size
!  20).
!EOE

!BOC
   grid=ESMF_GridCreateShape(countsPerDEDim1=(/5,5/), &
          countsPerDEDim2=(/7,7,6/), coordCompDep1=(/2/), &
          coordCompDep2=(/1/), rc=rc)   
!EOC 

!BOE
! The following call demonstrates the creation of a
! 10x20 2D curvilinear grid where where both
! coordinate component arrays are 10x20 also. 
!EOE

!BOC
   grid=ESMF_GridCreateShape(countsPerDEDim1=(/5,5/), &
          countsPerDEDim2=(/7,7,6/), coordCompDep1=(/1,2/), &
          coordCompDep2=(/1,2/), rc=rc)   
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
          coordCompDep1=(/1,2/), coordCompDep2=(/1,2/), &
          coordCompDep3=(/3/), rc=rc)   
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
!\subsubsection{Creation: Type and Kind}
!
! The default type and kind for a Grid is ESMF\_R8. 
!  To control the data type and kind more precisely the optional 
!  {\tt coordTypeKind} parameter may be used. The following illustrates
! the creation of a Grid with 4 byte Integer coordinates. 
!EOE

!BOC 
   grid=ESMF_GridCreateShape(coordTypeKind=ESMF_TYPEKIND_I4, &
          countsPerDEDim1=(/5,5/), countsPerDEDim2=(/7,7,6/), rc=rc)   
!EOC  

!BOE
!
!\subsubsection{Creation: Halo Depth}
!
! The default halo depth for a Grid is 0. 
! The shortcut Grid shape creation methods allow the user
! to set the halo, but it needs to be the same depth in all dimensions.
! (The haloDepth may be controlled more precisely by using the
!  more general grid create call.) The parameter {\tt haloDepth} 
!  may be used to set the coordinate halo depth. The following
!  is an example of setting the halo depth to 1.
!EOE

!BOC 
   grid=ESMF_GridCreateShape(countsPerDEDim1=(/5,5/), &
           countsPerDEDim2=(/7,7,6/), haloDepth=1, rc=rc)   
!EOC  


!BOE
!
!\subsubsection{Creation: Edge Connections}
!
! The {\tt ESMF\_GridCreateShape} command has three specific arguments
! {\tt connDim1}, {\tt connDim2}, and {\tt connDim3}. These can be used
! to setup different types of connections at the ends of each dimension.
! Each of these parameters is a two element array. The first element is 
! the connection type at the minimum end of the dimension and the second
! is the connection type at the maximum end. The default value
! for all the connections is ESMF\_GRIDCONN\_NONE, specifying no
! connection. Other options are ESMF\_GRIDCONN\_POLE specifying
! a pole connection, ESMF\_GRIDCONN\_BIPOLE specifying a bipole
! connection, and ESMF\_GRIDCONN\_PERIODIC specifying a periodic connection. 
! If one end of an index dimension specifies a periodic connection 
! then both must. The following constructs a sphere with a bipole
! at either end of dimension 2.
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
! This table shows the ESMF\_GRIDCONN\_ values which can be used to create various shapes. 
! Note that the dimensions the connections are on is arbitrary. For example,
! putting the settings for specifying a sphere into {\tt connDim2} and {\tt connDim3} instead would
! still result in a sphere, but oriented along a different axis. 
!
! \medskip
! \begin{tabular}{|l|c|c||c|c||}
! \hline
!         & {\bf connDim1(1)} & {\bf connDim1(2)}  & {\bf connDim2(1)} & {\bf connDim2(2)}  \\ 
! \hline
! {\bf Rectangle}  & NONE & NONE & NONE & NONE \\
! {\bf Bipole Sphere} & POLE & POLE & PERIODIC & PERIODIC \\
! {\bf Tripole Sphere} & POLE & BIPOLE & PERIODIC & PERIODIC \\
! {\bf Cylinder} & NONE & NONE & PERIODIC & PERIODIC \\
! {\bf Torus}  & PERIODIC & PERIODIC & PERIODIC & PERIODIC \\
! \hline
! \hline
! \end{tabular}
!
!EOE


!BOE
!\subsubsection{Creation: Set/Commit}\label{sec:usage:setcommit}
! 
!  As an alternative to the monolithic one call create, ESMF provides
! a set/commit paradigm for grid creation which allows the user
! to break up creation for clarity or convenience. 
! To do this, the user first creates an empty grid. Next, a series of 
! set calls are used to fill in the details of the grid. Note that, 
! a series of sets identical to the shortcut shape create calls
! has been provided for the user's convenience in using this
! paradigm.  Finally after the sets, a commit
! call is used to validate and create the final, usable, grid.
! The following is an example illustrating this technique.  
! It creates a rectangular 10x20 grid with the center and corner stagger locations. 
!EOE

!BOC
   
   ! Create empty grid
   grid=ESMF_GridCreateEmpty(rc=rc)

   ! Set grid size
   call ESMF_GridSetShapeBox(grid, countsPerDEDim1=(/5,5/), &
          countsPerDEDim1=(/7,7,6/), rc=rc)

   ! Turn grid into a usable ESMF Grid
   call ESMF_GridCommit(grid, ESMF_GRIDSTATUS_SHAPE_READY, rc=rc)

   ! Add Center Stagger Location
   call ESMF_GridAddStaggerLoc(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)

   ! Add Corner Stagger Location
   call ESMF_GridAddStaggerLoc(grid, staggerLoc=ESMF_STAGGERLOC_CORNER, rc=rc)
!EOC


!BOE
!\subsubsection{Stagger Location}\label{sec:usage:staggerloc}
!
! In this document, stagger location refers to the places in a Grid
! cell that can contain data. For example, data can be located
! at the cell center,  at the cell corner, at the cell face, and so on into
! higher dimensions. The ESMF Grid class
! allows the specification of multiple stagger locations per
! Grid.  When setting or retrieving
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
! {\tt ESMF\_GridAddStaggerLoc} is used. The following example
! adds the corner stagger location to {\tt grid}.
!EOE

!BOC 
   call ESMF_GridAddStaggerLoc(grid, staggerLoc=ESMF_STAGGER_CORNER, rc=rc)
!EOC  

!BOE
! The user may also add coordinate data at the same time as adding
! the stagger locations.  The following example
! adds the corner stagger location to {\tt grid}, plus associated 
! coordinate data. Note, the input coordinate arrays (CoordX, CoordY) may be either
! F90 or ESMF Array's (The F90 arrays are restricted to single DE to PET mappings).
! They also need to be of the proper size and rank to coorespond to the Grid. 
!EOE

!BOC 
   call ESMF_GridAddStaggerLoc(grid, staggerLoc=ESMF_STAGGER_CORNER, &
          comp1=CoordX, comp2=CoordY, rc=rc)
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
!\subsubsection{Grid Attributes}
!
! As is typical for ESMF classes, the Grid class allows
! the user to attach name-value pairs to a Grid object. 
! The following adds the attribute "Size" with the value 10. 
!EOE

!BOC
   call ESMF_GridSetAttribute(grid, "Size", 10, rc=rc)
!EOC



!BOE
! \subsubsection{Example: Grid Creation from Existing F90 Arrays}~\label{sec:example5}
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
!EOE

!BOC
   ! Use ESMF framework module
   use ESMF_Mod
   implicit none

   ! Local variables  
   integer:: rc, finalrc
   integer:: myPet, npets, rootPet
   type(ESMF_VM):: vm
   type(ESMF_Grid) :: patchgrid
   real(ESMF_KIND_R4), pointer :: fptrX(:,:),fptrY(:,:)
   integer :: petMap(2,2,1)
!EOC         

      finalrc = ESMF_SUCCESS
      call ESMF_Initialize(vm=vm, rc=rc)
      call ESMF_VMGet(vm, localPet=myPet, petCount=npets, rc=rc)

!BOC
   ! Create a grid with the correct size, shape, and distribution to
   ! hold the data. 

   ! Set petMap
   petMap(:,2,1) = (/3,4/)
   petMap(:,1,1) = (/1,2/)

   ! Create Grid
   grid=ESMF_GridCreateShape(coordTypeKind=ESMF_TYPEKIND_R4, &
                           countsPerDEDim1=(/10,10/), &
                           countsPerDEDim2=(/10,10/), &
                           coordCompDep1=(/1,2/), &
                           coordCompDep2=(/1,2/), &
                           petMap=petMap, &
                           rc=rc)   


   ! Add a center stagger location and at the same time set {\tt grid} to 
   ! reference the coordinate arrays. 
   call ESMF_GridAddStaggerLoc(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                  coordComp1=fptrX, coordComp2=fptrY, rc=rc)
 
   ! We now have all our coordinates so commit as regrid ready.
   call ESMF_GridCommit(grid2D1, ESMF_GRIDSTATUS_REGRID_READY, rc=rc)

!EOC


!BOE
! \subsubsection{Example: Grid Creation from Existing F90 Arrays Using CreateEmpty/Set/Commit}
!
!  This example illustrates the use of the CreateEmpty/Set/Commit paradigm.
!  It repeats the above example using this grid creation technique.
!EOE

!BOC
   ! Use ESMF framework module
   use ESMF_Mod
   implicit none

   ! Local variables  
   integer:: rc, finalrc
   integer:: myPet, npets, rootPet
   type(ESMF_VM):: vm
   type(ESMF_Grid) :: patchgrid
   real(ESMF_KIND_R4), pointer :: fptrX(:,:),fptrY(:,:)
   integer :: petMap(2,2,1)
!EOC         

      finalrc = ESMF_SUCCESS
      call ESMF_Initialize(vm=vm, rc=rc)
      call ESMF_VMGet(vm, localPet=myPet, petCount=npets, rc=rc)

!BOC
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
   grid=ESMF_GridSetShape(coordCompDep1=(/1,2/), coordCompDep2=(/1,2/), &
          rc=rc)   

  ! We now have all our information set in the grid so commit the shape.
  call ESMF_GridCommit(grid2D1, ESMF_GRIDSTATUS_SHAPE_READY, rc=rc)


  ! Add a center stagger location and at the same time set {\tt grid} to 
  ! reference the coordinate arrays. 
  call ESMF_GridAddStaggerLoc(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                  coordComp1=fptrX, coordComp2=fptrY, rc=rc)
 
  ! We now have all our coordinate data so commit as regrid ready.
  call ESMF_GridCommit(grid2D1, ESMF_GRIDSTATUS_REGRID_READY, rc=rc)

!EOC


!BOE
! \subsubsection{Example: 2D+1 Spherical Grid Creation}
!
! This example  illustrates the creation of a 2D spherical Grid with a
! 3rd undistributed dimension.  
! The Grid contains both the center stagger location and a corner
! (i.e. Arakawa B-Grid). The 2D horizaontal grid is distributed across the processors
!  according to  gridDist1 and gridDist2. The number of vertical levels is gridSizeVert.
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
                              coordCompDep1=(/1,2/), &
                              coordCompDep2=(/1,2/), &
                              coordCompDep3=(/3/), &
                              indexflag=ESMF_GLOBAL, &
                              rc=rc)   



  ! Add the center and corner stagger locations. 
  call ESMF_GridAddStaggerLoc(grid2D1,staggerLoc=ESMF_STAGGERLOC_CENTER,rc=rc)
  call ESMF_GridAddStaggerLoc(grid2D1,staggerLoc=ESMF_STAGGERLOC_CORNER,rc=rc)


  ! Set the horizontal coordinates by using the non-ESMF functions
  ! CalcHorzLat, CalcHorzLon to calculate them from the global indices. 
   call ESMF_GridLocalTileGet(grid2D1, coordComp=1, &
          computationalLBound=lbnd, computationalUBound=ubnd, rc=rc)


   ! Get pointers to the coordinate component storage
   call ESMF_GridLocalTileGetData(grid2D1, coordComp=1, coordsLon, rc=rc)
   call ESMF_GridLocalTileGetData(grid2D1, coordComp=2, coordsLat, rc=rc)

   ! Set the coordinates
   do j=lbnd(2),ubnd(2)
   do i=lbnd(1),ubnd(1)
        coordsLon(i,j) = CalcHorzLon(i,j)
        coordsLat(i,j) = CalcHorzLat(i,j)
   enddo
   enddo


   ! Set the vertical  coordinates by using the non-ESMF function
   ! CalcVert.

   ! We actually know teh bounds already, but go through the exercise anyway.
   call ESMF_GridLocalTileGet(grid2D1, coordComp=3, &
          computationalLBound=lbndV, computationalUBound=ubndV, rc=rc)

   ! Get pointers to the coordinate component storage
   call ESMF_GridLocalTileGetData(grid2D1, coordComp=3, coordsVert, rc=rc)

   ! Set the vertical coordinates
   do i=lbndV(1),ubndV(1)
        coordsVert(i) = CalcVert(i)
   enddo


   ! We now have all our coordinates so commit as regrid ready.
   call ESMF_GridCommit(grid2D1, ESMF_GRIDSTATUS_READY, rc=rc)
!EOC





!BOE
!\subsubsection{Creation: Advanced}
!
! Beyond the shortcut methods for creating a grid, there is
! a set of methods which give the user more control over the
! specifics of the grid. 
!
! The first set of these methods allows the user to create a Grid from 
! scratch with maximum control over the 
! specifics of the Grid to be created. The user can create a Grid and specify 
! the dimension, size, type, topology (via DistGrid), and distribution
! (via DistGrid). 
!
! The second advanced method of Grid creation allows the user to construct a
! Grid from a set of Arrays containing coordinate information. These Arrays
! need to have a coherent configuration (same dimension, sizes conforming
! to stagger locations, same distribution, same types, etc.) for the
! call to succeed. The Grid can either be created to contain the actual
! Arrays, or the Grid can be created containing copies of the orignal arrays. 
!
!
!\subsubsection{Creation: Advanced: Size, Shape, and Distribution}
!
! Typically the first step in creating a Grid is to describe its
! size, topology, and distribution. In the ESMF advanced create
! there are three options for doing this. 
!
! {\bf Need to figure out what to do with the below}
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
!EOE



!BOE
!\subsubsection{Stagger Location: Advanced}~\label{sec:usage:staggerloc:adv}
!
! This section discusses some of the more advanced options available during 
! the addition of stagger locations to a Grid. The first of these is
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
   call ESMF_GridAddStaggerLoc(grid, staggerLoc=staggerLoc, rc=rc)

   ! Set Corner
   call ESMF_StaggerLocSet(staggerLoc,where=(/1,1,1,1/),rc=rc)
   call ESMF_GridAddStaggerLoc(grid, staggerLoc=staggerLoc, rc=rc)
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
! (i.e. the center location is surrounded). The default is to add this padding 
! on the positive side, and to only add this padding where needed 
! (e.g. no padding for the center, padding
! on both dimensions for the corner, in only one dimension for the 
! edge in 2D.)  To change these defaults the {\tt staggerLocWidth} arguments 
! can be used to adjust the width and placement of the padding for each
! stagger location. 
!
!EOE

!BOE
The {\tt staggerLocLWidth} and 
{\tt staggerLocUWidth} arguments are both 1D arrays of the
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
   call ESMF_GridAddStaggerLoc(grid, staggerLoc=ESMF_STAGGERLOC_CORNER, &
          staggerLocLWidth=(/0,0/), staggerLocUWidth=(/0,0/), rc=rc)

   call ESMF_GridAddStaggerLoc(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
          staggerLocLWidth=(/1,1/), staggerLocUWidth=(/0,0/), rc=rc)

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
   call ESMF_GridAddStaggerLoc(grid, staggerLoc=ESMF_STAGGERLOC_CORNER, &
          staggerLocAlign=(/+1,+1/), rc=rc)
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
