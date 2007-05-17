! $Id: ESMF_GridCreate.F90,v 1.2 2007/05/17 23:32:02 oehmke Exp $
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
!
#define ESMF_FILENAME "ESMF_GridCreate.F90"
!
!     ESMF Grid Create Module
      module ESMF_GridCreateMod
!
!==============================================================================
!
! This file contains the Grid class definition and all Grid class
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!==============================================================================
!BOPI
! !MODULE: ESMF_GridMod - Grid class
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt ESMF\_Grid} class.  
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_UtilTypesMod    ! ESMF base class
      use ESMF_BaseMod        ! ESMF base class
      use ESMF_IOSpecMod      ! ESMF I/O class
      use ESMF_LogErrMod
      use ESMF_LocalArrayMod  ! ESMF local array class
      
!     NEED TO ADD MORE HERE
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!
! !PUBLIC TYPES:

!------------------------------------------------------------------------------
!

!------------------------------------------------------------------------------
!
! !PUBLIC DATA MEMBERS:

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_GridCreate.F90,v 1.2 2007/05/17 23:32:02 oehmke Exp $'



!==============================================================================

      contains


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreateBox"
!BOP
! !IROUTINE: ESMF_GridCreateBox - Create a new Spherical Grid

! !INTERFACE:
      function ESMF_GridCreateBox(name,coordTypeKind,  &
                        countsPerDEDim1,countsPerDeDim2, countsPerDEDim3, &
                        periodicDim1, periodicDim2, &
                        coordStore, coordIndexOrder, indexflag, gridType,     &
                        haloDepth, petMap, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateBox
!
! !ARGUMENTS:
      character (len=*), intent(in), optional :: name 
       type(ESMF_TypeKind),  intent(in),optional  :: coordTypeKind
       integer,               intent(in),                  :: countsPerDEDim1(:)
       integer,               intent(in),                  :: countsPerDEDim2(:)
       integer,               intent(in),   optional  :: countsPerDEDim3(:)
       logical,               intent(in), optional    :: periodicDim1
       logical,               intent(in), optional    :: periodicDim2
       type(ESMF_IndexFlag),  intent(in),   optional  :: indexflag
       type(ESMF_CoordStore), intent(in),   optional  :: coordStore
       type(ESMF_CoordIndexOrder),  intent(in),   optional  :: coordIndexOrder
       type(ESMF_GridType),  intent(in),   optional  :: gridType
       integer,               intent(in),   optional  :: haloDepth
       integer,               intent(in),   optional  :: petMap(:,:,:)
       integer,               intent(out),  optional  :: rc
!
! !DESCRIPTION:
! Create an {\tt ESMF\_Grid} object. This subroutine constructs a single tile rectangular
! grid. 
!
! The arguments are:
! \begin{description}
! \item[{[name]}]
!          {\tt ESMF\_Grid} name.
! \item[{[coordTypeKind]}] 
!     The type/kind of the grid coordinate data. 
!     If not specified then the type/kind will be 8 byte reals. 
! \item[{countsPerDEDim1}] 
!     This arrays specifies the number of cells per DE for dimension 1
!     for the exclusive region (the center stagger location). Dimension 1
!     is the dimension around the sphere (e.g. longitude).
!     If the array has only one entry, then the dimension is undistributed. 
! \item[countsPerDEDim2}] 
!     This array specifies the number of cells per DE for dimension 2
!     for the exclusive region (center stagger location). Dimension 2
!     is the dimension from pole to pole on the sphere (e.g. latitude).  
!     If the array has only one entry, then the dimension is undistributed. 
! \item[countsPerDEDim3}] 
!     This array specifies the number of cells per DE for dimension 3
!     for the exclusive region (center stagger location).  Dimension 3
!     is the dimension outward from the sphere (e.g. radius).  
!     If not specified  then grid is 2D. Also, If the array has only one entry,
!     then the dimension is undistributed. 
! \item[{[periodicDim1]}] 
!     If TRUE then the grid is connected such that dimension 1 is periodic.
!     If FALSE then there is no such connection. Defaults to FALSE.
! \item[{[periodicDim1]}] 
!     If TRUE then the grid is connected such that dimension 2 is periodic.
!     If FALSE then there is no such connection. Defaults to FALSE.
! \item[{[indexflag]}]
!      Flag that indicates how the DE-local indices are to be defined.
! \item[{[gridType]}]
!      Flag that indicates the type of the grid. If not given, defaults
!       to ESMF\_GRIDTYPE\_SPH.
! \item[{[coordStore]}]
!       Flag to set the coordinate storage array structure. The options
!       are ESMF\_COORDSTORE\_CURV which sets the coordinate
!       arrays to the same rank as the grid, and ESMF\_COORDSTORE\_RECTL
!       which yields grid rank 1D arrays. If not set defaults to  
!       ESMF\_COORDSTORE\_CURVL.
! \item[{[coordIndexOrder]}]
!       Flag to set the index order in the coordinate storage arrays. 
! \item[{[haloDepth}]
!       Sets the depth of the computational padding around the exclusive
!       regions on each DE.  The actual value for any edge is the maximum
!       between the halo and stagger location padding. If not specified 
!       this is set to 0.
! \item[{[petMap]}]
!       Sets the mapping of pets to the created DEs. This 3D
!       should be of size size(countsPerDEDim1)xsize(countsPerDEDim2)x
!       size(countsPerDEDim3). If countsPerDEDim3 isn't present, then
!       the last dimension is of size 1.   
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      end function ESMF_GridCreateBox

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreateSphere"
!BOP
! !IROUTINE: ESMF_GridCreateSphere - Create a new Spherical Grid

! !INTERFACE:
      function ESMF_GridCreateSphere(name,coordTypeKind,  &
                        countsPerDEDim1,countsPerDeDim2, countsPerDEDim3, &
                        coordStore, coordIndexOrder, indexflag, gridType,     &
                        haloDepth, petMap, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateSphere
!
! !ARGUMENTS:
      character (len=*), intent(in), optional :: name 
       type(ESMF_TypeKind),  intent(in),optional  :: coordTypeKind
       integer,               intent(in),                  :: countsPerDEDim1(:)
       integer,               intent(in),                  :: countsPerDEDim2(:)
       integer,               intent(in),   optional  :: countsPerDEDim3(:)
       type(ESMF_IndexFlag),  intent(in),   optional  :: indexflag
       type(ESMF_CoordStore), intent(in),   optional  :: coordStore
       type(ESMF_CoordIndexOrder),  intent(in),   optional  :: coordIndexOrder
       type(ESMF_GridType),  intent(in),   optional  :: gridType
       integer,               intent(in),   optional  :: haloDepth
       integer,               intent(in),   optional  :: petMap(:,:,:)
       integer,               intent(out),  optional  :: rc
!
! !DESCRIPTION:
! Create an {\tt ESMF\_Grid} object. This subroutine constructs a single tile spherical
! grid. A Spherical grid is connected across both poles and along the branch cut. 
!
! The arguments are:
! \begin{description}
! \item[{[name]}]
!          {\tt ESMF\_Grid} name.
! \item[{countsPerDEDim1}] 
!     This arrays specifies the number of cells per DE for dimension 1
!     for the exclusive region (the center stagger location). Dimension 1
!     is the dimension around the sphere (e.g. longitude).
!     If the array has only one entry, then the dimension is undistributed. 
! \item[countsPerDEDim2}] 
!     This array specifies the number of cells per DE for dimension 2
!     for the exclusive region (center stagger location). Dimension 2
!     is the dimension from pole to pole on the sphere (e.g. latitude).  
!     If the array has only one entry, then the dimension is undistributed. 
! \item[countsPerDEDim3}] 
!     This array specifies the number of cells per DE for dimension 3
!     for the exclusive region (center stagger location).  Dimension 3
!     is the dimension outward from the sphere (e.g. radius).  
!     If not specified  then grid is 2D. Also, If the array has only one entry,
!     then the dimension is undistributed. 
! \item[{[coordTypeKind]}] 
!     The type/kind of the grid coordinate data. 
!     If not specified then the type/kind will be 8 byte reals. 
! \item[{[indexflag]}]
!      Flag that indicates how the DE-local indices are to be defined.
! \item[{[gridType]}]
!      Flag that indicates the type of the grid. If not given, defaults
!       to ESMF\_GRIDTYPE\_SPH.
! \item[{[coordStore]}]
!       Flag to set the coordinate storage array structure. The options
!       are ESMF\_COORDSTORE\_CURV which sets the coordinate
!       arrays to the same rank as the grid, and ESMF\_COORDSTORE\_RECTL
!       which yields grid rank 1D arrays. If not set defaults to  
!       ESMF\_COORDSTORE\_CURVL.
! \item[{[coordIndexOrder]}]
!       Flag to set the index order in the coordinate storage arrays. 
! \item[{[haloDepth}]
!       Sets the depth of the computational padding around the exclusive
!       regions on each DE.  The actual value for any edge is the maximum
!       between the halo and stagger location padding. If not specified 
!       this is set to 0.
! \item[{[petMap]}]
!       Sets the mapping of pets to the created DEs. This 3D
!       should be of size size(countsPerDEDim1)xsize(countsPerDEDim2)x
!       size(countsPerDEDim3). If countsPerDEDim3 isn't present, then
!       the last dimension is of size 1.   
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      end function ESMF_GridCreateSphere



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreateTripole"
!BOP
! !IROUTINE: ESMF_GridCreateTripole - Create a new Spherical Grid

! !INTERFACE:
      function ESMF_GridCreateTripole(name,coordTypeKind,  &
                        countsPerDEDim1,countsPerDeDim2, countsPerDEDim3, &
                        coordStore, coordIndexOrder, indexflag, gridType,     &
                        haloDepth, petMap, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateTripole
!
! !ARGUMENTS:
      character (len=*), intent(in), optional :: name 
       type(ESMF_TypeKind),  intent(in),optional  :: coordTypeKind
       integer,               intent(in),                  :: countsPerDEDim1(:)
       integer,               intent(in),                  :: countsPerDEDim2(:)
       integer,               intent(in),   optional  :: countsPerDEDim3(:)
       type(ESMF_IndexFlag),  intent(in),   optional  :: indexflag
       type(ESMF_CoordStore), intent(in),   optional  :: coordStore
       type(ESMF_CoordIndexOrder),  intent(in),   optional  :: coordIndexOrder
       type(ESMF_GridType),  intent(in),   optional  :: gridType
       integer,               intent(in),   optional  :: haloDepth
       integer,               intent(in),   optional  :: petMap(:,:,:)
       integer,               intent(out),  optional  :: rc
!
! !DESCRIPTION:
! Create an {\tt ESMF\_Grid} object. This subroutine constructs a single tile tripole
! grid. A Tripole grid is connected across both poles and along the branch cut. 
!
! The arguments are:
! \begin{description}
! \item[{[name]}]
!          {\tt ESMF\_Grid} name.
! \item[{countsPerDEDim1}] 
!     This arrays specifies the number of cells per DE for dimension 1
!     for the exclusive region (the center stagger location). Dimension 1
!     is the dimension around the sphere (e.g. longitude).
!     If the array has only one entry, then the dimension is undistributed. 
! \item[countsPerDEDim2}] 
!     This array specifies the number of cells per DE for dimension 2
!     for the exclusive region (center stagger location). Dimension 2
!     is the dimension from pole to pole on the sphere (e.g. latitude).  
!     If the array has only one entry, then the dimension is undistributed. 
! \item[countsPerDEDim3}] 
!     This array specifies the number of cells per DE for dimension 3
!     for the exclusive region (center stagger location).  Dimension 3
!     is the dimension outward from the sphere (e.g. radius).  
!     If not specified  then grid is 2D. Also, If the array has only one entry,
!     then the dimension is undistributed. 
! \item[{[coordTypeKind]}] 
!     The type/kind of the grid coordinate data. 
!     If not specified then the type/kind will be 8 byte reals. 
! \item[{[indexflag]}]
!      Flag that indicates how the DE-local indices are to be defined.
! \item[{[gridType]}]
!      Flag that indicates the type of the grid. If not given, defaults
!       to ESMF\_GRIDTYPE\_SPH.
! \item[{[coordStore]}]
!       Flag to set the coordinate storage array structure. The options
!       are ESMF\_COORDSTORE\_CURV which sets the coordinate
!       arrays to the same rank as the grid, and ESMF\_COORDSTORE\_RECTL
!       which yields grid rank 1D arrays. If not set defaults to  
!       ESMF\_COORDSTORE\_CURVL.
! \item[{[coordIndexOrder]}]
!       Flag to set the index order in the coordinate storage arrays. 
! \item[{[haloDepth}]
!       Sets the depth of the computational padding around the exclusive
!       regions on each DE.  The actual value for any edge is the maximum
!       between the halo and stagger location padding. If not specified 
!       this is set to 0.
! \item[{[petMap]}]
!       Sets the mapping of pets to the created DEs. This 3D
!       should be of size size(countsPerDEDim1)xsize(countsPerDEDim2)x
!       size(countsPerDEDim3). If countsPerDEDim3 isn't present, then
!       the last dimension is of size 1.   
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      end function ESMF_GridCreateTripole



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreate"
!BOP
! !IROUTINE: ESMF_GridCreate - Create a new grid 

! !INTERFACE:
  ! Private name; call using ESMF_GridCreate()
      function ESMF_GridCreateFromCntsPerDE(name,coordTypeKind,countsPerDE, dimmap, &
                         lbounds, ubounds, coordCompRanks, &
                        coordCompDimMap, staggerLocs, &
                        staggerLocLWidth, staggerLocUWidth, &
                        staggerLocAligns,indexflag, gridType, noData,
                        computationalLWidth, computationalUWidth, petMap, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreate
!
! !ARGUMENTS:
       character (len=*), intent(in), optional :: name
       type (ESMF_StaggerLoc), intent(in),optional :: staggerLocs(:)
       type(ESMF_TypeKind),  intent(in),optional  :: coordTypeKind
       integer,               intent(in),                 ::   countPerDE(:,:)
       integer,               intent(in),   optional  :: dimmap(:)
       integer,               intent(in),   optional  :: lbounds(:)
       integer,               intent(in),   optional  :: ubounds(:)
       integer,               intent(in),   optional  :: coordCompRanks(:)
       integer,               intent(in),   optional  :: coordCompDimMap(:,:)
       integer,               intent(in),   optional  :: staggerLocLWidth(:,:)
       integer,               intent(in),   optional  :: staggerLocUWidth(:,:)
       integer,               intent(in),   optional  :: staggerLocAligns(:,:)
       type(ESMF_IndexFlag),  intent(in),   optional  :: indexflag
       integer,               intent(in),   optional  :: gridType
       logical,               intent(in),   optional  :: noData
       integer,               intent(in),   optional  :: computationalLWidth(:)
       integer,               intent(in),   optional  :: computationalUWidth(:)
       integer,               intent(in),   optional  :: petMap(:)
       integer,               intent(out), optional  :: rc
!
! !DESCRIPTION:
! Create an {\tt ESMF\_Grid} object. This subroutine constructs a 
! grid of size {\tt minIndex} to {\tt maxIndex}  with the default distGrid configuration.
! The grid will contain a set of stagger locations  as defined by the parameter
! {\tt staggerLocs}.  
!
! The arguments are:
! \begin{description}
! \item[{[name]}]
!          {\tt ESMF\_Grid} name.
! \item[{[coordTypeKind]}] 
!     The type/kind of the grid coordinate data. 
!     If not specified then the type/kind will be 8 byte reals. 
! \item[{countPerDE}] 
!        This is a 2D array. The first dimension is the size of the number
!         of distributed dimensions in the grid. The second is the size
!         of the maximum number of DE's in any dimension. Each entry
!         tells the number of points for that dimension for that DE. 
!         When a dimension has less than the maximum number of DEs then 0
!         should be used to fill the remaining slots. 
! \item[{[regDecomp]}] 
!      List that has the same number of elements as {\tt maxIndex}.
!      Each entry is the number of decounts for that dimension.
!      If not specified, the default decomposition will be deCountx1x1..x1. 
! \item[{[dimmap]}] 
!      List that has size(maxIndex) elements.
!      The elements map each dimension described by {\tt maxIndex} to a dimension 
!      in the grid.  (i.e. the values should range from 1 to gridrank). If not specified, 
!       the default is to map the dimensions against the lower dimensions of the
!       grid in sequence. 
! \item[{[lbounds]}] 
!      Lower bounds for tensor array dimensions. If {\tt ubounds}
!      is specified, but {\tt lbounds} is not then the lower bounds
!      default to {1,1,1,...}
! \item[{[ubounds]}] 
!      Upper bounds for tensor array dimensions.
! \item[{[coordCompRanks]}]
!      List that has as many elements as the grid rank .
!      Gives the dimension of each component (e.g. x) array. This is 
!      to allow factorization of the coordinate arrays. If not specified
!      all arrays are the same size as the grid. 
! \item[{[coordCompDimMap]}]
!      2D list of size grid rank x grid rank. This array describes the
!      map of each component array's dimensions onto the grids
!      dimensions. Each entry {\tt coordCompDimMap(i,j)} tells which
!      grid dimension component i's, jth dimension maps to. 
!      Note that if j is bigger than {\tt coordCompRanks(i)} than its ignored.        
! \item[{[staggerLocs]}]
!        The stagger locations which the newly created grid should contain.
!         If not specified, defaults to just the center stagger location. 
! \item[{[staggerLocLWidth]}] 
!      This array is of size grid rank by size(staggerLocs).
!      For each stagger location, it specifies the lower corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[staggerLocUWidth]}] 
!      This array is of size  grid rank by size(staggerLocs).
!      For each stagger location, it specifies the upper corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[staggerLocAligns]}] 
!      This array is of size  grid rank by size(staggerLocs).
!      For each stagger location, it specifies which element
!      has the same index value as the center. For example, 
!      for a 2D cell with corner stagger it specifies which 
!      of the 4 corners has the same index as the center. 
!      If this is set and staggerLocUWidth is not,
!      this determines the default array padding for a stagger. 
!      If not set, then this defaults to all negative. (e.g. 
!      The most negative part of the stagger in a cell is aligned with the 
!      center and the padding is all on the postive side.) 
! \item[{[indexflag]}]
!      Flag that indicates how the DE-local indices are to be defined.
! \item[{[gridType]}]
!      Flag that indicates the type of the grid. If not given, defaults
!       to ESMF\_GRIDTYPE\_UNKNOWN.
! \item[{[noData]}]
!      Flag that indicates if the internal arrays should be allocated
!      to hold the coordinate data. This could be set to .true. if the user
!      wishes to load their coordinate arrays later. Defaults to .false.
!      (i.e. the arrays are allocated by default). (CURRENTLY UNIMPLEMENTED)
! \item[{[computationalLWidth]}]
!       Array of the same size as {\tt maxIndex}. 
!       Sets the size of the computational padding around the exclusive
!       regions on each DE. If {\tt staggerLocLWidth} is also set
!       the actual value for any edge is the maximum between the two. 
! \item[{[computationalUWidth]}]
!       Array of the same size as {\tt maxIndex}. 
!       Sets the size of the computational padding around the exclusive
!       regions on each DE. If {\tt staggerLocUWidth} is also set
!       the actual value for any edge is the maximum between the two. 
! \item[{[petMap]}]
!       Sets the mapping of pets to the created DEs. This array should be
!       of the same size as the number of DEs implied by {\tt countePerDE}.
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      end function ESMF_GridCreateFromExtents

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreate"
!BOP
! !IROUTINE: ESMF_GridCreate - Create a new grid 

! !INTERFACE:
  ! Private name; call using ESMF_GridCreate()
      function ESMF_GridCreateFromExtents(name,coordTypeKind, minIndex,maxIndex, dimmap, &
                        regDecomp, lbounds, ubounds, coordCompRanks, &
                        coordCompDimMap, staggerLocs, &
                        staggerLocLWidth, staggerLocUWidth, &
                        staggerLocAligns,indexflag, gridType, noData,
                        computationalLWidth, computationalUWidth, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreate
!
! !ARGUMENTS:
       character (len=*), intent(in), optional :: name
       type (ESMF_StaggerLoc), intent(in),optional :: staggerLocs(:)
       type(ESMF_TypeKind),  intent(in),optional  :: coordTypeKind
       integer,               intent(in),   optional  :: minIndex(:)
       integer,               intent(in),             :: maxIndex(:)
       integer,               intent(in),   optional  :: regDecomp(:)
       integer,               intent(in),   optional  :: dimmap(:)
       integer,               intent(in),   optional  :: lbounds(:)
       integer,               intent(in),   optional  :: ubounds(:)
       integer,               intent(in),   optional  :: coordCompRanks(:)
       integer,               intent(in),   optional  :: coordCompDimMap(:,:)
       integer,               intent(in),   optional  :: staggerLocLWidth(:,:)
       integer,               intent(in),   optional  :: staggerLocUWidth(:,:)
       integer,               intent(in),   optional  :: staggerLocAligns(:,:)
       type(ESMF_IndexFlag),  intent(in),   optional  :: indexflag
       integer,               intent(in),   optional  :: gridType
       logical,               intent(in),   optional  :: noData
       integer,               intent(in),   optional  :: computationalLWidth(:)
       integer,               intent(in),   optional  :: computationalUWidth(:)
       integer,               intent(out),  optional  :: rc
!
! !DESCRIPTION:
! Create an {\tt ESMF\_Grid} object. This subroutine constructs a 
! grid of size {\tt minIndex} to {\tt maxIndex}  with the default distGrid configuration.
! The grid will contain a set of stagger locations  as defined by the parameter
! {\tt staggerLocs}.  
!
! The arguments are:
! \begin{description}
! \item[{[name]}]
!          {\tt ESMF\_Grid} name.
! \item[{[coordTypeKind]}]
!     The type/kind of the grid coordinate data. 
!     If not specified then the type/kind will be 8 byte reals.  
! \item[{[minIndex]}] 
!      The bottom extent of the grid array. If not given then the value defaults
!       to (1,1,1...).
! \item[{maxIndex}] 
!        The upper extent of the grid array.
! \item[{[regDecomp]}] 
!      List that has the same number of elements as {\tt maxIndex}.
!      Each entry is the number of decounts for that dimension.
!      If not specified, the default decomposition will be deCountx1x1..x1. 
! \item[{[dimmap]}] 
!      List that has size(maxIndex) elements.
!      The elements map each dimension described by {\tt maxIndex} to a dimension 
!      in the grid.  (i.e. the values should range from 1 to gridrank). If not specified, 
!       the default is to map the dimensions against the lower dimensions of the
!       grid in sequence. 
! \item[{[lbounds]}] 
!      Lower bounds for tensor array dimensions. If {\tt ubounds}
!      is specified, but {\tt lbounds} is not then the lower bounds
!      default to {1,1,1,...}
! \item[{[ubounds]}] 
!      Upper bounds for tensor array dimensions.
! \item[{[coordCompRanks]}]
!      List that has as many elements as the grid rank .
!      Gives the dimension of each component (e.g. x) array. This is 
!      to allow factorization of the coordinate arrays. If not specified
!      all arrays are the same size as the grid. 
! \item[{[coordCompDimMap]}]
!      2D list of size grid rank x grid rank. This array describes the
!      map of each component array's dimensions onto the grids
!      dimensions. Each entry {\tt coordCompDimMap(i,j)} tells which
!      grid dimension component i's, jth dimension maps to. 
!      Note that if j is bigger than {\tt coordCompRanks(i)} than its ignored.        
! \item[{[staggerLocs]}]
!        The stagger locations which the newly created grid should contain.
!         If not specified, defaults to just the center stagger location. 
! \item[{[staggerLocLWidth]}] 
!      This array is of size grid rank by size(staggerLocs).
!      For each stagger location, it specifies the lower corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[staggerLocUWidth]}] 
!      This array is of size  grid rank by size(staggerLocs).
!      For each stagger location, it specifies the upper corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[staggerLocAligns]}] 
!      This array is of size  grid rank by size(staggerLocs).
!      For each stagger location, it specifies which element
!      has the same index value as the center. For example, 
!      for a 2D cell with corner stagger it specifies which 
!      of the 4 corners has the same index as the center. 
!      If this is set and staggerLocUWidth is not,
!      this determines the default array padding for a stagger. 
!      If not set, then this defaults to all negative. (e.g. 
!      The most negative part of the stagger in a cell is aligned with the 
!      center and the padding is all on the postive side.) 
! \item[{[indexflag]}]
!      Flag that indicates how the DE-local indices are to be defined.
! \item[{[gridType]}]
!      Flag that indicates the type of the grid. If not given, defaults
!       to ESMF\_GRIDTYPE\_UNKNOWN.
! \item[{[noData]}]
!      Flag that indicates if the internal arrays should be allocated
!      to hold the coordinate data. This could be set to .true. if the user
!      wishes to load their coordinate arrays later. Defaults to .false.
!      (i.e. the arrays are allocated by default). (CURRENTLY UNIMPLEMENTED)
! \item[{[computationalLWidth]}]
!       Array of the same size as {\tt maxIndex}. 
!       Sets the size of the computational padding around the exclusive
!       regions on each DE. If {\tt staggerLocLWidth} is also set
!       the actual value for any edge is the maximum between the two. 
! \item[{[computationalUWidth]}]
!       Array of the same size as {\tt maxIndex}. 
!       Sets the size of the computational padding around the exclusive
!       regions on each DE. If {\tt staggerLocUWidth} is also set
!       the actual value for any edge is the maximum between the two. 
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      end function ESMF_GridCreateFromExtents

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreate"
!BOP
! !IROUTINE: ESMF_GridCreate - Create a new grid 

! !INTERFACE:
  ! Private name; call using ESMF_GridCreate()
      function ESMF_GridCreateFromDistGrid(name,coordTypeKind,distgrid, dimmap, &
                        lbounds, ubounds, coordCompRanks, coordCompDimMap, &
                        staggerLocs, staggerLocLWidth, staggerLocUWidth, &
                        staggerLocAligns, indexflag, gridType, noData, &
                        computationalLWidth, computationalUWidth, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreate
!
! !ARGUMENTS:
       character (len=*), intent(in), optional :: name
       type (ESMF_StaggerLoc), intent(in),   optional :: staggerLocs(:)
       type(ESMF_TypeKind),  intent(in),   optional  :: coordTypeKind
       type(ESMF_DistGrid),   intent(in)              :: distgrid
       integer,               intent(in),   optional  :: dimmap(:)
       integer,               intent(in),   optional  :: lbounds(:)
       integer,               intent(in),   optional  :: ubounds(:)
       integer,               intent(in),   optional  :: coordCompRanks(:)
       integer,               intent(in),   optional  :: coordCompDimMap(:,:)
       integer,               intent(in),   optional  :: staggerLocLWidth(:,:)
       integer,               intent(in),   optional  :: staggerLocUWidth(:,:)
       integer,               intent(in),   optional  :: staggerLocAligns(:,:)
       type(ESMF_IndexFlag),  intent(in),   optional  :: indexflag
       integer,               intent(in),   optional  :: gridType
       logical,               intent(in),   optional  :: noData
       integer,               intent(in),   optional  :: computationalLWidth(:)
       integer,               intent(in),   optional  :: computationalUWidth(:)
       integer,               intent(out),  optional  :: rc
!
! !DESCRIPTION:
! Create an {\tt ESMF\_Grid} object. This subroutine constructs a 
! grid as specified by a distGrid. Optional {\tt lbound} and {\tt ubound}
! arguments can be used to specify extra tensor dimensions. 
! The grid will contain a set of stagger locations  as defined by the parameter
! {\tt staggerLocs}.  
!
! The arguments are:
! \begin{description}
! \item[{[name]}]
!          {\tt ESMF\_Grid} name.
! \item[{[coordTypeKind]}] 
!     The type/kind of the grid coordinate data. 
!     If not specified then the type/kind will be 8 byte reals.  
! \item[distgrid]
!      {\tt ESMF\_DistGrid} object that describes how the array is decomposed and
!      distributed over DEs. The dimCount of distgrid must be smaller or equal
!      to the grid rank, otherwise a runtime ESMF error will be
!      raised.
! \item[{[dimmap]}] 
!      List that has as many elements as indicated by distGrid's dimCount value.
!      The elements map each dimension of distgrid to a dimension in the grid.
!       (i.e. the values should range from 1 to gridrank). If not specified, the default
!       is to map all of distgrid's dimensions against the lower dimensions of the
!       grid in sequence. 
! \item[{[lbounds]}] 
!      Lower bounds for tensor array dimensions.
! \item[{[ubounds]}] 
!      Upper bounds for tensor array dimensions.
! \item[{[coordCompRanks]}]
!      List that has as many elements as the grid rank .
!      Gives the dimension of each component (e.g. x) array. This is 
!      to allow factorization of the coordinate arrays. If not specified
!      all arrays are the same size as the grid. 
! \item[{[coordCompDimMap]}]
!      2D list of size grid rank x grid rank. This array describes the
!      map of each component array's dimensions onto the grids
!      dimensions. Each entry {\tt coordCompDimMap(i,j)} tells which
!      grid dimension component i's, jth dimension maps to. 
!      Note that if j is bigger than {\tt coordCompRanks(i)} than its ignored.        
! \item[{[staggerLocs]}]
!         The stagger locations which the newly created grid should contain. 
!         If not specified, defaults to just the center stagger location. 
! \item[{[staggerLocLWidth]}] 
!      This array is of size grid rank by size(staggerLocs).
!      For each stagger location, it specifies the lower corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[staggerLocUWidth]}] 
!      This array is of size  grid rank by size(staggerLocs).
!      For each stagger location, it specifies the upper corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[staggerLocAligns]}] 
!      This array is of size  grid rank by size(staggerLocs).
!      For each stagger location, it specifies which element
!      has the same index value as the center. For example, 
!      for a 2D cell with corner stagger it specifies which 
!      of the 4 corners has the same index as the center. 
!      If this is set and staggerLocUWidth is not,
!      this determines the default array padding for a stagger. 
!      If not set, then this defaults to all negative. (e.g. 
!      The most negative part of the stagger in a cell is aligned with the 
!      center and the padding is all on the postive side.) 
! \item[{[indexflag]}]
!      Flag that indicates how the DE-local indices are to be defined.
! \item[{[gridType]}]
!      Flag that indicates the type of the grid. If not given, defaults
!       to ESMF\_GRIDTYPE\_UNKNOWN.
! \item[{[noData]}]
!      Flag that indicates if the internal arrays should be allocated
!      to hold the coordinate data. This could be set to .true. if the user
!      wishes to load their coordinate arrays later. Defaults to .false.
!      (i.e. the arrays are allocated by default). (CURRENTLY UNIMPLEMENTED)
! \item[{[computationalLWidth]}]
!       Array of the same size as the {\tt distGrid} dimcount. 
!       Sets the size of the computational padding around the exclusive
!       regions on each DE. If {\tt staggerLocLWidth} is also set
!       the actual value for any edge is the maximum between the two. 
! \item[{[computationalUWidth]}]
!       Array of the same size as the {\tt distGrid} dimcount. 
!       Sets the size of the computational padding around the exclusive
!       regions on each DE. If {\tt staggerLocUWidth} is also set
!       the actual value for any edge is the maximum between the two. 
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
! !REQUIREMENTS:  TODO


      end function ESMF_GridCreateFromExtents

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreateFromArrays"
!BOP
! !IROUTINE: ESMF_GridCreateFromArrays - Create a new Grid from a set of Arrays

! !INTERFACE:
  ! Private name; call using ESMF_GridCreate()
      function ESMF_GridCreateFromArrays(name, arrays, staggerLocs, &
          staggerLocAligns, coordCompDimMap, doCopy, gridType, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateFromArrays
!
! !ARGUMENTS:
      character (len=*),     intent(in), optional :: name
      type (ESMF_Array),     intent(in)           :: arrays(:,:)
      type (ESMF_StaggerLoc), intent(in)          :: staggerLocs(:)
      integer,               intent(in), optional :: staggerLocAligns(:,:)
      integer,               intent(in), optional :: coordCompDimMap(:,:)
      integer,               intent(in), optional :: gridType 
      type(ESMF_CopyFlag),   intent(in), optional :: docopy
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Creates a new grid from a set of arrays. The arrays define the 
!      coordinates of each stagger location. The arrays should have the
!      proper size and dimension to represent the stagger locations and 
!      a coherent grid. 
!
!     The arguments are:
!     \begin{description}
!     \item[{[name]}]
!          {\tt ESMF\_Grid} name.
!     \item[{arrays}]
!          Two dimensional array where the first dimension is of size grid rank and
!          the second is the size of the number of stagger locations. This array
!          contains an ESMF Array for each grid component dimension and stagger location.
!     \item[{[staggerLocs]}]
!          The stagger locations of the arrays.
!    \item[{[staggerLocAligns]}] 
!      This array is of size  grid rank by size(staggerLocs).
!      For each stagger location, it specifies which element
!      has the same index value as the center. For example, 
!      for a 2D cell with corner stagger it specifies which 
!      of the 4 corners has the same index as the center. 
!      If this is set and staggerLocUWidth is not,
!      this determines the default array padding for a stagger. 
!      If not set, then this defaults to all negative. (e.g. 
!      The most negative part of the stagger in a cell is aligned with the 
!      center and the padding is all on the postive side.) 
!     \item[{[coordCompDimMap]}]
!      2D list of size grid rank x grid rank. This array describes the
!      map of each component array's dimensions onto the grids
!      dimensions. 
!     \item[{[doCopy]}]
!          Default to {\tt ESMF\_DATA\_REF}, makes the new grid reference the passed
!          in arrays. If set to {\tt ESMF\_DATA\_COPY} this routine makes a copy
!          of the arrays.
!    \item[{[gridType]}]
!      Flag that indicates the type of the grid. If not given, defaults
!       to ESMF\_GRIDTYPE\_UNKNOWN.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  TODO


      end function ESMF_GridCreateFromArrays


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreateLikeArray"
!BOP
! !IROUTINE: ESMF_GridCreateLikeArray - Create a new Grid with a similar shape to an array

! !INTERFACE:
  ! Private name; call using ESMF_GridCreate()
      function ESMF_GridCreateLikeArray(name, refStaggerLoc, array, &
         staggerLocs, coordCompRanks, coordCompDimMap, &
         staggerLocLWidth, staggerLocUWidth, &
         staggerLocAligns, gridType, &
         computationalLWidth, computationalUWidth, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateLikeArray
!
! !ARGUMENTS:
       character (len=*),     intent(in),    optional :: name
       type (ESMF_StaggerLoc),intent(in),    optional :: refStaggerLoc
       integer,               intent(in),   optional  :: coordCompDimMap(:,:)
       type (ESMF_Array),     intent(in)              :: array
       type (ESMF_StaggerLoc), intent(in)             :: staggerLocs(:)
       integer,               intent(in),   optional  :: coordCompRanks(:)
       integer,               intent(in),   optional  :: staggerLocLWidth(:,:)
       integer,               intent(in),   optional  :: staggerLocUWidth(:,:)
       integer,               intent(in),   optional  :: staggerLocAligns(:,:)
       integer,               intent(in),   optional  :: gridType
       integer,               intent(in),   optional  :: computationalLWidth(:,:)
       integer,               intent(in),   optional  :: computationalUWidth(:,:)
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Creates a new grid based on an array. The coordinate arrays for stagger location
!      {\tt refStaggerLoc} will be a copy of array. The rest of the stagger location's 
!      arrays will be based on array, but will vary slightly depending on the differences 
!      between them and refStaggerLoc. 
!
!     The arguments are:
!     \begin{description}
!     \item[{[name]}]
!          {\tt ESMF\_Grid} name.
! \item[{[refStaggerLoc]}]
!       The stagger location which to make the same size, shape, and distribution as array.
!       The default value for this parameter is the cell center. 
! \item[{array}]
!          The array whose size and shape the grid should copy. 
! \item[{staggerLocs}]
!          A list of all the stagger locations the grid should contain.
! \item[{[coordCompRanks]}]
!      List that has as many elements as the grid rank .
!      Gives the dimension of each component (e.g. x) array. This is 
!      to allow factorization of the coordinate arrays. If not specified
!      all arrays are the same size as the grid. 
! \item[{[coordCompDimMap]}]
!      2D list of size grid rank x grid rank. This array describes the
!      map of each component array's dimensions onto the grids
!      dimensions. Each entry {\tt coordCompDimMap(i,j)} tells which
!      grid dimension component i's, jth dimension maps to. 
!      Note that if j is bigger than {\tt coordCompRanks(i)} than its ignored.        
! \item[{[staggerLocLWidth]}] 
!      This array is of size grid rank by size(staggerLocs).
!      For each stagger location, it specifies the lower corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[staggerLocUWidth]}] 
!      This array is of size  grid rank by size(staggerLocs).
!      For each stagger location, it specifies the upper corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[staggerLocAligns]}] 
!      This array is of size  grid rank by size(staggerLocs).
!      For each stagger location, it specifies which element
!      has the same index value as the center. For example, 
!      for a 2D cell with corner stagger it specifies which 
!      of the 4 corners has the same index as the center. 
!      If this is set and staggerLocUWidth is not,
!      this determines the default array padding for a stagger. 
!      If not set, then this defaults to all negative. (e.g. 
!      The most negative part of the stagger in a cell is aligned with the 
!      center and the padding is all on the postive side.) 
! \item[{[gridType]}]
!      Flag that indicates the type of the grid. If not given, defaults
!       to ESMF\_GRIDTYPE\_UNKNOWN.
! \item[{[computationalLWidth]}]
!       Array of the same size as the {\tt distGrid} dimcount. 
!       Sets the size of the computational padding around the exclusive
!       regions on each DE. If {\tt staggerLocLWidth} is also set
!       the actual value for any edge is the maximum between the two. 
! \item[{[computationalUWidth]}]
!       Array of the same size as the {\tt distGrid} dimcount. 
!       Sets the size of the computational padding around the exclusive
!       regions on each DE. If {\tt staggerLocUWidth} is also set
!       the actual value for any edge is the maximum between the two. 
! \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
! !REQUIREMENTS:  TODO


      end function ESMF_GridCreateLikeArray


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreateEmpty"
!BOP
! !IROUTINE: ESMF_GridCreateEmpty - Create a new grid 

! !INTERFACE:
     function ESMF_GridCreateEmpty(rc)
!
! !RETURN VALUE:
     type(ESMF_Grid) :: ESMF_GridCreateEmpty
!
! !ARGUMENTS:
       integer,               intent(out),  optional  :: rc
!
! !DESCRIPTION:
! Partially create an {\tt ESMF\_Grid} object. This function allocates 
! an {\tt ESMF\_Grid} object, but doesn't allocate any coordinate storage or other
! internal structures. The {\tt ESMF\_GridSet} and {\tt ESMF\_GridAddStaggerLoc} calls
! can be used to set the values in the grid object. Before using the grid,
! {\tt ESMF\_GridCommit} needs to be called to validate the internal state and
! construct internal structures. 
!
! The arguments are:
! \begin{description}
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      end function ESMF_GridCreateEmpty


      end module ESMF_GridCreateMod

