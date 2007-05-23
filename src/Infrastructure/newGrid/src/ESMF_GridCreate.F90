! $Id: ESMF_GridCreate.F90,v 1.8 2007/05/23 23:06:45 oehmke Exp $
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
      '$Id: ESMF_GridCreate.F90,v 1.8 2007/05/23 23:06:45 oehmke Exp $'



!==============================================================================

      contains


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreate"
!BOP
! !IROUTINE: ESMF_GridCreate - Create a new grid 

! !INTERFACE:
  ! Private name; call using ESMF_GridCreate()
      function ESMF_GridCreateArb(name,coordTypeKind, minIndex,maxIndex, localIndices, &
                        dimmap, blockDecomp, lbounds, ubounds, coordCompRanks, &
                        coordCompDimMap, staggerLocs, &
                        staggerLocLWidth, staggerLocUWidth, &
                        staggerLocAligns,indexflag, gridType, noData,
                        computationalLWidth, computationalUWidth, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateBlk
!
! !ARGUMENTS:
       character (len=*), intent(in), optional :: name
       type (ESMF_StaggerLoc), intent(in),optional :: staggerLocs(:)
       type(ESMF_TypeKind),  intent(in),optional  :: coordTypeKind
       integer,               intent(in),   optional  :: minIndex(:)
       integer,               intent(in),             :: maxIndex(:)
       integer,               intent(in),             :: localIndices(:,:)
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
! \item[{[localIndices]}] 
!      2D array whose first dimension is the same rank as the distGrid, and whose
!      second dimension is the size of the number of grid locations distributed
!      to this grid. {\tt localIndices} contains a list of all the glocal index tuples
!      which should reside on this processor. 
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

      end function ESMF_GridCreateArb


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreate"
!BOP
! !IROUTINE: ESMF_GridCreate - Create a new grid 

! !INTERFACE:
  ! Private name; call using ESMF_GridCreate()
      function ESMF_GridCreateBlk(name,coordTypeKind, minIndex,maxIndex, dimmap, &
                        blockDecomp, lbounds, ubounds, coordCompRanks, &
                        coordCompDimMap, staggerLocs, &
                        staggerLocLWidth, staggerLocUWidth, &
                        staggerLocAligns,indexflag, gridType, noData,
                        computationalLWidth, computationalUWidth, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateBlk
!
! !ARGUMENTS:
       character (len=*), intent(in), optional :: name
       type (ESMF_StaggerLoc), intent(in),optional :: staggerLocs(:)
       type(ESMF_TypeKind),  intent(in),optional  :: coordTypeKind
       integer,               intent(in),   optional  :: minIndex(:)
       integer,               intent(in),             :: maxIndex(:)
       integer,               intent(in),   optional  :: blockDecomp(:)
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
! \item[{[blockDecomp]}] 
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

      end function ESMF_GridCreateBlk


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreate"
!BOP
! !IROUTINE: ESMF_GridCreate - Create a new grid 

! !INTERFACE:
  ! Private name; call using ESMF_GridCreate()
   function ESMF_GridCreateReg(name,coordTypeKind, minIndex, countsPerDE, dimmap, &
                         lbounds, ubounds, coordCompRanks, &
                        coordCompDimMap, staggerLocs, &
                        staggerLocLWidth, staggerLocUWidth, &
                        staggerLocAligns,indexflag, gridType, noData,
                        computationalLWidth, computationalUWidth, petMap, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateReg
!
! !ARGUMENTS:
       character (len=*), intent(in), optional :: name
       type (ESMF_StaggerLoc), intent(in),optional :: staggerLocs(:)
       type(ESMF_TypeKind),  intent(in),optional  :: coordTypeKind
       integer,               intent(in),         ::   countPerDE(:,:)
       integer,               intent(in),   optional  :: dimmap(:)
       integer,               intent(in),   optional  :: minIndex(:)
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
! \item[{[minIndex]}] 
!      Tuple to start the index ranges at. If not present, defaults
!      to (1,1,..1)
! \item[{countPerDE}] 
!        This is a 2D array. The first dimension is the size of the number
!         of distributed dimensions in the grid. The second is the size
!         of the maximum number of DE's in any dimension. Each entry
!         tells the number of points for that dimension for that DE. 
!         When a dimension has less than the maximum number of DEs then 0
!         should be used to fill the remaining slots. 
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

      end function ESMF_GridCreateReg


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
! !IROUTINE: ESMF_GridCreate - Create a new Grid from a set of Arrays

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


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreateShape"
!BOP
! !IROUTINE: ESMF_GridCreateShape - Create a new rectangular Grid

! !INTERFACE:
  ! Private name; call using ESMF_GridCreateShape()
      function ESMF_GridCreateShapeArb(name,coordTypeKind,  &
                        minIndex, maxIndex, localIndices, &
                        connDim1, connDim2, connDim3, &
                        coordCompDep1, coordCompDep2, coordCompDep3, &
                        indexflag, gridType, haloDepth, petMap, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateShapeArb
!
! !ARGUMENTS:
      character (len=*), intent(in), optional :: name 
       type(ESMF_TypeKind),  intent(in),    optional  :: coordTypeKind
       integer,               intent(in),   optional  :: minIndex(:)
       integer,               intent(in),             :: maxIndex(:)
       integer,               intent(in),             :: localIndices(:,:)
       type(ESMF_GridConn),   intent(in),   optional  :: connDim1(2)
       type(ESMF_GridConn),   intent(in),   optional  :: connDim2(2)
       type(ESMF_GridConn),   intent(in),   optional  :: connDim3(2)
       integer,               intent(in),   optional  :: coordCompDep1(:)
       integer,               intent(in),   optional  :: coordCompDep2(:)
       integer,               intent(in),   optional  :: coordCompDep3(:)
       type(ESMF_IndexFlag),  intent(in),   optional  :: indexflag
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
! \item[{[minIndex]}] 
!      The bottom extent of the grid array. If not given then the value defaults
!       to (1,1,1...).
! \item[{maxIndex}] 
!        The upper extent of the grid array.
! \item[{[localIndices]}] 
!      2D array whose first dimension is the same rank as the distGrid, and whose
!      second dimension is the size of the number of grid locations distributed
!      to this grid. {\tt localIndices} contains a list of all the glocal index tuples
!      which should reside on this processor. 
! \item[{[connDim1]}] 
!     Two element array describing the index dimension 1 connections.
!      The first element represents the minimum end of dimension 1.
!      The second element represents the minimum end of dimension 1.
!      The valid setting are ESMF\_GRIDCONN\_NONE, ESMF\_GRIDCONN\_POLE,
!       ESMF\_GRIDCONN\_BIPOLE, or ESMF\_GRIDCONN\_PERIODIC. 
!       If one element is set to ESMF\_GRIDCONN\_PERIODIC then both must be. 
! \item[{[connDim2]}] 
!     Two element array describing the index dimension 2 connections.
!      The first element represents the minimum end of dimension 2.
!      The second element represents the minimum end of dimension 2.
!      The valid setting are ESMF\_GRIDCONN\_NONE, ESMF\_GRIDCONN\_POLE,
!       ESMF\_GRIDCONN\_BIPOLE, or ESMF\_GRIDCONN\_PERIODIC. 
!       If one element is set to ESMF\_GRIDCONN\_PERIODIC then both must be. 
! \item[{[connDim3]}] 
!     Two element array describing the index dimension 3 connections.
!      The first element represents the minimum end of dimension 3.
!      The second element represents the minimum end of dimension 3.
!      The valid setting are ESMF\_GRIDCONN\_NONE, ESMF\_GRIDCONN\_POLE,
!       ESMF\_GRIDCONN\_BIPOLE, or ESMF\_GRIDCONN\_PERIODIC. 
!       If one element is set to ESMF\_GRIDCONN\_PERIODIC then both must be. 
! \item[{[indexflag]}]
!      Flag that indicates how the DE-local indices are to be defined.
! \item[{[coordCompDep1]}] 
!     This array specifies the dependence of the first 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the first
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is (/1/). 
! \item[{[coordCompDep2]}] 
!     This array specifies the dependence of the second 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the second
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is (/2/). 
! \item[{[coordCompDep3]}] 
!     This array specifies the dependence of the third 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the third
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is (/3/). 
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

      end function ESMF_GridCreateShapeArb

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreateShape"
!BOP
! !IROUTINE: ESMF_GridCreateShape - Create a new rectangular Grid

! !INTERFACE:
  ! Private name; call using ESMF_GridCreateShape()
      function ESMF_GridCreateShapeBlk(name,coordTypeKind,  &
                        minIndex, maxIndex, blockDecomp, &
                        connDim1, connDim2, connDim3, &
                        coordCompDep1, coordCompDep2, coordCompDep3, &
                        indexflag, gridType, haloDepth, petMap, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateShapeBlk
!
! !ARGUMENTS:
      character (len=*), intent(in), optional :: name 
       type(ESMF_TypeKind),  intent(in),    optional  :: coordTypeKind
       integer,               intent(in),   optional  :: minIndex(:)
       integer,               intent(in),             :: maxIndex(:)
       integer,               intent(in),   optional  :: blockDecomp(:)
       type(ESMF_GridConn),   intent(in),   optional  :: connDim1(2)
       type(ESMF_GridConn),   intent(in),   optional  :: connDim2(2)
       type(ESMF_GridConn),   intent(in),   optional  :: connDim3(2)
       integer,               intent(in),   optional  :: coordCompDep1(:)
       integer,               intent(in),   optional  :: coordCompDep2(:)
       integer,               intent(in),   optional  :: coordCompDep3(:)
       type(ESMF_IndexFlag),  intent(in),   optional  :: indexflag
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
! \item[{[minIndex]}] 
!      The bottom extent of the grid array. If not given then the value defaults
!       to (1,1,1...).
! \item[{maxIndex}] 
!        The upper extent of the grid array.
! \item[{[blockDecomp]}] 
!      List that has the same number of elements as {\tt maxIndex}.
!      Each entry is the number of decounts for that dimension.
!      If not specified, the default decomposition will be deCountx1x1..x1. 
! \item[{[connDim1]}] 
!     Two element array describing the index dimension 1 connections.
!      The first element represents the minimum end of dimension 1.
!      The second element represents the minimum end of dimension 1.
!      The valid setting are ESMF\_GRIDCONN\_NONE, ESMF\_GRIDCONN\_POLE,
!       ESMF\_GRIDCONN\_BIPOLE, or ESMF\_GRIDCONN\_PERIODIC. 
!       If one element is set to ESMF\_GRIDCONN\_PERIODIC then both must be. 
! \item[{[connDim2]}] 
!     Two element array describing the index dimension 2 connections.
!      The first element represents the minimum end of dimension 2.
!      The second element represents the minimum end of dimension 2.
!      The valid setting are ESMF\_GRIDCONN\_NONE, ESMF\_GRIDCONN\_POLE,
!       ESMF\_GRIDCONN\_BIPOLE, or ESMF\_GRIDCONN\_PERIODIC. 
!       If one element is set to ESMF\_GRIDCONN\_PERIODIC then both must be. 
! \item[{[connDim3]}] 
!     Two element array describing the index dimension 3 connections.
!      The first element represents the minimum end of dimension 3.
!      The second element represents the minimum end of dimension 3.
!      The valid setting are ESMF\_GRIDCONN\_NONE, ESMF\_GRIDCONN\_POLE,
!       ESMF\_GRIDCONN\_BIPOLE, or ESMF\_GRIDCONN\_PERIODIC. 
!       If one element is set to ESMF\_GRIDCONN\_PERIODIC then both must be. 
! \item[{[indexflag]}]
!      Flag that indicates how the DE-local indices are to be defined.
! \item[{[coordCompDep1]}] 
!     This array specifies the dependence of the first 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the first
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is (/1/). 
! \item[{[coordCompDep2]}] 
!     This array specifies the dependence of the second 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the second
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is (/2/). 
! \item[{[coordCompDep3]}] 
!     This array specifies the dependence of the third 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the third
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is (/3/). 
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

      end function ESMF_GridCreateShapeBlk

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreateShape"
!BOP
! !IROUTINE: ESMF_GridCreateShape - Create a new rectangular Grid

! !INTERFACE:
  ! Private name; call using ESMF_GridCreateShape()
      function ESMF_GridCreateShapeReg(name,coordTypeKind, minIndex,  &
                        countsPerDEDim1,countsPerDeDim2, countsPerDEDim3, &
                        connDim1, connDim2, connDim3, &
                        coordCompDep1, coordCompDep2, coordCompDep3, &
                        indexflag, gridType, haloDepth, petMap, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateShape
!
! !ARGUMENTS:
      character (len=*), intent(in), optional :: name 
       type(ESMF_TypeKind),  intent(in),    optional  :: coordTypeKind
       integer,               intent(in),   optional  :: minIndex(:)
       integer,               intent(in),             :: countsPerDEDim1(:)
       integer,               intent(in),             :: countsPerDEDim2(:)
       integer,               intent(in),   optional  :: countsPerDEDim3(:)
       type(ESMF_GridConn),   intent(in),   optional  :: connDim1(2)
       type(ESMF_GridConn),   intent(in),   optional  :: connDim2(2)
       type(ESMF_GridConn),   intent(in),   optional  :: connDim3(2)
       integer,               intent(in),   optional  :: coordCompDep1(:)
       integer,               intent(in),   optional  :: coordCompDep2(:)
       integer,               intent(in),   optional  :: coordCompDep3(:)
       type(ESMF_IndexFlag),  intent(in),   optional  :: indexflag
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
! \item[{[minIndex]}] 
!      Tuple to start the index ranges at. If not present, defaults
!      to (1,1,..1)
! \item[{countsPerDEDim1}] 
!     This arrays specifies the number of cells per DE for index dimension 1
!     for the exclusive region (the center stagger location).
!     If the array has only one entry, then the dimension is undistributed. 
! \item[{countsPerDEDim2}] 
!     This array specifies the number of cells per DE for index dimension 2
!     for the exclusive region (center stagger location). 
!     If the array has only one entry, then the dimension is undistributed. 
! \item[{[countsPerDEDim3]}] 
!     This array specifies the number of cells per DE for index dimension 3
!     for the exclusive region (center stagger location).  
!     If not specified  then grid is 2D. Also, If the array has only one entry,
!     then the dimension is undistributed. 
! \item[{[connDim1]}] 
!     Two element array describing the index dimension 1 connections.
!      The first element represents the minimum end of dimension 1.
!      The second element represents the minimum end of dimension 1.
!      The valid setting are ESMF\_GRIDCONN\_NONE, ESMF\_GRIDCONN\_POLE,
!       ESMF\_GRIDCONN\_BIPOLE, or ESMF\_GRIDCONN\_PERIODIC. 
!       If one element is set to ESMF\_GRIDCONN\_PERIODIC then both must be. 
! \item[{[connDim2]}] 
!     Two element array describing the index dimension 2 connections.
!      The first element represents the minimum end of dimension 2.
!      The second element represents the minimum end of dimension 2.
!      The valid setting are ESMF\_GRIDCONN\_NONE, ESMF\_GRIDCONN\_POLE,
!       ESMF\_GRIDCONN\_BIPOLE, or ESMF\_GRIDCONN\_PERIODIC. 
!       If one element is set to ESMF\_GRIDCONN\_PERIODIC then both must be. 
! \item[{[connDim3]}] 
!     Two element array describing the index dimension 3 connections.
!      The first element represents the minimum end of dimension 3.
!      The second element represents the minimum end of dimension 3.
!      The valid setting are ESMF\_GRIDCONN\_NONE, ESMF\_GRIDCONN\_POLE,
!       ESMF\_GRIDCONN\_BIPOLE, or ESMF\_GRIDCONN\_PERIODIC. 
!       If one element is set to ESMF\_GRIDCONN\_PERIODIC then both must be. 
! \item[{[indexflag]}]
!      Flag that indicates how the DE-local indices are to be defined.
! \item[{[coordCompDep1]}] 
!     This array specifies the dependence of the first 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the first
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is (/1/). 
! \item[{[coordCompDep2]}] 
!     This array specifies the dependence of the second 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the second
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is (/2/). 
! \item[{[coordCompDep3]}] 
!     This array specifies the dependence of the third 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the third
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is (/3/). 
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

      end function ESMF_GridCreateShapeReg




      end module ESMF_GridCreateMod

