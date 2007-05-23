! $Id: ESMF_Grid.F90,v 1.6 2007/05/23 17:51:58 oehmke Exp $
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
#define ESMF_FILENAME "ESMF_Grid.F90"
!
!     ESMF Grid Module
      module ESMF_GridMod
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
      '$Id: ESMF_Grid.F90,v 1.6 2007/05/23 17:51:58 oehmke Exp $'



!==============================================================================

      contains



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSet"
!BOP
! !IROUTINE: ESMF_GridSet - Set values in a Grid in preparation for committing and creating the grid. 

! !INTERFACE:
     function ESMF_GridSet(grid, name, arrayspec, minIndex,maxIndex, distgrid, dimmap, &
                        regDecomp, lbounds, ubounds, coordCompRanks, &
                        coordCompDimMap, staggerLocs, &
                        staggerLocLWidth, staggerLocUWidth, &
                        staggerLocAligns,indexflag, gridType, noData, &
                        computationalLWidth, computationalUWidth, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid),        intent(in)              :: grid 
      character (len=*),      intent(in),   optional  :: name
       type (ESMF_StaggerLoc), intent(in),  optional  :: staggerLocs(:)
       type(ESMF_ArraySpec),  intent(in),   optional  :: arrayspec
       integer,               intent(in),   optional  :: minIndex(:)
       integer,               intent(in),   optional  :: maxIndex(:)
       type(ESMF_DistGrid),   intent(in),   optional  :: distgrid
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
       integer,               intent(in),   optional  :: computationalLWidth(:,:)
       integer,               intent(in),   optional  :: computationalUWidth(:,:)
       integer,               intent(out),  optional  :: rc
!
! !DESCRIPTION:
!  Set values in a grid in preparation for committing and creating a grid. Note that 
! once a grid is committed and created it's an error to try to set values in it. Note also 
! that new values overwrite old values if previously set. 
!
! The arguments are:
! \begin{description}
!     \item[{grid}]
!          Partially created Grid to set information into.
! \item[{[name]}]
!          {\tt ESMF\_Grid} name.
! \item[{[arrayspec]}] 
!     {\tt ESMF\_ArraySpec} object containing the type/kind/rank information.
!     If not specified then the grid will be of rank size(maxIndex)+size(ubounds),
!     and the type/kind will be 8 byte reals. 
! \item[{[minIndex]}] 
!      The bottom extent of the grid array. If not given then the value defaults
!       to (1,1,1...).
! \item[{[maxIndex]}] 
!        The upper extent of the grid array.
! \item[{[distgrid]}]
!      {\tt ESMF\_DistGrid} object that describes how the array is decomposed and
!      distributed over DEs. The dimCount of distgrid must be smaller or equal
!      to the grid rank, otherwise a runtime ESMF error will be
!      raised.
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
!       Array of the same size as the {\tt distGrid} dimcount or size of {\tt maxIndex}. 
!       Sets the size of the computational padding around the exclusive
!       regions on each DE. If {\tt staggerLocLWidth} is also set
!       the actual value for any edge is the maximum between the two. 
! \item[{[computationalUWidth]}]
!       Array of the same size as the {\tt distGrid} dimcount or size of {\tt maxIndex}. 
!       Sets the size of the computational padding around the exclusive
!       regions on each DE. If {\tt staggerLocUWidth} is also set
!       the actual value for any edge is the maximum between the two. 
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      end function ESMF_GridSet


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCommit"
!BOP
! !IROUTINE: ESMF_GridCommit - Turn a partially created grid into usable grid. 

! !INTERFACE:
      subroutine ESMF_GridCommit(grid, status, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout)     :: grid
      type(ESMF_GridStatus)                 :: status 
      integer, intent(out), optional       :: rc
!
! !DESCRIPTION:
!    Turns a partially created grid into usable grid. This subroutine also performs checks
!    to ensure that the values in the grid are consistant before setting the grid as 
!    finally created.
!
!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          Grid object to perform commit on.
!     \item[{[status]}]
!          Grid status to commit to.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      end subroutine ESMF_GridCommit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGet"
!BOP
! !IROUTINE: ESMF_GridGet - get various types of information about a grid

! !INTERFACE:
      subroutine ESMF_GridGet(grid, name, rank, type, kind,  &
          arrayspec, tileCount, distGrid, delayout, staggerLocsCount,  &
          staggerLocs, coordCompRanks, coordCompDimMap, dimmap, &
          staggerLocLWidth, staggerLocUWidth, &
          staggerLocAligns, lbounds, ubounds, gridType, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid),       intent(in)            :: grid
      character (len=*),     intent(out), optional :: name
      type(ESMF_DataType),   intent(out), optional :: type
      type(ESMF_DataKind),   intent(out), optional :: kind
      integer,               intent(out), optional :: rank
      type(ESMF_DELayout),   intent(out), optional :: delayout
      integer,               intent(out), optional :: tileCount
      integer,               intent(out), optional :: lbounds(:)
      integer,               intent(out), optional :: ubounds(:)
      integer,               intent(out), optional :: dimmap(:)
      integer,               intent(out), optional :: tileCount
      integer,               intent(out), optional :: coordCompRanks(:)
      integer,               intent(out), optional :: coordCompDimMap(:,:)
      integer,               intent(out), optional :: staggerLocLWidth(:,:)
      integer,               intent(out), optional :: staggerLocUWidth(:,:)
      integer,               intent(out), optional :: staggerLocAligns(:,:)
      type (ESMF_ArraySpec), intent(out), optional :: arraySpec
      type (ESMF_StaggerLoc),intent(out), optional :: stagger
      type(ESMF_DistGrid),   intent(out), optional :: distGrid
      integer,               intent(out), optional :: staggerLocsCount
      integer,               intent(out), optional :: staggerLocs(:)
      integer,               intent(out), optional :: gridType
      integer,               intent(out), optional :: rc
!
! !DESCRIPTION:
!    Gets various types of tile information about a grid. 
!
!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          Grid to get the information from.
!     \item[{[name]}]
!          {\tt ESMF\_Grid} name.
!     \item[{[type]}]
!        Type of the Grid object.
!     \item[{[kind]}]
!        Kind of the Grid object.
!     \item[{[rank]}]
!        Rank of the Grid object.
!     \item[{[arraySpec]}]
!          Description of the grid's type, kind and rank.
!     \item[{[tileCount]}]
!          The number of logically rectangular tiles in the grid. 
!     \item[{[distGrid]}]
!          The structure describing the distribution of the grid. 
!     \item[{[delayout]}]
!        Upon return this holds the associated {\tt ESMF\_DELayout} object.
!     \item[{[staggerLocs]}]
!          The set of positions in each grid cell which can contain data.
!     \item[{[staggerLocsCount]}]
!          The number of positions in each grid cell which can contain data.
! \item[{[coordCompRanks]}]
!      List that has as many elements as the grid rank (from arrayspec).
!      Gives the dimension of each component (e.g. x) array. This is 
!      to allow factorization of the coordinate arrays. If not specified
!      all arrays are the same size as the grid. 
! \item[{[coordCompDimMap]}]
!      2D list of size grid rank x grid rank. This array describes the
!      map of each component array's dimensions onto the grids
!      dimensions. 
! \item[{[dimmap]}]
!      List that has as many elements as the distgrid rank. This array describes
!      mapping between the grids dimensions and the distgrid.
! \item[{[distGrid]}]
!          The structure describing the distribution of the grid.
! \item[{[lbounds]}] 
!      Lower bounds for tensor array dimensions.
! \item[{[ubounds]}] 
!      Upper bounds for tensor array dimensions. 
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
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      end subroutine ESMF_GridGet


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAddStaggerLoc"
!BOP
! !IROUTINE: ESMF_GridAddStaggerLoc - Add stagger location information to a partially created grid. 

! !INTERFACE:
 ! Private name; call using ESMF_GridAddStaggerLoc()
     function ESMF_GridAddStaggerLocNoSet(grid, staggerLoc, &
                        staggerLocLWidth, staggerLocUWidth, &
                        staggerLocAlign, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid),       intent(in)              :: grid 
      type (ESMF_StaggerLoc), intent(in)             :: staggerLoc
      integer,               intent(in),   optional  :: staggerLocLWidth(:)
      integer,               intent(in),   optional  :: staggerLocUWidth(:)
      integer,               intent(in),   optional  :: staggerLocAlign(:)
      integer,               intent(out),  optional  :: rc
!
! !DESCRIPTION:
!  Add a stagger location to a grid.
!
! The arguments are:
! \begin{description}
!     \item[{grid}]
!          Partially created Grid to set information into.
! \item[{[staggerLoc]}]
!        The stagger location to add.
! \item[{[staggerLocLWidth]}] 
!      This array should be the same rank as the grid. It specifies the lower corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[staggerLocUWidth]}] 
!      This array should be the same rank as the grid. It specifies the upper corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[staggerLocAlign]}] 
!      This array is of size  grid rank.
!      For this stagger location, it specifies which element
!      has the same index value as the center. For example, 
!      for a 2D cell with corner stagger it specifies which 
!      of the 4 corners has the same index as the center. 
!      If this is set and staggerLocUWidth is not,
!      this determines the default array padding for a stagger. 
!      If not set, then this defaults to all negative. (e.g. 
!      The most negative part of the stagger in a cell is aligned with the 
!      center and the padding is all on the postive side.) 
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      end function ESMF_GridAddStaggerLocNoSet


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAddStaggerLoc"
!BOP
! !IROUTINE: ESMF_GridAddStaggerLoc - Add stagger location information to a grid. 

! !INTERFACE:
 ! Private name; call using ESMF_GridAddStaggerLoc()
     function ESMF_GridAddStaggerLocFptr(grid, staggerLoc, &
                        comp1, comp2, comp3, &
                        staggerLocLWidth, staggerLocUWidth, &
                        staggerLocAlign, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid),       intent(in)             :: grid 
      type (ESMF_StaggerLoc), intent(in)       :: staggerLoc
      real (ESMF_KIND_R8), intent(in)            :: comp1(:), comp2(:)
      real (ESMF_KIND_R8), intent(in)            :: comp3(:)
      integer,               intent(in),   optional  :: staggerLocLWidth(:)
      integer,               intent(in),   optional  :: staggerLocUWidth(:)
      integer,               intent(in),   optional  :: staggerLocAlign(:)
      type(ESMF_CopyFlag), intent(in), optional :: docopy
      integer,               intent(out),  optional  :: rc
!
! !DESCRIPTION:
!  Add a stagger location to a grid. This subroutine lets the user add a
! stagger location and set the coordinates from F90 pointers at the same
! time. (This subroutine is only usable for grids up to 4D). 
!
! The arguments are:
! \begin{description}
!     \item[{grid}]
!          Partially created Grid to set information into.
! \item[{[staggerLoc]}]
!        The stagger location to add.
! \item[{[comp1]}]
!        The F90 pointer to coordinate data for the first coordinate component (e.g. x).
! \item[{[comp2]}]
!        The F90 pointer to coordinate data for the second coordinate component (e.g. y).
! \item[{[comp3]}]
!        The F90 pointer to coordinate data for the third coordinate component (e.g. z).
! \item[{[staggerLocLWidth]}] 
!      This array should be the same rank as the grid. It specifies the lower corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[staggerLocUWidth]}] 
!      This array should be the same rank as the grid. It specifies the upper corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[staggerLocAlign]}] 
!      This array is of size  grid rank.
!      For this stagger location, it specifies which element
!      has the same index value as the center. For example, 
!      for a 2D cell with corner stagger it specifies which 
!      of the 4 corners has the same index as the center. 
!      If this is set and staggerLocUWidth is not,
!      this determines the default array padding for a stagger. 
!      If not set, then this defaults to all negative. (e.g. 
!      The most negative part of the stagger in a cell is aligned with the 
!      center and the padding is all on the postive side.) 
!\item[{[doCopy]}]
!          Default to {\tt ESMF\_DATA\_REF}, makes the grid reference the passed
!          in array. If set to {\tt ESMF\_DATA\_COPY} this routine makes a copy
!          of the array.
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      end function ESMF_GridAddStaggerLocFptr


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAddStaggerLoc"
!BOP
! !IROUTINE: ESMF_GridAddStaggerLoc - Add stagger location information to a grid. 

! !INTERFACE:
 ! Private name; call using ESMF_GridAddStaggerLoc()
     function ESMF_GridAddStaggerLocArray(grid, staggerLoc, &
                        comp1, comp2, comp3, &
                        staggerLocLWidth, staggerLocUWidth, &
                        staggerLocAlign, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid),       intent(in)             :: grid 
      type (ESMF_StaggerLoc), intent(in)       :: staggerLoc
      type(ESMF_ARRAY), intent(in)            :: comp1(:), comp2(:)
      type(ESMF_ARRAY), intent(in)            :: comp3(:)
      integer,               intent(in),   optional  :: staggerLocLWidth(:)
      integer,               intent(in),   optional  :: staggerLocUWidth(:)
      integer,               intent(in),   optional  :: staggerLocAlign(:)
      type(ESMF_CopyFlag), intent(in), optional :: docopy
      integer,               intent(out),  optional  :: rc
!
! !DESCRIPTION:
!  Add a stagger location to a grid. This subroutine lets the user add a
! stagger location and set the coordinates from F90 pointers at the same
! time. (This subroutine is only usable for grids up to 4D). 
!
! The arguments are:
! \begin{description}
!     \item[{grid}]
!          Partially created Grid to set information into.
! \item[{[staggerLoc]}]
!        The stagger location to add.
! \item[{[comp1]}]
!        ESMF Array holding coordinate data for the first coordinate component (e.g. x).
! \item[{[comp2]}]
!         ESMF Array holding coordinate data for the second coordinate component (e.g. y).
! \item[{[comp3]}]
!         ESMF Array holding coordinate data for the third coordinate component (e.g. z).
! \item[{[staggerLocLWidth]}] 
!      This array should be the same rank as the grid. It specifies the lower corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[staggerLocUWidth]}] 
!      This array should be the same rank as the grid. It specifies the upper corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[staggerLocAlign]}] 
!      This array is of size  grid rank.
!      For this stagger location, it specifies which element
!      has the same index value as the center. For example, 
!      for a 2D cell with corner stagger it specifies which 
!      of the 4 corners has the same index as the center. 
!      If this is set and staggerLocUWidth is not,
!      this determines the default array padding for a stagger. 
!      If not set, then this defaults to all negative. (e.g. 
!      The most negative part of the stagger in a cell is aligned with the 
!      center and the padding is all on the postive side.) 
!\item[{[doCopy]}]
!          Default to {\tt ESMF\_DATA\_REF}, makes the grid reference the passed
!          in array. If set to {\tt ESMF\_DATA\_COPY} this routine makes a copy
!          of the array.
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      end function ESMF_GridAddStaggerLocArray




!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridStaggerLocGet"
!BOP
! !IROUTINE: ESMF_GridStaggerLocGet - Add stagger location information to a partially created grid. 

! !INTERFACE:
     subroutine ESMF_GridStaggerLocGet(grid, staggerLoc, &
                        staggerLocLWidth, staggerLocUWidth, &
                        staggerAlign, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid 
      type (ESMF_StaggerLoc), intent(in)  :: staggerLoc
      integer,               intent(in),   optional  :: staggerLocLWidth(:)
      integer,               intent(in),   optional  :: staggerLocUWidth(:)
      integer,               intent(in),   optional  :: staggerAlign(:)
      integer,               intent(out),  optional  :: rc
!
! !DESCRIPTION:
!  Get information about a particular stagger location.
!
! The arguments are:
! \begin{description}
!     \item[{grid}]
!          Grid to get information from.
! \item[{staggerLoc}]
!        The stagger location to add.
! \item[{[staggerLocLWidth]}] 
!      This array should be the same rank as the grid. It specifies the lower corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[staggerLocUWidth]}] 
!      This array should be the same rank as the grid. It specifies the upper corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[staggerAlign]}] 
!      This array is of size  grid rank.
!      For this stagger location, it specifies which element
!      has the same index value as the center. For example, 
!      for a 2D cell with corner stagger it specifies which 
!      of the 4 corners has the same index as the center. 
!      If this is set and staggerLocUWidth is not,
!      this determines the default array padding for a stagger. 
!      If not set, then this defaults to all negative. (e.g. 
!      The most negative part of the stagger in a cell is aligned with the 
!      center and the padding is all on the postive side.) 
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      end function ESMF_GridStaggerLocGet


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridLocalTileCalcBnds"
!BOP
! !IROUTINE: ESMF_GridLocalTileCalcBnds - Given 

! !INTERFACE:
     subroutine ESMF_GridLocalTileCalcBnds(grid, tile, localDE, coordComp, staggerLoc, &
                        staggerLocLWidth, staggerLocUWidth, &
                        staggerAlign, lBounds, uBounds, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid 
      integer,               intent(in), optional :: tile 
      integer,               intent(in), optional :: localDE
      integer, intent(in),  optional :: coordComp
      type (ESMF_StaggerLoc), intent(in)  :: staggerLoc
      integer,               intent(in),   optional  :: staggerLocLWidth(:)
      integer,               intent(in),   optional  :: staggerLocUWidth(:)
      integer,               intent(in),   optional  :: staggerAlign(:)
      integer,               intent(out)                 :: lBounds(:),uBounds(:)
      integer,               intent(out),  optional  :: rc
!
! !DESCRIPTION:
!  Given a tile and stagger location information this subroutine calculates
!  what the bounds of the array on this tile and localDE would be 
!  if that stagger location were added. This is useful for preallocating
!  coordinate arrays to be used in {\tt ESMF\_GridStaggerLocAdd}.
!
! The arguments are:
! \begin{description}
!\item[{grid}]
!          Grid to get information from.
!\item[{[tile]}]
!          The grid tile to get the information for. If not set, defaults to 
!          the first tile. 
!\item[{[localDE]}]
!          The local DE from which to get the information.  If not set, defaults to 
!          the first DE on this processor. 
!\item[{coordComp}]
!          The coordinate component to put the data in (e.g. 1=x). Defaults to 1. 
! \item[{staggerLoc}]
!        The stagger location to add.
! \item[{[staggerLocLWidth]}] 
!      This array should be the same rank as the grid. It specifies the lower corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[staggerLocUWidth]}] 
!      This array should be the same rank as the grid. It specifies the upper corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[staggerAlign]}] 
!      This array is of size  grid rank.
!      For this stagger location, it specifies which element
!      has the same index value as the center. For example, 
!      for a 2D cell with corner stagger it specifies which 
!      of the 4 corners has the same index as the center. 
!      If this is set and staggerLocUWidth is not,
!      this determines the default array padding for a stagger. 
!      If not set, then this defaults to all negative. (e.g. 
!      The most negative part of the stagger in a cell is aligned with the 
!      center and the padding is all on the postive side.) 
! \item[{lBounds}] 
!      This array should have size=rank of the specified coord. comp. 
!      It specifies the lower bounds
!       of the array for the given tile, DE, staggerLoc, and coordComp.
! \item[{uBounds}] 
!      This array should have size=rank of the coord. comp. It specifies the upper bounds
!       of the array for the given tile, DE, staggerLoc, and coordComp.
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      end function ESMF_GridStaggerLocCalcSize



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridLocalTileGet"
!BOP
! !IROUTINE: ESMF_GridLocalTileGet - get various types of information about the part of a grid tile which lies on this DE.

! !INTERFACE:
      subroutine ESMF_GridLocalTileGet(grid,tile,localDE,staggerLoc, &
          exclusiveLBound, exclusiveUBound, computationalLBound, &
          computationalUBound, totalLBound, totalUBound, &
          computationalLWidth, computationalUWidth, &
          totalLWidth, totalUWidth,rc)

!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in),optional :: tile
     integer, intent(in),optional :: localDE
      integer, intent(in),  optional :: coordComp
      type (ESMF_StaggerLoc), intent(in), optional  :: staggerLoc
      integer,      intent(out), optional :: exclusiveLBound(:)
      integer,      intent(out), optional :: exclusiveUBound(:)
      integer,      intent(out), optional :: computationalLBound(:)
      integer,      intent(out), optional :: computationalUBound(:)
      integer,      intent(out), optional :: totalLBound(:)
      integer,      intent(out), optional :: totalUBound(:)
      integer,      intent(out), optional :: computationalLWidth(:)
      integer,      intent(out), optional :: computationalUWidth(:)
      integer,      intent(out), optional :: totalLWidth(:)
      integer,      intent(out), optional :: totalUWidth(:)
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    Gets various types of information about the part of a grid tile which lies on this DE. 
!
!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          Grid to get the information from.
!     \item[{[tile]}]
!          The grid tile to get the information for. If not set, defaults to 
!          the first tile. 
!     \item[{[localDE]}]
!          The local DE from which to get the information.  If not set, defaults to 
!          the first DE on this processor. 
!     \item[{coordComp}]
!          The coordinate component to put the data in (e.g. 1=x). Defaults to 1. 
!     \item[{staggerLoc}]
!          The stagger location to get the information for. If not set, defaults
!          to center.  
!     \item[{[exclusiveLBound]}]
!        Upon return this holds the lower bounds of the exclusive region.
!        {\tt exclusiveLBound} must be allocated to be of size dimCount.
!     \item[{[exclusiveUBound]}]
!        Upon return this holds the upper bounds of the exclusive region.
!        {\tt exclusiveUBound} must be allocated to be of size dimCount.
!     \item[{[computationalLBound]}]
!        Upon return this holds the lower bounds of the computational region. 
!       {\tt computationalLBound} must be allocated to be of size dimCount.
!     \item[{[computationalUBound]}]
!        Upon return this holds the upper bounds of the computational region.
!        {\tt computationalUBound} must be allocated to be of size dimCount.
!     \item[{[totalLBound]}]
!        Upon return this holds the lower bounds of the total region.
!        {\tt totalLBound} must be allocated to be of size dimCount.
!     \item[{[totalUBound]}]
!        Upon return this holds the upper bounds of the total region.
!        {\tt totalUBound} must be allocated to be of size dimCount.
!     \item[{[computationalLWidth]}]
!        Upon return this holds the lower width of the computational region.
!        {\tt computationalLWidth} must be allocated to be of size dimCount.
!     \item[{[computationalUWidth]}]
!        Upon return this holds the upper width of the computational region.
!        {\tt computationalUWidth} must be allocated to be of size dimCount.
!     \item[{[totalLWidth]}]
!        Upon return this holds the lower width of the total memory region.
!        {\tt computationalUWidth} must be allocated to be of size dimCount.
!     \item[{[totalUWidth]}]
!        Upon return this holds the upper width of the total memory region.
!        {\tt totalUWidth} must be allocated to be of size dimCount.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      end subroutine ESMF_GridLocalTileGet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetCoordFromArray"
!BOP
! !IROUTINE: ESMF_GridSetCoordFromArray - Sets  the coordinates of a stagger location from an  Array.

! !INTERFACE:
      subroutine ESMF_GridSetCoordFromArray(grid, staggerLoc,coordComp, &
                            array, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type (ESMF_StaggerLoc), intent(in), optional  :: staggerLoc
      integer, intent(in),  :: coordComp
      type(ESMF_Array), intent(in) :: array
      type(ESMF_CopyFlag), intent(in), optional :: docopy
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    Sets the coordinates for a stagger location from an array. 
!
!     The arguments are:
!     \begin{description}
!     \item[{staggerLoc}]
!          The stagger location into which to copy the arrays. If not set,
!          defaults to center. 
!     \item[{coordComp}]
!          The coordinate component to put the data in (e.g. 1=x).
!     \item[{array}]
!          An array to set the grid coordinate information from.
!     \item[{[doCopy]}]
!          Default to {\tt ESMF\_DATA\_REF}, makes the grid reference the passed
!          in array. If set to {\tt ESMF\_DATA\_COPY} this routine makes a copy
!          of the array.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      end subroutine ESMF_GridSetCoordFromArray


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetCoordIntoArray"
!BOP
! !IROUTINE: ESMF_GridGetCoordIntoArray - Puts  the coordinates of a stagger location into an Array

! !INTERFACE:
      subroutine ESMF_GridGetCoordIntoArray(grid, staggerLoc,coordComp, array, &
                            docopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type (ESMF_StaggerLoc), intent(in),optional  :: staggerLoc
      integer, intent(in),  :: coordComp
      type(ESMF_Array), intent(out) :: array
      type(ESMF_CopyFlag), intent(in), optional :: docopy
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    Sets the coordinates for a stagger location into an array. If
!    the doCopy flag is set to {\tt ESMF\_DATA\_REF}, the array contains a
!    reference to the grid array.
!
!     The arguments are:
!     \begin{description}
!     \item[{staggerLoc}]
!          The stagger location from which to get the arrays. If not set, 
!          defaults to center. 
!     \item[{coordComp}]
!          The coordinate component to get the data from (e.g. 1=x).
!     \item[{array}]
!          An array into which to put the coordinate infomation. 
!     \item[{[doCopy]}]
!          Default to {\tt ESMF\_DATA\_COPY},  copys the coordinates
!          into the arrays. If set to {\tt ESMF\_DATA\_REF},
!          causes the array to reference the grid array containing the coordinates. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      end subroutine ESMF_GridGetCoordIntoArray



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridLocalTileSetData"
!BOP
! !IROUTINE: ESMF_GridLocalTileSetData - set the coordinate data for the particular
piece of tile on our processor from a fortran array.

! !INTERFACE:
      subroutine ESMF_GridLocalTileSetData(grid, tile, localDE, staggerLoc, &
                            coordComp, fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in),optional :: tile
      integer, intent(in),optional :: localDE
      type (ESMF_StaggerLoc), intent(in),optional :: staggerLoc
      integer, intent(in),  :: coordComp
      real, intent(in), optional :: fptr(:,:)
      type(ESMF_CopyFlag), intent(in), optional :: docopy
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    Sets the coordinate data for the piece of tile on a local DE from a fortran pointer.
!     This routine will need to be overloaded to cover the full range of ranks, types, 
!      and kinds. 
!
!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          Grid to get the information from.
!     \item[{[tile]}]
!          The grid tile to set the information for. If not set, defaults to 
!          the first tile. 
!     \item[{[localDE]}]
!          The local DE to set the information for. If not set, defaults to 
!          the first DE on this processor. 
!     \item[{staggerLoc}]
!          The stagger location to set the information for. If not set, defaults
!          to center. 
!     \item[{coordComp}]
!          The coordinate component to get the data from (e.g. 1=x).
!     \item[{fptr}]
!          The pointer to the coordinate data.
!     \item[{[doCopy]}]
!          Default to {\tt ESMF\_DATA\_REF}, makes the  grid reference the passed
!          in arrays. If set to {\tt ESMF\_DATA\_COPY} this routine makes a copy
!          of the arrays.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      end subroutine ESMF_GridLocalTileSetData

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridLocalTileGetData"
!BOP
! !IROUTINE: ESMF_GridLocalTileGetData - get the fortran data pointer for the piece of  a tile on this DE.

! !INTERFACE:
      subroutine ESMF_GridLocalTileGetData(grid, tile, localDE, &
                            staggerLoc, coordComp, fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in),optional :: tile
      integer, intent(in),optional :: localDE
      type (ESMF_StaggerLoc), intent(in),optional :: staggerLoc
      integer, intent(in) :: coordComp
      real, intent(out), optional :: fptr(:,:)
      type(ESMF_CopyFlag), intent(in), optional :: docopy
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    Gets a fortran pointer to the coordinate data for the piece of tile on a local DE. 
!    This routine will need to be overloaded to cover the full range of ranks, types, 
!    and kinds. 

!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          Grid to get the information from.
!     \item[{[tile]}]
!          The grid tile to get the information for. If not set, defaults to 
!          the first tile. 
!     \item[{[localDE]}]
!          The local DE to get the information for. If not set, defaults to 
!          the first DE on this processor. 
!     \item[{staggerLoc}]
!          The stagger location to get the information for. If not set, defaults
!          to center.
!     \item[{coordComp}]
!          The coordinate component to get the data from (e.g. 1=x).
!     \item[{[doCopy]}]
!          Default to {\tt ESMF\_DATA\_COPY},  copys the array. 
!          If set to {\tt ESMF\_DATA\_REF}, fptr is a reference to the data in the grid arrays. 
!     \item[{fptr}]
!          The pointer to the coordinate data.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      end subroutine ESMF_GridLocalTileGetData


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetLocalTileCoord"
!BOP
! !IROUTINE: ESMF_GridGetLocalTileCoord - Gets  the coordinates of a particular location in a tile.

! !INTERFACE:
      subroutine ESMF_GridGetLocalTileCoord(grid, staggerLoc, tile, localDE, &
                            indices, coords, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type (ESMF_StaggerLoc), intent(in), optional  :: staggerLoc
      integer, intent(in),optional :: tile
      integer, intent(in),optional :: localDE
      integer, intent(in) :: indices(:)
      real, intent(out) :: coords(:)
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Given stagger location, tile and a set of indices, returns the coordinates
!     of the position represented by the indices in the tile. This subroutine would
!     need to be type overloaded for the coordinates. 
!
!     The arguments are:
!     \begin{description}
!     \item[{staggerLoc}]
!          The stagger location from which to get the arrays. If not specified, 
!          defaults to the center. 
!     \item[{[tile]}]
!          The grid tile to get the information for. If not set, defaults to 
!          the first tile. 
!     \item[{[localDE]}]
!          The local DE to get the information for. If not set, defaults to 
!          the first DE on this processor. 
!     \item[{indices}]
!           Integer array containing the index coordinates in the tile for which to 
!           calculate the coordinates.
!     \item[{coords}]
!           Coordinates returned by subroutine. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      end subroutine ESMF_GridGetLocalTileCoord



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetLocalTileCoord"
!BOP
! !IROUTINE: ESMF_GridSetLocalTileCoord - Sets  the coordinates of a particular location in a tile.

! !INTERFACE:
      subroutine ESMF_GridSetLocalTileCoord(grid, staggerLoc, tile, localDE, &
                            indices, coords, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type (ESMF_StaggerLoc), intent(in), optional  :: staggerLoc
      integer, intent(in),optional :: tile
      integer, intent(in),optional :: localDE
      integer, intent(in) :: indices(:)
      real, intent(in) :: coords(:)
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Given a stagger location, tile and a set of indices, sets the coordinates
!     of the position represented by the indices in the tile to the value in coords.
!     This subroutine would need to be type overloaded for the coordinates. 
!
!     The arguments are:
!     \begin{description}
!     \item[{staggerLoc}]
!          The stagger location from which to get the arrays. If not specified, 
!          defaults to the center. 
!     \item[{[tile]}]
!          The grid tile to get the information for. If not set, defaults to 
!          the first tile. 
!     \item[{[localDE]}]
!          The local DE to get the information for. If not set, defaults to 
!          the first DE on this processor. 
!     \item[{indices}]
!           Integer array containing the index coordinates in the tile for which to 
!           calculate the coordinates.
!     \item[{coords}]
!           The location described by the indices will be set to {\it coords} . 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      end subroutine ESMF_GridSetLocalTileCoord


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridHalo"
!BOP
! !IROUTINE: ESMF_GridHalo - Do a halo operation on the coordinate arrays in a grid.

! !INTERFACE:
      subroutine ESMF_GridHalo(grid, regionFlag, haloLDepth, haloUDepth, rc)

! !ARGUMENTS:
      type(ESMF_Grid) :: ESMF_GridHalo
      type(ESMF_RegionFlag), intent(in), optional :: regionFlag
      integer, intent(in), optional :: haloLDepth(:),haloUDepth(:)
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Do a halo operation on a grid to update the coordinate info which is shared
!      with another DE.
!
!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          Grid structure to perform the halo operation on.
!     \item[{[regionFlag]}]
!          Specifies the reference for halo width arguments: {\tt ESMF\_REGION\_EXCLUSIVE} or {\tt ESMF\_REGION\_COMPUTATIONAL}.
!     \item[{[haloLDepth]}]
!           A vector the same size as the dimension of the distGrid. It specifies
!           the lower corner of the halo with respect to the lower corner of either
!            the computational region or the exclusive region (depending
!            on {\tt regionFlag}).  If not specified, uses the computational region,
!            lower bounds.  
!     \item[{[haloUDepth]}]
!           A vector the same size as the dimension of the distGrid. It specifies
!           the upper corner of the halo with respect to the lower corner of either
!           the computational region or the exclusive region (depending
!           on {\tt regionFlag}).  If not specified, uses the computational region,
!            upper bounds.    
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  TODO


      end function ESMF_GridHalo

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGenCoordUni"
!BOP
! !IROUTINE: ESMF_GridGenCoordUni - fill coordinate arrays with a uniform spread of values.

! !INTERFACE:
      subroutine ESMF_GridGenCoordsUni(grid, tile, begCoord, endCoord, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in),optional :: tile
      real, intent(in), optional :: begCoord(:), endCoord(:)
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    Generates coordinates and loads them into the grid. This 
!   method generates coordinates uniformly between {\tt begCoord} and
!   {\tt endCoord}. {\tt begCoord} is associated with the minimum
!   end of the index range and {\tt endCoord} is associated with
!   the maximum end. Note that its fine to have coordinates
!   go from big to small with increasing index by setting a larger value 
!   in {\tt begCoord} than {\tt endCoord}.
!
!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          Grid to get the information from.
!     \item[{[tile]}]
!          The grid tile to set the information for. If not set, defaults to 
!          the first tile. 
!     \item[{begCoord}]
!          Array the same rank as the grid. These values correspond to 
!          the minimum end of the index ranges, and is the starting value
!          for the uniform coordinates.
!     \item[{endCoord}]
!          Array the same rank as the grid. These values correspond to 
!          the maximum end of the index ranges, and is the ending value
!          for the uniform coordinates.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      end subroutine ESMF_GridGenCoordsUniform




!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridGetAttribute  - Retrieve an attribute
!
! !INTERFACE:
!      subroutine ESMF_GridGetAttribute(grid, name, <value argument>, rc)
!
! !ARGUMENTS:
!      type(ESMF_Grid), intent(in) :: grid
!      character (len = *), intent(in) :: name
!      <value argument>, see below for supported values
!      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns an attribute from the {\tt grid}.
!      Supported values for <value argument> are:
!     \begin{description}
!     \item integer(ESMF\_KIND\_I4), intent(out) :: value
!     \item integer(ESMF\_KIND\_I4), dimension(:), intent(out) :: valueList
!     \item integer(ESMF\_KIND\_I8), intent(out) :: value
!     \item integer(ESMF\_KIND\_I8), dimension(:), intent(out) :: valueList
!     \item real (ESMF\_KIND\_R4), intent(out) :: value
!     \item real (ESMF\_KIND\_R4), dimension(:), intent(out) :: valueList
!     \item real (ESMF\_KIND\_R8), intent(out) :: value
!     \item real (ESMF\_KIND\_R8), dimension(:), intent(out) :: valueList
!     \item type(ESMF\_Logical), intent(out) :: value
!     \item type(ESMF\_Logical), dimension(:), intent(out) :: valueList
!     \item character (len = *), intent(out), value
!     \end{description}
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [<value argument>]
!           The value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetAttribute - Set an attribute
!
! !INTERFACE:
!      subroutine ESMF_GridSetAttribute(grid, name, <value argument>, rc)
!
! !ARGUMENTS:
!      type(ESMF_Grid), intent(inout) :: grid
!      character (len = *), intent(in) :: name
!      <value argument>, see below for supported values    
!      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Attaches an attribute to the {\tt grid}.
!     The attribute has a {\tt name} and either a {\tt value} or a
!     {\tt valueList}.
!     Supported values for the <value argument> are:
!     \begin{description}
!     \item integer(ESMF\_KIND\_I4), intent(in) :: value
!     \item integer(ESMF\_KIND\_I4), dimension(:), intent(in) :: valueList
!     \item integer(ESMF\_KIND\_I8), intent(in) :: value
!     \item integer(ESMF\_KIND\_I8), dimension(:), intent(in) :: valueList
!     \item real (ESMF\_KIND\_R4), intent(in) :: value
!     \item real (ESMF\_KIND\_R4), dimension(:), intent(in) :: valueList
!     \item real (ESMF\_KIND\_R8), intent(in) :: value
!     \item real (ESMF\_KIND\_R8), dimension(:), intent(in) :: valueList
!     \item type(ESMF\_Logical), intent(in) :: value
!     \item type(ESMF\_Logical), dimension(:), intent(in) :: valueList
!     \item character (len = *), intent(in), value
!     \end{description}
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to set.
!     \item [<value argument>]
!           The value of the attribute to set.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridGetMetaData  - Retrieve grid metadata
!
! !INTERFACE:
!      subroutine ESMF_GridGetMetaData(grid, name, <value argument>, rc)
!
! !ARGUMENTS:
!      type(ESMF_Grid), intent(in) :: grid
!      character (len = *), intent(in) :: name
!      <value argument>, see below for supported values
!      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns metadata  from the {\tt grid}. 
!      Supported values for <value argument> are:
!     \begin{description}
!     \item integer(ESMF\_KIND\_I4), intent(out) :: value
!     \item integer(ESMF\_KIND\_I4), dimension(:), intent(out) :: valueList
!     \item integer(ESMF\_KIND\_I8), intent(out) :: value
!     \item integer(ESMF\_KIND\_I8), dimension(:), intent(out) :: valueList
!     \item real (ESMF\_KIND\_R4), intent(out) :: value
!     \item real (ESMF\_KIND\_R4), dimension(:), intent(out) :: valueList
!     \item real (ESMF\_KIND\_R8), intent(out) :: value
!     \item real (ESMF\_KIND\_R8), dimension(:), intent(out) :: valueList
!     \item type(ESMF\_Logical), intent(out) :: value
!     \item type(ESMF\_Logical), dimension(:), intent(out) :: valueList
!     \item character (len = *), intent(out), value
!     \end{description}
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [<value argument>]
!           The value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetMetaData - Set grid metadata
!
! !INTERFACE:
!      subroutine ESMF_GridSetMetaData(grid, name, <value argument>, rc)
!
! !ARGUMENTS:
!      type(ESMF_Grid), intent(inout) :: grid
!      character (len = *), intent(in) :: name
!      <value argument>, see below for supported values    
!      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Attaches metadata to {\tt grid}. Note that this is in general 
!     an ESMF internal operation and doing this carelessly can
!     cause an error. Users should instead use attributes. 
!     The attribute has a {\tt name} and either a {\tt value} or a
!     {\tt valueList}.
!     Supported values for the <value argument> are:
!     \begin{description}
!     \item integer(ESMF\_KIND\_I4), intent(in) :: value
!     \item integer(ESMF\_KIND\_I4), dimension(:), intent(in) :: valueList
!     \item integer(ESMF\_KIND\_I8), intent(in) :: value
!     \item integer(ESMF\_KIND\_I8), dimension(:), intent(in) :: valueList
!     \item real (ESMF\_KIND\_R4), intent(in) :: value
!     \item real (ESMF\_KIND\_R4), dimension(:), intent(in) :: valueList
!     \item real (ESMF\_KIND\_R8), intent(in) :: value
!     \item real (ESMF\_KIND\_R8), dimension(:), intent(in) :: valueList
!     \item type(ESMF\_Logical), intent(in) :: value
!     \item type(ESMF\_Logical), dimension(:), intent(in) :: valueList
!     \item character (len = *), intent(in), value
!     \end{description}
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to set.
!     \item [<value argument>]
!           The value of the attribute to set.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridDestroy"
!BOP
! !IROUTINE: ESMF_GridDestroy - Free all resources associated with a Grid 

! !INTERFACE:
      subroutine ESMF_GridDestroy(grid, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Destroys an {\tt ESMF\_Grid} object and all related internal structures.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          {\tt ESMF\_Grid} to be destroyed.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      end subroutine ESMF_GridDestroy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCalcStaggerLocCoord"
!BOPI
! !IROUTINE: ESMF_GridCalcStaggerLocCoord - Calculates the coordinates of a stagger location from another stagger locations coordinates .

! !INTERFACE:
      subroutine ESMF_GridCalcStaggerLocCoord(grid, srcStaggerLoc, dstStaggerLoc, &
                             method, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type (ESMF_StaggerLoc), intent(in)  :: srcStaggerLoc,dstStaggerLoc
      integer, intent(in) :: method
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Calculate the coordinates for the destination stagger location from the source
!     stagger location using the method.
!
!     The arguments are:
!     \begin{description}
!     \item[{srcStaggerLoc}]
!          The stagger location from which to get the coordinate info to 
!           calculate the desination's coordinates.
!     \item[{dstStaggerLoc}]
!          The stagger location for which to calculate the coordinates.
!     \item[{method}]
!           A flag indicating the method to use to do the calculations.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
! !REQUIREMENTS:  TODO

      end subroutine ESMF_GridCalcStaggerLocCoord


      end module ESMF_GridMod

