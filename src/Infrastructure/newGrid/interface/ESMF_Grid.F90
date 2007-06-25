! $Id: ESMF_Grid.F90,v 1.13 2007/06/25 16:28:55 oehmke Exp $
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
      module ESMF_newGridMod
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
! !MODULE: ESMF_newGridMod - Grid class
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt ESMF\_Grid} class.  
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_UtilTypesMod   ! ESMF base class
      use ESMF_BaseMod        ! ESMF base class
      use ESMF_IOSpecMod      ! ESMF I/O class
      use ESMF_LogErrMod
      use ESMF_ArrayMod
      use ESMF_LocalArrayMod  ! ESMF local array class
      use ESMF_InitMacrosMod    ! ESMF initializer macros
      use ESMF_LogErrMod        ! ESMF error handling
      use ESMF_VMMod
      use ESMF_DELayoutMod
      use ESMF_DistGridMod
      use ESMF_F90InterfaceMod  ! ESMF F90-C++ interface helper
      
!     NEED TO ADD MORE HERE
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
! ! ESMF_newGrid
!
!------------------------------------------------------------------------------

  ! F90 class type to hold pointer to C++ object
  type ESMF_newGrid
  sequence
  private
    type(ESMF_Pointer) :: this
    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------
!
! !PUBLIC TYPES:
!
public ESMF_newGrid


!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
!

! - ESMF-public methods:
  public ESMF_newGridCreateFromDistGrid
  public ESMF_newGridDestroy

! - ESMF-private methods:
  public ESMF_newGridGetInit  

!EOPI
! !PRIVATE MEMBER FUNCTIONS:

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Grid.F90,v 1.13 2007/06/25 16:28:55 oehmke Exp $'



!==============================================================================

      contains

!==============================================================================
#define NEWGRID_OUT ! put in for documentation 
#ifdef NEWGRID_OUT  ! Take out so you don't have to worry about compiler warnings for now
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridCommit"
!BOP
! !IROUTINE: ESMF_newGridCommit - Commit a Grid to a specified completion level

! !INTERFACE:
      subroutine ESMF_newGridCommit(grid, status, rc)
!
! !ARGUMENTS:
      type(ESMF_newGrid), intent(inout)     :: grid
      type(ESMF_newGridStatus)              :: status
      type(ESMF_DefaultFlag), optional   :: defaultflag
      integer, intent(out), optional     :: rc
!
! !DESCRIPTION:
!    This call is used to complete the {\tt grid} so that it is usable at
!    the level indicated by the {\tt status} flag.  For example, once committed
!    with a {\tt status} value of {\tt ESMF\_GRIDSTATUS\_SHAPE\_READY}, the 
!    {\tt grid} has sufficient size, rank, and distribution information to be
!    used as the basis for allocating Field data.  
!
!    It is necessary to call the {\tt ESMF\_GridCommit()} method after
!    creating a Grid object using the {\tt ESMF\_GridCreateEmpty()} method
!    and incrementally filling it in with {\tt ESMF\_GridSet()} calls.  The
!    {\tt EMF\_GridCommit()} call is a signal to the Grid that it can combine
!    the pieces of information that it's received and finish building any
!    necessary internal structures.  For example, an {\tt ESMF\_GridCommit()}
!    call with the {\tt status} flag set to 
!    {\tt ESMF\_GRIDSTATUS\_SHAPE\_READY} will trigger the {\tt grid} to 
!    build an internal DistGrid object that contains topology and distribution 
!    information.
!
!    It's possible using the {\tt ESMF\_GridCreateEmpty()/ESMF\_GridSet()}
!    approach that not all information is present when the {\tt ESMF\_GridCommit}
!    call is made.  If this is the case and the {\tt defaultflag} is set to
!    {\tt ESMF\_USE\_DEFAULTS} the Grid will attempt to build any internal
!    objects necessary to get to the desired {\tt status} by using reasonable
!    defaults.  If the {\tt defaultflag} is set to {\tt ESMF\_NO\_DEFAULTS} and
!    any information is missing, the {\tt ESMF\_GridCommit} call will fail.
!    If the {\tt defaultflag} argument is not passed in, {\it no} defaults
!    are used.
!
!    This subroutine calls {\tt ESMF\_GridValidate} internally 
!    to ensure that the values in the grid are consistent before returning.
!
!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          Grid object to commit.
!     \item[{status}]
!          Grid status to commit to.  For valid values see section
!          \ref{sec:opt:gridstatus}.  
!     \item[{[defaultFlag]}]
!          Indicates whether to use default values to achieve the desired
!          grid status.  For valid values see section \ref{opt:defaultflag}.
!          The default value is {\tt ESMF\_NO\_DEFAULTS}.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

      end subroutine ESMF_newGridCommit
#endif ! Take out so you don't have to worry about compiler warnings for now

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridCreate"
!BOP
! !IROUTINE: ESMF_newGridCreate - Create a Grid using a DistGrid

! !INTERFACE:
  ! Private name; call using ESMF_newGridCreate()
      function ESMF_newGridCreateFromDistGrid(name,coordTypeKind,distgrid, dimmap, &
                        lbounds, ubounds, coordRanks, coordDimMap, indexflag, gridType, rc)
!
! !RETURN VALUE:
      type(ESMF_newGrid) :: ESMF_newGridCreateFromDistGrid
!
! !ARGUMENTS:
       character (len=*), intent(in), optional :: name
       type(ESMF_TypeKind),  intent(in),   optional  :: coordTypeKind
       type(ESMF_DistGrid),   intent(in)              :: distgrid
       integer,               intent(in),   optional  :: dimmap(:)
       integer,               intent(in),   optional  :: lbounds(:)
       integer,               intent(in),   optional  :: ubounds(:)
       integer,               intent(in),   optional  :: coordRanks(:)
       integer,               intent(in),   optional  :: coordDimMap(:,:)
       type(ESMF_IndexFlag),  intent(in),   optional  :: indexflag
       integer,               intent(in),   optional  :: gridType
       integer,               intent(out),  optional  :: rc
!
! !DESCRIPTION:
! This is the most general form of creation for an {\tt ESMF\_Grid}
! object. This subroutine constructs a 
! grid as specified by a {\tt distgrid}. Optional {\tt lbound} and {\tt ubound}
! arguments can be used to specify extra tensor dimensions. 
!
! The arguments are:
! \begin{description}
! \item[{[name]}]
!     {\tt ESMF\_Grid} name.
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
! \item[{[coordRanks]}]
!      List that has as many elements as the grid rank .
!      Gives the dimension of each component (e.g. x) array. This is 
!      to allow factorization of the coordinate arrays. If not specified
!      all arrays are the same size as the grid. 
! \item[{[coordDimMap]}]
!      2D list of size grid rank x grid rank. This array describes the
!      map of each component array's dimensions onto the grids
!      dimensions. Each entry {\tt coordDimMap(i,j)} tells which
!      grid dimension component i's, jth dimension maps to. 
!      Note that if j is bigger than {\tt coordRanks(i)} than its ignored.        
! \item[{[indexflag]}]
!      Flag that indicates how the DE-local indices are to be defined.
! \item[{[gridType]}]
!      Flag that indicates the type of the grid. If not given, defaults
!       to ESMF\_GRIDTYPE\_UNKNOWN.
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
    integer :: localrc ! local error status
    type(ESMF_newGrid) :: grid              
    integer :: nameLen 
    type(ESMF_InterfaceInt) :: dimmapArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: lboundsArg ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: uboundsArg ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: coordRanksArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: coordDimMapArg ! Language Interface Helper Var


    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_DistGridGetInit, distgrid, rc)

    ! Translate F90 arguments to C++ friendly form
    !! name
    nameLen=0
    if (present(name)) then
       nameLen=len_trim(name)
    endif

    !! coordTypeKind
    ! It doesn't look like it needs to be translated, but test to make sure

    !! dimmap
    dimmapArg = ESMF_InterfaceIntCreate(dimmap, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    !! tensor bounds
    lboundsArg = ESMF_InterfaceIntCreate(lbounds, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    uboundsArg = ESMF_InterfaceIntCreate(ubounds, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    !! Description of array factorization
    coordRanksArg = ESMF_InterfaceIntCreate(coordRanks, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    coordDimMapArg = ESMF_InterfaceIntCreate(farray2D=coordDimMap, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Initialize this grid object as invalid
    grid%this = ESMF_NULL_POINTER

    ! Call C++ Subroutine to do the create
    call c_ESMC_gridcreatefromdistgrid(grid%this, nameLen, name, &
      coordTypeKind, distgrid, dimmapArg, lboundsArg, uboundsArg, coordRanksArg, coordDimMapArg, &
      indexflag, gridtype, rc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deallocate helper variables
    call ESMF_InterfaceIntDestroy(dimmapArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(lboundsArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(uboundsArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(coordRanksArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(coordDimMapArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set return value
    ESMF_newGridCreateFromDistGrid = grid

    ! Set init status
    ESMF_INIT_SET_CREATED(ESMF_newGridCreateFromDistGrid)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

    end function ESMF_newGridCreateFromDistGrid

#ifdef NEWGRID_OUT ! Take out so you don't have to worry about compiler warnings for now
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridCreateEmpty"
!BOP
! !IROUTINE: ESMF_newGridCreateEmpty - Create a Grid that has no contents

! !INTERFACE:
     function ESMF_newGridCreateEmpty(rc)
!
! !RETURN VALUE:
     type(ESMF_newGrid) :: ESMF_newGridCreateEmpty
!
! !ARGUMENTS:
       integer,               intent(out),  optional  :: rc
!
! !DESCRIPTION:
! Partially create an {\tt ESMF\_Grid} object. This function allocates 
! an {\tt ESMF\_Grid} object, but doesn't allocate any coordinate storage or other
! internal structures. The {\tt ESMF\_GridSet}  calls
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

      end function ESMF_newGridCreateEmpty


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridCreateShapeReg"
!BOP
! !IROUTINE: ESMF_newGridCreateShape - Create a Grid with a regular distribution

! !INTERFACE:
  ! Private name; call using ESMF_newGridCreateShape()
      function ESMF_newGridCreateShapeReg(name,coordTypeKind,  &
                        minIndex, maxIndex, regDecomp, &
                        connDim1, connDim2, connDim3, &
                        poleStaggerLoc1, poleStaggerLoc2, poleStaggerLoc3, &
                        bipolePos1, bipolePos2, bipolePos3, &
                        coordDep1, coordDep2, coordDep3, &
                        indexflag, gridType, haloDepth, petMap, rc)
!
! !RETURN VALUE:
      type(ESMF_newGrid) :: ESMF_newGridCreateShapeReg
!
! !ARGUMENTS:
      character (len=*), intent(in), optional :: name 
       type(ESMF_TypeKind),  intent(in),    optional  :: coordTypeKind
       integer,               intent(in),   optional  :: minIndex(:)
       integer,               intent(in),             :: maxIndex(:)
       integer,               intent(in),   optional  :: regDecomp(:)
       type(ESMF_newGridConn),   intent(in),   optional  :: connDim1(2)
       type(ESMF_newGridConn),   intent(in),   optional  :: connDim2(2)
       type(ESMF_newGridConn),   intent(in),   optional  :: connDim3(2)
       type(ESMF_StaggerLoc), intent(in),   optional  :: poleStaggerLoc1(2)
       type(ESMF_StaggerLoc), intent(in),   optional  :: poleStaggerLoc2(2)
       type(ESMF_StaggerLoc), intent(in),   optional  :: poleStaggerLoc3(2)
       integer,               intent(in),   optional  :: bipolePos1(2)
       integer,               intent(in),   optional  :: bipolePos2(2)
       integer,               intent(in),   optional  :: bipolePos3(2)
       integer,               intent(in),   optional  :: coordDep1(:)
       integer,               intent(in),   optional  :: coordDep2(:)
       integer,               intent(in),   optional  :: coordDep3(:)
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
! \item[{[poleStaggerLoc1]}] 
!     Two element array describing the index dimension 1 connections.
!      The first element represents the minimum end of dimension 1.
!      The second element represents the minimum end of dimension 1.
!      If a pole, this describes which staggerlocation is at the pole at each end.
!      If not present, the default is the edge.
! \item[{[poleStaggerLoc2]}] 
!     Two element array describing the index dimension 2 connections.
!      The first element represents the minimum end of dimension 2.
!      The second element represents the minimum end of dimension 2.
!      If a pole, this describes which staggerlocation is at the pole at each end.
!      If not present, the default is the edge.
! \item[{[poleStaggerLoc3]}] 
!     Two element array describing the index dimension 3 connections.
!      The first element represents the minimum end of dimension 3.
!      The second element represents the minimum end of dimension 3.
!      If a pole, this describes which staggerlocation is at the pole at each end.
!      If not present, the default is the edge.
! \item[{[bipolePos1]}] 
!     Two element array describing the index dimension 1 connections.
!      The first element represents the minimum end of dimension 1.
!      The second element represents the minimum end of dimension 1.
!      If a bipole, this gives the index position of one of the poles.
!      The other is half way around. If not present, the default is 1.
! \item[{[bipolePos2]}] 
!     Two element array describing the index dimension 2 connections.
!      The first element represents the minimum end of dimension 2.
!      The second element represents the minimum end of dimension 2.
!      If a bipole, this gives the index position of one of the poles.
!      The other is half way around. If not present, the default is 1.
! \item[{[bipolePos3]}] 
!     Two element array describing the index dimension 3 connections.
!      The first element represents the minimum end of dimension 3.
!      The second element represents the minimum end of dimension 3.
!      If a bipole, this gives the index position of one of the poles.
!      The other is half way around. If not present, the default is 1.
! \item[{[coordDep1]}] 
!     This array specifies the dependence of the first 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the first
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is (/1/). 
! \item[{[coordDep2]}] 
!     This array specifies the dependence of the second 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the second
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is (/2/). 
! \item[{[coordDep3]}] 
!     This array specifies the dependence of the third 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the third
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is (/3/). 
! \item[{[indexflag]}]
!      Flag that indicates how the DE-local indices are to be defined.
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

      end function ESMF_newGridCreateShapeReg

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridCreateShapeIrreg"
!BOP
! !IROUTINE: ESMF_newGridCreateShape - Create a Grid with an irregular distribution

! !INTERFACE:
  ! Private name; call using ESMF_newGridCreateShape()
      function ESMF_newGridCreateShapeIrreg(name,coordTypeKind, minIndex,  &
                        countsPerDEDim1,countsPerDeDim2, countsPerDEDim3, &
                        connDim1, connDim2, connDim3, &
                        poleStaggerLoc1, poleStaggerLoc2, poleStaggerLoc3, &
                        bipolePos1, bipolePos2, bipolePos3, &
                        coordDep1, coordDep2, coordDep3, &
                        indexflag, gridType, haloDepth, petMap, rc)
!
! !RETURN VALUE:
      type(ESMF_newGrid) :: ESMF_newGridCreateShapeIrreg
!
! !ARGUMENTS:
      character (len=*), intent(in), optional :: name 
       type(ESMF_TypeKind),  intent(in),    optional  :: coordTypeKind
       integer,               intent(in),   optional  :: minIndex(:)
       integer,               intent(in),             :: countsPerDEDim1(:)
       integer,               intent(in),             :: countsPerDEDim2(:)
       integer,               intent(in),   optional  :: countsPerDEDim3(:)
       type(ESMF_newGridConn),   intent(in),   optional  :: connDim1(2)
       type(ESMF_newGridConn),   intent(in),   optional  :: connDim2(2)
       type(ESMF_newGridConn),   intent(in),   optional  :: connDim3(2)
       type(ESMF_StaggerLoc), intent(in),   optional  :: poleStaggerLoc1(2)
       type(ESMF_StaggerLoc), intent(in),   optional  :: poleStaggerLoc2(2)
       type(ESMF_StaggerLoc), intent(in),   optional  :: poleStaggerLoc3(2)
       integer,               intent(in),   optional  :: bipolePos1(2)
       integer,               intent(in),   optional  :: bipolePos2(2)
       integer,               intent(in),   optional  :: bipolePos3(2)
       integer,               intent(in),   optional  :: coordDep1(:)
       integer,               intent(in),   optional  :: coordDep2(:)
       integer,               intent(in),   optional  :: coordDep3(:)
       type(ESMF_IndexFlag),  intent(in),   optional  :: indexflag
       integer,               intent(in),   optional  :: haloDepth
       integer,               intent(in),   optional  :: petMap(:,:,:)
       integer,               intent(out),  optional  :: rc
!
! !DESCRIPTION:
!
! This method creates a single tile, irregularly distributed grid 
! (see Figure \ref{fig:GridDecomps}).
! To specify the irregular distribution, the user passes in an array 
! for each grid dimension, where the length of the array is the number
! of DEs in the dimension.   Up to three dimensions can be specified, 
! using the countsPerDEDim1, countsPerDEDim2, countsPerDEDim3 arguments.
! The index of each array element corresponds to a DE number.  The 
! array value at the index is the number of grid cells on the DE in 
! that dimension.  The rank of the grid is equal to the number of 
! countsPerDEDim<> arrays that are specified. 
!
! To specify an undistributed dimension, the array in that dimension
! should have only one element, and its value should be the number of
! grid cells in that dimension.
!
! Section \ref{example:2DIrregUniGrid} shows an example
! of using this method to create a 2D Grid with uniformly spaced 
! coordinates.  This creation method can also be used as the basis for
! grids with rectilinear coordinates or curvilinear coordinates.
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
! \item[{[poleStaggerLoc1]}] 
!     Two element array describing the index dimension 1 connections.
!      The first element represents the minimum end of dimension 1.
!      The second element represents the minimum end of dimension 1.
!      If a pole, this describes which staggerlocation is at the pole at each end.
!      If not present, the default is the edge.
! \item[{[poleStaggerLoc2]}] 
!     Two element array describing the index dimension 2 connections.
!      The first element represents the minimum end of dimension 2.
!      The second element represents the minimum end of dimension 2.
!      If a pole, this describes which staggerlocation is at the pole at each end.
!      If not present, the default is the edge.
! \item[{[poleStaggerLoc3]}] 
!     Two element array describing the index dimension 3 connections.
!      The first element represents the minimum end of dimension 3.
!      The second element represents the minimum end of dimension 3.
!      If a pole, this describes which staggerlocation is at the pole at each end.
!      If not present, the default is the edge.
! \item[{[bipolePos1]}] 
!     Two element array describing the index dimension 1 connections.
!      The first element represents the minimum end of dimension 1.
!      The second element represents the minimum end of dimension 1.
!      If a bipole, this gives the index position of one of the poles.
!      The other is half way around. If not present, the default is 1.
! \item[{[bipolePos2]}] 
!     Two element array describing the index dimension 2 connections.
!      The first element represents the minimum end of dimension 2.
!      The second element represents the minimum end of dimension 2.
!      If a bipole, this gives the index position of one of the poles.
!      The other is half way around. If not present, the default is 1.
! \item[{[bipolePos3]}] 
!     Two element array describing the index dimension 3 connections.
!      The first element represents the minimum end of dimension 3.
!      The second element represents the minimum end of dimension 3.
!      If a bipole, this gives the index position of one of the poles.
!      The other is half way around. If not present, the default is 1. 
! \item[{[coordDep1]}] 
!     This array specifies the dependence of the first 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the first
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is (/1/). 
! \item[{[coordDep2]}] 
!     This array specifies the dependence of the second 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the second
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is (/2/). 
! \item[{[coordDep3]}] 
!     This array specifies the dependence of the third 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the third
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is (/3/). 
! \item[{[indexflag]}]
!      Flag that indicates how the DE-local indices are to be defined.
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

      end function ESMF_newGridCreateShapeReg

#endif ! Take out so you don't have to worry about compiler warnings for now

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridDestroy"
!BOP
! !IROUTINE: ESMF_newGridDestroy - Free all resources associated with a Grid 

! !INTERFACE:
      subroutine ESMF_newGridDestroy(grid, rc)
!
! !ARGUMENTS:
      type(ESMF_newGrid) :: grid
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

    integer :: localrc ! local error status

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_newGridGetInit, grid, rc)

    ! Call F90/C++ interface subroutine
    call c_ESMC_GridDestroy(grid, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Mark this Grid object as invalid
    grid%this = ESMF_NULL_POINTER

    ! Set init code
    ESMF_INIT_SET_DELETED(grid)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

 end subroutine ESMF_newGridDestroy

#ifdef NEWGRID_OUT ! Take out so you don't have to worry about compiler warnings for now

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridGenCoordUni"
!BOP
! !IROUTINE: ESMF_newGridGenCoordUni - Fill coordinates with uniformly spaced values

! !INTERFACE:
      subroutine ESMF_newGridGenCoordUni(grid, tile, begCoord, endCoord, rc)
!
! !ARGUMENTS:
      type(ESMF_newGrid), intent(in) :: grid
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

      end subroutine ESMF_newGridGenCoordUni


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridGet"
!BOP
! !IROUTINE: ESMF_newGridGet - Get information about a Grid

! !INTERFACE:
      subroutine ESMF_newGridGet(grid, name, coordTypeKind, rank,  &
          tileCount, distGrid, delayout, staggerLocsCount,  &
          staggerLocs, coordRanks, coordDimMap, dimmap, &
          staggerLocLWidth, staggerLocUWidth, &
          staggerLocAligns, lbounds, ubounds, gridType, rc)
!
! !ARGUMENTS:
      type(ESMF_newGrid),       intent(in)            :: grid
      character (len=*),     intent(out), optional :: name
      type(ESMF_TypeKind),  intent(in),   optional :: coordTypeKind
      integer,               intent(out), optional :: rank
      type(ESMF_DELayout),   intent(out), optional :: delayout
      integer,               intent(out), optional :: tileCount
      integer,               intent(out), optional :: lbounds(:)
      integer,               intent(out), optional :: ubounds(:)
      integer,               intent(out), optional :: dimmap(:)
      integer,               intent(out), optional :: coordRanks(:)
      integer,               intent(out), optional :: coordDimMap(:,:)
      type(ESMF_DistGrid),   intent(out), optional :: distGrid
      integer,               intent(out), optional :: staggerLocsCount
      integer,               intent(out), optional :: staggerLocs(:)
      integer,               intent(out), optional :: gridType
      integer,               intent(out), optional :: rc
!
! !DESCRIPTION:
!    Gets various types of tile information about a grid. 
!
!The arguments are:
!\begin{description}
!\item[{grid}]
!   Grid to get the information from.
!\item[{[name]}]
!   {\tt ESMF\_Grid} name.
!\item[{[coordTypeKind]}] 
!   The type/kind of the grid coordinate data. 
!   If not specified then the type/kind will be 8 byte reals.  
!\item[{[rank]}]
!   Rank of the Grid object.
!\item[{[tileCount]}]
!   The number of logically rectangular tiles in the grid. 
!\item[{[distGrid]}]
!   The structure describing the distribution of the grid. 
!\item[{[delayout]}]
!   Upon return this holds the associated {\tt ESMF\_DELayout} object.
!\item[{[staggerLocsCount]}]
!   The number of stagger locations which have coordinate arrays allocated.
!\item[{[staggerLocs]}]
!   An array of size {\tt staggerLocsCount}. It contains a list of 
!   the stagger locations which have coordinate data allocated. 
! \item[{[coordRanks]}]
!   List that has as many elements as the grid rank (from arrayspec).
!   Gives the dimension of each component (e.g. x) array. This is 
!   to allow factorization of the coordinate arrays. If not specified
!   all arrays are the same size as the grid. 
!\item[{[coordDimMap]}]
!   2D list of size grid rank x grid rank. This array describes the
!   map of each component array's dimensions onto the grids
!   dimensions. 
!\item[{[dimmap]}]
!   List that has as many elements as the distgrid rank. This array describes
!   mapping between the grids dimensions and the distgrid.
!\item[{[distGrid]}]
!   The structure describing the distribution of the grid.
!\item[{[lbounds]}] 
!   Lower bounds for tensor array dimensions.
!\item[{[ubounds]}] 
!   Upper bounds for tensor array dimensions. 
!\item[{[gridType]}]
!   Flag that indicates the type of the grid. If not given, defaults
!    to ESMF\_GRIDTYPE\_UNKNOWN.
!\item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!\end{description}
!
!EOP

end subroutine ESMF_newGridGet


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridGetCoordIntoArray"
!BOP
! !IROUTINE: ESMF_newGridGetCoord - Get coordinates and put in an ESMF Array

! !INTERFACE:
  ! Private name; call using ESMF_newGridGetCoord()
      subroutine ESMF_newGridGetCoordIntoArray(grid, staggerLoc,coord, array, &
                            docopy, rc)
!
! !ARGUMENTS:
      type(ESMF_newGrid), intent(in) :: grid
      type (ESMF_StaggerLoc), intent(in),optional  :: staggerLoc
      integer, intent(in),  :: coord
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
!     \item[{coord}]
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

      end subroutine ESMF_newGridGetCoordIntoArray

#endif

! -------------------------- ESMF-private method ------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridGetInit"
!BOPI
! !IROUTINE: ESMF_newGridGetInit - Internal access routine for init code
!
! !INTERFACE:
      function ESMF_newGridGetInit(grid)
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_newGridGetInit
!
! !ARGUMENTS:
      type(ESMF_newGrid), intent(in), optional :: grid
!
! !DESCRIPTION:
! Access deep object init code.
!
! The arguments are:
! \begin{description}
! \item [grid]
! Grid object.
! \end{description}
!
!EOPI

    if (present(grid)) then
      ESMF_newGridGetInit = ESMF_INIT_GET(grid)
    else
      ESMF_newGridGetInit = ESMF_INIT_CREATED
    endif

    end function ESMF_newGridGetInit
!------------------------------------------------------------------------------

#ifdef NEWGRID_OUT  ! Take out so you don't have to worry about compiler warnings for now

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridGetLocalTileCoord"
!BOP
! !IROUTINE: ESMF_newGridGetLocalTileCoord - Get pointer to coordinates of a local tile

! !INTERFACE:
      subroutine ESMF_newGridGetLocalTileCoord(grid, tile, localDE, &
                            staggerLoc, coord, fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_newGrid), intent(in) :: grid
      integer, intent(in),optional :: tile
      integer, intent(in),optional :: localDE
      type (ESMF_StaggerLoc), intent(in),optional :: staggerLoc
      integer, intent(in) :: coord
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
!     \item[{coord}]
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

      end subroutine ESMF_newGridGetLocalTileCoord


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridGetLocalTileInfo"
!BOP
! !IROUTINE: ESMF_newGridGetLocalTileInfo - Get information about a local tile

! !INTERFACE:
      subroutine ESMF_newGridGetLocalTileInfo(grid, tile, coord, localDE, staggerLoc, &
          exclusiveLBound, exclusiveUBound, computationalLBound, computationalUBound, &
          totalLBound, totalUBound, computationalLWidth, computationalUWidth, &
          totalLWidth, totalUWidth,rc)

!
! !ARGUMENTS:
      type(ESMF_newGrid),        intent(in)            :: grid
      integer,                intent(in),  optional :: tile
      integer,                intent(in),  optional :: localDE
      integer,                intent(in)            :: coord
      type (ESMF_StaggerLoc), intent(in),  optional :: staggerLoc
      integer,                intent(out), optional :: exclusiveLBound(:)
      integer,                intent(out), optional :: exclusiveUBound(:)
      integer,                intent(out), optional :: computationalLBound(:)
      integer,                intent(out), optional :: computationalUBound(:)
      integer,                intent(out), optional :: totalLBound(:)
      integer,                intent(out), optional :: totalUBound(:)
      integer,                intent(out), optional :: computationalLWidth(:)
      integer,                intent(out), optional :: computationalUWidth(:)
      integer,                intent(out), optional :: totalLWidth(:)
      integer,                intent(out), optional :: totalUWidth(:)
      integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!    Gets various types of information about the part of a grid tile which lies on this DE. 
!
!The arguments are:
!\begin{description}
!\item[{grid}]
!    Grid to get the information from.
!\item[{[tile]}]
!    The grid tile to get the information for. If not set, defaults to 
!    the first tile. 
!\item[{[localDE]}]
!     The local DE from which to get the information.  If not set, defaults to 
!     the first DE on this processor. 
!\item[{coord}]
!     The coordinate component to get the information for (e.g. 1=x). 
!\item[{staggerLoc}]
!     The stagger location to get the information for. If not set, defaults to center.  
!\item[{[exclusiveLBound]}]
!     Upon return this holds the lower bounds of the exclusive region.
!     {\tt exclusiveLBound} must be allocated to be of size dimCount.
!\item[{[exclusiveUBound]}]
!     Upon return this holds the upper bounds of the exclusive region.
!     {\tt exclusiveUBound} must be allocated to be of size dimCount.
!\item[{[computationalLBound]}]
!     Upon return this holds the lower bounds of the computational region. 
!     {\tt computationalLBound} must be allocated to be of size dimCount.
!\item[{[computationalUBound]}]
!     Upon return this holds the upper bounds of the computational region.
!     {\tt computationalUBound} must be allocated to be of size dimCount.
!\item[{[totalLBound]}]
!     Upon return this holds the lower bounds of the total region.
!     {\tt totalLBound} must be allocated to be of size dimCount.
!\item[{[totalUBound]}]
!     Upon return this holds the upper bounds of the total region.
!     {\tt totalUBound} must be allocated to be of size dimCount.
!\item[{[computationalLWidth]}]
!     Upon return this holds the lower width of the computational region.
!     {\tt computationalLWidth} must be allocated to be of size dimCount.
!\item[{[computationalUWidth]}]
!     Upon return this holds the upper width of the computational region.
!     {\tt computationalUWidth} must be allocated to be of size dimCount.
!\item[{[totalLWidth]}]
!     Upon return this holds the lower width of the total memory region.
!     {\tt computationalUWidth} must be allocated to be of size dimCount.
!\item[{[totalUWidth]}]
!     Upon return this holds the upper width of the total memory region.
!     {\tt totalUWidth} must be allocated to be of size dimCount.
!\item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!\end{description}
!
!EOP

      end subroutine ESMF_newGridGetLocalTileInfo


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridGetStaggerLocInfo"
!BOP
! !IROUTINE: ESMF_newGridGetStaggerLocInfo - Get information about a stagger location 

! !INTERFACE:
     subroutine ESMF_newGridGetStaggerLocInfo(grid, staggerLoc, coord, isAllocated &
                        coordLWidth, coordLocUWidth, coordAlign, rc)
!
! !ARGUMENTS:
      type(ESMF_newGrid),        intent(in)              :: grid 
      type (ESMF_StaggerLoc), intent(in)              :: staggerLoc
      integer,                intent(in)              :: coord
      logical,                intent(in),   optional  :: isAllocated
      integer,                intent(in),   optional  :: coordLWidth(:)
      integer,                intent(in),   optional  :: coordWidth(:)
      integer,                intent(in),   optional  :: coordAlign(:)
      integer,                intent(out),  optional  :: rc
!
! !DESCRIPTION:
!  Get information about a particular stagger location.
!
! The arguments are:
! \begin{description}
! \item[{grid}]
!      Grid to get information from.
! \item[{staggerLoc}]
!      The stagger location to add.
! \item[{coord}]
!      The coordinate component to get the information for (e.g. 1=x). 
! \item[{[isAllocated]}] 
!      If TRUE, then this stagger location has coordinate data allocated.  
! \item[{[coordLWidth]}] 
!      This array should be the same rank as the grid. It specifies the lower corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[coordUWidth]}] 
!      This array should be the same rank as the grid. It specifies the upper corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[coordAlign]}] 
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

      end function ESMF_newGridGetStaggerLocInfo


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridSet"
!BOP
! !IROUTINE: ESMF_newGridSet - Set the values in a Grid which has been created with CreateEmpty. 

! !INTERFACE:
  ! Private name; call using ESMF_newGridSet()
      function ESMF_newGridSetFromDistGrid(grid,name,coordTypeKind,distgrid, dimmap, &
                        lbounds, ubounds, indexflag, gridType, rc)
!
! !RETURN VALUE:

!
! !ARGUMENTS:
       type(ESMF_newGrid),       intent(in)              :: grid
       character (len=*),     intent(in),   optional  :: name
       type(ESMF_TypeKind),   intent(in),   optional  :: coordTypeKind
       type(ESMF_DistGrid),   intent(in)              :: distgrid
       integer,               intent(in),   optional  :: dimmap(:)
       integer,               intent(in),   optional  :: lbounds(:)
       integer,               intent(in),   optional  :: ubounds(:)
       type(ESMF_IndexFlag),  intent(in),   optional  :: indexflag
       integer,               intent(in),   optional  :: gridType
       integer,               intent(out),  optional  :: rc
!
! !DESCRIPTION:
!  Set values in a grid in preparation for committing and creating a grid. Note that 
! once a grid is committed and created it's an error to try to set values in it. Note also 
! that new values overwrite old values if previously set. 
!
! The arguments are:
! \begin{description}
! \item[{grid}]
!     Partially created Grid to set information into.
! \item[{[name]}]
!     {\tt ESMF\_Grid} name.
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
! \item[{[indexflag]}]
!      Flag that indicates how the DE-local indices are to be defined.
! \item[{[gridType]}]
!      Flag that indicates the type of the grid. If not given, defaults
!       to ESMF\_GRIDTYPE\_UNKNOWN.
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
      end function ESMF_newGridSetFromDistGrid

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridSetCoordNoValues"

!BOP
! !IROUTINE: ESMF_newGridSetCoord - Allocate coordinate arrays but don't set their values

! !INTERFACE:
  ! Private name; call using ESMF_newGridSetCoord()
     function ESMF_newGridSetCoordNoValues(grid, staggerLoc, coord,  &
                coordLWidth, coordUWidth, coordAlign, &
                computationalLWidth, computationalUWidth, &
                totalLWidth, totalUWidth,rc)

!
! !ARGUMENTS:
      type(ESMF_newGrid),        intent(in)              :: grid 
      type (ESMF_StaggerLoc), intent(in),optional     :: staggerLoc
      integer,                intent(in),             :: coord
      integer,                intent(in),optional     :: coordLWidth(:)
      integer,                intent(in),optional     :: coordUWidth(:)
      integer,                intent(in),optional     :: coordAlign(:)
      integer,                intent(out), optional   :: computationalLWidth(:)
      integer,                intent(out), optional   :: computationalUWidth(:)
      integer,                intent(out), optional   :: totalLWidth(:)
      integer,                intent(out), optional   :: totalUWidth(:)
      integer,                intent(out),optional    :: rc
!
! !DESCRIPTION:
!  This call allocates space for a coordinate array at a stagger location, but doesn't set the values.
!
! The arguments are:
! \begin{description}
!     \item[{grid}]
!      Partially created Grid to set information into.
! \item[{[staggerLoc]}]
!      The stagger location to add. If not present, defaults to center. 
! \item[{coord}]
!      The coordinate component to get the data from (e.g. 1=x).
! \item[{[coordLWidth]}] 
!      This array should be the same rank as the grid. It specifies the lower corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[coordUWidth]}] 
!      This array should be the same rank as the grid. It specifies the upper corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[coordAlign]}] 
!      This array is of size  grid rank.
!      For this stagger location, it specifies which element
!      has the same index value as the center. For example, 
!      for a 2D cell with corner stagger it specifies which 
!      of the 4 corners has the same index as the center. 
!      If this is set and coordUWidth is not,
!      this determines the default array padding for a stagger. 
!      If not set, then this defaults to all negative. (e.g. 
!      The most negative part of the stagger in a cell is aligned with the 
!      center and the padding is all on the postive side.) 
!\item[{[computationalLWidth]}]
!     The lower boundary of the computatational region in reference to the exclusive region. 
!     If {\tt coordLWidth} is present then the actual computational width
!     is the max on a dimension by dimension basis between {\tt coordLWidth} and
!      {\tt computationalLWidth}.
!\item[{[computationalUWidth]}]
!     The lower boundary of the computatational region in reference to the exclusive region. 
!     If {\tt coordUWidth} is present then the actual computational width
!     is the max on a dimension by dimension basis between {\tt coordUWidth} and
!      {\tt computationalUWidth}.
!\item[{[totalLWidth]}]
!     The lower boundary of the computatational region in reference to the computational region. 
!     Note, the computational region includes the extra padding specified by {\tt ccordLWidth}.
!\item[{[totalUWidth]}]
!     The lower boundary of the computatational region in reference to the computational region. 
!     Note, the computational region includes the extra padding specified by {\tt coordLWidth}.
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP

      end function ESMF_newGridSetCoordNoValues


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridSetCoordFromArray"
!BOP
! !IROUTINE: ESMF_newGridSetCoord - Set coordinates using ESMF Arrays

! !INTERFACE:
      subroutine ESMF_newGridSetCoordFromArray(grid, staggerLoc, coord, coordDep, &
                            array, doCopy, coordAlign, rc)
!
! !ARGUMENTS:
      type(ESMF_newGrid),        intent(in),           :: grid
      type (ESMF_StaggerLoc), intent(in), optional  :: staggerLoc
      integer,                intent(in),           :: coord
      type(ESMF_Array),       intent(in),           :: array
      type(ESMF_CopyFlag),    intent(in), optional  :: docopy
      integer,                intent(in), optional  :: coordAlign(:)
      integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!    Sets the coordinates for a stagger location from an array. 
!
!     The arguments are:
!\begin{description}
!\item[{staggerLoc}]
!    The stagger location into which to copy the arrays. If not set,
!    defaults to center. 
!\item[{coord}]
!    The coordinate component to put the data in (e.g. 1=x).
!\item[{array}]
!    An array to set the grid coordinate information from.
!\item[{[doCopy]}]
!    Default to {\tt ESMF\_DATA\_REF}, makes the grid reference the passed
!    in array. If set to {\tt ESMF\_DATA\_COPY} this routine makes a copy
!    of the array.
!\item[{[coordAlign]}] 
!    This array is of size  grid rank.
!    For this stagger location, it specifies which element
!    has the same index value as the center. For example, 
!    for a 2D cell with corner stagger it specifies which 
!    of the 4 corners has the same index as the center. 
!    If this is set and coordUWidth is not,
!    this determines the default array padding for a stagger. 
!    If not set, then this defaults to all negative. (e.g. 
!    The most negative part of the stagger in a cell is aligned with the 
!    center and the padding is all on the postive side.) 
!\item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!\end{description}
!
!EOP

      end subroutine ESMF_newGridSetCoordFromArray


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridSetCoordFromFptr"
!BOP
! !IROUTINE: ESMF_newGridSetCoord - Set coordinates from fortran pointer

! !INTERFACE:
      subroutine ESMF_newGridSetCoordFromFptr2DR8(grid, staggerLoc, coord, &
                   fptr, doCopy, coordLWidth, coordUWidth, coordAlign, &
                   computationalLWidth, computationalUWidth, &
                   totalLWidth, totalUWidth,rc)
!
! !ARGUMENTS:
      type(ESMF_newGrid),        intent(in),           :: grid
      type (ESMF_StaggerLoc), intent(in),  optional :: staggerLoc
      integer,                intent(in),           :: coord
      real(ESMF_KIND_R8),     intent(out), optional :: fptr(:,:)
      type(ESMF_CopyFlag),    intent(in),  optional :: docopy
      integer,                intent(in),  optional :: coordLWidth(:)
      integer,                intent(in),  optional :: coordUWidth(:)
      integer,                intent(in),  optional :: coordAlign(:)
      integer,                intent(out), optional :: computationalLWidth(:)
      integer,                intent(out), optional :: computationalUWidth(:)
      integer,                intent(out), optional :: totalLWidth(:)
      integer,                intent(out), optional :: totalUWidth(:)
      integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!    Sets the coordinates for a stagger location from a fortran pointer. Note that
! this call only works for grids whose distribution is 1-to-1 DE to PET. If your situtation
! is different, you can either set the coords from an ESMF Array created from LocalArrays or you can use  
! {\tt ESMF\_GridSetLocalTileCoord}. If any of the widths are specified then they must be consistent
! with the Grid's distgrid and the size of {\tt fptr}.
!
!     The arguments are:
!\begin{description}
!\item[{staggerLoc}]
!    The stagger location into which to copy the arrays. If not set,
!    defaults to center. 
!\item[{coord}]
!    The coordinate component to put the data in (e.g. 1=x).
!\item[{fptr}]
!     The fortran pointer to the coordinate data.
!\item[{[doCopy]}]
!    Default to {\tt ESMF\_DATA\_REF}, makes the grid reference the passed
!    in array. If set to {\tt ESMF\_DATA\_COPY} this routine makes a copy
!    of the array.
!\item[{[coordLWidth]}] 
!    This array should be the same rank as the grid. It specifies the lower corner of the computational
!    region with respect to the lower corner of the exclusive region.
!\item[{[coordUWidth]}] 
!    This array should be the same rank as the grid. It specifies the upper corner of the computational
!    region with respect to the lower corner of the exclusive region.
!\item[{[coordAlign]}] 
!    This array is of size  grid rank.
!    For this stagger location, it specifies which element
!    has the same index value as the center. For example, 
!    for a 2D cell with corner stagger it specifies which 
!    of the 4 corners has the same index as the center. 
!    If this is set and coordUWidth is not,
!    this determines the default array padding for a stagger. 
!    If not set, then this defaults to all negative. (e.g. 
!    The most negative part of the stagger in a cell is aligned with the 
!    center and the padding is all on the postive side.) 
!\item[{[computationalLWidth]}]
!    The lower boundary of the computatational region in reference to the exclusive region. 
!    If {\tt coordLWidth} is present then the actual computational width
!    is the max on a dimension by dimension basis between {\tt coordLWidth} and
!    {\tt computationalLWidth}.
!\item[{[computationalUWidth]}]
!    The lower boundary of the computatational region in reference to the exclusive region. 
!    If {\tt coordUWidth} is present then the actual computational width
!    is the max on a dimension by dimension basis between {\tt coordUWidth} and
!    {\tt computationalUWidth}.
!\item[{[totalLWidth]}]
!    The lower boundary of the computatational region in reference to the computational region. 
!    Note, the computational region includes the extra padding specified by {\tt ccordLWidth}.
!\item[{[totalUWidth]}]
!    The lower boundary of the computatational region in reference to the computational region. 
!    Note, the computational region includes the extra padding specified by {\tt coordLWidth}.
!\item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!\end{description}
!
!EOP

      end subroutine ESMF_newGridSetCoordFromFptr2DR8


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridSetLocalTileCoord"
!BOP
! !IROUTINE: ESMF_newGridSetLocalTileCoord - Set the coordinates for a local tile from a Fortran array

! !INTERFACE:
      subroutine ESMF_newGridSetLocalTileCoord2R8(grid, staggerLoc, coord, tile, localDE, &
                             fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_newGrid),        intent(in)            :: grid
      type (ESMF_StaggerLoc), intent(in),optional   :: staggerLoc
      integer,                intent(in),           :: coord
      integer,                intent(in),optional   :: tile
      integer,                intent(in),optional   :: localDE
      real(ESMF_KIND_R8),     intent(out),optional  :: fptr(:,:)
      type(ESMF_CopyFlag),    intent(in),optional   :: docopy
      integer,                intent(out),optional  :: rc
!
! !DESCRIPTION:
!    Sets the coordinate data for the piece of tile on a local DE from a fortran pointer.
!    This routine will need to be overloaded to cover the full range of ranks, types, and kinds. 
!
!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          Grid to get the information from.
!     \item[{staggerLoc}]
!          The stagger location to set the information for. If not set, defaults
!          to center. 
!     \item[{coord}]
!          The coordinate component to get the data from (e.g. 1=x).
!     \item[{[tile]}]
!          The grid tile to set the information for. If not set, defaults to 
!          the first tile. 
!     \item[{[localDE]}]
!          The local DE to set the information for. If not set, defaults to 
!          the first DE on this processor. 
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

      end subroutine ESMF_newGridSetLocalTileCoord



!------------------------------------------------------------------------------

!==============================================================================
! UNREVIEWED AND UNIMPLEMENTED METHODS
!==============================================================================

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridAddMetricFromArray"
!BOPI
! !IROUTINE: ESMF_newGridAddMetricFromArray - Add a new metric from an Array.

! !INTERFACE:
  ! Private name; call using ESMF_newGridAddMetric()
      subroutine ESMF_newGridSetMetricFromArray(grid, name, metricTag, staggerLoc, &
                            array, metricDep, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_newGrid), intent(in) :: grid
      character (len=*), intent(in) :: name
      type(ESMF_MetricTag),  intent(in),    optional  :: metricTag
      type (ESMF_StaggerLoc), intent(in), optional  :: staggerLoc
      type(ESMF_Array), intent(in) :: array
      integer,               intent(in),   optional  :: metricDep(:)
      type(ESMF_CopyFlag), intent(in), optional :: docopy
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    Adds a grid metric from an array. 
!
!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          The grid to set the metric in.
!     \item [name]
!           The name of the attribute to set.
!      \item[{[metricTag]}] 
!           Identifies the type of metric to ESMF. If not present, defaults
!           to ESMF\_METRICTAG\_NONE.            
!     \item[{[staggerLoc]}]
!          The stagger location into which to copy the arrays. If not set,
!          defaults to center. 
!     \item[{array}]
!          An array to set the grid metric information from.
!     \item[{metricDep}]
!          This array specifies the dependence of the metric dimensions 
!          on the grid index dimensions. The size of the 
!          array specifies the number of dimensions of the metric.
!          The values specify which of the grid index dimensions the corresponding
!           map to. If a value is 0, then that dimension doesn't correspond to 
!          a grid dimension. If not present the default is (/1,2,3,.../). 
!     \item[{[doCopy]}]
!          Default to {\tt ESMF\_DATA\_REF}, makes the grid reference the passed
!          in array. If set to {\tt ESMF\_DATA\_COPY} this routine makes a copy
!          of the array.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

      end subroutine ESMF_newGridAddMetricFromArray


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridAddMetricFromFptr"
!BOPI
! !IROUTINE: ESMF_newGridSetMetricFromFptr - Sets metric data from a Fortran pointer.

! !INTERFACE:
  ! Private name; call using ESMF_newGridAddMetric()
      subroutine ESMF_newGridSetMetricFromFptr(grid, name, metricTag, staggerLoc, metricDep, &
                            fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_newGrid), intent(in) :: grid
      character (len=*), intent(in) :: name
       type(ESMF_MetricTag),  intent(in),    optional  :: metricTag
      type (ESMF_StaggerLoc), intent(in), optional  :: staggerLoc
      real, intent(in), pointer :: fptr
      integer,               intent(in),   optional  :: metricDep(:)
      type(ESMF_CopyFlag), intent(in), optional :: docopy
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    Adds a grid metric from a Fortran pointer. This subroutine only
!    works when there's a 1-to-1 DE to PET match. If not, then
!    use Array to set the metric data. This subroutine also needs to 
!    be overloaded to cover the full range of type/kinds.
!
!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          The grid to set the metric in.
!     \item [name]
!           The name of the attribute to set.
!      \item[{[metricTag]}] 
!           Identifies the type of metric to ESMF. If not present, defaults
!           to ESMF\_METRICTAG\_NONE.            
!     \item[{[staggerLoc]}]
!          The stagger location into which to copy the arrays. If not set,
!          defaults to center. 
!     \item[{metricDep}]
!          This array specifies the dependence of the metric dimensions 
!          on the grid index dimensions. The size of the 
!          array specifies the number of dimensions of the metric.
!          The values specify which of the grid index dimensions the corresponding
!           map to. If a value is 0, then that dimension doesn't correspond to 
!          a grid dimension. If not present the default is (/1,2,3,.../). 
!     \item[{[doCopy]}]
!          Default to {\tt ESMF\_DATA\_REF}, makes the grid reference the passed
!          in array. If set to {\tt ESMF\_DATA\_COPY} this routine makes a copy
!          of the array.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

      end subroutine ESMF_newGridAddMetricFromFptr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridAddMetricNoValues"
!BOPI
! !IROUTINE: ESMF_newGridAddMetricNoValues - Allocates space for metric, but doesn't set data.

! !INTERFACE:
! Private name; call using ESMF_newGridAddMetric()
      subroutine ESMF_newGridAddMetricNoSet(grid, name, metricTag, metricTypeKind, 
                   staggerLoc, metricDep, lbounds, ubounds, &                         
                   metricLWidth, metricUWidth, metricAlign, &
                   computationalLWidth, computationalUWidth, rc)
!
! !ARGUMENTS:
      type(ESMF_newGrid), intent(in) :: grid
      character (len=*), intent(in) :: name
       type(ESMF_MetricTag),  intent(in),    optional  :: metricTag
       type(ESMF_TypeKind),  intent(in),    optional  :: metricTypeKind
      type (ESMF_StaggerLoc), intent(in), optional  :: staggerLoc
      integer,               intent(in),   optional  :: metricDep(:)
      integer,               intent(in),   optional  :: lbounds(:)
      integer,               intent(in),   optional  :: ubounds(:)
      integer,               intent(in),   optional  :: metricLWidth(:)
      integer,               intent(in),   optional  :: metricUWidth(:)
      integer,               intent(in),   optional  :: metricAlign(:)
      type(ESMF_IndexFlag),  intent(in),   optional  :: indexflag
      integer,               intent(in),   optional  :: computationalLWidth(:)
      integer,               intent(in),   optional  :: computationalUWidth(:)
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Builds storage for a metric, but doesn't set its data. 
!
!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          The grid to set the metric in.
!     \item [name]
!           The name of the attribute to set.           
!      \item[{[metricTag]}] 
!           Identifies the type of metric to ESMF. If not present, defaults
!           to ESMF\_METRICTAG\_NONE. 
!      \item[{[metricTypeKind]}] 
!           The type/kind of the grid coordinate data. 
!           If not specified then the type/kind will be 8 byte reals. 
!     \item[{[staggerLoc]}]
!          The stagger location into which to copy the arrays. If not set,
!          defaults to center. 
!     \item[{metricDep}]
!          This array specifies the dependence of the metric dimensions 
!          on the grid index dimensions. The size of the 
!          array specifies the number of dimensions of the metric.
!          The values specify which of the grid index dimensions the corresponding
!           map to. If a value is 0, then that dimension doesn't correspond to 
!          a grid dimension. If not present the default is (/1,2,3,.../). 
!     \item[{[lbounds]}] 
!          Lower bounds for tensor array dimensions.
!     \item[{[ubounds]}] 
!          Upper bounds for tensor array dimensions.
! \item[{[metricLWidth]}] 
!      This array should be the same rank as the grid. It specifies the lower corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[metricUWidth]}] 
!      This array should be the same rank as the grid. It specifies the upper corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[metricAlign]}] 
!      This array is of size  grid rank.
!      For this stagger location, it specifies which element
!      has the same index value as the center. For example, 
!      for a 2D cell with corner stagger it specifies which 
!      of the 4 corners has the same index as the center. 
!      If this is set and metricUWidth is not,
!      this determines the default array padding for a stagger. 
!      If not set, then this defaults to all negative. (e.g. 
!      The most negative part of the stagger in a cell is aligned with the 
!      center and the padding is all on the postive side.) 
!     \item[{[indexflag]}]
!          Flag that indicates how the DE-local indices are to be defined.
!     \item[{[computationalLWidth]}]
!          Array of the same size as the {\tt distGrid} dimcount. 
!          Sets the size of the computational padding around the exclusive
!          regions on each DE. If {\tt metricLWidth} is also set
!          the actual value for any edge is the maximum between the two. 
!     \item[{[computationalUWidth]}]
!          Array of the same size as the {\tt distGrid} dimcount. 
!          Sets the size of the computational padding around the exclusive
!          regions on each DE. If {\tt metricUWidth} is also set
!          the actual value for any edge is the maximum between the two. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

      end subroutine ESMF_newGridAddMetricNoValues


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridAddMetricSub"
!BOPI
! !IROUTINE: ESMF_newGridAddMetricSub - Add a subroutine interface to generate metric data.

! !INTERFACE:
  ! Private name; call using ESMF_newGridAddMetric()
      subroutine ESMF_newGridAddMetricSub(grid, name, metricTag, metricTypeKind, 
                   staggerLoc, metricDep, lbounds, ubounds, &                         
                   metricLWidth, metricUWidth, metricAlign, &
                   computationalLWidth, computationalUWidth,&
                   sub, userDataR4, userDataR8, rc)
!
! !ARGUMENTS:
      type(ESMF_newGrid), intent(in) :: grid
      character (len=*), intent(in) :: name
       type(ESMF_MetricTag),  intent(in),    optional  :: metricTag
       type(ESMF_TypeKind),  intent(in),    optional  :: metricTypeKind
      type (ESMF_StaggerLoc), intent(in), optional  :: staggerLoc
      integer,               intent(in),   optional  :: metricDep(:)
      integer,               intent(in),   optional  :: lbounds(:)
      integer,               intent(in),   optional  :: ubounds(:)
      integer,               intent(in),   optional  :: metricLWidth(:)
      integer,               intent(in),   optional  :: metricUWidth(:)
      integer,               intent(in),   optional  :: metricAlign(:)
      type(ESMF_IndexFlag),  intent(in),   optional  :: indexflag
      integer,               intent(in),   optional  :: computationalLWidth(:)
      integer,               intent(in),   optional  :: computationalUWidth(:)
      subroutine             intent(in),             :: sub
      real(ESMF_KIND_R4),    intent(in),   optional  :: userDataR4(:)
      real(ESMF_KIND_R4),    intent(in),   optional  :: userDataR8(:)
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Sets a subroutine to generate metric data from. The user may 
!    also pass user data through to the subroutine to use in computation. 
!
!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          The grid to set the metric in.
!     \item [name]
!           The name of the attribute to set.           
!      \item[{[metricTag]}] 
!           Identifies the type of metric to ESMF. If not present, defaults
!           to ESMF\_METRICTAG\_NONE. 
!      \item[{[metricTypeKind]}] 
!           The type/kind of the grid coordinate data. 
!           If not specified then the type/kind will be 8 byte reals. 
!     \item[{[staggerLoc]}]
!          The stagger location into which to copy the arrays. If not set,
!          defaults to center. 
!     \item[{metricDep}]
!          This array specifies the dependence of the metric dimensions 
!          on the grid index dimensions. The size of the 
!          array specifies the number of dimensions of the metric.
!          The values specify which of the grid index dimensions the corresponding
!           map to. If a value is 0, then that dimension doesn't correspond to 
!          a grid dimension. If not present the default is (/1,2,3,.../). 
!     \item[{[lbounds]}] 
!          Lower bounds for tensor array dimensions.
!     \item[{[ubounds]}] 
!          Upper bounds for tensor array dimensions.
! \item[{[metricLWidth]}] 
!      This array should be the same rank as the grid. It specifies the lower corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[metricUWidth]}] 
!      This array should be the same rank as the grid. It specifies the upper corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[metricAlign]}] 
!      This array is of size  grid rank.
!      For this stagger location, it specifies which element
!      has the same index value as the center. For example, 
!      for a 2D cell with corner stagger it specifies which 
!      of the 4 corners has the same index as the center. 
!      If this is set and metricUWidth is not,
!      this determines the default array padding for a stagger. 
!      If not set, then this defaults to all negative. (e.g. 
!      The most negative part of the stagger in a cell is aligned with the 
!      center and the padding is all on the postive side.) 
!     \item[{[indexflag]}]
!          Flag that indicates how the DE-local indices are to be defined.
!     \item[{[computationalLWidth]}]
!          Array of the same size as the {\tt distGrid} dimcount. 
!          Sets the size of the computational padding around the exclusive
!          regions on each DE. If {\tt metricLWidth} is also set
!          the actual value for any edge is the maximum between the two. 
!     \item[{[computationalUWidth]}]
!          Array of the same size as the {\tt distGrid} dimcount. 
!          Sets the size of the computational padding around the exclusive
!          regions on each DE. If {\tt metricUWidth} is also set
!          the actual value for any edge is the maximum between the two. 
!     \item[{sub}]
!          The subroutine to generate the metric data.
!     \item[{sub}]
!          The subroutine to generate the metric data.
!     \item[{userDataR4}]
!          User data to pass to the subroutine.
!     \item[{userDataR8}]
!          User data to pass to the subroutine.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

      end subroutine ESMF_newGridAddMetricSub


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridCalcStaggerLocCoord"
!BOPI
! !IROUTINE: ESMF_newGridCalcStaggerLocCoord - Calculates the coordinates of a set of stagger locations from another stagger location's coordinates .

! !INTERFACE:
      subroutine ESMF_newGridCalcStaggerLocCoord(grid, srcStaggerLoc, dstStaggerLocs, &
                             method, rc)
!
! !ARGUMENTS:
      type(ESMF_newGrid), intent(in) :: grid
      type (ESMF_StaggerLoc), intent(in)  :: srcStaggerLoc
      type (ESMF_StaggerLoc), intent(in),optional  :: dstStaggerLocs(:)
      integer, intent(in) :: method
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Calculate the coordinates for the destination stagger locations from the source
!     stagger location using the method.
!
!     The arguments are:
!     \begin{description}
!     \item[{srcStaggerLoc}]
!          The stagger location from which to get the coordinate info to 
!           calculate the desination's coordinates.
!     \item[{dstStaggerLocs}]
!          The array of stagger locations for which to calculate the coordinates.
!          If not present, defaults to every stagger location containing coordiantes. 
!     \item[{method}]
!           A flag indicating the method to use to do the calculations.
!           Not yet implemented, defaults to averaging. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

      end subroutine ESMF_newGridCalcStaggerLocCoord


!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridCreateArb"
!BOPI
! !IROUTINE: ESMF_newGridCreate - Create a new grid 

! !INTERFACE:
  ! Private name; call using ESMF_newGridCreate()
      function ESMF_newGridCreateArb(name,coordTypeKind, minIndex,maxIndex, localIndices, &
                        dimmap, arbDecomp, lbounds, ubounds, coordRanks, &
                        coordDimMap, &
                        indexflag, gridType, noData,
                        computationalLWidth, computationalUWidth,  &
                        connectionList, connectionTransformList, rc)
!
! !RETURN VALUE:
      type(ESMF_newGrid) :: ESMF_newGridCreateArb
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
       integer,               intent(in),   optional  :: coordRanks(:)
       integer,               intent(in),   optional  :: coordDimMap(:,:)
       type(ESMF_IndexFlag),  intent(in),   optional  :: indexflag
       integer,               intent(in),   optional  :: gridType
       logical,               intent(in),   optional  :: noData
       integer,               intent(in),   optional  :: computationalLWidth(:)
       integer,               intent(in),   optional  :: computationalUWidth(:)
       integer,               intent(in),   optional  :: connectionList(:,:)
       integer,               intent(in),   optional  :: connectionTransformList(:,:)
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
! \item[{[coordRanks]}]
!      List that has as many elements as the grid rank .
!      Gives the dimension of each component (e.g. x) array. This is 
!      to allow factorization of the coordinate arrays. If not specified
!      all arrays are the same size as the grid. 
! \item[{[coordDimMap]}]
!      2D list of size grid rank x grid rank. This array describes the
!      map of each component array's dimensions onto the grids
!      dimensions. Each entry {\tt coordDimMap(i,j)} tells which
!      grid dimension component i's, jth dimension maps to. 
!      Note that if j is bigger than {\tt coordRanks(i)} than its ignored.        
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
! \item[{[connectionList]}]
!          List of connections between patches in index space. The second dimension
!          of {\tt connectionList} steps through the connection interface elements, 
!          defined by the first index. The first index must be of size
!          {\tt 2 x dimCount + 2}, where {\tt dimCount} is the rank of the 
!          decomposed index space. Each {\tt connectionList} element specifies
!          the connection interface in the format
!
!         {\tt (/patchIndex\_A,
!          patchIndex\_B, positionVector, orientationVector/)} where:
!          \begin{itemize}
!          \item {\tt patchIndex\_A} and {\tt patchIndex\_B} are the patch
!                index of the two connected patches respectively,
!          \item {\tt positionVector} is the vector that points from patch A's
!                minCorner to patch B's minCorner.
!          \item {\tt orientationVector} associates each dimension of patch A
!                with a dimension in patch B's index space. Negative index
!                values may be used to indicate a reversal in index orientation.
!          \end{itemize}
!     \item[{[connectionTransformList]}]
!          List of transforms associated with patch connections defined in 
!          {\tt connectionList}. The second dimension of {\tt connectionTransformList}
!          steps through the connection transforms, defined by the first index. The
!          first index must be of size {\tt 5 + dimCount}, where {\tt dimCount}
!          is the rank of the decomposed index space. Each 
!          {\tt connectionTransformList} element specifies a connection transform 
!          by a list of integer values in the format {\tt (/connectionIndex,
!          direction, staggerSrc, staggerDst, offsetDst, signVector/)}, where
!          \begin{itemize}
!          \item {\tt connectionIndex} corresponds to the index of the connection in
!                {\tt connectionList},
!          \item {\tt direction} can be {\tt +1} to specify forward direction,
!                i.e. source patch of the transform is patch\_A and destination
!                patch is patch\_B of the corresponding connection, or {\tt -1}
!                to indicate reverse direction through the connection. The only
!                other valid {\tt direction} value is {\tt 0} which indicates a 
!                bidirectional connection with source and destination definitions
!                as in the forward case. 
!          \item {\tt staggerSrc} and {\tt staggerDst} indicate staggering location
!                in the source and destination patch interface, respectively,
!          \item {\tt offsetDst} is a vector of size {\tt dimCount} that 
!                specifies the index offset on the destination side of 
!                this connection,
!          \item {\tt signVector} is of size {\tt dimCount} with elements either
!                {\tt +1} or {\tt -1} to indicate optional sign change of vector
!                components along the respective directions.
!          \end{itemize}
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI

      end function ESMF_newGridCreateArb


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridCreateFromArrays"
!BOPI
! !IROUTINE: ESMF_newGridCreate - Create a Grid from a set of Arrays

! !INTERFACE:
  ! Private name; call using ESMF_newGridCreate()
      function ESMF_newGridCreateFromArrays(name, arrays, staggerLocs, &
          staggerLocAligns, coordDimMap, doCopy, gridType, rc)
!
! !RETURN VALUE:
      type(ESMF_newGrid) :: ESMF_newGridCreateFromArrays
!
! !ARGUMENTS:
      character (len=*),     intent(in), optional :: name
      type (ESMF_Array),     intent(in)           :: arrays(:,:)
      type (ESMF_StaggerLoc), intent(in)          :: staggerLocs(:)
      integer,               intent(in), optional :: staggerLocAligns(:,:)
      integer,               intent(in), optional :: coordDimMap(:,:)
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
!     \item[{[coordDimMap]}]
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
!EOPI


      end function ESMF_newGridCreateFromArrays



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridCreateReg"
!BOPI
! !IROUTINE: ESMF_newGridCreate - Create a Grid with a regular decomposition

! !INTERFACE:
  ! Private name; call using ESMF_newGridCreate()
      function ESMF_newGridCreateReg(name,coordTypeKind, minIndex,maxIndex, dimmap, &
                        regDecomp, lbounds, ubounds, coordRanks, &
                        coordDimMap, &
                        indexflag, gridType, noData, &
                        computationalLWidth, computationalUWidth, &
                        connectionList, connectionTransformList, rc)
!
! !RETURN VALUE:
      type(ESMF_newGrid) :: ESMF_newGridCreateReg
!
! !ARGUMENTS:
       character (len=*), intent(in), optional :: name
       type(ESMF_TypeKind),  intent(in),optional  :: coordTypeKind
       integer,               intent(in),   optional  :: minIndex(:)
       integer,               intent(in),             :: maxIndex(:)
       integer,               intent(in),   optional  :: blockDecomp(:)
       integer,               intent(in),   optional  :: dimmap(:)
       integer,               intent(in),   optional  :: lbounds(:)
       integer,               intent(in),   optional  :: ubounds(:)
       integer,               intent(in),   optional  :: coordRanks(:)
       integer,               intent(in),   optional  :: coordDimMap(:,:)
       type(ESMF_IndexFlag),  intent(in),   optional  :: indexflag
       integer,               intent(in),   optional  :: gridType
       logical,               intent(in),   optional  :: noData
       integer,               intent(in),   optional  :: computationalLWidth(:)
       integer,               intent(in),   optional  :: computationalUWidth(:)
       integer,               intent(in),   optional  :: connectionList(:,:)
       integer,               intent(in),   optional  :: connectionTransformList(:,:)
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
! \item[{[coordRanks]}]
!      List that has as many elements as the grid rank .
!      Gives the dimension of each component (e.g. x) array. This is 
!      to allow factorization of the coordinate arrays. If not specified
!      all arrays are the same size as the grid. 
! \item[{[coordDimMap]}]
!      2D list of size grid rank x grid rank. This array describes the
!      map of each component array's dimensions onto the grids
!      dimensions. Each entry {\tt coordDimMap(i,j)} tells which
!      grid dimension component i's, jth dimension maps to. 
!      Note that if j is bigger than {\tt coordRanks(i)} than its ignored. ! \item[{[indexflag]}]
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
! \item[{[connectionList]}]
!          List of connections between patches in index space. The second dimension
!          of {\tt connectionList} steps through the connection interface elements, 
!          defined by the first index. The first index must be of size
!          {\tt 2 x dimCount + 2}, where {\tt dimCount} is the rank of the 
!          decomposed index space. Each {\tt connectionList} element specifies
!          the connection interface in the format
!
!         {\tt (/patchIndex\_A,
!          patchIndex\_B, positionVector, orientationVector/)} where:
!          \begin{itemize}
!          \item {\tt patchIndex\_A} and {\tt patchIndex\_B} are the patch
!                index of the two connected patches respectively,
!          \item {\tt positionVector} is the vector that points from patch A's
!                minCorner to patch B's minCorner.
!          \item {\tt orientationVector} associates each dimension of patch A
!                with a dimension in patch B's index space. Negative index
!                values may be used to indicate a reversal in index orientation.
!          \end{itemize}
!     \item[{[connectionTransformList]}]
!          List of transforms associated with patch connections defined in 
!          {\tt connectionList}. The second dimension of {\tt connectionTransformList}
!          steps through the connection transforms, defined by the first index. The
!          first index must be of size {\tt 5 + dimCount}, where {\tt dimCount}
!          is the rank of the decomposed index space. Each 
!          {\tt connectionTransformList} element specifies a connection transform 
!          by a list of integer values in the format {\tt (/connectionIndex,
!          direction, staggerSrc, staggerDst, offsetDst, signVector/)}, where
!          \begin{itemize}
!          \item {\tt connectionIndex} corresponds to the index of the connection in
!                {\tt connectionList},
!          \item {\tt direction} can be {\tt +1} to specify forward direction,
!                i.e. source patch of the transform is patch\_A and destination
!                patch is patch\_B of the corresponding connection, or {\tt -1}
!                to indicate reverse direction through the connection. The only
!                other valid {\tt direction} value is {\tt 0} which indicates a 
!                bidirectional connection with source and destination definitions
!                as in the forward case. 
!          \item {\tt staggerSrc} and {\tt staggerDst} indicate staggering location
!                in the source and destination patch interface, respectively,
!          \item {\tt offsetDst} is a vector of size {\tt dimCount} that 
!                specifies the index offset on the destination side of 
!                this connection,
!          \item {\tt signVector} is of size {\tt dimCount} with elements either
!                {\tt +1} or {\tt -1} to indicate optional sign change of vector
!                components along the respective directions.
!          \end{itemize}
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI

      end function ESMF_newGridCreateReg


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridCreateIrreg"
!BOPI
! !IROUTINE: ESMF_newGridCreate - Create a Grid with an irregular decomposition

! !INTERFACE:
  ! Private name; call using ESMF_newGridCreate()
   function ESMF_newGridCreateIrreg(name,coordTypeKind, minIndex, countsPerDE, dimmap, &
                         lbounds, ubounds, coordRanks, &
                        coordDimMap, &
                        indexflag, gridType, noData, &
                        computationalLWidth, computationalUWidth, petMap, &
                         connectionList, connectionTransformList, rc)
!
! !RETURN VALUE:
      type(ESMF_newGrid) :: ESMF_newGridCreateIrreg
!
! !ARGUMENTS:
       character (len=*), intent(in), optional :: name
       type(ESMF_TypeKind),  intent(in),optional  :: coordTypeKind
       integer,               intent(in),         ::   countPerDE(:,:)
       integer,               intent(in),   optional  :: dimmap(:)
       integer,               intent(in),   optional  :: minIndex(:)
       integer,               intent(in),   optional  :: lbounds(:)
       integer,               intent(in),   optional  :: ubounds(:)
       integer,               intent(in),   optional  :: coordRanks(:)
       integer,               intent(in),   optional  :: coordDimMap(:,:)
       type(ESMF_IndexFlag),  intent(in),   optional  :: indexflag
       integer,               intent(in),   optional  :: gridType
       logical,               intent(in),   optional  :: noData
       integer,               intent(in),   optional  :: computationalLWidth(:)
       integer,               intent(in),   optional  :: computationalUWidth(:)
       integer,               intent(in),   optional  :: petMap(:)
       integer,               intent(in),   optional  :: connectionList(:,:)
       integer,               intent(in),   optional  :: connectionTransformList(:,:)
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
! \item[{[coordRanks]}]
!      List that has as many elements as the grid rank .
!      Gives the dimension of each component (e.g. x) array. This is 
!      to allow factorization of the coordinate arrays. If not specified
!      all arrays are the same size as the grid. 
! \item[{[coordDimMap]}]
!      2D list of size grid rank x grid rank. This array describes the
!      map of each component array's dimensions onto the grids
!      dimensions. Each entry {\tt coordDimMap(i,j)} tells which
!      grid dimension component i's, jth dimension maps to. 
!      Note that if j is bigger than {\tt coordRanks(i)} than its ignored.        
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
! \item[{[connectionList]}]
!          List of connections between patches in index space. The second dimension
!          of {\tt connectionList} steps through the connection interface elements, 
!          defined by the first index. The first index must be of size
!          {\tt 2 x dimCount + 2}, where {\tt dimCount} is the rank of the 
!          decomposed index space. Each {\tt connectionList} element specifies
!          the connection interface in the format
!
!         {\tt (/patchIndex\_A,
!          patchIndex\_B, positionVector, orientationVector/)} where:
!          \begin{itemize}
!          \item {\tt patchIndex\_A} and {\tt patchIndex\_B} are the patch
!                index of the two connected patches respectively,
!          \item {\tt positionVector} is the vector that points from patch A's
!                minCorner to patch B's minCorner.
!          \item {\tt orientationVector} associates each dimension of patch A
!                with a dimension in patch B's index space. Negative index
!                values may be used to indicate a reversal in index orientation.
!          \end{itemize}
!     \item[{[connectionTransformList]}]
!          List of transforms associated with patch connections defined in 
!          {\tt connectionList}. The second dimension of {\tt connectionTransformList}
!          steps through the connection transforms, defined by the first index. The
!          first index must be of size {\tt 5 + dimCount}, where {\tt dimCount}
!          is the rank of the decomposed index space. Each 
!          {\tt connectionTransformList} element specifies a connection transform 
!          by a list of integer values in the format {\tt (/connectionIndex,
!          direction, staggerSrc, staggerDst, offsetDst, signVector/)}, where
!          \begin{itemize}
!          \item {\tt connectionIndex} corresponds to the index of the connection in
!                {\tt connectionList},
!          \item {\tt direction} can be {\tt +1} to specify forward direction,
!                i.e. source patch of the transform is patch\_A and destination
!                patch is patch\_B of the corresponding connection, or {\tt -1}
!                to indicate reverse direction through the connection. The only
!                other valid {\tt direction} value is {\tt 0} which indicates a 
!                bidirectional connection with source and destination definitions
!                as in the forward case. 
!          \item {\tt staggerSrc} and {\tt staggerDst} indicate staggering location
!                in the source and destination patch interface, respectively,
!          \item {\tt offsetDst} is a vector of size {\tt dimCount} that 
!                specifies the index offset on the destination side of 
!                this connection,
!          \item {\tt signVector} is of size {\tt dimCount} with elements either
!                {\tt +1} or {\tt -1} to indicate optional sign change of vector
!                components along the respective directions.
!          \end{itemize}
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI

      end function ESMF_newGridCreateIrreg


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridCreate"
!BOPI
! !IROUTINE: ESMF_newGridCreate - Create a new grid 

! !INTERFACE:
  ! Private name; call using ESMF_newGridCreate()
   function ESMF_newGridCreateRegP(name,coordTypeKind, minIndex, countsPerDE, dimmap, &
                         lbounds, ubounds, coordRanks, &
                        coordDimMap, &
                        indexflag, gridType, noData, &
                        computationalLWidth, computationalUWidth, petMap, &
                         connectionList, connectionTransformList, rc)
!
! !RETURN VALUE:
      type(ESMF_newGrid) :: ESMF_newGridCreateReg
!
! !ARGUMENTS:
       character (len=*), intent(in), optional :: name
       type(ESMF_TypeKind),  intent(in),optional  :: coordTypeKind
       integer,               intent(in),         ::   countPerDE(:,:,:)
       integer,               intent(in),   optional  :: dimmap(:)
       integer,               intent(in),   optional  :: minIndex(:,:)
       integer,               intent(in),   optional  :: lbounds(:)
       integer,               intent(in),   optional  :: ubounds(:)
       integer,               intent(in),   optional  :: coordRanks(:)
       integer,               intent(in),   optional  :: coordDimMap(:,:)
       type(ESMF_IndexFlag),  intent(in),   optional  :: indexflag
       integer,               intent(in),   optional  :: gridType
       logical,               intent(in),   optional  :: noData
       integer,               intent(in),   optional  :: computationalLWidth(:,:)
       integer,               intent(in),   optional  :: computationalUWidth(:,:)
       integer,               intent(in),   optional  :: petMap(:,:)
       integer,               intent(in),   optional  :: connectionList(:,:)
       integer,               intent(in),   optional  :: connectionTransformList(:,:)
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
! \item[{[coordRanks]}]
!      List that has as many elements as the grid rank .
!      Gives the dimension of each component (e.g. x) array. This is 
!      to allow factorization of the coordinate arrays. If not specified
!      all arrays are the same size as the grid. 
! \item[{[coordDimMap]}]
!      2D list of size grid rank x grid rank. This array describes the
!      map of each component array's dimensions onto the grids
!      dimensions. Each entry {\tt coordDimMap(i,j)} tells which
!      grid dimension component i's, jth dimension maps to. 
!      Note that if j is bigger than {\tt coordRanks(i)} than its ignored.        
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
! \item[{[connectionList]}]
!          List of connections between patches in index space. The second dimension
!          of {\tt connectionList} steps through the connection interface elements, 
!          defined by the first index. The first index must be of size
!          {\tt 2 x dimCount + 2}, where {\tt dimCount} is the rank of the 
!          decomposed index space. Each {\tt connectionList} element specifies
!          the connection interface in the format
!
!         {\tt (/patchIndex\_A,
!          patchIndex\_B, positionVector, orientationVector/)} where:
!          \begin{itemize}
!          \item {\tt patchIndex\_A} and {\tt patchIndex\_B} are the patch
!                index of the two connected patches respectively,
!          \item {\tt positionVector} is the vector that points from patch A's
!                minCorner to patch B's minCorner.
!          \item {\tt orientationVector} associates each dimension of patch A
!                with a dimension in patch B's index space. Negative index
!                values may be used to indicate a reversal in index orientation.
!          \end{itemize}
!     \item[{[connectionTransformList]}]
!          List of transforms associated with patch connections defined in 
!          {\tt connectionList}. The second dimension of {\tt connectionTransformList}
!          steps through the connection transforms, defined by the first index. The
!          first index must be of size {\tt 5 + dimCount}, where {\tt dimCount}
!          is the rank of the decomposed index space. Each 
!          {\tt connectionTransformList} element specifies a connection transform 
!          by a list of integer values in the format {\tt (/connectionIndex,
!          direction, staggerSrc, staggerDst, offsetDst, signVector/)}, where
!          \begin{itemize}
!          \item {\tt connectionIndex} corresponds to the index of the connection in
!                {\tt connectionList},
!          \item {\tt direction} can be {\tt +1} to specify forward direction,
!                i.e. source patch of the transform is patch\_A and destination
!                patch is patch\_B of the corresponding connection, or {\tt -1}
!                to indicate reverse direction through the connection. The only
!                other valid {\tt direction} value is {\tt 0} which indicates a 
!                bidirectional connection with source and destination definitions
!                as in the forward case. 
!          \item {\tt staggerSrc} and {\tt staggerDst} indicate staggering location
!                in the source and destination patch interface, respectively,
!          \item {\tt offsetDst} is a vector of size {\tt dimCount} that 
!                specifies the index offset on the destination side of 
!                this connection,
!          \item {\tt signVector} is of size {\tt dimCount} with elements either
!                {\tt +1} or {\tt -1} to indicate optional sign change of vector
!                components along the respective directions.
!          \end{itemize}
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI

      end function ESMF_newGridCreateRegP



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridCreateShapeArb"
!BOPI
! !IROUTINE: ESMF_newGridCreateShape - Create a Grid with an arbitrary distribution

! !INTERFACE:
  ! Private name; call using ESMF_newGridCreateShape()
      function ESMF_newGridCreateShapeArb(name,coordTypeKind,  &
                        minIndex, maxIndex, localIndices, &
                        connDim1, connDim2, connDim3, &
                        poleStaggerLoc1, poleStaggerLoc2, poleStaggerLoc3, &
                        bipolePos1, bipolePos2, bipolePos3, &
                        coordDep1, coordDep2, coordDep3, &
                        indexflag, gridType, haloDepth, petMap, rc)
!
! !RETURN VALUE:
      type(ESMF_newGrid) :: ESMF_newGridCreateShapeArb
!
! !ARGUMENTS:
      character (len=*), intent(in), optional :: name 
       type(ESMF_TypeKind),  intent(in),    optional  :: coordTypeKind
       integer,               intent(in),   optional  :: minIndex(:)
       integer,               intent(in),             :: maxIndex(:)
       integer,               intent(in),             :: localIndices(:,:)
       type(ESMF_newGridConn),   intent(in),   optional  :: connDim1(2)
       type(ESMF_newGridConn),   intent(in),   optional  :: connDim2(2)
       type(ESMF_newGridConn),   intent(in),   optional  :: connDim3(2)
       type(ESMF_StaggerLoc), intent(in),   optional  :: poleStaggerLoc1(2)
       type(ESMF_StaggerLoc), intent(in),   optional  :: poleStaggerLoc2(2)
       type(ESMF_StaggerLoc), intent(in),   optional  :: poleStaggerLoc3(2)
       integer,               intent(in),   optional  :: bipolePos1(2)
       integer,               intent(in),   optional  :: bipolePos2(2)
       integer,               intent(in),   optional  :: bipolePos3(2)
       integer,               intent(in),   optional  :: coordDep1(:)
       integer,               intent(in),   optional  :: coordDep2(:)
       integer,               intent(in),   optional  :: coordDep3(:)
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
! \item[{[poleStaggerLoc1]}] 
!     Two element array describing the index dimension 1 connections.
!      The first element represents the minimum end of dimension 1.
!      The second element represents the minimum end of dimension 1.
!      If a pole, this describes which staggerlocation is at the pole at each end.
!      If not present, the default is the edge.
! \item[{[poleStaggerLoc2]}] 
!     Two element array describing the index dimension 2 connections.
!      The first element represents the minimum end of dimension 2.
!      The second element represents the minimum end of dimension 2.
!      If a pole, this describes which staggerlocation is at the pole at each end.
!      If not present, the default is the edge.
! \item[{[poleStaggerLoc3]}] 
!     Two element array describing the index dimension 3 connections.
!      The first element represents the minimum end of dimension 3.
!      The second element represents the minimum end of dimension 3.
!      If a pole, this describes which staggerlocation is at the pole at each end.
!      If not present, the default is the edge.
! \item[{[bipolePos1]}] 
!     Two element array describing the index dimension 1 connections.
!      The first element represents the minimum end of dimension 1.
!      The second element represents the minimum end of dimension 1.
!      If a bipole, this gives the index position of one of the poles.
!      The other is half way around. If not present, the default is 1.
! \item[{[bipolePos2]}] 
!     Two element array describing the index dimension 2 connections.
!      The first element represents the minimum end of dimension 2.
!      The second element represents the minimum end of dimension 2.
!      If a bipole, this gives the index position of one of the poles.
!      The other is half way around. If not present, the default is 1.
! \item[{[bipolePos3]}] 
!     Two element array describing the index dimension 3 connections.
!      The first element represents the minimum end of dimension 3.
!      The second element represents the minimum end of dimension 3.
!      If a bipole, this gives the index position of one of the poles.
!      The other is half way around. If not present, the default is 1.
! \item[{[indexflag]}]
!      Flag that indicates how the DE-local indices are to be defined.
! \item[{[coordDep1]}] 
!     This array specifies the dependence of the first 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the first
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is (/1/). 
! \item[{[coordDep2]}] 
!     This array specifies the dependence of the second 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the second
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is (/2/). 
! \item[{[coordDep3]}] 
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
!EOPI

      end function ESMF_newGridCreateShapeArb

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_newGridGetAttribute  - Retrieve an attribute
!
! !INTERFACE:
!      subroutine ESMF_newGridGetAttribute(grid, name, <value argument>, rc)
!
! !ARGUMENTS:
!      type(ESMF_newGrid), intent(in) :: grid
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
!EOPI


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_newGridGetInternalState - Get private data block pointer
!
! !INTERFACE:
!      subroutine ESMF_newGridGetInternalState(gridcomp, dataPointer, rc)
!
! !ARGUMENTS:
!      type(ESMF_newGrid), intent(inout) :: gridcomp
!      type(any), pointer, intent(in) :: dataPointer
!      integer, intent(out) :: rc
!
! !DESCRIPTION:
!  Get a pointer to a user defined object from ESMF.  A corresponding 
!  {\tt ESMF\_GridSetInternalState} call sets the pointer, and this call
!  retrieves the pointer. Note that the {\tt dataPointer} argument needs to be a derived type
!  which contains only a pointer of the type of the data block defined
!  by the user.  When making this call the pointer needs to be unassociated.
!  When the call returns the pointer will now reference the original
!  data block which was set during the previous call to
!  {\tt ESMF\_GridSetInternalState}.

!    
!  The arguments are:
!  \begin{description}
!   \item[grid]
!    An {\tt ESMF\_Grid} object.
!   \item[dataPointer]
!    A derived type, containing only an unassociated pointer 
!    to the private data block.
!    The framework will fill in the pointer. When this call returns the
!    pointer is set to the same address set during 
!    {\tt ESMF\_GridSetInternalState}.
!    This level of indirection is needed to reliably set and retrieve 
!    the data block no matter which architecture or compiler is used.  
!   \item[rc] 
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!    Note: unlike most other ESMF routines, this argument is not optional
!    because of implementation considerations.
!   \end{description}
!
!EOPI


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridGetMetricIntoFptr"
!BOPI
! !IROUTINE: ESMF_newGridGetMetricIntoFptr -  Gets metric data from a grid and puts it into a Fortran pointer.

! !INTERFACE:
  ! Private name; call using ESMF_newGridGetMetric()
      subroutine ESMF_newGridGetMetricIntoFptr(grid, name, metricTag, staggerLoc, dep, &
                            fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_newGrid), intent(in) :: grid
      character (len=*), intent(in) :: name
      type(ESMF_MetricTag),  intent(in),    optional  :: metricTag
      type (ESMF_StaggerLoc), intent(out), optional  :: staggerLoc
      real, intent(in), pointer :: fptr
      integer,               intent(out),   optional  :: dep(:)
      type(ESMF_CopyFlag), intent(in), optional :: docopy
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    Gets a grid metric into a Fortran pointer. This subroutine only
!    works when there's a 1-to-1 DE to PET match. If not, then
!    use Array to get the metric data. This subroutine also needs to 
!    be overloaded to cover the full range of type/kinds.
!
!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          The grid to get the metric from.
!     \item [name]
!           The name of the metric to get.
!      \item[{[metricTag]}] 
!           Identifies the type of metric to ESMF. If not present, defaults
!           to ESMF\_METRICTAG\_NONE. 
!     \item[{[staggerLoc]}]
!          The stagger location where the metric is located.            
!     \item[{[dep]}]
!          This array specifies the dependence of the metric dimensions 
!          on the grid index dimensions. The size of the 
!          array specifies the number of dimensions of the metric.
!          The values specify which of the grid index dimensions the corresponding
!           map to. If a value is 0, then that dimension doesn't correspond to 
!          a grid dimension. If not present the default is (/1,2,3,.../). 
!     \item[{fptr}]
!          The pointer to the metric data. 
!     \item[{[doCopy]}]
!          Default to {\tt ESMF\_DATA\_REF}, makes the grid reference the passed
!          in array. If set to {\tt ESMF\_DATA\_COPY} this routine makes a copy
!          of the array.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

      end subroutine ESMF_newGridGetMetricIntoFptr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridHalo"
!BOPI
! !IROUTINE: ESMF_newGridHalo - Do a halo operation on the coordinate arrays in a grid.

! !INTERFACE:
      subroutine ESMF_newGridHalo(grid, regionFlag, haloLDepth, haloUDepth, rc)

! !ARGUMENTS:
      type(ESMF_newGrid) :: ESMF_newGridHalo
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
!EOPI


      end function ESMF_newGridHalo



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridLocalTileCalcBnds"
!BOPI
! !IROUTINE: ESMF_newGridLocalTileCalcBnds - Given 

! !INTERFACE:
     subroutine ESMF_newGridLocalTileCalcBnds(grid, tile, localDE, coord, staggerLoc, &
                        staggerLocLWidth, staggerLocUWidth, &
                        staggerAlign, lBounds, uBounds, rc)
!
! !ARGUMENTS:
      type(ESMF_newGrid), intent(in) :: grid 
      integer,               intent(in), optional :: tile 
      integer,               intent(in), optional :: localDE
      integer, intent(in),  optional :: coord
      type (ESMF_StaggerLoc), intent(in)  :: staggerLoc
      integer,               intent(in),   optional  :: staggerLocLWidth(:)
      integer,               intent(in),   optional  :: staggerLocUWidth(:)
      integer,               intent(in),   optional  :: staggerAlign(:)
      integer,               intent(out)             :: lBounds(:),uBounds(:)
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
!\item[{coord}]
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
!       of the array for the given tile, DE, staggerLoc, and coord.
! \item[{uBounds}] 
!      This array should have size=rank of the coord. comp. It specifies the upper bounds
!       of the array for the given tile, DE, staggerLoc, and coord.
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI

      end function ESMF_newGridLocalTileCalcBnds


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridLocalTileGetCoord"
!BOPI
! !IROUTINE: ESMF_newGridLocalTileGetCoord - Get coordinates of a local tile given indices

! !INTERFACE:
      subroutine ESMF_newGridLocalTileGetCoordIntoArray(grid, staggerLoc, &
                 tile, localDE, indices, coords, rc)
!
! !ARGUMENTS:
      type(ESMF_newGrid), intent(in) :: grid
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
!EOPI

      end subroutine ESMF_newGridLocalTileGetCoord


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridLocalTileGetMetric"
!BOPI
! !IROUTINE: ESMF_newGridLocalTileGetMetric - get the fortran data pointer for the piece of  metic data on this tile on this DE.

! !INTERFACE:
      subroutine ESMF_newGridLocalTileGetMetric(grid, name, metricTag, tile, localDE, &
                                      fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_newGrid), intent(in) :: grid
      character (len=*), intent(in) :: name
       type(ESMF_MetricTag),  intent(in),    optional  :: metricTag
      integer, intent(in),optional :: tile
      integer, intent(in),optional :: localDE
      real, intent(out), optional :: fptr(:,:)
      type(ESMF_CopyFlag), intent(in), optional :: docopy
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    Gets a fortran pointer to the metric data for the piece of tile on a local DE. 
!    This routine will need to be overloaded to cover the full range of ranks, types, 
!    and kinds. 

!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          Grid to get the information from.
!     \item [name]
!           The name of the metric to get.
!      \item[{[metricTag]}] 
!           Identifies the type of metric to ESMF. If not present, defaults
!           to ESMF\_METRICTAG\_NONE. 
!     \item[{[tile]}]
!          The grid tile to get the information for. If not set, defaults to 
!          the first tile. 
!     \item[{[localDE]}]
!          The local DE to get the information for. If not set, defaults to 
!          the first DE on this processor. 
!     \item[{[doCopy]}]
!          Default to {\tt ESMF\_DATA\_COPY},  copys the array. 
!          If set to {\tt ESMF\_DATA\_REF}, fptr is a reference to the data in the 
!          grid arrays. 
!     \item[{fptr}]
!          The pointer to the coordinate data.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

      end subroutine ESMF_newGridLocalTileGetMetric


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridLocalTileSetCoord"
!BOPI
! !IROUTINE: ESMF_newGridLocalTileSetCoord - Set the coordinates of a local tile given indices

! !INTERFACE:
      subroutine ESMF_newGridLocalTileSetCoord(grid, staggerLoc, tile, localDE, &
                            indices, coords, rc)
!
! !ARGUMENTS:
      type(ESMF_newGrid), intent(in) :: grid
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
!EOPI

      end subroutine ESMF_newGridLocalTileSetCoord


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridLocalTileSetMetric"
!BOPI
! !IROUTINE: ESMF_newGridLocalTileSetMetric - set the metic data on this tile on this DE using a fortran pointer.

! !INTERFACE:
      subroutine ESMF_newGridLocalTileSetMetric(grid, name, metricTag, tile, localDE, &
                                      fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_newGrid), intent(in) :: grid
      character (len=*), intent(in) :: name
      type(ESMF_MetricTag),  intent(in),    optional  :: metricTag
      integer, intent(in),optional :: tile
      integer, intent(in),optional :: localDE
      real, intent(out), optional :: fptr(:,:)
      type(ESMF_CopyFlag), intent(in), optional :: docopy
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    Sets a fortran pointer to the metric data for the piece of tile on a local DE. 
!    This routine will need to be overloaded to cover the full range of ranks, types, 
!    and kinds. 

!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          Grid to get the information from.
!     \item [name]
!           The name of the metric to set.
!      \item[{[metricTag]}] 
!           Identifies the type of metric to ESMF. If not present, defaults
!           to ESMF\_METRICTAG\_NONE. 
!     \item[{[tile]}]
!          The grid tile to set the information in. If not set, defaults to 
!          the first tile. 
!     \item[{[localDE]}]
!          The local DE to set the information in. If not set, defaults to 
!          the first DE on this processor. 
!     \item[{[doCopy]}]
!          Default to {\tt ESMF\_DATA\_COPY},  copys the array. 
!          If set to {\tt ESMF\_DATA\_REF}, fptr is a reference to the data in the 
!          grid arrays. 
!     \item[{fptr}]
!          The pointer to the metric data.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

      end subroutine ESMF_newGridLocalTileSetMetric


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridMetricGet"
!BOPI
! !IROUTINE: ESMF_newGridMetricGet - Get information about a particular metric. 

! !INTERFACE:
     subroutine ESMF_newGridMetricGet(grid, name, metricTag, staggerLoc, &
                        metricLWidth, metricUWidth, &
                        metricAlign, rc)
!
! !ARGUMENTS:
      type(ESMF_newGrid), intent(in) :: grid 
      character (len=*), intent(in) :: name
      type (ESMF_StaggerLoc), intent(out), optional  :: staggerLoc
      type(ESMF_MetricTag),  intent(out),   optional  :: metricTag
      integer,               intent(out),   optional  :: metricLWidth(:)
      integer,               intent(out),   optional  :: metricUWidth(:)
      integer,               intent(out),   optional  :: metricAlign(:)
      integer,               intent(out),  optional  :: rc
!
! !DESCRIPTION:
!  Get information about a particular stagger location.
!
! The arguments are:
!\begin{description}
!\item[{grid}]
!          Grid to get information from.
!\item [name]
!           The name of the metric to set.
!\item[{[metricTag]}] 
!           Identifies the type of metric to ESMF. 
!\item[{staggerLoc}]
!        The stagger location at which the metric is located.
!\item[{[metricLWidth]}] 
!        This array should be the same rank as the grid. It specifies the lower corner of the computational
!        region with respect to the lower corner of the exclusive region.
!\item[{[metricUWidth]}] 
!        This array should be the same rank as the grid. It specifies the upper corner of the computational
!       region with respect to the lower corner of the exclusive region.
!\item[{[metricAlign]}] 
!      This array is of size  grid rank.
!      For this stagger location, it specifies which element
!      has the same index value as the center. For example, 
!      for a 2D cell with corner stagger it specifies which 
!      of the 4 corners has the same index as the center. 
!      If this is set and metricUWidth is not,
!      this determines the default array padding for a stagger. 
!      If not set, then this defaults to all negative. (e.g. 
!      The most negative part of the stagger in a cell is aligned with the 
!      center and the padding is all on the postive side.) 
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI

      end function ESMF_newGridMetricGet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridGetMetricIntoArray"
!BOPI
! !IROUTINE: ESMF_newGridGetMetricIntoArray - Gets metric data from a grid and puts it into an Array.

! !INTERFACE:
  ! Private name; call using ESMF_newGridGetMetric()
      subroutine ESMF_newGridGetMetricIntoArray(grid, name, metricTag, staggerLoc, &
                            array, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_newGrid), intent(in) :: grid
      character (len=*), intent(in) :: name
       type(ESMF_MetricTag),  intent(in),    optional  :: metricTag
      type (ESMF_StaggerLoc), intent(out), optional  :: staggerLoc
      type(ESMF_Array), intent(inout) :: array
      type(ESMF_CopyFlag), intent(in), optional :: docopy
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    Sets a grid metric from an array. 
!
!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          The grid to get the metric from.
!     \item [name]
!           The name of the attribute to get.
!      \item[{[metricTag]}] 
!           Identifies the type of metric to ESMF. If not present, defaults
!           to ESMF\_METRICTAG\_NONE.            
!     \item[{[staggerLoc]}]
!          The stagger location where the metric is located. 
!     \item[{array}]
!          An array to set the grid metric information from.
!     \item[{[doCopy]}]
!          Default to {\tt ESMF\_DATA\_REF}, makes the grid reference the passed
!          in array. If set to {\tt ESMF\_DATA\_COPY} this routine makes a copy
!          of the array.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

      end subroutine ESMF_newGridGetMetricIntoArray

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridLocalTileMetricGet"
!BOPI
! !IROUTINE: ESMF_newGridLocalTileMetricGet - get various types of information about the part of some metric data which lies on this DE.

! !INTERFACE:
      subroutine ESMF_newGridLocalTileMetricGet(grid, name, tile, coord, localDE, staggerLoc, &
          exclusiveLBound, exclusiveUBound, computationalLBound, &
          computationalUBound, totalLBound, totalUBound, &
          computationalLWidth, computationalUWidth, &
          totalLWidth, totalUWidth,rc)

!
! !ARGUMENTS:
      type(ESMF_newGrid), intent(in) :: grid
      integer, intent(in),optional :: tile
      character (len=*), intent(in):: name
     integer, intent(in),optional :: localDE
      integer, intent(in),  optional :: coord
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
!     \item [name]
!           The name of the metric to get information for.
!     \item[{[tile]}]
!          The grid tile to get the information for. If not set, defaults to 
!          the first tile. 
!     \item[{[localDE]}]
!          The local DE from which to get the information.  If not set, defaults to 
!          the first DE on this processor. 
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
!EOPI

      end subroutine ESMF_newGridLocalTileMetricGet


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridSet3DCoordFromArray"

!BOPI
! !IROUTINE: ESMF_newGridSetCoord - Set 3D coordinate values from ESMF Arrays

! !INTERFACE:
  ! Private name; call using ESMF_newGridSetCoord()
     function ESMF_newGridSet3DCoordFromArray(grid, staggerLoc, &
                        coord1, coord2, coord3, &
                        coordLWidth, coordUWidth, &
                        coordAlign, rc)
!
! !ARGUMENTS:
      type(ESMF_newGrid),       intent(in)             :: grid 
      type (ESMF_StaggerLoc), intent(in)       :: staggerLoc
      type(ESMF_ARRAY), intent(in)            :: coord1(:), coord2(:)
      type(ESMF_ARRAY), intent(in)            :: coord3(:)
      integer,               intent(in),   optional  :: coordLWidth(:)
      integer,               intent(in),   optional  :: coordUWidth(:)
      integer,               intent(in),   optional  :: coordAlign(:)
      type(ESMF_CopyFlag),   intent(in), optional :: docopy
      integer,               intent(out),  optional  :: rc
!
! !DESCRIPTION:
!  Add a stagger location to a grid. This subroutine lets the user add a
! stagger location and set the coordinates from F90 pointers at the same
! time. This subroutine is only usable for grids up to 4D. 
!
! The arguments are:
! \begin{description}
!     \item[{grid}]
!      Partially created Grid to set information into.
! \item[{[staggerLoc]}]
!        The stagger location to add.
! \item[{[coord1]}]
!        ESMF Array holding coordinate data for the first coordinate component (e.g. x).
! \item[{[coord2]}]
!         ESMF Array holding coordinate data for the second coordinate component (e.g. y).
! \item[{[coord3]}]
!         ESMF Array holding coordinate data for the third coordinate component (e.g. z).
! \item[{[coordLWidth]}] 
!      This array should be the same rank as the grid. It specifies the lower corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[coordUWidth]}] 
!      This array should be the same rank as the grid. It specifies the upper corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[coordAlign]}] 
!      This array is of size  grid rank.
!      For this stagger location, it specifies which element
!      has the same index value as the center. For example, 
!      for a 2D cell with corner stagger it specifies which 
!      of the 4 corners has the same index as the center. 
!      If this is set and coordUWidth is not,
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
!EOPI

      end function ESMF_newGridSet3DCoordFromArray

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_newGridSetAttribute - Set an attribute
!
! !INTERFACE:
!      subroutine ESMF_newGridSetAttribute(grid, name, <value argument>, rc)
!
! !ARGUMENTS:
!      type(ESMF_newGrid), intent(inout) :: grid
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
!EOPI

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridSetCoordFptr"
!BOPI
! !IROUTINE: ESMF_newGridSetCoord - Set coordinate values from R8 Fortran arrays 

! !INTERFACE:
  ! Private name; call using ESMF_newGridSetCoord()
     function ESMF_newGridSetCoordFptr(grid, staggerLoc, &
                        coord1, coord2, coord3, &
                        coordLWidth, coordUWidth, &
                        coordAlign, rc)
!
! !ARGUMENTS:
      type(ESMF_newGrid),       intent(in)             :: grid 
      type (ESMF_StaggerLoc), intent(in)       :: staggerLoc
      real (ESMF_KIND_R8), intent(in)            :: coord1(:), coord2(:)
      real (ESMF_KIND_R8), intent(in)            :: coord3(:)
      integer,               intent(in),   optional  :: coordLWidth(:)
      integer,               intent(in),   optional  :: coordUWidth(:)
      integer,               intent(in),   optional  :: coordAlign(:)
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
! \item[{[coord1]}]
!        The F90 pointer to coordinate data for the first coordinate component (e.g. x).
! \item[{[coord2]}]
!        The F90 pointer to coordinate data for the second coordinate component (e.g. y).
! \item[{[coord3]}]
!        The F90 pointer to coordinate data for the third coordinate component (e.g. z).
! \item[{[coordLWidth]}] 
!      This array should be the same rank as the grid. It specifies the lower corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[coordUWidth]}] 
!      This array should be the same rank as the grid. It specifies the upper corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[coordAlign]}] 
!      This array is of size  grid rank.
!      For this stagger location, it specifies which element
!      has the same index value as the center. For example, 
!      for a 2D cell with corner stagger it specifies which 
!      of the 4 corners has the same index as the center. 
!      If this is set and coordUWidth is not,
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
!EOPI

      end function ESMF_newGridSetCoordFptr


!------------------------------------------------------------------------------


#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridSetInternalState"
!BOPI
! !IROUTINE: ESMF_newGridSetInternalState - Set private data block pointer
!
! !INTERFACE:
!      subroutine ESMF_newGridSetInternalState(gridcomp, dataPointer, rc)
!
! !ARGUMENTS:
!      type(ESMF_newGrid), intent(inout) :: gridcomp
!      type(any), pointer, intent(in) :: dataPointer
!      integer, intent(out) :: rc
!
! !DESCRIPTION:
!  Registers a pointer to a user defined structure in ESMF. A corresponding 
!  {\tt ESMF\_GridGetInternalState} call retrieves the data pointer.
!    
!  The arguments are:
!  \begin{description}
!   \item[grid]
!    An {\tt ESMF\_Grid} object.
!   \item[dataPointer]
!    A pointer to the private data block, wrapped in a derived type which
!    contains only a pointer to the block.  This level of indirection is
!    needed to reliably set and retrieve the data block no matter which
!    architecture or compiler is used.  
!   \item[rc] 
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!    Note: unlike most other ESMF routines, this argument is not optional
!    because of implementation considerations.
!   \end{description}
!
!EOPI
 
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_newGridSetMetricFromArray"
!BOPI
! !IROUTINE: ESMF_newGridSetMetricFromArray - Add a new metric from an Array.

! !INTERFACE:
  ! Private name; call using ESMF_newGridSetMetric()
      subroutine ESMF_newGridSetMetricFromArray(grid, name, metricTag, &
                            array, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_newGrid), intent(in) :: grid
      character (len=*), intent(in) :: name
      type(ESMF_MetricTag),  intent(in),    optional  :: metricTag
      type(ESMF_Array), intent(in) :: array
      type(ESMF_CopyFlag), intent(in), optional :: docopy
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    Sets a grid metric from an array. 
!
!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          The grid to set the metric in.
!     \item [name]
!           The name of the attribute to set.
!      \item[{[metricTag]}] 
!           Identifies the type of metric to ESMF. If not present, defaults
!           to ESMF\_METRICTAG\_NONE.            
!     \item[{array}]
!          An array to set the grid metric information from.
!     \item[{[doCopy]}]
!          Default to {\tt ESMF\_DATA\_REF}, makes the grid reference the passed
!          in array. If set to {\tt ESMF\_DATA\_COPY} this routine makes a copy
!          of the array.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

      end subroutine ESMF_newGridSetMetricFromArray

!------------------------------------------------------------------------------
#endif ! Take out so you don't have to worry about compiler warnings for now

      end module ESMF_newGridMod

