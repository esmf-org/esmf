! $Id: ESMF_Grid.F90,v 1.17 2007/07/25 19:19:58 cdeluca Exp $
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
      use ESMF_StaggerLocMod
      use ESMF_DistGridMod
      use ESMF_F90InterfaceMod  ! ESMF F90-C++ interface helper
      
!     NEED TO ADD MORE HERE
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
! ! ESMF_Grid
!
!------------------------------------------------------------------------------

  ! F90 class type to hold pointer to C++ object
  type ESMF_Grid
  sequence
  private
    type(ESMF_Pointer) :: this
    ESMF_INIT_DECLARE
  end type


!------------------------------------------------------------------------------
! ! ESMF_GridStatus
!
!------------------------------------------------------------------------------
  type ESMF_GridStatus
  sequence
!  private
     integer :: gridstatus
  end type

!------------------------------------------------------------------------------
! ! ESMF_GridConn
!
!------------------------------------------------------------------------------
  type ESMF_GridConn
  sequence
!  private
     integer :: gridconn
  end type

!------------------------------------------------------------------------------
! ! ESMF_DefaultFlag
!
!------------------------------------------------------------------------------
! TODO: eventually move this elsewhere (e.g. Util)
  type ESMF_DefaultFlag
  sequence
!  private
     integer :: defaultflag
  end type

!------------------------------------------------------------------------------
!
! !PUBLIC TYPES:
!
public ESMF_Grid, ESMF_GridStatus, ESMF_DefaultFlag, ESMF_GridConn


!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
!

! - ESMF-public methods:
  public ESMF_GridAllocCoord
  public ESMF_GridCommit
  public ESMF_GridCreate
  public ESMF_GridCreateShape
  public ESMF_GridSetShape
  public ESMF_GridCreateEmpty
  public ESMF_GridDestroy
  public ESMF_GridGet
  public ESMF_GridGetCoord
  public ESMF_GridGetLocalTileCoord
  public ESMF_GridGetLocalTileInfo
  public ESMF_GridSet
  public ESMF_GridSetCoord


! - ESMF-private methods:
  public ESMF_GridGetInit  

!EOPI
! !PRIVATE MEMBER FUNCTIONS:

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Grid.F90,v 1.17 2007/07/25 19:19:58 cdeluca Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================




! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_GridAllocCoord -- Generic interface

! !INTERFACE:
interface ESMF_GridAllocCoord

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_GridAllocCoordNoValues

      
! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_GridAllocCoord} functions.   
!EOPI 
end interface


! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_GridCreate -- Generic interface

! !INTERFACE:
interface ESMF_GridCreate

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_GridCreateFromDistGrid

      
! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_GridCreate} functions.   
!EOPI 
end interface



! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_GridCreateShape -- Generic interface

! !INTERFACE:
interface ESMF_GridCreateShape

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_GridCreateShapeIrreg

      
! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_GridCreateShape} functions.   
!EOPI 
end interface


! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_GridGetCoord -- Generic interface

! !INTERFACE:
interface ESMF_GridGetCoord

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_GridGetCoordIntoArray

      
! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_GridGetCoord} functions.   
!EOPI 
end interface


! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_GridGetLocalTileCoord -- Generic interface

! !INTERFACE:
interface ESMF_GridGetLocalTileCoord

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_GridGetLocalTileCoord1DR8
      module procedure ESMF_GridGetLocalTileCoord2DR8
      module procedure ESMF_GridGetLocalTileCoord3DR8
      module procedure ESMF_GridGetLocalTileCoord1DR4
      module procedure ESMF_GridGetLocalTileCoord2DR4
      module procedure ESMF_GridGetLocalTileCoord3DR4
      
! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_GridGetLocalTileCoord} functions.   
!EOPI 
end interface


! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_GridSet -- Generic interface

! !INTERFACE:
interface ESMF_GridSet

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_GridSetFromDistGrid

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_GridSet} functions.   
!EOPI 
end interface
      

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_GridSetCoord -- Generic interface

! !INTERFACE:
interface ESMF_GridSetCoord

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_GridSetCoordFromArray

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_GridSetCoord} functions.   
!EOPI 
end interface

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_GridSetShape -- Generic interface

! !INTERFACE:
interface ESMF_GridSetShape

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_GridSetShapeIrreg

      
! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_GridSetShape} functions.   
!EOPI 
end interface



!==============================================================================

      contains

!==============================================================================


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAllocCoord"

!BOP
! !IROUTINE: ESMF_GridAllocCoord - Allocate coordinate arrays but don't set their values

! !INTERFACE:
  ! Private name; call using ESMF_GridAllocCoord()
     subroutine ESMF_GridAllocCoordNoValues(grid, staggerLoc,  &
                staggerLWidth, staggerUWidth, staggerAlign, &
                computationalLWidth, computationalUWidth, &
                totalLWidth, totalUWidth,rc)

!
! !ARGUMENTS:
      type(ESMF_Grid),        intent(in)              :: grid 
      type (ESMF_StaggerLoc), intent(in),optional     :: staggerLoc
      integer,                intent(in),optional     :: staggerLWidth(:)
      integer,                intent(in),optional     :: staggerUWidth(:)
      integer,                intent(in),optional     :: staggerAlign(:)
      integer,                intent(out), optional   :: computationalLWidth(:) ! N. IMP
      integer,                intent(out), optional   :: computationalUWidth(:) ! N. IMP
      integer,                intent(out), optional   :: totalLWidth(:)         ! N. IMP
      integer,                intent(out), optional   :: totalUWidth(:)         ! N. IMP
      integer,                intent(out),optional    :: rc
!
! !DESCRIPTION:
! 
!  When a Grid is created all of its potential stagger locations can hold coordinate
!  data, but none of them have storage allocated. This call allocates coordinate
!  storage (creates internal ESMF\_Arrays and associated memory) for  a particular
!  stagger location. Note that this
!  call doesn't assign any values to the storage, it only allocates it. The
!  remaining options {\tt staggerLWidth}, etc. allow the user to adjust the 
!  padding on the coordinate arrays.
!
! The arguments are:
! \begin{description}
!     \item[{grid}]
!       Grid to allocate coordinate storage in.  
! \item[{[staggerLoc]}]
!      The stagger location to add. Please see Section~\ref{sec:opt:staggerloc} for a list 
!      of predefined stagger locations. If not present, defaults to ESMF\_STAGGERLOC\_CENTER.
! \item[{[staggerLWidth]}] 
!      This array should be the same rank as the grid. It specifies the lower corner of the stagger
!      region with respect to the lower corner of the exclusive region.
! \item[{[staggerUWidth]}] 
!      This array should be the same rank as the grid. It specifies the upper corner of the stagger
!      region with respect to the upper corner of the exclusive region.
! \item[{[staggerAlign]}] 
!      This array is of size  grid rank.
!      For this stagger location, it specifies which element
!      has the same index value as the center. For example, 
!      for a 2D cell with corner stagger it specifies which 
!      of the 4 corners has the same index as the center. 
!      If this is set and either staggerUWidth or staggerLWidth is not,
!      this determines the default array padding for a stagger. 
!      If not set, then this defaults to all negative. (e.g. 
!      The most negative part of the stagger in a cell is aligned with the 
!      center and the padding is all on the postive side.) 
!\item[{[computationalLWidth]}]
!     The lower boundary of the computatational region in reference to the exclusive region. 
!     If {\tt staggerLWidth} is present then the actual computational width
!     is the max on a dimension by dimension basis between {\tt staggerLWidth} and
!      {\tt computationalLWidth}. [CURRENTLY NOT IMPLEMENTED]
!\item[{[computationalUWidth]}]
!     The lower boundary of the computatational region in reference to the exclusive region. 
!     If {\tt staggerUWidth} is present then the actual computational width
!     is the max on a dimension by dimension basis between {\tt staggerUWidth} and
!      {\tt computationalUWidth}. [CURRENTLY NOT IMPLEMENTED]
!\item[{[totalLWidth]}]
!     The lower boundary of the computatational region in reference to the computational region. 
!     Note, the computational region includes the extra padding specified by {\tt ccordLWidth}.
!     [CURRENTLY NOT IMPLEMENTED]
!\item[{[totalUWidth]}]
!     The lower boundary of the computatational region in reference to the computational region. 
!     Note, the computational region includes the extra padding specified by {\tt staggerLWidth}.
!     [CURRENTLY NOT IMPLEMENTED]
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP

    integer :: tmp_staggerloc
    integer :: localrc ! local error status
    type(ESMF_InterfaceInt) :: staggerLWidthArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: staggerUWidthArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: staggerAlignArg  ! Language Interface Helper Var

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_GridGetInit, grid, rc)


    ! Check for Not Implemented inputs
    if (present(computationalLWidth)) then
       call ESMF_LogMsgSetError(ESMF_RC_NOT_IMPL, & 
                 "- computationalLWidth specification not yet implemented", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if (present(computationalUWidth)) then
       call ESMF_LogMsgSetError(ESMF_RC_NOT_IMPL, & 
                 "- computationalUWidth specification not yet implemented", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if (present(totalLWidth)) then
       call ESMF_LogMsgSetError(ESMF_RC_NOT_IMPL, & 
                 "- totalLWidth specification not yet implemented", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if (present(totalUWidth)) then
       call ESMF_LogMsgSetError(ESMF_RC_NOT_IMPL, & 
                 "- totalUWidth specification not yet implemented", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    ! handle staggerloc
    if (present(staggerLoc)) then
       tmp_staggerloc=staggerLoc%staggerloc
    else
       tmp_staggerloc=ESMF_STAGGERLOC_CENTER%staggerloc
    endif

    !! staggerLWidth
    staggerLWidthArg = ESMF_InterfaceIntCreate(staggerLWidth, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    !! staggerUWidth
    staggerUWidthArg = ESMF_InterfaceIntCreate(staggerUWidth, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    !! staggerAlign
    staggerAlignArg = ESMF_InterfaceIntCreate(staggerAlign, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    !! staggerAlign
    staggerAlignArg = ESMF_InterfaceIntCreate(staggerAlign, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


    ! Call C++ Subroutine to do the create
    call c_ESMC_gridalloccoord(grid%this,tmp_staggerloc, &
      staggerLWidthArg, staggerUWidthArg, staggerAlignArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deallocate helper variables
    call ESMF_InterfaceIntDestroy(staggerLWidthArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_InterfaceIntDestroy(staggerUWidthArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_InterfaceIntDestroy(staggerAlignArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAllocCoordNoValues


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCommit"
!BOP
! !IROUTINE: ESMF_GridCommit - Commit a Grid to a specified completion level

! !INTERFACE:
      subroutine ESMF_GridCommit(grid, status, defaultflag, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout)     :: grid
      type(ESMF_GridStatus),optional     :: status      ! NOT IMPLEMENTED
      type(ESMF_DefaultFlag), optional   :: defaultflag ! NOT IMPLEMENTED
      integer, intent(out), optional     :: rc
!
! !DESCRIPTION:
!    This call is used to complete the {\tt grid} so that it is usable at
!    the level indicated by the {\tt status} flag.  For example, once committed
!    with a {\tt status} value of {\tt ESMF\_GRIDSTATUS\_SHAPE\_READY}, the 
!    {\tt grid} will have sufficient size, rank, and distribution information to be
!    used as the basis for allocating Field data. (The integration of 
!    Field and Grid classes has't yet happened, so you can't currently 
!    allocating Fields based on Grids no matter what the status.)
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
!          \ref{sec:opt:gridstatus}. [CURRENTLY NOT IMPLEMENTED]  
!     \item[{[defaultFlag]}]
!          Indicates whether to use default values to achieve the desired
!          grid status. The default value is {\tt ESMF\_NO\_DEFAULTS}.  
!          [CURRENTLY NOT IMPLEMENTED]
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
    integer :: localrc ! local error status

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_GridGetInit, grid, rc)

    ! Check for Not Implemented options
    if (present(status)) then
       call ESMF_LogMsgSetError(ESMF_RC_NOT_IMPL, & 
                 "- status not yet implemented", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if (present(defaultflag)) then
       call ESMF_LogMsgSetError(ESMF_RC_NOT_IMPL, & 
                 "- defaultflag not yet implemented", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif



    ! Call C++ Subroutine to do the create
    call c_ESMC_gridcommit(grid%this, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridCommit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreate"
!BOP
! !IROUTINE: ESMF_GridCreate - Create a Grid from a distgrid

! !INTERFACE:
  ! Private name; call using ESMF_GridCreate()
      function ESMF_GridCreateFromDistGrid(name,coordTypeKind,distgrid, dimmap, &
                        lbounds, ubounds, coordRank, coordDimMap, indexflag, gridType, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateFromDistGrid
!
! !ARGUMENTS:
       character (len=*), intent(in), optional :: name
       type(ESMF_TypeKind),  intent(in),   optional  :: coordTypeKind
       type(ESMF_DistGrid),   intent(in)              :: distgrid
       integer,               intent(in),   optional  :: dimmap(:)
       integer,               intent(in),   optional  :: lbounds(:)
       integer,               intent(in),   optional  :: ubounds(:)
       integer,               intent(in),   optional  :: coordRank(:)
       integer,               intent(in),   optional  :: coordDimMap(:,:)
       type(ESMF_IndexFlag),  intent(in),   optional  :: indexflag
       integer,               intent(in),   optional  :: gridType ! NOT IMPLEMENTED
       integer,               intent(out),  optional  :: rc
!
! !DESCRIPTION:
! This is the most general form of creation for an {\tt ESMF\_Grid}
! object. It allows the user to fully specify the topology and index space
! (of the distributed dimensions) using the DistGrid methods and then build a grid out
! of the resulting {\tt distgrid}.  Optional {\tt lbound} and {\tt ubound}
! arguments can be used to specify extra undistributed dimensions. The {\tt dimmap} argument
! specifies how the distributed (from {\tt distgrid}) and undistributed (from {\tt bounds})
! dimensions are intermixed. The {\tt coordRank} and {\tt coordDimMap} arguments
! allow the user to specify how the coordinate arrays should map to the grid
! dimensions. (Note, though, that creating a grid does not allocate coordinate
! storage. A method such as {\tt ESMF\_GridAllocCoord()} must be called
! before adding coordinate values.)
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
!      Lower bounds for tensor array dimensions. Must be the same size as {\tt ubounds}.
! \item[{[ubounds]}] 
!      Upper bounds for tensor array dimensions. Must be the same size as {\tt lbounds}.
! \item[{[coordRank]}]
!      List that has as many elements as the grid rank .
!      Gives the dimension of each component (e.g. x) array. This is 
!      to allow factorization of the coordinate arrays. If not specified
!      all arrays are the same size as the grid. [NOTE FACTORIZATION HAS
!      NOT CURRENTLY BEEN IMPLEMENTED].
! \item[{[coordDimMap]}]
!      2D list of size grid rank x grid rank. This array describes the
!      map of each component array's dimensions onto the grids
!      dimensions. Each entry {\tt coordDimMap(i,j)} tells which
!      grid dimension component i's, jth dimension maps to. 
!      Note that if j is bigger than {\tt coordRank(i)} than its ignored.        
! \item[{[indexflag]}]
!      Indicates whether the indices in the grid are to be interpreted to form
!      a flat pseudo global index space ({\tt ESMF\_INDEX\_GLOBAL}), or are to 
!      be taken as patch local ({\tt ESMF\_INDEX\_DELOCAL}), which is the default.      
! \item[{[gridType]}]
!      Flag that indicates the type of the grid. If not given, defaults
!       to ESMF\_GRIDTYPE\_UNKNOWN. [CURRENTLY NOT IMPLEMENTED]
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
    integer :: localrc ! local error status
    type(ESMF_Grid) :: grid              
    integer :: nameLen 
    type(ESMF_InterfaceInt) :: dimmapArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: lboundsArg ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: uboundsArg ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: coordRankArg  ! Language Interface Helper Var
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
    coordRankArg = ESMF_InterfaceIntCreate(coordRank, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    coordDimMapArg = ESMF_InterfaceIntCreate(farray2D=coordDimMap, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Initialize this grid object as invalid
    grid%this = ESMF_NULL_POINTER

    ! Call C++ Subroutine to do the create
    call c_ESMC_gridcreatefromdistgrid(grid%this, nameLen, name, &
      coordTypeKind, distgrid, dimmapArg, lboundsArg, uboundsArg, coordRankArg, coordDimMapArg, &
      indexflag, gridtype, localrc)
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
    call ESMF_InterfaceIntDestroy(coordRankArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(coordDimMapArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set return value
    ESMF_GridCreateFromDistGrid = grid

    ! Set init status
    ESMF_INIT_SET_CREATED(ESMF_GridCreateFromDistGrid)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

    end function ESMF_GridCreateFromDistGrid


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreateEmpty"
!BOP
! !IROUTINE: ESMF_GridCreateEmpty - Create a Grid that has no contents

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

    integer :: localrc ! local error status
    type(ESMF_Grid) :: grid              

    ! Initialize this grid object as invalid
    grid%this = ESMF_NULL_POINTER

    ! Call C++ Subroutine to do the create
    call c_ESMC_gridcreateempty(grid%this, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


    ! Set return value
    ESMF_GridCreateEmpty = grid

    ! Set init status
    ESMF_INIT_SET_CREATED(ESMF_GridCreateEmpty)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS


      end function ESMF_GridCreateEmpty

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreateShapeIrreg"
!BOP
! !IROUTINE: ESMF_GridCreateShape - Create a Grid with an irregular distribution

! !INTERFACE:
  ! Private name; call using ESMF_GridCreateShape()
      function ESMF_GridCreateShapeIrreg(name,coordTypeKind, minIndex,  &
                        countsPerDEDim1,countsPerDeDim2, countsPerDEDim3, &
                        connDim1, connDim2, connDim3, &
                        poleStaggerLoc1, poleStaggerLoc2, poleStaggerLoc3, &
                        bipolePos1, bipolePos2, bipolePos3, &
                        coordDep1, coordDep2, coordDep3, &
                        indexflag, gridType, petMap, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateShapeIrreg
!
! !ARGUMENTS:
      character (len=*), intent(in), optional :: name 
       type(ESMF_TypeKind),  intent(in),    optional  :: coordTypeKind
       integer,               intent(in),   optional  :: minIndex(:)
       integer,               intent(in)              :: countsPerDEDim1(:)
       integer,               intent(in)              :: countsPerDEDim2(:)
       integer,               intent(in),   optional  :: countsPerDEDim3(:)
       type(ESMF_GridConn),   intent(in),   optional  :: connDim1(:)        ! N. IMP.
       type(ESMF_GridConn),   intent(in),   optional  :: connDim2(:)        ! N. IMP.
       type(ESMF_GridConn),   intent(in),   optional  :: connDim3(:)        ! N. IMP.
       type(ESMF_StaggerLoc), intent(in),   optional  :: poleStaggerLoc1(2) ! N. IMP.
       type(ESMF_StaggerLoc), intent(in),   optional  :: poleStaggerLoc2(2) ! N. IMP.
       type(ESMF_StaggerLoc), intent(in),   optional  :: poleStaggerLoc3(2) ! N. IMP.
       integer,               intent(in),   optional  :: bipolePos1(2)      ! N. IMP.
       integer,               intent(in),   optional  :: bipolePos2(2)      ! N. IMP.
       integer,               intent(in),   optional  :: bipolePos3(2)      ! N. IMP.
       integer,               intent(in),   optional  :: coordDep1(:)
       integer,               intent(in),   optional  :: coordDep2(:)
       integer,               intent(in),   optional  :: coordDep3(:)
       type(ESMF_IndexFlag),  intent(in),   optional  :: indexflag
       integer,               intent(in),   optional  :: gridType           ! N. IMP.
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
!      Fortran array describing the index dimension 1 connections.
!      The first element represents the minimum end of dimension 1.
!      The second element represents the maximum end of dimension 1.
!      If array is only one element long, then that element is used
!      for both the minimum and maximum end. 
!      Please see Section~\ref{sec:opt:gridconn} for a list of valid 
!      options. If not present, defaults to ESMF\_GRIDCONN\_NONE. 
!     [CURRENTLY NOT IMPLEMENTED]
! \item[{[connDim2]}] 
!      Fortran array describing the index dimension 2 connections.
!      The first element represents the minimum end of dimension 2.
!      The second element represents the maximum end of dimension 2.
!      If array is only one element long, then that element is used
!      for both the minimum and maximum end. 
!      Please see Section~\ref{sec:opt:gridconn} for a list of valid 
!      options. If not present, defaults to ESMF\_GRIDCONN\_NONE. 
!     [CURRENTLY NOT IMPLEMENTED]
! \item[{[connDim3]}] 
!      Fortran array describing the index dimension 3 connections.
!      The first element represents the minimum end of dimension 3.
!      The second element represents the maximum end of dimension 3.
!      If array is only one element long, then that element is used
!      for both the minimum and maximum end. 
!      Please see Section~\ref{sec:opt:gridconn} for a list of valid 
!      options. If not present, defaults to ESMF\_GRIDCONN\_NONE. 
!     [CURRENTLY NOT IMPLEMENTED]
! \item[{[poleStaggerLoc1]}] 
!     Two element array describing the index dimension 1 connections.
!      The first element represents the minimum end of dimension 1.
!      The second element represents the maximum end of dimension 1.
!      If a pole, this describes which staggerlocation is at the pole at each end.
!      Please see Section~\ref{sec:opt:staggerloc} for a list 
!      of predefined stagger locations. If not present, defaults to ESMF\_STAGGERLOC\_CENTER.
!     [CURRENTLY NOT IMPLEMENTED]
! \item[{[poleStaggerLoc2]}] 
!     Two element array describing the index dimension 2 connections.
!      The first element represents the minimum end of dimension 2.
!      The second element represents the maximum end of dimension 2.
!      If a pole, this describes which staggerlocation is at the pole at each end.
!      Please see Section~\ref{sec:opt:staggerloc} for a list 
!      of predefined stagger locations. If not present, defaults to ESMF\_STAGGERLOC\_CENTER.
!     [CURRENTLY NOT IMPLEMENTED]
! \item[{[poleStaggerLoc3]}] 
!     Two element array describing the index dimension 3 connections.
!      The first element represents the minimum end of dimension 3.
!      The second element represents the maximum end of dimension 3.
!      If a pole, this describes which staggerlocation is at the pole at each end.
!      Please see Section~\ref{sec:opt:staggerloc} for a list 
!      of predefined stagger locations. If not present, defaults to ESMF\_STAGGERLOC\_CENTER.
!     [CURRENTLY NOT IMPLEMENTED]
! \item[{[bipolePos1]}] 
!     Two element array describing the index dimension 1 connections.
!      The first element represents the minimum end of dimension 1.
!      The second element represents the maximum end of dimension 1.
!      If a bipole, this gives the index position of one of the poles.
!      The other is half way around. If not present, the default is 1.
!     [CURRENTLY NOT IMPLEMENTED]
! \item[{[bipolePos2]}] 
!     Two element array describing the index dimension 2 connections.
!      The first element represents the minimum end of dimension 2.
!      The second element represents the maximum end of dimension 2.
!      If a bipole, this gives the index position of one of the poles.
!      The other is half way around. If not present, the default is 1.
!     [CURRENTLY NOT IMPLEMENTED]
! \item[{[bipolePos3]}] 
!     Two element array describing the index dimension 3 connections.
!      The first element represents the minimum end of dimension 3.
!      The second element represents the maximum end of dimension 3.
!      If a bipole, this gives the index position of one of the poles.
!      The other is half way around. If not present, the default is 1. 
!     [CURRENTLY NOT IMPLEMENTED]
! \item[{[coordDep1]}] 
!     This array specifies the dependence of the first 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the first
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is (/1,2,3/). 
! \item[{[coordDep2]}] 
!     This array specifies the dependence of the second 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the second
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is (/1,2,3/). 
! \item[{[coordDep3]}] 
!     This array specifies the dependence of the third 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the third
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is (/1,2,3/). 
! \item[{[indexflag]}]
!      Flag that indicates how the DE-local indices are to be defined.
! \item[{[petMap]}]
!       Sets the mapping of pets to the created DEs. This 3D
!       should be of size size(countsPerDEDim1) x size(countsPerDEDim2) x
!       size(countsPerDEDim3). If countsPerDEDim3 isn't present, then
!       the last dimension is of size 1.   
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
    type(ESMF_DistGrid)  :: distgrid
    type(ESMF_DELayout)  :: delayout
    integer, pointer     :: petList(:)
    integer, pointer     :: lbounds(:)
    integer, pointer     :: ubounds(:)
    integer, pointer     :: coordRank(:)
    integer, pointer     :: coordDimMap(:,:)
    integer              :: localrc
    integer              :: rank,i,distRank,undistRank,maxSizeDEDim
    integer, pointer     :: minIndexDG(:),maxIndexDG(:)
    integer, pointer     :: dimMap(:),minIndexLocal(:), deDimCount(:)
    integer, pointer     :: deBlockList(:,:,:),minPerDEDim(:,:),maxPerDEDim(:,:)
    integer              :: deCount
    integer              :: d,i1,i2,i3,k
    type(ESMF_GridConn)  :: connDim1Local(2)
    type(ESMF_GridConn)  :: connDim2Local(2)
    type(ESMF_GridConn)  :: connDim3Local(2)
    integer              :: connCount, petListCount 

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Compute the Grid Rank and Derivatives ---------------------------------------------------
    ! rank
    if (present(countsPerDEDim3)) then
	rank=3
    else
	rank=2
    endif

    ! rank of distributed part
    distRank=0 

    if (size(countsPerDEDim1) .gt. 1) then
       distRank=distRank+1
    endif

    if (size(countsPerDEDim2) .gt. 1) then
       distRank=distRank+1
    endif

    if (rank .gt. 2) then
       if (size(countsPerDEDim3) .gt. 1) then
           distRank=distRank+1
        endif
    endif

    ! ranks of the undistributed part of the grid
    undistRank=rank-distRank

    ! Argument Consistency Checking --------------------------------------------------------------
    if (size(countsPerDEDim1) .lt. 1) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- size 0 countsPerDEDim1 not allowed", & 
               ESMF_CONTEXT, rc) 
         return 
    endif

    if (size(countsPerDEDim2) .lt. 1) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- size 0 countsPerDEDim2 not allowed", & 
               ESMF_CONTEXT, rc) 
         return 
    endif

    if (present(countsPerDEDim3)) then
        if (size(countsPerDEDim3) .lt. 1) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
                    "- size 0 countsPerDEDim3 not allowed", & 
                    ESMF_CONTEXT, rc) 
            return 
        endif
    endif

    if ((rank .lt. 3) .and. present(connDim3)) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- connDim3 not allowed when grid is less than rank 3", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if ((rank .lt. 3) .and. present(poleStaggerLoc3)) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- poleStaggerLoc3 not allowed when grid is less than rank 3", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if ((rank .lt. 3) .and. present(bipolePos3)) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- bipolePos3 not allowed when grid is less than rank 3", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif


    if ((rank .lt. 3) .and. present(coordDep3)) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- coordDep3 not allowed when grid is less than rank 3", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if (present(coordDep1)) then
       if ((size(coordDep1) < 1) .or. (size(coordDep1)>rank)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- coordDep1 size incompatible with grid rank", & 
               ESMF_CONTEXT, rc) 
          return 
       endif
    endif

    if (present(coordDep2)) then
       if ((size(coordDep2) < 1) .or. (size(coordDep2)>rank)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- coordDep2 size incompatible with grid rank", & 
               ESMF_CONTEXT, rc) 
          return 
       endif
    endif

    if (present(coordDep3)) then
       if ((size(coordDep3) < 1) .or. (size(coordDep3)>rank)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- coordDep3 size incompatible with grid rank", & 
               ESMF_CONTEXT, rc) 
          return 
       endif
    endif

    if (present(minIndex)) then
       if (size(minIndex) .ne. rank) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- minIndex size must equal grid rank", & 
               ESMF_CONTEXT, rc) 
          return 
       endif
    endif


    if (present(petMap)) then
       if (rank .gt. 2) then
          if ((size(petMap,1) .ne. size(countsPerDEDim1)) .or. &
              (size(petMap,2) .ne. size(countsPerDEDim2)) .or. &
              (size(petMap,3) .ne. size(countsPerDEDim3))) then
              call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
                     "- petMap wrong size in one or more dimensions", & 
                     ESMF_CONTEXT, rc) 
              return 
          endif
       else
          if ((size(petMap,1) .ne. size(countsPerDEDim1)) .or. &
              (size(petMap,2) .ne. size(countsPerDEDim2)) .or. &
              (size(petMap,3) .ne. 1)) then
              call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
                     "- petMap wrong size in one or more dimensions", & 
                     ESMF_CONTEXT, rc) 
              return 
          endif
       endif
    endif


   ! Check for non-valid connection types here



   ! TODO: can you create an array without a distgrid??? What if everything they specify is undistributed?
   !       for now make a totally undistributed grid an error. Work on handling it later.
   !       Perhaps don't use lbounds, ubounds
    if (distRank .eq. 0) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- Need to have at least one distributed dimension", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif


   !TODO: Consider making some of these a separate local subroutine (particularly if you're going to 
   !      have 3 of these ShapeCreate subroutines with only minor changes


    ! Set Defaults ------------------------------------------------------------------

    ! Set default for minIndex 
    allocate(minIndexLocal(rank), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating minIndexLocal", &
                                     ESMF_CONTEXT, rc)) return

    if (present(minIndex)) then
       minIndexLocal(:)=minIndex(:)
    else
       do i=1,rank
          minIndexLocal(i)=1
       enddo
    endif

    ! Set Default for connections (although they don't work yet in distgrid/array, so they aren't really used anywhere yet.)
    if (present(connDim1)) then
       if (size(connDim1) .eq. 1) then
          connDim1Local(1)=connDim1(1)     
          connDim1Local(2)=connDim1(1)    ! if only 1 connection is specified then repeat for both ends  
       else if (size(connDim1) .ge. 2) then
          connDim1Local(1)=connDim1(1)
          connDim1Local(2)=connDim1(2)
       endif
    else
!       connDim1Local(1)=ESMF_GRIDCONN_NONE ! if not present then default to no connection
!       connDim1Local(2)=ESMF_GRIDCONN_NONE
    endif

    if (present(connDim2)) then
       if (size(connDim2) .eq. 1) then
          connDim2Local(1)=connDim2(1)     
          connDim2Local(2)=connDim2(1)    ! if only 1 connection is specified then repeat for both ends  
       else if (size(connDim2) .ge. 2) then
          connDim2Local(1)=connDim2(1)
          connDim2Local(2)=connDim2(2)
       endif
    else
!       connDim2Local(1)=ESMF_GRIDCONN_NONE ! if not present then default to no connection
!       connDim2Local(2)=ESMF_GRIDCONN_NONE
    endif

    if (present(connDim3)) then
       if (size(connDim3) .eq. 1) then
          connDim3Local(1)=connDim3(1)     
          connDim3Local(2)=connDim3(1)    ! if only 1 connection is specified then repeat for both ends  
       else if (size(connDim3) .ge. 2) then
          connDim3Local(1)=connDim3(1)
          connDim3Local(2)=connDim3(2)
       endif
    else
!       connDim3Local(1)=ESMF_GRIDCONN_NONE ! if not present then default to no connection
!       connDim3Local(2)=ESMF_GRIDCONN_NONE
    endif


   ! Calc minIndex,maxIndex,dimMap for DistGrid -----------------------------------
   allocate(minIndexDG(distRank), stat=localrc)
   if (ESMF_LogMsgFoundAllocError(localrc, "Allocating minIndexDG", &
               ESMF_CONTEXT, rc)) return
   allocate(maxIndexDG(distRank), stat=localrc)
   if (ESMF_LogMsgFoundAllocError(localrc, "Allocating minIndexDG", &
               ESMF_CONTEXT, rc)) return
   allocate(dimMap(distRank), stat=localrc)
   if (ESMF_LogMsgFoundAllocError(localrc, "Allocating dimMap", &
               ESMF_CONTEXT, rc)) return
          

   ! Fill in minIndex, maxIndex, dimMap
   d=1
   if (size(countsPerDEDim1) .gt. 1) then
      minIndexDG(d)=minIndexLocal(1)
      maxIndexDG(d)=sum(countsPerDeDim1)+minIndexDG(d)-1
      dimMap(d)=1      
      d=d+1
   endif

   if (size(countsPerDEDim2) .gt. 1) then
      minIndexDG(d)=minIndexLocal(2)
      maxIndexDG(d)=sum(countsPerDeDim2)+minIndexDG(d)-1
      dimMap(d)=2      
      d=d+1
   endif

   if (rank .gt. 2) then
      if (size(countsPerDEDim3) .gt. 1) then
         minIndexDG(d)=minIndexLocal(2)
         maxIndexDG(d)=sum(countsPerDeDim2)+minIndexDG(d)-1
         dimMap(d)=3      
         d=d+1
      endif
   endif


  ! Setup deBlockList for DistGrid ------------------------------------------------
  ! count de blocks
  deCount=1
  deCount=deCount*size(countsPerDEDim1) 
  deCount=deCount*size(countsPerDEDim2)
  if (rank .gt. 2) then
     deCount=deCount*size(countsPerDEDim3)
  endif 
 
  ! Calc the max size of a DEDim
  maxSizeDEDim=1
  if (size(countsPerDEDim1) .gt. maxSizeDEDim) then
      maxSizeDEDim=size(countsPerDEDim1)
  endif
  if (size(countsPerDEDim2) .gt. maxSizeDEDim) then
      maxSizeDEDim=size(countsPerDEDim2)
  endif
  if (rank .gt. 2) then
      if (size(countsPerDEDim3) .gt. maxSizeDEDim) then
         maxSizeDEDim=size(countsPerDEDim3)
      endif
  endif
  

  ! generate deblocklist
  allocate(maxPerDEDim(distRank,maxSizeDEDim), stat=localrc)
  if (ESMF_LogMsgFoundAllocError(localrc, "Allocating maxPerDEDim", &
              ESMF_CONTEXT, rc)) return
  allocate(minPerDEDim(distRank,maxSizeDEDim), stat=localrc)
  if (ESMF_LogMsgFoundAllocError(localrc, "Allocating minPerDEDim", &
              ESMF_CONTEXT, rc)) return
 allocate(deDimCount(distRank), stat=localrc)
  if (ESMF_LogMsgFoundAllocError(localrc, "Allocating maxPerDEDim", &
              ESMF_CONTEXT, rc)) return


  ! Calc the maximum end of each DE in a Dim, and the size of each DEDim
  d=1
  if (size(countsPerDEDim1) .gt. 1) then
      deDimCount(d)=size(countsPerDEDim1)
      minPerDeDim(d,1)=minIndexLocal(1)
      maxPerDeDim(d,1)=minIndexLocal(1)+countsPerDEDim1(1)-1
      do i=2,deDimCount(d) 
         minPerDEDim(d,i)=maxPerDEDim(d,i-1)+1
         maxPerDEDim(d,i)=minPerDEDim(d,i)+countsPerDEDim1(i)-1
      enddo
      d=d+1  ! advance to next distgrid dimension
  endif

  if (size(countsPerDEDim2) .gt. 1) then
      deDimCount(d)=size(countsPerDEDim2)
      minPerDeDim(d,1)=minIndexLocal(2)
      maxPerDeDim(d,1)=minIndexLocal(2)+countsPerDEDim2(1)-1
      do i=2,deDimCount(d) 
         minPerDEDim(d,i)=maxPerDEDim(d,i-1)+1
         maxPerDEDim(d,i)=minPerDEDim(d,i)+countsPerDEDim2(i)-1
      enddo
      d=d+1  ! advance to next distgrid dimension
  endif

  if (rank .gt. 2) then
  if (size(countsPerDEDim3) .gt. 1) then
      deDimCount(d)=size(countsPerDEDim3)
      minPerDeDim(d,1)=minIndexLocal(3)
      maxPerDeDim(d,1)=minIndexLocal(3)+countsPerDEDim3(1)-1
      do i=2,deDimCount(d) 
         minPerDEDim(d,i)=maxPerDEDim(d,i-1)+1
         maxPerDEDim(d,i)=minPerDEDim(d,i)+countsPerDEDim3(i)-1
      enddo
      d=d+1  ! advance to next distgrid dimension
  endif
  endif


  ! allocate deblocklist
  allocate(deBlockList(distRank,2,deCount), stat=localrc)
  if (ESMF_LogMsgFoundAllocError(localrc, "Allocating deBlockList", &
              ESMF_CONTEXT, rc)) return

  ! Fill in DeBlockList
  if (distRank .eq. 1) then
     k=1
     do i1=1,deDimCount(1)
        deBlockList(1,1,k)=minPerDEDim(1,i1)
        deBlockList(1,2,k)=maxPerDEDim(1,i1)
        k=k+1
     enddo
  else if (distRank .eq. 2) then
     k=1
     do i2=1,deDimCount(2)
     do i1=1,deDimCount(1)
        deBlockList(1,1,k)=minPerDEDim(1,i1)
        deBlockList(1,2,k)=maxPerDEDim(1,i1)
        deBlockList(2,1,k)=minPerDEDim(2,i2)
        deBlockList(2,2,k)=maxPerDEDim(2,i2)
        k=k+1
     enddo
     enddo
  else if (distRank .eq. 3) then
     k=1
     do i3=1,deDimCount(3)
     do i2=1,deDimCount(2)
     do i1=1,deDimCount(1)
        deBlockList(1,1,k)=minPerDEDim(1,i1)
        deBlockList(1,2,k)=maxPerDEDim(1,i1)
        deBlockList(2,1,k)=minPerDEDim(2,i2)
        deBlockList(2,2,k)=maxPerDEDim(2,i2)
        deBlockList(3,1,k)=minPerDEDim(3,i3)
        deBlockList(3,2,k)=maxPerDEDim(3,i3)
        k=k+1
     enddo
     enddo
     enddo
  endif
  
!  do i=1,deCount
!     write(*,*) i,"min=",deBlockList(:,1,i)," max=",deBlockList(:,2,i)
!  enddo


   
   ! Setup Connections between patch sides ----------------------------------------

   ! CONNECTIONS DON'T WORK YET SO NOT IMPLEMENTED


   ! Process PetMap --------------------------------------------------------------
   if (present(petMap)) then

      !! Allocate petList
      allocate(petList(deCount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating petList", &
              ESMF_CONTEXT, rc)) return


      !! copy petMap to petList
      if (rank .gt. 2) then
	 k=1
     	 do i3=1,size(countsPerDEDim3)
         do i2=1,size(countsPerDEDim2)
         do i1=1,size(countsPerDEDim1)
            petList(k)=petMap(i1,i2,i3)
            k=k+1
         enddo
         enddo
         enddo
      else 
	 k=1
     	 do i3=1,1
         do i2=1,size(countsPerDEDim2)
         do i1=1,size(countsPerDEDim1)
            petList(k)=petMap(i1,i2,i3)
            k=k+1
         enddo
         enddo
         enddo
      endif

      !! create delayout from the petList
      delayout=ESMF_DELayoutCreate(petList=petList,rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

      !! Get rid of list
      deallocate(petList)
   else
      !! create a default delayout
      delayout=ESMF_DELayoutCreate(deCount=deCount,rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
   endif


   

   ! Create DistGrid --------------------------------------------------------------

    distgrid=ESMF_DistGridCreate(minIndex=minIndexDG, maxIndex=maxIndexDG, &
               deBlockList=deBlockList, delayout=delayout, indexflag=indexflag, rc=localrc)   
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


   ! Calc lbounds, ubounds for Grid -----------------------------------------------
   if (undistRank .gt. 0) then
      allocate(lbounds(undistRank), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating lbounds", &
              ESMF_CONTEXT, rc)) return
      allocate(ubounds(undistRank), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating ubounds", &
              ESMF_CONTEXT, rc)) return     

      ! Fill in minIndex, maxIndex, dimMap
       d=1
      if (size(countsPerDEDim1) .eq. 1) then
         lbounds(d)=minIndexLocal(1)
         ubounds(d)=countsPerDEDim1(1)+lbounds(d)-1
         d=d+1
      endif

      if (size(countsPerDEDim2) .eq. 1) then
         lbounds(d)=minIndexLocal(2)
         ubounds(d)=countsPerDEDim2(1)+lbounds(d)-1
         d=d+1
      endif

      if (rank .gt. 2) then
         if (size(countsPerDEDim3) .eq. 1) then
            lbounds(d)=minIndexLocal(3)
            ubounds(d)=countsPerDEDim3(1)+lbounds(d)-1
            d=d+1
         endif
      endif
   endif


   ! Convert coordDeps to coordRank and coordDimMap -------------------------------
   allocate(coordRank(rank), stat=localrc)
   if (ESMF_LogMsgFoundAllocError(localrc, "Allocating coordRank", &
              ESMF_CONTEXT, rc)) return
   allocate(coordDimMap(rank,rank), stat=localrc)
   if (ESMF_LogMsgFoundAllocError(localrc, "Allocating coordDimMap", &
              ESMF_CONTEXT, rc)) return

   if (present(coordDep1)) then
      coordRank(1)=size(coordDep1)
      coordDimMap(1,:)=0
      do i=1,size(coordDep1)
         coordDimMap(1,i)=coordDep1(i)
      enddo
   else 
      coordRank(1)=rank
      do i=1,rank
         coordDimMap(1,i)=i      
      enddo
   endif

   if (present(coordDep2)) then
      coordRank(2)=size(coordDep2)
      coordDimMap(2,:)=0
      do i=1,size(coordDep2)
         coordDimMap(2,i)=coordDep2(i)
      enddo
   else 
      coordRank(2)=rank
      do i=1,rank
         coordDimMap(2,i)=i      
      enddo
   endif

   if (rank .gt. 2) then
      if (present(coordDep3)) then 
         coordRank(3)=size(coordDep3)
          coordDimMap(3,:)=0
          do i=1,size(coordDep3)
             coordDimMap(3,i)=coordDep3(i)
          enddo
      else 
        coordRank(3)=rank
        do i=1,rank
	   coordDimMap(3,i)=i      
        enddo
      endif
   endif

  
   ! Create Grid from specification -----------------------------------------------
   if (undistRank .gt. 0) then
       ESMF_GridCreateShapeIrreg=ESMF_GridCreateFromDistGrid(name, coordTypeKind, &
                                    distgrid, dimmap, lbounds, ubounds, &
                                    coordRank, coordDimMap, indexflag, &
                                    gridType, localrc)
    else
       ESMF_GridCreateShapeIrreg=ESMF_GridCreateFromDistGrid(name, coordTypeKind, &
                                    distgrid=distgrid, dimmap=dimmap, &
                                    coordRank=coordRank, coordDimMap=coordDimMap, &
                                    indexflag=indexflag, &
                                    gridtype=gridType, rc=localrc)
    endif
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return



    ! Clean up memory
    deallocate(coordRank)
    deallocate(coordDimMap)
    deallocate(minIndexDG)
    deallocate(maxIndexDG)
    deallocate(dimMap)
    deallocate(maxPerDEDim)
    deallocate(minPerDEDim)
    deallocate(deDimCount)
    deallocate(deBlockList)
    if (undistRank .gt. 0) then
       deallocate(lbounds)
       deallocate(ubounds)
    endif

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    end function ESMF_GridCreateShapeIrreg



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
!     Destroys an {\tt ESMF\_Grid} object and related internal structures. This call
!    does not destroy the internal coordinate Arrays, or the internally generated DistGrid. 
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
    ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid, rc)

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

 end subroutine ESMF_GridDestroy


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGet"
!BOP
! !IROUTINE: ESMF_GridGet - Get information about a Grid

! !INTERFACE:
      subroutine ESMF_GridGet(grid, name, coordTypeKind, rank,  &
          tileCount, distGrid, staggerLocsCount,  &
          dimmap, lbounds, ubounds, coordRank, coordDimMap, &
          indexFlag, gridType, localDECount,distRank,undistRank, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid),       intent(in)            :: grid
      character (len=*),     intent(out), optional :: name
      type(ESMF_TypeKind),   intent(out), optional :: coordTypeKind
      integer,               intent(out), optional :: rank
      integer,               intent(out), optional :: tileCount
      type(ESMF_DistGrid),   intent(out), optional :: distGrid
      integer,               intent(out), optional :: lbounds(:)
      integer,               intent(out), optional :: ubounds(:)
      integer,               intent(out), optional :: dimmap(:)
      integer,               intent(out), optional :: coordRank(:)
      integer,               intent(out), optional :: coordDimMap(:,:)
      integer,               intent(out), optional :: staggerLocsCount
      integer,               intent(out), optional :: localDECount
      integer,               intent(out), optional :: distRank
      integer,               intent(out), optional :: undistRank
      type(ESMF_IndexFlag),  intent(out), optional :: indexflag
      integer,               intent(out), optional :: gridType  ! NOT IMPLEMENTED
      integer,               intent(out), optional :: rc
!
! !DESCRIPTION:
!    Gets various types of information about a grid. 
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
!\item[{[staggerLocsCount]}]
!   The number of stagger locations.
! \item[{[coordRank]}]
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
!\item[{[localDECount]}]
!   The number of DE's in this grid on this processor
!\item[{[distRank]}]
!   The rank of the distributed part of the grid. Should be equal to the distgrid's
!   dimCount. 
!\item[{[undistRank]}]
!   The rank of the undistributed part of the grid.
!\item[{[gridType]}]
!   Flag that indicates the type of the grid. If not given, defaults
!    to ESMF\_GRIDTYPE\_UNKNOWN. [CURRENTLY NOT IMPLEMENTED]
!\item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!\end{description}
!
!EOP
    integer :: localrc ! local error status
    type(ESMF_InterfaceInt) :: dimmapArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: lboundsArg ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: uboundsArg ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: coordRankArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: coordDimMapArg ! Language Interface Helper Var


    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
     ESMF_INIT_CHECK_DEEP_SHORT(ESMF_GridGetInit, grid, rc)

    ! name 
    if (present(name)) then
      call c_ESMC_GetName(grid, name, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
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
    coordRankArg = ESMF_InterfaceIntCreate(coordRank, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    coordDimMapArg = ESMF_InterfaceIntCreate(farray2D=coordDimMap, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call C++ Subroutine to do the get
    call c_ESMC_gridget(grid%this, &
      coordTypeKind, rank, tileCount, distgrid,  staggerLocsCount, &
      dimmapArg, lboundsArg, uboundsArg, coordRankArg, coordDimMapArg, &
      indexflag, gridtype, localDECount, distRank, undistRank, localrc)
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
    call ESMF_InterfaceIntDestroy(coordRankArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(coordDimMapArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set Deep Classes as created
    if (present(distgrid)) then
       call ESMF_DistGridSetInitCreated(distgrid, rc=localrc)
       if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
   	   ESMF_CONTEXT, rcToReturn=rc)) return
    endif


    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

end subroutine ESMF_GridGet


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetCoordIntoArray"
!BOP
! !IROUTINE: ESMF_GridGetCoord - Get coordinates and put in an ESMF Array

! !INTERFACE:
  ! Private name; call using ESMF_GridGetCoord()
      subroutine ESMF_GridGetCoordIntoArray(grid, staggerLoc,coord, array, &
                            docopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type (ESMF_StaggerLoc), intent(in),optional  :: staggerLoc
      integer, intent(in)  :: coord
      type(ESMF_Array), intent(out) :: array
      type(ESMF_CopyFlag), intent(in), optional :: docopy ! NOT IMPLEMENTED
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    This method allows the user to get access to the ESMF Array holding
!    coordinate data at a particular stagger location. This is useful, for example, 
!    to set the coordinate values. To have an Array to access, the coordinate Arrays
!    must have already been allocated, for example by {\tt ESMF\_GridAllocCoord} or
!    {\tt ESMF\_GridSetCoord}.
!
!     The arguments are:
!     \begin{description}
!     \item[{staggerLoc}]
!          The stagger location from which to get the arrays. 
!          Please see Section~\ref{sec:opt:staggerloc} for a list 
!          of predefined stagger locations. If not present, defaults to ESMF\_STAGGERLOC\_CENTER.
!     \item[{coord}]
!          The coordinate component to get the data from (e.g. 1=x).
!     \item[{array}]
!          An array into which to put the coordinate infomation. 
!     \item[{[doCopy]}]
!          Default to {\tt ESMF\_DATA\_COPY},  copys the coordinates
!          into the arrays. If set to {\tt ESMF\_DATA\_REF},
!          causes the array to reference the grid array containing the coordinates.
!          [THE COPY OPTION IS CURRENTLY NOT IMPLEMENTED] 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

    integer :: tmp_staggerloc
    integer :: localrc ! local error status

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_GridGetInit, grid, rc)

    ! handle staggerloc
    if (present(staggerLoc)) then
       tmp_staggerloc=staggerLoc%staggerloc
    else
       tmp_staggerloc=ESMF_STAGGERLOC_CENTER%staggerloc
    endif

    ! Call C++ Subroutine to do the create
    call c_ESMC_gridgetcoordintoarray(grid%this,tmp_staggerloc, coord, &
      array, docopy, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


    ! Set Array as created
    call ESMF_ArraySetInitCreated(array,localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetCoordIntoArray

! -------------------------- ESMF-private method ------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetInit"
!BOPI
! !IROUTINE: ESMF_GridGetInit - Internal access routine for init code
!
! !INTERFACE:
      function ESMF_GridGetInit(grid)
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_GridGetInit
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in), optional :: grid
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
      ESMF_GridGetInit = ESMF_INIT_GET(grid)
    else
      ESMF_GridGetInit = ESMF_INIT_CREATED
    endif

    end function ESMF_GridGetInit
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetLocalTileCoord"
!BOP
! !IROUTINE: ESMF_GridGetLocalTileCoord - Get pointer to coordinates of a local tile

! !INTERFACE:
  ! Private name; call using ESMF_GridGetLocalTileCoord()
      subroutine ESMF_GridGetLocalTileCoord1DR4(grid, localDE, &
                            staggerLoc, coord, fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in),optional :: localDE
      type (ESMF_StaggerLoc), intent(in),optional :: staggerLoc
      integer, intent(in) :: coord
      real(ESMF_Kind_R4), pointer :: fptr(:)
      type(ESMF_CopyFlag), intent(in), optional :: docopy
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    This method gets a fortran pointer to the piece of memory which holds the 
!    coordinate data for the given coordinate and stagger locations on the given local DE. 
!    This is useful, for example, for setting the coordinate values in a Grid, or
!    for reading the coordinate values. Eventually this method will be overloaded
!    for the full range of ESMF supported types and dimensions. 
!
!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          Grid to get the information from.
!     \item[{[localDE]}]
!          The local DE to get the information for. If not set, defaults to 
!          the first DE on this processor. (localDE starts at 0)
!     \item[{staggerLoc}]
!          The stagger location to get the information for. 
!          Please see Section~\ref{sec:opt:staggerloc} for a list 
!          of predefined stagger locations. If not present, defaults to ESMF\_STAGGERLOC\_CENTER.
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


 ! Local variables 
 type(ESMF_Array) :: array 
 integer :: status ! local error status 
 integer :: localDeCount, rank 
 type(ESMF_TypeKind) :: typekind 
 type(ESMF_LocalArray), allocatable :: larrayList(:) 
 type(ESMF_CopyFlag) :: docopyInt
 integer :: lDE
 integer :: coordRank(ESMF_MAXDIM)

 ! Initialize return code 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid, rc) 

 ! Check consistency 
 call ESMF_GridGet(grid, coordTypeKind=typekind, rank=rank, coordRank=coordRank, &
                   localDECount=localDECount, rc=status) 
 if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return
 
 ! Require farrayPtr typekind to match Grid typekind 
 if (typekind .ne. ESMF_TYPEKIND_R4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- farrayPtr typekind does not match Grid typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

! make sure coord is legitiamte
if ((coord .lt. 1) .or. (coord .gt. rank)) then
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- coord outside of range allowed for this Grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

 ! Require farrayPtr rank to match coordinate rank 
 if (coordRank(coord) .ne. 1) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- farrayPtr rank does not match requested coordinate rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

! Set Defaults
if (present(docopy)) then
   docopyInt=docopy
else
  docopyInt=ESMF_DATA_REF
endif

if (present(localDE)) then
   lDE=localDE
else
  lDE=0
endif


 ! Require DELayout to be 1 DE per PET 
 if (localDeCount < 0) then 
 call ESMF_LogMsgSetError(ESMF_RC_CANNOT_GET, & 
 "- Negative number of localDeCount prohibits request", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

 if (localDeCount == 0) then 
 call ESMF_LogMsgSetError(ESMF_RC_CANNOT_GET, & 
 "- localDeCount == 0 prohibits request", & 
 ESMF_CONTEXT, rc) 
 return 
 endif
 
 if (lDE>=localDeCount) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- localDE too big", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

 if (lDE<0) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- localDE can't be less than 0", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 


 ! Get the Array 
 call ESMF_GridGetCoordIntoArray(grid, staggerLoc,coord, array, &
                                ESMF_DATA_REF, rc=status)
 if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return

 ! Obtain the native F90 array pointer via the LocalArray interface 
 allocate(larrayList(localDeCount))
 
 call ESMF_ArrayGet(array, larrayList=larrayList, rc=status) 
 if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return
 
 call ESMF_LocalArrayGetData(larrayList(lDE+1), fptr, doCopy, rc=status) 
 if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(larrayList) 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

      end subroutine ESMF_GridGetLocalTileCoord1DR4


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetLocalTileCoord"
!BOP
! !IROUTINE: ESMF_GridGetLocalTileCoord - Get pointer to coordinates of a local tile

! !INTERFACE:
  ! Private name; call using ESMF_GridGetLocalTileCoord()
      subroutine ESMF_GridGetLocalTileCoord2DR4(grid, localDE, &
                            staggerLoc, coord, fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in),optional :: localDE
      type (ESMF_StaggerLoc), intent(in),optional :: staggerLoc
      integer, intent(in) :: coord
      real(ESMF_Kind_R4), pointer :: fptr(:,:)
      type(ESMF_CopyFlag), intent(in), optional :: docopy
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    This method gets a fortran pointer to the piece of memory which holds the 
!    coordinate data for the given coordinate and stagger locations on the given local DE. 
!    This is useful, for example, for setting the coordinate values in a Grid, or
!    for reading the coordinate values. Eventually this method will be overloaded
!    for the full range of ESMF supported types and dimensions. 
!
!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          Grid to get the information from.
!     \item[{[localDE]}]
!          The local DE to get the information for. If not set, defaults to 
!          the first DE on this processor. (localDE starts at 0)
!     \item[{staggerLoc}]
!          The stagger location to get the information for. 
!          Please see Section~\ref{sec:opt:staggerloc} for a list 
!          of predefined stagger locations. If not present, defaults to
!          ESMF\_STAGGERLOC\_CENTER.
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


 ! Local variables 
 type(ESMF_Array) :: array 
 integer :: status ! local error status 
 integer :: localDeCount, rank 
 type(ESMF_TypeKind) :: typekind 
 type(ESMF_LocalArray), allocatable :: larrayList(:) 
 type(ESMF_CopyFlag) :: docopyInt
 integer :: lDE
 integer :: coordRank(ESMF_MAXDIM)

 ! Initialize return code 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid, rc) 

 ! Check consistency 
 call ESMF_GridGet(grid, coordTypeKind=typekind, rank=rank, coordRank=coordRank, &
                   localDECount=localDECount, rc=status) 
 if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return
 
 ! Require farrayPtr typekind to match Grid typekind 
 if (typekind .ne. ESMF_TYPEKIND_R4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- farrayPtr typekind does not match Grid typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

! make sure coord is legitiamte
if ((coord .lt. 1) .or. (coord .gt. rank)) then
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- coord outside of range allowed for this Grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

 ! Require farrayPtr rank to match coordinate rank 
 if (coordRank(coord) .ne. 2) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- farrayPtr rank does not match requested coordinate rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

! Set Defaults
if (present(docopy)) then
   docopyInt=docopy
else
  docopyInt=ESMF_DATA_REF
endif

if (present(localDE)) then
   lDE=localDE
else
  lDE=0
endif


 ! Require DELayout to be 1 DE per PET 
 if (localDeCount < 0) then 
 call ESMF_LogMsgSetError(ESMF_RC_CANNOT_GET, & 
 "- Negative number of localDeCount prohibits request", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

 if (localDeCount == 0) then 
 call ESMF_LogMsgSetError(ESMF_RC_CANNOT_GET, & 
 "- localDeCount == 0 prohibits request", & 
 ESMF_CONTEXT, rc) 
 return 
 endif
 
 if (lDE>=localDeCount) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- localDE too big", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

 if (lDE<0) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- localDE can't be less than 0", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 


 ! Get the Array 
 call ESMF_GridGetCoordIntoArray(grid, staggerLoc,coord, array, &
                                ESMF_DATA_REF, rc=status)
 if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return

 ! Obtain the native F90 array pointer via the LocalArray interface 
 allocate(larrayList(localDeCount))
 
 call ESMF_ArrayGet(array, larrayList=larrayList, rc=status) 
 if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return
 
 call ESMF_LocalArrayGetData(larrayList(lDE+1), fptr, doCopy, rc=status) 
 if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(larrayList) 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

      end subroutine ESMF_GridGetLocalTileCoord2DR4


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetLocalTileCoord"
!BOP
! !IROUTINE: ESMF_GridGetLocalTileCoord - Get pointer to coordinates of a local tile

! !INTERFACE:
  ! Private name; call using ESMF_GridGetLocalTileCoord()
      subroutine ESMF_GridGetLocalTileCoord3DR4(grid, localDE, &
                            staggerLoc, coord, fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in),optional :: localDE
      type (ESMF_StaggerLoc), intent(in),optional :: staggerLoc
      integer, intent(in) :: coord
      real(ESMF_Kind_R4), pointer :: fptr(:,:,:)
      type(ESMF_CopyFlag), intent(in), optional :: docopy
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    This method gets a fortran pointer to the piece of memory which holds the 
!    coordinate data for the given coordinate and stagger locations on the given local DE. 
!    This is useful, for example, for setting the coordinate values in a Grid, or
!    for reading the coordinate values. Eventually this method will be overloaded
!    for the full range of ESMF supported types and dimensions. 
!
!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          Grid to get the information from.
!     \item[{[localDE]}]
!          The local DE to get the information for. If not set, defaults to 
!          the first DE on this processor. (localDE starts at 0)
!     \item[{staggerLoc}]
!          The stagger location to get the information for. 
!          Please see Section~\ref{sec:opt:staggerloc} for a list 
!          of predefined stagger locations. If not present, defaults to
!          ESMF\_STAGGERLOC\_CENTER.
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


 ! Local variables 
 type(ESMF_Array) :: array 
 integer :: status ! local error status 
 integer :: localDeCount, rank 
 type(ESMF_TypeKind) :: typekind 
 type(ESMF_LocalArray), allocatable :: larrayList(:) 
 type(ESMF_CopyFlag) :: docopyInt
 integer :: lDE
 integer :: coordRank(ESMF_MAXDIM)

 ! Initialize return code 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid, rc) 

 ! Check consistency 
 call ESMF_GridGet(grid, coordTypeKind=typekind, rank=rank, coordRank=coordRank, &
                   localDECount=localDECount, rc=status) 
 if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return
 
 ! Require farrayPtr typekind to match Grid typekind 
 if (typekind .ne. ESMF_TYPEKIND_R4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- farrayPtr typekind does not match Grid typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

! make sure coord is legitiamte
if ((coord .lt. 1) .or. (coord .gt. rank)) then
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- coord outside of range allowed for this Grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

 ! Require farrayPtr rank to match coordinate rank 
 if (coordRank(coord) .ne. 3) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- farrayPtr rank does not match requested coordinate rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

! Set Defaults
if (present(docopy)) then
   docopyInt=docopy
else
  docopyInt=ESMF_DATA_REF
endif

if (present(localDE)) then
   lDE=localDE
else
  lDE=0
endif


 ! Require DELayout to be 1 DE per PET 
 if (localDeCount < 0) then 
 call ESMF_LogMsgSetError(ESMF_RC_CANNOT_GET, & 
 "- Negative number of localDeCount prohibits request", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

 if (localDeCount == 0) then 
 call ESMF_LogMsgSetError(ESMF_RC_CANNOT_GET, & 
 "- localDeCount == 0 prohibits request", & 
 ESMF_CONTEXT, rc) 
 return 
 endif
 
 if (lDE>=localDeCount) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- localDE too big", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

 if (lDE<0) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- localDE can't be less than 0", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 


 ! Get the Array 
 call ESMF_GridGetCoordIntoArray(grid, staggerLoc,coord, array, &
                                ESMF_DATA_REF, rc=status)
 if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return

 ! Obtain the native F90 array pointer via the LocalArray interface 
 allocate(larrayList(localDeCount))
 
 call ESMF_ArrayGet(array, larrayList=larrayList, rc=status) 
 if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return
 
 call ESMF_LocalArrayGetData(larrayList(lDE+1), fptr, doCopy, rc=status) 
 if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(larrayList) 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

      end subroutine ESMF_GridGetLocalTileCoord3DR4


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetLocalTileCoord"
!BOP
! !IROUTINE: ESMF_GridGetLocalTileCoord - Get pointer to coordinates of a local tile

! !INTERFACE:
  ! Private name; call using ESMF_GridGetLocalTileCoord()
      subroutine ESMF_GridGetLocalTileCoord1DR8(grid, localDE, &
                            staggerLoc, coord, fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in),optional :: localDE
      type (ESMF_StaggerLoc), intent(in),optional :: staggerLoc
      integer, intent(in) :: coord
      real(ESMF_Kind_R8), pointer :: fptr(:)
      type(ESMF_CopyFlag), intent(in), optional :: docopy
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    This method gets a fortran pointer to the piece of memory which holds the 
!    coordinate data for the given coordinate and stagger locations on the given local DE. 
!    This is useful, for example, for setting the coordinate values in a Grid, or
!    for reading the coordinate values. Eventually this method will be overloaded
!    for the full range of ESMF supported types and dimensions. 
!
!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          Grid to get the information from.
!     \item[{[localDE]}]
!          The local DE to get the information for. If not set, defaults to 
!          the first DE on this processor. (localDE starts at 0)
!     \item[{staggerLoc}]
!          The stagger location to get the information for. 
!          Please see Section~\ref{sec:opt:staggerloc} for a list 
!          of predefined stagger locations. If not present, defaults to
!          ESMF\_STAGGERLOC\_CENTER.
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


 ! Local variables 
 type(ESMF_Array) :: array 
 integer :: status ! local error status 
 integer :: localDeCount, rank 
 type(ESMF_TypeKind) :: typekind 
 type(ESMF_LocalArray), allocatable :: larrayList(:) 
 type(ESMF_CopyFlag) :: docopyInt
 integer :: lDE
 integer :: coordRank(ESMF_MAXDIM)

 ! Initialize return code 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid, rc) 

 ! Check consistency 
 call ESMF_GridGet(grid, coordTypeKind=typekind, rank=rank, coordRank=coordRank, &
                   localDECount=localDECount, rc=status) 
 if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return
 
 ! Require farrayPtr typekind to match Grid typekind 
 if (typekind .ne. ESMF_TYPEKIND_R8) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- farrayPtr typekind does not match Grid typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

! make sure coord is legitiamte
if ((coord .lt. 1) .or. (coord .gt. rank)) then
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- coord outside of range allowed for this Grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

 ! Require farrayPtr rank to match coordinate rank 
 if (coordRank(coord) .ne. 1) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- farrayPtr rank does not match requested coordinate rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

! Set Defaults
if (present(docopy)) then
   docopyInt=docopy
else
  docopyInt=ESMF_DATA_REF
endif

if (present(localDE)) then
   lDE=localDE
else
  lDE=0
endif


 ! Require DELayout to be 1 DE per PET 
 if (localDeCount < 0) then 
 call ESMF_LogMsgSetError(ESMF_RC_CANNOT_GET, & 
 "- Negative number of localDeCount prohibits request", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

 if (localDeCount == 0) then 
 call ESMF_LogMsgSetError(ESMF_RC_CANNOT_GET, & 
 "- localDeCount == 0 prohibits request", & 
 ESMF_CONTEXT, rc) 
 return 
 endif
 
 if (lDE>=localDeCount) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- localDE too big", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

 if (lDE<0) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- localDE can't be less than 0", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 


 ! Get the Array 
 call ESMF_GridGetCoordIntoArray(grid, staggerLoc,coord, array, &
                                ESMF_DATA_REF, rc=status)
 if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return

 ! Obtain the native F90 array pointer via the LocalArray interface 
 allocate(larrayList(localDeCount))
 
 call ESMF_ArrayGet(array, larrayList=larrayList, rc=status) 
 if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return
 
 call ESMF_LocalArrayGetData(larrayList(lDE+1), fptr, doCopy, rc=status) 
 if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(larrayList) 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

      end subroutine ESMF_GridGetLocalTileCoord1DR8


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetLocalTileCoord"
!BOP
! !IROUTINE: ESMF_GridGetLocalTileCoord - Get pointer to coordinates of a local tile

! !INTERFACE:
  ! Private name; call using ESMF_GridGetLocalTileCoord()
      subroutine ESMF_GridGetLocalTileCoord2DR8(grid, localDE, &
                            staggerLoc, coord, fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in),optional :: localDE
      type (ESMF_StaggerLoc), intent(in),optional :: staggerLoc
      integer, intent(in) :: coord
      real(ESMF_Kind_R8), pointer :: fptr(:,:)
      type(ESMF_CopyFlag), intent(in), optional :: docopy
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    This method gets a fortran pointer to the piece of memory which holds the 
!    coordinate data for the given coordinate and stagger locations on the given local DE. 
!    This is useful, for example, for setting the coordinate values in a Grid, or
!    for reading the coordinate values. Eventually this method will be overloaded
!    for the full range of ESMF supported types and dimensions. 
!
!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          Grid to get the information from.
!     \item[{[localDE]}]
!          The local DE to get the information for. If not set, defaults to 
!          the first DE on this processor. (localDE starts at 0)
!     \item[{staggerLoc}]
!          The stagger location to get the information for. 
!          Please see Section~\ref{sec:opt:staggerloc} for a list 
!          of predefined stagger locations. If not present, defaults to
!          ESMF\_STAGGERLOC\_CENTER.
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


 ! Local variables 
 type(ESMF_Array) :: array 
 integer :: status ! local error status 
 integer :: localDeCount, rank 
 type(ESMF_TypeKind) :: typekind 
 type(ESMF_LocalArray), allocatable :: larrayList(:) 
 type(ESMF_CopyFlag) :: docopyInt
 integer :: lDE
 integer :: coordRank(ESMF_MAXDIM)

 ! Initialize return code 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid, rc) 

 ! Check consistency 
 call ESMF_GridGet(grid, coordTypeKind=typekind, rank=rank, coordRank=coordRank, &
                   localDECount=localDECount, rc=status) 
 if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return
 
 ! Require farrayPtr typekind to match Grid typekind 
 if (typekind .ne. ESMF_TYPEKIND_R8) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- farrayPtr typekind does not match Grid typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

! make sure coord is legitiamte
if ((coord .lt. 1) .or. (coord .gt. rank)) then
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- coord outside of range allowed for this Grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

 ! Require farrayPtr rank to match coordinate rank 
 if (coordRank(coord) .ne. 2) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- farrayPtr rank does not match requested coordinate rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

! Set Defaults
if (present(docopy)) then
   docopyInt=docopy
else
  docopyInt=ESMF_DATA_REF
endif

if (present(localDE)) then
   lDE=localDE
else
  lDE=0
endif


 ! Require DELayout to be 1 DE per PET 
 if (localDeCount < 0) then 
 call ESMF_LogMsgSetError(ESMF_RC_CANNOT_GET, & 
 "- Negative number of localDeCount prohibits request", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

 if (localDeCount == 0) then 
 call ESMF_LogMsgSetError(ESMF_RC_CANNOT_GET, & 
 "- localDeCount == 0 prohibits request", & 
 ESMF_CONTEXT, rc) 
 return 
 endif
 
 if (lDE>=localDeCount) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- localDE too big", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

 if (lDE<0) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- localDE can't be less than 0", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 


 ! Get the Array 
 call ESMF_GridGetCoordIntoArray(grid, staggerLoc,coord, array, &
                                ESMF_DATA_REF, rc=status)
 if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return

 ! Obtain the native F90 array pointer via the LocalArray interface 
 allocate(larrayList(localDeCount))
 
 call ESMF_ArrayGet(array, larrayList=larrayList, rc=status) 
 if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return
 
 call ESMF_LocalArrayGetData(larrayList(lDE+1), fptr, doCopy, rc=status) 
 if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(larrayList) 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

      end subroutine ESMF_GridGetLocalTileCoord2DR8


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetLocalTileCoord"
!BOP
! !IROUTINE: ESMF_GridGetLocalTileCoord - Get pointer to coordinates of a local tile

! !INTERFACE:
  ! Private name; call using ESMF_GridGetLocalTileCoord()
      subroutine ESMF_GridGetLocalTileCoord3DR8(grid, localDE, &
                            staggerLoc, coord, fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in),optional :: localDE
      type (ESMF_StaggerLoc), intent(in),optional :: staggerLoc
      integer, intent(in) :: coord
      real(ESMF_Kind_R8), pointer :: fptr(:,:,:)
      type(ESMF_CopyFlag), intent(in), optional :: docopy
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    This method gets a fortran pointer to the piece of memory which holds the 
!    coordinate data for the given coordinate and stagger locations on the given local DE. 
!    This is useful, for example, for setting the coordinate values in a Grid, or
!    for reading the coordinate values. Eventually this method will be overloaded
!    for the full range of ESMF supported types and dimensions. 
!
!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          Grid to get the information from.
!     \item[{[localDE]}]
!          The local DE to get the information for. If not set, defaults to 
!          the first DE on this processor. (localDE starts at 0)
!     \item[{staggerLoc}]
!          The stagger location to get the information for. 
!          Please see Section~\ref{sec:opt:staggerloc} for a list 
!          of predefined stagger locations. If not present, defaults to
!          ESMF\_STAGGERLOC\_CENTER.
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


 ! Local variables 
 type(ESMF_Array) :: array 
 integer :: status ! local error status 
 integer :: localDeCount, rank 
 type(ESMF_TypeKind) :: typekind 
 type(ESMF_LocalArray), allocatable :: larrayList(:) 
 type(ESMF_CopyFlag) :: docopyInt
 integer :: lDE
 integer :: coordRank(ESMF_MAXDIM)

 ! Initialize return code 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid, rc) 

 ! Check consistency 
 call ESMF_GridGet(grid, coordTypeKind=typekind, rank=rank, coordRank=coordRank, &
                   localDECount=localDECount, rc=status) 
 if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return
 
 ! Require farrayPtr typekind to match Grid typekind 
 if (typekind .ne. ESMF_TYPEKIND_R8) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- farrayPtr typekind does not match Grid typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

! make sure coord is legitiamte
if ((coord .lt. 1) .or. (coord .gt. rank)) then
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- coord outside of range allowed for this Grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

 ! Require farrayPtr rank to match coordinate rank 
 if (coordRank(coord) .ne. 3) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- farrayPtr rank does not match requested coordinate rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

! Set Defaults
if (present(docopy)) then
   docopyInt=docopy
else
  docopyInt=ESMF_DATA_REF
endif

if (present(localDE)) then
   lDE=localDE
else
  lDE=0
endif


 ! Require DELayout to be 1 DE per PET 
 if (localDeCount < 0) then 
 call ESMF_LogMsgSetError(ESMF_RC_CANNOT_GET, & 
 "- Negative number of localDeCount prohibits request", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

 if (localDeCount == 0) then 
 call ESMF_LogMsgSetError(ESMF_RC_CANNOT_GET, & 
 "- localDeCount == 0 prohibits request", & 
 ESMF_CONTEXT, rc) 
 return 
 endif
 
 if (lDE>=localDeCount) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- localDE too big", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

 if (lDE<0) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- localDE can't be less than 0", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 


 ! Get the Array 
 call ESMF_GridGetCoordIntoArray(grid, staggerLoc,coord, array, &
                                ESMF_DATA_REF, rc=status)
 if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return

 ! Obtain the native F90 array pointer via the LocalArray interface 
 allocate(larrayList(localDeCount))
 
 call ESMF_ArrayGet(array, larrayList=larrayList, rc=status) 
 if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return
 
 call ESMF_LocalArrayGetData(larrayList(lDE+1), fptr, doCopy, rc=status) 
 if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(larrayList) 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

      end subroutine ESMF_GridGetLocalTileCoord3DR8


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetLocalTileInfo"
!BOP
! !IROUTINE: ESMF_GridGetLocalTileInfo - Get information about a local tile

! !INTERFACE:
      subroutine ESMF_GridGetLocalTileInfo(grid, coord, localDE, staggerLoc, &
          exclusiveLBound, exclusiveUBound, staggerLBound, staggerUBound, &
          computationalLBound, computationalUBound, totalLBound, totalUBound, rc)

!
! !ARGUMENTS:
      type(ESMF_Grid),        intent(in)            :: grid
      integer,                intent(in),  optional :: localDE
      integer,                intent(in)            :: coord
      type (ESMF_StaggerLoc), intent(in),  optional :: staggerLoc
      integer,                intent(out), optional :: exclusiveLBound(:)
      integer,                intent(out), optional :: exclusiveUBound(:)
      integer,                intent(out), optional :: staggerLBound(:)
      integer,                intent(out), optional :: staggerUBound(:)
      integer,                intent(out), optional :: computationalLBound(:)
      integer,                intent(out), optional :: computationalUBound(:)
      integer,                intent(out), optional :: totalLBound(:)
      integer,                intent(out), optional :: totalUBound(:)
      integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!  This method gets information about the range of index space which a particular
!  piece of coordinate data occupies. Note that unlike the output from the 
!  Array, these values also include the undistributed dimensions and are
!  ordered to reflect the order of the indices in the coordinate. So, for example,
!  {\tt totalLBound} and {\tt totalUBound} should match the bounds of the fortran array
!  retreived by {\tt ESMF\_GridGetLocalTileCoord}. 
!
!The arguments are:
!\begin{description}
!\item[{grid}]
!    Grid to get the information from.
!\item[{[localDE]}]
!     The local DE from which to get the information.  If not set, defaults to 
!     the first DE on this processor. (localDE starts at 0)
!\item[{coord}]
!     The coordinate component to get the information for (e.g. 1=x). 
!\item[{staggerLoc}]
!     The stagger location to get the information for. 
!     Please see Section~\ref{sec:opt:staggerloc} for a list 
!     of predefined stagger locations. If not present, defaults to
!     ESMF\_STAGGERLOC\_CENTER.
!\item[{[exclusiveLBound]}]
!     Upon return this holds the lower bounds of the exclusive region.
!     {\tt exclusiveLBound} must be allocated to be of size equal to the coord rank.
!\item[{[exclusiveUBound]}]
!     Upon return this holds the upper bounds of the exclusive region.
!     {\tt exclusiveUBound} must be allocated to be of size equal to the coord rank.
!\item[{[staggerLBound]}]
!     Upon return this holds the lower bounds of the stagger region.
!     {\tt staggerLBound} must be allocated to be of size equal to the coord rank.
!\item[{[staggerUBound]}]
!     Upon return this holds the upper bounds of the stagger region.
!     {\tt exclusiveUBound} must be allocated to be of size equal to the coord rank.
!\item[{[computationalLBound]}]
!     Upon return this holds the lower bounds of the computational region. 
!     {\tt computationalLBound} must be allocated to be of size equal to the coord rank.
!\item[{[computationalUBound]}]
!     Upon return this holds the upper bounds of the computational region.
!     {\tt computationalUBound} must be allocated to be of size equal to the coord rank.
!\item[{[totalLBound]}]
!     Upon return this holds the lower bounds of the total region.
!     {\tt totalLBound} must be allocated to be of size equal to the coord rank.
!\item[{[totalUBound]}]
!     Upon return this holds the upper bounds of the total region.
!     {\tt totalUBound} must be allocated to be of size equal to the coord rank.
!\item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!\end{description}
!
!EOP

    integer :: status ! local error status
    type(ESMF_InterfaceInt) :: exclusiveLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: exclusiveUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: staggerLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: staggerUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: totalLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: totalUBoundArg ! helper variable
    integer :: tmp_staggerloc

    ! Initialize return code
    status = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid, rc)

    ! handle staggerloc
    if (present(staggerLoc)) then
       tmp_staggerloc=staggerLoc%staggerloc
    else
       tmp_staggerloc=ESMF_STAGGERLOC_CENTER%staggerloc  ! default
    endif

    ! process optional arguments
    exclusiveLBoundArg=ESMF_InterfaceIntCreate(exclusiveLBound, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    exclusiveUBoundArg=ESMF_InterfaceIntCreate(exclusiveUBound, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    staggerLBoundArg=ESMF_InterfaceIntCreate(staggerLBound, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    staggerUBoundArg=ESMF_InterfaceIntCreate(staggerUBound, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalLBoundArg=ESMF_InterfaceIntCreate(computationalLBound, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalUBoundArg=ESMF_InterfaceIntCreate(computationalUBound, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalLBoundArg=ESMF_InterfaceIntCreate(totalLBound, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalUBoundArg = ESMF_InterfaceIntCreate(totalUBound, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_GridGetLocalTileInfo(grid, coord, localDE, tmp_staggerLoc, &
      exclusiveLBoundArg, exclusiveUBoundArg, staggerLBoundArg, staggerUBoundArg,&
      computationalLBoundArg, computationalUBoundArg, &
      totalLBoundArg, totalUBoundArg, status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deallocate interface ints
    call ESMF_InterfaceIntDestroy(exclusiveLBoundArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveUBoundArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(staggerLBoundArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(staggerUBoundArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalLBoundArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalUBoundArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalLBoundArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalUBoundArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetLocalTileInfo



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetFromDistGrid"
!BOP
! !IROUTINE: ESMF_GridSet - Set the values in a Grid which has been created with CreateEmpty. 

! !INTERFACE:
  ! Private name; call using ESMF_GridSet()
    subroutine ESMF_GridSetFromDistGrid(grid,name,coordTypeKind,distgrid, &
                 dimmap, lbounds, ubounds, coordRank, coordDimMap, &
                 indexflag, gridType, rc)
!
! !RETURN VALUE:

!
! !ARGUMENTS:
       type(ESMF_Grid),       intent(inout)           :: grid
       character (len=*),     intent(in),   optional  :: name
       type(ESMF_TypeKind),   intent(in),   optional  :: coordTypeKind
       type(ESMF_DistGrid),   intent(in),   optional  :: distgrid
       integer,               intent(in),   optional  :: dimmap(:)
       integer,               intent(in),   optional  :: lbounds(:)
       integer,               intent(in),   optional  :: ubounds(:)
       integer,               intent(in),   optional  :: coordRank(:)
       integer,               intent(in),   optional  :: coordDimMap(:,:)
       type(ESMF_IndexFlag),  intent(in),   optional  :: indexflag
       integer,               intent(in),   optional  :: gridType ! NOT IMPLEMENTED
       integer,               intent(out),  optional  :: rc
!
! !DESCRIPTION:
!  Set values in a grid in preparation for committing and creating a grid. This method
!  is called between {\tt ESMF\_GridCreateEmpty} and {\tt ESMF\_GridCommit}. Note that 
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
!       to ESMF\_GRIDTYPE\_UNKNOWN. [CURRENTLY NOT IMPLEMENTED]
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
    integer :: localrc ! local error status
    integer :: nameLen 
    type(ESMF_InterfaceInt) :: dimmapArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: lboundsArg ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: uboundsArg ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: coordRankArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: coordDimMapArg ! Language Interface Helper Var


    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_DistGridGetInit, distgrid, rc)
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_GridGetInit, grid, rc)

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
    coordRankArg = ESMF_InterfaceIntCreate(coordRank, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    coordDimMapArg = ESMF_InterfaceIntCreate(farray2D=coordDimMap, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call C++ Subroutine to do the create
    call c_ESMC_gridsetfromdistgrid(grid%this, nameLen, name, &
      coordTypeKind, distgrid, dimmapArg, lboundsArg, uboundsArg, coordRankArg, coordDimMapArg, &
      indexflag, gridtype, localrc)
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
    call ESMF_InterfaceIntDestroy(coordRankArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(coordDimMapArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridSetFromDistGrid


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetCoordFromArray"
!BOP
! !IROUTINE: ESMF_GridSetCoord - Set coordinates using ESMF Arrays

! !INTERFACE:
      subroutine ESMF_GridSetCoordFromArray(grid, staggerLoc, coord, &
                            array, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid),        intent(in)            :: grid
      type (ESMF_StaggerLoc), intent(in), optional  :: staggerLoc
      integer,                intent(in)            :: coord
      type(ESMF_Array),       intent(in)            :: array
      type(ESMF_CopyFlag),    intent(in), optional  :: docopy ! NOT IMPLEMENTED
      integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   This method sets the passed in Array as the holder of the coordinate data
!   for stagger location {\tt staggerloc} and coordinate {\tt coord}. If the location
!   already contains an Array, then this one overwrites it. 
!    
!     The arguments are:
!\begin{description}
!\item[{staggerLoc}]
!    The stagger location into which to copy the arrays. 
!    Please see Section~\ref{sec:opt:staggerloc} for a list 
!    of predefined stagger locations. If not present, defaults to
!    ESMF\_STAGGERLOC\_CENTER.
!\item[{coord}]
!    The coordinate component to put the data in (e.g. 1=x).
!\item[{array}]
!    An array to set the grid coordinate information from.
!\item[{[doCopy]}]
!    Default to {\tt ESMF\_DATA\_REF}, makes the grid reference the passed
!    in array. If set to {\tt ESMF\_DATA\_COPY} this routine makes a copy
!    of the array. [THE COPY OPTION IS CURRENTLY NOT IMPLEMENTED] 
!\item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!\end{description}
!
!EOP
    integer :: tmp_staggerloc
    integer :: localrc ! local error status

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayGetInit, array, rc)
!    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_GridGetInit, grid, rc)

    ! handle staggerloc
    if (present(staggerLoc)) then
       tmp_staggerloc=staggerLoc%staggerloc
    else
       tmp_staggerloc=ESMF_STAGGERLOC_CENTER%staggerloc
    endif

    ! Call C++ Subroutine to do the create
    call c_ESMC_gridsetcoordfromarray(grid%this,tmp_staggerloc, coord, &
      array, docopy, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridSetCoordFromArray

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetShapeIrreg"
!BOP
! !IROUTINE: ESMF_GridSetShape - Create a Grid with an irregular distribution

! !INTERFACE:
  ! Private name; call using ESMF_GridSetShape()
     subroutine ESMF_GridSetShapeIrreg(grid, name,coordTypeKind, minIndex,  &
                        countsPerDEDim1,countsPerDeDim2, countsPerDEDim3, &
                        connDim1, connDim2, connDim3, &
                        poleStaggerLoc1, poleStaggerLoc2, poleStaggerLoc3, &
                        bipolePos1, bipolePos2, bipolePos3, &
                        coordDep1, coordDep2, coordDep3, &
                        indexflag, gridType, petMap, rc)

!
! !ARGUMENTS:
	type (ESMF_Grid) :: grid
      character (len=*), intent(in), optional :: name 
       type(ESMF_TypeKind),  intent(in),    optional  :: coordTypeKind
       integer,               intent(in),   optional  :: minIndex(:)
       integer,               intent(in)              :: countsPerDEDim1(:)
       integer,               intent(in)              :: countsPerDEDim2(:)
       integer,               intent(in),   optional  :: countsPerDEDim3(:)
       type(ESMF_GridConn),   intent(in),   optional  :: connDim1(:)        ! N. IMP.
       type(ESMF_GridConn),   intent(in),   optional  :: connDim2(:)        ! N. IMP.
       type(ESMF_GridConn),   intent(in),   optional  :: connDim3(:)        ! N. IMP.
       type(ESMF_StaggerLoc), intent(in),   optional  :: poleStaggerLoc1(2) ! N. IMP.
       type(ESMF_StaggerLoc), intent(in),   optional  :: poleStaggerLoc2(2) ! N. IMP.
       type(ESMF_StaggerLoc), intent(in),   optional  :: poleStaggerLoc3(2) ! N. IMP.
       integer,               intent(in),   optional  :: bipolePos1(2)      ! N. IMP.
       integer,               intent(in),   optional  :: bipolePos2(2)      ! N. IMP.
       integer,               intent(in),   optional  :: bipolePos3(2)      ! N. IMP.
       integer,               intent(in),   optional  :: coordDep1(:)
       integer,               intent(in),   optional  :: coordDep2(:)
       integer,               intent(in),   optional  :: coordDep3(:)
       type(ESMF_IndexFlag),  intent(in),   optional  :: indexflag
       integer,               intent(in),   optional  :: gridType           ! N. IMP.
       integer,               intent(in),   optional  :: petMap(:,:,:)
       integer,               intent(out),  optional  :: rc
!
! !DESCRIPTION:
!
! WARNING: Due to current implementation limitations this 
!          set doesn't behave like the other set and may change 
!          previous settings in a irregular way. It is recommended
!          that this set be called only once on each partially formed Grid. 
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
!      Fortran array describing the index dimension 1 connections.
!      The first element represents the minimum end of dimension 1.
!      The second element represents the maximum end of dimension 1.
!      If array is only one element long, then that element is used
!      for both the minimum and maximum end. 
!      Please see Section~\ref{sec:opt:gridconn} for a list of valid 
!      options. If not present, defaults to ESMF\_GRIDCONN\_NONE. 
!     [CURRENTLY NOT IMPLEMENTED]
! \item[{[connDim2]}] 
!      Fortran array describing the index dimension 2 connections.
!      The first element represents the minimum end of dimension 2.
!      The second element represents the maximum end of dimension 2.
!      If array is only one element long, then that element is used
!      for both the minimum and maximum end. 
!      Please see Section~\ref{sec:opt:gridconn} for a list of valid 
!      options. If not present, defaults to ESMF\_GRIDCONN\_NONE. 
!     [CURRENTLY NOT IMPLEMENTED]
! \item[{[connDim3]}] 
!      Fortran array describing the index dimension 3 connections.
!      The first element represents the minimum end of dimension 3.
!      The second element represents the maximum end of dimension 3
!      If array is only one element long, then that element is used
!      for both the minimum and maximum end. 
!      Please see Section~\ref{sec:opt:gridconn} for a list of valid 
!      options. If not present, defaults to ESMF\_GRIDCONN\_NONE. 
!      [CURRENTLY NOT IMPLEMENTED]
! \item[{[poleStaggerLoc1]}] 
!     Two element array describing the index dimension 1 connections.
!      The first element represents the minimum end of dimension 1.
!      The second element represents the maximum end of dimension 1.
!      If a pole, this describes which staggerlocation is at the pole at each end.
!      Please see Section~\ref{sec:opt:staggerloc} for a list 
!      of predefined stagger locations. If not present, defaults to
!      ESMF\_STAGGERLOC\_CENTER.
!      [CURRENTLY NOT IMPLEMENTED]
! \item[{[poleStaggerLoc2]}] 
!     Two element array describing the index dimension 2 connections.
!      The first element represents the minimum end of dimension 2.
!      The second element represents the maximum end of dimension 2.
!      If a pole, this describes which staggerlocation is at the pole at each end.
!      Please see Section~\ref{sec:opt:staggerloc} for a list 
!      of predefined stagger locations. If not present, defaults to
!      ESMF\_STAGGERLOC\_CENTER.
!      [CURRENTLY NOT IMPLEMENTED]
! \item[{[poleStaggerLoc3]}] 
!     Two element array describing the index dimension 3 connections.
!      The first element represents the minimum end of dimension 3.
!      The second element represents the maximum end of dimension 3.
!      If a pole, this describes which staggerlocation is at the pole at each end.
!      If not present, the default is the edge.
!      Please see Section~\ref{sec:opt:staggerloc} for a list 
!      of predefined stagger locations. If not present, defaults to
!      ESMF\_STAGGERLOC\_CENTER.
!      [CURRENTLY NOT IMPLEMENTED]
! \item[{[bipolePos1]}] 
!     Two element array describing the index dimension 1 connections.
!      The first element represents the minimum end of dimension 1.
!      The second element represents the maximum end of dimension 1.
!      If a bipole, this gives the index position of one of the poles.
!      The other is half way around. If not present, the default is 1.
!      [CURRENTLY NOT IMPLEMENTED]
! \item[{[bipolePos2]}] 
!     Two element array describing the index dimension 2 connections.
!      The first element represents the minimum end of dimension 2.
!      The second element represents the maximum end of dimension 2.
!      If a bipole, this gives the index position of one of the poles.
!      The other is half way around. If not present, the default is 1.
!      [CURRENTLY NOT IMPLEMENTED]
! \item[{[bipolePos3]}] 
!     Two element array describing the index dimension 3 connections.
!      The first element represents the minimum end of dimension 3.
!      The second element represents the maximum end of dimension 3.
!      If a bipole, this gives the index position of one of the poles.
!      The other is half way around. If not present, the default is 1. 
!      [CURRENTLY NOT IMPLEMENTED]
! \item[{[coordDep1]}] 
!     This array specifies the dependence of the first 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the first
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is (/1,2,3/). 
! \item[{[coordDep2]}] 
!     This array specifies the dependence of the second 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the second
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is (/1,2,3/). 
! \item[{[coordDep3]}] 
!     This array specifies the dependence of the third 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the third
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is (/1,2,3/). 
! \item[{[indexflag]}]
!      Flag that indicates how the DE-local indices are to be defined.
! \item[{[petMap]}]
!       Sets the mapping of pets to the created DEs. This 3D
!       should be of size size(countsPerDEDim1) x size(countsPerDEDim2) x
!       size(countsPerDEDim3). If countsPerDEDim3 isn't present, then
!       the last dimension is of size 1.   
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
    type(ESMF_DistGrid)  :: distgrid
    type(ESMF_DELayout)  :: delayout
    integer, pointer     :: petList(:)
    integer, pointer     :: lbounds(:)
    integer, pointer     :: ubounds(:)
    integer, pointer     :: coordRank(:)
    integer, pointer     :: coordDimMap(:,:)
    integer              :: localrc
    integer              :: rank,i,distRank,undistRank,maxSizeDEDim
    integer, pointer     :: minIndexDG(:),maxIndexDG(:)
    integer, pointer     :: dimMap(:),minIndexLocal(:), deDimCount(:)
    integer, pointer     :: deBlockList(:,:,:),minPerDEDim(:,:),maxPerDEDim(:,:)
    integer              :: deCount
    integer              :: d,i1,i2,i3,k
    type(ESMF_GridConn)  :: connDim1Local(2)
    type(ESMF_GridConn)  :: connDim2Local(2)
    type(ESMF_GridConn)  :: connDim3Local(2)
    integer              :: connCount, petListCount 

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Compute the Grid Rank and Derivatives ---------------------------------------------------
    ! rank
    if (present(countsPerDEDim3)) then
	rank=3
    else
	rank=2
    endif

    ! rank of distributed part
    distRank=0 

    if (size(countsPerDEDim1) .gt. 1) then
       distRank=distRank+1
    endif

    if (size(countsPerDEDim2) .gt. 1) then
       distRank=distRank+1
    endif

    if (rank .gt. 2) then
       if (size(countsPerDEDim3) .gt. 1) then
           distRank=distRank+1
        endif
    endif

    ! ranks of the undistributed part of the grid
    undistRank=rank-distRank

    ! Argument Consistency Checking --------------------------------------------------------------
    if (size(countsPerDEDim1) .lt. 1) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- size 0 countsPerDEDim1 not allowed", & 
               ESMF_CONTEXT, rc) 
         return 
    endif

    if (size(countsPerDEDim2) .lt. 1) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- size 0 countsPerDEDim2 not allowed", & 
               ESMF_CONTEXT, rc) 
         return 
    endif

    if (present(countsPerDEDim3)) then
        if (size(countsPerDEDim3) .lt. 1) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
                    "- size 0 countsPerDEDim3 not allowed", & 
                    ESMF_CONTEXT, rc) 
            return 
        endif
    endif

    if ((rank .lt. 3) .and. present(connDim3)) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- connDim3 not allowed when grid is less than rank 3", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if ((rank .lt. 3) .and. present(poleStaggerLoc3)) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- poleStaggerLoc3 not allowed when grid is less than rank 3", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if ((rank .lt. 3) .and. present(bipolePos3)) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- bipolePos3 not allowed when grid is less than rank 3", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif


    if ((rank .lt. 3) .and. present(coordDep3)) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- coordDep3 not allowed when grid is less than rank 3", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if (present(coordDep1)) then
       if ((size(coordDep1) < 1) .or. (size(coordDep1)>rank)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- coordDep1 size incompatible with grid rank", & 
               ESMF_CONTEXT, rc) 
          return 
       endif
    endif

    if (present(coordDep2)) then
       if ((size(coordDep2) < 1) .or. (size(coordDep2)>rank)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- coordDep2 size incompatible with grid rank", & 
               ESMF_CONTEXT, rc) 
          return 
       endif
    endif

    if (present(coordDep3)) then
       if ((size(coordDep3) < 1) .or. (size(coordDep3)>rank)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- coordDep3 size incompatible with grid rank", & 
               ESMF_CONTEXT, rc) 
          return 
       endif
    endif

    if (present(minIndex)) then
       if (size(minIndex) .ne. rank) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- minIndex size must equal grid rank", & 
               ESMF_CONTEXT, rc) 
          return 
       endif
    endif


    if (present(petMap)) then
       if (rank .gt. 2) then
          if ((size(petMap,1) .ne. size(countsPerDEDim1)) .or. &
              (size(petMap,2) .ne. size(countsPerDEDim2)) .or. &
              (size(petMap,3) .ne. size(countsPerDEDim3))) then
              call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
                     "- petMap wrong size in one or more dimensions", & 
                     ESMF_CONTEXT, rc) 
              return 
          endif
       else
          if ((size(petMap,1) .ne. size(countsPerDEDim1)) .or. &
              (size(petMap,2) .ne. size(countsPerDEDim2)) .or. &
              (size(petMap,3) .ne. 1)) then
              call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
                     "- petMap wrong size in one or more dimensions", & 
                     ESMF_CONTEXT, rc) 
              return 
          endif
       endif
    endif


   ! Check for non-valid connection types here



   ! TODO: can you create an array without a distgrid??? What if everything they specify is undistributed?
   !       for now make a totally undistributed grid an error. Work on handling it later.
   !       Perhaps don't use lbounds, ubounds
    if (distRank .eq. 0) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- Need to have at least one distributed dimension", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif


   !TODO: Consider making some of these a separate local subroutine (particularly if you're going to 
   !      have 3 of these ShapeCreate subroutines with only minor changes


    ! Set Defaults ------------------------------------------------------------------

    ! Set default for minIndex 
    allocate(minIndexLocal(rank), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating minIndexLocal", &
                                     ESMF_CONTEXT, rc)) return

    if (present(minIndex)) then
       minIndexLocal(:)=minIndex(:)
    else
       do i=1,rank
          minIndexLocal(i)=1
       enddo
    endif

    ! Set Default for connections (although they don't work yet in distgrid/array, so they aren't really used anywhere yet.)
    if (present(connDim1)) then
       if (size(connDim1) .eq. 1) then
          connDim1Local(1)=connDim1(1)     
          connDim1Local(2)=connDim1(1)    ! if only 1 connection is specified then repeat for both ends  
       else if (size(connDim1) .ge. 2) then
          connDim1Local(1)=connDim1(1)
          connDim1Local(2)=connDim1(2)
       endif
    else
!       connDim1Local(1)=ESMF_GRIDCONN_NONE ! if not present then default to no connection
!       connDim1Local(2)=ESMF_GRIDCONN_NONE
    endif

    if (present(connDim2)) then
       if (size(connDim2) .eq. 1) then
          connDim2Local(1)=connDim2(1)     
          connDim2Local(2)=connDim2(1)    ! if only 1 connection is specified then repeat for both ends  
       else if (size(connDim2) .ge. 2) then
          connDim2Local(1)=connDim2(1)
          connDim2Local(2)=connDim2(2)
       endif
    else
!       connDim2Local(1)=ESMF_GRIDCONN_NONE ! if not present then default to no connection
!       connDim2Local(2)=ESMF_GRIDCONN_NONE
    endif

    if (present(connDim3)) then
       if (size(connDim3) .eq. 1) then
          connDim3Local(1)=connDim3(1)     
          connDim3Local(2)=connDim3(1)    ! if only 1 connection is specified then repeat for both ends  
       else if (size(connDim3) .ge. 2) then
          connDim3Local(1)=connDim3(1)
          connDim3Local(2)=connDim3(2)
       endif
    else
!       connDim3Local(1)=ESMF_GRIDCONN_NONE ! if not present then default to no connection
!       connDim3Local(2)=ESMF_GRIDCONN_NONE
    endif


   ! Calc minIndex,maxIndex,dimMap for DistGrid -----------------------------------
   allocate(minIndexDG(distRank), stat=localrc)
   if (ESMF_LogMsgFoundAllocError(localrc, "Allocating minIndexDG", &
               ESMF_CONTEXT, rc)) return
   allocate(maxIndexDG(distRank), stat=localrc)
   if (ESMF_LogMsgFoundAllocError(localrc, "Allocating minIndexDG", &
               ESMF_CONTEXT, rc)) return
   allocate(dimMap(distRank), stat=localrc)
   if (ESMF_LogMsgFoundAllocError(localrc, "Allocating dimMap", &
               ESMF_CONTEXT, rc)) return
          

   ! Fill in minIndex, maxIndex, dimMap
   d=1
   if (size(countsPerDEDim1) .gt. 1) then
      minIndexDG(d)=minIndexLocal(1)
      maxIndexDG(d)=sum(countsPerDeDim1)+minIndexDG(d)-1
      dimMap(d)=1      
      d=d+1
   endif

   if (size(countsPerDEDim2) .gt. 1) then
      minIndexDG(d)=minIndexLocal(2)
      maxIndexDG(d)=sum(countsPerDeDim2)+minIndexDG(d)-1
      dimMap(d)=2      
      d=d+1
   endif

   if (rank .gt. 2) then
      if (size(countsPerDEDim3) .gt. 1) then
         minIndexDG(d)=minIndexLocal(2)
         maxIndexDG(d)=sum(countsPerDeDim2)+minIndexDG(d)-1
         dimMap(d)=3      
         d=d+1
      endif
   endif


  ! Setup deBlockList for DistGrid ------------------------------------------------
  ! count de blocks
  deCount=1
  deCount=deCount*size(countsPerDEDim1) 
  deCount=deCount*size(countsPerDEDim2)
  if (rank .gt. 2) then
     deCount=deCount*size(countsPerDEDim3)
  endif 
 
  ! Calc the max size of a DEDim
  maxSizeDEDim=1
  if (size(countsPerDEDim1) .gt. maxSizeDEDim) then
      maxSizeDEDim=size(countsPerDEDim1)
  endif
  if (size(countsPerDEDim2) .gt. maxSizeDEDim) then
      maxSizeDEDim=size(countsPerDEDim2)
  endif
  if (rank .gt. 2) then
      if (size(countsPerDEDim3) .gt. maxSizeDEDim) then
         maxSizeDEDim=size(countsPerDEDim3)
      endif
  endif
  

  ! generate deblocklist
  allocate(maxPerDEDim(distRank,maxSizeDEDim), stat=localrc)
  if (ESMF_LogMsgFoundAllocError(localrc, "Allocating maxPerDEDim", &
              ESMF_CONTEXT, rc)) return
  allocate(minPerDEDim(distRank,maxSizeDEDim), stat=localrc)
  if (ESMF_LogMsgFoundAllocError(localrc, "Allocating minPerDEDim", &
              ESMF_CONTEXT, rc)) return
 allocate(deDimCount(distRank), stat=localrc)
  if (ESMF_LogMsgFoundAllocError(localrc, "Allocating maxPerDEDim", &
              ESMF_CONTEXT, rc)) return


  ! Calc the maximum end of each DE in a Dim, and the size of each DEDim
  d=1
  if (size(countsPerDEDim1) .gt. 1) then
      deDimCount(d)=size(countsPerDEDim1)
      minPerDeDim(d,1)=minIndexLocal(1)
      maxPerDeDim(d,1)=minIndexLocal(1)+countsPerDEDim1(1)-1
      do i=2,deDimCount(d) 
         minPerDEDim(d,i)=maxPerDEDim(d,i-1)+1
         maxPerDEDim(d,i)=minPerDEDim(d,i)+countsPerDEDim1(i)-1
      enddo
      d=d+1  ! advance to next distgrid dimension
  endif

  if (size(countsPerDEDim2) .gt. 1) then
      deDimCount(d)=size(countsPerDEDim2)
      minPerDeDim(d,1)=minIndexLocal(2)
      maxPerDeDim(d,1)=minIndexLocal(2)+countsPerDEDim2(1)-1
      do i=2,deDimCount(d) 
         minPerDEDim(d,i)=maxPerDEDim(d,i-1)+1
         maxPerDEDim(d,i)=minPerDEDim(d,i)+countsPerDEDim2(i)-1
      enddo
      d=d+1  ! advance to next distgrid dimension
  endif

  if (rank .gt. 2) then
  if (size(countsPerDEDim3) .gt. 1) then
      deDimCount(d)=size(countsPerDEDim3)
      minPerDeDim(d,1)=minIndexLocal(3)
      maxPerDeDim(d,1)=minIndexLocal(3)+countsPerDEDim3(1)-1
      do i=2,deDimCount(d) 
         minPerDEDim(d,i)=maxPerDEDim(d,i-1)+1
         maxPerDEDim(d,i)=minPerDEDim(d,i)+countsPerDEDim3(i)-1
      enddo
      d=d+1  ! advance to next distgrid dimension
  endif
  endif


  ! allocate deblocklist
  allocate(deBlockList(distRank,2,deCount), stat=localrc)
  if (ESMF_LogMsgFoundAllocError(localrc, "Allocating deBlockList", &
              ESMF_CONTEXT, rc)) return

  ! Fill in DeBlockList
  if (distRank .eq. 1) then
     k=1
     do i1=1,deDimCount(1)
        deBlockList(1,1,k)=minPerDEDim(1,i1)
        deBlockList(1,2,k)=maxPerDEDim(1,i1)
        k=k+1
     enddo
  else if (distRank .eq. 2) then
     k=1
     do i2=1,deDimCount(2)
     do i1=1,deDimCount(1)
        deBlockList(1,1,k)=minPerDEDim(1,i1)
        deBlockList(1,2,k)=maxPerDEDim(1,i1)
        deBlockList(2,1,k)=minPerDEDim(2,i2)
        deBlockList(2,2,k)=maxPerDEDim(2,i2)
        k=k+1
     enddo
     enddo
  else if (distRank .eq. 3) then
     k=1
     do i3=1,deDimCount(3)
     do i2=1,deDimCount(2)
     do i1=1,deDimCount(1)
        deBlockList(1,1,k)=minPerDEDim(1,i1)
        deBlockList(1,2,k)=maxPerDEDim(1,i1)
        deBlockList(2,1,k)=minPerDEDim(2,i2)
        deBlockList(2,2,k)=maxPerDEDim(2,i2)
        deBlockList(3,1,k)=minPerDEDim(3,i3)
        deBlockList(3,2,k)=maxPerDEDim(3,i3)
        k=k+1
     enddo
     enddo
     enddo
  endif
  
!  do i=1,deCount
!     write(*,*) i,"min=",deBlockList(:,1,i)," max=",deBlockList(:,2,i)
!  enddo


   
   ! Setup Connections between patch sides ----------------------------------------

   ! CONNECTIONS DON'T WORK YET SO NOT IMPLEMENTED

   ! Process PetMap --------------------------------------------------------------
   if (present(petMap)) then

      !! Allocate petList
      allocate(petList(deCount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating petList", &
              ESMF_CONTEXT, rc)) return


      !! copy petMap to petList
      if (rank .gt. 2) then
	 k=1
     	 do i3=1,size(countsPerDEDim3)
         do i2=1,size(countsPerDEDim2)
         do i1=1,size(countsPerDEDim1)
            petList(k)=petMap(i1,i2,i3)
            k=k+1
         enddo
         enddo
         enddo
      else 
	 k=1
     	 do i3=1,1
         do i2=1,size(countsPerDEDim2)
         do i1=1,size(countsPerDEDim1)
            petList(k)=petMap(i1,i2,i3)
            k=k+1
         enddo
         enddo
         enddo
      endif

      !! create delayout from the petList
      delayout=ESMF_DELayoutCreate(petList=petList,rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

      !! Get rid of list
      deallocate(petList)
   else
      !! create a default delayout
      delayout=ESMF_DELayoutCreate(deCount=deCount,rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
   endif


   ! Create DistGrid --------------------------------------------------------------

    distgrid=ESMF_DistGridCreate(minIndex=minIndexDG, maxIndex=maxIndexDG, &
               deBlockList=deBlockList, delayout=delayout, indexflag=indexflag, rc=localrc)   
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


   ! Calc lbounds, ubounds for Grid -----------------------------------------------
   if (undistRank .gt. 0) then
      allocate(lbounds(undistRank), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating lbounds", &
              ESMF_CONTEXT, rc)) return
      allocate(ubounds(undistRank), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating ubounds", &
              ESMF_CONTEXT, rc)) return     

      ! Fill in minIndex, maxIndex, dimMap
       d=1
      if (size(countsPerDEDim1) .eq. 1) then
         lbounds(d)=minIndexLocal(1)
         ubounds(d)=countsPerDEDim1(1)+lbounds(d)-1
         d=d+1
      endif

      if (size(countsPerDEDim2) .eq. 1) then
         lbounds(d)=minIndexLocal(2)
         ubounds(d)=countsPerDEDim2(1)+lbounds(d)-1
         d=d+1
      endif

      if (rank .gt. 2) then
         if (size(countsPerDEDim3) .eq. 1) then
            lbounds(d)=minIndexLocal(3)
            ubounds(d)=countsPerDEDim3(1)+lbounds(d)-1
            d=d+1
         endif
      endif
   endif


   ! Convert coordDeps to coordRank and coordDimMap -------------------------------
   allocate(coordRank(rank), stat=localrc)
   if (ESMF_LogMsgFoundAllocError(localrc, "Allocating coordRank", &
              ESMF_CONTEXT, rc)) return
   allocate(coordDimMap(rank,rank), stat=localrc)
   if (ESMF_LogMsgFoundAllocError(localrc, "Allocating coordDimMap", &
              ESMF_CONTEXT, rc)) return

   if (present(coordDep1)) then
      coordRank(1)=size(coordDep1)
      coordDimMap(1,:)=0
      do i=1,size(coordDep1)
         coordDimMap(1,i)=coordDep1(i)
      enddo
   else 
      coordRank(1)=rank
      do i=1,rank
         coordDimMap(1,i)=i      
      enddo
   endif

   if (present(coordDep2)) then
      coordRank(2)=size(coordDep2)
      coordDimMap(2,:)=0
      do i=1,size(coordDep2)
         coordDimMap(2,i)=coordDep2(i)
      enddo
   else 
      coordRank(2)=rank
      do i=1,rank
         coordDimMap(2,i)=i      
      enddo
   endif

   if (rank .gt. 2) then
      if (present(coordDep3)) then 
         coordRank(3)=size(coordDep3)
          coordDimMap(3,:)=0
          do i=1,size(coordDep3)
             coordDimMap(3,i)=coordDep3(i)
          enddo
      else 
        coordRank(3)=rank
        do i=1,rank
	   coordDimMap(3,i)=i      
        enddo
      endif
   endif

  
   ! Create Grid from specification -----------------------------------------------
   if (undistRank .gt. 0) then
       call ESMF_GridSetFromDistGrid(grid, name, coordTypeKind, &
                                    distgrid, dimmap, lbounds, ubounds, &
                                    coordRank, coordDimMap, indexflag, &
                                    gridType, localrc)
    else
       call ESMF_GridSetFromDistGrid(grid, name, coordTypeKind, &
                                    distgrid=distgrid, dimmap=dimmap, &
                                    coordRank=coordRank, coordDimMap=coordDimMap, &
                                    indexflag=indexflag, &
                                    gridtype=gridType, rc=localrc)
    endif
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return



    ! Clean up memory
    deallocate(coordRank)
    deallocate(coordDimMap)
    deallocate(minIndexDG)
    deallocate(maxIndexDG)
    deallocate(dimMap)
    deallocate(maxPerDEDim)
    deallocate(minPerDEDim)
    deallocate(deDimCount)
    deallocate(deBlockList)
    if (undistRank .gt. 0) then
       deallocate(lbounds)
       deallocate(ubounds)
    endif

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    end subroutine ESMF_GridSetShapeIrreg





!------------------------------------------------------------------------------

!==============================================================================
! UNREVIEWED AND UNIMPLEMENTED METHODS
!==============================================================================

#ifdef NEWGRID_NOT_THIS_PHASE
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAddMetricFromArray"
!BOPI
! !IROUTINE: ESMF_GridAddMetricFromArray - Add a new metric from an Array.

! !INTERFACE:
  ! Private name; call using ESMF_GridAddMetric()
      subroutine ESMF_GridSetMetricFromArray(grid, name, staggerLoc, &
                            array, metricDep, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len=*), intent(in) :: name
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

      end subroutine ESMF_GridAddMetricFromArray


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAddMetricFromFptr"
!BOPI
! !IROUTINE: ESMF_GridSetMetricFromFptr - Sets metric data from a Fortran pointer.

! !INTERFACE:
  ! Private name; call using ESMF_GridAddMetric()
      subroutine ESMF_GridSetMetricFromFptr(grid, name, staggerLoc, metricDep, &
                            fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len=*), intent(in) :: name
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

      end subroutine ESMF_GridAddMetricFromFptr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAddMetricNoValues"
!BOPI
! !IROUTINE: ESMF_GridAddMetricNoValues - Allocates space for metric, but doesn't set data.

! !INTERFACE:
! Private name; call using ESMF_GridAddMetric()
      subroutine ESMF_GridAddMetricNoSet(grid, name, metricTypeKind, 
                   staggerLoc, metricDep, lbounds, ubounds, &                         
                   metricLWidth, metricUWidth, metricAlign, &
                   computationalLWidth, computationalUWidth, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len=*), intent(in) :: name
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

      end subroutine ESMF_GridAddMetricNoValues


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAddMetricSub"
!BOPI
! !IROUTINE: ESMF_GridAddMetricSub - Add a subroutine interface to generate metric data.

! !INTERFACE:
  ! Private name; call using ESMF_GridAddMetric()
      subroutine ESMF_GridAddMetricSub(grid, name, metricTypeKind, 
                   staggerLoc, metricDep, lbounds, ubounds, &                         
                   metricLWidth, metricUWidth, metricAlign, &
                   computationalLWidth, computationalUWidth,&
                   sub, userDataR4, userDataR8, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len=*), intent(in) :: name
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
      integer,               intent(in),             :: sub
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

      end subroutine ESMF_GridAddMetricSub


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCalcStaggerLocCoord"
!BOPI
! !IROUTINE: ESMF_GridCalcStaggerLocCoord - Calculates the coordinates of a set of stagger locations from another stagger location's coordinates .

! !INTERFACE:
      subroutine ESMF_GridCalcStaggerLocCoord(grid, srcStaggerLoc, dstStaggerLocs, &
                             method, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
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

      end subroutine ESMF_GridCalcStaggerLocCoord

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreateShapeReg"
!BOPI
! !IROUTINE: ESMF_GridCreateShape - Create a Grid with a regular distribution

! !INTERFACE:
  ! Private name; call using ESMF_GridCreateShape()
      function ESMF_GridCreateShapeReg(name,coordTypeKind,  &
                        minIndex, maxIndex, regDecomp, &
                        connDim1, connDim2, connDim3, &
                        poleStaggerLoc1, poleStaggerLoc2, poleStaggerLoc3, &
                        bipolePos1, bipolePos2, bipolePos3, &
                        coordDep1, coordDep2, coordDep3, &
                        indexflag, gridType, haloDepth, petMap, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateShapeReg
!
! !ARGUMENTS:
      character (len=*), intent(in), optional :: name 
       type(ESMF_TypeKind),  intent(in),    optional  :: coordTypeKind
       integer,               intent(in),   optional  :: minIndex(:)
       integer,               intent(in)              :: maxIndex(:)
       integer,               intent(in),   optional  :: regDecomp(:)
       type(ESMF_GridConn),   intent(in),   optional  :: connDim1(2)
       type(ESMF_GridConn),   intent(in),   optional  :: connDim2(2)
       type(ESMF_GridConn),   intent(in),   optional  :: connDim3(2)
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
       integer,               intent(in),   optional  :: gridtype
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
!      The second element represents the maximum end of dimension 1.
!      The valid setting are ESMF\_GRIDCONN\_NONE, ESMF\_GRIDCONN\_POLE,
!       ESMF\_GRIDCONN\_BIPOLE, or ESMF\_GRIDCONN\_PERIODIC. 
!       If one element is set to ESMF\_GRIDCONN\_PERIODIC then both must be. 
! \item[{[connDim2]}] 
!     Two element array describing the index dimension 2 connections.
!      The first element represents the minimum end of dimension 2.
!      The second element represents the maximum end of dimension 2.
!      The valid setting are ESMF\_GRIDCONN\_NONE, ESMF\_GRIDCONN\_POLE,
!       ESMF\_GRIDCONN\_BIPOLE, or ESMF\_GRIDCONN\_PERIODIC. 
!       If one element is set to ESMF\_GRIDCONN\_PERIODIC then both must be. 
! \item[{[connDim3]}] 
!     Two element array describing the index dimension 3 connections.
!      The first element represents the minimum end of dimension 3.
!      The second element represents the maximum end of dimension 3.
!      The valid setting are ESMF\_GRIDCONN\_NONE, ESMF\_GRIDCONN\_POLE,
!       ESMF\_GRIDCONN\_BIPOLE, or ESMF\_GRIDCONN\_PERIODIC. 
!       If one element is set to ESMF\_GRIDCONN\_PERIODIC then both must be. 
! \item[{[poleStaggerLoc1]}] 
!     Two element array describing the index dimension 1 connections.
!      The first element represents the minimum end of dimension 1.
!      The second element represents the maximum end of dimension 1.
!      If a pole, this describes which staggerlocation is at the pole at each end.
!      If not present, the default is the edge.
! \item[{[poleStaggerLoc2]}] 
!     Two element array describing the index dimension 2 connections.
!      The first element represents the minimum end of dimension 2.
!      The second element represents the maximum end of dimension 2.
!      If a pole, this describes which staggerlocation is at the pole at each end.
!      If not present, the default is the edge.
! \item[{[poleStaggerLoc3]}] 
!     Two element array describing the index dimension 3 connections.
!      The first element represents the minimum end of dimension 3.
!      The second element represents the maximum end of dimension 3.
!      If a pole, this describes which staggerlocation is at the pole at each end.
!      If not present, the default is the edge.
! \item[{[bipolePos1]}] 
!     Two element array describing the index dimension 1 connections.
!      The first element represents the minimum end of dimension 1.
!      The second element represents the maximum end of dimension 1.
!      If a bipole, this gives the index position of one of the poles.
!      The other is half way around. If not present, the default is 1.
! \item[{[bipolePos2]}] 
!     Two element array describing the index dimension 2 connections.
!      The first element represents the minimum end of dimension 2.
!      The second element represents the maximum end of dimension 2.
!      If a bipole, this gives the index position of one of the poles.
!      The other is half way around. If not present, the default is 1.
! \item[{[bipolePos3]}] 
!     Two element array describing the index dimension 3 connections.
!      The first element represents the minimum end of dimension 3.
!      The second element represents the maximum end of dimension 3.
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
!EOPI

      end function ESMF_GridCreateShapeReg


!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreateArb"
!BOPI
! !IROUTINE: ESMF_GridCreate - Create a new grid 

! !INTERFACE:
  ! Private name; call using ESMF_GridCreate()
      function ESMF_GridCreateArb(name,coordTypeKind, minIndex,maxIndex, localIndices, &
                        dimmap, arbDecomp, lbounds, ubounds, coordRank, &
                        coordDimMap, &
                        indexflag, gridType, noData,
                        computationalLWidth, computationalUWidth,  &
                        connectionList, connectionTransformList, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateArb
!
! !ARGUMENTS:
       character (len=*), intent(in), optional :: name
       type (ESMF_StaggerLoc), intent(in),optional :: staggerLocs(:)
       type(ESMF_TypeKind),  intent(in),optional  :: coordTypeKind
       integer,               intent(in),   optional  :: minIndex(:)
       integer,               intent(in)              :: maxIndex(:)
       integer,               intent(in)              :: localIndices(:,:)
       integer,               intent(in),   optional  :: dimmap(:)
       integer,               intent(in),   optional  :: lbounds(:)
       integer,               intent(in),   optional  :: ubounds(:)
       integer,               intent(in),   optional  :: coordRank(:)
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
! \item[{[coordRank]}]
!      List that has as many elements as the grid rank .
!      Gives the dimension of each component (e.g. x) array. This is 
!      to allow factorization of the coordinate arrays. If not specified
!      all arrays are the same size as the grid. 
! \item[{[coordDimMap]}]
!      2D list of size grid rank x grid rank. This array describes the
!      map of each component array's dimensions onto the grids
!      dimensions. Each entry {\tt coordDimMap(i,j)} tells which
!      grid dimension component i's, jth dimension maps to. 
!      Note that if j is bigger than {\tt coordRank(i)} than its ignored.        
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

      end function ESMF_GridCreateArb


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreateFromArrays"
!BOPI
! !IROUTINE: ESMF_GridCreate - Create a Grid from a set of Arrays

! !INTERFACE:
  ! Private name; call using ESMF_GridCreate()
      function ESMF_GridCreateFromArrays(name, arrays, staggerLocs, &
          staggerLocAligns, coordDimMap, doCopy, gridType, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateFromArrays
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


      end function ESMF_GridCreateFromArrays



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreateReg"
!BOPI
! !IROUTINE: ESMF_GridCreate - Create a Grid with a regular decomposition

! !INTERFACE:
  ! Private name; call using ESMF_GridCreate()
      function ESMF_GridCreateReg(name,coordTypeKind, minIndex,maxIndex, dimmap, &
                        regDecomp, lbounds, ubounds, coordRank, &
                        coordDimMap, &
                        indexflag, gridType, noData, &
                        computationalLWidth, computationalUWidth, &
                        connectionList, connectionTransformList, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateReg
!
! !ARGUMENTS:
       character (len=*), intent(in), optional :: name
       type(ESMF_TypeKind),  intent(in),optional  :: coordTypeKind
       integer,               intent(in),   optional  :: minIndex(:)
       integer,               intent(in)              :: maxIndex(:)
       integer,               intent(in),   optional  :: blockDecomp(:)
       integer,               intent(in),   optional  :: dimmap(:)
       integer,               intent(in),   optional  :: lbounds(:)
       integer,               intent(in),   optional  :: ubounds(:)
       integer,               intent(in),   optional  :: coordRank(:)
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
! \item[{[coordRank]}]
!      List that has as many elements as the grid rank .
!      Gives the dimension of each component (e.g. x) array. This is 
!      to allow factorization of the coordinate arrays. If not specified
!      all arrays are the same size as the grid. 
! \item[{[coordDimMap]}]
!      2D list of size grid rank x grid rank. This array describes the
!      map of each component array's dimensions onto the grids
!      dimensions. Each entry {\tt coordDimMap(i,j)} tells which
!      grid dimension component i's, jth dimension maps to. 
!      Note that if j is bigger than {\tt coordRank(i)} than its ignored. ! \item[{[indexflag]}]
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

      end function ESMF_GridCreateReg


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreateIrreg"
!BOPI
! !IROUTINE: ESMF_GridCreate - Create a Grid with an irregular decomposition

! !INTERFACE:
  ! Private name; call using ESMF_GridCreate()
   function ESMF_GridCreateIrreg(name,coordTypeKind, minIndex, countsPerDE, dimmap, &
                         lbounds, ubounds, coordRank, &
                        coordDimMap, &
                        indexflag, gridType, noData, &
                        computationalLWidth, computationalUWidth, petMap, &
                         connectionList, connectionTransformList, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateIrreg
!
! !ARGUMENTS:
       character (len=*), intent(in), optional :: name
       type(ESMF_TypeKind),  intent(in),optional  :: coordTypeKind
       integer,               intent(in)          ::   countPerDE(:,:)
       integer,               intent(in),   optional  :: dimmap(:)
       integer,               intent(in),   optional  :: minIndex(:)
       integer,               intent(in),   optional  :: lbounds(:)
       integer,               intent(in),   optional  :: ubounds(:)
       integer,               intent(in),   optional  :: coordRank(:)
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
! \item[{[coordRank]}]
!      List that has as many elements as the grid rank .
!      Gives the dimension of each component (e.g. x) array. This is 
!      to allow factorization of the coordinate arrays. If not specified
!      all arrays are the same size as the grid. 
! \item[{[coordDimMap]}]
!      2D list of size grid rank x grid rank. This array describes the
!      map of each component array's dimensions onto the grids
!      dimensions. Each entry {\tt coordDimMap(i,j)} tells which
!      grid dimension component i's, jth dimension maps to. 
!      Note that if j is bigger than {\tt coordRank(i)} than its ignored.        
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

      end function ESMF_GridCreateIrreg


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreate"
!BOPI
! !IROUTINE: ESMF_GridCreate - Create a new grid 

! !INTERFACE:
  ! Private name; call using ESMF_GridCreate()
   function ESMF_GridCreateRegP(name,coordTypeKind, minIndex, countsPerDE, dimmap, &
                         lbounds, ubounds, coordRank, &
                        coordDimMap, &
                        indexflag, gridType, noData, &
                        computationalLWidth, computationalUWidth, petMap, &
                         connectionList, connectionTransformList, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateReg
!
! !ARGUMENTS:
       character (len=*), intent(in), optional :: name
       type(ESMF_TypeKind),  intent(in),optional  :: coordTypeKind
       integer,               intent(in),         ::   countPerDE(:,:,:)
       integer,               intent(in),   optional  :: dimmap(:)
       integer,               intent(in),   optional  :: minIndex(:,:)
       integer,               intent(in),   optional  :: lbounds(:)
       integer,               intent(in),   optional  :: ubounds(:)
       integer,               intent(in),   optional  :: coordRank(:)
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
! \item[{[coordRank]}]
!      List that has as many elements as the grid rank .
!      Gives the dimension of each component (e.g. x) array. This is 
!      to allow factorization of the coordinate arrays. If not specified
!      all arrays are the same size as the grid. 
! \item[{[coordDimMap]}]
!      2D list of size grid rank x grid rank. This array describes the
!      map of each component array's dimensions onto the grids
!      dimensions. Each entry {\tt coordDimMap(i,j)} tells which
!      grid dimension component i's, jth dimension maps to. 
!      Note that if j is bigger than {\tt coordRank(i)} than its ignored.        
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

      end function ESMF_GridCreateRegP



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreateShapeArb"
!BOPI
! !IROUTINE: ESMF_GridCreateShape - Create a Grid with an arbitrary distribution

! !INTERFACE:
  ! Private name; call using ESMF_GridCreateShape()
      function ESMF_GridCreateShapeArb(name,coordTypeKind,  &
                        minIndex, maxIndex, localIndices, &
                        connDim1, connDim2, connDim3, &
                        poleStaggerLoc1, poleStaggerLoc2, poleStaggerLoc3, &
                        bipolePos1, bipolePos2, bipolePos3, &
                        coordDep1, coordDep2, coordDep3, &
                        indexflag, gridType, haloDepth, petMap, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateShapeArb
!
! !ARGUMENTS:
      character (len=*), intent(in), optional :: name 
       type(ESMF_TypeKind),  intent(in),    optional  :: coordTypeKind
       integer,               intent(in),   optional  :: minIndex(:)
       integer,               intent(in)              :: maxIndex(:)
       integer,               intent(in)              :: localIndices(:,:)
       type(ESMF_GridConn),   intent(in),   optional  :: connDim1(2)
       type(ESMF_GridConn),   intent(in),   optional  :: connDim2(2)
       type(ESMF_GridConn),   intent(in),   optional  :: connDim3(2)
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
!      The second element represents the maximum end of dimension 1.
!      The valid setting are ESMF\_GRIDCONN\_NONE, ESMF\_GRIDCONN\_POLE,
!       ESMF\_GRIDCONN\_BIPOLE, or ESMF\_GRIDCONN\_PERIODIC. 
!       If one element is set to ESMF\_GRIDCONN\_PERIODIC then both must be. 
! \item[{[connDim2]}] 
!     Two element array describing the index dimension 2 connections.
!      The first element represents the minimum end of dimension 2.
!      The second element represents the maximum end of dimension 2.
!      The valid setting are ESMF\_GRIDCONN\_NONE, ESMF\_GRIDCONN\_POLE,
!       ESMF\_GRIDCONN\_BIPOLE, or ESMF\_GRIDCONN\_PERIODIC. 
!       If one element is set to ESMF\_GRIDCONN\_PERIODIC then both must be. 
! \item[{[connDim3]}] 
!     Two element array describing the index dimension 3 connections.
!      The first element represents the minimum end of dimension 3.
!      The second element represents the maximum end of dimension 3.
!      The valid setting are ESMF\_GRIDCONN\_NONE, ESMF\_GRIDCONN\_POLE,
!       ESMF\_GRIDCONN\_BIPOLE, or ESMF\_GRIDCONN\_PERIODIC. 
!       If one element is set to ESMF\_GRIDCONN\_PERIODIC then both must be. 
! \item[{[poleStaggerLoc1]}] 
!     Two element array describing the index dimension 1 connections.
!      The first element represents the minimum end of dimension 1.
!      The second element represents the maximum end of dimension 1.
!      If a pole, this describes which staggerlocation is at the pole at each end.
!      If not present, the default is the edge.
! \item[{[poleStaggerLoc2]}] 
!     Two element array describing the index dimension 2 connections.
!      The first element represents the minimum end of dimension 2.
!      The second element represents the maximum end of dimension 2.
!      If a pole, this describes which staggerlocation is at the pole at each end.
!      If not present, the default is the edge.
! \item[{[poleStaggerLoc3]}] 
!     Two element array describing the index dimension 3 connections.
!      The first element represents the minimum end of dimension 3.
!      The second element represents the maximum end of dimension 3.
!      If a pole, this describes which staggerlocation is at the pole at each end.
!      If not present, the default is the edge.
! \item[{[bipolePos1]}] 
!     Two element array describing the index dimension 1 connections.
!      The first element represents the minimum end of dimension 1.
!      The second element represents the maximum end of dimension 1.
!      If a bipole, this gives the index position of one of the poles.
!      The other is half way around. If not present, the default is 1.
! \item[{[bipolePos2]}] 
!     Two element array describing the index dimension 2 connections.
!      The first element represents the minimum end of dimension 2.
!      The second element represents the maximum end of dimension 2.
!      If a bipole, this gives the index position of one of the poles.
!      The other is half way around. If not present, the default is 1.
! \item[{[bipolePos3]}] 
!     Two element array describing the index dimension 3 connections.
!      The first element represents the minimum end of dimension 3.
!      The second element represents the maximum end of dimension 3.
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

      end function ESMF_GridCreateShapeArb



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGenCoordUni"
!BOPI
! !IROUTINE: ESMF_GridGenCoordUni - Fill coordinates with uniformly spaced values

! !INTERFACE:
      subroutine ESMF_GridGenCoordUni(grid, tile, begCoord, endCoord, rc)
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
!EOPI

      end subroutine ESMF_GridGenCoordUni


!------------------------------------------------------------------------------
!BOPI
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
!EOPI


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_GridGetInternalState - Get private data block pointer
!
! !INTERFACE:
!      subroutine ESMF_GridGetInternalState(gridcomp, dataPointer, rc)
!
! !ARGUMENTS:
!      type(ESMF_Grid), intent(inout) :: gridcomp
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
#define ESMF_METHOD "ESMF_GridGetMetricIntoFptr"
!BOPI
! !IROUTINE: ESMF_GridGetMetricIntoFptr -  Gets metric data from a grid and puts it into a Fortran pointer.

! !INTERFACE:
  ! Private name; call using ESMF_GridGetMetric()
      subroutine ESMF_GridGetMetricIntoFptr(grid, name, staggerLoc, dep, &
                            fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len=*), intent(in) :: name
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

      end subroutine ESMF_GridGetMetricIntoFptr




!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetStaggerLocInfo"
!BOPI
! !IROUTINE: ESMF_GridGetStaggerLocInfo - Get information about a stagger location 

! !INTERFACE:
     subroutine ESMF_GridGetStaggerLocInfo(grid, staggerLoc, coord, isAllocated, &
                        staggerLWidth, staggerUWidth, staggerAlign, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid),        intent(in)              :: grid 
      type (ESMF_StaggerLoc), intent(in)              :: staggerLoc
      integer,                intent(in)              :: coord
      logical,                intent(in),   optional  :: isAllocated
      integer,                intent(in),   optional  :: staggerLWidth(:)
      integer,                intent(in),   optional  :: staggerUWidth(:)
      integer,                intent(in),   optional  :: staggerAlign(:)
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
! \item[{[staggerLWidth]}] 
!      This array should be the same rank as the grid. It specifies the lower corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[staggerUWidth]}] 
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
!EOPI

      end subroutine ESMF_GridGetStaggerLocInfo



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridHalo"
!BOPI
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
!EOPI


      end function ESMF_GridHalo



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridLocalTileCalcBnds"
!BOPI
! !IROUTINE: ESMF_GridLocalTileCalcBnds - Given 

! !INTERFACE:
     subroutine ESMF_GridLocalTileCalcBnds(grid, tile, localDE, coord, staggerLoc, &
                        staggerLocLWidth, staggerLocUWidth, &
                        staggerAlign, lBounds, uBounds, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid 
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

      end function ESMF_GridLocalTileCalcBnds


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridLocalTileGetCoord"
!BOPI
! !IROUTINE: ESMF_GridLocalTileGetCoord - Get coordinates of a local tile given indices

! !INTERFACE:
      subroutine ESMF_GridLocalTileGetCoordIntoArray(grid, staggerLoc, &
                 tile, localDE, indices, coords, rc)
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
!EOPI

      end subroutine ESMF_GridLocalTileGetCoord


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridLocalTileGetMetric"
!BOPI
! !IROUTINE: ESMF_GridLocalTileGetMetric - get the fortran data pointer for the piece of  metic data on this tile on this DE.

! !INTERFACE:
      subroutine ESMF_GridLocalTileGetMetric(grid, name, tile, localDE, &
                                      fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len=*), intent(in) :: name
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

      end subroutine ESMF_GridLocalTileGetMetric


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridLocalTileSetCoord"
!BOPI
! !IROUTINE: ESMF_GridLocalTileSetCoord - Set the coordinates of a local tile given indices

! !INTERFACE:
      subroutine ESMF_GridLocalTileSetCoord(grid, staggerLoc, tile, localDE, &
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
!EOPI

      end subroutine ESMF_GridLocalTileSetCoord


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridLocalTileSetMetric"
!BOPI
! !IROUTINE: ESMF_GridLocalTileSetMetric - set the metic data on this tile on this DE using a fortran pointer.

! !INTERFACE:
      subroutine ESMF_GridLocalTileSetMetric(grid, name, tile, localDE, &
                                      fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len=*), intent(in) :: name
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

      end subroutine ESMF_GridLocalTileSetMetric


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetCoordFromFptr"
!BOPI
! !IROUTINE: ESMF_GridSetCoord - Set coordinates from fortran pointer

! !INTERFACE:
      subroutine ESMF_GridSetCoordFromFptr2DR8(grid, staggerLoc, coord, &
                   fptr, doCopy, staggerLWidth, staggerUWidth, staggerAlign, &
                   computationalLWidth, computationalUWidth, &
                   totalLWidth, totalUWidth,rc)
!
! !ARGUMENTS:
      type(ESMF_Grid),        intent(in)            :: grid
      type (ESMF_StaggerLoc), intent(in),  optional :: staggerLoc
      integer,                intent(in)            :: coord
      real(ESMF_KIND_R8),     intent(out), optional :: fptr(:,:)
      type(ESMF_CopyFlag),    intent(in),  optional :: docopy
      integer,                intent(in),  optional :: staggerLWidth(:)
      integer,                intent(in),  optional :: staggerUWidth(:)
      integer,                intent(in),  optional :: staggerAlign(:)
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
!\item[{[staggerLWidth]}] 
!    This array should be the same rank as the grid. It specifies the lower corner of the computational
!    region with respect to the lower corner of the exclusive region.
!\item[{[staggerUWidth]}] 
!    This array should be the same rank as the grid. It specifies the upper corner of the computational
!    region with respect to the lower corner of the exclusive region.
!\item[{[staggerAlign]}] 
!    This array is of size  grid rank.
!    For this stagger location, it specifies which element
!    has the same index value as the center. For example, 
!    for a 2D cell with corner stagger it specifies which 
!    of the 4 corners has the same index as the center. 
!    If this is set and staggerUWidth is not,
!    this determines the default array padding for a stagger. 
!    If not set, then this defaults to all negative. (e.g. 
!    The most negative part of the stagger in a cell is aligned with the 
!    center and the padding is all on the postive side.) 
!\item[{[computationalLWidth]}]
!    The lower boundary of the computatational region in reference to the exclusive region. 
!    If {\tt staggerLWidth} is present then the actual computational width
!    is the max on a dimension by dimension basis between {\tt staggerLWidth} and
!    {\tt computationalLWidth}.
!\item[{[computationalUWidth]}]
!    The lower boundary of the computatational region in reference to the exclusive region. 
!    If {\tt staggerUWidth} is present then the actual computational width
!    is the max on a dimension by dimension basis between {\tt staggerUWidth} and
!    {\tt computationalUWidth}.
!\item[{[totalLWidth]}]
!    The lower boundary of the computatational region in reference to the computational region. 
!    Note, the computational region includes the extra padding specified by {\tt ccordLWidth}.
!\item[{[totalUWidth]}]
!    The lower boundary of the computatational region in reference to the computational region. 
!    Note, the computational region includes the extra padding specified by {\tt staggerLWidth}.
!\item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!\end{description}
!
!EOPI

      end subroutine ESMF_GridSetCoordFromFptr2DR8


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetLocalTileCoord"
!BOPI
! !IROUTINE: ESMF_GridSetLocalTileCoord - Set the coordinates for a local tile from a Fortran array

! !INTERFACE:
      subroutine ESMF_GridSetLocalTileCoord2DR8(grid, staggerLoc, coord, tile, localDE, &
                             fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid),        intent(in)            :: grid
      type (ESMF_StaggerLoc), intent(in),optional   :: staggerLoc
      integer,                intent(in)            :: coord
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
!          the first DE on this processor. (localDE starts at 0)
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
!EOPI

      end subroutine ESMF_GridSetLocalTileCoord2DR8


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridMetricGet"
!BOPI
! !IROUTINE: ESMF_GridMetricGet - Get information about a particular metric. 

! !INTERFACE:
     subroutine ESMF_GridMetricGet(grid, name, staggerLoc, &
                        metricLWidth, metricUWidth, &
                        metricAlign, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid 
      character (len=*), intent(in) :: name
      type (ESMF_StaggerLoc), intent(out), optional  :: staggerLoc
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

      end function ESMF_GridMetricGet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetMetricIntoArray"
!BOPI
! !IROUTINE: ESMF_GridGetMetricIntoArray - Gets metric data from a grid and puts it into an Array.

! !INTERFACE:
  ! Private name; call using ESMF_GridGetMetric()
      subroutine ESMF_GridGetMetricIntoArray(grid, name, staggerLoc, &
                            array, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len=*), intent(in) :: name
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

      end subroutine ESMF_GridGetMetricIntoArray

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridLocalTileMetricGet"
!BOPI
! !IROUTINE: ESMF_GridLocalTileMetricGet - get various types of information about the part of some metric data which lies on this DE.

! !INTERFACE:
      subroutine ESMF_GridLocalTileMetricGet(grid, name, tile, coord, localDE, staggerLoc, &
          exclusiveLBound, exclusiveUBound, computationalLBound, &
          computationalUBound, totalLBound, totalUBound, &
          computationalLWidth, computationalUWidth, &
          totalLWidth, totalUWidth,rc)

!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
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

      end subroutine ESMF_GridLocalTileMetricGet


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSet3DCoordFromArray"

!BOPI
! !IROUTINE: ESMF_GridSetCoord - Set 3D coordinate values from ESMF Arrays

! !INTERFACE:
  ! Private name; call using ESMF_GridSetCoord()
     function ESMF_GridSet3DCoordFromArray(grid, staggerLoc, &
                        coord1, coord2, coord3, &
                        staggerLWidth, staggerUWidth, &
                        staggerAlign, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid),       intent(in)             :: grid 
      type (ESMF_StaggerLoc), intent(in)       :: staggerLoc
      type(ESMF_ARRAY), intent(in)            :: coord1(:), coord2(:)
      type(ESMF_ARRAY), intent(in)            :: coord3(:)
      integer,               intent(in),   optional  :: staggerLWidth(:)
      integer,               intent(in),   optional  :: staggerUWidth(:)
      integer,               intent(in),   optional  :: staggerAlign(:)
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
! \item[{[staggerLWidth]}] 
!      This array should be the same rank as the grid. It specifies the lower corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[staggerUWidth]}] 
!      This array should be the same rank as the grid. It specifies the upper corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[staggerAlign]}] 
!      This array is of size  grid rank.
!      For this stagger location, it specifies which element
!      has the same index value as the center. For example, 
!      for a 2D cell with corner stagger it specifies which 
!      of the 4 corners has the same index as the center. 
!      If this is set and staggerUWidth is not,
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

      end function ESMF_GridSet3DCoordFromArray

!------------------------------------------------------------------------------
!BOPI
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
!EOPI

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetCoordFptr"
!BOPI
! !IROUTINE: ESMF_GridSetCoord - Set coordinate values from R8 Fortran arrays 

! !INTERFACE:
  ! Private name; call using ESMF_GridSetCoord()
     function ESMF_GridSetCoordFptr(grid, staggerLoc, &
                        coord1, coord2, coord3, &
                        staggerLWidth, staggerUWidth, &
                        staggerAlign, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid),       intent(in)             :: grid 
      type (ESMF_StaggerLoc), intent(in)       :: staggerLoc
      real (ESMF_KIND_R8), intent(in)            :: coord1(:), coord2(:)
      real (ESMF_KIND_R8), intent(in)            :: coord3(:)
      integer,               intent(in),   optional  :: staggerLWidth(:)
      integer,               intent(in),   optional  :: staggerUWidth(:)
      integer,               intent(in),   optional  :: staggerAlign(:)
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
! \item[{[staggerLWidth]}] 
!      This array should be the same rank as the grid. It specifies the lower corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[staggerUWidth]}] 
!      This array should be the same rank as the grid. It specifies the upper corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[staggerAlign]}] 
!      This array is of size  grid rank.
!      For this stagger location, it specifies which element
!      has the same index value as the center. For example, 
!      for a 2D cell with corner stagger it specifies which 
!      of the 4 corners has the same index as the center. 
!      If this is set and staggerUWidth is not,
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

      end function ESMF_GridSetCoordFptr


!------------------------------------------------------------------------------


#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetInternalState"
!BOPI
! !IROUTINE: ESMF_GridSetInternalState - Set private data block pointer
!
! !INTERFACE:
!      subroutine ESMF_GridSetInternalState(gridcomp, dataPointer, rc)
!
! !ARGUMENTS:
!      type(ESMF_Grid), intent(inout) :: gridcomp
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
#define ESMF_METHOD "ESMF_GridSetMetricFromArray"
!BOPI
! !IROUTINE: ESMF_GridSetMetricFromArray - Add a new metric from an Array.

! !INTERFACE:
  ! Private name; call using ESMF_GridSetMetric()
      subroutine ESMF_GridSetMetricFromArray(grid, name, &
                            array, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len=*), intent(in) :: name
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

      end subroutine ESMF_GridSetMetricFromArray

!------------------------------------------------------------------------------
#endif

      end module ESMF_GridMod

