! $Id.F90,v 1.22 2007/09/05 18:31:55 oehmke Exp $
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
      use ESMF_UtilTypesMod   
      use ESMF_BaseMod          ! ESMF base class
      use ESMF_LogErrMod
      use ESMF_ArrayMod
      use ESMF_LocalArrayMod    ! ESMF local array class
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
  public ESMF_GridCreateEmpty
  public ESMF_GridCreateShapeTile

  public ESMF_GridDestroy

  public ESMF_GridGet
  public ESMF_GridGetCoord

  public ESMF_GridSet
  public ESMF_GridSetCoord

  public ESMF_GridSetShape
  public ESMF_GridValidate


! - ESMF-internal methods:
  public ESMF_GridGetInit  

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Grid.F90,v 1.29 2007/09/27 00:16:07 oehmke Exp $'

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
! !IROUTINE: ESMF_GridCreateShapeTile -- Generic interface

! !INTERFACE:
interface ESMF_GridCreateShapeTile

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_GridCreateShapeTileReg
      module procedure ESMF_GridCreateShapeTileIrreg
      
! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_GridCreateShapeTile} functions.   
!EOPI 
end interface


! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_GridGet -- Get information from a Grid

! !INTERFACE:
  interface ESMF_GridGet

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_GridGet
      module procedure ESMF_GridGetPLocalDePSloc
      
! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_GridGet} functions.   
!EOPI 
end interface

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_GridGetCoord -- Generic interface

! !INTERFACE:
interface ESMF_GridGetCoord

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_GridGetCoord1DR4
      module procedure ESMF_GridGetCoord2DR4
      module procedure ESMF_GridGetCoord3DR4
      module procedure ESMF_GridGetCoord1DR8
      module procedure ESMF_GridGetCoord2DR8
      module procedure ESMF_GridGetCoord3DR8
      module procedure ESMF_GridGetCoordBounds
      module procedure ESMF_GridGetCoordIntoArray
      
! !DESCRIPTION: 
! This interface provides a single entry point for the various 
! types of {\tt ESMF\_GridGetCoord} functions.   
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
     subroutine ESMF_GridAllocCoordNoValues(grid, staggerloc,  &
                staggerLWidth, staggerUWidth, staggerAlign, &
                computationalLWidth, computationalUWidth, &
                totalLWidth, totalUWidth,rc)

!
! !ARGUMENTS:
      type(ESMF_Grid),        intent(in)              :: grid 
      type (ESMF_StaggerLoc), intent(in),optional     :: staggerloc
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
! \item[{[staggerloc]}]
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
    if (present(staggerloc)) then
       tmp_staggerloc=staggerloc%staggerloc
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
!    allocate Fields based on Grids no matter what the status.)
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
! !IROUTINE: ESMF_GridCreate - Create a Grid from a DistGrid

! !INTERFACE:
  ! Private name; call using ESMF_GridCreate()
      function ESMF_GridCreateFromDistGrid(name,coordTypeKind,distgrid, dimmap, &
                        lbounds, ubounds, coordRank, coordDimMap, indexflag, rc)
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
!      List that has as many elements as indicated by distgrid's dimCount value.
!      The elements map each dimension of distgrid to a dimension in the grid.
!       (i.e. the values should range from 1 to gridrank). If not specified, the default
!       is to map all of distgrid's dimensions against the lower dimensions of the
!       grid in sequence. 
! \item[{[lbounds]}] 
!      Lower bounds for undistributed array dimensions. Must be the same size as {\tt ubounds}.
! \item[{[ubounds]}] 
!      Upper bounds for undistributed array dimensions. Must be the same size as {\tt lbounds}.
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

    !! undistributed bounds
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
      indexflag, localrc)
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
#define ESMF_METHOD "ESMF_GridCreateShapeTileIrreg"
!BOP
! !IROUTINE: ESMF_GridCreateShapeTile - Create a Grid with an irregular distribution

! !INTERFACE:
  ! Private name; call using ESMF_GridCreateShapeTile()
      function ESMF_GridCreateShapeTileIrreg(name,coordTypeKind, minIndex,  &
                        countsPerDEDim1,countsPerDeDim2, countsPerDEDim3, &
                        connDim1, connDim2, connDim3, &
                        poleStaggerLoc1, poleStaggerLoc2, poleStaggerLoc3, &
                        bipolePos1, bipolePos2, bipolePos3, &
                        coordDep1, coordDep2, coordDep3, &
                        indexflag, petMap, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateShapeTileIrreg
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
!      to /1,1,1,.../.
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
!     arrays map to. If not present the default is /1,2,3/. 
! \item[{[coordDep2]}] 
!     This array specifies the dependence of the second 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the second
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is /1,2,3/. 
! \item[{[coordDep3]}] 
!     This array specifies the dependence of the third 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the third
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is /1,2,3/. 
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
         minIndexDG(d)=minIndexLocal(3)
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
       ESMF_GridCreateShapeTileIrreg=ESMF_GridCreateFromDistGrid(name, coordTypeKind, &
                                    distgrid, dimmap, lbounds, ubounds, &
                                    coordRank, coordDimMap, indexflag, &
                                    localrc)
    else
       ESMF_GridCreateShapeTileIrreg=ESMF_GridCreateFromDistGrid(name, coordTypeKind, &
                                    distgrid=distgrid, dimmap=dimmap, &
                                    coordRank=coordRank, coordDimMap=coordDimMap, &
                                    indexflag=indexflag, &
                                    rc=localrc)
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
    end function ESMF_GridCreateShapeTileIrreg


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreateShapeTileReg"
!BOP
! !IROUTINE: ESMF_GridCreateShapeTile - Create a Grid with a regular distribution

! !INTERFACE:
  ! Private name; call using ESMF_GridCreateShapeTile()
      function ESMF_GridCreateShapeTileReg(name, coordTypeKind, &
                        regDecomp, decompFlag, minIndex, maxIndex, &
                        connDim1, connDim2, connDim3, &
                        poleStaggerLoc1, poleStaggerLoc2, poleStaggerLoc3, &
                        bipolePos1, bipolePos2, bipolePos3, &
                        coordDep1, coordDep2, coordDep3, &
                        indexflag, petMap, rc)


!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateShapeTileReg
!
! !ARGUMENTS:
       character (len=*), intent(in), optional :: name 
       type(ESMF_TypeKind),  intent(in),    optional  :: coordTypeKind
       integer,               intent(in),   optional  :: regDecomp(:)
       type(ESMF_DecompFlag), intent(in),   optional  :: decompflag(:)
       integer,               intent(in),   optional  :: minIndex(:)
       integer,               intent(in)              :: maxIndex(:)
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
       integer,               intent(in),   optional  :: petMap(:,:,:)
       integer,               intent(out),  optional  :: rc
!
! !DESCRIPTION:
!
! This method creates a single tile, regularly distributed grid 
! (see Figure \ref{fig:GridDecomps}).
! To specify the distribution, the user passes in an array 
! ({\tt regDecomp}) specifying the number of DEs to divide each 
! dimension into. If the number of DEs is 1 than the dimension is undistributed.
! The array {\tt decompFlag} indicates how the division into DEs is to
! occur.  The default is to divide the range as evenly as possible.
!
! If {\tt regDecomp} is not specified then the Grid is divided among the DEs
! along the first dimension and the rest of the dimensions are undistributed.  

! The arguments are:
! \begin{description}
! \item[{[name]}]
!      {\tt ESMF\_Grid} name.
! \item[{[coordTypeKind]}] 
!      The type/kind of the grid coordinate data. 
!      If not specified then the type/kind will be 8 byte reals. 
! \item[{[regDecomp]}] 
!      List that has the same number of elements as {\tt maxIndex}.
!      Each entry is the number of decounts for that dimension.
!      If not specified, the default decomposition will be petCountx1x1..x1. 
! \item[{[decompflag]}]
!      List of decomposition flags indicating how each dimension of the
!      patch is to be divided between the DEs. The default setting
!      is {\tt ESMF\_DECOMP\_HOMOGEN} in all dimensions. 
! \item[{[minIndex]}] 
!      The bottom extent of the grid array. If not given then the value defaults
!      to /1,1,1,.../.
! \item[{maxIndex}] 
!      The upper extent of the grid array.
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
!     arrays map to. If not present the default is /1,2,3/. 
! \item[{[coordDep2]}] 
!     This array specifies the dependence of the second 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the second
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is /1,2,3/. 
! \item[{[coordDep3]}] 
!     This array specifies the dependence of the third 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the third
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is /1,2,3/. 
! \item[{[indexflag]}]
!      Flag that indicates how the DE-local indices are to be defined.
! \item[{[petMap]}]
!       Sets the mapping of pets to the created DEs. This 3D
!       should be of size regDecomp(1) x regDecomp(2) x regDecomp(3)
!       If the Grid is 2D, then the last dimension is of size 1.   
!       If the Grid contains undistributed dimensions then these
!       should also be of size 1. 
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
    type(ESMF_DistGrid)  :: distgrid
    type(ESMF_DELayout)  :: delayout
    type(ESMF_VM)        :: vm
    integer, pointer     :: petList(:)
    integer, pointer     :: lbounds(:)
    integer, pointer     :: ubounds(:)
    integer, pointer     :: coordRank(:)
    integer, pointer     :: coordDimMap(:,:)
    integer              :: localrc
    integer              :: rank,i,distRank,undistRank,maxSizeDEDim
    integer, pointer     :: minIndexDG(:),maxIndexDG(:)
    integer, pointer     :: regDecompDG(:)
    type(ESMF_DecompFlag), pointer :: decompflagDG(:)
    integer, pointer     :: regDecompLocal(:)
    type(ESMF_DecompFlag), pointer :: decompflagLocal(:)
    integer, pointer     :: dimMap(:),minIndexLocal(:), deDimCount(:)
    integer              :: deCount
    integer              :: d,ud,i1,i2,i3,k
    type(ESMF_GridConn)  :: connDim1Local(2)
    type(ESMF_GridConn)  :: connDim2Local(2)
    type(ESMF_GridConn)  :: connDim3Local(2)
    integer              :: connCount, petListCount

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Compute the Grid Rank and Derivatives ---------------------------------------------------
    ! rank
    rank=size(maxIndex)
    if ((rank < 2) .or. (rank > 3)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- maxIndex size and thus Grid rank must be either 2 or 3 when using create shape ", & 
               ESMF_CONTEXT, rc) 
         return 
    endif

    ! rank of distributed part
    if (present(regDecomp)) then
       distRank=0 
       do i=1,size(regDecomp)
         if (regDecomp(i) .gt. 1) distRank=distRank+1
       enddo
    else
       distRank=1
    endif 

    ! ranks of the undistributed part of the grid
    undistRank=rank-distRank

    ! Argument Consistency Checking --------------------------------------------------------------
    if (present(regDecomp)) then
        if (size(regDecomp) .lt. rank) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
                    "- regDecomp size doesn't match Grid rank ", & 
                    ESMF_CONTEXT, rc) 
            return 
        endif
    endif

    if (present(decompFlag)) then
        if (size(decompFlag) .lt. rank) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
                    "- decompFlag size doesn't match Grid rank ", & 
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

    ! Set default for regDecomp 
    allocate(regDecompLocal(rank), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating regDecompLocal", &
                                     ESMF_CONTEXT, rc)) return

    if (present(regDecomp)) then
       regDecompLocal(:)=regDecomp(:)
    else
       ! The default is 1D divided among all the Pets
       call ESMF_VMGetGlobal(vm,rc=localrc)
       if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
       call ESMF_VMGet(vm,petCount=regDecompLocal(1),rc=localrc)
       if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
       do i=2,rank
          regDecompLocal(i)=1
       enddo
       
    endif

    ! Set default for decompFlag 
    allocate(decompFlagLocal(rank), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating decompFlagLocal", &
                                     ESMF_CONTEXT, rc)) return

    if (present(decompFlag)) then
       decompFlagLocal=decompFlag
    else
       decompFlagLocal=ESMF_DECOMP_HOMOGEN
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

  ! Further Error Checking which is easier after setting defaults ----------------------
  if (present(petMap)) then
     if (rank .gt. 2) then
          if ((size(petMap,1) .ne. regDecompLocal(1)) .or. &
              (size(petMap,2) .ne. regDecompLocal(2)) .or. &
              (size(petMap,3) .ne. regDecompLocal(3))) then
              call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
                     "- petMap wrong size in one or more dimensions", & 
                     ESMF_CONTEXT, rc) 
              return 
          endif
      else
          if ((size(petMap,1) .ne. regDecompLocal(1)) .or. &
              (size(petMap,2) .ne. regDecompLocal(2)) .or. &
              (size(petMap,3) .ne. 1)) then
              call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
                     "- petMap wrong size in one or more dimensions", & 
                     ESMF_CONTEXT, rc) 
              return 
          endif
      endif
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
   allocate(regDecompDG(distRank), stat=localrc)
   if (ESMF_LogMsgFoundAllocError(localrc, "Allocating dimMap", &
               ESMF_CONTEXT, rc)) return
   allocate(decompFlagDG(distRank), stat=localrc)
   if (ESMF_LogMsgFoundAllocError(localrc, "Allocating dimMap", &
               ESMF_CONTEXT, rc)) return
   allocate(lbounds(undistRank), stat=localrc)
   if (ESMF_LogMsgFoundAllocError(localrc, "Allocating lbounds", &
              ESMF_CONTEXT, rc)) return
   allocate(ubounds(undistRank), stat=localrc)
   if (ESMF_LogMsgFoundAllocError(localrc, "Allocating ubounds", &
              ESMF_CONTEXT, rc)) return     

   ! Fill in minIndex, maxIndex, dimMap, lbound, ubounds
   d=1
   ud=1
   if ((regDecompLocal(1) .gt. 1) .or. all(regDecompLocal .eq. 1)) then
      minIndexDG(d)=minIndexLocal(1)
      maxIndexDG(d)=maxIndex(1)
      dimMap(d)=1      
      regDecompDG(d)=regDecompLocal(1)
      decompFlagDG(d)=decompFlagLocal(1)
      d=d+1
   else
     lbounds(ud)=minIndexLocal(1)
     ubounds(ud)=maxIndex(1)
     ud=ud+1
   endif

   if (regDecompLocal(2) .gt. 1) then
      minIndexDG(d)=minIndexLocal(2)
      maxIndexDG(d)=maxIndex(2)
      dimMap(d)=2      
      regDecompDG(d)=regDecompLocal(2)
      decompFlagDG(d)=decompFlagLocal(2)
      d=d+1
   else
     lbounds(ud)=minIndexLocal(2)
     ubounds(ud)=maxIndex(2)
     ud=ud+1
   endif

   if (rank .gt. 2) then
      if (regDecompLocal(3) .gt. 1) then
         minIndexDG(d)=minIndexLocal(3)
         maxIndexDG(d)=maxIndex(3)
         dimMap(d)=3      
         regDecompDG(d)=regDecompLocal(3)
         decompFlagDG(d)=decompFlagLocal(3)
         d=d+1
       else
         lbounds(ud)=minIndexLocal(3)
         ubounds(ud)=maxIndex(3)
         ud=ud+1
       endif
   endif

   
   ! Setup Connections between patch sides ----------------------------------------

   ! CONNECTIONS DON'T WORK YET SO NOT IMPLEMENTED


   ! Process PetMap --------------------------------------------------------------
   !! Calculate deCount
   deCount=1
   do i=1,rank
      deCount=deCount*regDecompLocal(i)
   enddo

   ! create DELayout based on presence of petMap
   if (present(petMap)) then
      !! Allocate petList
      allocate(petList(deCount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating petList", &
              ESMF_CONTEXT, rc)) return


      !! copy petMap to petList
      if (rank .gt. 2) then
	 k=1
     	 do i3=1,regDecompLocal(3)
         do i2=1,regDecompLocal(2)
         do i1=1,regDecompLocal(1)
            petList(k)=petMap(i1,i2,i3)
            k=k+1
         enddo
         enddo
         enddo
      else 
	 k=1
     	 do i3=1,1
         do i2=1,regDecompLocal(2)
         do i1=1,regDecompLocal(1)
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
              regDecomp=regDecompDG, decompFlag=decompFlagDG, delayout=delayout, &
              indexflag=indexflag, rc=localrc)   
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return



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
       ESMF_GridCreateShapeTileReg=ESMF_GridCreateFromDistGrid(name, coordTypeKind, &
                                    distgrid, dimmap, lbounds, ubounds, &
                                    coordRank, coordDimMap, indexflag, &
                                    localrc)
    else
       ESMF_GridCreateShapeTileReg=ESMF_GridCreateFromDistGrid(name, coordTypeKind, &
                                    distgrid=distgrid, dimmap=dimmap, &
                                    coordRank=coordRank, coordDimMap=coordDimMap, &
                                    indexflag=indexflag, &
                                    rc=localrc)
    endif
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return



    ! Clean up memory
    deallocate(regDecompLocal)
    deallocate(decompFlagLocal)
    deallocate(regDecompDG)
    deallocate(decompFlagDG)
    deallocate(coordRank)
    deallocate(coordDimMap)
    deallocate(minIndexDG)
    deallocate(maxIndexDG)
    deallocate(dimMap)
    deallocate(lbounds)
    deallocate(ubounds)
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    end function ESMF_GridCreateShapeTileReg

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
      subroutine ESMF_GridGet(grid, name, coordTypeKind, &
          rank, distRank, undistRank,  &
          tileCount, staggerlocsCount, localDECount, distgrid, &
          dimmap, lbounds, ubounds, coordRank, coordDimMap, &
          indexFlag, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid),       intent(in)            :: grid
      character (len=*),     intent(out), optional :: name
      type(ESMF_TypeKind),   intent(out), optional :: coordTypeKind
      integer,               intent(out), optional :: rank
      integer,               intent(out), optional :: distRank
      integer,               intent(out), optional :: undistRank
      integer,               intent(out), optional :: tileCount
      integer,               intent(out), optional :: staggerlocsCount
      integer,               intent(out), optional :: localDECount
      type(ESMF_DistGrid),   intent(out), optional :: distgrid
      integer,               intent(out), optional :: dimmap(:)
      integer,               intent(out), optional :: lbounds(:)
      integer,               intent(out), optional :: ubounds(:)
      integer,               intent(out), optional :: coordRank(:)
      integer,               intent(out), optional :: coordDimMap(:,:)
      type(ESMF_IndexFlag),  intent(out), optional :: indexflag
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
!\item[{[distRank]}]
!   The rank of the distributed part of the grid. Should be equal to the distgrid's
!   dimCount. 
!\item[{[undistRank]}]
!   The rank of the undistributed part of the grid.
!\item[{[tileCount]}]
!   The number of logically rectangular tiles in the grid. 
!\item[{[staggerlocsCount]}]
!   The number of stagger locations.
!\item[{[localDECount]}]
!   The number of DEs in this grid on this PET.
!\item[{[distgrid]}]
!   The structure describing the distribution of the grid. 
!\item[{[dimmap]}]
!   List that has as many elements as the distgrid rank. This array describes
!   mapping between the grids dimensions and the distgrid.
!\item[{[lbounds]}] 
!   Lower bounds for undistributed array dimensions.
!\item[{[ubounds]}] 
!   Upper bounds for undistributed array dimensions. 
! \item[{[coordRank]}]
!   List that has as many elements as the grid rank (from arrayspec).
!   Gives the dimension of each component (e.g. x) array. This is 
!   to allow factorization of the coordinate arrays. If not specified
!   all arrays are the same size as the grid. 
!\item[{[coordDimMap]}]
!   2D list of size grid rank x grid rank. This array describes the
!   map of each component array's dimensions onto the grids
!   dimensions. 
! \item[{[indexflag]}]
!    Flag that indicates how the DE-local indices are to be defined.
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

    !! undistributed bounds
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
      coordTypeKind, rank, tileCount, distgrid,  staggerlocsCount, &
      dimmapArg, lboundsArg, uboundsArg, coordRankArg, coordDimMapArg, &
      indexflag, localDECount, distRank, undistRank, localrc)
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
#define ESMF_METHOD "ESMF_GridGetPLocalDePSloc"
!BOP
! !IROUTINE: ESMF_GridGet - Get Grid information for a specific stagger location

! !INTERFACE:
  ! Private name; call using ESMF_GridGet()
      subroutine ESMF_GridGetPLocalDePSloc(grid, localDe, staggerloc, &
          exclusiveLBound, exclusiveUBound, &
          staggerLBound, staggerUBound, rc)

!
! !ARGUMENTS:
      type(ESMF_Grid),        intent(in)            :: grid
      integer,                intent(in),  optional :: localDe
      type (ESMF_StaggerLoc), intent(in)            :: staggerloc
      integer,                intent(out), optional :: exclusiveLBound(:)
      integer,                intent(out), optional :: exclusiveUBound(:)
      integer,                intent(out), optional :: staggerLBound(:)
      integer,                intent(out), optional :: staggerUBound(:)
      integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!  This method gets information about the range of index space which a particular
!  stagger location occupies. This call differs from the coordinate bound calls
!  (e.g. {\tt ESMF\_GridGetCoord}) in that a given coordinate array may only occupy 
!  a subset of the Grid's dimensions, and
!  so these calls may not give all the bounds of the stagger location. The bounds from this call
!  are the full bounds, and so
!  for example, give the appropriate bounds for creating a data array on the stagger location.
!  Note that unlike the output from the Array, these values also include the undistributed dimensions and are
!  ordered to reflect the order of the indices in the Grid. This call will still give correct
!  values even if the stagger location does not contain coordinate arrays (e.g. if 
!  {\tt ESMF\_GridAllocCoord} hasn't yet been called on the stagger location).
!
!The arguments are:
!\begin{description}
!\item[{grid}]
!    Grid to get the information from.
!\item[{[localDe]}]
!     The local DE from which to get the information.  If not set, defaults to 
!     the first DE on this processor. (localDE starts at 0)
!\item[{staggerloc}]
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
!     {\tt staggerUBound} must be allocated to be of size equal to the coord rank.
!\item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!\end{description}
!
!EOP

    integer :: localrc ! local error status
    type(ESMF_InterfaceInt) :: exclusiveLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: exclusiveUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: staggerLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: staggerUBoundArg ! helper variable
    integer :: tmp_staggerloc

    ! Initialize return code
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_GridGetInit, grid, rc)
    tmp_staggerloc=staggerloc%staggerloc

    ! process optional arguments
    exclusiveLBoundArg=ESMF_InterfaceIntCreate(exclusiveLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    exclusiveUBoundArg=ESMF_InterfaceIntCreate(exclusiveUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    staggerLBoundArg=ESMF_InterfaceIntCreate(staggerLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    staggerUBoundArg=ESMF_InterfaceIntCreate(staggerUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments

    call c_ESMC_GridGetPLocalDePSloc(grid, localDE, tmp_staggerLoc, &
      exclusiveLBoundArg, exclusiveUBoundArg, staggerLBoundArg, staggerUBoundArg,&
      localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deallocate interface ints
    call ESMF_InterfaceIntDestroy(exclusiveLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(staggerLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(staggerUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetPLocalDePSloc


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridGetCoord - Get coordinates and put in a native Fortran array

! !INTERFACE:
!      subroutine ESMF_GridGetCoord(grid, localDE, coordDim, staggerloc, &
!         exclusiveLBound, exclusiveUBound, staggerLBound, staggerUBound, &
!         computationalLBound, computationalUBound, totalLBound, totalUBound, &
!         <pointer argument>, doCopy, rc)
!
! !ARGUMENTS:
!     type(ESMF_Grid),        intent(in) :: grid
!     integer,                intent(in), optional :: localDE
!     integer,                intent(in) :: coordDim
!     type (ESMF_StaggerLoc), intent(in), optional :: staggerloc
!     integer,                intent(out), optional :: exclusiveLBound(:)
!     integer,                intent(out), optional :: exclusiveUBound(:)
!     integer,                intent(out), optional :: staggerLBound(:)
!     integer,                intent(out), optional :: staggerUBound(:)
!     integer,                intent(out), optional :: computationalLBound(:)
!     integer,                intent(out), optional :: computationalUBound(:)
!     integer,                intent(out), optional :: totalLBound(:)
!     integer,                intent(out), optional :: totalUBound(:)
!     <pointer argument>, see below for supported values
!     type(ESMF_CopyFlag),    intent(in), optional :: docopy
!     integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!     This method gets a Fortran pointer to the piece of memory which holds the 
!     coordinate data on the local DE for the given coordinate dimension and stagger
!     locations. 
!     This is useful, for example, for setting the coordinate values in a Grid, or
!     for reading the coordinate values.  Currently this method supports up to three
!     coordinate dimensions, of either R4 or R8 datatype.  See below for specific 
!     supported values.  If the coordinates that you are trying to retrieve are of
!     higher dimension, use the {\tt ESMF\_GetCoord()} interface that returns coordinate
!     values in an {\tt ESMF\_Array} instead.  That interface supports the retrieval of
!     coordinates up to 7D. 
!
!     Supported values for the <pointer argument> are: 
!     \begin{description}
!     \item real(ESMF_KIND_R4), pointer :: fptr(:)
!     \item real(ESMF_KIND_R4), pointer :: fptr(:,:)     
!     \item real(ESMF_KIND_R4), pointer :: fptr(:,:,:)
!     \item real(ESMF_KIND_R8), pointer :: fptr(:)
!     \item real(ESMF_KIND_R8), pointer :: fptr(:,:)     
!     \item real(ESMF_KIND_R8), pointer :: fptr(:,:,:)
!
!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          Grid to get the information from.
!     \item[{[localDE]}]
!          The local DE to get the information for. If not set, defaults to 
!          the first DE on this processor. (localDE starts at 0)
!     \item[{coordDim}]
!          The coordinate dimension to get the data from (e.g. 1=x).
!     \item[{staggerloc}]
!          The stagger location to get the information for. 
!          Please see Section~\ref{sec:opt:staggerloc} for a list 
!          of predefined stagger locations. If not present, defaults to ESMF\_STAGGERLOC\_CENTER.
!     \item[{[exclusiveLBound]}]
!          Upon return this holds the lower bounds of the exclusive region.
!          {\tt exclusiveLBound} must be allocated to be of size equal to the coord rank.
!     \item[{[exclusiveUBound]}]
!          Upon return this holds the upper bounds of the exclusive region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the coord rank.
!     \item[{[staggerLBound]}]
!          Upon return this holds the lower bounds of the stagger region.
!          {\tt staggerLBound} must be allocated to be of size equal to the coord rank.
!     \item[{[staggerUBound]}]
!          Upon return this holds the upper bounds of the stagger region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the coord rank.
!     \item[{[computationalLBound]}]
!          Upon return this holds the lower bounds of the computational region. 
!          {\tt computationalLBound} must be allocated to be of size equal to the coord rank.
!     \item[{[computationalUBound]}]
!          Upon return this holds the upper bounds of the computational region.
!          {\tt computationalUBound} must be allocated to be of size equal to the coord rank.
!     \item[{[totalLBound]}]
!          Upon return this holds the lower bounds of the total region.
!          {\tt totalLBound} must be allocated to be of size equal to the coord rank.
!     \item[{[totalUBound]}]
!          Upon return this holds the upper bounds of the total region.
!          {\tt totalUBound} must be allocated to be of size equal to the coord rank.
!     \item[{fptr}]
!          The pointer to the coordinate data.
!     \item[{[doCopy]}]
!          If not specified, default to {\tt ESMF\_DATA\_REF}, in this case fptr is a reference to 
!          the data in the Grid coordinate arrays. Please see Section~\ref{opt:copyflag} for
!          further description and a list of other valid values. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetCoord1DR4"
!BOPI
! !IROUTINE: ESMF_GridGetCoord - Get pointer to 1DR4 coordinates

! !INTERFACE:
  ! Private name; call using ESMF_GridGetCoord()
      subroutine ESMF_GridGetCoord1DR4(grid, localDE, coordDim, staggerloc, & 
          exclusiveLBound, exclusiveUBound, staggerLBound, staggerUBound, &
          computationalLBound, computationalUBound, totalLBound, totalUBound, &
          fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid),        intent(in) :: grid
      integer,                intent(in), optional :: localDE
      integer,                intent(in) :: coordDim
      type (ESMF_StaggerLoc), intent(in), optional :: staggerloc
      integer,                intent(out), optional :: exclusiveLBound(:)
      integer,                intent(out), optional :: exclusiveUBound(:)
      integer,                intent(out), optional :: staggerLBound(:)
      integer,                intent(out), optional :: staggerUBound(:)
      integer,                intent(out), optional :: computationalLBound(:)
      integer,                intent(out), optional :: computationalUBound(:)
      integer,                intent(out), optional :: totalLBound(:)
      integer,                intent(out), optional :: totalUBound(:)
      real(ESMF_KIND_R4), pointer                   :: fptr(:)
      type(ESMF_CopyFlag),    intent(in), optional :: docopy
      integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!    This method gets a Fortran pointer to the piece of memory which holds the 
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
!     \item[{coordDim}]
!          The coordinate dimension to get the data from (e.g. 1=x).
!     \item[{staggerloc}]
!          The stagger location to get the information for. 
!          Please see Section~\ref{sec:opt:staggerloc} for a list 
!          of predefined stagger locations. If not present, defaults to ESMF\_STAGGERLOC\_CENTER.
!     \item[{[exclusiveLBound]}]
!          Upon return this holds the lower bounds of the exclusive region.
!          {\tt exclusiveLBound} must be allocated to be of size equal to the coord rank.
!     \item[{[exclusiveUBound]}]
!          Upon return this holds the upper bounds of the exclusive region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the coord rank.
!     \item[{[staggerLBound]}]
!          Upon return this holds the lower bounds of the stagger region.
!          {\tt staggerLBound} must be allocated to be of size equal to the coord rank.
!     \item[{[staggerUBound]}]
!          Upon return this holds the upper bounds of the stagger region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the coord rank.
!     \item[{[computationalLBound]}]
!          Upon return this holds the lower bounds of the computational region. 
!          {\tt computationalLBound} must be allocated to be of size equal to the coord rank.
!     \item[{[computationalUBound]}]
!          Upon return this holds the upper bounds of the computational region.
!          {\tt computationalUBound} must be allocated to be of size equal to the coord rank.
!     \item[{[totalLBound]}]
!          Upon return this holds the lower bounds of the total region.
!          {\tt totalLBound} must be allocated to be of size equal to the coord rank.
!     \item[{[totalUBound]}]
!          Upon return this holds the upper bounds of the total region.
!          {\tt totalUBound} must be allocated to be of size equal to the coord rank.
!     \item[{fptr}]
!          The pointer to the coordinate data.
!     \item[{[doCopy]}]
!          If not specified, default to {\tt ESMF\_DATA\_REF}, in this case fptr is a reference to 
!          the data in the Grid coordinate arrays. Please see Section~\ref{opt:copyflag} for
!          further description and a list of other valid values. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

    ! Local variables 
    type(ESMF_Array) :: array 
    integer :: localrc ! local error status 
    integer :: localDeCount, rank 
    type(ESMF_TypeKind) :: typekind 
    type(ESMF_LocalArray), allocatable :: larrayList(:) 
    type(ESMF_CopyFlag) :: docopyInt
    integer :: lDE
    integer :: coordRank(ESMF_MAXDIM)
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
    localrc = ESMF_RC_NOT_IMPL 
    if (present(rc)) rc = ESMF_RC_NOT_IMPL 

    ! Check init status of arguments 
    ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid, rc) 

    ! Check consistency 
    call ESMF_GridGet(grid, coordTypeKind=typekind, rank=rank, coordRank=coordRank, &
                      localDECount=localDECount, rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! Require farrayPtr typekind to match Grid typekind 
    if (typekind .ne. ESMF_TYPEKIND_R4) then 
      call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
        "- farrayPtr typekind does not match Grid typekind", & 
        ESMF_CONTEXT, rc) 
      return 
    endif 

    ! make sure coord is legitimate
    if ((coordDim .lt. 1) .or. (coordDim .gt. rank)) then
      call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
        "- coordinate dimension outside of range specified for this Grid", & 
        ESMF_CONTEXT, rc) 
      return 
    endif 

    ! Require farrayPtr rank to match coordinate rank 
    if (coordRank(coordDim) .ne. 1) then 
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
        "- negative number of localDeCount prohibits request", & 
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
        "- localDE too big", ESMF_CONTEXT, rc) 
      return 
    endif 

    if (lDE<0) then 
      call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
        "- localDE can't be less than 0", ESMF_CONTEXT, rc) 
      return 
    endif 

    ! handle staggerloc
    if (present(staggerloc)) then
       tmp_staggerloc=staggerloc%staggerloc
    else
       tmp_staggerloc=ESMF_STAGGERLOC_CENTER%staggerloc  ! default
    endif

    ! Get the Array 
    call ESMF_GridGetCoordIntoArray(grid, staggerloc, coordDim, array, &
                                    ESMF_DATA_REF, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return

    ! Obtain the native F90 array pointer via the LocalArray interface 
    allocate(larrayList(localDeCount))
 
    call ESMF_ArrayGet(array, larrayList=larrayList, rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    call ESMF_LocalArrayGetData(larrayList(lDE+1), fptr, doCopy, rc=localrc) 
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
      ESMF_CONTEXT, rcToReturn=rc)) return 
    deallocate(larrayList) 

    ! process optional arguments
    exclusiveLBoundArg=ESMF_InterfaceIntCreate(exclusiveLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    exclusiveUBoundArg=ESMF_InterfaceIntCreate(exclusiveUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    staggerLBoundArg=ESMF_InterfaceIntCreate(staggerLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    staggerUBoundArg=ESMF_InterfaceIntCreate(staggerUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalLBoundArg=ESMF_InterfaceIntCreate(computationalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalUBoundArg=ESMF_InterfaceIntCreate(computationalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalLBoundArg=ESMF_InterfaceIntCreate(totalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalUBoundArg = ESMF_InterfaceIntCreate(totalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_GridGetCoordBounds(grid, localDE, coordDim, tmp_staggerloc, &
      exclusiveLBoundArg, exclusiveUBoundArg, staggerLBoundArg, staggerUBoundArg,&
      computationalLBoundArg, computationalUBoundArg, &
      totalLBoundArg, totalUBoundArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deallocate interface ints
    call ESMF_InterfaceIntDestroy(exclusiveLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(staggerLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(staggerUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully 
    if (present(rc)) rc = ESMF_SUCCESS 

    end subroutine ESMF_GridGetCoord1DR4


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetCoord2DR4"
!BOPI
! !IROUTINE: ESMF_GridGetCoord - Get pointer to 2DR4 coordinates

! !INTERFACE:
  ! Private name; call using ESMF_GridGetCoord()
      subroutine ESMF_GridGetCoord2DR4(grid, localDE, coordDim, staggerloc, &
          exclusiveLBound, exclusiveUBound, staggerLBound, staggerUBound, &
          computationalLBound, computationalUBound, totalLBound, totalUBound, &
          fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in),optional :: localDE
      integer, intent(in) :: coordDim
      type (ESMF_StaggerLoc), intent(in),optional :: staggerloc
      integer,                intent(out), optional :: exclusiveLBound(:)
      integer,                intent(out), optional :: exclusiveUBound(:)
      integer,                intent(out), optional :: staggerLBound(:)
      integer,                intent(out), optional :: staggerUBound(:)
      integer,                intent(out), optional :: computationalLBound(:)
      integer,                intent(out), optional :: computationalUBound(:)
      integer,                intent(out), optional :: totalLBound(:)
      integer,                intent(out), optional :: totalUBound(:)
      real(ESMF_KIND_R4), pointer :: fptr(:,:)
      type(ESMF_CopyFlag), intent(in), optional :: docopy
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    This method gets a Fortran pointer to the piece of memory which holds the 
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
!     \item[{coordDim}]
!          The coordinate dimension to get the data from (e.g. 1=x).
!     \item[{staggerloc}]
!          The stagger location to get the information for. 
!          Please see Section~\ref{sec:opt:staggerloc} for a list 
!          of predefined stagger locations. If not present, defaults to
!          ESMF\_STAGGERLOC\_CENTER.
!     \item[{[exclusiveLBound]}]
!          Upon return this holds the lower bounds of the exclusive region.
!          {\tt exclusiveLBound} must be allocated to be of size equal to the coord rank.
!     \item[{[exclusiveUBound]}]
!          Upon return this holds the upper bounds of the exclusive region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the coord rank.
!     \item[{[staggerLBound]}]
!          Upon return this holds the lower bounds of the stagger region.
!          {\tt staggerLBound} must be allocated to be of size equal to the coord rank.
!     \item[{[staggerUBound]}]
!          Upon return this holds the upper bounds of the stagger region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the coord rank.
!     \item[{[computationalLBound]}]
!          Upon return this holds the lower bounds of the computational region. 
!          {\tt computationalLBound} must be allocated to be of size equal to the coord rank.
!     \item[{[computationalUBound]}]
!          Upon return this holds the upper bounds of the computational region.
!          {\tt computationalUBound} must be allocated to be of size equal to the coord rank.
!     \item[{[totalLBound]}]
!          Upon return this holds the lower bounds of the total region.
!          {\tt totalLBound} must be allocated to be of size equal to the coord rank.
!     \item[{[totalUBound]}]
!          Upon return this holds the upper bounds of the total region.
!          {\tt totalUBound} must be allocated to be of size equal to the coord rank.
!     \item[{fptr}]
!          The pointer to the coordinate data.
!     \item[{[doCopy]}]
!          If not specified, default to {\tt ESMF\_DATA\_REF}, in this case fptr is a reference to 
!          the data in the Grid coordinate arrays. Please see Section~\ref{opt:copyflag} for
!          further description and a list of other valid values. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI


    ! Local variables 
    type(ESMF_Array) :: array 
    integer :: localrc ! local error status 
    integer :: localDeCount, rank 
    type(ESMF_TypeKind) :: typekind 
    type(ESMF_LocalArray), allocatable :: larrayList(:) 
    type(ESMF_CopyFlag) :: docopyInt
    integer :: lDE
    integer :: coordRank(ESMF_MAXDIM)
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
    localrc = ESMF_RC_NOT_IMPL 
    if (present(rc)) rc = ESMF_RC_NOT_IMPL 

    ! Check init status of arguments 
    ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid, rc) 

    ! Check consistency 
    call ESMF_GridGet(grid, coordTypeKind=typekind, rank=rank, coordRank=coordRank, &
                      localDECount=localDECount, rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! Require farrayPtr typekind to match Grid typekind 
    if (typekind .ne. ESMF_TYPEKIND_R4) then 
    call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
      "- farrayPtr typekind does not match Grid typekind", & 
      ESMF_CONTEXT, rc) 
    return 
    endif 

    ! make sure coord is legitimate
    if ((coordDim .lt. 1) .or. (coordDim .gt. rank)) then
      call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
        "- coordinate dimension outside of range specified for this Grid", & 
        ESMF_CONTEXT, rc) 
      return 
    endif 

    ! Require farrayPtr rank to match coordinate rank 
    if (coordRank(coordDim) .ne. 2) then 
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
        "- localDE too big", ESMF_CONTEXT, rc) 
      return 
    endif 

    if (lDE<0) then 
      call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
        "- localDE can't be less than 0", & 
        ESMF_CONTEXT, rc) 
      return 
    endif 


    ! handle staggerloc
    if (present(staggerloc)) then
       tmp_staggerloc=staggerloc%staggerloc
    else
       tmp_staggerloc=ESMF_STAGGERLOC_CENTER%staggerloc  ! default
    endif

    ! Get the Array 

    call ESMF_GridGetCoordIntoArray(grid, staggerloc,coordDim, array, &
                                    ESMF_DATA_REF, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return

    ! Obtain the native F90 array pointer via the LocalArray interface 
    allocate(larrayList(localDeCount))
 
    call ESMF_ArrayGet(array, larrayList=larrayList, rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return
 
    call ESMF_LocalArrayGetData(larrayList(lDE+1), fptr, doCopy, rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return 
    deallocate(larrayList) 

    ! process optional arguments
    exclusiveLBoundArg=ESMF_InterfaceIntCreate(exclusiveLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    exclusiveUBoundArg=ESMF_InterfaceIntCreate(exclusiveUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    staggerLBoundArg=ESMF_InterfaceIntCreate(staggerLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    staggerUBoundArg=ESMF_InterfaceIntCreate(staggerUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalLBoundArg=ESMF_InterfaceIntCreate(computationalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalUBoundArg=ESMF_InterfaceIntCreate(computationalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalLBoundArg=ESMF_InterfaceIntCreate(totalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalUBoundArg = ESMF_InterfaceIntCreate(totalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_GridGetCoordBounds(grid, localDE, coordDim, tmp_staggerloc, &
      exclusiveLBoundArg, exclusiveUBoundArg, staggerLBoundArg, staggerUBoundArg,&
      computationalLBoundArg, computationalUBoundArg, &
      totalLBoundArg, totalUBoundArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deallocate interface ints
    call ESMF_InterfaceIntDestroy(exclusiveLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(staggerLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(staggerUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully 
    if (present(rc)) rc = ESMF_SUCCESS 

    end subroutine ESMF_GridGetCoord2DR4


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetCoord3DR4"
!BOPI
! !IROUTINE: ESMF_GridGetCoord - Get pointer to 3DR4 coordinates

! !INTERFACE:
  ! Private name; call using ESMF_GridGetCoord()
      subroutine ESMF_GridGetCoord3DR4(grid, localDE, coordDim, staggerloc, &
          exclusiveLBound, exclusiveUBound, staggerLBound, staggerUBound, &
          computationalLBound, computationalUBound, totalLBound, totalUBound, &      
          fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in),optional :: localDE
      integer, intent(in) :: coordDim
      type (ESMF_StaggerLoc), intent(in),  optional :: staggerloc
      integer,                intent(out), optional :: exclusiveLBound(:)
      integer,                intent(out), optional :: exclusiveUBound(:)
      integer,                intent(out), optional :: staggerLBound(:)
      integer,                intent(out), optional :: staggerUBound(:)
      integer,                intent(out), optional :: computationalLBound(:)
      integer,                intent(out), optional :: computationalUBound(:)
      integer,                intent(out), optional :: totalLBound(:)
      integer,                intent(out), optional :: totalUBound(:)
      real(ESMF_KIND_R4), pointer :: fptr(:,:,:)
      type(ESMF_CopyFlag), intent(in), optional :: docopy
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    This method gets a Fortran pointer to the piece of memory which holds the 
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
!     \item[{coordDim}]
!          The coordinate dimension to get the data from (e.g. 1=x).
!     \item[{staggerloc}]
!          The stagger location to get the information for. 
!          Please see Section~\ref{sec:opt:staggerloc} for a list 
!          of predefined stagger locations. If not present, defaults to
!          ESMF\_STAGGERLOC\_CENTER.
!     \item[{[exclusiveLBound]}]
!          Upon return this holds the lower bounds of the exclusive region.
!          {\tt exclusiveLBound} must be allocated to be of size equal to the coord rank.
!     \item[{[exclusiveUBound]}]
!          Upon return this holds the upper bounds of the exclusive region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the coord rank.
!     \item[{[staggerLBound]}]
!          Upon return this holds the lower bounds of the stagger region.
!          {\tt staggerLBound} must be allocated to be of size equal to the coord rank.
!     \item[{[staggerUBound]}]
!          Upon return this holds the upper bounds of the stagger region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the coord rank.
!     \item[{[computationalLBound]}]
!          Upon return this holds the lower bounds of the computational region. 
!          {\tt computationalLBound} must be allocated to be of size equal to the coord rank.
!     \item[{[computationalUBound]}]
!          Upon return this holds the upper bounds of the computational region.
!          {\tt computationalUBound} must be allocated to be of size equal to the coord rank.
!     \item[{[totalLBound]}]
!          Upon return this holds the lower bounds of the total region.
!          {\tt totalLBound} must be allocated to be of size equal to the coord rank.
!     \item[{[totalUBound]}]
!          Upon return this holds the upper bounds of the total region.
!          {\tt totalUBound} must be allocated to be of size equal to the coord rank.
!     \item[{fptr}]
!          The pointer to the coordinate data.
!     \item[{[doCopy]}]
!          If not specified, default to {\tt ESMF\_DATA\_REF}, in this case fptr is a reference to 
!          the data in the Grid coordinate arrays. Please see Section~\ref{opt:copyflag} for
!          further description and a list of other valid values. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI


 ! Local variables 
 type(ESMF_Array) :: array 
 integer :: localrc ! local error status 
 integer :: localDeCount, rank 
 type(ESMF_TypeKind) :: typekind 
 type(ESMF_LocalArray), allocatable :: larrayList(:) 
 type(ESMF_CopyFlag) :: docopyInt
 integer :: lDE
 integer :: coordRank(ESMF_MAXDIM)
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
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid, rc) 

 ! Check consistency 
 call ESMF_GridGet(grid, coordTypeKind=typekind, rank=rank, coordRank=coordRank, &
                   localDECount=localDECount, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return
 
 ! Require farrayPtr typekind to match Grid typekind 
 if (typekind .ne. ESMF_TYPEKIND_R4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- farrayPtr typekind does not match Grid typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

! make sure coord is legitimate
if ((coordDim .lt. 1) .or. (coordDim .gt. rank)) then
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- coordinate dimension outside of range specified for this Grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

 ! Require farrayPtr rank to match coordinate rank 
 if (coordRank(coordDim) .ne. 3) then 
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


    ! handle staggerloc
    if (present(staggerloc)) then
       tmp_staggerloc=staggerloc%staggerloc
    else
       tmp_staggerloc=ESMF_STAGGERLOC_CENTER%staggerloc  ! default
    endif

    ! Get the Array 

    call ESMF_GridGetCoordIntoArray(grid, staggerloc, coordDim, array, &
                                    ESMF_DATA_REF, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return

    ! Obtain the native F90 array pointer via the LocalArray interface 
    allocate(larrayList(localDeCount))
 
    call ESMF_ArrayGet(array, larrayList=larrayList, rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return
 
    call ESMF_LocalArrayGetData(larrayList(lDE+1), fptr, doCopy, rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return 
    deallocate(larrayList) 


    ! process optional arguments
    exclusiveLBoundArg=ESMF_InterfaceIntCreate(exclusiveLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    exclusiveUBoundArg=ESMF_InterfaceIntCreate(exclusiveUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    staggerLBoundArg=ESMF_InterfaceIntCreate(staggerLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    staggerUBoundArg=ESMF_InterfaceIntCreate(staggerUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalLBoundArg=ESMF_InterfaceIntCreate(computationalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalUBoundArg=ESMF_InterfaceIntCreate(computationalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalLBoundArg=ESMF_InterfaceIntCreate(totalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalUBoundArg = ESMF_InterfaceIntCreate(totalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_GridGetCoordBounds(grid, localDE, coordDim, tmp_staggerloc, &
      exclusiveLBoundArg, exclusiveUBoundArg, staggerLBoundArg, staggerUBoundArg,&
      computationalLBoundArg, computationalUBoundArg, &
      totalLBoundArg, totalUBoundArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deallocate interface ints
    call ESMF_InterfaceIntDestroy(exclusiveLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(staggerLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(staggerUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully 
    if (present(rc)) rc = ESMF_SUCCESS 

    end subroutine ESMF_GridGetCoord3DR4


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetCoord1DR8"
!BOPI
! !IROUTINE: ESMF_GridGetCoord - Get pointer to 1DR8 coordinates

! !INTERFACE:
  ! Private name; call using ESMF_GridGetCoord()
      subroutine ESMF_GridGetCoord1DR8(grid, localDE, coordDim, staggerloc, & 
          exclusiveLBound, exclusiveUBound, staggerLBound, staggerUBound, &
          computationalLBound, computationalUBound, totalLBound, totalUBound, &
          fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in),optional :: localDE
      integer, intent(in) :: coordDim
      type (ESMF_StaggerLoc), intent(in),optional :: staggerloc
      integer,                intent(out), optional :: exclusiveLBound(:)
      integer,                intent(out), optional :: exclusiveUBound(:)
      integer,                intent(out), optional :: staggerLBound(:)
      integer,                intent(out), optional :: staggerUBound(:)
      integer,                intent(out), optional :: computationalLBound(:)
      integer,                intent(out), optional :: computationalUBound(:)
      integer,                intent(out), optional :: totalLBound(:)
      integer,                intent(out), optional :: totalUBound(:)
      real(ESMF_KIND_R8), pointer :: fptr(:)
      type(ESMF_CopyFlag), intent(in), optional :: docopy
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    This method gets a Fortran pointer to the piece of memory which holds the 
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
!     \item[{coordDim}]
!          The coordinate dimension to get the data from (e.g. 1=x).
!     \item[{staggerloc}]
!          The stagger location to get the information for. 
!          Please see Section~\ref{sec:opt:staggerloc} for a list 
!          of predefined stagger locations. If not present, defaults to
!          ESMF\_STAGGERLOC\_CENTER.
!     \item[{[exclusiveLBound]}]
!          Upon return this holds the lower bounds of the exclusive region.
!          {\tt exclusiveLBound} must be allocated to be of size equal to the coord rank.
!     \item[{[exclusiveUBound]}]
!          Upon return this holds the upper bounds of the exclusive region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the coord rank.
!     \item[{[staggerLBound]}]
!          Upon return this holds the lower bounds of the stagger region.
!          {\tt staggerLBound} must be allocated to be of size equal to the coord rank.
!     \item[{[staggerUBound]}]
!          Upon return this holds the upper bounds of the stagger region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the coord rank.
!     \item[{[computationalLBound]}]
!          Upon return this holds the lower bounds of the computational region. 
!          {\tt computationalLBound} must be allocated to be of size equal to the coord rank.
!     \item[{[computationalUBound]}]
!          Upon return this holds the upper bounds of the computational region.
!          {\tt computationalUBound} must be allocated to be of size equal to the coord rank.
!     \item[{[totalLBound]}]
!          Upon return this holds the lower bounds of the total region.
!          {\tt totalLBound} must be allocated to be of size equal to the coord rank.
!     \item[{[totalUBound]}]
!          Upon return this holds the upper bounds of the total region.
!          {\tt totalUBound} must be allocated to be of size equal to the coord rank.
!     \item[{fptr}]
!          The pointer to the coordinate data.
!     \item[{[doCopy]}]
!          If not specified, default to {\tt ESMF\_DATA\_REF}, in this case fptr is a reference to 
!          the data in the Grid coordinate arrays. Please see Section~\ref{opt:copyflag} for
!          further description and a list of other valid values. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI


 ! Local variables 
 type(ESMF_Array) :: array 
 integer :: localrc ! local error status 
 integer :: localDeCount, rank 
 type(ESMF_TypeKind) :: typekind 
 type(ESMF_LocalArray), allocatable :: larrayList(:) 
 type(ESMF_CopyFlag) :: docopyInt
 integer :: lDE
 integer :: coordRank(ESMF_MAXDIM)
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
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid, rc) 

 ! Check consistency 
 call ESMF_GridGet(grid, coordTypeKind=typekind, rank=rank, coordRank=coordRank, &
                   localDECount=localDECount, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return
 
 ! Require farrayPtr typekind to match Grid typekind 
 if (typekind .ne. ESMF_TYPEKIND_R8) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- farrayPtr typekind does not match Grid typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

! make sure coord is legitimate
if ((coordDim .lt. 1) .or. (coordDim .gt. rank)) then
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- coordinate dimension outside of range specified for this Grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

 ! Require farrayPtr rank to match coordinate rank 
 if (coordRank(coordDim) .ne. 1) then 
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

    ! handle staggerloc
    if (present(staggerloc)) then
       tmp_staggerloc=staggerloc%staggerloc
    else
       tmp_staggerloc=ESMF_STAGGERLOC_CENTER%staggerloc  ! default
    endif


    ! Get the Array 
    call ESMF_GridGetCoordIntoArray(grid, staggerloc, coordDim, array, &
                                    ESMF_DATA_REF, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return

    ! Obtain the native F90 array pointer via the LocalArray interface 
    allocate(larrayList(localDeCount))
 
    call ESMF_ArrayGet(array, larrayList=larrayList, rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return
 
    call ESMF_LocalArrayGetData(larrayList(lDE+1), fptr, doCopy, rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return 
    deallocate(larrayList) 


    ! process optional arguments
    exclusiveLBoundArg=ESMF_InterfaceIntCreate(exclusiveLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    exclusiveUBoundArg=ESMF_InterfaceIntCreate(exclusiveUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    staggerLBoundArg=ESMF_InterfaceIntCreate(staggerLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    staggerUBoundArg=ESMF_InterfaceIntCreate(staggerUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalLBoundArg=ESMF_InterfaceIntCreate(computationalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalUBoundArg=ESMF_InterfaceIntCreate(computationalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalLBoundArg=ESMF_InterfaceIntCreate(totalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalUBoundArg = ESMF_InterfaceIntCreate(totalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_GridGetCoordBounds(grid, localDE, coordDim, tmp_staggerloc, &
      exclusiveLBoundArg, exclusiveUBoundArg, staggerLBoundArg, staggerUBoundArg,&
      computationalLBoundArg, computationalUBoundArg, &
      totalLBoundArg, totalUBoundArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deallocate interface ints
    call ESMF_InterfaceIntDestroy(exclusiveLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(staggerLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(staggerUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully 
    if (present(rc)) rc = ESMF_SUCCESS 

    end subroutine ESMF_GridGetCoord1DR8


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetCoord2DR8"
!BOPI
! !IROUTINE: ESMF_GridGetCoord - Get pointer to 2DR8 coordinates

! !INTERFACE:
  ! Private name; call using ESMF_GridGetCoord()
      subroutine ESMF_GridGetCoord2DR8(grid, localDE, coordDim, staggerloc, & 
          exclusiveLBound, exclusiveUBound, staggerLBound, staggerUBound, &
          computationalLBound, computationalUBound, totalLBound, totalUBound, &
          fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in),optional :: localDE
      integer, intent(in) :: coordDim
      type (ESMF_StaggerLoc), intent(in),optional :: staggerloc
      integer,                intent(out), optional :: exclusiveLBound(:)
      integer,                intent(out), optional :: exclusiveUBound(:)
      integer,                intent(out), optional :: staggerLBound(:)
      integer,                intent(out), optional :: staggerUBound(:)
      integer,                intent(out), optional :: computationalLBound(:)
      integer,                intent(out), optional :: computationalUBound(:)
      integer,                intent(out), optional :: totalLBound(:)
      integer,                intent(out), optional :: totalUBound(:)
      real(ESMF_KIND_R8), pointer :: fptr(:,:)
      type(ESMF_CopyFlag), intent(in), optional :: docopy
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    This method gets a Fortran pointer to the piece of memory which holds the 
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
!     \item[{coordDim}]
!          The coordinate dimension to get the data from (e.g. 1=x).
!     \item[{staggerloc}]
!          The stagger location to get the information for. 
!          Please see Section~\ref{sec:opt:staggerloc} for a list 
!          of predefined stagger locations. If not present, defaults to
!          ESMF\_STAGGERLOC\_CENTER.
!     \item[{[exclusiveLBound]}]
!          Upon return this holds the lower bounds of the exclusive region.
!          {\tt exclusiveLBound} must be allocated to be of size equal to the coord rank.
!     \item[{[exclusiveUBound]}]
!          Upon return this holds the upper bounds of the exclusive region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the coord rank.
!     \item[{[staggerLBound]}]
!          Upon return this holds the lower bounds of the stagger region.
!          {\tt staggerLBound} must be allocated to be of size equal to the coord rank.
!     \item[{[staggerUBound]}]
!          Upon return this holds the upper bounds of the stagger region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the coord rank.
!     \item[{[computationalLBound]}]
!          Upon return this holds the lower bounds of the computational region. 
!          {\tt computationalLBound} must be allocated to be of size equal to the coord rank.
!     \item[{[computationalUBound]}]
!          Upon return this holds the upper bounds of the computational region.
!          {\tt computationalUBound} must be allocated to be of size equal to the coord rank.
!     \item[{[totalLBound]}]
!          Upon return this holds the lower bounds of the total region.
!          {\tt totalLBound} must be allocated to be of size equal to the coord rank.
!     \item[{[totalUBound]}]
!          Upon return this holds the upper bounds of the total region.
!          {\tt totalUBound} must be allocated to be of size equal to the coord rank.
!     \item[{fptr}]
!          The pointer to the coordinate data.
!     \item[{[doCopy]}]
!          If not specified, default to {\tt ESMF\_DATA\_REF}, in this case fptr is a reference to 
!          the data in the Grid coordinate arrays. Please see Section~\ref{opt:copyflag} for
!          further description and a list of other valid values. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI


 ! Local variables 
 type(ESMF_Array) :: array 
 integer :: localrc ! local error status 
 integer :: localDeCount, rank 
 type(ESMF_TypeKind) :: typekind 
 type(ESMF_LocalArray), allocatable :: larrayList(:) 
 type(ESMF_CopyFlag) :: docopyInt
 integer :: lDE
 integer :: coordRank(ESMF_MAXDIM)
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
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid, rc) 

 ! Check consistency 
 call ESMF_GridGet(grid, coordTypeKind=typekind, rank=rank, coordRank=coordRank, &
                   localDECount=localDECount, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return
 
 ! Require farrayPtr typekind to match Grid typekind 
 if (typekind .ne. ESMF_TYPEKIND_R8) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- farrayPtr typekind does not match Grid typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

! make sure coord is legitimate
if ((coordDim .lt. 1) .or. (coordDim .gt. rank)) then
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- coordinate dimension outside of range specified for this Grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

 ! Require farrayPtr rank to match coordinate rank 
 if (coordRank(coordDim) .ne. 2) then 
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

    ! handle staggerloc
    if (present(staggerloc)) then
       tmp_staggerloc=staggerloc%staggerloc
    else
       tmp_staggerloc=ESMF_STAGGERLOC_CENTER%staggerloc  ! default
    endif

    ! Get the Array 
    call ESMF_GridGetCoordIntoArray(grid, staggerloc,coordDim, array, &
                                    ESMF_DATA_REF, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return

    ! Obtain the native F90 array pointer via the LocalArray interface 
    allocate(larrayList(localDeCount))
 
    call ESMF_ArrayGet(array, larrayList=larrayList, rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return
 
    call ESMF_LocalArrayGetData(larrayList(lDE+1), fptr, doCopy, rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return 
    deallocate(larrayList) 


    ! process optional arguments
    exclusiveLBoundArg=ESMF_InterfaceIntCreate(exclusiveLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    exclusiveUBoundArg=ESMF_InterfaceIntCreate(exclusiveUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    staggerLBoundArg=ESMF_InterfaceIntCreate(staggerLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    staggerUBoundArg=ESMF_InterfaceIntCreate(staggerUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalLBoundArg=ESMF_InterfaceIntCreate(computationalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalUBoundArg=ESMF_InterfaceIntCreate(computationalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalLBoundArg=ESMF_InterfaceIntCreate(totalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalUBoundArg = ESMF_InterfaceIntCreate(totalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_GridGetCoordBounds(grid, localDE, coordDim, tmp_staggerloc, &
      exclusiveLBoundArg, exclusiveUBoundArg, staggerLBoundArg, staggerUBoundArg,&
      computationalLBoundArg, computationalUBoundArg, &
      totalLBoundArg, totalUBoundArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deallocate interface ints
    call ESMF_InterfaceIntDestroy(exclusiveLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(staggerLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(staggerUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully 
    if (present(rc)) rc = ESMF_SUCCESS 

    end subroutine ESMF_GridGetCoord2DR8


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetCoord3DR8"
!BOPI
! !IROUTINE: ESMF_GridGetCoord - Get pointer to 3DR8 coordinates

! !INTERFACE:
  ! Private name; call using ESMF_GridGetCoord()
      subroutine ESMF_GridGetCoord3DR8(grid, localDE, coordDim, staggerloc, & 
          exclusiveLBound, exclusiveUBound, staggerLBound, staggerUBound, &
          computationalLBound, computationalUBound, totalLBound, totalUBound, &
          fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in),optional :: localDE
      integer, intent(in) :: coordDim
      type (ESMF_StaggerLoc), intent(in),optional :: staggerloc
      integer,                intent(out), optional :: exclusiveLBound(:)
      integer,                intent(out), optional :: exclusiveUBound(:)
      integer,                intent(out), optional :: staggerLBound(:)
      integer,                intent(out), optional :: staggerUBound(:)
      integer,                intent(out), optional :: computationalLBound(:)
      integer,                intent(out), optional :: computationalUBound(:)
      integer,                intent(out), optional :: totalLBound(:)
      integer,                intent(out), optional :: totalUBound(:)
      real(ESMF_KIND_R8), pointer :: fptr(:,:,:)
      type(ESMF_CopyFlag), intent(in), optional :: docopy
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    This method gets a Fortran pointer to the piece of memory which holds the 
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
!     \item[{coordDim}]
!          The coordinate dimension to get the data from (e.g. 1=x).
!     \item[{staggerloc}]
!          The stagger location to get the information for. 
!          Please see Section~\ref{sec:opt:staggerloc} for a list 
!          of predefined stagger locations. If not present, defaults to
!          ESMF\_STAGGERLOC\_CENTER.
!     \item[{[exclusiveLBound]}]
!          Upon return this holds the lower bounds of the exclusive region.
!          {\tt exclusiveLBound} must be allocated to be of size equal to the coord rank.
!     \item[{[exclusiveUBound]}]
!          Upon return this holds the upper bounds of the exclusive region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the coord rank.
!     \item[{[staggerLBound]}]
!          Upon return this holds the lower bounds of the stagger region.
!          {\tt staggerLBound} must be allocated to be of size equal to the coord rank.
!     \item[{[staggerUBound]}]
!          Upon return this holds the upper bounds of the stagger region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the coord rank.
!     \item[{[computationalLBound]}]
!          Upon return this holds the lower bounds of the computational region. 
!          {\tt computationalLBound} must be allocated to be of size equal to the coord rank.
!     \item[{[computationalUBound]}]
!          Upon return this holds the upper bounds of the computational region.
!          {\tt computationalUBound} must be allocated to be of size equal to the coord rank.
!     \item[{[totalLBound]}]
!          Upon return this holds the lower bounds of the total region.
!          {\tt totalLBound} must be allocated to be of size equal to the coord rank.
!     \item[{[totalUBound]}]
!          Upon return this holds the upper bounds of the total region.
!          {\tt totalUBound} must be allocated to be of size equal to the coord rank.
!     \item[{fptr}]
!          The pointer to the coordinate data.
!     \item[{[doCopy]}]
!          If not specified, default to {\tt ESMF\_DATA\_REF}, in this case fptr is a reference to 
!          the data in the Grid coordinate arrays. Please see Section~\ref{opt:copyflag} for
!          further description and a list of other valid values. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI


 ! Local variables 
 type(ESMF_Array) :: array 
 integer :: localrc ! local error status 
 integer :: localDeCount, rank 
 type(ESMF_TypeKind) :: typekind 
 type(ESMF_LocalArray), allocatable :: larrayList(:) 
 type(ESMF_CopyFlag) :: docopyInt
 integer :: lDE
 integer :: coordRank(ESMF_MAXDIM)
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
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid, rc) 

 ! Check consistency 
 call ESMF_GridGet(grid, coordTypeKind=typekind, rank=rank, coordRank=coordRank, &
                   localDECount=localDECount, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return
 
 ! Require farrayPtr typekind to match Grid typekind 
 if (typekind .ne. ESMF_TYPEKIND_R8) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- farrayPtr typekind does not match Grid typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

! make sure coord is legitimate
if ((coordDim .lt. 1) .or. (coordDim .gt. rank)) then
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- coordinate dimension outside of range specified for this Grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

 ! Require farrayPtr rank to match coordinate rank 
 if (coordRank(coordDim) .ne. 3) then 
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

    ! handle staggerloc
    if (present(staggerloc)) then
       tmp_staggerloc=staggerloc%staggerloc
    else
       tmp_staggerloc=ESMF_STAGGERLOC_CENTER%staggerloc  ! default
    endif


    ! Get the Array 
    call ESMF_GridGetCoordIntoArray(grid, staggerloc, coordDim, array, &
                                    ESMF_DATA_REF, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return

    ! Obtain the native F90 array pointer via the LocalArray interface 
    allocate(larrayList(localDeCount))
 
    call ESMF_ArrayGet(array, larrayList=larrayList, rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return
 
    call ESMF_LocalArrayGetData(larrayList(lDE+1), fptr, doCopy, rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return 
    deallocate(larrayList) 


    ! process optional arguments
    exclusiveLBoundArg=ESMF_InterfaceIntCreate(exclusiveLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    exclusiveUBoundArg=ESMF_InterfaceIntCreate(exclusiveUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    staggerLBoundArg=ESMF_InterfaceIntCreate(staggerLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    staggerUBoundArg=ESMF_InterfaceIntCreate(staggerUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalLBoundArg=ESMF_InterfaceIntCreate(computationalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalUBoundArg=ESMF_InterfaceIntCreate(computationalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalLBoundArg=ESMF_InterfaceIntCreate(totalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalUBoundArg = ESMF_InterfaceIntCreate(totalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_GridGetCoordBounds(grid, localDE, coordDim, tmp_staggerloc, &
      exclusiveLBoundArg, exclusiveUBoundArg, staggerLBoundArg, staggerUBoundArg,&
      computationalLBoundArg, computationalUBoundArg, &
      totalLBoundArg, totalUBoundArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deallocate interface ints
    call ESMF_InterfaceIntDestroy(exclusiveLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(staggerLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(staggerUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully 
    if (present(rc)) rc = ESMF_SUCCESS 

    end subroutine ESMF_GridGetCoord3DR8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetCoordBounds"
!BOP
! !IROUTINE: ESMF_GridGetCoord - Get coordinate bounds

! !INTERFACE:
  ! Private name; call using ESMF_GridGetCoord()
      subroutine ESMF_GridGetCoordBounds(grid, localDE, coordDim, staggerloc, &
          exclusiveLBound, exclusiveUBound, staggerLBound, staggerUBound, &
          computationalLBound, computationalUBound, totalLBound, totalUBound, rc)

!
! !ARGUMENTS:
      type(ESMF_Grid),        intent(in)            :: grid
      integer,                intent(in),  optional :: localDE
      integer,                intent(in)            :: coordDim
      type (ESMF_StaggerLoc), intent(in),  optional :: staggerloc
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
!  piece of coordinate data occupies.  In other words, this method returns the 
!  bounds of the coordinate arrays.  Note that unlike the output from the 
!  Array, these values also include the undistributed dimensions and are
!  ordered to reflect the order of the indices in the coordinate. So, for example,
!  {\tt totalLBound} and {\tt totalUBound} should match the bounds of the Fortran array
!  retrieved by {\tt ESMF\_GridGetCoord}. 
!
!The arguments are:
!\begin{description}
!\item[{grid}]
!    Grid to get the information from.
!\item[{[localDE]}]
!     The local DE from which to get the information.  If not set, defaults to 
!     the first DE on this processor. (localDE starts at 0)
!\item[{coordDim}]
!     The coordinate dimension to get the information for (e.g. 1=x). 
!\item[{staggerloc}]
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

    integer :: localrc ! local error status
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
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid, rc)

    ! handle staggerloc
    if (present(staggerloc)) then
       tmp_staggerloc=staggerloc%staggerloc
    else
       tmp_staggerloc=ESMF_STAGGERLOC_CENTER%staggerloc  ! default
    endif

    ! process optional arguments
    exclusiveLBoundArg=ESMF_InterfaceIntCreate(exclusiveLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    exclusiveUBoundArg=ESMF_InterfaceIntCreate(exclusiveUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    staggerLBoundArg=ESMF_InterfaceIntCreate(staggerLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    staggerUBoundArg=ESMF_InterfaceIntCreate(staggerUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalLBoundArg=ESMF_InterfaceIntCreate(computationalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalUBoundArg=ESMF_InterfaceIntCreate(computationalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalLBoundArg=ESMF_InterfaceIntCreate(totalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalUBoundArg = ESMF_InterfaceIntCreate(totalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_GridGetCoordBounds(grid, localDE, coordDim, tmp_staggerloc, &
      exclusiveLBoundArg, exclusiveUBoundArg, staggerLBoundArg, staggerUBoundArg,&
      computationalLBoundArg, computationalUBoundArg, &
      totalLBoundArg, totalUBoundArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deallocate interface ints
    call ESMF_InterfaceIntDestroy(exclusiveLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(staggerLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(staggerUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetCoordBounds


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetCoordIntoArray"
!BOP
! !IROUTINE: ESMF_GridGetCoord - Get coordinates and put in an ESMF Array

! !INTERFACE:
  ! Private name; call using ESMF_GridGetCoord()
      subroutine ESMF_GridGetCoordIntoArray(grid, staggerloc,coordDim, array, &
                            docopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type (ESMF_StaggerLoc), intent(in),optional  :: staggerloc
      integer, intent(in)  :: coordDim
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
!     \item[{staggerloc}]
!          The stagger location from which to get the arrays. 
!          Please see Section~\ref{sec:opt:staggerloc} for a list 
!          of predefined stagger locations. If not present, defaults to ESMF\_STAGGERLOC\_CENTER.
!     \item[{coordDim}]
!          The coordinate dimension to get the data from (e.g. 1=x).
!     \item[{array}]
!          An array into which to put the coordinate infomation. 
!     \item[{[doCopy]}]
!          If not specified, default to {\tt ESMF\_DATA\_REF}, in this case {\tt array} is a reference to 
!          the Grid coordinate Arrays. Please see Section~\ref{opt:copyflag} for
!          further description and a list of other valid values. 
!          [THE ESMF\_DATA\_COPY OPTION IS CURRENTLY NOT IMPLEMENTED] 
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
    if (present(staggerloc)) then
       tmp_staggerloc=staggerloc%staggerloc
    else
       tmp_staggerloc=ESMF_STAGGERLOC_CENTER%staggerloc
    endif

    ! Call C++ Subroutine to do the create
    call c_ESMC_gridgetcoordintoarray(grid%this,tmp_staggerloc, coordDim, &
      array, docopy, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


    ! Set Array as created
    call ESMF_ArraySetInitCreated(array,localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetCoordIntoArray

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetFromDistGrid"
!BOP
! !IROUTINE: ESMF_GridSet - Set the values in a Grid which has been created with CreateEmpty 

! !INTERFACE:
  ! Private name; call using ESMF_GridSet()
    subroutine ESMF_GridSetFromDistGrid(grid,name,coordTypeKind,distgrid, &
                 dimmap, lbounds, ubounds, coordRank, coordDimMap, &
                 indexflag, rc)
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
!      List that has as many elements as indicated by distgrid's dimCount value.
!      The elements map each dimension of distgrid to a dimension in the grid.
!       (i.e. the values should range from 1 to gridrank). If not specified, the default
!       is to map all of distgrid's dimensions against the lower dimensions of the
!       grid in sequence. 
! \item[{[lbounds]}] 
!      Lower bounds for undistributed array dimensions.
! \item[{[ubounds]}] 
!      Upper bounds for undistributed array dimensions.
! \item[{[indexflag]}]
!      Flag that indicates how the DE-local indices are to be defined.
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

    !! undistributed bounds
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
      indexflag, localrc)
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
      subroutine ESMF_GridSetCoordFromArray(grid, staggerloc, coordDim, &
                            array, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid),        intent(in)            :: grid
      type (ESMF_StaggerLoc), intent(in), optional  :: staggerloc
      integer,                intent(in)            :: coordDim
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
!\item[{staggerloc}]
!    The stagger location into which to copy the arrays. 
!    Please see Section~\ref{sec:opt:staggerloc} for a list 
!    of predefined stagger locations. If not present, defaults to
!    ESMF\_STAGGERLOC\_CENTER.
!\item[{coordDim}]
!    The coordinate dimension to put the data in (e.g. 1=x).
!\item[{array}]
!    An array to set the grid coordinate information from.
!\item[{[doCopy]}]
!    If not specified, default to {\tt ESMF\_DATA\_REF}, in this case the Grid coordinate
!    Array is a reference to {\tt array}. Please see Section~\ref{opt:copyflag} for
!    further description and a list of other valid values. 
!    [THE ESMF\_DATA\_COPY OPTION IS CURRENTLY NOT IMPLEMENTED] 
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
    if (present(staggerloc)) then
       tmp_staggerloc=staggerloc%staggerloc
    else
       tmp_staggerloc=ESMF_STAGGERLOC_CENTER%staggerloc
    endif

    ! Call C++ Subroutine to do the create
    call c_ESMC_gridsetcoordfromarray(grid%this,tmp_staggerloc, coordDim, &
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
                        indexflag, petMap, rc)

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
!      to /1,1,1,.../.
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
!     arrays map to. If not present the default is /1,2,3/. 
! \item[{[coordDep2]}] 
!     This array specifies the dependence of the second 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the second
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is /1,2,3/. 
! \item[{[coordDep3]}] 
!     This array specifies the dependence of the third 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the third
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is /1,2,3/. 
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
         minIndexDG(d)=minIndexLocal(3)
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
                                    localrc)
    else
       call ESMF_GridSetFromDistGrid(grid, name, coordTypeKind, &
                                    distgrid=distgrid, dimmap=dimmap, &
                                    coordRank=coordRank, coordDimMap=coordDimMap, &
                                    indexflag=indexflag, &
                                    rc=localrc)
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


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridValidate()"
!BOP
! !IROUTINE: ESMF_GridValidate - Validate Grid internals

! !INTERFACE:
  subroutine ESMF_GridValidate(grid, rc)
!
! !ARGUMENTS:
    type(ESMF_Grid), intent(in)              :: grid
    integer,         intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Validates that the {\tt Grid} is internally consistent.
!      Note that one of the checks that the Grid validate does
!      is the Grid status. Currently, the validate will return
!      an error if the grid is not at least 
!      {\tt ESMF\_GRIDSTATUS\_SHAPE\_READY}. This means 
!      if a Grid was created with {\tt ESMF\_GridCreateEmpty}
!      it must also have been commited with {\tt ESMF\_GridCommit}
!      to be valid. If a Grid was created with another create
!      call it should automatically have the correct status level
!      to pass the status part of the validate. 
!      The Grid validate at this time doesn't check for the presence
!      or consistency of the Grid coordinates.  
!      The method returns an error code if problems are found.  
!
!     The arguments are:
!     \begin{description}
!     \item[grid] 
!          Specified {\tt ESMF\_Grid} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_GridValidate(grid, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_GridValidate
!------------------------------------------------------------------------------

! -------------------------- ESMF-internal method -----------------------------
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

      end module ESMF_GridMod

