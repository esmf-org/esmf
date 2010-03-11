! $Id.F90,v 1.22 2007/09/05 18:31:55 oehmke Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research,
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
      use ESMF_ArraySpecMod

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

  type(ESMF_GridStatus), parameter :: &
                      ESMF_GRIDSTATUS_INVALID=ESMF_GridStatus(-1), &
                      ESMF_GRIDSTATUS_UNINIT=ESMF_GridStatus(0), &
                      ESMF_GRIDSTATUS_NOT_READY=ESMF_GridStatus(1), &
		      ESMF_GRIDSTATUS_SHAPE_READY=ESMF_GridStatus(2)


!------------------------------------------------------------------------------
! ! ESMF_GridItem
!
!------------------------------------------------------------------------------
  type ESMF_GridItem
  sequence
!  private
     integer :: gridItem
  end type

  type(ESMF_GridItem), parameter :: &
                      ESMF_GRIDITEM_INVALID=ESMF_GridItem(-2), &
                      ESMF_GRIDITEM_UNINIT=ESMF_GridItem(-1), &
                      ESMF_GRIDITEM_MASK=ESMF_GridItem(0), &
                      ESMF_GRIDITEM_AREA=ESMF_GridItem(1), &
                      ESMF_GRIDITEM_AREAM=ESMF_GridItem(2), &
                      ESMF_GRIDITEM_FRAC=ESMF_GridItem(3)



!------------------------------------------------------------------------------
! ! ESMF_GridConn
!
!------------------------------------------------------------------------------
  type ESMF_GridConn
  sequence
!  private
     integer :: gridconn
  end type

  type(ESMF_GridConn), parameter :: &
    ESMF_GRIDCONN_NONE = ESMF_GridConn(0), &
    ESMF_GRIDCONN_PERIODIC = ESMF_GridConn(1), &
    ESMF_GRIDCONN_POLE = ESMF_GridConn(2), &
    ESMF_GRIDCONN_BIPOLE = ESMF_GridConn(3)

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
! ! ESMF_GridDecompType
!
!------------------------------------------------------------------------------
  type ESMF_GridDecompType
  sequence
!  private
     integer :: griddecomptype
  end type

  type (ESMF_GridDecompType), parameter :: &
	ESMF_GRID_INVALID = ESMF_GridDecompType(1), &
	ESMF_GRID_NONARBITRARY = ESMF_GridDecompType(2), &
        ESMF_GRID_ARBITRARY = ESMF_GridDecompType(3)

!------------------------------------------------------------------------------
! ! Special dimenaion for Arbitrarily distributed dimension
!
!------------------------------------------------------------------------------
integer,parameter :: ESMF_GRID_ARBDIM = -1
!------------------------------------------------------------------------------
!
! !PUBLIC TYPES:
!
public ESMF_Grid
public  ESMF_GridConn,  ESMF_GRIDCONN_NONE, ESMF_GRIDCONN_PERIODIC, &
                        ESMF_GRIDCONN_POLE, ESMF_GRIDCONN_BIPOLE
public  ESMF_GridStatus,  ESMF_GRIDSTATUS_INVALID, ESMF_GRIDSTATUS_UNINIT, &
                      ESMF_GRIDSTATUS_NOT_READY,  ESMF_GRIDSTATUS_SHAPE_READY
public  ESMF_GridItem,  ESMF_GRIDITEM_INVALID, ESMF_GRIDITEM_UNINIT, &
                      ESMF_GRIDITEM_MASK, ESMF_GRIDITEM_AREA, &
                      ESMF_GRIDITEM_AREAM, ESMF_GRIDITEM_FRAC
public  ESMF_DefaultFlag
public  ESMF_GridDecompType, ESMF_GRID_INVALID, ESMF_GRID_NONARBITRARY, ESMF_GRID_ARBITRARY

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
!
! - ESMF-public methods:
  public ESMF_GridAddCoord
  public ESMF_GridCommit

  public ESMF_GridCreate
  public ESMF_GridCreateEmpty
  public ESMF_GridCreateShapeTile

  public ESMF_GridDestroy

  public ESMF_GridGet
  public ESMF_GridGetCoord
  public ESMF_GridGetStatus

  public ESMF_GridGetIndCoord ! HOPEFULLY TEMPORARY SEPARATE INTERFACE
  public ESMF_GridGetDecompType
  
  public ESMF_GridSet
  public ESMF_GridSetCoord

  public ESMF_GridAddItem
  public ESMF_GridGetItem
  public ESMF_GridSetItem

  public ESMF_GridSetCommitShapeTile
  public ESMF_GridSerialize
  public ESMF_GridDeserialize

  public ESMF_GridMatch

  public ESMF_GridValidate

!  public ESMF_GridTest ! For debugging 

  public ESMF_GridConvertIndex      ! For Arbitrarily distributed grid only

  public operator(.eq.), operator(.ne.) 
  public operator(.gt.), operator(.ge.)  
  public ESMF_ArrayCreateFromGrid
  public ESMF_GridGetArrayInfo

  public ESMF_GRID_ARBDIM

! - ESMF-internal methods:
  public ESMF_GridGetInit  

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Grid.F90,v 1.144.2.2 2010/03/11 17:54:44 oehmke Exp $'
!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================


! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_GridAddCoord -- Generic interface

! !INTERFACE:
interface ESMF_GridAddCoord

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_GridAddCoordNoValues
!      module procedure ESMF_GridAddCoordArrayList  ! Currently not public
      
! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_GridAddCoord} functions.   
!EOPI 
end interface


! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_GridAddItem -- Generic interface

! !INTERFACE:
interface ESMF_GridAddItem

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_GridAddItemNoValues
      
! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_GridAddItem} functions.   
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
      module procedure ESMF_GridCreateFromDistGridArb
      module procedure ESMF_GridCreateFromFile

      
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
      module procedure ESMF_GridCreateShapeTileArb
      
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
      module procedure ESMF_GridGetDefault
      module procedure ESMF_GridGetPLocalDePSloc
      module procedure ESMF_GridGetPSloc
      module procedure ESMF_GridGetIndex
      
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
!     module procedure ESMF_GridGetCoordR8
      
! !DESCRIPTION: 
! This interface provides a single entry point for the various 
! types of {\tt ESMF\_GridGetCoord} functions.   
!EOPI 
end interface

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_GridGetItem -- Generic interface

! !INTERFACE:
interface ESMF_GridGetItem

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_GridGetItem1DI4
      module procedure ESMF_GridGetItem2DI4
      module procedure ESMF_GridGetItem3DI4
      module procedure ESMF_GridGetItem1DR4
      module procedure ESMF_GridGetItem2DR4
      module procedure ESMF_GridGetItem3DR4
      module procedure ESMF_GridGetItem1DR8
      module procedure ESMF_GridGetItem2DR8
      module procedure ESMF_GridGetItem3DR8
      module procedure ESMF_GridGetItemBounds
      module procedure ESMF_GridGetItemIntoArray
      
! !DESCRIPTION: 
! This interface provides a single entry point for the various 
! types of {\tt ESMF\_GridGetItem} functions.   
!EOPI 
end interface


! -------------------------- ESMF-public method -------------------------------
!TODO: Temporary until I work out the proper overloading
!BOPI
! !IROUTINE: ESMF_GridGetIndCoord -- Generic interface

! !INTERFACE:
interface ESMF_GridGetIndCoord

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_GridGetCoordR4
      module procedure ESMF_GridGetCoordR8
      
! !DESCRIPTION: 
! This interface provides a single entry point for the various 
! types of {\tt ESMF\_GridGetIndCoord} functions.   
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
! !IROUTINE: ESMF_GridSetItem -- Generic interface

! !INTERFACE:
interface ESMF_GridSetItem

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_GridSetItemFromArray

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_GridSetItem} functions.   
!EOPI 
end interface


! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_GridSetCommitShapeTile -- Generic interface

! !INTERFACE:
interface ESMF_GridSetCommitShapeTile

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_GridSetCmmitShapeTileReg
      module procedure ESMF_GridSetCmmitShapeTileIrreg
      module procedure ESMF_GridSetCmmitShapeTileArb

      
! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_GridSetCommitShapeTile} functions.   
!EOPI 
end interface
!==============================================================================
!BOPI
! !INTERFACE:
      interface operator (.eq.)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_GridConnEqual

! !DESCRIPTION:
!     This interface overloads the equality operator for the specific
!     ESMF GridConn.  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface operator (.ne.)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_GridConnNotEqual

! !DESCRIPTION:
!     This interface overloads the inequality operator for the specific
!     ESMF GridConn.  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface
!!==============================================================================
!BOPI
! !INTERFACE:
      interface operator (.eq.)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_GridDecompEqual

! !DESCRIPTION:
!     This interface overloads the equality operator for the specific
!     ESMF_GridDecompType.  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface operator (.ne.)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_GridDecompNotEqual

! !DESCRIPTION:
!     This interface overloads the inequality operator for the specific
!     ESMF_GridDecompType.  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface operator (.eq.)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_GridStatusEqual

! !DESCRIPTION:
!     This interface overloads the equality operator for the specific
!     ESMF GridStatus.  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface operator (.ne.)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_GridStatusNotEqual

! !DESCRIPTION:
!     This interface overloads the inequality operator for the specific
!     ESMF GridStatus.  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface operator (.gt.)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_GridStatusGreater

! !DESCRIPTION:
!     This interface overloads the inequality operator for the specific
!     ESMF GridStatus.  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface operator (.lt.)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_GridStatusLess

! !DESCRIPTION:
!     This interface overloads the inequality operator for the specific
!     ESMF GridStatus.  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface operator (.ge.)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_GridStatusGreaterEqual

! !DESCRIPTION:
!     This interface overloads the inequality operator for the specific
!     ESMF GridStatus.  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface operator (.le.)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_GridStatusLessEqual

! !DESCRIPTION:
!     This interface overloads the inequality operator for the specific
!     ESMF GridStatus.  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface
!
!==============================================================================

      contains

!==============================================================================


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAddCoord"

!BOP
! !IROUTINE: ESMF_GridAddCoord - Allocate coordinate arrays but don't set their values

! !INTERFACE:
  ! Private name; call using ESMF_GridAddCoord()
     subroutine ESMF_GridAddCoordNoValues(grid, staggerloc,  &
                staggerEdgeLWidth, staggerEdgeUWidth, staggerAlign, &
                staggerMemLBound, totalLWidth, totalUWidth,rc)

!
! !ARGUMENTS:
      type(ESMF_Grid),        intent(in)              :: grid 
      type (ESMF_StaggerLoc), intent(in),optional     :: staggerloc
      integer,                intent(in),optional     :: staggerEdgeLWidth(:)
      integer,                intent(in),optional     :: staggerEdgeUWidth(:)
      integer,                intent(in),optional     :: staggerAlign(:)
      integer,                intent(in),optional     :: staggerMemLBound(:)      
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
!  remaining options {\tt staggerEdgeLWidth}, etc. allow the user to adjust the 
!  padding on the coordinate arrays.
!
! The arguments are:
! \begin{description}
!     \item[{grid}]
!       Grid to allocate coordinate storage in.  
! \item[{[staggerloc]}]
!      The stagger location to add. Please see Section~\ref{sec:opt:staggerloc} for a list 
!      of predefined stagger locations. If not present, defaults to ESMF\_STAGGERLOC\_CENTER.
! \item[{[staggerEdgeLWidth]}] 
!      This array should be the same dimCount as the grid. It specifies the lower corner of the stagger
!      region with respect to the lower corner of the exclusive region.
! \item[{[staggerEdgeUWidth]}] 
!      This array should be the same dimCount as the grid. It specifies the upper corner of the stagger
!      region with respect to the upper corner of the exclusive region.
! \item[{[staggerAlign]}] 
!      This array is of size  grid dimCount.
!      For this stagger location, it specifies which element
!      has the same index value as the center. For example, 
!      for a 2D cell with corner stagger it specifies which 
!      of the 4 corners has the same index as the center. 
!      If this is set and either staggerEdgeUWidth or staggerEdgeLWidth is not,
!      this determines the default array padding for a stagger. 
!      If not set, then this defaults to all negative. (e.g. 
!      The most negative part of the stagger in a cell is aligned with the 
!      center and the padding is all on the postive side.) 
! \item[{[staggerMemLBound]}] 
!      Specifies the lower index range of the memory of every DE in this staggerloc in this Grid. 
!      Only used when Grid indexflag is {\tt ESMF\_INDEX\_USER}. 
!\item[{[totalLWidth]}]
!     The lower boundary of the computatational region in reference to the computational region. 
!     Note, the computational region includes the extra padding specified by {\tt ccordLWidth}.
!     [CURRENTLY NOT IMPLEMENTED]
!\item[{[totalUWidth]}]
!     The lower boundary of the computatational region in reference to the computational region. 
!     Note, the computational region includes the extra padding specified by {\tt staggerEdgeLWidth}.
!     [CURRENTLY NOT IMPLEMENTED]
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP

    integer :: tmp_staggerloc
    integer :: localrc ! local error status
    type(ESMF_GridDecompType) :: decompType ! Arbitrary or not
    type(ESMF_InterfaceInt) :: staggerEdgeLWidthArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: staggerEdgeUWidthArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: staggerAlignArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: staggerMemLBoundArg  ! Language Interface Helper Var

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_GridGetInit, grid, rc)

    ! Get Grid decomposition type
    call ESMF_GridGetDecompType(grid, decompType, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
	  
    ! check for not implemented parameters
    if (present(totalLWidth)) then
       totalLWidth = 0
       call ESMF_LogMsgSetError(ESMF_RC_NOT_IMPL, & 
                 "- totalLWidth specification not yet implemented", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if (present(totalUWidth)) then
       totalUWidth = 0
       call ESMF_LogMsgSetError(ESMF_RC_NOT_IMPL, & 
                 "- totalUWidth specification not yet implemented", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    ! handle staggerloc
    if (present(staggerloc)) then
	 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. &
 	     (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
                 ESMF_CONTEXT, rc) 
           return
	 else
	    tmp_staggerloc=staggerloc%staggerloc
	 endif
    else
       tmp_staggerloc=ESMF_STAGGERLOC_CENTER%staggerloc
    endif

    if (decompType .eq. ESMF_GRID_ARBITRARY) then
	if (present(staggerEdgeLWidth) .or. present(staggerEdgeUWidth) .or. &
	    present(staggerAlign)) then
	    call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- stagger arguments should not be set for arbitrary grid", & 
                 ESMF_CONTEXT, rc) 
	    return
	else
	    ! Call C++ Subroutine to do the create
    	    call c_ESMC_gridaddcoordarb(grid%this,tmp_staggerloc, localrc)
    	    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      		ESMF_CONTEXT, rcToReturn=rc)) return
	endif
     else
        !! staggerEdgeLWidth
    	staggerEdgeLWidthArg = ESMF_InterfaceIntCreate(staggerEdgeLWidth, rc=localrc)
    	if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      	  ESMF_CONTEXT, rcToReturn=rc)) return

    	!! staggerEdgeUWidth
    	staggerEdgeUWidthArg = ESMF_InterfaceIntCreate(staggerEdgeUWidth, rc=localrc)
    	if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      	   ESMF_CONTEXT, rcToReturn=rc)) return

    	!! staggerAlign
    	staggerAlignArg = ESMF_InterfaceIntCreate(staggerAlign, rc=localrc)
    	if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    	  ESMF_CONTEXT, rcToReturn=rc)) return

        !! staggerMemLBound
        staggerMemLBoundArg = ESMF_InterfaceIntCreate(staggerMemLBound, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

    	! Call C++ Subroutine to do the create
    	call c_ESMC_gridaddcoord(grid%this,tmp_staggerloc, &
          staggerEdgeLWidthArg, staggerEdgeUWidthArg, staggerAlignArg, staggerMemLBoundArg, localrc)
    	if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

    	! Deallocate helper variables
    	call ESMF_InterfaceIntDestroy(staggerEdgeLWidthArg, rc=localrc)
    	if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    	  ESMF_CONTEXT, rcToReturn=rc)) return

    	call ESMF_InterfaceIntDestroy(staggerEdgeUWidthArg, rc=localrc)
    	if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    	  ESMF_CONTEXT, rcToReturn=rc)) return

    	call ESMF_InterfaceIntDestroy(staggerAlignArg, rc=localrc)
    	  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      	    ESMF_CONTEXT, rcToReturn=rc)) return
            
        call ESMF_InterfaceIntDestroy(staggerMemLBoundArg, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     endif

     if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAddCoordNoValues

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAddCoord"
!BOPI
! !IROUTINE: ESMF_GridAddCoord - Set coordinates using array of ESMF Arrays

! !INTERFACE:
  ! Private name; call using ESMF_GridAddCoord()
      subroutine ESMF_GridAddCoordArrayList(grid, staggerloc, &
                   arrayList, doCopy, staggerEdgeLWidth,   &
                   staggerEdgeUWidth, staggerAlign, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid),        intent(in)            :: grid
      type (ESMF_StaggerLoc), intent(in), optional  :: staggerloc
      type(ESMF_Array),       intent(in)            :: arrayList(:)
      type(ESMF_CopyFlag),    intent(in), optional  :: docopy ! NOT IMPLEMENTED
      integer,                intent(in),optional   :: staggerEdgeLWidth(:)
      integer,                intent(in),optional   :: staggerEdgeUWidth(:)
      integer,                intent(in),optional   :: staggerAlign(:)
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
!\item[{arrayList}]
!    An array to set the grid coordinate information from.
!\item[{[doCopy]}]
!    If not specified, default to {\tt ESMF\_DATA\_REF}, in this case the Grid 
!    coordinate Array will be set to a reference to {\tt array}. Please see 
!    Section~\ref{opt:copyflag} for further description and a list of
!    valid values. 
!    [THE ESMF\_DATA\_COPY OPTION IS CURRENTLY NOT IMPLEMENTED] 
! \item[{[staggerEdgeLWidth]}] 
!      This array should be the same rank as the grid. It specifies the lower corner of the stagger
!      region with respect to the lower corner of the exclusive region.
! \item[{[staggerEdgeUWidth]}] 
!      This array should be the same rank as the grid. It specifies the upper corner of the stagger
!      region with respect to the upper corner of the exclusive region.
! \item[{[staggerAlign]}] 
!      This array is of size  grid rank.
!      For this stagger location, it specifies which element
!      has the same index value as the center. For example, 
!      for a 2D cell with corner stagger it specifies which 
!      of the 4 corners has the same index as the center. 
!      If this is set and either staggerEdgeUWidth or staggerEdgeLWidth is not,
!      this determines the default array padding for a stagger. 
!      If not set, then this defaults to all negative. (e.g. 
!      The most negative part of the stagger in a cell is aligned with the 
!      center and the padding is all on the postive side.) 
!\item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!\end{description}
!
!EOPI
    integer :: tmp_staggerloc
    integer :: localrc ! local error status
    type(ESMF_InterfaceInt) :: staggerEdgeLWidthArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: staggerEdgeUWidthArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: staggerAlignArg  ! Language Interface Helper Var
    integer :: i,arrayCount
    type(ESMF_Pointer), allocatable :: arrayPointerList(:) ! helper variable

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Get size of array list
    arrayCount=size(arrayList)

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_GridGetInit, grid, rc)
    do i=1, arrayCount
       ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayGetInit, arrayList(i), rc)
    enddo

    ! handle staggerloc
    if (present(staggerloc)) then
       tmp_staggerloc=staggerloc%staggerloc
    else
       tmp_staggerloc=ESMF_STAGGERLOC_CENTER%staggerloc
    endif

    !! staggerLWidth
    staggerEdgeLWidthArg = ESMF_InterfaceIntCreate(staggerEdgeLWidth, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    !! staggerEdgeUWidth
    staggerEdgeUWidthArg = ESMF_InterfaceIntCreate(staggerEdgeUWidth, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    !! staggeAlign
    staggerAlignArg = ESMF_InterfaceIntCreate(staggerAlign, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    !! staggerAlign
    staggerAlignArg = ESMF_InterfaceIntCreate(staggerAlign, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


    ! Copy C++ pointers of deep objects into a simple ESMF_Pointer array
    ! This is necessary in order to strip off the F90 init check members
    ! when passing into C++
    allocate(arrayPointerList(arrayCount))
    do i=1, arrayCount
      call ESMF_ArrayGetThis(arrayList(i), arrayPointerList(i), localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    enddo

    ! Call C++ Subroutine to do the create
    call c_ESMC_gridaddcoordarraylist(grid%this,tmp_staggerloc, &
      arrayCount, arrayPointerList, docopy, staggerEdgeLWidthArg,     &
      staggerEdgeUWidthArg, staggerAlignArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! cleanup
    deallocate(arrayPointerList)
    call ESMF_InterfaceIntDestroy(staggerEdgeLWidthArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_InterfaceIntDestroy(staggerEdgeUWidthArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_InterfaceIntDestroy(staggerAlignArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


    if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAddCoordArrayList


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAddItem"

!BOP
! !IROUTINE: ESMF_GridAddItem - Allocate item array but don't set their values

! !INTERFACE:
  ! Private name; call using ESMF_GridAddItem()
     subroutine ESMF_GridAddItemNoValues(grid, staggerloc, item, itemTypeKind, &
                staggerEdgeLWidth, staggerEdgeUWidth, staggerAlign,  &
                staggerMemLBound,  totalLWidth, totalUWidth,rc)
!
! !ARGUMENTS:
      type(ESMF_Grid),        intent(in)              :: grid 
      type (ESMF_StaggerLoc), intent(in),optional     :: staggerloc
      type (ESMF_GridItem),   intent(in)              :: item
      type (ESMF_TypeKind),   intent(in),optional     :: itemTypeKind
      integer,                intent(in),optional     :: staggerEdgeLWidth(:)
      integer,                intent(in),optional     :: staggerEdgeUWidth(:)
      integer,                intent(in),optional     :: staggerAlign(:)
      integer,                intent(in),optional     :: staggerMemLBound(:)      
      integer,                intent(out), optional   :: totalLWidth(:)         ! N. IMP
      integer,                intent(out), optional   :: totalUWidth(:)         ! N. IMP
      integer,                intent(out),optional    :: rc
!
! !DESCRIPTION:
! 
!  When a Grid is created all of its potential stagger locations can hold item
!  data, but none of them have storage allocated. This call allocates item
!  storage (creates an internal ESMF\_Array and associated memory) for  a particular
!  stagger location. Note that this
!  call doesn't assign any values to the storage, it only allocates it. The
!  remaining options {\tt staggerEdgeLWidth}, etc. allow the user to adjust the 
!  padding on the item array.
!
! The arguments are:
! \begin{description}
!     \item[{grid}]
!       Grid to allocate coordinate storage in.  
! \item[{[staggerloc]}]
!      The stagger location to add. Please see Section~\ref{sec:opt:staggerloc} for a list 
!      of predefined stagger locations. If not present, defaults to ESMF\_STAGGERLOC\_CENTER.
! \item[{item}]
!      The grid item to add. Please see Section~\ref{sec:opt:griditem} for a list of valid items. 
! \item[{itemTypeKind}]
!      The typekind of the  item to add. 
! \item[{[staggerEdgeLWidth]}] 
!      This array should be the same dimCount as the grid. It specifies the lower corner of the stagger
!      region with respect to the lower corner of the exclusive region.
! \item[{[staggerEdgeUWidth]}] 
!      This array should be the same dimCount as the grid. It specifies the upper corner of the stagger
!      region with respect to the upper corner of the exclusive region.
! \item[{[staggerAlign]}] 
!      This array is of size  grid dimCount.
!      For this stagger location, it specifies which element
!      has the same index value as the center. For example, 
!      for a 2D cell with corner stagger it specifies which 
!      of the 4 corners has the same index as the center. 
!      If this is set and either staggerEdgeUWidth or staggerEdgeLWidth is not,
!      this determines the default array padding for a stagger. 
!      If not set, then this defaults to all negative. (e.g. 
!      The most negative part of the stagger in a cell is aligned with the 
!      center and the padding is all on the postive side.) 
! \item[{[staggerMemLBound]}] 
!      Specifies the lower index range of the memory of every DE in this staggerloc in this Grid. 
!      Only used when Grid indexflag is {\tt ESMF\_INDEX\_USER}. 
!\item[{[totalLWidth]}]
!     The lower boundary of the computatational region in reference to the computational region. 
!     Note, the computational region includes the extra padding specified by {\tt ccordLWidth}.
!     [CURRENTLY NOT IMPLEMENTED]
!\item[{[totalUWidth]}]
!     The lower boundary of the computatational region in reference to the computational region. 
!     Note, the computational region includes the extra padding specified by {\tt staggerEdgeLWidth}.
!     [CURRENTLY NOT IMPLEMENTED]
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP

    integer :: tmp_staggerloc
    integer :: localrc ! local error status
    type(ESMF_InterfaceInt) :: staggerEdgeLWidthArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: staggerEdgeUWidthArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: staggerAlignArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: staggerMemLBoundArg  ! Language Interface Helper Var
    type(ESMF_GridDecompType) :: decompType     ! decompose type: arbitrary or non-arbitrary

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_GridGetInit, grid, rc)

    ! Get Grid decomposition type
    call ESMF_GridGetDecompType(grid, decompType, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
	  
    ! check for not implemented parameters
    if (present(totalLWidth)) then
       totalLWidth = 0
       call ESMF_LogMsgSetError(ESMF_RC_NOT_IMPL, & 
                 "- totalLWidth specification not yet implemented", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if (present(totalUWidth)) then
       totalUWidth = 0
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

    ! Check if the grid is arbitrary
    if (decompType .eq. ESMF_GRID_ARBITRARY) then
       if (present(staggerEdgeLWidth) .or. present(staggerEdgeUWidth)) then
	  call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- staggerEdgeLWidth and staggerEdigeUWidth are not allowed for arbitrary grid", &
	         ESMF_CONTEXT, rc) 
          return
       endif
       if (present(staggerAlign)) then
	  call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- staggerAlign is not allowed for arbitrarily distributed grid", &
  	         ESMF_CONTEXT, rc) 
          return
       endif
       if (present(staggerloc) .and. staggerloc .ne. ESMF_STAGGERLOC_CENTER) then
	  call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", &
  	         ESMF_CONTEXT, rc) 
          return
       endif

       ! Call C++ Subroutine to do the create
       call c_ESMC_gridadditemarb(grid%this,tmp_staggerloc, item, itemTypeKind, localrc)
       if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

    else

       !! staggerEdgeLWidth
       staggerEdgeLWidthArg = ESMF_InterfaceIntCreate(staggerEdgeLWidth, rc=localrc)
       if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

       !! staggerEdgeUWidth
       staggerEdgeUWidthArg = ESMF_InterfaceIntCreate(staggerEdgeUWidth, rc=localrc)
       if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

       !! staggerAlign
       staggerAlignArg = ESMF_InterfaceIntCreate(staggerAlign, rc=localrc)
       if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

       !! staggerMemLBound
       staggerMemLBoundArg = ESMF_InterfaceIntCreate(staggerMemLBound, rc=localrc)
       if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

       ! Call C++ Subroutine to do the create
       call c_ESMC_gridadditem(grid%this,tmp_staggerloc, item, itemTypeKind, &
          staggerEdgeLWidthArg, staggerEdgeUWidthArg, staggerAlignArg, &
          staggerMemLBoundArg,  localrc)
       if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

       ! Deallocate helper variables
       call ESMF_InterfaceIntDestroy(staggerEdgeLWidthArg, rc=localrc)
       if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

       call ESMF_InterfaceIntDestroy(staggerEdgeUWidthArg, rc=localrc)
       if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

       call ESMF_InterfaceIntDestroy(staggerAlignArg, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

       call ESMF_InterfaceIntDestroy(staggerMemLBoundArg, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
 
    endif

    if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAddItemNoValues


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCommit"
!BOPI
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
!    {\tt grid} will have sufficient size, dimCount, and distribution information to be
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
!EOPI
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
#define ESMF_METHOD "GridConvertIndex"

!BOPI
! !IROUTINE: ESMF_GridConvertIndex - Convert Arbitrary Grid index into DistGrid index
! !INTERFACE:
      subroutine ESMF_GridConvertIndex(grid,gridindex, distgridindex, rc)
!
! !ARGUMENTS:
         type(ESMF_Grid),       intent(in)            :: grid
	 integer	      , intent(in)            :: gridindex(:)
	 integer	      , intent(out)            :: distgridindex(:)
	 integer	      , intent(out), optional :: rc
!
! !DESCRIPTION:
!  
!  Convert a multi-dimensional index of the arbitrarily distributed grid into the
!  index of the 1D DistGrid.  The associated DistGrid for an arbitrarily distributed
!  grid is 1D plus any undistributed dimension.  The function
!  calculates the index of the DistGrid for a given index from the original Grid.
!
! The arguments are:
! \begin{description}
!\item[{grid}]
!     The grid to get the information from to create the Array.
! \item[{[gridindex]}]
!     The Grid index to be converted.
! \item[{[distgridindex]}]
!     The DistGrid index to be returned.
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!\end{description}
!
!EOPI

    integer ::  localrc
    integer ::  DimCount, distDimCount, undistDimCount
    integer, pointer ::  minIndex(:)   
    integer, pointer ::  maxIndex(:)
    integer, pointer ::  distgridToGridMap(:)
    integer          :: i,j,k
    integer ::  index1D    ! the return value
    type(ESMF_InterfaceInt)   :: gridIndexArg
    type(ESMF_GridDecompType) :: decompType
    type(ESMF_DistGrid) :: distGrid
    integer, allocatable :: undistdim(:)
    logical  :: found
    integer :: distGridDimCount, arbDim


    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_GridGetInit, grid, rc)

    ! Get Grid decomposition type
    call ESMF_GridGetDecompType(grid, decompType, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
	  
    ! Check if the grid is arbitrary
    if (decompType .ne. ESMF_GRID_ARBITRARY) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- ESMF_GridConvertIndex only works for arbritrarily distributed grid", & 
                 ESMF_CONTEXT, rc) 
       return
    endif  

    ! Get info from Grid
    call ESMF_GridGet(grid, distgrid= distGrid, DimCount=DimCount, &
		      rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! allocate minIndex and maxIndex
    allocate(minIndex(DimCount), maxIndex(DimCount), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating minIndex and maxIndex", &
                                     ESMF_CONTEXT, rc)) return 
 
    ! Get minIndex and maxIndex from the grid
    call ESMF_GridGetIndex(grid, minIndex= minIndex, maxIndex=maxIndex, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! find out how many dimensions are arbitrarily distributed
     call ESMF_DistGridGet(distGrid, dimcount = distGridDimCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    if (distGridDimCount .gt. dimCount) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                   "- distgrid dimension has to be less than or equal to dimCount", & 
                     ESMF_CONTEXT, rc) 
        return 
     endif

    ! set distDimCount - number of dimensions arbitrarily distributed
    !     undistDimCount - number of dimensions not arbitrarily distributed
    if (distGridDimCount .eq. 1) then
       ! all dimensions are arbitrarily distributed
       distDimCount = dimCount
       undistDimCount = 0
    else
       undistDimCount = distGridDimCount - 1
       distDimCount = dimCount - undistDimCount
    endif

    ! Check index dimension
    if (size(gridindex) .ne. dimCount) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
            "- gridindex dimension is different from the grid DimCount", & 
            ESMF_CONTEXT, rc) 
       return
    endif

    ! Check index out of bound
    do i=1,dimCount
       if (gridindex(i) .lt. minIndex(i) .and. gridindex(i) .gt. maxIndex(i)) then
	   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
               "- gridindex is out of bound", & 
                      ESMF_CONTEXT, rc) 
	   return
       endif
    enddo

    ! clean up memory allocation
    deallocate(minIndex)
    deallocate(maxIndex)

    ! Call the C function to get the index of the 1D distgrid
    !! index
    gridIndexArg = ESMF_InterfaceIntCreate(gridindex, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call c_ESMC_gridconvertindex(grid%this, gridIndexArg, index1D, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (undistDimCount .ne. 0) then
      allocate(distgridToGridMap(dimCount), stat=localrc)
      call ESMF_GridGet(grid, arbDim=arbDim, &
	distgridToGridMap=distgridToGridMap, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      k=1
      allocate(undistdim(undistDimCount))
      do i=1,dimCount
        found = .false.
	do j=1,distDimCount
	  if (i .eq. distgridToGridMap(j)) found=.true.
	enddo
        if (.not. found) then
	  undistdim(k)=i
          k=k+1
        endif
      enddo
     
      k=1
      do i=1,distGridDimCount
	if (i .eq. arbDim) then
           distgridindex(i)=index1D
        else
	   distgridindex(i)=gridindex(undistdim(k))
           k=k+1
        endif
      enddo
      deallocate(undistdim)
      deallocate(distgridToGridMap)
    else
      distgridindex(1)=index1D
    endif  

    ! clean up memory allocation
    call ESMF_InterfaceIntDestroy(GridIndexArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    return

end subroutine ESMF_GridConvertIndex

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ArrayCreateFromGrid"

!BOPI
! !IROUTINE: ESMF_ArrayCreateFromGrid - Create an Array to hold data for a stagger location

! !INTERFACE:
      function ESMF_ArrayCreateFromGrid(grid,staggerloc,name,typekind, &
                           gridToArrayMap, ungriddedLBound, ungriddedUBound, &
                           totalLWidth, totalUWidth, rc)
!
! !RETURN VALUE:
      type(ESMF_Array) :: ESMF_ArrayCreateFromGrid
!
! !ARGUMENTS:
       type(ESMF_Grid),       intent(in)            :: grid
       type(ESMF_StaggerLoc), intent(in),  optional :: staggerloc
       character (len=*),     intent(in),  optional :: name
       type(ESMF_TypeKind),   intent(in),  optional :: typekind
       integer,               intent(in),  optional :: gridToArrayMap(:)
       integer,               intent(in),  optional :: ungriddedLBound(:)
       integer,               intent(in),  optional :: ungriddedUBound(:)
       integer,               intent(in),  optional :: totalLWidth(:)
       integer,               intent(in),  optional :: totalUWidth(:)
       integer,               intent(out), optional :: rc

!
! !DESCRIPTION:
!  
!  Create an ESMF Array which is suitable to hold data for a particular
!  stagger location in a Grid. The Array will have the correct bounds, distgridToGridMap,
!  distgrid, etc. The {\tt totalWidth} variables can be used to add extra padding
!  around the Array (e.g. for use as a halo). 
!
! The arguments are:
! \begin{description}
!\item[{grid}]
!     The grid to get the information from to create the Array.
!\item[{staggerloc}]
!     The stagger location to build the Array for. 
!     Please see Section~\ref{sec:opt:staggerloc} for a list 
!     of predefined stagger locations. If not present, defaults to
!      ESMF\_STAGGERLOC\_CENTER.
! \item[{[name]}]
!     {\tt ESMF\_Grid} name.
! \item[{[typekind]}] 
!     The type/kind of the newly created array data. 
!     If not specified then the type/kind will be 8 byte reals.  
!\item[{[gridToArrayMap]}]
!     Indicates where each grid dimension goes in the newly created Array.
!     {\tt The array gridToArrayMap} should be at least of size equal to the grid's dimCount.
!     If not set defaults to (1,2,3,....). An entry of 0 indicates the grid dimension
!     won't be used in the creation of the Array.
!\item[{[ungriddedLBound]}]
!     The lower bounds of the non-grid Array dimensions.
!\item[{[ungriddedUBound]}]
!     The upper bounds of the non-grid array dimensions.  
!\item[{[totalLWidth]}]
!     Extra padding to be added to the Array. {\tt totalLWidth} is the amount
!     that the lower boundary of the Array should be dropped relative
!     to the lower bound of the exclusive region.
!\item[{[totalUWidth]}]
!     Extra padding to be added to the Array. {\tt totalUWidth} is the amount
!     that the upper boundary of the Array should be raised relative
!     to the upper bound of the exclusive region.
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
    integer :: localrc ! local error status
    type(ESMF_Array) :: array             
    type(ESMF_ArraySpec) :: arrayspec         
    type(ESMF_DistGrid) :: distgrid
    type(ESMF_IndexFlag) :: indexflag    
    type(ESMF_TypeKind) :: localTypeKind
    type(ESMF_StaggerLoc) :: localStaggerLoc
    integer, pointer :: arrayLBound(:),arrayUBound(:)
    integer, pointer :: distgridToArrayMap(:)
    integer :: dimCount
    integer :: i,ungriddedDimCount, arrayDimCount, undistArrayDimCount
    logical :: contains_nonzero
    integer :: gridUsedDimCount
   
    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_GridGetInit, grid, rc)

    ! Set Default TypeKind if neccessary
    if (present(typekind)) then
       localTypeKind=typekind
    else
       localTypeKind=ESMF_TYPEKIND_R8
    endif

    ! Set Default StaggerLoc if neccessary
    if (present(staggerloc)) then
       localStaggerLoc=staggerloc
    else
       localStaggerLoc=ESMF_STAGGERLOC_CENTER
    endif


    ! Both the bounds need to be present if either is.
    if ((present(ungriddedLBound) .or. present(ungriddedUBound)) .and. &
        .not. (present(ungriddedLBound) .and. present(ungriddedUBound))) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
               "- if either ungriddedBound is present both need to be", & 
                      ESMF_CONTEXT, rc) 
       return 
    endif

    ! The bounds need to be the same size
    if (present(ungriddedLBound) .and. present(ungriddedUBound)) then
       if (size(ungriddedLBound) .ne. size(ungriddedUBound)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
                "- ungriddedLBound and ungriddedUBound must be the same size ", & 
                  ESMF_CONTEXT, rc) 
          return 
       endif
    endif

   ! Get the ungridded dimCount
   ungriddedDimCount=0
   if (present(ungriddedUBound)) then
      ungriddedDimCount=size(ungriddedUBound)
   endif

    ! Get info from Grid
    call ESMF_GridGet(grid, dimCount=dimCount,  &
                      indexflag=indexflag, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! calc undist Array DimCount
    undistArrayDimCount=ungriddedDimCount

    ! Make sure gridToArrayMap is correct size
    if (present(gridToArrayMap)) then
       if (size(gridToArrayMap) < dimCount) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- gridToArrayMap needs to at least be of the Grid's dimCount", & 
                      ESMF_CONTEXT, rc) 
          return 
       endif
    endif

   ! calc the number of dimensions from the grid being used (e.g. with non-zero mapping)
    if (present(gridToArrayMap)) then
       gridUsedDimCount=0
       do i=1,dimCount
          if (gridToArrayMap(i) .gt. 0) then
             gridUsedDimCount=gridUsedDimCount+1
          endif
       enddo
   else
       ! Default assumes all grid dims are used so add number of grid dims
       gridUsedDimCount=dimCount
   endif

    ! calc full Array DimCount
    ! Its the ungriddedDimCount + the number of non-zero entries in gridToArrayMap
    arrayDimCount=ungriddedDimCount+gridUsedDimCount


    ! Make sure gridToArrayMap is correct size
    if (present(gridToArrayMap)) then
       do i=1,dimCount
          if ((gridToArrayMap(i) <0) .or. (gridToArrayMap(i) > arrayDimCount)) then
              call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                   "- gridToArrayMap value is outside range", & 
                          ESMF_CONTEXT, rc) 
              return 
          endif
       enddo
    endif

    ! Make sure gridToArrayMap contains at least one non-zero entry
    if (present(gridToArrayMap)) then
       contains_nonzero=.false.
       do i=1,dimCount
          if (gridToArrayMap(i) >0) then
	     contains_nonzero=.true.
          endif
       enddo
       if (.not. contains_nonzero) then 
             call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                   "- gridToArrayMap must contains at least one value greater than 0", & 
                          ESMF_CONTEXT, rc) 
              return 
       endif
    endif

   ! construct ArraySpec
   call ESMF_ArraySpecSet(arrayspec,rank=arrayDimCount,typekind=localTypeKind, rc=localrc)
   if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

    ! allocate distgridToArrayMap
    allocate(distgridToArrayMap(dimCount) , stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating distgridToArrayMap", &
                                     ESMF_CONTEXT, rc)) return   

    ! allocate undistributed Bounds
    allocate(arrayLBound(undistArrayDimCount) , stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating gridLBound", &
                                     ESMF_CONTEXT, rc)) return   
    allocate(arrayUBound(undistArrayDimCount) , stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating gridUBound", &
                                     ESMF_CONTEXT, rc)) return   


    ! Get dimmap and undistibuted bounds
    call ESMF_GridGetArrayInfo(grid, localstaggerloc,                         &
                            gridToArrayMap, ungriddedLBound, ungriddedUBound, &
                            distgrid, distgridToArrayMap, arrayLBound, arrayUBound,   &
                            rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return



    ! create Array
    array=ESMF_ArrayCreate(arrayspec=arrayspec, &
              distgrid=distgrid, distgridToArrayMap=distgridToArrayMap, &
              totalLWidth=totalLWidth, totalUWidth=totalUWidth, &
              indexflag=indexflag, &
              undistLBound=arrayLBound, undistUBound=arrayUBound, name=name, &
              rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return


    ! Set return value
    ESMF_ArrayCreateFromGrid = array


    ! cleanup
    deallocate(distgridToArrayMap)
    deallocate(arrayLBound)
    deallocate(arrayUBound)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

    end function ESMF_ArrayCreateFromGrid


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetArrayInfo"

!BOPI
! !IROUTINE: ESMF_GridGetArrayInfo - get information to make an Array from a Grid

! !INTERFACE:
      subroutine ESMF_GridGetArrayInfo(grid, staggerloc, &
                           gridToFieldMap, ungriddedLBound, ungriddedUBound, &
                           staggerDistgrid, distgridToArrayMap, &
                           undistLBound, undistUBound,   &
                           rc)
!
! !ARGUMENTS:
       type(ESMF_Grid),       intent(in)            :: grid
       type(ESMF_StaggerLoc), intent(in),  optional :: staggerloc
       integer,               intent(in),  optional :: gridToFieldMap(:)
       integer,               intent(in),  optional :: ungriddedLBound(:)
       integer,               intent(in),  optional :: ungriddedUBound(:)
       type(ESMF_DistGrid),   intent(out), optional :: staggerDistgrid 
       integer,               intent(out)           :: distgridToArrayMap(:)
       integer,               intent(out)           :: undistLBound(:)
       integer,               intent(out)           :: undistUBound(:)
       integer,               intent(out), optional :: rc

!
! !DESCRIPTION:
!  
! This subroutine gets information from a Grid which is useful in creating an
! Array corresponding to a Field. This subroutine returns the distgridToArray map and 
! undistBounds which can be used to create an Array the same size and shape as the Grid. 
! Optionally, the user can pass in non-grid bounds, the subroutine then
! returns a map and undistbounds which include these non-grid bounds. 
!
! The arguments are:
! \begin{description}
!\item[{grid}]
!     The grid to get the information from to create the Array.
!\item[{staggerloc}]
!     The stagger location to build the Array for. 
!     Please see Section~\ref{sec:opt:staggerloc} for a list 
!     of predefined stagger locations. If not present, defaults to
!      ESMF\_STAGGERLOC\_CENTER.
!\item[staggerDistgrid]
!   The class that describes the stagger locations in the grids distribution.
!\item[{[gridToFieldMap]}]
!     Indicates how the grid dimension map to the field that the newly created array 
!     is associated with. {\tt The array gridToFieldMap} should be at least of size equal 
!     to the grid's dimCount. If not set defaults to (1,2,3,....). An entry of 0 indicates 
!     the grid dimension isn't mapped to the Array. 
!\item[{[ungriddedLBound]}]
!     The lower bounds of the non-grid Array dimensions.
!\item[{[ungriddedUBound]}]
!     The upper bounds of the non-grid array dimensions.  
!\item[{distgridToArrayMap}]
!     The distgrid to Array dimension map (must be allocated to at least
!     the number of dimensions of the distGrid).
!\item[{undistLBound}]
!     Undistributed lower bounds (must be of size grid undistDimCount+size(ungriddedUBound))
!\item[{undistUBound}]
!     Undistributed upper bounds (must be of size grid undistDimCount+size(ungriddedUBound))
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
    integer :: localrc ! local error status
    type(ESMF_StaggerLoc) :: localStaggerLoc
    type(ESMF_GridDecompType) :: decompType
    integer, pointer :: arrayDimType(:)
    integer, pointer :: arrayDimInd(:)
    integer, pointer :: distgridToGridMap(:)
    integer :: dimCount,distDimCount, arrayDimCount
    integer :: i,j,k,ungriddedDimCount, undistArrayDimCount, bndpos
    integer :: gridComputationalEdgeLWidth(ESMF_MAXDIM)
    integer :: gridComputationalEdgeUWidth(ESMF_MAXDIM)
    integer :: tmpArrayComputationalEdgeLWidth(ESMF_MAXDIM)
    integer :: tmpArrayComputationalEdgeUWidth(ESMF_MAXDIM)
    integer :: localGridToFieldMap(ESMF_MAXDIM)
    logical :: filled(ESMF_MAXDIM)
    logical :: contains_nonzero   
    integer :: fieldDimCount
    integer :: gridUsedDimCount
    integer :: arbdim, rep_arb, rep_noarb
    logical :: found
    type(ESMF_DistGrid) :: distgrid
    

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_GridGetInit, grid, rc)

    ! Get DecomposeType
    call ESMF_GridGetDecompType(grid, decompType, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


    ! Set Default StaggerLoc if neccessary
    if (present(staggerloc)) then
       localStaggerLoc=staggerloc
    else
       localStaggerLoc=ESMF_STAGGERLOC_CENTER
    endif


    ! Both the bounds need to be present if either is.
    if ((present(ungriddedLBound) .or. present(ungriddedUBound)) .and. &
        .not. (present(ungriddedLBound) .and. present(ungriddedUBound))) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
               "- if either ungriddedBound is present both need to be", & 
                      ESMF_CONTEXT, rc) 
       return 
    endif

    ! The bounds need to be the same size
    if (present(ungriddedLBound) .and. present(ungriddedUBound)) then
       if (size(ungriddedLBound) .ne. size(ungriddedUBound)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
                "- ungriddedLBound and ungriddedUBound must be the same size ", & 
                  ESMF_CONTEXT, rc) 
          return 
       endif
    endif

   ! Get the ungridded dimCount
   ungriddedDimCount=0
   if (present(ungriddedUBound)) then
      ungriddedDimCount=size(ungriddedUBound)
   endif

    ! Get info from Grid
    call ESMF_GridGet(grid, dimCount=dimCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! calc undist Array DimCount
    undistArrayDimCount=ungriddedDimCount

    ! Make sure gridToFieldMap is correct size
    if (present(gridToFieldMap)) then
       if (size(gridToFieldMap) < dimCount) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- gridToFieldMap needs to at least be of the Grid's dimCount", & 
                      ESMF_CONTEXT, rc) 
          return 
       endif
    endif
    
    ! Get grid distgrid
    call ESMF_GridGet(grid, localStaggerLoc, distgrid, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

    ! if argument is present, then pass out distgrid
    if (present(staggerDistGrid)) then
          staggerDistGrid=distgrid
    endif


    ! if the Grid is arbitrary, the array dimension will be different depending on how many
    ! grid dimensions are arbitrarily distributed
    if (decompType .eq. ESMF_GRID_NONARBITRARY) then

       ! calc the number of dimensions from the grid being used (e.g. with non-zero mapping)
       if (present(gridToFieldMap)) then
          gridUsedDimCount=0
          do i=1,dimCount
             if (gridToFieldMap(i) .gt. 0) then
                gridUsedDimCount=gridUsedDimCount+1
             endif
          enddo
      else
          ! Default assumes all grid dims are used so add number of grid dims
          gridUsedDimCount=dimCount
      endif

      ! calc full Array DimCount
      ! Its the ungriddedDimCount + the number of non-zero entries in gridToFieldMap
       arrayDimCount=ungriddedDimCount+gridUsedDimCount

       ! Make sure gridToFieldMap is correct size
       if (present(gridToFieldMap)) then
          do i=1,dimCount
             if ((gridToFieldMap(i) <0) .or. (gridToFieldMap(i) > arrayDimCount)) then
                 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- gridToFieldMap value is outside range", & 
                          ESMF_CONTEXT, rc) 
                 return 
             endif
          enddo
       endif

      ! Remove this check to allow Fields with no Grid dimensions
#if 0
       ! Make sure gridToFieldMap contains at least one non-zero entry
       if (present(gridToFieldMap)) then
          contains_nonzero=.false.
          do i=1,dimCount
             if (gridToFieldMap(i) >0) then
	        contains_nonzero=.true.
             endif
          enddo
          if (.not. contains_nonzero) then 
                call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                      "- gridToFieldMap must contains at least one value greater than 0", & 
                             ESMF_CONTEXT, rc) 
                return 
          endif
       endif
#endif

       ! Check distgridToArrayMap
       if (size(distgridToArrayMap) < dimCount) then
           call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
                      "- distgridToArrayMap is too small", & 
                          ESMF_CONTEXT, rc) 
           return 
       endif


      ! set default GridToFieldMap
      if (present(gridToFieldMap)) then
        localGridToFieldMap(1:dimCount)=gridToFieldMap(1:dimCount)
      else
         do i=1,dimCount
           localGridToFieldMap(i)=i
         enddo
      endif  


       ! allocate distgridToGridMap
       allocate(distgridToGridMap(dimCount) , stat=localrc)
       if (ESMF_LogMsgFoundAllocError(localrc, "Allocating distgridToGridMap", &
                                        ESMF_CONTEXT, rc)) return   
       ! Get info from Grid
       call ESMF_GridGet(grid, distgridToGridMap=distgridToGridMap, rc=localrc)
       if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

       ! construct distgridToArrayMap
       do i=1,dimCount
          distgridToArrayMap(i)=localGridToFieldMap(distgridToGridMap(i))
       enddo

       ! construct array based on the presence of distributed dimensions
       ! if there are undistributed dimensions ...
       if (undistArrayDimCount .gt. 0) then      

          !! allocate array dim. info arrays
          allocate(arrayDimType(arrayDimCount) , stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "Allocating gridUBound", &
                                         ESMF_CONTEXT, rc)) return   
          allocate(arrayDimInd(arrayDimCount) , stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "Allocating gridUBound", &
                                         ESMF_CONTEXT, rc)) return   

          !! set which dimensions are used by the distgrid
          arrayDimType(:)=0 ! initialize to no type
          do i=1,dimCount
             if (distGridToArrayMap(i) .gt. 0) then ! skip replicated dims 
                arrayDimType(distGridToArrayMap(i))=1 ! set to distributed
             endif
          enddo

          ! TODO: make the below cleaner given no grid undistdim
          !! Fill in ungridded bound info
          bndpos=1
          do i=1,arrayDimCount
             if (arrayDimType(i) .eq. 0) then
                arrayDimInd(i)=bndpos
                arrayDimType(i)=2 ! set to undistributed Array
                bndpos=bndpos+1
             endif
          enddo

          !! Finally setup new Array bounds based on info in arrayDimType and arrayDimInd
          bndpos=1
          do i=1,arrayDimCount
             if (arrayDimType(i) .eq. 2) then
                undistLBound(bndpos)=ungriddedLBound(arrayDimInd(i))
                undistUBound(bndpos)=ungriddedUBound(arrayDimInd(i))
                bndpos=bndpos+1
             endif
          enddo

          !! cleanup
          deallocate(arrayDimType)
          deallocate(arrayDimInd)
        endif

        ! cleanup
        deallocate(distgridToGridMap)
    else
       ! Code for Arbitrarily Distributed Grid
       call ESMF_DistGridGet(distgrid, dimCount=distDimCount, rc=localrc)    
       if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return

       if (present(gridToFieldMap)) then
          gridUsedDimCount=0
          do i=1,dimCount
             if (gridToFieldMap(i) .gt. 0) then
                gridUsedDimCount=gridUsedDimCount+1
             endif
          enddo
      else
          ! Default assumes all grid dims are used so add number of grid dims
          gridUsedDimCount=dimCount
      endif

      ! calc full Array DimCount
      ! Its the ungriddedDimCount + the number of non-zero entries in gridToFieldMap
       fieldDimCount=ungriddedDimCount+gridUsedDimCount

       ! Make sure gridToFieldMap is correct size
       ! check for replicated dimension
       if (present(gridToFieldMap)) then
          do i=1,dimCount
             if ((gridToFieldMap(i) <0) .or. (gridToFieldMap(i) > fieldDimCount)) then
                 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                      "- gridToFieldMap value is outside range", & 
                          ESMF_CONTEXT, rc) 
                 return 
             endif
          enddo
       endif

       ! set default GridToFieldMap
       if (present(gridToFieldMap)) then
         localGridToFieldMap(1:dimCount)=gridToFieldMap(1:dimCount)
       else
          do i=1,dimCount
            localGridToFieldMap(i)=i
          enddo
       endif  

       ! If there is replicated dimension, check if they are arbitrarily distributed dimension
       ! The array dimension varies depends whether the replicated dimensions are arb. or not
       allocate(distgridToGridMap(dimCount))
       call ESMF_GridGet(grid, distgridToGridMap=distgridToGridMap, &
		arbdim=arbdim, rc=localrc)
       if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return

       ! Check distgridToArrayMap
       if (size(distgridToArrayMap) < distDimCount) then
           call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
                      "- distgridToArrayMap is too small", & 
                          ESMF_CONTEXT, rc) 
           return 
       endif

       ! count how many replicated dimensions are not arbitrary and if any of replicated dimension
       ! is arbitrary.  Assuming if one arbitrary dim is replicated, all the arbitrary dimension
       ! should also be replicated.  This check is done in ESMF_FieldCreate already

       ! initialze distgridToArrayMap
       do i=1,distDimCount
	   distgridToArrayMap(i)= i
       enddo

       ! if there is any replicated dimensions, reassign distgridToArrayMap
       rep_arb = 0
       rep_noarb = 0
       if (gridUsedDimCount < dimCount) then
         k = 1
         do i=1,dimCount
           found = .false.
	   if (localGridToFieldMap(i) .eq. 0) then
	     do j=1,dimCount
		if (distgridToGridMap(j) .eq. i) then
		  found = .true.
		  exit
	        endif
             enddo
	     if (found) then 
                distgridToArrayMap(arbdim) = 0
		rep_arb = 1
             else
		rep_noarb = rep_noarb+1
		if (k .eq. arbdim) k = k + 1
                distgridToArrayMap(k) = 0
		k=k+1
             endif
           endif
         enddo
         j=1
         do i=1,distDimCount
	  if (distgridToArrayMap(i) .ne. 0) then 
	     distgridToArrayMap(i)= j
	     j=j+1
          endif
         enddo
       endif

       arrayDimCount=ungriddedDimCount+distDimCount-rep_noarb-rep_arb

       deallocate(distgridToGridMap)
              
       ! construct array based on the presence of distributed dimensions
       ! if there are undistributed dimensions ...
       if (undistArrayDimCount .gt. 0) then      
	  ! Copy ungriddedBound to undistBound
	  do i=1,undistArrayDimCount
            undistLBound(i)=ungriddedLBound(i)
            undistUBound(i)=ungriddedUBound(i)
	  enddo
       endif
    endif

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

    end  subroutine ESMF_GridGetArrayInfo


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreate"
!BOP
! !IROUTINE: ESMF_GridCreate - Create a Grid from a DistGrid

! !INTERFACE:
  ! Private name; call using ESMF_GridCreate()
      function ESMF_GridCreateFromDistGrid(name,coordTypeKind,distgrid, &
                         distgridToGridMap, coordDimCount, coordDimMap, &
                         gridEdgeLWidth, gridEdgeUWidth, gridAlign, gridMemLBound, &
                         indexflag, destroyDistGrid, destroyDELayout, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateFromDistGrid
!
! !ARGUMENTS:
       character (len=*), intent(in), optional :: name
       type(ESMF_TypeKind),  intent(in),   optional  :: coordTypeKind
       type(ESMF_DistGrid),   intent(in)              :: distgrid
       integer,               intent(in),   optional  :: distgridToGridMap(:)
       integer,               intent(in),   optional  :: coordDimCount(:)
       integer,               intent(in),   optional  :: coordDimMap(:,:)
       integer,               intent(in),   optional  :: gridEdgeLWidth(:)
       integer,               intent(in),   optional  :: gridEdgeUWidth(:)
       integer,               intent(in),   optional  :: gridAlign(:)
       integer,               intent(in),   optional  :: gridMemLBound(:)
       type(ESMF_IndexFlag),  intent(in),   optional  :: indexflag
       logical,               intent(in),   optional  :: destroyDistGrid
       logical,               intent(in),   optional  :: destroyDELayout
       integer,               intent(out),  optional  :: rc
!
! !DESCRIPTION:
! This is the most general form of creation for an {\tt ESMF\_Grid}
! object. It allows the user to fully specify the topology and index space
! using the DistGrid methods and then build a grid out
! of the resulting DistGrid. Note that since the Grid created by this call 
! uses {\tt distgrid} as a description of its index space, the resulting Grid 
! will have exactly the same number of dimensions (i.e. the same dimCount) as 
! {\tt distgrid}. The {\tt distgridToGridMap} argument
! specifies how the Grid dimensions are mapped to the {\tt distgrid}. 
! The {\tt coordDimCount} and {\tt coordDimMap} arguments
! allow the user to specify how the coordinate arrays should map to the grid
! dimensions. (Note, though, that creating a grid does not allocate coordinate
! storage. A method such as {\tt ESMF\_GridAddCoord()} must be called
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
!      distributed over DEs. 
! \item[{[distgridToGridMap]}] 
!      List that has dimCount elements.
!      The elements map each dimension of distgrid to a dimension in the grid.
!       (i.e. the values should range from 1 to dimCount). If not specified, the default
!       is to map all of distgrid's dimensions against the dimensions of the
!       grid in sequence. 
! \item[{[coordDimCount]}]
!      List that has dimCount elements.
!      Gives the dimension of each component (e.g. x) array. This is 
!      to allow factorization of the coordinate arrays. If not specified
!      all arrays are the same size as the grid. 
! \item[{[coordDimMap]}]
!      2D list of size dimCount x  dimCount. This array describes the
!      map of each component array's dimensions onto the grids
!      dimensions. Each entry {\tt coordDimMap(i,j)} tells which
!      grid dimension component i's, jth dimension maps to. 
!      Note that if j is bigger than {\tt coordDimCount(i)} it is ignored.        
!      The default for each row i is {\tt coordDimMap(i,:)=(1,2,3,4,...)}.        
! \item[{[gridEdgeLWidth]}] 
!      The padding around the lower edges of the grid. This padding is between
!      the index space corresponding to the cells and the boundary of the 
!      the exclusive region. This extra space is to contain the extra
!      padding for non-center stagger locations, and should be big enough
!      to hold any stagger in the grid. 
! \item[{[gridEdgeUWidth]}] 
!      The padding around the upper edges of the grid. This padding is between
!      the index space corresponding to the cells and the boundary of the 
!      the exclusive region. This extra space is to contain the extra
!      padding for non-center stagger locations, and should be big enough
!      to hold any stagger in the grid. 
! \item[{[gridAlign]}] 
!     Specification of how the stagger locations should align with the cell
!     index space (can be overridden by the individual staggerAligns). If
!     the {\tt gridEdgeWidths} are not specified than this parameter
!     implies the EdgeWidths.
! \item[{[gridMemLBound]}] 
!      Specifies the lower index range of the memory of every DE in this Grid. 
!      Only used when indexflag is {\tt ESMF\_INDEX\_USER}. May be overridden
!      by staggerMemLBound. 
! \item[{[indexflag]}]
!      Indicates the indexing scheme to be used in the new Grid. Please see
!      Section~\ref{opt:indexflag} for the list of options. If not present,
!      defaults to ESMF\_INDEX\_DELOCAL.
! \item[{[destroyDistgrid]}]
!      If true, when the Grid is destroyed the DistGrid will be destroyed also. 
!      Defaults to false. 
! \item[{[destroyDELayout]}]
!      If true, when the Grid is destroyed the DELayout will be destroyed also. 
!      Defaults to false. 
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
    integer :: localrc ! local error status
    type(ESMF_Grid) :: grid              
    integer :: nameLen 
    type(ESMF_InterfaceInt) :: gridEdgeLWidthArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: gridEdgeUWidthArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: gridAlignArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: gridMemLBoundArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: distgridToGridMapArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: coordDimCountArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: coordDimMapArg ! Language Interface Helper Var
    integer :: intDestroyDistgrid,intDestroyDELayout
    integer, allocatable :: collocationPDim(:)
    logical  :: arbSeqIndexFlag
    integer :: i, deCount, distDimCount, arbDim
    type(ESMF_DELayout) :: delayout

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

    !! Check if the DistGrid is an arbitrary distgrid
    arbDim = -1
    call ESMF_DistGridGet(distgrid, delayout=delayout, dimCount=distDimCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_DELayoutGet(delayout, localDeCount=deCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    if (deCount .gt. 0) then
      allocate(collocationPDim(distDimCount))  ! dimCount
      call ESMF_DistGridGet(distgrid,   &
           collocationPDim=collocationPDim, rc=localrc)
      do i=1,distDimCount
          call ESMF_DistGridGet(distgrid, localDe=0, collocation=collocationPDim(i), &
              arbSeqIndexFlag=arbSeqIndexFlag, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
          if (arbSeqIndexFlag) arbDim = i
      enddo
      deallocate(collocationPDim)
    endif

    if (arbDim .ne. -1) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                   "- distgrid should not contain arbitrary sequence indices", & 
                          ESMF_CONTEXT, rc) 
        return
    endif

    !! coordTypeKind
    ! It doesn't look like it needs to be translated, but test to make sure

    !! staggerWidths
    gridEdgeLWidthArg = ESMF_InterfaceIntCreate(gridEdgeLWidth, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    gridEdgeUWidthArg = ESMF_InterfaceIntCreate(gridEdgeUWidth, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    gridAlignArg = ESMF_InterfaceIntCreate(gridAlign, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    gridMemLBoundArg = ESMF_InterfaceIntCreate(gridMemLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    !! distgridToGridMap
    distgridToGridMapArg = ESMF_InterfaceIntCreate(distgridToGridMap, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    !! Description of array factorization
    coordDimCountArg = ESMF_InterfaceIntCreate(coordDimCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    coordDimMapArg = ESMF_InterfaceIntCreate(farray2D=coordDimMap, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    !! Convert destroyDistGrid flag
    if (present(destroyDistgrid)) then
        if (destroyDistgrid) then
           intDestroyDistgrid=1
        else
           intDestroyDistgrid=0
         endif
    else
           intDestroyDistgrid=0
    endif

    !! Convert destroyDELayout flag
    if (present(destroyDELayout)) then
        if (destroyDELayout) then
           intDestroyDELayout=1
        else
           intDestroyDELayout=0
         endif
    else
           intDestroyDELayout=0
    endif


    ! Initialize this grid object as invalid
    grid%this = ESMF_NULL_POINTER

    ! Call C++ Subroutine to do the create
    call c_ESMC_gridcreatefromdistgrid(grid%this, nameLen, name, &
      coordTypeKind, distgrid, distgridToGridMapArg, &
      coordDimCountArg, coordDimMapArg, &
      gridEdgeLWidthArg, gridEdgeUWidthArg, gridAlignArg, gridMemLBoundArg,&
      indexflag, intDestroyDistGrid, intDestroyDELayout, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deallocate helper variables
    call ESMF_InterfaceIntDestroy(gridEdgeLWidthArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(gridEdgeUWidthArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(gridAlignArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(gridMemLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(distgridToGridMapArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(coordDimCountArg, rc=localrc)
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
#define ESMF_METHOD "ESMF_GridCreate"
!BOP
! !IROUTINE: ESMF_GridCreate - Create a Arbitrary Grid from a DistGrid

! !INTERFACE:
  ! Private name; call using ESMF_GridCreate()
      function ESMF_GridCreateFromDistGridArb(name,coordTypeKind,distgrid, &
			 indexArray, distDim, coordDimCount, coordDimMap, &
                         destroyDistGrid, destroyDELayout, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateFromDistGridArb
!
! !ARGUMENTS:
       character (len=*), intent(in), optional :: name
       type(ESMF_TypeKind),  intent(in),   optional  :: coordTypeKind
       type(ESMF_DistGrid),   intent(in)              :: distgrid
       integer,               intent(in)              :: indexArray(:,:)
       integer,               intent(in),   optional  :: distDim(:)
       integer,               intent(in),   optional  :: coordDimCount(:)
       integer,               intent(in),   optional  :: coordDimMap(:,:)
       logical,               intent(in),   optional  :: destroyDistGrid
       logical,               intent(in),   optional  :: destroyDELayout
       integer,               intent(out),  optional  :: rc
!
! !DESCRIPTION:
! This is the lower level function to create an arbitrailiy distributed {\tt ESMF\_Grid}
! object. It allows the user to fully specify the topology and index space
! (of the distributed dimensions) using the DistGrid methods and then build a grid out
! of the resulting {\tt distgrid}.  The {\tt indexArray(2,dimCount)}, 
! argument is required to specifies the topology of the grid.
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
!      distributed over DEs. 
! \item[{[indexArray]}] 
!      The minIndex and maxIndex array of size {\tt 2} x {\tt dimCount}
!      {\tt indexArray(1,:)} is the minIndex and {\tt indexArray(2,:)} is the maxIndex
! \item[{[distDim]}]
!       This array specifies which dimensions are arbitrarily distributed.
!       The size of the array specifies the total distributed dimensions.
!       if not specified, the default is that all dimensions will be arbitrarily
!       distributed.  
! \item[{[coordDimCount]}]
!      List that has dimCount elements.
!      Gives the dimension of each component (e.g. x) array. This is 
!      to allow factorization of the coordinate arrays. If not specified
!      each component is assumed to be size 1. Note, the default value is different
!      from the same argument for a non-arbitrarily distributed grid. 
! \item[{[coordDimMap]}]
!      2D list of size dimCount x dimCount. This array describes the
!      map of each coordinate array's dimensions onto the grids
!      dimensions.  {\tt coordDimMap(i,j)} is the grid dimension of the jth dimension
!      of the i'th coordinate array.  If not specified, the default value of
!      {\tt coordDimMap(i,1)} is /ESMF\_GRID\_ARBDIM/ if the ith dimension of the grid is
!      arbitrarily distributed, or {\tt i} if the ith dimension is not distributed.
!      Note that if j is bigger than {\tt coordDimCount(i)} then it's ignored.        
! \item[{[destroyDistgrid]}]
!      If true, when the Grid is destroyed the DistGrid will be destroyed also. 
!      Defaults to false. 
! \item[{[destroyDELayout]}]
!      If true, when the Grid is destroyed the DELayout will be destroyed also. 
!      Defaults to false. 
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
    integer :: localrc ! local error status
    type(ESMF_Grid) :: grid              
    integer :: nameLen 
    type(ESMF_InterfaceInt) :: minIndexArg     ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: maxIndexArg     ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: localArbIndexArg ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: distDimArg      ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: coordDimCountArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: coordDimMapArg ! Language Interface Helper Var
    integer :: intDestroyDistgrid,intDestroyDELayout
    integer :: dimCount, distDimCount, undistDimCount, dimCount1
    integer, pointer :: local1DIndices(:), localArbIndex(:,:), distSize(:)
    integer, pointer :: undistMinIndex(:), undistMaxIndex(:)
    integer, pointer :: minIndexPPatch(:,:), maxIndexPPatch(:,:)
    integer :: patchCount, localCounts
    integer, pointer :: minIndexLocal(:), maxIndexLocal(:)
    logical, pointer :: isDistDim(:)
    integer :: i, j, k, arbDim, deCount
    integer, allocatable :: distDimLocal(:)
    integer, allocatable :: collocationPDim(:)
    logical  :: arbSeqIndexFlag
    type(ESMF_DELayout) :: delayout

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

    !! find out grid dimension
    dimCount = size(indexArray,2)
    
    !! find out undistDimCount and distDimCount
    call ESMF_DistGridGet(distgrid, dimCount=dimCount1, patchCount=patchCount, &
    	rc=localrc)
    !! dimCount1 should be equal or less than dimCount
    if (dimCount1 .gt. dimCount) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                   "- distgrid dimension has to be less or equal to dimCount", & 
                          ESMF_CONTEXT, rc) 
        return 
     endif
    if (patchCount .ne. 1) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                   "- distgrid patch count has to be 1", & 
                          ESMF_CONTEXT, rc) 
        return 
    endif
    distDimCount = dimCount - dimCount1 + 1
    undistDimCount = dimCount - distDimCount

    !! distDim is a 1D array of size distDimCount.  The values are the 
    !! Grid dimensions that are arbitrarily distributed. 
    if (present(distDim)) then
      if (size(distDim) .ne. distDimCount) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                   "- dimension of distDim has to be the same as the arbitrary distributed dim", & 
                          ESMF_CONTEXT, rc) 
	return
      endif
    endif

    !! fill minIndexLocal
    allocate(minIndexLocal(dimCount), maxIndexLocal(dimCount))
    do i=1,dimCount
	  minIndexLocal(i) = indexArray(1,i)
          maxIndexLocal(i) = indexArray(2,i)
    enddo

    !! set distSize
    allocate(distSize(distDimCount))
    allocate(isDistDim(dimCount))
    allocate(distDimLocal(distDimCount))
    isDistDim(:) = .false.
    if (present(distDim)) then
	do i=1,distDimCount
	  distSize(i)=maxIndexLocal(distDim(i))-minIndexLocal(distDim(i))+1
          isDistDim(distDim(i))=.true.
	  distDimLocal(i)=distDim(i)
        enddo
    else
	do i=1,distDimCount
	  distSize(i)=maxIndexLocal(i)-minIndexLocal(i)+1
          isDistDim(i)=.true.
          distDimLocal(i)=i
        enddo
    endif

    !! Arbitrary grid indices
    minIndexArg = ESMF_InterfaceIntCreate(minIndexLocal, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    maxIndexArg = ESMF_InterfaceIntCreate(maxIndexLocal, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    !! distDim
    distDimArg = ESMF_InterfaceIntCreate(distDimLocal, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_DistGridGet(distgrid,localDe=0, elementCount=localCounts, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    !! reconstruct the localArbIndex from local1DIndices
    allocate(local1DIndices(localCounts))
    call ESMF_DistGridGet(distgrid,localDe=0, seqIndexList=local1DIndices, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    !! find out the dimension 
    allocate(localArbIndex(localCounts,distDimCount))
    
    !! I hope this is correct....
    !! This is kind of redundant.  Because if we create the grid using shapetile API, the local1DIndices
    !! were calculated by the input localArbIndex and we should not need to re-calculate the localArbIndex.
    !! We only need this when user creates an arbitrary grid from a distgrid.  The question is (1) do we need
    !! to store the localArbIndex in the Grid data structure or not?  (2) shall we allow user to pass localArbIndex
    !! to the ESMF_CreateGridFromDistGrid()?  If we do, we have to check if the distgrid indices matches with
    !! the input localArbIndex
    do i=1,localCounts
      !! make it 0-based first before calculations
      local1DIndices(i)=local1DIndices(i)-1
      if (distDimCount .ge. 2) then
        do j=distDimCount,2	
          !! add 1 to make the result 1-based
	  localArbIndex(i,j) = mod(local1DIndices(i),distSize(j))+1
          local1DIndices(i)=local1DIndices(i)/distSize(j)
        enddo
      endif    
      localArbIndex(i,1) = local1DIndices(i)+1
    enddo

    localArbIndexArg = ESMF_InterfaceIntCreate(farray2D=localArbIndex, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    !! Check the non-arbitrary dimensions in DistGrid and make sure they are
    !! consistent with the minIndex and maxIndex 
    !! First, find out which dimension in DistGrid is arbitrary
    arbDim = -1
    call ESMF_DistGridGet(distgrid, delayout=delayout, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_DELayoutGet(delayout, localDeCount=deCount, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    if (deCount .gt. 0) then
      allocate(collocationPDim(dimCount1))  ! dimCount
      call ESMF_DistGridGet(distgrid,   &
           collocationPDim=collocationPDim, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

      do i=1,dimCount1
          call ESMF_DistGridGet(distgrid, localDe=0, collocation=collocationPDim(i), &
              arbSeqIndexFlag=arbSeqIndexFlag, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
          if (arbSeqIndexFlag) arbDim = i
      enddo
      deallocate(collocationPDim)
    endif

    if (arbDim .eq. -1) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                   "- distgrid should contain arbitrary sequence indices", & 
                          ESMF_CONTEXT, rc) 
	return
    endif

    if (undistDimCount .ne. 0) then
      allocate(minIndexPPatch(dimCount1,1))
      allocate(maxIndexPPatch(dimCount1,1))
      call ESMF_DistGridGet(distgrid, minIndexPDimPPatch=minIndexPPatch, &
	  maxIndexPDimPPatch=maxIndexPPatch, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

      allocate(undistMinIndex(undistDimCount))
      allocate(undistMaxIndex(undistDimCount))
      k = 1
      do j=1,dimCount
         if (.not. isDistDim(j)) then
	   undistMinIndex(k) = minIndexLocal(j)
	   undistMaxIndex(k) = maxIndexLocal(j)
           k = k+1
         endif
      enddo

      k = 1
      do i=1,dimCount1
        if (arbDim .ne. i) then
          if ((undistMinIndex(k) .ne. minIndexPPatch(i,1)) .or. &
            (undistMaxIndex(k) .ne. maxIndexPPatch(i,1))) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- Grid min/max index does not match with DistGrid min/max index", & 
               ESMF_CONTEXT, rc) 
            return
          endif
          k = k + 1
	endif
      enddo
    endif

    !! Description of array factorization
    coordDimCountArg = ESMF_InterfaceIntCreate(coordDimCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    coordDimMapArg = ESMF_InterfaceIntCreate(farray2D=coordDimMap, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
	
    !! Convert destroyDistGrid flag
    if (present(destroyDistgrid)) then
        if (destroyDistgrid) then
           intDestroyDistgrid=1
        else
           intDestroyDistgrid=0
         endif
    else
           intDestroyDistgrid=0
    endif

    !! Convert destroyDELayout flag
    if (present(destroyDELayout)) then
        if (destroyDELayout) then
           intDestroyDELayout=1
        else
           intDestroyDELayout=0
         endif
    else
           intDestroyDELayout=0
    endif

    ! Initialize this grid object as invalid
    grid%this = ESMF_NULL_POINTER

    ! Call C++ Subroutine to do the create         
    call c_ESMC_gridcreatedistgridarb(grid%this, nameLen, name, &
      coordTypeKind, distgrid, distDimArg, arbDim, &
      coordDimCountArg, coordDimMapArg, &
      minIndexArg, maxIndexArg, localArbIndexArg, localCounts, &
      intDestroyDistGrid, intDestroyDELayout, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deallocate helper variables
    deallocate(minIndexLocal)
    deallocate(maxIndexLocal)
    deallocate(distSize)
    deallocate(isDistDim)
    deallocate(distDimLocal)
    deallocate(local1DIndices)
    deallocate(localArbIndex)
    if (undistDimCount .ne. 0) then
      deallocate(minIndexPPatch)
      deallocate(maxIndexPPatch)
      deallocate(undistMinIndex)
      deallocate(undistMaxIndex)
    endif

    call ESMF_InterfaceIntDestroy(distDimArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(minIndexArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(maxIndexArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(localArbIndexArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(coordDimCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(coordDimMapArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set return value
    ESMF_GridCreateFromDistGridArb = grid

    ! Set init status
    ESMF_INIT_SET_CREATED(ESMF_GridCreateFromDistGridArb)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

    end function ESMF_GridCreateFromDistGridArb


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreateFromFile"
!BOP
! !IROUTINE: ESMF_GridCreate - Create a Grid from a file

! !INTERFACE:
  ! Private name; call using ESMF_GridCreate()
     function ESMF_GridCreateFromFile(fileName, convention, purpose, rc)
!
! !RETURN VALUE:
     type(ESMF_Grid) :: ESMF_GridCreateFromFile
!
! !ARGUMENTS:
       character (len=*), intent(in)            :: fileName
       character (len=*), intent(in),  optional :: convention
       character (len=*), intent(in),  optional :: purpose
       integer,           intent(out), optional :: rc
!
! !DESCRIPTION:
! Create an {\tt ESMF\_Grid} object from specifications in a file 
! containing an ESMF GridSpec Attribute package in XML format. Currently limited
! to creating a 2D regularly distributed rectilinear Grid; in the future more
! dimensions, grid types and distributions will be supported.
! See Section~\ref{example:GridCrFromFile} for an example, as well as the
! accompanying file
! ESMF\_DIR/src/Infrastructure/Grid/etc/esmf\_grid\_shape\_tile.xml.
!
! Requires the third party Xerces C++ XML Parser library to be installed.
! For more details, see the "ESMF Users Guide",
! "Building and Installing the ESMF, Third Party Libraries, Xerces" and
! the website http://xerces.apache.org/xerces-c.
!
! The arguments are:
! \begin{description}
! \item[fileName] 
!      The name of the XML file to be read, containing ESMF GridSpec Attributes.
! \item [{[convention]}] 
!      The convention of a grid Attribute package. [CURRENTLY NOT IMPLEMENTED]
! \item [{[purpose]}] 
!      The purpose of a grid Attribute package.    [CURRENTLY NOT IMPLEMENTED]
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      Equals {\tt ESMF\_RC\_LIB\_NOT\_PRESENT} if Xerces is not present.
! \end{description}
!
!EOP

    type(ESMF_Grid) :: grid 
    character(ESMF_MAXSTR) :: attrvalue
    integer :: maxIndex(2), regDecomp(2)  ! TODO: allow more dimensions
    integer :: fileNameLen, localrc
    logical :: xercesPresent

    ! Initialize return code; assume failure until success is certain 
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following test when dummy argument actually used
    if (present(convention)) then
       if (convention==convention) continue;
    endif

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following test when dummy argument actually used
    if (present(purpose)) then
    	if (purpose==purpose) continue;
    endif

    ! get length of given fileName for C++ validation
    fileNameLen = len_trim(fileName)

    ! assume Xerces XML C++ API library present until proven otherwise
    xercesPresent = .true.

    ! Initialize this grid object as invalid
    grid%this = ESMF_NULL_POINTER

    grid = ESMF_GridCreateEmpty(rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                              ESMF_CONTEXT, rcToReturn=rc)) then
      call ESMF_GridDestroy(grid)
      return
    endif

    ! Read the attribute file; place attributes onto grid base
    ! TODO: use convention, purpose
    ! use C call rather than F90, to circumvent mutually dependency
    ! between Grid and Attribute
    call c_ESMC_AttributeRead(grid, fileNameLen, fileName, localrc)

    if (localrc==ESMF_RC_LIB_NOT_PRESENT) xercesPresent = .false.
    if (localrc .ne. ESMF_SUCCESS .and. xercesPresent) localrc = ESMF_FAILURE
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                              ESMF_CONTEXT, rcToReturn=rc)) then
      call ESMF_GridDestroy(grid)
      return
    endif

    ! Get the GridSpec "NX" and "NY" Attributes set from file
    ! (required, for maxIndex in GriCreate())
    ! use C calls rather than F90, to circumvent mutually dependency
    ! between Grid and Attribute
    call c_ESMC_AttPackGetChar(grid, 'NX', attrValue, &
                               'GridSpec', 'General', 'grid', localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                              ESMF_CONTEXT, rcToReturn=rc)) then
      call ESMF_GridDestroy(grid)
      return
    endif

    ! convert from character to integer
    read(attrValue, *, iostat=localrc) maxIndex(1)
    if (localrc.ne.0) then
      call ESMF_LogMsgSetError(ESMF_RC_VAL_WRONG, ESMF_ERR_PASSTHRU, &
                               ESMF_CONTEXT, rcToReturn=rc)
      call ESMF_GridDestroy(grid)
      return
    end if

    call c_ESMC_AttPackGetChar(grid, 'NY', attrValue, &
                               'GridSpec', 'General', 'grid', localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                              ESMF_CONTEXT, rcToReturn=rc)) then
      call ESMF_GridDestroy(grid)
      return
    endif

    ! convert from character to integer
    read(attrValue, *, iostat=localrc) maxIndex(2)
    if (localrc.ne.0) then
      call ESMF_LogMsgSetError(ESMF_RC_VAL_WRONG, ESMF_ERR_PASSTHRU, &
                               ESMF_CONTEXT, rcToReturn=rc)
      call ESMF_GridDestroy(grid)
      return
    end if

    ! Get the ESMF "RegDecompX" and "RegDecompY" Attributes set from file
    ! TODO:  make optional
    ! use C calls rather than F90, to circumvent mutually dependency
    ! between Grid and Attribute
    call c_ESMC_AttPackGetChar(grid, 'RegDecompX', attrValue, &
                               'ESMF', 'General', 'grid', localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                              ESMF_CONTEXT, rcToReturn=rc)) then
      call ESMF_GridDestroy(grid)
      return
    endif

    ! convert from character to integer
    read(attrValue, *, iostat=localrc) regDecomp(1)
    if (localrc.ne.0) then
      call ESMF_LogMsgSetError(ESMF_RC_VAL_WRONG, ESMF_ERR_PASSTHRU, &
                               ESMF_CONTEXT, rcToReturn=rc)
      call ESMF_GridDestroy(grid)
      return
    end if

    call c_ESMC_AttPackGetChar(grid, 'RegDecompY', attrValue, &
                               'ESMF', 'General', 'grid', localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                              ESMF_CONTEXT, rcToReturn=rc)) then
      call ESMF_GridDestroy(grid)
      return
    endif

    ! convert from character to integer
    read(attrValue, *, iostat=localrc) regDecomp(2)
    if (localrc.ne.0) then
      call ESMF_LogMsgSetError(ESMF_RC_VAL_WRONG, ESMF_ERR_PASSTHRU, &
                               ESMF_CONTEXT, rcToReturn=rc)
      call ESMF_GridDestroy(grid)
      return
    end if

    ! Create single tile grid with global indices, and with specified 
    ! regular distribution
    ! TODO:  when RegDecompX,Y optional and not specified, don't pass regDecomp
    call ESMF_GridSetCommitShapeTile(grid, maxIndex=maxIndex, &
                                     regDecomp=regDecomp, &
                                     indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                              ESMF_CONTEXT, rcToReturn=rc)) then
      call ESMF_GridDestroy(grid)
      return
    endif

    ! TODO: Add coordinates
    ! call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)

    ! Set return value
    ESMF_GridCreateFromFile = grid

    ! Set init status
    ESMF_INIT_SET_CREATED(ESMF_GridCreateFromFile)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

    end function ESMF_GridCreateFromFile

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
! internal structures. The {\tt ESMF\_GridSetCommitShapeTile} calls
! can be used to set the values in the grid object and to construct the 
! internal structure. 
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
                        gridEdgeLWidth, gridEdgeUWidth, gridAlign, &
                        gridMemLBound, indexflag, petMap, rc)
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
       integer,               intent(in),   optional  :: gridEdgeLWidth(:)
       integer,               intent(in),   optional  :: gridEdgeUWidth(:)
       integer,               intent(in),   optional  :: gridAlign(:)
       integer,               intent(in),   optional  :: gridMemLBound(:)
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
! that dimension.  The dimCount of the grid is equal to the number of 
! countsPerDEDim<> arrays that are specified. 
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
! \item[{countsPerDEDim2}] 
!     This array specifies the number of cells per DE for index dimension 2
!     for the exclusive region (center stagger location). 
! \item[{[countsPerDEDim3]}] 
!     This array specifies the number of cells per DE for index dimension 3
!     for the exclusive region (center stagger location).  
!     If not specified  then grid is 2D. 
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
!     arrays map to. If not present the default is 1,2,...,grid rank. 
! \item[{[coordDep2]}] 
!     This array specifies the dependence of the second 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the second
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is 1,2,...,grid rank. 
! \item[{[coordDep3]}] 
!     This array specifies the dependence of the third 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the third
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is 1,2,...,grid rank. 
! \item[{[gridEdgeLWidth]}] 
!      The padding around the lower edges of the grid. This padding is between
!      the index space corresponding to the cells and the boundary of the 
!      the exclusive region. This extra space is to contain the extra
!      padding for non-center stagger locations, and should be big enough
!      to hold any stagger in the grid. 
! \item[{[gridEdgeUWidth]}] 
!      The padding around the upper edges of the grid. This padding is between
!      the index space corresponding to the cells and the boundary of the 
!      the exclusive region. This extra space is to contain the extra
!      padding for non-center stagger locations, and should be big enough
!      to hold any stagger in the grid. 
! \item[{[gridAlign]}] 
!     Specification of how the stagger locations should align with the cell
!     index space (can be overridden by the individual staggerAligns). If
!     the {\tt gridEdgeWidths} are not specified than this parameter
!     implies the EdgeWidths.
! \item[{[gridMemLBound]}] 
!      Specifies the lower index range of the memory of every DE in this Grid. 
!      Only used when indexflag is {\tt ESMF\_INDEX\_USER}. May be overridden
!      by staggerMemLBound. 
! \item[{[indexflag]}]
!      Indicates the indexing scheme to be used in the new Grid. Please see
!      Section~\ref{opt:indexflag} for the list of options. If not present,
!      defaults to ESMF\_INDEX\_DELOCAL.
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
    integer, pointer     :: coordDimCount(:)
    integer, pointer     :: coordDimMap(:,:)
    integer              :: localrc
    integer              :: dimCount,i,maxSizeDEDim
    integer, pointer     :: distgridToGridMap(:), deDimCount(:)
    integer, pointer     :: minIndexLocal(:)
    integer, pointer     :: maxIndexLocal(:)
    integer, pointer     :: gridEdgeLWidthLocal(:)
    integer, pointer     :: gridEdgeUWidthLocal(:)
    integer, pointer     :: gridAlignLocal(:)
    integer, pointer     :: countsPerDEDim1Local(:)
    integer, pointer     :: countsPerDEDim2Local(:)
    integer, pointer     :: countsPerDEDim3Local(:)
    integer, pointer     :: deBlockList(:,:,:),minPerDEDim(:,:),maxPerDEDim(:,:)
    integer              :: deCount
    integer              :: d,i1,i2,i3,k
    type(ESMF_GridConn)  :: connDim1Local(2)
    type(ESMF_GridConn)  :: connDim2Local(2)
    type(ESMF_GridConn)  :: connDim3Local(2)
    integer              :: top

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following test when dummy argument actually used
    if (present(polestaggerloc1)) then
    	if (polestaggerloc1(1)==polestaggerloc1(1)) continue;
    endif

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following test when dummy argument actually used
    if (present(polestaggerloc2)) then
    	if (polestaggerloc2(1)==polestaggerloc2(1)) continue;
    endif

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following test when dummy argument actually used
    if (present(bipolepos1)) then
    	if (bipolepos1(1)==bipolepos1(1)) continue;
    endif

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following test when dummy argument actually used
    if (present(bipolepos2)) then
    	if (bipolepos2(1)==bipolepos2(1)) continue;
    endif



    ! Compute the Grid DimCount and Derivatives ---------------------------------------------------
    ! dimCount
    if (present(countsPerDEDim3)) then
	dimCount=3
    else
	dimCount=2
    endif

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

    if ((dimCount .lt. 3) .and. present(connDim3)) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- connDim3 not allowed when grid is less than dimCount 3", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if ((dimCount .lt. 3) .and. present(poleStaggerLoc3)) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- poleStaggerLoc3 not allowed when grid is less than dimCount 3", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if ((dimCount .lt. 3) .and. present(bipolePos3)) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- bipolePos3 not allowed when grid is less than dimCount 3", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif


    if ((dimCount .lt. 3) .and. present(coordDep3)) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- coordDep3 not allowed when grid is less than dimCount 3", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if (present(coordDep1)) then
       if ((size(coordDep1) < 1) .or. (size(coordDep1)>dimCount)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- coordDep1 size incompatible with grid dimCount", & 
               ESMF_CONTEXT, rc) 
          return 
       endif
    endif

    if (present(coordDep2)) then
       if ((size(coordDep2) < 1) .or. (size(coordDep2)>dimCount)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- coordDep2 size incompatible with grid dimCount", & 
               ESMF_CONTEXT, rc) 
          return 
       endif
    endif

    if (present(coordDep3)) then
       if ((size(coordDep3) < 1) .or. (size(coordDep3)>dimCount)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- coordDep3 size incompatible with grid dimCount", & 
               ESMF_CONTEXT, rc) 
          return 
       endif
    endif

    if (present(minIndex)) then
       if (size(minIndex) .ne. dimCount) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- minIndex size must equal grid dimCount", & 
               ESMF_CONTEXT, rc) 
          return 
       endif
    endif


    if (present(petMap)) then
       if (dimCount .gt. 2) then
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



    ! Check DimCount of gridWidths and Aligns
    if (present(gridEdgeLWidth)) then
        if (size(gridEdgeLWidth) .ne. dimCount) then
           call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
                     "- gridEdgeLWidth must be of size equal to Grid dimCount", & 
                     ESMF_CONTEXT, rc) 
              return
        endif 
    endif

    if (present(gridEdgeUWidth)) then
        if (size(gridEdgeUWidth) .ne. dimCount) then
           call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
                     "- gridEdgeUWidth must be of size equal to Grid dimCount", & 
                     ESMF_CONTEXT, rc) 
              return
        endif 
    endif

    if (present(gridAlign)) then
        if (size(gridAlign) .ne. dimCount) then
           call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
                     "- gridAlign must be of size equal to Grid dimCount", & 
                     ESMF_CONTEXT, rc) 
              return
        endif 
    endif

   ! make sure connected dimensions don't have an edge width
   if (present(connDim1)) then
      if (size(connDim1) .eq. 1) then
         if (connDim1(1) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeLWidth)) then
               if (gridEdgeLWidth(1) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have LWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
            if (present(gridEdgeUWidth)) then
               if (gridEdgeUWidth(1) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have UWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
      else if (size(connDim1) .eq. 2) then
         if (connDim1(1) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeLWidth)) then
               if (gridEdgeLWidth(1) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have LWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
         if (connDim1(2) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeUWidth)) then
               if (gridEdgeUWidth(1) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have UWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
      endif
   endif

   ! make sure connected dimensions don't have an edge width
   if (present(connDim2)) then
      if (size(connDim2) .eq. 1) then
         if (connDim2(1) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeLWidth)) then
               if (gridEdgeLWidth(2) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have LWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
            if (present(gridEdgeUWidth)) then
               if (gridEdgeUWidth(2) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have UWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
      else if (size(connDim2) .eq. 2) then
         if (connDim2(1) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeLWidth)) then
               if (gridEdgeLWidth(2) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have LWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
         if (connDim2(2) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeUWidth)) then
               if (gridEdgeUWidth(2) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have UWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
      endif
   endif


   ! make sure connected dimensions don't have an edge width
   if (present(connDim3)) then
      if (size(connDim3) .eq. 1) then
         if (connDim3(1) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeLWidth)) then
               if (gridEdgeLWidth(3) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have LWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
            if (present(gridEdgeUWidth)) then
               if (gridEdgeUWidth(3) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have UWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
      else if (size(connDim3) .eq. 2) then
         if (connDim3(1) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeLWidth)) then
               if (gridEdgeLWidth(3) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have LWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
         if (connDim3(2) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeUWidth)) then
               if (gridEdgeUWidth(3) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have UWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
      endif
   endif

   ! check for gridMemLBound issues
   if (present(gridMemLBound)) then
      if (.not. present(indexflag)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                "- when using gridMemLBound must specify indexflag=ESMF_INDEX_USER ", & 
                 ESMF_CONTEXT, rc) 
              return
      else if (.not. (indexflag .eq. ESMF_INDEX_USER)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                "- when using gridMemLBound must specify indexflag=ESMF_INDEX_USER ", & 
                 ESMF_CONTEXT, rc) 
              return
      endif
   else
      if (present(indexflag)) then
         if (indexflag .eq. ESMF_INDEX_USER) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                "- when using indexflag=ESMF_INDEX_USER must provide gridMemLBound ", & 
                   ESMF_CONTEXT, rc) 
              return
         endif
      endif
   endif


   ! Check for non-valid connection types here

   !TODO: Consider making some of these a separate local subroutine (particularly if you're going to 
   !      have 3 of these ShapeCreate subroutines with only minor changes


    ! Copy vales for countsPerDEDim --------------------------------------------
    allocate(countsPerDEDim1Local(size(countsPerDEDim1)), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating countsPerDEDim1Local", &
                                     ESMF_CONTEXT, rc)) return
    countsPerDEDim1Local=countsPerDEDim1

    allocate(countsPerDEDim2Local(size(countsPerDEDim2)), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating countsPerDEDim2Local", &
                                     ESMF_CONTEXT, rc)) return
    countsPerDEDim2Local=countsPerDEDim2

    if (dimCount .gt. 2) then
       allocate(countsPerDEDim3Local(size(countsPerDEDim3)), stat=localrc)
       if (ESMF_LogMsgFoundAllocError(localrc, "Allocating countsPerDEDim3Local", &
                                      ESMF_CONTEXT, rc)) return
       countsPerDEDim3Local=countsPerDEDim3
    endif


    ! Set Defaults -------------------------------------------------------------

    ! Set default for minIndex 
    allocate(minIndexLocal(dimCount), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating minIndexLocal", &
                                     ESMF_CONTEXT, rc)) return

    if (present(minIndex)) then
       minIndexLocal(:)=minIndex(:)
    else
       do i=1,dimCount
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
       connDim1Local(1)=ESMF_GRIDCONN_NONE ! if not present then default to no connection
       connDim1Local(2)=ESMF_GRIDCONN_NONE
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
       connDim2Local(1)=ESMF_GRIDCONN_NONE ! if not present then default to no connection
       connDim2Local(2)=ESMF_GRIDCONN_NONE
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
       connDim3Local(1)=ESMF_GRIDCONN_NONE ! if not present then default to no connection
       connDim3Local(2)=ESMF_GRIDCONN_NONE
    endif


    ! check for not implemented functionality
    if (connDim1Local(1) .ne. ESMF_GRIDCONN_NONE .or. &
        connDim1Local(2) .ne. ESMF_GRIDCONN_NONE) then
       call ESMF_LogMsgSetError(ESMF_RC_NOT_IMPL, & 
                 "- Only ESMF_GRIDCONN_NONE Grid connection implemented so far", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if (connDim2Local(1) .ne. ESMF_GRIDCONN_NONE .or. &
        connDim2Local(2) .ne. ESMF_GRIDCONN_NONE) then
       call ESMF_LogMsgSetError(ESMF_RC_NOT_IMPL, & 
                 "- Only ESMF_GRIDCONN_NONE Grid connection implemented so far", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if (connDim3Local(1) .ne. ESMF_GRIDCONN_NONE .or. &
        connDim3Local(2) .ne. ESMF_GRIDCONN_NONE) then
       call ESMF_LogMsgSetError(ESMF_RC_NOT_IMPL, & 
                 "- Only ESMF_GRIDCONN_NONE Grid connection implemented so far", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif



   ! Make alterations to size due to GridEdgeWidths ----------------------------
    allocate(gridEdgeLWidthLocal(dimCount), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating gridEdgeLWidthLocal", &
                                     ESMF_CONTEXT, rc)) return
    allocate(gridEdgeUWidthLocal(dimCount), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating gridEdgeUWidthLocal", &
                                     ESMF_CONTEXT, rc)) return
    allocate(gridAlignLocal(dimCount), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating gridAlignLocal", &
                                     ESMF_CONTEXT, rc)) return

    call ESMF_GridLUADefault(dimCount, &
                             gridEdgeLWidth, gridEdgeUWidth, gridAlign, &
                             gridEdgeLWidthLocal, gridEdgeUWidthLocal, gridAlignLocal, &
                             rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


#if 0
    ! Modify lower bound
    do i=1,dimCount
       minIndexLocal(i)=minIndexLocal(i)-gridEdgeLWidthLocal(i)
    enddo

    ! Modify lower size
    countsPerDEDim1Local(1)=countsPerDEDim1Local(1)+gridEdgeLWidthLocal(1)

    countsPerDEDim2Local(1)=countsPerDEDim2Local(1)+gridEdgeLWidthLocal(2)
  
    if (dimCount .gt. 2) then
       countsPerDEDim3Local(1)=countsPerDEDim3Local(1)+gridEdgeLWidthLocal(3)
    endif


    ! Modify upper size
    top=size(countsPerDEDim1Local)
    countsPerDEDim1Local(top)=countsPerDEDim1Local(top)+gridEdgeUWidthLocal(1)

    top=size(countsPerDEDim2Local)
    countsPerDEDim2Local(top)=countsPerDEDim2Local(top)+gridEdgeUWidthLocal(2)
  
    if (dimCount .gt. 2) then
       top=size(countsPerDEDim3Local)
       countsPerDEDim3Local(top)=countsPerDEDim3Local(top)+gridEdgeUWidthLocal(3)
    endif
#endif


   ! Calc minIndex,maxIndex,distgridToGridMap for DistGrid -----------------------------------
    ! Set default for maxIndex 
    allocate(maxIndexLocal(dimCount), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating maxIndexLocal", &
                                     ESMF_CONTEXT, rc)) return

    maxIndexLocal(1)=sum(countsPerDEDim1Local)+minIndexLocal(1)-1
    maxIndexLocal(2)=sum(countsPerDEDim2Local)+minIndexLocal(2)-1

    if (dimCount .gt. 2) then
      maxIndexLocal(3)=sum(countsPerDEDim3Local)+minIndexLocal(3)-1
    endif


   allocate(distgridToGridMap(dimCount), stat=localrc)
   if (ESMF_LogMsgFoundAllocError(localrc, "Allocating distgridToGridMap", &
               ESMF_CONTEXT, rc)) return
   do i=1,dimCount
     distgridToGridMap(i)=i
   enddo    

  ! Setup deBlockList for DistGrid ------------------------------------------------
  ! count de blocks
  deCount=1
  deCount=deCount*size(countsPerDEDim1Local) 
  deCount=deCount*size(countsPerDEDim2Local)
  if (dimCount .gt. 2) then
     deCount=deCount*size(countsPerDEDim3Local)
  endif 
 
  ! Calc the max size of a DEDim
  maxSizeDEDim=1
  if (size(countsPerDEDim1Local) .gt. maxSizeDEDim) then
      maxSizeDEDim=size(countsPerDEDim1Local)
  endif
  if (size(countsPerDEDim2Local) .gt. maxSizeDEDim) then
      maxSizeDEDim=size(countsPerDEDim2Local)
  endif
  if (dimCount .gt. 2) then
      if (size(countsPerDEDim3Local) .gt. maxSizeDEDim) then
         maxSizeDEDim=size(countsPerDEDim3Local)
      endif
  endif
  

  ! generate deblocklist
  allocate(maxPerDEDim(dimCount,maxSizeDEDim), stat=localrc)
  if (ESMF_LogMsgFoundAllocError(localrc, "Allocating maxPerDEDim", &
              ESMF_CONTEXT, rc)) return
  allocate(minPerDEDim(dimCount,maxSizeDEDim), stat=localrc)
  if (ESMF_LogMsgFoundAllocError(localrc, "Allocating minPerDEDim", &
              ESMF_CONTEXT, rc)) return
 allocate(deDimCount(dimCount), stat=localrc)
  if (ESMF_LogMsgFoundAllocError(localrc, "Allocating maxPerDEDim", &
              ESMF_CONTEXT, rc)) return


  ! Calc the maximum end of each DE in a Dim, and the size of each DEDim
  d=1
  deDimCount(d)=size(countsPerDEDim1Local)
  minPerDeDim(d,1)=minIndexLocal(d)
  maxPerDeDim(d,1)=minIndexLocal(d)+countsPerDEDim1Local(1)-1
  do i=2,deDimCount(d) 
     minPerDEDim(d,i)=maxPerDEDim(d,i-1)+1
     maxPerDEDim(d,i)=minPerDEDim(d,i)+countsPerDEDim1Local(i)-1
  enddo

  d=2
  deDimCount(d)=size(countsPerDEDim2Local)
  minPerDeDim(d,1)=minIndexLocal(d)
  maxPerDeDim(d,1)=minIndexLocal(d)+countsPerDEDim2Local(1)-1
  do i=2,deDimCount(d) 
     minPerDEDim(d,i)=maxPerDEDim(d,i-1)+1
     maxPerDEDim(d,i)=minPerDEDim(d,i)+countsPerDEDim2Local(i)-1
  enddo

  if (dimCount .gt. 2) then
    d=3
    deDimCount(d)=size(countsPerDEDim3Local)
    minPerDeDim(d,1)=minIndexLocal(d)
    maxPerDeDim(d,1)=minIndexLocal(d)+countsPerDEDim3Local(1)-1
    do i=2,deDimCount(d) 
       minPerDEDim(d,i)=maxPerDEDim(d,i-1)+1
       maxPerDEDim(d,i)=minPerDEDim(d,i)+countsPerDEDim3Local(i)-1
    enddo
  endif

  ! allocate deblocklist
  allocate(deBlockList(dimCount,2,deCount), stat=localrc)
  if (ESMF_LogMsgFoundAllocError(localrc, "Allocating deBlockList", &
              ESMF_CONTEXT, rc)) return

  ! Fill in DeBlockList
  if (dimCount .eq. 2) then
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
  else if (dimCount .eq. 3) then
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
      if (dimCount .gt. 2) then
	 k=1
     	 do i3=1,size(countsPerDEDim3Local)
         do i2=1,size(countsPerDEDim2Local)
         do i1=1,size(countsPerDEDim1Local)
            petList(k)=petMap(i1,i2,i3)
            k=k+1
         enddo
         enddo
         enddo
      else 
	 k=1
     	 do i3=1,1
         do i2=1,size(countsPerDEDim2Local)
         do i1=1,size(countsPerDEDim1Local)
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
    distgrid=ESMF_DistGridCreate(minIndex=minIndexLocal, maxIndex=maxIndexLocal, &
               deBlockList=deBlockList, delayout=delayout, indexflag=indexflag, rc=localrc)   
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

   ! Convert coordDeps to coordDimCount and coordDimMap -------------------------------
   allocate(coordDimCount(dimCount), stat=localrc)
   if (ESMF_LogMsgFoundAllocError(localrc, "Allocating coordDimCount", &
              ESMF_CONTEXT, rc)) return
   allocate(coordDimMap(dimCount,dimCount), stat=localrc)
   if (ESMF_LogMsgFoundAllocError(localrc, "Allocating coordDimMap", &
              ESMF_CONTEXT, rc)) return

   if (present(coordDep1)) then
      coordDimCount(1)=size(coordDep1)
      coordDimMap(1,:)=0
      do i=1,size(coordDep1)
         coordDimMap(1,i)=coordDep1(i)
      enddo
   else 
      coordDimCount(1)=dimCount
      do i=1,dimCount
         coordDimMap(1,i)=i      
      enddo
   endif

   if (present(coordDep2)) then
      coordDimCount(2)=size(coordDep2)
      coordDimMap(2,:)=0
      do i=1,size(coordDep2)
         coordDimMap(2,i)=coordDep2(i)
      enddo
   else 
      coordDimCount(2)=dimCount
      do i=1,dimCount
         coordDimMap(2,i)=i      
      enddo
   endif

   if (dimCount .gt. 2) then
      if (present(coordDep3)) then 
         coordDimCount(3)=size(coordDep3)
          coordDimMap(3,:)=0
          do i=1,size(coordDep3)
             coordDimMap(3,i)=coordDep3(i)
          enddo
      else 
        coordDimCount(3)=dimCount
        do i=1,dimCount
	   coordDimMap(3,i)=i      
        enddo
      endif
   endif

  
   ! Create Grid from specification -----------------------------------------------
   ESMF_GridCreateShapeTileIrreg=ESMF_GridCreateFromDistGrid(name, coordTypeKind, &
                                    distgrid=distgrid, distgridToGridMap=distgridToGridMap, &
                                    coordDimCount=coordDimCount, coordDimMap=coordDimMap, &
                                    gridEdgeLWidth=gridEdgeLWidthLocal, &
                                    gridEdgeUWidth=gridEdgeUWidthLocal, &
                                    gridAlign=gridAlignLocal, &
				    gridMemLBound=gridMemLBound, &
                                    indexflag=indexflag, & 
                                    destroyDistGrid=.true., &
                                    destroyDELayout=.true., &
                                   rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Clean up memory
    deallocate(coordDimCount)
    deallocate(coordDimMap)
    deallocate(minIndexLocal)
    deallocate(maxIndexLocal)
    deallocate(distgridToGridMap)
    deallocate(maxPerDEDim)
    deallocate(minPerDEDim)
    deallocate(deDimCount)
    deallocate(deBlockList)
    deallocate(gridEdgeLWidthLocal)
    deallocate(gridEdgeUWidthLocal)
    deallocate(gridAlignLocal)
    deallocate(countsPerDEDim1Local) 
    deallocate(countsPerDEDim2Local) 
    if (dimCount .gt. 2) then
       deallocate(countsPerDEDim3Local) 
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
                        gridEdgeLWidth, gridEdgeUWidth, gridAlign, &
                        gridMemLBound, indexflag, petMap, rc)


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
       integer,               intent(in),   optional  :: gridEdgeLWidth(:)
       integer,               intent(in),   optional  :: gridEdgeUWidth(:)
       integer,               intent(in),   optional  :: gridAlign(:)
       integer,               intent(in),   optional  :: gridMemLBound(:)
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
! dimension into. The array {\tt decompFlag} indicates how the division into DEs is to
! occur.  The default is to divide the range as evenly as possible.
!
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
!      is {\tt ESMF\_DECOMP\_HOMOGEN} in all dimensions. Please see
!      Section~\ref{opt:decompflag} for a full description of the 
!      possible options. 
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
!     arrays map to. If not present the default is 1,2,...,grid rank. 
! \item[{[coordDep2]}] 
!     This array specifies the dependence of the second 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the second
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is 1,2,...,grid rank.  
! \item[{[coordDep3]}] 
!     This array specifies the dependence of the third 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the third
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is 1,2,...,grid rank.  
! \item[{[gridEdgeLWidth]}] 
!      The padding around the lower edges of the grid. This padding is between
!      the index space corresponding to the cells and the boundary of the 
!      the exclusive region. This extra space is to contain the extra
!      padding for non-center stagger locations, and should be big enough
!      to hold any stagger in the grid. 
! \item[{[gridEdgeUWidth]}] 
!      The padding around the upper edges of the grid. This padding is between
!      the index space corresponding to the cells and the boundary of the 
!      the exclusive region. This extra space is to contain the extra
!      padding for non-center stagger locations, and should be big enough
!      to hold any stagger in the grid. 
! \item[{[gridAlign]}] 
!     Specification of how the stagger locations should align with the cell
!     index space (can be overridden by the individual staggerAligns). If
!     the {\tt gridEdgeWidths} are not specified than this parameter
!     implies the EdgeWidths.
! \item[{[gridMemLBound]}] 
!      Specifies the lower index range of the memory of every DE in this Grid. 
!      Only used when indexflag is {\tt ESMF\_INDEX\_USER}. May be overridden
!      by staggerMemLBound. 
! \item[{[indexflag]}]
!      Indicates the indexing scheme to be used in the new Grid. Please see
!      Section~\ref{opt:indexflag} for the list of options. If not present,
!      defaults to ESMF\_INDEX\_DELOCAL.
! \item[{[petMap]}]
!       Sets the mapping of pets to the created DEs. This 3D
!       should be of size regDecomp(1) x regDecomp(2) x regDecomp(3)
!       If the Grid is 2D, then the last dimension is of size 1.   
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
    type(ESMF_DistGrid)  :: distgrid
    type(ESMF_DELayout)  :: delayout
    type(ESMF_VM)        :: vm
    integer, pointer     :: petList(:)
    integer, pointer     :: coordDimCount(:)
    integer, pointer     :: coordDimMap(:,:)
    integer              :: localrc
    integer              :: dimCount,i
    integer, pointer     :: regDecompLocal(:)
    type(ESMF_DecompFlag), pointer :: decompflagLocal(:)
    integer, pointer     :: distgridToGridMap(:)
    integer, pointer     :: minIndexLocal(:), maxIndexLocal(:)
    integer, pointer     :: gridEdgeLWidthLocal(:)
    integer, pointer     :: gridEdgeUWidthLocal(:)
    integer, pointer     :: gridAlignLocal(:)
    integer              :: deCount
    integer              :: i1,i2,i3,k
    type(ESMF_GridConn)  :: connDim1Local(2)
    type(ESMF_GridConn)  :: connDim2Local(2)
    type(ESMF_GridConn)  :: connDim3Local(2)

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following test when dummy argument actually used
    if (present(polestaggerloc1)) then
    	if (polestaggerloc1(1)==polestaggerloc1(1)) continue;
    endif

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following test when dummy argument actually used
    if (present(polestaggerloc2)) then
    	if (polestaggerloc2(1)==polestaggerloc2(1)) continue;
    endif

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following test when dummy argument actually used
    if (present(bipolepos1)) then
    	if (bipolepos1(1)==bipolepos1(1)) continue;
    endif

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following test when dummy argument actually used
    if (present(bipolepos2)) then
    	if (bipolepos2(1)==bipolepos2(1)) continue;
    endif


    ! Compute the Grid DimCount and Derivatives ---------------------------------------------------
    ! dimCount
    dimCount=size(maxIndex)
    if ((dimCount < 2) .or. (dimCount > 3)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- maxIndex size and thus Grid dimCount must be either 2 or 3 when using create shape ", & 
               ESMF_CONTEXT, rc) 
         return 
    endif

    ! Argument Consistency Checking --------------------------------------------------------------
    if (present(regDecomp)) then
        if (size(regDecomp) .lt. dimCount) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
                    "- regDecomp size doesn't match Grid dimCount ", & 
                    ESMF_CONTEXT, rc) 
            return 
        endif
    endif

    if (present(decompFlag)) then
        if (size(decompFlag) .lt. dimCount) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
                    "- decompFlag size doesn't match Grid dimCount ", & 
                    ESMF_CONTEXT, rc) 
            return 
        endif
    endif


    if ((dimCount .lt. 3) .and. present(connDim3)) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- connDim3 not allowed when grid is less than dimCount 3", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if ((dimCount .lt. 3) .and. present(poleStaggerLoc3)) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- poleStaggerLoc3 not allowed when grid is less than dimCount 3", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if ((dimCount .lt. 3) .and. present(bipolePos3)) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- bipolePos3 not allowed when grid is less than dimCount 3", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif


    if ((dimCount .lt. 3) .and. present(coordDep3)) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- coordDep3 not allowed when grid is less than dimCount 3", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if (present(coordDep1)) then
       if ((size(coordDep1) < 1) .or. (size(coordDep1)>dimCount)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- coordDep1 size incompatible with grid dimCount", & 
               ESMF_CONTEXT, rc) 
          return 
       endif
    endif

    if (present(coordDep2)) then
       if ((size(coordDep2) < 1) .or. (size(coordDep2)>dimCount)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- coordDep2 size incompatible with grid dimCount", & 
               ESMF_CONTEXT, rc) 
          return 
       endif
    endif

    if (present(coordDep3)) then
       if ((size(coordDep3) < 1) .or. (size(coordDep3)>dimCount)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- coordDep3 size incompatible with grid dimCount", & 
               ESMF_CONTEXT, rc) 
          return 
       endif
    endif

    if (present(minIndex)) then
       if (size(minIndex) .ne. dimCount) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- minIndex size must equal grid dimCount", & 
               ESMF_CONTEXT, rc) 
          return 
       endif
    endif

    ! Check DimCount of gridWidths and Aligns
    if (present(gridEdgeLWidth)) then
        if (size(gridEdgeLWidth) .ne. dimCount) then
           call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
                     "- gridEdgeLWidth must be of size equal to Grid dimCount", & 
                     ESMF_CONTEXT, rc) 
              return
        endif 
    endif

    if (present(gridEdgeUWidth)) then
        if (size(gridEdgeUWidth) .ne. dimCount) then
           call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
                     "- gridEdgeUWidth must be of size equal to Grid dimCount", & 
                     ESMF_CONTEXT, rc) 
              return
        endif 
    endif

    if (present(gridAlign)) then
        if (size(gridAlign) .ne. dimCount) then
           call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
                     "- gridAlign must be of size equal to Grid dimCount", & 
                     ESMF_CONTEXT, rc) 
              return
        endif 
    endif


   ! make sure connected dimensions don't have an edge width
   if (present(connDim1)) then
      if (size(connDim1) .eq. 1) then
         if (connDim1(1) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeLWidth)) then
               if (gridEdgeLWidth(1) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have LWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
            if (present(gridEdgeUWidth)) then
               if (gridEdgeUWidth(1) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have UWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
      else if (size(connDim1) .eq. 2) then
         if (connDim1(1) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeLWidth)) then
               if (gridEdgeLWidth(1) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have LWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
         if (connDim1(2) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeUWidth)) then
               if (gridEdgeUWidth(1) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have UWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
      endif
   endif

   ! make sure connected dimensions don't have an edge width
   if (present(connDim2)) then
      if (size(connDim2) .eq. 1) then
         if (connDim2(1) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeLWidth)) then
               if (gridEdgeLWidth(2) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have LWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
            if (present(gridEdgeUWidth)) then
               if (gridEdgeUWidth(2) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have UWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
      else if (size(connDim2) .eq. 2) then
         if (connDim2(1) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeLWidth)) then
               if (gridEdgeLWidth(2) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have LWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
         if (connDim2(2) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeUWidth)) then
               if (gridEdgeUWidth(2) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have UWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
      endif
   endif


   ! make sure connected dimensions don't have an edge width
   if (present(connDim3)) then
      if (size(connDim3) .eq. 1) then
         if (connDim3(1) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeLWidth)) then
               if (gridEdgeLWidth(3) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have LWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
            if (present(gridEdgeUWidth)) then
               if (gridEdgeUWidth(3) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have UWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
      else if (size(connDim3) .eq. 2) then
         if (connDim3(1) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeLWidth)) then
               if (gridEdgeLWidth(3) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have LWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
         if (connDim3(2) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeUWidth)) then
               if (gridEdgeUWidth(3) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have UWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
      endif
   endif

   ! check for gridMemLBound issues
   if (present(gridMemLBound)) then
      if (.not. present(indexflag)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                "- when using gridMemLBound must specify indexflag=ESMF_INDEX_USER ", & 
                 ESMF_CONTEXT, rc) 
              return
      else if (.not.(indexflag .eq. ESMF_INDEX_USER)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                "- when using gridMemLBound must specify indexflag=ESMF_INDEX_USER ", & 
                 ESMF_CONTEXT, rc) 
              return
      endif
   else
      if (present(indexflag)) then
         if (indexflag .eq. ESMF_INDEX_USER) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                "- when using indexflag=ESMF_INDEX_USER must provide gridMemLBound ", & 
                   ESMF_CONTEXT, rc) 
              return
         endif
      endif
   endif


   ! Check for non-valid connection types here

   !TODO: Consider making some of these a separate local subroutine (particularly if you're going to 
   !      have 3 of these ShapeCreate subroutines with only minor changes


    ! Set Defaults ------------------------------------------------------------------

    ! Set default for minIndex
    allocate(minIndexLocal(dimCount), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating minIndexLocal", &
                                     ESMF_CONTEXT, rc)) return

    if (present(minIndex)) then
       minIndexLocal(:)=minIndex(:)
    else
       do i=1,dimCount
          minIndexLocal(i)=1
       enddo
    endif


    ! Set default for maxIndex
    allocate(maxIndexLocal(dimCount), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating maxIndexLocal", &
                                     ESMF_CONTEXT, rc)) return
    maxIndexLocal(:)=maxIndex(:)


    ! Set default for regDecomp 
    allocate(regDecompLocal(dimCount), stat=localrc)
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
       do i=2,dimCount
          regDecompLocal(i)=1
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
       connDim1Local(1)=ESMF_GRIDCONN_NONE ! if not present then default to no connection
       connDim1Local(2)=ESMF_GRIDCONN_NONE
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
       connDim2Local(1)=ESMF_GRIDCONN_NONE ! if not present then default to no connection
       connDim2Local(2)=ESMF_GRIDCONN_NONE
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
       connDim3Local(1)=ESMF_GRIDCONN_NONE ! if not present then default to no connection
       connDim3Local(2)=ESMF_GRIDCONN_NONE
    endif



    ! check for not implemented functionality
    if (connDim1Local(1) .ne. ESMF_GRIDCONN_NONE .or. &
        connDim1Local(2) .ne. ESMF_GRIDCONN_NONE) then
       call ESMF_LogMsgSetError(ESMF_RC_NOT_IMPL, & 
                 "- Only ESMF_GRIDCONN_NONE Grid connection implemented so far", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if (connDim2Local(1) .ne. ESMF_GRIDCONN_NONE .or. &
        connDim2Local(2) .ne. ESMF_GRIDCONN_NONE) then
       call ESMF_LogMsgSetError(ESMF_RC_NOT_IMPL, & 
                 "- Only ESMF_GRIDCONN_NONE Grid connection implemented so far", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if (connDim3Local(1) .ne. ESMF_GRIDCONN_NONE .or. &
        connDim3Local(2) .ne. ESMF_GRIDCONN_NONE) then
       call ESMF_LogMsgSetError(ESMF_RC_NOT_IMPL, & 
                 "- Only ESMF_GRIDCONN_NONE Grid connection implemented so far", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

  if (present(petMap)) then
     if (dimCount .gt. 2) then
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

   ! Modify Bounds by GridEdgeUWidth and GridEdgeLWidth  -------------------------
   ! setup maxIndexLocal to hold modified bounds
    allocate(gridEdgeLWidthLocal(dimCount), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating gridEdgeLWidthLocal", &
                                     ESMF_CONTEXT, rc)) return
    allocate(gridEdgeUWidthLocal(dimCount), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating gridEdgeUWidthLocal", &
                                     ESMF_CONTEXT, rc)) return
    allocate(gridAlignLocal(dimCount), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating gridAlignLocal", &
                                     ESMF_CONTEXT, rc)) return

    call ESMF_GridLUADefault(dimCount, &
                             gridEdgeLWidth, gridEdgeUWidth, gridAlign, &
                             gridEdgeLWidthLocal, gridEdgeUWidthLocal, gridAlignLocal, &
                             rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


#if 0 
    ! Modify lower bound
    do i=1,dimCount
       minIndexLocal(i)=minIndexLocal(i)-gridEdgeLWidthLocal(i)
    enddo

    ! Modify upper bound
    do i=1,dimCount
       maxIndexLocal(i)=maxIndexLocal(i)+gridEdgeUWidthLocal(i)
    enddo
#endif


   ! Set default for decomp flag based on gridEdgeWidths -----------------------------------
   ! NOTE: This is a temporary fix until we have something better implemented in distGrid

    ! Set default for decompFlag 
    allocate(decompFlagLocal(dimCount), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating decompFlagLocal", &
                                     ESMF_CONTEXT, rc)) return

    if (present(decompFlag)) then
        decompFlagLocal(:)=decompFlag(:)
    else
        decompFlagLocal(:)=ESMF_DECOMP_HOMOGEN
    endif


   allocate(distgridToGridMap(dimCount), stat=localrc)
   if (ESMF_LogMsgFoundAllocError(localrc, "Allocating distgridToGridMap", &
               ESMF_CONTEXT, rc)) return          
   do i=1,dimCount
     distgridToGridMap(i)=i
   enddo    

   ! Setup Connections between patch sides ----------------------------------------

   ! CONNECTIONS DON'T WORK YET SO NOT IMPLEMENTED


   ! Process PetMap --------------------------------------------------------------
   !! Calculate deCount
   deCount=1
   do i=1,dimCount
      deCount=deCount*regDecompLocal(i)
   enddo

   ! create DELayout based on presence of petMap
   if (present(petMap)) then
      !! Allocate petList
      allocate(petList(deCount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating petList", &
              ESMF_CONTEXT, rc)) return


      !! copy petMap to petList
      if (dimCount .gt. 2) then
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
    distgrid=ESMF_DistGridCreate(minIndex=minIndexLocal, maxIndex=maxIndexLocal, &
              regDecomp=regDecompLocal, decompFlag=decompFlagLocal, delayout=delayout,&
              indexflag=indexflag, &
#if 0
              regDecompFirstExtra=gridEdgeLWidthLocal, &
              regDecompLastExtra=gridEdgeUWidthLocal, &
#endif
              rc=localrc)   
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


   ! Convert coordDeps to coordDimCount and coordDimMap -------------------------------
   allocate(coordDimCount(dimCount), stat=localrc)
   if (ESMF_LogMsgFoundAllocError(localrc, "Allocating coordDimCount", &
              ESMF_CONTEXT, rc)) return
   allocate(coordDimMap(dimCount,dimCount), stat=localrc)
   if (ESMF_LogMsgFoundAllocError(localrc, "Allocating coordDimMap", &
              ESMF_CONTEXT, rc)) return

   if (present(coordDep1)) then
      coordDimCount(1)=size(coordDep1)
      coordDimMap(1,:)=0
      do i=1,size(coordDep1)
         coordDimMap(1,i)=coordDep1(i)
      enddo
   else 
      coordDimCount(1)=dimCount
      do i=1,dimCount
         coordDimMap(1,i)=i      
      enddo
   endif

   if (present(coordDep2)) then
      coordDimCount(2)=size(coordDep2)
      coordDimMap(2,:)=0
      do i=1,size(coordDep2)
         coordDimMap(2,i)=coordDep2(i)
      enddo
   else 
      coordDimCount(2)=dimCount
      do i=1,dimCount
         coordDimMap(2,i)=i      
      enddo
   endif

   if (dimCount .gt. 2) then
      if (present(coordDep3)) then 
         coordDimCount(3)=size(coordDep3)
          coordDimMap(3,:)=0
          do i=1,size(coordDep3)
             coordDimMap(3,i)=coordDep3(i)
          enddo
      else 
        coordDimCount(3)=dimCount
        do i=1,dimCount
	   coordDimMap(3,i)=i      
        enddo
      endif
   endif

  
   ESMF_GridCreateShapeTileReg=ESMF_GridCreateFromDistGrid(name, coordTypeKind, &
                                    distgrid=distgrid, distgridToGridMap=distgridToGridMap, &
                                    coordDimCount=coordDimCount, coordDimMap=coordDimMap, &
                                    gridEdgeLWidth=gridEdgeLWidthLocal, &
                                    gridEdgeUWidth=gridEdgeUWidthLocal, &
                                    gridAlign=gridAlignLocal, &
                                    gridMemLBound=gridMemLBound, &
                                    indexflag=indexflag, &
                                    destroyDistGrid=.true., &
                                    destroyDELayout=.true., &
                                    rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


    ! Clean up memory
    deallocate(regDecompLocal)
    deallocate(decompFlagLocal)
    deallocate(coordDimCount)
    deallocate(coordDimMap)
    deallocate(minIndexLocal)
    deallocate(maxIndexLocal)
    deallocate(distgridToGridMap)
    deallocate(gridEdgeLWidthLocal)
    deallocate(gridEdgeUWidthLocal)
    deallocate(gridAlignLocal)

 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    end function ESMF_GridCreateShapeTileReg


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreateShapeTileArb"
!BOP
! !IROUTINE: ESMF_GridCreateShapeTile - Create a Grid with an arbitrary distribution

! !INTERFACE:
  ! Private name; call using ESMF_GridCreateShapeTile()
      function ESMF_GridCreateShapeTileArb(name,coordTypeKind, minIndex,  &
			maxIndex, localArbIndex, localArbIndexCount, &
                        connDim1, connDim2, connDim3, &
                        poleStaggerLoc1, poleStaggerLoc2, poleStaggerLoc3, &
                        bipolePos1, bipolePos2, bipolePos3, &
                        coordDep1, coordDep2, coordDep3, &
                        distDim, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateShapeTileArb
!
! !ARGUMENTS:
      character (len=*), intent(in), optional :: name 
       type(ESMF_TypeKind),  intent(in),    optional  :: coordTypeKind
       integer,               intent(in),   optional  :: minIndex(:)
       integer,               intent(in)              :: maxIndex(:)
       integer,               intent(in)              :: localArbIndex(:,:)
       integer,               intent(in)   	      :: localArbIndexCount
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
       integer,               intent(in),   optional  :: distDim(:)
       integer,               intent(out),  optional  :: rc
!
! !DESCRIPTION:
!
! This method creates a single tile, arbitrarily distributed grid 
! (see Figure \ref{fig:GridDecomps}).
! To specify the arbitrary distribution, the user passes in an 2D array 
! of local indices, where the first dimension is the number of local grid cells
! specified by {\tt localArbIndexCount} and the second dimension is the number of distributed
! dimensions.
!
! {\tt distDim} specifies which grid dimensions are arbitrarily distributed. The 
! size of {\tt distDim} has to agree with the size of the second dimension of 
! {\tt localArbIndex}. 
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
! \item[{[maxIndex]}] 
!      The upper extend of the grid index ranges.
! \item[{[localArbIndex]}] 
!      This 2D array specifies the indices of the local grid cells.  The 
!      dimensions should be localArbIndexCount * number of Distributed grid dimensions
!      where localArbIndexCount is the input argument specified below
! \item[{localArbIndexCount}] 
!      number of grid cells in the local DE. It is okay to have 0
!      grid cell in a local DE.  
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
!     The size of the array specifies the number of dimensions of the 
!     first coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. The format should be /ESMF\_GRID\_ARBDIM/ where
!     /ESMF\_GRID\_ARBDIM/ is mapped to the collapsed 1D dimension from all
!     the arbitrarily distributed dimensions.  n is the dimension that 
!     is not distributed (if exists).  
!     If not present the default is /ESMF\_GRID\_ARBDIM/ if the first dimension
!     is arbitararily distributed, or /n/ if not distributed (i.e. n=1)
! \item[{[coordDep2]}] 
!     The size of the array specifies the number of dimensions of the 
!     second coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. The format should be /ESMF\_GRID\_ARBDIM/ where
!     /ESMF\_GRID\_ARBDIM/ is mapped to the collapsed 1D dimension from all
!     the arbitrarily distributed dimensions.  n is the dimension that 
!     is not distributed (if exists).  
!     If not present the default is /ESMF\_GRID\_ARBDIM/ if this dimension
!     is arbitararily distributed, or /n/ if not distributed (i.e. n=2)
! \item[{[coordDep3]}] 
!     The size of the array specifies the number of dimensions of the 
!     third coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. The format should be /ESMF\_GRID\_ARBDIM/ where
!     /ESMF\_GRID\_ARBDIM/ is mapped to the collapsed 1D dimension from all
!     the arbitrarily distributed dimensions.  n is the dimension that 
!     is not distributed (if exists).  
!     If not present the default is /ESMF\_GRID\_ARBDIM/ if this dimension
!     is arbitararily distributed, or /n/ if not distributed (i.e. n=3)
! \item[{[distDim]}]
!       This array specifies which dimensions are arbitrarily distributed.
!       The size of the array specifies the total distributed dimensions.
!       if not specified, defaults is all dimensions will be arbitrarily
!       distributed.  The size has to agree with the size of the second
!       dimension of {\tt localArbIndex}.
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
    type(ESMF_DistGrid)  :: distgrid
    integer, pointer     :: undistLBound(:)
    integer, pointer     :: undistUBound(:)
    integer, pointer     :: coordDimCount(:)
    integer, pointer     :: coordDimMap(:,:)
    integer              :: localrc
    integer              :: dimCount,distDimCount,undistDimCount
    integer, pointer     :: indexArray(:,:)
    integer              :: i,j,ud
    type(ESMF_GridConn)  :: connDim1Local(2)
    type(ESMF_GridConn)  :: connDim2Local(2)
    type(ESMF_GridConn)  :: connDim3Local(2)
    integer, pointer     :: distSize(:)
    integer, pointer     :: distDimLocal(:)
    logical, pointer     :: isDist(:)
    integer, pointer     :: local1DIndices(:)
    integer              :: ind
    logical              :: found

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following test when dummy argument actually used
    if (present(polestaggerloc1)) then
    	if (polestaggerloc1(1)==polestaggerloc1(1)) continue;
    endif

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following test when dummy argument actually used
    if (present(polestaggerloc2)) then
    	if (polestaggerloc2(1)==polestaggerloc2(1)) continue;
    endif

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following test when dummy argument actually used
    if (present(bipolepos1)) then
    	if (bipolepos1(1)==bipolepos1(1)) continue;
    endif

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following test when dummy argument actually used
    if (present(bipolepos2)) then
    	if (bipolepos2(1)==bipolepos2(1)) continue;
    endif


    ! Compute the Grid DimCount and Derivatives ---------------------------------------------------
    ! dimCount
    dimCount=size(maxIndex)
    if ((dimCount < 2) .or. (dimCount > 3)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- maxIndex size and thus Grid dimCount must be either 2 or 3 when using create shape ", & 
               ESMF_CONTEXT, rc) 
         return 
    endif
    
    ! number of distributed dimension, distDimCount, is determined by the second dim of 
    ! localArbIndex
    distDimCount = size(localArbIndex,2)
    if (distDimCount > dimCount) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- the second dim of localArbIndex must be equal or less than grid dimension", & 
               ESMF_CONTEXT, rc) 
         return 
    endif
 
    allocate(distDimLocal(distDimCount), stat=localrc)
    allocate(isDist(dimCount), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating distDimLocal or isDist", &
                                     ESMF_CONTEXT, rc)) return

    isDist(:)=.false.
    ! check distribution info
    if (present(distDim)) then
       if (size(distDim) .ne. distDimCount) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
                 "- distDim must match with the second dimension of localArbIndex", & 
                 ESMF_CONTEXT, rc) 
            return 
       endif
       distDimLocal(:)=distDim(:)
       do i=1,distDimCount
	  isDist(distDimLocal(i))=.true.
       enddo
    else
       do i=1,distDimCount
         distDimLocal(i)=i
       enddo
       isDist(1:distDimCount)=.true.
    endif

    ! Argument Consistency Checking --------------------------------------------------------------
    if ((dimCount .lt. 3) .and. present(connDim3)) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- connDim3 not allowed when grid is less than dimCount 3", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if ((dimCount .lt. 3) .and. present(poleStaggerLoc3)) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- poleStaggerLoc3 not allowed when grid is less than dimCount 3", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if ((dimCount .lt. 3) .and. present(bipolePos3)) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- bipolePos3 not allowed when grid is less than dimCount 3", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if (present(minIndex)) then
       if (size(minIndex) .ne. dimCount) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- minIndex size must equal grid dimCount", & 
               ESMF_CONTEXT, rc) 
          return 
       endif
    endif

   ! Check for non-valid connection types here

    ! Set Defaults -------------------------------------------------------------
    ! Set default for minIndex 
    allocate(indexArray(2,dimCount), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating minIndexLocal", &
                                     ESMF_CONTEXT, rc)) return

    if (present(minIndex)) then
       indexArray(1,:)=minIndex(:)
    else
        indexArray(1,:)=1
    endif

    ! Set default for maxIndex
    indexArray(2,:)=maxIndex(:)

    ! dimCount of distributed part
    allocate(distSize(distDimCount),stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating distSize", &
                                   ESMF_CONTEXT, rc)) return

    do i=1,distDimCount   
       ind = distDimLocal(i)
       distSize(i)=indexArray(2,ind)-indexArray(1,ind)+1
    enddo

    ! dimCounts of the undistributed part of the grid
    undistDimCount=dimCount-distDimCount

    ! can't have all undistributed dimensions
    if (distDimCount .eq. 0) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- Need to have at least one distributed dimension", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    ! convert localArbIndex into 1D index array for DistGrid
    ! Check localArbIndex dimension matched with localArbIndexCount and diskDimCount
    if (size(localArbIndex, 1) .ne. localArbIndexCount) then
       	  call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- localArbIndex 1st dimension has to match with localArbIndexCount", & 
                 ESMF_CONTEXT, rc) 
          return
    endif

    allocate(local1DIndices(localArbIndexCount), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating local1DIndices", &
                                     ESMF_CONTEXT, rc)) return
      
    if (localArbIndexCount .gt. 0) then
       ! use 0-based index to calculate the 1D index and add 1 back at the end
       do i = 1, localArbIndexCount
          local1DIndices(i) = localArbIndex(i,1)-1
	  if (distDimCount .ge. 2) then 
	     do j = 2,distDimCount
	        local1DIndices(i) = local1DIndices(i)*distSize(j) + localArbIndex(i,j)-1
	     enddo
	  endif
          local1DIndices(i) = local1DIndices(i)+1
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
       connDim1Local(1)=ESMF_GRIDCONN_NONE ! if not present then default to no connection
       connDim1Local(2)=ESMF_GRIDCONN_NONE
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
       connDim2Local(1)=ESMF_GRIDCONN_NONE ! if not present then default to no connection
       connDim2Local(2)=ESMF_GRIDCONN_NONE
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
       connDim3Local(1)=ESMF_GRIDCONN_NONE ! if not present then default to no connection
       connDim3Local(2)=ESMF_GRIDCONN_NONE
    endif


    ! check for not implemented functionality
    if (connDim1Local(1) .ne. ESMF_GRIDCONN_NONE .or. &
        connDim1Local(2) .ne. ESMF_GRIDCONN_NONE) then
       call ESMF_LogMsgSetError(ESMF_RC_NOT_IMPL, & 
                 "- Only ESMF_GRIDCONN_NONE Grid connection implemented so far", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if (connDim2Local(1) .ne. ESMF_GRIDCONN_NONE .or. &
        connDim2Local(2) .ne. ESMF_GRIDCONN_NONE) then
       call ESMF_LogMsgSetError(ESMF_RC_NOT_IMPL, & 
                 "- Only ESMF_GRIDCONN_NONE Grid connection implemented so far", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if (connDim3Local(1) .ne. ESMF_GRIDCONN_NONE .or. &
        connDim3Local(2) .ne. ESMF_GRIDCONN_NONE) then
       call ESMF_LogMsgSetError(ESMF_RC_NOT_IMPL, & 
                 "- Only ESMF_GRIDCONN_NONE Grid connection implemented so far", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

   ! Setup Connections between patch sides ----------------------------------------

   ! CONNECTIONS DON'T WORK YET SO NOT IMPLEMENTED

   ! Convert coordDeps to coordDimCount and coordDimMap -------------------------------
   allocate(coordDimCount(dimCount), stat=localrc)
   if (ESMF_LogMsgFoundAllocError(localrc, "Allocating coordDimCount", &
              ESMF_CONTEXT, rc)) return
   allocate(coordDimMap(dimCount,dimCount), stat=localrc)
   if (ESMF_LogMsgFoundAllocError(localrc, "Allocating coordDimMap", &
              ESMF_CONTEXT, rc)) return

   if (present(coordDep1)) then
      ! error checking, if this dimension is arbitrary, one of the 
      ! coordinate dimension has to be be ESMF_GRID_ARBDIM
      if (isDist(1)) then
	found = .false.
	do i=1,size(coordDep1)
	  if (coordDep1(i) .eq. ESMF_GRID_ARBDIM) found = .true.
        enddo
	if (.not. found) then
           call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- coordDep1 does not contain ESMF_GRID_ARBDIM", & 
                 ESMF_CONTEXT, rc) 
	    return
        endif
      endif	
      coordDimCount(1)=size(coordDep1)
      coordDimMap(1,:)=0
      do i=1,size(coordDep1)
         coordDimMap(1,i)=coordDep1(i)
      enddo
   else 
      coordDimCount(1)=1
      ! ESMF_GRID_ARBDIM if 1 is distributed, otherwise 1
      if (isDist(1)) then
        coordDimMap(1,1)=ESMF_GRID_ARBDIM      
      else
	coordDimMap(1,1)=1
      endif
   endif

   if (present(coordDep2)) then
      ! error checking, one of the dimensions has to be ESMF_GRID_ARBDIM
      ! if dimension 2 is arbitrary
      if (isDist(2)) then
	found = .false.
	do i=1,size(coordDep2)
	  if (coordDep2(i) .eq. ESMF_GRID_ARBDIM) found = .true.
        enddo
	if (.not. found) then
           call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- coordDep2 does not contain ESMF_GRID_ARBDIM", & 
                 ESMF_CONTEXT, rc) 
	    return
        endif
      endif	
      coordDimCount(2)=size(coordDep2)
      coordDimMap(2,:)=0
      do i=1,size(coordDep2)
         coordDimMap(2,i)=coordDep2(i)
      enddo
   else 
      coordDimCount(2)=1
      ! ESMF_GRID_ARBDIM if 1 is distributed, otherwise 1
      if (isDist(2)) then
        coordDimMap(2,1)=ESMF_GRID_ARBDIM      
      else
	coordDimMap(2,1)=2
      endif
   endif

   if (dimCount .gt. 2) then
      if (present(coordDep3)) then 
        ! error checking, one of the dimensions has to be ESMF_GRID_ARBDIM
        ! if dimension 3 is arbitrary
        if (isDist(3)) then
	  found = .false.
	  do i=1,size(coordDep3)
	    if (coordDep3(i) .eq. ESMF_GRID_ARBDIM) found = .true.
          enddo
	  if (.not. found) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- coordDep3 does not contain ESMF_GRID_ARBDIM", & 
                 ESMF_CONTEXT, rc) 
	    return
          endif
        endif	
        coordDimCount(3)=size(coordDep3)
        coordDimMap(3,:)=0
        do i=1,size(coordDep3)
           coordDimMap(3,i)=coordDep3(i)
        enddo
      else 
        coordDimCount(3)=1
        ! ESMF_GRID_ARBDIM if 1 is distributed, otherwise 1
        if (isDist(3)) then
          coordDimMap(3,1)=ESMF_GRID_ARBDIM      
        else
	  coordDimMap(3,1)=3
        endif
      endif
   endif


   ! Calc undistLBound, undistUBound for Grid -----------------------------------------------
   if (undistDimCount .gt. 0) then
     allocate(undistLBound(undistDimCount), stat=localrc)
     if (ESMF_LogMsgFoundAllocError(localrc, "Allocating undistLBound", &
              ESMF_CONTEXT, rc)) return
     allocate(undistUBound(undistDimCount), stat=localrc)
     if (ESMF_LogMsgFoundAllocError(localrc, "Allocating undistUBound", &
              ESMF_CONTEXT, rc)) return     
   
      ! Fill in undistLBound, undistUBound
      ud=1
      do i=1,dimCount
         if (.not. isDist(i)) then
           undistLBound(ud)=indexArray(1,i)
           undistUBound(ud)=indexArray(2,i)
           ud=ud+1
         endif
      enddo
   endif
  

   ! Create DistGrid --------------------------------------------------------------
   if (undistDimCount .gt. 0) then 
       distgrid=ESMF_DistGridCreate(local1DIndices, 1, undistLBound, undistUBound, rc=localrc)   
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
   else
      distgrid=ESMF_DistGridCreate(local1DIndices, rc=localrc)   
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
   endif
   ! Create Grid from specification -----------------------------------------------
   ESMF_GridCreateShapeTileArb=ESMF_GridCreateFromDistGridArb(name, coordTypeKind, &
			       distgrid, indexArray, &
                               distDim=distDimLocal, &
			       coordDimCount=coordDimCount, coordDimMap=coordDimMap, &
                               destroyDistGrid=.true., &
                               destroyDELayout=.false., &
                               rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Clean up memory
    deallocate(indexArray)
    deallocate(local1DIndices)
    deallocate(isDist)
    deallocate(distDimLocal)
    deallocate(coordDimCount)
    deallocate(coordDimMap)
    if (undistDimCount .gt. 0) then
      deallocate(undistLBound)
      deallocate(undistUBound)
    endif
    deallocate(distSize)
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    end function ESMF_GridCreateShapeTileArb


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
!    does destroy internally created DistGrid and DELayout classes, for example those created by 
!    {\tt ESMF\_GridCreateShapeTile()}. It also destroys internally created coordinate/item Arrays,
!    for example those created by {\tt ESMF\_GridAddCoord()}. However, if the user uses an externally 
!    created class, for example creating an Array and setting it using {\tt ESMF\_GridSetCoord()}, then
!    that class is not destroyed by this method.
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
#define ESMF_METHOD "ESMF_GridGetDefault"
!BOP
! !IROUTINE: ESMF_GridGet - Get information about a Grid

! !INTERFACE:
  ! Private name; call using ESMF_GridGet()
      subroutine ESMF_GridGetDefault(grid, name, coordTypeKind, &
          dimCount, tileCount, staggerlocsCount, localDECount, distgrid, &
          distgridToGridMap, coordDimCount, coordDimMap, &
          localArbIndexCount, localArbIndex, arbDim, &
          memDimCount, arbDimCount, &
          gridEdgeLWidth, gridEdgeUWidth, gridAlign,  &
          indexFlag, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid),       intent(in)            :: grid
      character (len=*),     intent(out), optional :: name
      type(ESMF_TypeKind),   intent(out), optional :: coordTypeKind
      integer,               intent(out), optional :: dimCount
      integer,               intent(out), optional :: tileCount
      integer,               intent(out), optional :: staggerlocsCount
      integer,               intent(out), optional :: localDECount
      type(ESMF_DistGrid),   intent(out), optional :: distgrid
      integer,       target, intent(out), optional :: distgridToGridMap(:)
      integer,       target, intent(out), optional :: coordDimCount(:)
      integer,       target, intent(out), optional :: coordDimMap(:,:)
      integer,               intent(out), optional :: localArbIndexCount
      integer,       target, intent(out), optional :: localArbIndex(:,:)
      integer,               intent(out), optional :: arbDim
      integer,               intent(out), optional :: memDimCount
      integer,               intent(out), optional :: arbDimCount
      integer,       target, intent(out), optional :: gridEdgeLWidth(:)
      integer,       target, intent(out), optional :: gridEdgeUWidth(:)
      integer,       target, intent(out), optional :: gridAlign(:)
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
!\item[{[dimCount]}]
!   DimCount of the Grid object.
!\item[{[tileCount]}]
!   The number of logically rectangular tiles in the grid. 
!\item[{[staggerlocsCount]}]
!   The number of stagger locations.
!\item[{[localDECount]}]
!   The number of DEs in this grid on this PET.
!\item[{[distgrid]}]
!   The structure describing the distribution of the grid. 
!\item[{[distgridToGridMap]}]
!   List that has as many elements as the distgrid dimCount. This array describes
!   mapping between the grids dimensions and the distgrid.
! \item[{[coordDimCount]}]
!   List that has as many elements as the grid dimCount (from arrayspec).
!   Gives the dimension of each component (e.g. x) array. This is 
!   to allow factorization of the coordinate arrays. If not specified
!   all arrays are the same size as the grid. 
!\item[{[coordDimMap]}]
!   2D list of size grid dimCount x grid dimCount. This array describes the
!   map of each component array's dimensions onto the grids
!   dimensions.
! \item[{[localArbIndexCount]}] 
!   The number of local cells for an arbitrarily distributed grid
! \item[{[localArbIndex]}] 
!   The 2D array storing the local cell indices for an arbitrarily distributed grid. The size of the array 
!   is localArbIndexCount * arbDimCount 
! \item[{[arbDim]}] 
!   The distgrid dimension that is mapped by the arbitrarily distributed grid dimensions.
! \item[{[memDimCount]}] 
!   The count of the memory dimensions, it is the same as dimCount for a non-arbitrarily distributed grid,
!   and equal or less for a arbitrarily distributed grid.
! \item[{[arbDimCount]}] 
!   The number of dimensions distributed arbitrarily for an arbitrary grid, 0 if the grid is non-arbitrary.
! \item[{[gridEdgeLWidth]}] 
!   The padding around the lower edges of the grid. The array should
!   be of size greater or equal to the Grid dimCount.
! \item[{[gridEdgeUWidth]}] 
!      The padding around the upper edges of the grid. The array should
!   be of size greater or equal to the Grid dimCount. 
! \item[{[gridAlign]}] 
!     Specification of how the stagger locations should align with the cell
!     index space. The array should be of size greater or equal to the Grid dimCount. 
! \item[{[indexflag]}]
!    Flag indicating the indexing scheme being used in the Grid. Please
!    see Section~\ref{opt:indexflag} for the list of options. 
!\item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!\end{description}
!
!EOP
    integer :: localrc ! local error status
    type(ESMF_GridDecompType) :: decompType  ! check if arbitrary
    type(ESMF_InterfaceInt) :: distgridToGridMapArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: coordDimCountArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: coordDimMapArg ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: gridEdgeLWidthArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: gridEdgeUWidthArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: gridAlignArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: localArbIndexArg ! Language Interface Helper Var

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
     ESMF_INIT_CHECK_DEEP_SHORT(ESMF_GridGetInit, grid, rc)
   
    ! Get Grid decomposition type
    call ESMF_GridGetDecompType(grid, decompType, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
	  
    if (decompType .eq. ESMF_Grid_NONARBITRARY) then
	if (present(localArbIndexCount) .or. present(localArbIndex) .or. present(arbDim)) then
         call ESMF_LogMsgSetError(ESMF_RC_NOT_IMPL, & 
                 "- localArbIndexCount, localArbIndex or arbDim does not exist for a non-arbitrarily distributed grid", & 
                 ESMF_CONTEXT, rc)
         return 
	endif
    endif

    ! name 
    if (present(name)) then
      call c_ESMC_GetName(grid, name, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    !! coordTypeKind
    ! It doesn't look like it needs to be translated, but test to make sure

    !! distgridToGridMap
    distgridToGridMapArg = ESMF_InterfaceIntCreate(distgridToGridMap, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    !! Description of array factorization
    coordDimCountArg = ESMF_InterfaceIntCreate(coordDimCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    coordDimMapArg = ESMF_InterfaceIntCreate(farray2D=coordDimMap, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    !! Grid Boundary Info
    gridEdgeLWidthArg = ESMF_InterfaceIntCreate(gridEdgeLWidth, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    gridEdgeUWidthArg = ESMF_InterfaceIntCreate(gridEdgeUWidth, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    gridAlignArg = ESMF_InterfaceIntCreate(gridAlign, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    !! Arbitrarily distributed grid local indices
    localArbIndexArg = ESMF_InterfaceIntCreate(farray2D=localArbIndex, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Call C++ Subroutine to do the get
    call c_ESMC_gridget(grid%this, &
      coordTypeKind, dimCount, tileCount, distgrid,  staggerlocsCount, &
      distgridToGridMapArg, coordDimCountArg, &
      localArbIndexCount, localArbIndexArg, arbDim, &
      memDimCount, arbDimCount, coordDimMapArg, &
      gridEdgeLWidthArg, gridEdgeUWidthArg, gridAlignArg, &
      indexflag, localDECount, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deallocate helper variables
    call ESMF_InterfaceIntDestroy(distgridToGridMapArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(coordDimCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(coordDimMapArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(localArbIndexArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(gridEdgeLWidthArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(gridEdgeUWidthArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(gridAlignArg, rc=localrc)
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

end subroutine ESMF_GridGetDefault

!------------------------------------------------------------------------------
#undef  ESMF_METHOD  
#define ESMF_METHOD "ESMF_GridGetIndex"
!BOPI
! !IROUTINE: ESMF_GridGet - Get information about the min and max index of the grid dimension
! !INTERFACE:
  ! Private name; call using ESMF_GridGet()
      subroutine ESMF_GridGetIndex(grid, tileNo, minIndex, maxIndex, rc)

!
! !Arguments:
    type(ESMF_Grid),    intent(in)            :: grid
	integer,        intent(in), optional  :: tileNo
	integer,target, intent(out),optional  :: minIndex(:)
	integer,target, intent(out)           :: maxIndex(:)
	integer,        intent(out), optional :: rc
!
! !DESCRIPTON:
!  This method gets the minimal index and maximal index of a given tile of the grid

!The arguments are:
!\begin{description}
!\item[{grid}]
!    Grid to get the information from.
!\item[{[tileNo]}]
!     The tile number from which to get the information. The default is 0. 
!\item[{[minIndex]}]
!     The minimal grid index for the given tile.
!\item[{[maxIndex]}]
!     The maximal grid index for the given tile.
!\item[{[rc]}]
!     The return value.

!\end{description}
!
!EOPI

    integer :: localrc ! local error status
    type(ESMF_InterfaceInt) :: minIndexArg ! helper variable
    type(ESMF_InterfaceInt) :: maxIndexArg ! helper variable	
    type(ESMF_GridDecompType) :: decompType
    integer :: localTileNo ! local TileNo	
  
    ! Initialize return code
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_GridGetInit, grid, rc)
   
    if (present(tileNo)) then
        ! Get Grid decomposition type
	call ESMF_GridGetDecompType(grid, decompType, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
	if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. &
	    (tileNo .ne. 1)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
              "- tileNo has to be 1 for arbitrarily distributed grid", & 
              ESMF_CONTEXT, rc) 
	  return
	elseif (tileNo .ne. 1) then 
          call ESMF_LogMsgSetError(ESMF_RC_NOT_IMPL, & 
              "- multiple tiles is not implemented", & 
              ESMF_CONTEXT, rc) 
	  return
      endif
      localTileNo = tileNo
    else
      localTileNo = 1
    endif

    ! process optional arguments
    minIndexArg=ESMF_InterfaceIntCreate(minIndex, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    maxIndexArg=ESMF_InterfaceIntCreate(maxIndex, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

     call c_ESMC_gridgetindex(grid, localTileNo, minIndexArg, maxIndexArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deallocate interface ints
    call ESMF_InterfaceIntDestroy(minIndexArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(maxIndexArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
   
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetIndex

!------------------------------------------------------------------------------
#undef  ESMF_METHOD  
#define ESMF_METHOD "ESMF_GridGetPLocalDePSloc"
!BOP
! !IROUTINE: ESMF_GridGet - Get information about a particular DE in a stagger location in a Grid

! !INTERFACE:
  ! Private name; call using ESMF_GridGet()
      subroutine ESMF_GridGetPLocalDePSloc(grid, localDe, staggerloc, &
          exclusiveLBound, exclusiveUBound, exclusiveCount,  &
          computationalLBound, computationalUBound, computationalCount, rc)

!
! !ARGUMENTS:
      type(ESMF_Grid),        intent(in)            :: grid
      integer,                intent(in)            :: localDe
      type (ESMF_StaggerLoc), intent(in)            :: staggerloc
      integer,        target, intent(out), optional :: exclusiveLBound(:)
      integer,        target, intent(out), optional :: exclusiveUBound(:)
      integer,        target, intent(out), optional :: exclusiveCount(:)
      integer,        target, intent(out), optional :: computationalLBound(:)
      integer,        target, intent(out), optional :: computationalUBound(:)
      integer,        target, intent(out), optional :: computationalCount(:)
      integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!  This method gets information about the range of index space which a 
!  particular stagger location occupies. This call differs from the coordinate 
!  bound calls (e.g. {\tt ESMF\_GridGetCoord}) in that a given coordinate 
!  array may only occupy a subset of the Grid's dimensions, and
!  so these calls may not give all the bounds of the stagger location. 
!  The bounds from this call are the full bounds, and so
!  for example, give the appropriate bounds for allocating a Fortran array to hold 
!  data residing on the stagger location.
!  Note that unlike the output from the Array, these values also include the 
!  undistributed dimensions and are
!  ordered to reflect the order of the indices in the Grid. This call will 
!  still give correct values even if the stagger location does not contain
!  coordinate arrays (e.g. if  {\tt ESMF\_GridAddCoord} hasn't yet 
!  been called on the stagger location).
!
!The arguments are:
!\begin{description}
!\item[{grid}]
!    Grid to get the information from.
!\item[{[localDe]}]
!     The local DE from which to get the information. {\tt [0,..,localDeCount-1]} 
!\item[{staggerloc}]
!     The stagger location to get the information for. 
!     Please see Section~\ref{sec:opt:staggerloc} for a list 
!     of predefined stagger locations.
!\item[{[exclusiveLBound]}]
!     Upon return this holds the lower bounds of the exclusive region.
!     {\tt exclusiveLBound} must be allocated to be of size equal to the Grid dimCount.
!     Please see Section~\ref{sec:grid:usage:bounds} for a description
!     of the regions and their associated bounds and counts. 
!\item[{[exclusiveUBound]}]
!     Upon return this holds the upper bounds of the exclusive region.
!     {\tt exclusiveUBound} must be allocated to be of size equal to the Grid dimCount.
!     Please see Section~\ref{sec:grid:usage:bounds} for a description
!     of the regions and their associated bounds and counts. 
!\item[{[exclusiveCount]}]
!     Upon return this holds the number of items in the exclusive region per dimension
!     (i.e. {\tt exclusiveUBound-exclusiveLBound+1}). {\tt exclusiveCount} must
!      be allocated to be of size equal to the Grid dimCount.
!     Please see Section~\ref{sec:grid:usage:bounds} for a description
!     of the regions and their associated bounds and counts. 
!\item[{[computationalLBound]}]
!     Upon return this holds the lower bounds of the computational region.
!     {\tt computationalLBound} must be allocated to be of size equal to the Grid dimCount.
!     Please see Section~\ref{sec:grid:usage:bounds} for a description
!     of the regions and their associated bounds and counts. 
!\item[{[computationalUBound]}]
!     Upon return this holds the upper bounds of the computational region.
!     {\tt computationalUBound} must be allocated to be of size equal to the Grid dimCount.
!     Please see Section~\ref{sec:grid:usage:bounds} for a description
!     of the regions and their associated bounds and counts. 
!\item[{[computationalCount]}]
!     Upon return this holds the number of items in the computational region per dimension.
!     (i.e. {\tt computationalUBound-computationalLBound+1}). {\tt computationalCount} must
!      be allocated to be of size equal to the Grid dimCount.
!     Please see Section~\ref{sec:grid:usage:bounds} for a description
!     of the regions and their associated bounds and counts. 
!\item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!\end{description}
!
!EOP

    integer :: localrc ! local error status
    type(ESMF_InterfaceInt) :: exclusiveLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: exclusiveUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: exclusiveCountArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalCountArg ! helper variable
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
    exclusiveCountArg=ESMF_InterfaceIntCreate(exclusiveCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalLBoundArg=ESMF_InterfaceIntCreate(computationalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalUBoundArg=ESMF_InterfaceIntCreate(computationalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalCountArg=ESMF_InterfaceIntCreate(computationalCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments

    call c_ESMC_GridGetPLocalDePSloc(grid, localDE, tmp_staggerLoc, &
      exclusiveLBoundArg, exclusiveUBoundArg, exclusiveCountArg, &
      computationalLBoundArg, computationalUBoundArg, computationalCountArg, &
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
    call ESMF_InterfaceIntDestroy(exclusiveCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetPLocalDePSloc

!------------------------------------------------------------------------------------
#undef  ESMF_METHOD  
#define ESMF_METHOD "ESMF_GridGetPSloc"
!BOP
! !IROUTINE: ESMF_GridGet - Get information about a particular stagger location in a Grid

! !INTERFACE:
  ! Private name; call using ESMF_GridGet()
      subroutine ESMF_GridGetPSloc(grid, staggerloc, &
          staggerDistgrid, minIndex, maxIndex, rc)

!
! !ARGUMENTS:
      type(ESMF_Grid),        intent(in)            :: grid
      type (ESMF_StaggerLoc), intent(in)            :: staggerloc
      type(ESMF_DistGrid),   intent(out), optional :: staggerDistgrid
      integer,        target, intent(out), optional :: minIndex(:)
      integer,        target, intent(out), optional :: maxIndex(:)
      integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!  This method gets information about a particular stagger location. 
!  This information is useful for creating an ESMF Array to hold
!  the data at the stagger location. 
!
!The arguments are:
!\begin{description}
!\item[{grid}]
!    Grid to get the information from.
!\item[{staggerloc}]
!     The stagger location to get the information for. 
!     Please see Section~\ref{sec:opt:staggerloc} for a list 
!     of predefined stagger locations. 
!\item[{[staggerDistgrid]}]
!   The structure describing the distribution of this staggerloc in this grid. 
!\item[{[minIndex]}]
!     Upon return this holds the global lower index of this stagger location.
!     {\tt minIndex} must be allocated to be of size equal to the grid DimCount.
!     Note that this value is only for the first Grid tile, as multigrid support
!     is added, this interface will likely be changed or moved to adapt.  
!\item[{[maxIndex]}]
!     Upon return this holds the global upper index of this stagger location.
!     {\tt maxIndex} must be allocated to be of size equal to the grid DimCount.
!     Note that this value is only for the first Grid tile, as multigrid support
!     is added, this interface will likely be changed or moved to adapt.  
!\item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!\end{description}
!
!EOP

    integer :: localrc ! local error status
    type(ESMF_InterfaceInt) :: minIndexArg ! helper variable
    type(ESMF_InterfaceInt) :: maxIndexArg ! helper variable
    integer :: tmp_staggerloc

    ! Initialize return code
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_GridGetInit, grid, rc)
    tmp_staggerloc=staggerloc%staggerloc

    ! process optional arguments
    minIndexArg=ESMF_InterfaceIntCreate(minIndex, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    maxIndexArg=ESMF_InterfaceIntCreate(maxIndex, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments

    call c_ESMC_GridGetPSloc(grid, tmp_staggerLoc, &
         staggerDistgrid, minIndexArg, maxIndexArg,localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deallocate interface ints
    call ESMF_InterfaceIntDestroy(minIndexArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(maxIndexArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set Deep Classes as created
    if (present(staggerDistgrid)) then
       call ESMF_DistGridSetInitCreated(staggerDistgrid, rc=localrc)
       if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
   	   ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetPSloc



!------------------------------------------------------------------------------
#undef  ESMF_METHOD  
#define ESMF_METHOD "ESMF_GridGetDecompType"
!BOPI
! !IROUTINE: ESMF_GridGetDecompType - Get decomposition type: arbitrary or not
! !INTERFACE:
      subroutine ESMF_GridGetDecompType(grid, decompType, rc)

!
! !ARGUMENTS:
    type(ESMF_Grid),           intent(in)            :: grid
    type(ESMF_GridDecompType), intent(out)           :: decompType
    integer,                   intent(out), optional :: rc
!
    integer :: localrc

    call c_ESMC_gridGetDecompType(grid, decompType, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

    if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_GridGetDecompType


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridGetCoord - Get Grid coordinate bounds and a Fortran pointer to coordinate data

! !INTERFACE:
!      subroutine ESMF_GridGetCoord(grid, localDE, coordDim, staggerloc, &
!         exclusiveLBound, exclusiveUBound, exclusiveCount,              &
!         computationalLBound, computationalUBound, computationalCount,  &
!         totalLBound, totalUBound, totalCount,                          &
!         <pointer argument>, doCopy, rc)
! 
! !ARGUMENTS:
!     type(ESMF_Grid),        intent(in)            :: grid
!     integer,                intent(in)            :: localDE
!     integer,                intent(in)            :: coordDim
!     type (ESMF_StaggerLoc), intent(in),  optional :: staggerloc
!     integer,                intent(out), optional :: exclusiveLBound(:)
!     integer,                intent(out), optional :: exclusiveUBound(:)
!     integer,                intent(out), optional :: exclusiveCount(:)
!     integer,                intent(out), optional :: computationalLBound(:)
!     integer,                intent(out), optional :: computationalUBound(:)
!     integer,                intent(out), optional :: computationalCount(:)
!     integer,                intent(out), optional :: totalLBound(:)
!     integer,                intent(out), optional :: totalUBound(:)
!     integer,                intent(out), optional :: totalCount(:)
!     <pointer argument>, see below for supported values
!     type(ESMF_CopyFlag),    intent(in), optional  :: docopy
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
!     \item real(ESMF\_KIND\_R4), pointer :: fptr(:)
!     \item real(ESMF\_KIND\_R4), pointer :: fptr(:,:)     
!     \item real(ESMF\_KIND\_R4), pointer :: fptr(:,:,:)
!     \item real(ESMF\_KIND\_R8), pointer :: fptr(:)
!     \item real(ESMF\_KIND\_R8), pointer :: fptr(:,:)     
!     \item real(ESMF\_KIND\_R8), pointer :: fptr(:,:,:)
!     \end{description}
!
!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          Grid to get the information from.
!     \item[{[localDE]}]
!          The local DE to get the information for. {\tt [0,..,localDeCount-1]}
!     \item[{coordDim}]
!          The coordinate dimension to get the data from (e.g. 1=x).
!     \item[{staggerloc}]
!          The stagger location to get the information for. 
!          Please see Section~\ref{sec:opt:staggerloc} for a list 
!          of predefined stagger locations. If not present, defaults to ESMF\_STAGGERLOC\_CENTER.
!     \item[{[exclusiveLBound]}]
!          Upon return this holds the lower bounds of the exclusive region.
!          {\tt exclusiveLBound} must be allocated to be of size equal to the coord dimCount.
!     \item[{[exclusiveUBound]}]
!          Upon return this holds the upper bounds of the exclusive region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the coord dimCount.
!     \item[{[exclusiveCount]}]
!          Upon return this holds the number of items in the exclusive region per dimension
!          (i.e. {\tt exclusiveUBound-exclusiveLBound+1}). {\tt exclusiveCount} must
!          be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalLBound]}]
!          Upon return this holds the lower bounds of the stagger region.
!          {\tt computationalLBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalUBound]}]
!          Upon return this holds the upper bounds of the stagger region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalCount]}]
!          Upon return this holds the number of items in the computational region per dimension
!          (i.e. {\tt computationalUBound-computationalLBound+1}). {\tt computationalCount}
!          must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalLBound]}]
!          Upon return this holds the lower bounds of the total region.
!          {\tt totalLBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalUBound]}]
!          Upon return this holds the upper bounds of the total region.
!          {\tt totalUBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalCount]}]
!          Upon return this holds the number of items in the total region per dimension
!          (i.e. {\tt totalUBound-totalLBound+1}). {\tt totalCount} must
!          be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{fptr}]
!          The pointer to the coordinate data.
!     \item[{[doCopy]}]
!          If not specified, default to {\tt ESMF\_DATA\_REF}, in this case
!          fptr is a reference to the data in the Grid coordinate arrays. 
!          Please see Section~\ref{opt:copyflag} for further description and a
!          list of valid values. 
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
          exclusiveLBound, exclusiveUBound, exclusiveCount,                 &
          computationalLBound, computationalUBound, computationalCount,     &
          totalLBound, totalUBound, totalCount,                             &
          fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid),        intent(in) :: grid
      integer,                intent(in) :: localDE
      integer,                intent(in) :: coordDim
      type (ESMF_StaggerLoc), intent(in), optional :: staggerloc
      integer,        target, intent(out), optional :: exclusiveLBound(:)
      integer,        target, intent(out), optional :: exclusiveUBound(:)
      integer,        target, intent(out), optional :: exclusiveCount(:)
      integer,        target, intent(out), optional :: computationalLBound(:)
      integer,        target, intent(out), optional :: computationalUBound(:)
      integer,        target, intent(out), optional :: computationalCount(:)
      integer,        target, intent(out), optional :: totalLBound(:)
      integer,        target, intent(out), optional :: totalUBound(:)
      integer,        target, intent(out), optional :: totalCount(:)
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
!          The local DE to get the information for. {\tt [0,..,localDeCount-1]}
!     \item[{coordDim}]
!          The coordinate dimension to get the data from (e.g. 1=x).
!     \item[{staggerloc}]
!          The stagger location to get the information for. 
!          Please see Section~\ref{sec:opt:staggerloc} for a list 
!          of predefined stagger locations. If not present, defaults to ESMF\_STAGGERLOC\_CENTER.
!     \item[{[exclusiveLBound]}]
!          Upon return this holds the lower bounds of the exclusive region.
!          {\tt exclusiveLBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[exclusiveUBound]}]
!          Upon return this holds the upper bounds of the exclusive region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[exclusiveCount]}]
!          Upon return this holds the number of items in the exclusive region per dimension
!          (i.e. {\tt exclusiveUBound-exclusiveLBound+1}). {\tt exclusiveCount} must
!          be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalLBound]}]
!          Upon return this holds the lower bounds of the stagger region.
!          {\tt computationalLBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalUBound]}]
!          Upon return this holds the upper bounds of the stagger region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalCount]}]
!          Upon return this holds the number of items in the computational region per dimension
!          (i.e. {\tt computationalUBound-computationalLBound+1}). {\tt computationalCount}
!          must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalLBound]}]
!          Upon return this holds the lower bounds of the total region.
!          {\tt totalLBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalUBound]}]
!          Upon return this holds the upper bounds of the total region.
!          {\tt totalUBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalCount]}]
!          Upon return this holds the number of items in the total region per dimension
!          (i.e. {\tt totalUBound-totalLBound+1}). {\tt totalCount} must
!          be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{fptr}]
!          The pointer to the coordinate data.
!     \item[{[doCopy]}]
!          If not specified, default to {\tt ESMF\_DATA\_REF}, in this case
!          fptr is a reference to the data in the Grid coordinate arrays. 
!          Please see Section~\ref{opt:copyflag} for further description and a
!          list of valid values. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

    ! Local variables 
    type(ESMF_Array) :: array 
    integer :: localrc ! local error status 
    integer :: localDeCount, dimCount 
    type(ESMF_TypeKind) :: typekind 
    type(ESMF_LocalArray), allocatable :: larrayList(:) 
    type(ESMF_CopyFlag) :: docopyInt
    integer :: coordDimCount(ESMF_MAXDIM)
    type(ESMF_InterfaceInt) :: exclusiveLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: exclusiveUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: exclusiveCountArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalCountArg ! helper variable
    type(ESMF_InterfaceInt) :: totalLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: totalUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: totalCountArg ! helper variable
    integer :: tmp_staggerloc
    type(ESMF_GridDecompType) :: decompType

    ! Initialize return code 
    localrc = ESMF_RC_NOT_IMPL 
    if (present(rc)) rc = ESMF_RC_NOT_IMPL 

    ! Check init status of arguments 
    ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid, rc) 

    call ESMF_GridGetDecompType(grid, decompType, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Check consistency 
    call ESMF_GridGet(grid, coordTypeKind=typekind, dimCount=dimCount, coordDimCount=coordDimCount, &
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
    if ((coordDim .lt. 1) .or. (coordDim .gt. dimCount)) then
      call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
        "- coordinate dimension outside of range specified for this Grid", & 
        ESMF_CONTEXT, rc) 
      return 
    endif 

    ! Require farrayPtr dimCount to match coordinate dimCount 
    if (coordDimCount(coordDim) .ne. 1) then 
      call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
        "- farrayPtr dimCount does not match requested coordinate dimCount", & 
        ESMF_CONTEXT, rc) 
      return 
    endif 

    ! Set Defaults
    if (present(docopy)) then
      docopyInt=docopy
    else
      docopyInt=ESMF_DATA_REF
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
 
    if (localDE>=localDeCount) then 
      call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
        "- localDE too big", ESMF_CONTEXT, rc) 
      return 
    endif 

    if (localDE<0) then 
      call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
        "- localDE can't be less than 0", ESMF_CONTEXT, rc) 
      return 
    endif 

     ! handle staggerloc
    if (present(staggerloc)) then
       if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. &
	  (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
                 ESMF_CONTEXT, rc) 
           return
	else
       	   tmp_staggerloc=staggerloc%staggerloc
	endif
    else
       tmp_staggerloc=ESMF_STAGGERLOC_CENTER%staggerloc
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
 
    call ESMF_LocalArrayGet(larrayList(localDE+1), fptr, doCopy, rc=localrc) 
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
    exclusiveCountArg=ESMF_InterfaceIntCreate(exclusiveCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalLBoundArg=ESMF_InterfaceIntCreate(computationalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalUBoundArg=ESMF_InterfaceIntCreate(computationalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalCountArg=ESMF_InterfaceIntCreate(computationalCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalLBoundArg=ESMF_InterfaceIntCreate(totalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalUBoundArg = ESMF_InterfaceIntCreate(totalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalCountArg = ESMF_InterfaceIntCreate(totalCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_GridGetCoordBounds(grid, localDE, coordDim, tmp_staggerloc, &
      exclusiveLBoundArg, exclusiveUBoundArg, exclusiveCountArg, &
      computationalLBoundArg, computationalUBoundArg, computationalCountArg,&
      totalLBoundArg, totalUBoundArg, totalCountArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deallocate interface ints
    call ESMF_InterfaceIntDestroy(exclusiveLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalCountArg, rc=localrc)
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
          exclusiveLBound, exclusiveUBound, exclusiveCount,                 &
          computationalLBound, computationalUBound, computationalCount,     &
          totalLBound, totalUBound, totalCount,                             &
          fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in),optional :: localDE
      integer, intent(in) :: coordDim
      type (ESMF_StaggerLoc), intent(in),optional :: staggerloc
      integer,        target, intent(out), optional :: exclusiveLBound(:)
      integer,        target, intent(out), optional :: exclusiveUBound(:)
      integer,        target, intent(out), optional :: exclusiveCount(:)
      integer,        target, intent(out), optional :: computationalLBound(:)
      integer,        target, intent(out), optional :: computationalUBound(:)
      integer,        target, intent(out), optional :: computationalCount(:)
      integer,        target, intent(out), optional :: totalLBound(:)
      integer,        target, intent(out), optional :: totalUBound(:)
      integer,        target, intent(out), optional :: totalCount(:)
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
!          The local DE to get the information for. {\tt [0,..,localDeCount-1]}
!     \item[{coordDim}]
!          The coordinate dimension to get the data from (e.g. 1=x).
!     \item[{staggerloc}]
!          The stagger location to get the information for. 
!          Please see Section~\ref{sec:opt:staggerloc} for a list 
!          of predefined stagger locations. If not present, defaults to
!          ESMF\_STAGGERLOC\_CENTER.
!     \item[{[exclusiveLBound]}]
!          Upon return this holds the lower bounds of the exclusive region.
!          {\tt exclusiveLBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[exclusiveUBound]}]
!          Upon return this holds the upper bounds of the exclusive region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[exclusiveCount]}]
!          Upon return this holds the number of items in the exclusive region per dimension
!          (i.e. {\tt exclusiveUBound-exclusiveLBound+1}). {\tt exclusiveCount} must
!          be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalLBound]}]
!          Upon return this holds the lower bounds of the stagger region.
!          {\tt computationalLBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalUBound]}]
!          Upon return this holds the upper bounds of the stagger region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalCount]}]
!          Upon return this holds the number of items in the computational region per dimension
!          (i.e. {\tt computationalUBound-computationalLBound+1}). {\tt computationalCount}
!          must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalLBound]}]
!          Upon return this holds the lower bounds of the total region.
!          {\tt totalLBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalUBound]}]
!          Upon return this holds the upper bounds of the total region.
!          {\tt totalUBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalCount]}]
!          Upon return this holds the number of items in the total region per dimension
!          (i.e. {\tt totalUBound-totalLBound+1}). {\tt totalCount} must
!          be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{fptr}]
!          The pointer to the coordinate data.
!     \item[{[doCopy]}]
!          If not specified, default to {\tt ESMF\_DATA\_REF}, in this case
!          fptr is a reference to the data in the Grid coordinate arrays. 
!          Please see Section~\ref{opt:copyflag} for further description and a
!          list of valid values. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI


    ! Local variables 
    type(ESMF_Array) :: array 
    integer :: localrc ! local error status 
    integer :: localDeCount, dimCount 
    type(ESMF_TypeKind) :: typekind 
    type(ESMF_LocalArray), allocatable :: larrayList(:) 
    type(ESMF_CopyFlag) :: docopyInt
    integer :: coordDimCount(ESMF_MAXDIM)
    type(ESMF_InterfaceInt) :: exclusiveLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: exclusiveUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: exclusiveCountArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalCountArg ! helper variable
    type(ESMF_InterfaceInt) :: totalLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: totalUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: totalCountArg ! helper variable
    integer :: tmp_staggerloc
    type(ESMF_GridDecompType) :: decompType

    ! Initialize return code 
    localrc = ESMF_RC_NOT_IMPL 
    if (present(rc)) rc = ESMF_RC_NOT_IMPL 

    ! Check init status of arguments 
    ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid, rc) 

    call ESMF_GridGetDecompType(grid, decompType, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Check consistency 
    call ESMF_GridGet(grid, coordTypeKind=typekind, dimCount=dimCount, coordDimCount=coordDimCount, &
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
    if ((coordDim .lt. 1) .or. (coordDim .gt. dimCount)) then
      call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
        "- coordinate dimension outside of range specified for this Grid", & 
        ESMF_CONTEXT, rc) 
      return 
    endif 

    ! Require farrayPtr dimCount to match coordinate dimCount 
    if (coordDimCount(coordDim) .ne. 2) then 
    call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
      "- farrayPtr dimCount does not match requested coordinate dimCount", & 
      ESMF_CONTEXT, rc) 
    return 
    endif 

    ! Set Defaults
    if (present(docopy)) then
      docopyInt=docopy
    else
      docopyInt=ESMF_DATA_REF
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
 
    if (localDE>=localDeCount) then 
      call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
        "- localDE too big", ESMF_CONTEXT, rc) 
      return 
    endif 

    if (localDE<0) then 
      call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
        "- localDE can't be less than 0", & 
        ESMF_CONTEXT, rc) 
      return 
    endif 

     ! handle staggerloc
    if (present(staggerloc)) then
       if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. &
	  (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
                 ESMF_CONTEXT, rc) 
           return
	else
       	   tmp_staggerloc=staggerloc%staggerloc
	endif
    else
       tmp_staggerloc=ESMF_STAGGERLOC_CENTER%staggerloc
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
 
    call ESMF_LocalArrayGet(larrayList(localDE+1), fptr, doCopy, rc=localrc) 
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
    exclusiveCountArg=ESMF_InterfaceIntCreate(exclusiveCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalLBoundArg=ESMF_InterfaceIntCreate(computationalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalUBoundArg=ESMF_InterfaceIntCreate(computationalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalCountArg=ESMF_InterfaceIntCreate(computationalCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalLBoundArg=ESMF_InterfaceIntCreate(totalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalUBoundArg = ESMF_InterfaceIntCreate(totalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalCountArg = ESMF_InterfaceIntCreate(totalCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_GridGetCoordBounds(grid, localDE, coordDim, tmp_staggerloc, &
      exclusiveLBoundArg, exclusiveUBoundArg, exclusiveCountArg, &
      computationalLBoundArg, computationalUBoundArg, computationalCountArg,&
      totalLBoundArg, totalUBoundArg, totalCountArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deallocate interface ints
    call ESMF_InterfaceIntDestroy(exclusiveLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalCountArg, rc=localrc)
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
          exclusiveLBound, exclusiveUBound, exclusiveCount,                 &
          computationalLBound, computationalUBound, computationalCount,     &
          totalLBound, totalUBound, totalCount,                             &      
          fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in) :: localDE
      integer, intent(in) :: coordDim
      type (ESMF_StaggerLoc), intent(in),  optional :: staggerloc
      integer,        target, intent(out), optional :: exclusiveLBound(:)
      integer,        target, intent(out), optional :: exclusiveUBound(:)
      integer,        target, intent(out), optional :: exclusiveCount(:)
      integer,        target, intent(out), optional :: computationalLBound(:)
      integer,        target, intent(out), optional :: computationalUBound(:)
      integer,        target, intent(out), optional :: computationalCount(:)
      integer,        target, intent(out), optional :: totalLBound(:)
      integer,        target, intent(out), optional :: totalUBound(:)
      integer,        target, intent(out), optional :: totalCount(:)
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
!          The local DE to get the information for. {\tt [0,..,localDeCount-1]}
!     \item[{coordDim}]
!          The coordinate dimension to get the data from (e.g. 1=x).
!     \item[{staggerloc}]
!          The stagger location to get the information for. 
!          Please see Section~\ref{sec:opt:staggerloc} for a list 
!          of predefined stagger locations. If not present, defaults to
!          ESMF\_STAGGERLOC\_CENTER.
!     \item[{[exclusiveLBound]}]
!          Upon return this holds the lower bounds of the exclusive region.
!          {\tt exclusiveLBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[exclusiveUBound]}]
!          Upon return this holds the upper bounds of the exclusive region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[exclusiveCount]}]
!          Upon return this holds the number of items in the exclusive region per dimension
!          (i.e. {\tt exclusiveUBound-exclusiveLBound+1}). {\tt exclusiveCount} must
!          be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalLBound]}]
!          Upon return this holds the lower bounds of the stagger region.
!          {\tt computationalLBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalUBound]}]
!          Upon return this holds the upper bounds of the stagger region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalCount]}]
!          Upon return this holds the number of items in the computational region per dimension
!          (i.e. {\tt computationalUBound-computationalLBound+1}). {\tt computationalCount}
!          must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalLBound]}]
!          Upon return this holds the lower bounds of the total region.
!          {\tt totalLBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalUBound]}]
!          Upon return this holds the upper bounds of the total region.
!          {\tt totalUBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalCount]}]
!          Upon return this holds the number of items in the total region per dimension
!          (i.e. {\tt totalUBound-totalLBound+1}). {\tt totalCount} must
!          be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{fptr}]
!          The pointer to the coordinate data.
!     \item[{[doCopy]}]
!          If not specified, default to {\tt ESMF\_DATA\_REF}, in this case
!          fptr is a reference to the data in the Grid coordinate arrays. 
!          Please see Section~\ref{opt:copyflag} for further description and a
!          list of valid values. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI


 ! Local variables 
 type(ESMF_Array) :: array 
 integer :: localrc ! local error status 
 integer :: localDeCount, dimCount 
 type(ESMF_TypeKind) :: typekind 
 type(ESMF_LocalArray), allocatable :: larrayList(:) 
 type(ESMF_CopyFlag) :: docopyInt
 integer :: coordDimCount(ESMF_MAXDIM)
 type(ESMF_InterfaceInt) :: exclusiveLBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: exclusiveUBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: exclusiveCountArg ! helper variable
 type(ESMF_InterfaceInt) :: computationalLBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: computationalUBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: computationalCountArg ! helper variable
 type(ESMF_InterfaceInt) :: totalLBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: totalUBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: totalCountArg ! helper variable
 integer :: tmp_staggerloc
 type(ESMF_GridDecompType) :: decompType

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid, rc) 

 call ESMF_GridGetDecompType(grid, decompType, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) return


 ! Check consistency 
 call ESMF_GridGet(grid, coordTypeKind=typekind, dimCount=dimCount, coordDimCount=coordDimCount, &
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
if ((coordDim .lt. 1) .or. (coordDim .gt. dimCount)) then
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- coordinate dimension outside of range specified for this Grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

 ! Require farrayPtr dimCount to match coordinate dimCount 
 if (coordDimCount(coordDim) .ne. 3) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- farrayPtr dimCount does not match requested coordinate dimCount", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

! Set Defaults
if (present(docopy)) then
   docopyInt=docopy
else
  docopyInt=ESMF_DATA_REF
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
 
 if (localDE>=localDeCount) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- localDE too big", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

 if (localDE<0) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- localDE can't be less than 0", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 


    ! handle staggerloc
    if (present(staggerloc)) then
       if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. &
	  (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
                 ESMF_CONTEXT, rc) 
           return
	else
       	   tmp_staggerloc=staggerloc%staggerloc
	endif
    else
       tmp_staggerloc=ESMF_STAGGERLOC_CENTER%staggerloc
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
 
    call ESMF_LocalArrayGet(larrayList(localDE+1), fptr, doCopy, rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return 
    deallocate(larrayList) 


    ! process optional arguments
    ! for non-arbitrarily grid only
    ! should check these optional arguments are not present for arbitrary grid????
    if (decompType .ne. ESMF_GRID_ARBITRARY) then

    exclusiveLBoundArg=ESMF_InterfaceIntCreate(exclusiveLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    exclusiveUBoundArg=ESMF_InterfaceIntCreate(exclusiveUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    exclusiveCountArg=ESMF_InterfaceIntCreate(exclusiveCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalLBoundArg=ESMF_InterfaceIntCreate(computationalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalUBoundArg=ESMF_InterfaceIntCreate(computationalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalCountArg=ESMF_InterfaceIntCreate(computationalCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalLBoundArg=ESMF_InterfaceIntCreate(totalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalUBoundArg = ESMF_InterfaceIntCreate(totalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalCountArg = ESMF_InterfaceIntCreate(totalCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_GridGetCoordBounds(grid, localDE, coordDim, tmp_staggerloc, &
      exclusiveLBoundArg, exclusiveUBoundArg, exclusiveCountArg, &
      computationalLBoundArg, computationalUBoundArg, computationalCountArg,&
      totalLBoundArg, totalUBoundArg, totalCountArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deallocate interface ints
    call ESMF_InterfaceIntDestroy(exclusiveLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    endif

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
          exclusiveLBound, exclusiveUBound, exclusiveCount,                 &
          computationalLBound, computationalUBound, computationalCount,     &
          totalLBound, totalUBound, totalCount,                             &
          fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in)         :: localDE
      integer, intent(in) :: coordDim
      type (ESMF_StaggerLoc), intent(in),optional :: staggerloc
      integer,        target, intent(out), optional :: exclusiveLBound(:)
      integer,        target, intent(out), optional :: exclusiveUBound(:)
      integer,        target, intent(out), optional :: exclusiveCount(:)
      integer,        target, intent(out), optional :: computationalLBound(:)
      integer,        target, intent(out), optional :: computationalUBound(:)
      integer,        target, intent(out), optional :: computationalCount(:)
      integer,        target, intent(out), optional :: totalLBound(:)
      integer,        target, intent(out), optional :: totalUBound(:)
      integer,        target, intent(out), optional :: totalCount(:)
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
!          The local DE to get the information for. {\tt [0,..,localDeCount-1]}
!     \item[{coordDim}]
!          The coordinate dimension to get the data from (e.g. 1=x).
!     \item[{staggerloc}]
!          The stagger location to get the information for. 
!          Please see Section~\ref{sec:opt:staggerloc} for a list 
!          of predefined stagger locations. If not present, defaults to
!          ESMF\_STAGGERLOC\_CENTER.
!     \item[{[exclusiveLBound]}]
!          Upon return this holds the lower bounds of the exclusive region.
!          {\tt exclusiveLBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[exclusiveUBound]}]
!          Upon return this holds the upper bounds of the exclusive region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[exclusiveCount]}]
!          Upon return this holds the number of items in the exclusive region per dimension
!          (i.e. {\tt exclusiveUBound-exclusiveLBound+1}). {\tt exclusiveCount} must
!          be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalLBound]}]
!          Upon return this holds the lower bounds of the stagger region.
!          {\tt computationalLBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalUBound]}]
!          Upon return this holds the upper bounds of the stagger region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalCount]}]
!          Upon return this holds the number of items in the computational region per dimension
!          (i.e. {\tt computationalUBound-computationalLBound+1}). {\tt computationalCount}
!          must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalLBound]}]
!          Upon return this holds the lower bounds of the total region.
!          {\tt totalLBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalUBound]}]
!          Upon return this holds the upper bounds of the total region.
!          {\tt totalUBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalCount]}]
!          Upon return this holds the number of items in the total region per dimension
!          (i.e. {\tt totalUBound-totalLBound+1}). {\tt totalCount} must
!          be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{fptr}]
!          The pointer to the coordinate data.
!     \item[{[doCopy]}]
!          If not specified, default to {\tt ESMF\_DATA\_REF}, in this case
!          fptr is a reference to the data in the Grid coordinate arrays. 
!          Please see Section~\ref{opt:copyflag} for further description and a
!          list of valid values. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI


    ! Local variables 
    type(ESMF_Array) :: array 
    integer :: localrc ! local error status 
    integer :: localDeCount, dimCount 
    type(ESMF_TypeKind) :: typekind 
    type(ESMF_LocalArray), allocatable :: larrayList(:) 
    type(ESMF_CopyFlag) :: docopyInt
    integer :: coordDimCount(ESMF_MAXDIM)
    type(ESMF_InterfaceInt) :: exclusiveLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: exclusiveUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: exclusiveCountArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalCountArg ! helper variable
    type(ESMF_InterfaceInt) :: totalLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: totalUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: totalCountArg ! helper variable
    integer :: tmp_staggerloc
    type(ESMF_GridDecompType) :: decompType

    ! Initialize return code 
    localrc = ESMF_RC_NOT_IMPL 
    if (present(rc)) rc = ESMF_RC_NOT_IMPL 

    ! Check init status of arguments 
    ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid, rc) 

    call ESMF_GridGetDecompType(grid, decompType, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


    ! Check consistency 
    call ESMF_GridGet(grid, coordTypeKind=typekind, dimCount=dimCount, coordDimCount=coordDimCount, &
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
    if ((coordDim .lt. 1) .or. (coordDim .gt. dimCount)) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
       "- coordinate dimension outside of range specified for this Grid", & 
       ESMF_CONTEXT, rc) 
       return 
    endif 

    ! Require farrayPtr dimCount to match coordinate dimCount 
    if (coordDimCount(coordDim) .ne. 1) then 
       call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
       "- farrayPtr dimCount does not match requested coordinate dimCount", & 
       ESMF_CONTEXT, rc) 
       return 
    endif 

    ! Set Defaults
    if (present(docopy)) then
       docopyInt=docopy
    else
       docopyInt=ESMF_DATA_REF
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
 
    if (localDE>=localDeCount) then 
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
           "- localDE too big", & 
           ESMF_CONTEXT, rc) 
       return 
    endif 

    if (localDE<0) then 
        call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
           "- localDE can't be less than 0", & 
           ESMF_CONTEXT, rc) 
        return 
    endif 

    ! handle staggerloc
    if (present(staggerloc)) then
       if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. &
	  (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
                 ESMF_CONTEXT, rc) 
           return
	else
       	   tmp_staggerloc=staggerloc%staggerloc
	endif
    else
       tmp_staggerloc=ESMF_STAGGERLOC_CENTER%staggerloc
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

    call ESMF_LocalArrayGet(larrayList(localDE+1), fptr, doCopy, rc=localrc) 
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
    exclusiveCountArg=ESMF_InterfaceIntCreate(exclusiveCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalLBoundArg=ESMF_InterfaceIntCreate(computationalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalUBoundArg=ESMF_InterfaceIntCreate(computationalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalCountArg=ESMF_InterfaceIntCreate(computationalCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalLBoundArg=ESMF_InterfaceIntCreate(totalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalUBoundArg = ESMF_InterfaceIntCreate(totalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalCountArg = ESMF_InterfaceIntCreate(totalCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_GridGetCoordBounds(grid, localDE, coordDim, tmp_staggerloc, &
      exclusiveLBoundArg, exclusiveUBoundArg, exclusiveCountArg, &
      computationalLBoundArg, computationalUBoundArg, computationalCountArg,&
      totalLBoundArg, totalUBoundArg, totalCountArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


    ! Deallocate interface ints
    call ESMF_InterfaceIntDestroy(exclusiveLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalCountArg, rc=localrc)
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
          exclusiveLBound, exclusiveUBound, exclusiveCount,                 &
          computationalLBound, computationalUBound, computationalCount,     &
          totalLBound, totalUBound, totalCount,                             &
          fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in) :: localDE
      integer, intent(in) :: coordDim
      type (ESMF_StaggerLoc), intent(in),optional :: staggerloc
      integer,        target, intent(out), optional :: exclusiveLBound(:)
      integer,        target, intent(out), optional :: exclusiveUBound(:)
      integer,        target, intent(out), optional :: exclusiveCount(:)
      integer,        target, intent(out), optional :: computationalLBound(:)
      integer,        target, intent(out), optional :: computationalUBound(:)
      integer,        target, intent(out), optional :: computationalCount(:)
      integer,        target, intent(out), optional :: totalLBound(:)
      integer,        target, intent(out), optional :: totalUBound(:)
      integer,        target, intent(out), optional :: totalCount(:)
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
!          The local DE to get the information for. {\tt [0,..,localDeCount-1]}
!     \item[{coordDim}]
!          The coordinate dimension to get the data from (e.g. 1=x).
!     \item[{staggerloc}]
!          The stagger location to get the information for. 
!          Please see Section~\ref{sec:opt:staggerloc} for a list 
!          of predefined stagger locations. If not present, defaults to
!          ESMF\_STAGGERLOC\_CENTER.
!     \item[{[exclusiveLBound]}]
!          Upon return this holds the lower bounds of the exclusive region.
!          {\tt exclusiveLBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[exclusiveUBound]}]
!          Upon return this holds the upper bounds of the exclusive region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[exclusiveCount]}]
!          Upon return this holds the number of items in the exclusive region per dimension
!          (i.e. {\tt exclusiveUBound-exclusiveLBound+1}). {\tt exclusiveCount} must
!          be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalLBound]}]
!          Upon return this holds the lower bounds of the stagger region.
!          {\tt computationalLBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalUBound]}]
!          Upon return this holds the upper bounds of the stagger region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalCount]}]
!          Upon return this holds the number of items in the computational region per dimension
!          (i.e. {\tt computationalUBound-computationalLBound+1}). {\tt computationalCount}
!          must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalLBound]}]
!          Upon return this holds the lower bounds of the total region.
!          {\tt totalLBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalUBound]}]
!          Upon return this holds the upper bounds of the total region.
!          {\tt totalUBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalCount]}]
!          Upon return this holds the number of items in the total region per dimension
!          (i.e. {\tt totalUBound-totalLBound+1}). {\tt totalCount} must
!          be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{fptr}]
!          The pointer to the coordinate data.
!     \item[{[doCopy]}]
!          If not specified, default to {\tt ESMF\_DATA\_REF}, in this case
!          fptr is a reference to the data in the Grid coordinate arrays. 
!          Please see Section~\ref{opt:copyflag} for further description and a
!          list of valid values. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI


 ! Local variables 
 type(ESMF_Array) :: array 
 integer :: localrc ! local error status 
 integer :: localDeCount, dimCount 
 type(ESMF_TypeKind) :: typekind 
 type(ESMF_LocalArray), allocatable :: larrayList(:) 
 type(ESMF_CopyFlag) :: docopyInt
 integer :: coordDimCount(ESMF_MAXDIM)
 type(ESMF_InterfaceInt) :: exclusiveLBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: exclusiveUBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: exclusiveCountArg ! helper variable
 type(ESMF_InterfaceInt) :: computationalLBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: computationalUBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: computationalCountArg ! helper variable
 type(ESMF_InterfaceInt) :: totalLBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: totalUBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: totalCountArg ! helper variable
 integer :: tmp_staggerloc
 type(ESMF_GridDecompType) :: decompType

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid, rc) 

 call ESMF_GridGetDecompType(grid, decompType, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) return


 ! Check consistency 
 call ESMF_GridGet(grid, coordTypeKind=typekind, dimCount=dimCount, coordDimCount=coordDimCount, &
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
if ((coordDim .lt. 1) .or. (coordDim .gt. dimCount)) then
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- coordinate dimension outside of range specified for this Grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

 ! Require farrayPtr dimCount to match coordinate dimCount 
 if (coordDimCount(coordDim) .ne. 2) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- farrayPtr dimCount does not match requested coordinate dimCount", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

! Set Defaults
if (present(docopy)) then
   docopyInt=docopy
else
  docopyInt=ESMF_DATA_REF
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
 
 if (localDE>=localDeCount) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- localDE too big", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

 if (localDE<0) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- localDE can't be less than 0", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

    ! handle staggerloc
    if (present(staggerloc)) then
       if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. &
	  (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
                 ESMF_CONTEXT, rc) 
           return
	else
       	   tmp_staggerloc=staggerloc%staggerloc
	endif
    else
       tmp_staggerloc=ESMF_STAGGERLOC_CENTER%staggerloc
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
 
    call ESMF_LocalArrayGet(larrayList(localDE+1), fptr, doCopy, rc=localrc) 
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
    exclusiveCountArg=ESMF_InterfaceIntCreate(exclusiveCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalLBoundArg=ESMF_InterfaceIntCreate(computationalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalUBoundArg=ESMF_InterfaceIntCreate(computationalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalCountArg=ESMF_InterfaceIntCreate(computationalCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalLBoundArg=ESMF_InterfaceIntCreate(totalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalUBoundArg = ESMF_InterfaceIntCreate(totalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalCountArg = ESMF_InterfaceIntCreate(totalCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_GridGetCoordBounds(grid, localDE, coordDim, tmp_staggerloc, &
      exclusiveLBoundArg, exclusiveUBoundArg, exclusiveCountArg, &
      computationalLBoundArg, computationalUBoundArg, computationalCountArg,&
      totalLBoundArg, totalUBoundArg, totalCountArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deallocate interface ints
    call ESMF_InterfaceIntDestroy(exclusiveLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalCountArg, rc=localrc)
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
          exclusiveLBound, exclusiveUBound, exclusiveCount,                 &
          computationalLBound, computationalUBound, computationalCount,     &
          totalLBound, totalUBound, totalCount,                             &
          fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in) :: localDE
      integer, intent(in) :: coordDim
      type (ESMF_StaggerLoc), intent(in),optional :: staggerloc
      integer,        target, intent(out), optional :: exclusiveLBound(:)
      integer,        target, intent(out), optional :: exclusiveUBound(:)
      integer,        target, intent(out), optional :: exclusiveCount(:)
      integer,        target, intent(out), optional :: computationalLBound(:)
      integer,        target, intent(out), optional :: computationalUBound(:)
      integer,        target, intent(out), optional :: computationalCount(:)
      integer,        target, intent(out), optional :: totalLBound(:)
      integer,        target, intent(out), optional :: totalUBound(:)
      integer,        target, intent(out), optional :: totalCount(:)
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
!          The local DE to get the information for. {\tt [0,..,localDeCount-1]}
!     \item[{coordDim}]
!          The coordinate dimension to get the data from (e.g. 1=x).
!     \item[{staggerloc}]
!          The stagger location to get the information for. 
!          Please see Section~\ref{sec:opt:staggerloc} for a list 
!          of predefined stagger locations. If not present, defaults to
!          ESMF\_STAGGERLOC\_CENTER.
!     \item[{[exclusiveLBound]}]
!          Upon return this holds the lower bounds of the exclusive region.
!          {\tt exclusiveLBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[exclusiveUBound]}]
!          Upon return this holds the upper bounds of the exclusive region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[exclusiveCount]}]
!          Upon return this holds the number of items in the exclusive region per dimension
!          (i.e. {\tt exclusiveUBound-exclusiveLBound+1}). {\tt exclusiveCount} must
!          be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalLBound]}]
!          Upon return this holds the lower bounds of the stagger region.
!          {\tt computationalLBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalUBound]}]
!          Upon return this holds the upper bounds of the stagger region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalCount]}]
!          Upon return this holds the number of items in the computational region per dimension
!          (i.e. {\tt computationalUBound-computationalLBound+1}). {\tt computationalCount}
!          must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalLBound]}]
!          Upon return this holds the lower bounds of the total region.
!          {\tt totalLBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalUBound]}]
!          Upon return this holds the upper bounds of the total region.
!          {\tt totalUBound} must be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalCount]}]
!          Upon return this holds the number of items in the total region per dimension
!          (i.e. {\tt totalUBound-totalLBound+1}). {\tt totalCount} must
!          be allocated to be of size equal to the coord dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{fptr}]
!          The pointer to the coordinate data.
!     \item[{[doCopy]}]
!          If not specified, default to {\tt ESMF\_DATA\_REF}, in this case
!          fptr is a reference to the data in the Grid coordinate arrays. 
!          Please see Section~\ref{opt:copyflag} for further description and a
!          list of valid values. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI


 ! Local variables 
 type(ESMF_Array) :: array 
 integer :: localrc ! local error status 
 integer :: localDeCount, dimCount 
 type(ESMF_TypeKind) :: typekind 
 type(ESMF_LocalArray), allocatable :: larrayList(:) 
 type(ESMF_CopyFlag) :: docopyInt
 integer :: coordDimCount(ESMF_MAXDIM)
 type(ESMF_InterfaceInt) :: exclusiveLBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: exclusiveUBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: exclusiveCountArg ! helper variable
 type(ESMF_InterfaceInt) :: computationalLBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: computationalUBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: computationalCountArg ! helper variable
 type(ESMF_InterfaceInt) :: totalLBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: totalUBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: totalCountArg ! helper variable
 integer :: tmp_staggerloc
 type(ESMF_GridDecompType) :: decompType

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid, rc) 

 call ESMF_GridGetDecompType(grid, decompType, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)) return


 ! Check consistency 
 call ESMF_GridGet(grid, coordTypeKind=typekind, dimCount=dimCount, coordDimCount=coordDimCount, &
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
if ((coordDim .lt. 1) .or. (coordDim .gt. dimCount)) then
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- coordinate dimension outside of range specified for this Grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

 ! Require farrayPtr dimCount to match coordinate dimCount 
 if (coordDimCount(coordDim) .ne. 3) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- farrayPtr dimCount does not match requested coordinate dimCount", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

! Set Defaults
if (present(docopy)) then
   docopyInt=docopy
else
  docopyInt=ESMF_DATA_REF
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
 
 if (localDE>=localDeCount) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- localDE too big", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

 if (localDE<0) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- localDE can't be less than 0", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

    ! handle staggerloc
    if (present(staggerloc)) then
       if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. &
	  (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
                 ESMF_CONTEXT, rc) 
           return
	else
       	   tmp_staggerloc=staggerloc%staggerloc
	endif
    else
       tmp_staggerloc=ESMF_STAGGERLOC_CENTER%staggerloc
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
 
    call ESMF_LocalArrayGet(larrayList(localDE+1), fptr, doCopy, rc=localrc) 
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
    exclusiveCountArg=ESMF_InterfaceIntCreate(exclusiveCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalLBoundArg=ESMF_InterfaceIntCreate(computationalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalUBoundArg=ESMF_InterfaceIntCreate(computationalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalCountArg=ESMF_InterfaceIntCreate(computationalCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalLBoundArg=ESMF_InterfaceIntCreate(totalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalUBoundArg = ESMF_InterfaceIntCreate(totalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalCountArg = ESMF_InterfaceIntCreate(totalCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_GridGetCoordBounds(grid, localDE, coordDim, tmp_staggerloc, &
      exclusiveLBoundArg, exclusiveUBoundArg, exclusiveCountArg, &
      computationalLBoundArg, computationalUBoundArg, computationalCountArg,&
      totalLBoundArg, totalUBoundArg, totalCountArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deallocate interface ints
    call ESMF_InterfaceIntDestroy(exclusiveLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully 
    if (present(rc)) rc = ESMF_SUCCESS 

    end subroutine ESMF_GridGetCoord3DR8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetCoordBounds"
!BOP
! !IROUTINE: ESMF_GridGetCoord -  Get Grid coordinate bounds

! !INTERFACE:
  ! Private name; call using ESMF_GridGetCoord()
      subroutine ESMF_GridGetCoordBounds(grid, localDE, coordDim, staggerloc, &
          exclusiveLBound, exclusiveUBound, exclusiveCount,                   &
          computationalLBound, computationalUBound, computationalCount,       &
          totalLBound, totalUBound, totalCount, rc)

!
! !ARGUMENTS:
      type(ESMF_Grid),        intent(in)            :: grid
      integer,                intent(in)            :: localDE
      integer,                intent(in)            :: coordDim
      type (ESMF_StaggerLoc), intent(in),  optional :: staggerloc
      integer,        target, intent(out), optional :: exclusiveLBound(:)
      integer,        target, intent(out), optional :: exclusiveUBound(:)
      integer,        target, intent(out), optional :: exclusiveCount(:)
      integer,        target, intent(out), optional :: computationalLBound(:)
      integer,        target, intent(out), optional :: computationalUBound(:)
      integer,        target, intent(out), optional :: computationalCount(:)
      integer,        target, intent(out), optional :: totalLBound(:)
      integer,        target, intent(out), optional :: totalUBound(:)
      integer,        target, intent(out), optional :: totalCount(:)
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
!     The local DE from which to get the information. {\tt [0,..,localDeCount-1]}
!\item[{coordDim}]
!     The coordinate dimension to get the information for (e.g. 1=x). 
!\item[{staggerloc}]
!     The stagger location to get the information for. 
!     Please see Section~\ref{sec:opt:staggerloc} for a list 
!     of predefined stagger locations. If not present, defaults to
!     ESMF\_STAGGERLOC\_CENTER.
!\item[{[exclusiveLBound]}]
!     Upon return this holds the lower bounds of the exclusive region.
!     {\tt exclusiveLBound} must be allocated to be of size equal to the coord dimCount.
!     Please see Section~\ref{sec:grid:usage:bounds} for a description
!     of the regions and their associated bounds and counts. 
!\item[{[exclusiveUBound]}]
!     Upon return this holds the upper bounds of the exclusive region.
!     {\tt exclusiveUBound} must be allocated to be of size equal to the coord dimCount.
!     Please see Section~\ref{sec:grid:usage:bounds} for a description
!     of the regions and their associated bounds and counts. 
!\item[{[exclusiveCount]}]
!     Upon return this holds the number of items in the exclusive region per dimension
!     (i.e. {\tt exclusiveUBound-exclusiveLBound+1}). {\tt exclusiveCount} must
!     be allocated to be of size equal to the coord dimCount.
!     Please see Section~\ref{sec:grid:usage:bounds} for a description
!     of the regions and their associated bounds and counts. 
!\item[{[computationalLBound]}]
!     Upon return this holds the lower bounds of the stagger region.
!     {\tt computationalLBound} must be allocated to be of size equal to the coord dimCount.
!     Please see Section~\ref{sec:grid:usage:bounds} for a description
!     of the regions and their associated bounds and counts. 
!\item[{[computationalUBound]}]
!     Upon return this holds the upper bounds of the stagger region.
!     {\tt computationalUBound} must be allocated to be of size equal to the coord dimCount.
!     Please see Section~\ref{sec:grid:usage:bounds} for a description
!     of the regions and their associated bounds and counts. 
!\item[{[computationalCount]}]
!     Upon return this holds the number of items in the computational region per dimension
!     (i.e. {\tt computationalUBound-computationalLBound+1}). {\tt computationalCount}
!      must be allocated to be of size equal to the coord dimCount.
!     Please see Section~\ref{sec:grid:usage:bounds} for a description
!     of the regions and their associated bounds and counts. 
!\item[{[totalLBound]}]
!     Upon return this holds the lower bounds of the total region.
!     {\tt totalLBound} must be allocated to be of size equal to the coord dimCount.
!     Please see Section~\ref{sec:grid:usage:bounds} for a description
!     of the regions and their associated bounds and counts. 
!\item[{[totalUBound]}]
!     Upon return this holds the upper bounds of the total region.
!     {\tt totalUBound} must be allocated to be of size equal to the coord dimCount.
!     Please see Section~\ref{sec:grid:usage:bounds} for a description
!     of the regions and their associated bounds and counts. 
!\item[{[totalCount]}]
!     Upon return this holds the number of items in the total region per dimension
!     (i.e. {\tt totalUBound-totalLBound+1}). {\tt totalCount} must
!      be allocated to be of size equal to the coord dimCount.
!     Please see Section~\ref{sec:grid:usage:bounds} for a description
!     of the regions and their associated bounds and counts. 
!\item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!\end{description}
!
!EOP

    integer :: localrc ! local error status
    type(ESMF_InterfaceInt) :: exclusiveLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: exclusiveUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: exclusiveCountArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalCountArg ! helper variable
    type(ESMF_InterfaceInt) :: totalLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: totalUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: totalCountArg ! helper variable
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
    exclusiveCountArg=ESMF_InterfaceIntCreate(exclusiveCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalLBoundArg=ESMF_InterfaceIntCreate(computationalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalUBoundArg=ESMF_InterfaceIntCreate(computationalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalCountArg=ESMF_InterfaceIntCreate(computationalCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalLBoundArg=ESMF_InterfaceIntCreate(totalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalUBoundArg = ESMF_InterfaceIntCreate(totalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalCountArg = ESMF_InterfaceIntCreate(totalCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_GridGetCoordBounds(grid, localDE, coordDim, tmp_staggerloc, &
      exclusiveLBoundArg, exclusiveUBoundArg, exclusiveCountArg, &
      computationalLBoundArg, computationalUBoundArg, computationalCountArg,&
      totalLBoundArg, totalUBoundArg, totalCountArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deallocate interface ints
    call ESMF_InterfaceIntDestroy(exclusiveLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalCountArg, rc=localrc)
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
!    must have already been allocated, for example by {\tt ESMF\_GridAddCoord} or
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
!          If not specified, default to {\tt ESMF\_DATA\_REF}, in this case 
!          {\tt array} will contain a reference to the Grid coordinate Arrays.
!           Please see Section~\ref{opt:copyflag} for
!          further description and a list of valid values. 
!          [THE ESMF\_DATA\_COPY OPTION IS CURRENTLY NOT IMPLEMENTED] 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

    integer :: tmp_staggerloc
    integer :: localrc ! local error status
    type(ESMF_GridDecompType) :: decompType

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_GridGetInit, grid, rc)

    call ESMF_GridGetDecompType(grid, decompType, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! handle staggerloc
    if (present(staggerloc)) then
       if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. &
	  (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
                 ESMF_CONTEXT, rc) 
           return
	else
       	   tmp_staggerloc=staggerloc%staggerloc
	endif
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
#define ESMF_METHOD "ESMF_GridGetCoordR4"
!BOP
! !IROUTINE: ESMF_GridGetCoord - Get coordinates from a specific index location

! !INTERFACE:
  ! Private name; call using ESMF_GridGetCoord()
      subroutine ESMF_GridGetCoordR4(grid, localDE, staggerloc, index, coord, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in)                 :: grid
      type (ESMF_StaggerLoc), intent(in),optional :: staggerloc
      integer, intent(in)                         :: localDE
      integer, intent(in)                         :: index(:)
      real(ESMF_KIND_R4)                          :: coord(:)
      integer, intent(out), optional              :: rc
!
! !DESCRIPTION:
!     Given a specific index location in a Grid, this method returns the full set
!   of coordinates from that index location. This method will eventually be overloaded
!   to support the full complement of types supported by the Grid. 
!
!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          Grid to get the information from.
!     \item[{[localDE]}]
!          The local DE to get the information for. {\tt [0,..,localDeCount-1]}
!     \item[{staggerloc}]
!          The stagger location to get the information for. 
!          Please see Section~\ref{sec:opt:staggerloc} for a list 
!          of predefined stagger locations. If not present, defaults to
!          ESMF\_STAGGERLOC\_CENTER.
!     \item[{index}]
!          This array holds the index location to be queried in the Grid. This array must
!          at least be of the size Grid rank.
!     \item[{coord}]
!          This array will be filled with the coordinate data. This array must
!          at least be of the size Grid rank.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI


 ! Local variables 
 integer :: localrc
 integer :: tmp_staggerloc

   ! Initialize return code 
   localrc = ESMF_RC_NOT_IMPL 
   if (present(rc)) rc = ESMF_RC_NOT_IMPL 

   ! Check init status of arguments 
   ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid, rc) 

   ! Have default option for staggerloc
   if (present(staggerloc)) then
       tmp_staggerloc=staggerloc%staggerloc
   else
       tmp_staggerloc=ESMF_STAGGERLOC_CENTER%staggerloc  ! default
   endif

   ! NOTE THERE IS NO INPUT VALUE CHECKING HERE BECAUSE IT'S DONE IN 
   ! THE C++ VERSION. 

   ! Call into the C++ interface
   call c_esmc_gridgetcoordr4(grid, localDE, tmp_staggerloc, &  
                              index, coord, localrc)
   if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


    ! Return successfully 
    if (present(rc)) rc = ESMF_SUCCESS 

    end subroutine ESMF_GridGetCoordR4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetCoordR8"
!BOP
! !IROUTINE: ESMF_GridGetCoord - Get coordinates from a specific index location

! !INTERFACE:
  ! Private name; call using ESMF_GridGetCoord()
      subroutine ESMF_GridGetCoordR8(grid, localDE, staggerloc, index, coord, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in)                 :: grid
      type (ESMF_StaggerLoc), intent(in),optional :: staggerloc
      integer, intent(in)                         :: localDE
      integer, intent(in)                         :: index(:)
      real(ESMF_KIND_R8)                          :: coord(:)
      integer, intent(out), optional              :: rc
!
! !DESCRIPTION:
!     Given a specific index location in a Grid, this method returns the full set
!   of coordinates from that index location. This method will eventually be overloaded
!   to support the full complement of types supported by the Grid. 
!
!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          Grid to get the information from.
!     \item[{[localDE]}]
!          The local DE to get the information for. {\tt [0,..,localDeCount-1]}
!     \item[{staggerloc}]
!          The stagger location to get the information for. 
!          Please see Section~\ref{sec:opt:staggerloc} for a list 
!          of predefined stagger locations. If not present, defaults to
!          ESMF\_STAGGERLOC\_CENTER.
!     \item[{index}]
!          This array holds the index location to be queried in the Grid. This array must
!          at least be of the size Grid rank.
!     \item[{coord}]
!          This array will be filled with the coordinate data. This array must
!          at least be of the size Grid rank.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI


 ! Local variables 
 integer :: localrc
 integer :: tmp_staggerloc

   ! Initialize return code 
   localrc = ESMF_RC_NOT_IMPL 
   if (present(rc)) rc = ESMF_RC_NOT_IMPL 

   ! Check init status of arguments 
   ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid, rc) 

   ! Have default option for staggerloc
   if (present(staggerloc)) then
       tmp_staggerloc=staggerloc%staggerloc
   else
       tmp_staggerloc=ESMF_STAGGERLOC_CENTER%staggerloc  ! default
   endif

   ! NOTE THERE IS NO INPUT VALUE CHECKING HERE BECAUSE IT'S DONE IN 
   ! THE C++ VERSION. 

   ! Call into the C++ interface
   call c_esmc_gridgetcoordr8(grid, localDE, tmp_staggerloc, &  
                              index, coord, localrc)
   if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


    ! Return successfully 
    if (present(rc)) rc = ESMF_SUCCESS 

    end subroutine ESMF_GridGetCoordR8


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridGetItem - Get Grid coordinate bounds and an F90 pointer to coordinate data

! !INTERFACE:
!      subroutine ESMF_GridGetItem(grid, localDE, staggerloc, item,      &
!         exclusiveLBound, exclusiveUBound, exclusiveCount,              &
!         computationalLBound, computationalUBound, computationalCount,  &
!         totalLBound, totalUBound, totalCount,                          &
!         <pointer argument>, doCopy, rc)
! 
! !ARGUMENTS:
!     type(ESMF_Grid),        intent(in) :: grid
!     integer,                intent(in) :: localDE
!     type (ESMF_StaggerLoc), intent(in),  optional :: staggerloc
!     type (ESMF_GridItem),   intent(in)            :: item
!     integer,                intent(out), optional :: exclusiveLBound(:)
!     integer,                intent(out), optional :: exclusiveUBound(:)
!     integer,                intent(out), optional :: exclusiveCount(:)
!     integer,                intent(out), optional :: computationalLBound(:)
!     integer,                intent(out), optional :: computationalUBound(:)
!     integer,                intent(out), optional :: computationalCount(:)
!     integer,                intent(out), optional :: totalLBound(:)
!     integer,                intent(out), optional :: totalUBound(:)
!     integer,                intent(out), optional :: totalCount(:)
!     <pointer argument>, see below for supported values
!     type(ESMF_CopyFlag),    intent(in), optional :: docopy
!     integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!     This method gets a Fortran pointer to the piece of memory which holds the 
!     item data on the local DE for the given stagger locations. 
!     This is useful, for example, for setting the item values in a Grid, or
!     for reading the item values.  Currently this method supports up to three
!     grid dimensions, but is limited to the I4 datatype.  See below for specific 
!     supported values.  If the item values that you are trying to retrieve are of
!     higher dimension, use the {\tt ESMF\_GetItem()} interface that returns coordinate
!     values in an {\tt ESMF\_Array} instead.  That interface supports the retrieval of
!     coordinates up to 7D. 
!
!     Supported values for the <pointer argument> are: 
!     \begin{description}
!     \item integer(ESMF\_KIND\_I4), pointer :: fptr(:)
!     \item integer(ESMF\_KIND\_I4), pointer :: fptr(:,:)     
!     \item integer(ESMF\_KIND\_I4), pointer :: fptr(:,:,:)
!     \item real(ESMF\_KIND\_R4), pointer :: fptr(:)
!     \item real(ESMF\_KIND\_R4), pointer :: fptr(:,:)     
!     \item real(ESMF\_KIND\_R4), pointer :: fptr(:,:,:)
!     \item real(ESMF\_KIND\_R8), pointer :: fptr(:)
!     \item real(ESMF\_KIND\_R8), pointer :: fptr(:,:)     
!     \item real(ESMF\_KIND\_R8), pointer :: fptr(:,:,:)
!     \end{description}
!
!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          Grid to get the information from.
!     \item[{localDE}]
!          The local DE to get the information for. {\tt [0,..,localDeCount-1]}
!     \item[{staggerloc}]
!          The stagger location to get the information for. 
!          Please see Section~\ref{sec:opt:staggerloc} for a list 
!          of predefined stagger locations. If not present, defaults to ESMF\_STAGGERLOC\_CENTER.
!     \item[{item}]
!          The item to get the information for. Please see Section~\ref{sec:opt:griditem} for a 
!          list of valid items.  
!     \item[{[exclusiveLBound]}]
!          Upon return this holds the lower bounds of the exclusive region.
!          {\tt exclusiveLBound} must be allocated to be of size equal to the grid dimCount.
!     \item[{[exclusiveUBound]}]
!          Upon return this holds the upper bounds of the exclusive region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the grid dimCount.
!     \item[{[exclusiveCount]}]
!          Upon return this holds the number of items in the exclusive region per dimension
!          (i.e. {\tt exclusiveUBound-exclusiveLBound+1}). {\tt exclusiveCount} must
!          be allocated to be of size equal to the grid dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalLBound]}]
!          Upon return this holds the lower bounds of the stagger region.
!          {\tt computationalLBound} must be allocated to be of size equal to the grid dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalUBound]}]
!          Upon return this holds the upper bounds of the stagger region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the grid dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalCount]}]
!          Upon return this holds the number of items in the computational region per dimension
!          (i.e. {\tt computationalUBound-computationalLBound+1}). {\tt computationalCount}
!          must be allocated to be of size equal to the grid dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalLBound]}]
!          Upon return this holds the lower bounds of the total region.
!          {\tt totalLBound} must be allocated to be of size equal to the grid dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalUBound]}]
!          Upon return this holds the upper bounds of the total region.
!          {\tt totalUBound} must be allocated to be of size equal to the grid dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalCount]}]
!          Upon return this holds the number of items in the total region per dimension
!          (i.e. {\tt totalUBound-totalLBound+1}). {\tt totalCount} must
!          be allocated to be of size equal to the grid dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{fptr}]
!          The pointer to the item data.
!     \item[{[doCopy]}]
!          If not specified, default to {\tt ESMF\_DATA\_REF}, in this case
!          fptr is a reference to the data in the Grid item arrays. 
!          Please see Section~\ref{opt:copyflag} for further description and a
!          list of valid values. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetItem1DI4"
!BOPI
! !IROUTINE: ESMF_GridGetItem - Get pointer to 1DI4 coordinates

! !INTERFACE:
  ! Private name; call using ESMF_GridGetItem()
      subroutine ESMF_GridGetItem1DI4(grid, localDE, staggerloc, item,      & 
          exclusiveLBound, exclusiveUBound, exclusiveCount,                 &
          computationalLBound, computationalUBound, computationalCount,     &
          totalLBound, totalUBound, totalCount,                             &
          fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid),        intent(in) :: grid
      integer,                intent(in) :: localDE
      type (ESMF_StaggerLoc), intent(in),  optional :: staggerloc
      type (ESMF_GridItem),   intent(in)            :: item
      integer,        target, intent(out), optional :: exclusiveLBound(:)
      integer,        target, intent(out), optional :: exclusiveUBound(:)
      integer,        target, intent(out), optional :: exclusiveCount(:)
      integer,        target, intent(out), optional :: computationalLBound(:)
      integer,        target, intent(out), optional :: computationalUBound(:)
      integer,        target, intent(out), optional :: computationalCount(:)
      integer,        target, intent(out), optional :: totalLBound(:)
      integer,        target, intent(out), optional :: totalUBound(:)
      integer,        target, intent(out), optional :: totalCount(:)
      integer(ESMF_KIND_I4), pointer                   :: fptr(:)
      type(ESMF_CopyFlag),    intent(in), optional :: docopy
      integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!    This method gets a Fortran pointer to the piece of memory which holds the 
!    item data for the stagger locations on the given local DE. 
!    This is useful, for example, for setting the item values in a Grid, or
!    for reading the item values.
!
!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          Grid to get the information from.
!     \item[{localDE}]
!          The local DE to get the information for. {\tt [0,..,localDeCount-1]}
!     \item[{staggerloc}]
!          The stagger location to get the information for. 
!          Please see Section~\ref{sec:opt:staggerloc} for a list 
!          of predefined stagger locations. If not present, defaults to ESMF\_STAGGERLOC\_CENTER.
!     \item[{item}]
!          The item to get the information for. Please see Section~\ref{sec:opt:griditem} for a 
!          list of valid items.  
!     \item[{[exclusiveLBound]}]
!          Upon return this holds the lower bounds of the exclusive region.
!          {\tt exclusiveLBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[exclusiveUBound]}]
!          Upon return this holds the upper bounds of the exclusive region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[exclusiveCount]}]
!          Upon return this holds the number of items in the exclusive region per dimension
!          (i.e. {\tt exclusiveUBound-exclusiveLBound+1}). {\tt exclusiveCount} must
!          be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalLBound]}]
!          Upon return this holds the lower bounds of the stagger region.
!          {\tt computationalLBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalUBound]}]
!          Upon return this holds the upper bounds of the stagger region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalCount]}]
!          Upon return this holds the number of items in the computational region per dimension
!          (i.e. {\tt computationalUBound-computationalLBound+1}). {\tt computationalCount}
!          must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalLBound]}]
!          Upon return this holds the lower bounds of the total region.
!          {\tt totalLBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalUBound]}]
!          Upon return this holds the upper bounds of the total region.
!          {\tt totalUBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalCount]}]
!          Upon return this holds the number of items in the total region per dimension
!          (i.e. {\tt totalUBound-totalLBound+1}). {\tt totalCount} must
!          be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{fptr}]
!          The pointer to the item data.
!     \item[{[doCopy]}]
!          If not specified, default to {\tt ESMF\_DATA\_REF}, in this case
!          fptr is a reference to the data in the Grid item arrays. 
!          Please see Section~\ref{opt:copyflag} for further description and a
!          list of valid values. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

    ! Local variables 
    type(ESMF_Array) :: array 
    integer :: localrc ! local error status 
    integer :: localDeCount, dimCount 
    type(ESMF_TypeKind) :: typekind 
    type(ESMF_LocalArray), allocatable :: larrayList(:) 
    type(ESMF_CopyFlag) :: docopyInt
    integer :: coordDimCount(ESMF_MAXDIM)
    type(ESMF_InterfaceInt) :: exclusiveLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: exclusiveUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: exclusiveCountArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalCountArg ! helper variable
    type(ESMF_InterfaceInt) :: totalLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: totalUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: totalCountArg ! helper variable
    integer :: tmp_staggerloc

    ! Initialize return code 
    localrc = ESMF_RC_NOT_IMPL 
    if (present(rc)) rc = ESMF_RC_NOT_IMPL 

    ! Check init status of arguments 
    ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid, rc) 

    ! Check consistency 
    call ESMF_GridGet(grid, dimCount=dimCount, &
                      localDECount=localDECount, rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! Require farrayPtr dimCount to match grid dimCount 
    if (dimCount .ne. 1) then 
      call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
        "- farrayPtr dimCount does not match requested item dimCount", & 
        ESMF_CONTEXT, rc) 
      return 
    endif 

    ! Set Defaults
    if (present(docopy)) then
      docopyInt=docopy
    else
      docopyInt=ESMF_DATA_REF
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
 
    if (localDE>=localDeCount) then 
      call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
        "- localDE too big", ESMF_CONTEXT, rc) 
      return 
    endif 

    if (localDE<0) then 
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
    call ESMF_GridGetItemIntoArray(grid, staggerloc, item, array, &
                                    ESMF_DATA_REF, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return

    ! Obtain the native F90 array pointer via the LocalArray interface 
    allocate(larrayList(localDeCount))
 
    call ESMF_ArrayGet(array, larrayList=larrayList, rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    call ESMF_LocalArrayGet(larrayList(localDE+1), fptr, doCopy, rc=localrc) 
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
    exclusiveCountArg=ESMF_InterfaceIntCreate(exclusiveCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalLBoundArg=ESMF_InterfaceIntCreate(computationalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalUBoundArg=ESMF_InterfaceIntCreate(computationalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalCountArg=ESMF_InterfaceIntCreate(computationalCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalLBoundArg=ESMF_InterfaceIntCreate(totalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalUBoundArg = ESMF_InterfaceIntCreate(totalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalCountArg = ESMF_InterfaceIntCreate(totalCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_GridGetItemBounds(grid, localDE, tmp_staggerloc, item, &
      exclusiveLBoundArg, exclusiveUBoundArg, exclusiveCountArg, &
      computationalLBoundArg, computationalUBoundArg, computationalCountArg,&
      totalLBoundArg, totalUBoundArg, totalCountArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deallocate interface ints
    call ESMF_InterfaceIntDestroy(exclusiveLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully 
    if (present(rc)) rc = ESMF_SUCCESS 

    end subroutine ESMF_GridGetItem1DI4


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetItem2DI4"
!BOPI
! !IROUTINE: ESMF_GridGetItem - Get pointer to 2DI4 item

! !INTERFACE:
  ! Private name; call using ESMF_GridGetItem()
      subroutine ESMF_GridGetItem2DI4(grid, localDE, staggerloc, item, &
          exclusiveLBound, exclusiveUBound, exclusiveCount,                 &
          computationalLBound, computationalUBound, computationalCount,     &
          totalLBound, totalUBound, totalCount,                             &
          fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in) :: localDE
      type (ESMF_StaggerLoc), intent(in), optional  :: staggerloc
      type (ESMF_GridItem),   intent(in)            :: item
      integer,        target, intent(out), optional :: exclusiveLBound(:)
      integer,        target, intent(out), optional :: exclusiveUBound(:)
      integer,        target, intent(out), optional :: exclusiveCount(:)
      integer,        target, intent(out), optional :: computationalLBound(:)
      integer,        target, intent(out), optional :: computationalUBound(:)
      integer,        target, intent(out), optional :: computationalCount(:)
      integer,        target, intent(out), optional :: totalLBound(:)
      integer,        target, intent(out), optional :: totalUBound(:)
      integer,        target, intent(out), optional :: totalCount(:)
      integer(ESMF_KIND_I4), pointer :: fptr(:,:)
      type(ESMF_CopyFlag), intent(in), optional :: docopy
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    This method gets a Fortran pointer to the piece of memory which holds the 
!    item data for the stagger locations on the given local DE. 
!    This is useful, for example, for setting the item values in a Grid, or
!    for reading the item values.
!
!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          Grid to get the information from.
!     \item[{localDE}]
!          The local DE to get the information for. {\tt [0,..,localDeCount-1]}
!     \item[{staggerloc}]
!          The stagger location to get the information for. 
!          Please see Section~\ref{sec:opt:staggerloc} for a list 
!          of predefined stagger locations. If not present, defaults to
!          ESMF\_STAGGERLOC\_CENTER.
!     \item[{item}]
!          The item to get the information for. Please see Section~\ref{sec:opt:griditem} for a 
!          list of valid items.  
!     \item[{[exclusiveLBound]}]
!          Upon return this holds the lower bounds of the exclusive region.
!          {\tt exclusiveLBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[exclusiveUBound]}]
!          Upon return this holds the upper bounds of the exclusive region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[exclusiveCount]}]
!          Upon return this holds the number of items in the exclusive region per dimension
!          (i.e. {\tt exclusiveUBound-exclusiveLBound+1}). {\tt exclusiveCount} must
!          be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalLBound]}]
!          Upon return this holds the lower bounds of the stagger region.
!          {\tt computationalLBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalUBound]}]
!          Upon return this holds the upper bounds of the stagger region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalCount]}]
!          Upon return this holds the number of items in the computational region per dimension
!          (i.e. {\tt computationalUBound-computationalLBound+1}). {\tt computationalCount}
!          must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalLBound]}]
!          Upon return this holds the lower bounds of the total region.
!          {\tt totalLBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalUBound]}]
!          Upon return this holds the upper bounds of the total region.
!          {\tt totalUBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalCount]}]
!          Upon return this holds the number of items in the total region per dimension
!          (i.e. {\tt totalUBound-totalLBound+1}). {\tt totalCount} must
!          be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{fptr}]
!          The pointer to the item data.
!     \item[{[doCopy]}]
!          If not specified, default to {\tt ESMF\_DATA\_REF}, in this case
!          fptr is a reference to the data in the Grid item arrays. 
!          Please see Section~\ref{opt:copyflag} for further description and a
!          list of valid values. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI


    ! Local variables 
    type(ESMF_Array) :: array 
    integer :: localrc ! local error status 
    integer :: localDeCount, dimCount 
    type(ESMF_TypeKind) :: typekind 
    type(ESMF_LocalArray), allocatable :: larrayList(:) 
    type(ESMF_CopyFlag) :: docopyInt
    integer :: coordDimCount(ESMF_MAXDIM)
    type(ESMF_InterfaceInt) :: exclusiveLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: exclusiveUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: exclusiveCountArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalCountArg ! helper variable
    type(ESMF_InterfaceInt) :: totalLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: totalUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: totalCountArg ! helper variable
    integer :: tmp_staggerloc

    ! Initialize return code 
    localrc = ESMF_RC_NOT_IMPL 
    if (present(rc)) rc = ESMF_RC_NOT_IMPL 

    ! Check init status of arguments 
    ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid, rc) 

    ! Check consistency 
    call ESMF_GridGet(grid, dimCount=dimCount, &
                      localDECount=localDECount, rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
      ESMF_CONTEXT, rcToReturn=rc)) return
 

    ! Require farrayPtr dimCount to match grid dimCount 
    if (dimCount .ne. 2) then 
    call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
      "- farrayPtr dimCount does not match requested item dimCount", & 
      ESMF_CONTEXT, rc) 
    return 
    endif 

    ! Set Defaults
    if (present(docopy)) then
      docopyInt=docopy
    else
      docopyInt=ESMF_DATA_REF
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
 
    if (localDE>=localDeCount) then 
      call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
        "- localDE too big", ESMF_CONTEXT, rc) 
      return 
    endif 

    if (localDE<0) then 
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
    call ESMF_GridGetItemIntoArray(grid, staggerloc, item, array, &
                                    ESMF_DATA_REF, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return

    ! Obtain the native F90 array pointer via the LocalArray interface 
    allocate(larrayList(localDeCount))
 
    call ESMF_ArrayGet(array, larrayList=larrayList, rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return
 
    call ESMF_LocalArrayGet(larrayList(localDE+1), fptr, doCopy, rc=localrc) 
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
    exclusiveCountArg=ESMF_InterfaceIntCreate(exclusiveCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalLBoundArg=ESMF_InterfaceIntCreate(computationalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalUBoundArg=ESMF_InterfaceIntCreate(computationalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalCountArg=ESMF_InterfaceIntCreate(computationalCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalLBoundArg=ESMF_InterfaceIntCreate(totalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalUBoundArg = ESMF_InterfaceIntCreate(totalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalCountArg = ESMF_InterfaceIntCreate(totalCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_GridGetItemBounds(grid, localDE, tmp_staggerloc, item, &
      exclusiveLBoundArg, exclusiveUBoundArg, exclusiveCountArg, &
      computationalLBoundArg, computationalUBoundArg, computationalCountArg,&
      totalLBoundArg, totalUBoundArg, totalCountArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deallocate interface ints
    call ESMF_InterfaceIntDestroy(exclusiveLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully 
    if (present(rc)) rc = ESMF_SUCCESS 

    end subroutine ESMF_GridGetItem2DI4


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetItem3DI4"
!BOPI
! !IROUTINE: ESMF_GridGetItem - Get pointer to 3DI4 item

! !INTERFACE:
  ! Private name; call using ESMF_GridGetItem()
      subroutine ESMF_GridGetItem3DI4(grid, localDE, staggerloc, item, &
          exclusiveLBound, exclusiveUBound, exclusiveCount,                 &
          computationalLBound, computationalUBound, computationalCount,     &
          totalLBound, totalUBound, totalCount,                             &      
          fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in) :: localDE
      type (ESMF_StaggerLoc), intent(in),  optional :: staggerloc
      type (ESMF_GridItem),   intent(in)            :: item
      integer,        target, intent(out), optional :: exclusiveLBound(:)
      integer,        target, intent(out), optional :: exclusiveUBound(:)
      integer,        target, intent(out), optional :: exclusiveCount(:)
      integer,        target, intent(out), optional :: computationalLBound(:)
      integer,        target, intent(out), optional :: computationalUBound(:)
      integer,        target, intent(out), optional :: computationalCount(:)
      integer,        target, intent(out), optional :: totalLBound(:)
      integer,        target, intent(out), optional :: totalUBound(:)
      integer,        target, intent(out), optional :: totalCount(:)
      integer(ESMF_KIND_I4), pointer :: fptr(:,:,:)
      type(ESMF_CopyFlag), intent(in), optional :: docopy
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    This method gets a Fortran pointer to the piece of memory which holds the 
!    maks data and stagger locations on the given local DE. 
!    This is useful, for example, for setting the item values in a Grid, or
!    for reading the item values. 
!
!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          Grid to get the information from.
!     \item[{localDE}]
!          The local DE to get the information for. {\tt [0,..,localDeCount-1]}
!     \item[{staggerloc}]
!          The stagger location to get the information for. 
!          Please see Section~\ref{sec:opt:staggerloc} for a list 
!          of predefined stagger locations. If not present, defaults to
!          ESMF\_STAGGERLOC\_CENTER.
!     \item[{item}]
!          The item to get the information for. Please see Section~\ref{sec:opt:griditem} for a 
!          list of valid items.  
!     \item[{[exclusiveLBound]}]
!          Upon return this holds the lower bounds of the exclusive region.
!          {\tt exclusiveLBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[exclusiveUBound]}]
!          Upon return this holds the upper bounds of the exclusive region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[exclusiveCount]}]
!          Upon return this holds the number of items in the exclusive region per dimension
!          (i.e. {\tt exclusiveUBound-exclusiveLBound+1}). {\tt exclusiveCount} must
!          be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalLBound]}]
!          Upon return this holds the lower bounds of the stagger region.
!          {\tt computationalLBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalUBound]}]
!          Upon return this holds the upper bounds of the stagger region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalCount]}]
!          Upon return this holds the number of items in the computational region per dimension
!          (i.e. {\tt computationalUBound-computationalLBound+1}). {\tt computationalCount}
!          must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalLBound]}]
!          Upon return this holds the lower bounds of the total region.
!          {\tt totalLBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalUBound]}]
!          Upon return this holds the upper bounds of the total region.
!          {\tt totalUBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalCount]}]
!          Upon return this holds the number of items in the total region per dimension
!          (i.e. {\tt totalUBound-totalLBound+1}). {\tt totalCount} must
!          be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{fptr}]
!          The pointer to the item data.
!     \item[{[doCopy]}]
!          If not specified, default to {\tt ESMF\_DATA\_REF}, in this case
!          fptr is a reference to the data in the Grid item arrays. 
!          Please see Section~\ref{opt:copyflag} for further description and a
!          list of valid values. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI


 ! Local variables 
 type(ESMF_Array) :: array 
 integer :: localrc ! local error status 
 integer :: localDeCount, dimCount 
 type(ESMF_TypeKind) :: typekind 
 type(ESMF_LocalArray), allocatable :: larrayList(:) 
 type(ESMF_CopyFlag) :: docopyInt
 integer :: coordDimCount(ESMF_MAXDIM)
 type(ESMF_InterfaceInt) :: exclusiveLBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: exclusiveUBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: exclusiveCountArg ! helper variable
 type(ESMF_InterfaceInt) :: computationalLBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: computationalUBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: computationalCountArg ! helper variable
 type(ESMF_InterfaceInt) :: totalLBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: totalUBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: totalCountArg ! helper variable
 integer :: tmp_staggerloc

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid, rc) 

 ! Check consistency 
 call ESMF_GridGet(grid, dimCount=dimCount, &
                   localDECount=localDECount, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return
 
 ! Require farrayPtr dimCount to match coordinate dimCount 
 if (dimCount .ne. 3) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- farrayPtr dimCount does not match requested item dimCount", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

! Set Defaults
if (present(docopy)) then
   docopyInt=docopy
else
  docopyInt=ESMF_DATA_REF
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
 
 if (localDE>=localDeCount) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- localDE too big", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

 if (localDE<0) then 
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

    call ESMF_GridGetItemIntoArray(grid, staggerloc, item, array, &
                                    ESMF_DATA_REF, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return

    ! Obtain the native F90 array pointer via the LocalArray interface 
    allocate(larrayList(localDeCount))
 
    call ESMF_ArrayGet(array, larrayList=larrayList, rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return
 
    call ESMF_LocalArrayGet(larrayList(localDE+1), fptr, doCopy, rc=localrc) 
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
    exclusiveCountArg=ESMF_InterfaceIntCreate(exclusiveCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalLBoundArg=ESMF_InterfaceIntCreate(computationalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalUBoundArg=ESMF_InterfaceIntCreate(computationalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalCountArg=ESMF_InterfaceIntCreate(computationalCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalLBoundArg=ESMF_InterfaceIntCreate(totalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalUBoundArg = ESMF_InterfaceIntCreate(totalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalCountArg = ESMF_InterfaceIntCreate(totalCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_GridGetItemBounds(grid, localDE, tmp_staggerloc, item, &
      exclusiveLBoundArg, exclusiveUBoundArg, exclusiveCountArg, &
      computationalLBoundArg, computationalUBoundArg, computationalCountArg,&
      totalLBoundArg, totalUBoundArg, totalCountArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deallocate interface ints
    call ESMF_InterfaceIntDestroy(exclusiveLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully 
    if (present(rc)) rc = ESMF_SUCCESS 

    end subroutine ESMF_GridGetItem3DI4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetItem1DR4"
!BOPI
! !IROUTINE: ESMF_GridGetItem - Get pointer to 1DR4 coordinates

! !INTERFACE:
  ! Private name; call using ESMF_GridGetItem()
      subroutine ESMF_GridGetItem1DR4(grid, localDE, staggerloc, item,      & 
          exclusiveLBound, exclusiveUBound, exclusiveCount,                 &
          computationalLBound, computationalUBound, computationalCount,     &
          totalLBound, totalUBound, totalCount,                             &
          fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid),        intent(in) :: grid
      integer,                intent(in) :: localDE
      type (ESMF_StaggerLoc), intent(in),  optional :: staggerloc
      type (ESMF_GridItem),   intent(in)            :: item
      integer,        target, intent(out), optional :: exclusiveLBound(:)
      integer,        target, intent(out), optional :: exclusiveUBound(:)
      integer,        target, intent(out), optional :: exclusiveCount(:)
      integer,        target, intent(out), optional :: computationalLBound(:)
      integer,        target, intent(out), optional :: computationalUBound(:)
      integer,        target, intent(out), optional :: computationalCount(:)
      integer,        target, intent(out), optional :: totalLBound(:)
      integer,        target, intent(out), optional :: totalUBound(:)
      integer,        target, intent(out), optional :: totalCount(:)
      real(ESMF_KIND_R4), pointer                   :: fptr(:)
      type(ESMF_CopyFlag),    intent(in), optional :: docopy
      integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!    This method gets a Fortran pointer to the piece of memory which holds the 
!    item data for the stagger locations on the given local DE. 
!    This is useful, for example, for setting the item values in a Grid, or
!    for reading the item values.
!
!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          Grid to get the information from.
!     \item[{localDE}]
!          The local DE to get the information for. {\tt [0,..,localDeCount-1]}
!     \item[{staggerloc}]
!          The stagger location to get the information for. 
!          Please see Section~\ref{sec:opt:staggerloc} for a list 
!          of predefined stagger locations. If not present, defaults to ESMF\_STAGGERLOC\_CENTER.
!     \item[{item}]
!          The item to get the information for. Please see Section~\ref{sec:opt:griditem} for a 
!          list of valid items.   
!     \item[{[exclusiveLBound]}]
!          Upon return this holds the lower bounds of the exclusive region.
!          {\tt exclusiveLBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[exclusiveUBound]}]
!          Upon return this holds the upper bounds of the exclusive region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[exclusiveCount]}]
!          Upon return this holds the number of items in the exclusive region per dimension
!          (i.e. {\tt exclusiveUBound-exclusiveLBound+1}). {\tt exclusiveCount} must
!          be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalLBound]}]
!          Upon return this holds the lower bounds of the stagger region.
!          {\tt computationalLBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalUBound]}]
!          Upon return this holds the upper bounds of the stagger region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalCount]}]
!          Upon return this holds the number of items in the computational region per dimension
!          (i.e. {\tt computationalUBound-computationalLBound+1}). {\tt computationalCount}
!          must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalLBound]}]
!          Upon return this holds the lower bounds of the total region.
!          {\tt totalLBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalUBound]}]
!          Upon return this holds the upper bounds of the total region.
!          {\tt totalUBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalCount]}]
!          Upon return this holds the number of items in the total region per dimension
!          (i.e. {\tt totalUBound-totalLBound+1}). {\tt totalCount} must
!          be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{fptr}]
!          The pointer to the item data.
!     \item[{[doCopy]}]
!          If not specified, default to {\tt ESMF\_DATA\_REF}, in this case
!          fptr is a reference to the data in the Grid item arrays. 
!          Please see Section~\ref{opt:copyflag} for further description and a
!          list of valid values. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

    ! Local variables 
    type(ESMF_Array) :: array 
    integer :: localrc ! local error status 
    integer :: localDeCount, dimCount 
    type(ESMF_TypeKind) :: typekind 
    type(ESMF_LocalArray), allocatable :: larrayList(:) 
    type(ESMF_CopyFlag) :: docopyInt
    integer :: coordDimCount(ESMF_MAXDIM)
    type(ESMF_InterfaceInt) :: exclusiveLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: exclusiveUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: exclusiveCountArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalCountArg ! helper variable
    type(ESMF_InterfaceInt) :: totalLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: totalUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: totalCountArg ! helper variable
    integer :: tmp_staggerloc

    ! Initialize return code 
    localrc = ESMF_RC_NOT_IMPL 
    if (present(rc)) rc = ESMF_RC_NOT_IMPL 

    ! Check init status of arguments 
    ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid, rc) 

    ! Check consistency 
    call ESMF_GridGet(grid, dimCount=dimCount, &
                      localDECount=localDECount, rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! Require farrayPtr dimCount to match grid dimCount 
    if (dimCount .ne. 1) then 
      call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
        "- farrayPtr dimCount does not match requested item dimCount", & 
        ESMF_CONTEXT, rc) 
      return 
    endif 

    ! Set Defaults
    if (present(docopy)) then
      docopyInt=docopy
    else
      docopyInt=ESMF_DATA_REF
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
 
    if (localDE>=localDeCount) then 
      call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
        "- localDE too big", ESMF_CONTEXT, rc) 
      return 
    endif 

    if (localDE<0) then 
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
    call ESMF_GridGetItemIntoArray(grid, staggerloc, item, array, &
                                    ESMF_DATA_REF, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return

    ! Obtain the native F90 array pointer via the LocalArray interface 
    allocate(larrayList(localDeCount))
 
    call ESMF_ArrayGet(array, larrayList=larrayList, rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    call ESMF_LocalArrayGet(larrayList(localDE+1), fptr, doCopy, rc=localrc) 
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
    exclusiveCountArg=ESMF_InterfaceIntCreate(exclusiveCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalLBoundArg=ESMF_InterfaceIntCreate(computationalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalUBoundArg=ESMF_InterfaceIntCreate(computationalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalCountArg=ESMF_InterfaceIntCreate(computationalCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalLBoundArg=ESMF_InterfaceIntCreate(totalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalUBoundArg = ESMF_InterfaceIntCreate(totalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalCountArg = ESMF_InterfaceIntCreate(totalCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_GridGetItemBounds(grid, localDE, tmp_staggerloc, item, &
      exclusiveLBoundArg, exclusiveUBoundArg, exclusiveCountArg, &
      computationalLBoundArg, computationalUBoundArg, computationalCountArg,&
      totalLBoundArg, totalUBoundArg, totalCountArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deallocate interface ints
    call ESMF_InterfaceIntDestroy(exclusiveLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully 
    if (present(rc)) rc = ESMF_SUCCESS 

    end subroutine ESMF_GridGetItem1DR4


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetItem2DR4"
!BOPI
! !IROUTINE: ESMF_GridGetItem - Get pointer to 2DR4 item

! !INTERFACE:
  ! Private name; call using ESMF_GridGetItem()
      subroutine ESMF_GridGetItem2DR4(grid, localDE, staggerloc, item, &
          exclusiveLBound, exclusiveUBound, exclusiveCount,                 &
          computationalLBound, computationalUBound, computationalCount,     &
          totalLBound, totalUBound, totalCount,                             &
          fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in) :: localDE
      type (ESMF_StaggerLoc), intent(in), optional  :: staggerloc
      type (ESMF_GridItem),   intent(in)            :: item
      integer,        target, intent(out), optional :: exclusiveLBound(:)
      integer,        target, intent(out), optional :: exclusiveUBound(:)
      integer,        target, intent(out), optional :: exclusiveCount(:)
      integer,        target, intent(out), optional :: computationalLBound(:)
      integer,        target, intent(out), optional :: computationalUBound(:)
      integer,        target, intent(out), optional :: computationalCount(:)
      integer,        target, intent(out), optional :: totalLBound(:)
      integer,        target, intent(out), optional :: totalUBound(:)
      integer,        target, intent(out), optional :: totalCount(:)
      real(ESMF_KIND_R4), pointer :: fptr(:,:)
      type(ESMF_CopyFlag), intent(in), optional :: docopy
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    This method gets a Fortran pointer to the piece of memory which holds the 
!    item data for the stagger locations on the given local DE. 
!    This is useful, for example, for setting the item values in a Grid, or
!    for reading the item values.
!
!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          Grid to get the information from.
!     \item[{localDE}]
!          The local DE to get the information for. {\tt [0,..,localDeCount-1]}
!     \item[{staggerloc}]
!          The stagger location to get the information for. 
!          Please see Section~\ref{sec:opt:staggerloc} for a list 
!          of predefined stagger locations. If not present, defaults to
!          ESMF\_STAGGERLOC\_CENTER.
!     \item[{item}]
!          The item to get the information for. Please see Section~\ref{sec:opt:griditem} for a 
!          list of valid items.   
!     \item[{[exclusiveLBound]}]
!          Upon return this holds the lower bounds of the exclusive region.
!          {\tt exclusiveLBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[exclusiveUBound]}]
!          Upon return this holds the upper bounds of the exclusive region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[exclusiveCount]}]
!          Upon return this holds the number of items in the exclusive region per dimension
!          (i.e. {\tt exclusiveUBound-exclusiveLBound+1}). {\tt exclusiveCount} must
!          be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalLBound]}]
!          Upon return this holds the lower bounds of the stagger region.
!          {\tt computationalLBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalUBound]}]
!          Upon return this holds the upper bounds of the stagger region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalCount]}]
!          Upon return this holds the number of items in the computational region per dimension
!          (i.e. {\tt computationalUBound-computationalLBound+1}). {\tt computationalCount}
!          must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalLBound]}]
!          Upon return this holds the lower bounds of the total region.
!          {\tt totalLBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalUBound]}]
!          Upon return this holds the upper bounds of the total region.
!          {\tt totalUBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalCount]}]
!          Upon return this holds the number of items in the total region per dimension
!          (i.e. {\tt totalUBound-totalLBound+1}). {\tt totalCount} must
!          be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{fptr}]
!          The pointer to the item data.
!     \item[{[doCopy]}]
!          If not specified, default to {\tt ESMF\_DATA\_REF}, in this case
!          fptr is a reference to the data in the Grid item arrays. 
!          Please see Section~\ref{opt:copyflag} for further description and a
!          list of valid values. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI


    ! Local variables 
    type(ESMF_Array) :: array 
    integer :: localrc ! local error status 
    integer :: localDeCount, dimCount 
    type(ESMF_TypeKind) :: typekind 
    type(ESMF_LocalArray), allocatable :: larrayList(:) 
    type(ESMF_CopyFlag) :: docopyInt
    integer :: coordDimCount(ESMF_MAXDIM)
    type(ESMF_InterfaceInt) :: exclusiveLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: exclusiveUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: exclusiveCountArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalCountArg ! helper variable
    type(ESMF_InterfaceInt) :: totalLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: totalUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: totalCountArg ! helper variable
    integer :: tmp_staggerloc

    ! Initialize return code 
    localrc = ESMF_RC_NOT_IMPL 
    if (present(rc)) rc = ESMF_RC_NOT_IMPL 

    ! Check init status of arguments 
    ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid, rc) 

    ! Check consistency 
    call ESMF_GridGet(grid, dimCount=dimCount, &
                      localDECount=localDECount, rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
      ESMF_CONTEXT, rcToReturn=rc)) return
 

    ! Require farrayPtr dimCount to match grid dimCount 
    if (dimCount .ne. 2) then 
    call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
      "- farrayPtr dimCount does not match requested item dimCount", & 
      ESMF_CONTEXT, rc) 
    return 
    endif 

    ! Set Defaults
    if (present(docopy)) then
      docopyInt=docopy
    else
      docopyInt=ESMF_DATA_REF
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
 
    if (localDE>=localDeCount) then 
      call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
        "- localDE too big", ESMF_CONTEXT, rc) 
      return 
    endif 

    if (localDE<0) then 
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
    call ESMF_GridGetItemIntoArray(grid, staggerloc, item, array, &
                                    ESMF_DATA_REF, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return

    ! Obtain the native F90 array pointer via the LocalArray interface 
    allocate(larrayList(localDeCount))
 
    call ESMF_ArrayGet(array, larrayList=larrayList, rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return
 
    call ESMF_LocalArrayGet(larrayList(localDE+1), fptr, doCopy, rc=localrc) 
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
    exclusiveCountArg=ESMF_InterfaceIntCreate(exclusiveCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalLBoundArg=ESMF_InterfaceIntCreate(computationalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalUBoundArg=ESMF_InterfaceIntCreate(computationalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalCountArg=ESMF_InterfaceIntCreate(computationalCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalLBoundArg=ESMF_InterfaceIntCreate(totalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalUBoundArg = ESMF_InterfaceIntCreate(totalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalCountArg = ESMF_InterfaceIntCreate(totalCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_GridGetItemBounds(grid, localDE, tmp_staggerloc, item, &
      exclusiveLBoundArg, exclusiveUBoundArg, exclusiveCountArg, &
      computationalLBoundArg, computationalUBoundArg, computationalCountArg,&
      totalLBoundArg, totalUBoundArg, totalCountArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deallocate interface ints
    call ESMF_InterfaceIntDestroy(exclusiveLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully 
    if (present(rc)) rc = ESMF_SUCCESS 

    end subroutine ESMF_GridGetItem2DR4


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetItem3DR4"
!BOPI
! !IROUTINE: ESMF_GridGetItem - Get pointer to 3DR4 item

! !INTERFACE:
  ! Private name; call using ESMF_GridGetItem()
      subroutine ESMF_GridGetItem3DR4(grid, localDE, staggerloc, item, &
          exclusiveLBound, exclusiveUBound, exclusiveCount,                 &
          computationalLBound, computationalUBound, computationalCount,     &
          totalLBound, totalUBound, totalCount,                             &      
          fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in) :: localDE
      type (ESMF_StaggerLoc), intent(in),  optional :: staggerloc
      type (ESMF_GridItem),   intent(in)            :: item
      integer,        target, intent(out), optional :: exclusiveLBound(:)
      integer,        target, intent(out), optional :: exclusiveUBound(:)
      integer,        target, intent(out), optional :: exclusiveCount(:)
      integer,        target, intent(out), optional :: computationalLBound(:)
      integer,        target, intent(out), optional :: computationalUBound(:)
      integer,        target, intent(out), optional :: computationalCount(:)
      integer,        target, intent(out), optional :: totalLBound(:)
      integer,        target, intent(out), optional :: totalUBound(:)
      integer,        target, intent(out), optional :: totalCount(:)
      real(ESMF_KIND_R4), pointer :: fptr(:,:,:)
      type(ESMF_CopyFlag), intent(in), optional :: docopy
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    This method gets a Fortran pointer to the piece of memory which holds the 
!    maks data and stagger locations on the given local DE. 
!    This is useful, for example, for setting the item values in a Grid, or
!    for reading the item values. 
!
!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          Grid to get the information from.
!     \item[{localDE}]
!          The local DE to get the information for. {\tt [0,..,localDeCount-1]}
!     \item[{staggerloc}]
!          The stagger location to get the information for. 
!          Please see Section~\ref{sec:opt:staggerloc} for a list 
!          of predefined stagger locations. If not present, defaults to
!          ESMF\_STAGGERLOC\_CENTER.
!     \item[{item}]
!          The item to get the information for. Please see Section~\ref{sec:opt:griditem} for a 
!          list of valid items.   
!     \item[{[exclusiveLBound]}]
!          Upon return this holds the lower bounds of the exclusive region.
!          {\tt exclusiveLBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[exclusiveUBound]}]
!          Upon return this holds the upper bounds of the exclusive region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[exclusiveCount]}]
!          Upon return this holds the number of items in the exclusive region per dimension
!          (i.e. {\tt exclusiveUBound-exclusiveLBound+1}). {\tt exclusiveCount} must
!          be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalLBound]}]
!          Upon return this holds the lower bounds of the stagger region.
!          {\tt computationalLBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalUBound]}]
!          Upon return this holds the upper bounds of the stagger region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalCount]}]
!          Upon return this holds the number of items in the computational region per dimension
!          (i.e. {\tt computationalUBound-computationalLBound+1}). {\tt computationalCount}
!          must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalLBound]}]
!          Upon return this holds the lower bounds of the total region.
!          {\tt totalLBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalUBound]}]
!          Upon return this holds the upper bounds of the total region.
!          {\tt totalUBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalCount]}]
!          Upon return this holds the number of items in the total region per dimension
!          (i.e. {\tt totalUBound-totalLBound+1}). {\tt totalCount} must
!          be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{fptr}]
!          The pointer to the item data.
!     \item[{[doCopy]}]
!          If not specified, default to {\tt ESMF\_DATA\_REF}, in this case
!          fptr is a reference to the data in the Grid item arrays. 
!          Please see Section~\ref{opt:copyflag} for further description and a
!          list of valid values. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI


 ! Local variables 
 type(ESMF_Array) :: array 
 integer :: localrc ! local error status 
 integer :: localDeCount, dimCount 
 type(ESMF_TypeKind) :: typekind 
 type(ESMF_LocalArray), allocatable :: larrayList(:) 
 type(ESMF_CopyFlag) :: docopyInt
 integer :: coordDimCount(ESMF_MAXDIM)
 type(ESMF_InterfaceInt) :: exclusiveLBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: exclusiveUBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: exclusiveCountArg ! helper variable
 type(ESMF_InterfaceInt) :: computationalLBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: computationalUBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: computationalCountArg ! helper variable
 type(ESMF_InterfaceInt) :: totalLBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: totalUBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: totalCountArg ! helper variable
 integer :: tmp_staggerloc

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid, rc) 

 ! Check consistency 
 call ESMF_GridGet(grid, dimCount=dimCount, &
                   localDECount=localDECount, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return
 
 ! Require farrayPtr dimCount to match coordinate dimCount 
 if (dimCount .ne. 3) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- farrayPtr dimCount does not match requested item dimCount", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

! Set Defaults
if (present(docopy)) then
   docopyInt=docopy
else
  docopyInt=ESMF_DATA_REF
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
 
 if (localDE>=localDeCount) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- localDE too big", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

 if (localDE<0) then 
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

    call ESMF_GridGetItemIntoArray(grid, staggerloc, item, array, &
                                    ESMF_DATA_REF, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return

    ! Obtain the native F90 array pointer via the LocalArray interface 
    allocate(larrayList(localDeCount))
 
    call ESMF_ArrayGet(array, larrayList=larrayList, rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return
 
    call ESMF_LocalArrayGet(larrayList(localDE+1), fptr, doCopy, rc=localrc) 
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
    exclusiveCountArg=ESMF_InterfaceIntCreate(exclusiveCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalLBoundArg=ESMF_InterfaceIntCreate(computationalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalUBoundArg=ESMF_InterfaceIntCreate(computationalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalCountArg=ESMF_InterfaceIntCreate(computationalCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalLBoundArg=ESMF_InterfaceIntCreate(totalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalUBoundArg = ESMF_InterfaceIntCreate(totalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalCountArg = ESMF_InterfaceIntCreate(totalCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_GridGetItemBounds(grid, localDE, tmp_staggerloc, item, &
      exclusiveLBoundArg, exclusiveUBoundArg, exclusiveCountArg, &
      computationalLBoundArg, computationalUBoundArg, computationalCountArg,&
      totalLBoundArg, totalUBoundArg, totalCountArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deallocate interface ints
    call ESMF_InterfaceIntDestroy(exclusiveLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully 
    if (present(rc)) rc = ESMF_SUCCESS 

    end subroutine ESMF_GridGetItem3DR4


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetItem1DR8"
!BOPI
! !IROUTINE: ESMF_GridGetItem - Get pointer to 1DR8 coordinates

! !INTERFACE:
  ! Private name; call using ESMF_GridGetItem()
      subroutine ESMF_GridGetItem1DR8(grid, localDE, staggerloc, item,      & 
          exclusiveLBound, exclusiveUBound, exclusiveCount,                 &
          computationalLBound, computationalUBound, computationalCount,     &
          totalLBound, totalUBound, totalCount,                             &
          fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid),        intent(in) :: grid
      integer,                intent(in) :: localDE
      type (ESMF_StaggerLoc), intent(in),  optional :: staggerloc
      type (ESMF_GridItem),   intent(in)            :: item
      integer,        target, intent(out), optional :: exclusiveLBound(:)
      integer,        target, intent(out), optional :: exclusiveUBound(:)
      integer,        target, intent(out), optional :: exclusiveCount(:)
      integer,        target, intent(out), optional :: computationalLBound(:)
      integer,        target, intent(out), optional :: computationalUBound(:)
      integer,        target, intent(out), optional :: computationalCount(:)
      integer,        target, intent(out), optional :: totalLBound(:)
      integer,        target, intent(out), optional :: totalUBound(:)
      integer,        target, intent(out), optional :: totalCount(:)
      real(ESMF_KIND_R8), pointer                   :: fptr(:)
      type(ESMF_CopyFlag),    intent(in), optional :: docopy
      integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!    This method gets a Fortran pointer to the piece of memory which holds the 
!    item data for the stagger locations on the given local DE. 
!    This is useful, for example, for setting the item values in a Grid, or
!    for reading the item values.
!
!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          Grid to get the information from.
!     \item[{localDE}]
!          The local DE to get the information for. {\tt [0,..,localDeCount-1]}
!     \item[{staggerloc}]
!          The stagger location to get the information for. 
!          Please see Section~\ref{sec:opt:staggerloc} for a list 
!          of predefined stagger locations. If not present, defaults to ESMF\_STAGGERLOC\_CENTER.
!     \item[{item}]
!          The item to get the information for. Please see Section~\ref{sec:opt:griditem} for a 
!          list of valid items.   
!     \item[{[exclusiveLBound]}]
!          Upon return this holds the lower bounds of the exclusive region.
!          {\tt exclusiveLBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[exclusiveUBound]}]
!          Upon return this holds the upper bounds of the exclusive region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[exclusiveCount]}]
!          Upon return this holds the number of items in the exclusive region per dimension
!          (i.e. {\tt exclusiveUBound-exclusiveLBound+1}). {\tt exclusiveCount} must
!          be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalLBound]}]
!          Upon return this holds the lower bounds of the stagger region.
!          {\tt computationalLBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalUBound]}]
!          Upon return this holds the upper bounds of the stagger region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalCount]}]
!          Upon return this holds the number of items in the computational region per dimension
!          (i.e. {\tt computationalUBound-computationalLBound+1}). {\tt computationalCount}
!          must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalLBound]}]
!          Upon return this holds the lower bounds of the total region.
!          {\tt totalLBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalUBound]}]
!          Upon return this holds the upper bounds of the total region.
!          {\tt totalUBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalCount]}]
!          Upon return this holds the number of items in the total region per dimension
!          (i.e. {\tt totalUBound-totalLBound+1}). {\tt totalCount} must
!          be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{fptr}]
!          The pointer to the item data.
!     \item[{[doCopy]}]
!          If not specified, default to {\tt ESMF\_DATA\_REF}, in this case
!          fptr is a reference to the data in the Grid item arrays. 
!          Please see Section~\ref{opt:copyflag} for further description and a
!          list of valid values. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

    ! Local variables 
    type(ESMF_Array) :: array 
    integer :: localrc ! local error status 
    integer :: localDeCount, dimCount 
    type(ESMF_TypeKind) :: typekind 
    type(ESMF_LocalArray), allocatable :: larrayList(:) 
    type(ESMF_CopyFlag) :: docopyInt
    integer :: coordDimCount(ESMF_MAXDIM)
    type(ESMF_InterfaceInt) :: exclusiveLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: exclusiveUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: exclusiveCountArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalCountArg ! helper variable
    type(ESMF_InterfaceInt) :: totalLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: totalUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: totalCountArg ! helper variable
    integer :: tmp_staggerloc

    ! Initialize return code 
    localrc = ESMF_RC_NOT_IMPL 
    if (present(rc)) rc = ESMF_RC_NOT_IMPL 

    ! Check init status of arguments 
    ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid, rc) 

    ! Check consistency 
    call ESMF_GridGet(grid, dimCount=dimCount, &
                      localDECount=localDECount, rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! Require farrayPtr dimCount to match grid dimCount 
    if (dimCount .ne. 1) then 
      call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
        "- farrayPtr dimCount does not match requested item dimCount", & 
        ESMF_CONTEXT, rc) 
      return 
    endif 

    ! Set Defaults
    if (present(docopy)) then
      docopyInt=docopy
    else
      docopyInt=ESMF_DATA_REF
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
 
    if (localDE>=localDeCount) then 
      call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
        "- localDE too big", ESMF_CONTEXT, rc) 
      return 
    endif 

    if (localDE<0) then 
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
    call ESMF_GridGetItemIntoArray(grid, staggerloc, item, array, &
                                    ESMF_DATA_REF, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return

    ! Obtain the native F90 array pointer via the LocalArray interface 
    allocate(larrayList(localDeCount))
 
    call ESMF_ArrayGet(array, larrayList=larrayList, rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    call ESMF_LocalArrayGet(larrayList(localDE+1), fptr, doCopy, rc=localrc) 
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
    exclusiveCountArg=ESMF_InterfaceIntCreate(exclusiveCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalLBoundArg=ESMF_InterfaceIntCreate(computationalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalUBoundArg=ESMF_InterfaceIntCreate(computationalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalCountArg=ESMF_InterfaceIntCreate(computationalCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalLBoundArg=ESMF_InterfaceIntCreate(totalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalUBoundArg = ESMF_InterfaceIntCreate(totalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalCountArg = ESMF_InterfaceIntCreate(totalCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_GridGetItemBounds(grid, localDE, tmp_staggerloc, item, &
      exclusiveLBoundArg, exclusiveUBoundArg, exclusiveCountArg, &
      computationalLBoundArg, computationalUBoundArg, computationalCountArg,&
      totalLBoundArg, totalUBoundArg, totalCountArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deallocate interface ints
    call ESMF_InterfaceIntDestroy(exclusiveLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully 
    if (present(rc)) rc = ESMF_SUCCESS 

    end subroutine ESMF_GridGetItem1DR8


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetItem2DR8"
!BOPI
! !IROUTINE: ESMF_GridGetItem - Get pointer to 2DR8 item

! !INTERFACE:
  ! Private name; call using ESMF_GridGetItem()
      subroutine ESMF_GridGetItem2DR8(grid, localDE, staggerloc, item, &
          exclusiveLBound, exclusiveUBound, exclusiveCount,                 &
          computationalLBound, computationalUBound, computationalCount,     &
          totalLBound, totalUBound, totalCount,                             &
          fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in) :: localDE
      type (ESMF_StaggerLoc), intent(in), optional  :: staggerloc
      type (ESMF_GridItem),   intent(in)            :: item
      integer,        target, intent(out), optional :: exclusiveLBound(:)
      integer,        target, intent(out), optional :: exclusiveUBound(:)
      integer,        target, intent(out), optional :: exclusiveCount(:)
      integer,        target, intent(out), optional :: computationalLBound(:)
      integer,        target, intent(out), optional :: computationalUBound(:)
      integer,        target, intent(out), optional :: computationalCount(:)
      integer,        target, intent(out), optional :: totalLBound(:)
      integer,        target, intent(out), optional :: totalUBound(:)
      integer,        target, intent(out), optional :: totalCount(:)
      real(ESMF_KIND_R8), pointer :: fptr(:,:)
      type(ESMF_CopyFlag), intent(in), optional :: docopy
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    This method gets a Fortran pointer to the piece of memory which holds the 
!    item data for the stagger locations on the given local DE. 
!    This is useful, for example, for setting the item values in a Grid, or
!    for reading the item values.
!
!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          Grid to get the information from.
!     \item[{localDE}]
!          The local DE to get the information for. {\tt [0,..,localDeCount-1]}
!     \item[{staggerloc}]
!          The stagger location to get the information for. 
!          Please see Section~\ref{sec:opt:staggerloc} for a list 
!          of predefined stagger locations. If not present, defaults to
!          ESMF\_STAGGERLOC\_CENTER.
!     \item[{item}]
!          The item to get the information for. Please see Section~\ref{sec:opt:griditem} for a 
!          list of valid items.   
!     \item[{[exclusiveLBound]}]
!          Upon return this holds the lower bounds of the exclusive region.
!          {\tt exclusiveLBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[exclusiveUBound]}]
!          Upon return this holds the upper bounds of the exclusive region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[exclusiveCount]}]
!          Upon return this holds the number of items in the exclusive region per dimension
!          (i.e. {\tt exclusiveUBound-exclusiveLBound+1}). {\tt exclusiveCount} must
!          be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalLBound]}]
!          Upon return this holds the lower bounds of the stagger region.
!          {\tt computationalLBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalUBound]}]
!          Upon return this holds the upper bounds of the stagger region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalCount]}]
!          Upon return this holds the number of items in the computational region per dimension
!          (i.e. {\tt computationalUBound-computationalLBound+1}). {\tt computationalCount}
!          must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalLBound]}]
!          Upon return this holds the lower bounds of the total region.
!          {\tt totalLBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalUBound]}]
!          Upon return this holds the upper bounds of the total region.
!          {\tt totalUBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalCount]}]
!          Upon return this holds the number of items in the total region per dimension
!          (i.e. {\tt totalUBound-totalLBound+1}). {\tt totalCount} must
!          be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{fptr}]
!          The pointer to the item data.
!     \item[{[doCopy]}]
!          If not specified, default to {\tt ESMF\_DATA\_REF}, in this case
!          fptr is a reference to the data in the Grid item arrays. 
!          Please see Section~\ref{opt:copyflag} for further description and a
!          list of valid values. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI


    ! Local variables 
    type(ESMF_Array) :: array 
    integer :: localrc ! local error status 
    integer :: localDeCount, dimCount 
    type(ESMF_TypeKind) :: typekind 
    type(ESMF_LocalArray), allocatable :: larrayList(:) 
    type(ESMF_CopyFlag) :: docopyInt
    integer :: coordDimCount(ESMF_MAXDIM)
    type(ESMF_InterfaceInt) :: exclusiveLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: exclusiveUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: exclusiveCountArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalCountArg ! helper variable
    type(ESMF_InterfaceInt) :: totalLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: totalUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: totalCountArg ! helper variable
    integer :: tmp_staggerloc

    ! Initialize return code 
    localrc = ESMF_RC_NOT_IMPL 
    if (present(rc)) rc = ESMF_RC_NOT_IMPL 

    ! Check init status of arguments 
    ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid, rc) 

    ! Check consistency 
    call ESMF_GridGet(grid, dimCount=dimCount, &
                      localDECount=localDECount, rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
      ESMF_CONTEXT, rcToReturn=rc)) return
 

    ! Require farrayPtr dimCount to match grid dimCount 
    if (dimCount .ne. 2) then 
    call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
      "- farrayPtr dimCount does not match requested item dimCount", & 
      ESMF_CONTEXT, rc) 
    return 
    endif 

    ! Set Defaults
    if (present(docopy)) then
      docopyInt=docopy
    else
      docopyInt=ESMF_DATA_REF
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
 
    if (localDE>=localDeCount) then 
      call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
        "- localDE too big", ESMF_CONTEXT, rc) 
      return 
    endif 

    if (localDE<0) then 
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
    call ESMF_GridGetItemIntoArray(grid, staggerloc, item, array, &
                                    ESMF_DATA_REF, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return

    ! Obtain the native F90 array pointer via the LocalArray interface 
    allocate(larrayList(localDeCount))
 
    call ESMF_ArrayGet(array, larrayList=larrayList, rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return
 
    call ESMF_LocalArrayGet(larrayList(localDE+1), fptr, doCopy, rc=localrc) 
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
    exclusiveCountArg=ESMF_InterfaceIntCreate(exclusiveCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalLBoundArg=ESMF_InterfaceIntCreate(computationalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalUBoundArg=ESMF_InterfaceIntCreate(computationalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalCountArg=ESMF_InterfaceIntCreate(computationalCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalLBoundArg=ESMF_InterfaceIntCreate(totalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalUBoundArg = ESMF_InterfaceIntCreate(totalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalCountArg = ESMF_InterfaceIntCreate(totalCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_GridGetItemBounds(grid, localDE, tmp_staggerloc, item, &
      exclusiveLBoundArg, exclusiveUBoundArg, exclusiveCountArg, &
      computationalLBoundArg, computationalUBoundArg, computationalCountArg,&
      totalLBoundArg, totalUBoundArg, totalCountArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deallocate interface ints
    call ESMF_InterfaceIntDestroy(exclusiveLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully 
    if (present(rc)) rc = ESMF_SUCCESS 

    end subroutine ESMF_GridGetItem2DR8


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetItem3DR8"
!BOPI
! !IROUTINE: ESMF_GridGetItem - Get pointer to 3DR8 item

! !INTERFACE:
  ! Private name; call using ESMF_GridGetItem()
      subroutine ESMF_GridGetItem3DR8(grid, localDE, staggerloc, item, &
          exclusiveLBound, exclusiveUBound, exclusiveCount,                 &
          computationalLBound, computationalUBound, computationalCount,     &
          totalLBound, totalUBound, totalCount,                             &      
          fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in) :: localDE
      type (ESMF_StaggerLoc), intent(in),  optional :: staggerloc
      type (ESMF_GridItem),   intent(in)            :: item
      integer,        target, intent(out), optional :: exclusiveLBound(:)
      integer,        target, intent(out), optional :: exclusiveUBound(:)
      integer,        target, intent(out), optional :: exclusiveCount(:)
      integer,        target, intent(out), optional :: computationalLBound(:)
      integer,        target, intent(out), optional :: computationalUBound(:)
      integer,        target, intent(out), optional :: computationalCount(:)
      integer,        target, intent(out), optional :: totalLBound(:)
      integer,        target, intent(out), optional :: totalUBound(:)
      integer,        target, intent(out), optional :: totalCount(:)
      real(ESMF_KIND_R8), pointer :: fptr(:,:,:)
      type(ESMF_CopyFlag), intent(in), optional :: docopy
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    This method gets a Fortran pointer to the piece of memory which holds the 
!    maks data and stagger locations on the given local DE. 
!    This is useful, for example, for setting the item values in a Grid, or
!    for reading the item values. 
!
!     The arguments are:
!     \begin{description}
!     \item[{grid}]
!          Grid to get the information from.
!     \item[{localDE}]
!          The local DE to get the information for. {\tt [0,..,localDeCount-1]}
!     \item[{staggerloc}]
!          The stagger location to get the information for. 
!          Please see Section~\ref{sec:opt:staggerloc} for a list 
!          of predefined stagger locations. If not present, defaults to
!          ESMF\_STAGGERLOC\_CENTER.
!     \item[{item}]
!          The item to get the information for. Please see Section~\ref{sec:opt:griditem} for a 
!          list of valid items.   
!     \item[{[exclusiveLBound]}]
!          Upon return this holds the lower bounds of the exclusive region.
!          {\tt exclusiveLBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[exclusiveUBound]}]
!          Upon return this holds the upper bounds of the exclusive region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[exclusiveCount]}]
!          Upon return this holds the number of items in the exclusive region per dimension
!          (i.e. {\tt exclusiveUBound-exclusiveLBound+1}). {\tt exclusiveCount} must
!          be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalLBound]}]
!          Upon return this holds the lower bounds of the stagger region.
!          {\tt computationalLBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalUBound]}]
!          Upon return this holds the upper bounds of the stagger region.
!          {\tt exclusiveUBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalCount]}]
!          Upon return this holds the number of items in the computational region per dimension
!          (i.e. {\tt computationalUBound-computationalLBound+1}). {\tt computationalCount}
!          must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalLBound]}]
!          Upon return this holds the lower bounds of the total region.
!          {\tt totalLBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalUBound]}]
!          Upon return this holds the upper bounds of the total region.
!          {\tt totalUBound} must be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[totalCount]}]
!          Upon return this holds the number of items in the total region per dimension
!          (i.e. {\tt totalUBound-totalLBound+1}). {\tt totalCount} must
!          be allocated to be of size equal to the item dimCount.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{fptr}]
!          The pointer to the item data.
!     \item[{[doCopy]}]
!          If not specified, default to {\tt ESMF\_DATA\_REF}, in this case
!          fptr is a reference to the data in the Grid item arrays. 
!          Please see Section~\ref{opt:copyflag} for further description and a
!          list of valid values. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI


 ! Local variables 
 type(ESMF_Array) :: array 
 integer :: localrc ! local error status 
 integer :: localDeCount, dimCount 
 type(ESMF_TypeKind) :: typekind 
 type(ESMF_LocalArray), allocatable :: larrayList(:) 
 type(ESMF_CopyFlag) :: docopyInt
 integer :: coordDimCount(ESMF_MAXDIM)
 type(ESMF_InterfaceInt) :: exclusiveLBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: exclusiveUBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: exclusiveCountArg ! helper variable
 type(ESMF_InterfaceInt) :: computationalLBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: computationalUBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: computationalCountArg ! helper variable
 type(ESMF_InterfaceInt) :: totalLBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: totalUBoundArg ! helper variable
 type(ESMF_InterfaceInt) :: totalCountArg ! helper variable
 integer :: tmp_staggerloc

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid, rc) 

 ! Check consistency 
 call ESMF_GridGet(grid, dimCount=dimCount, &
                   localDECount=localDECount, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return
 
 ! Require farrayPtr dimCount to match coordinate dimCount 
 if (dimCount .ne. 3) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- farrayPtr dimCount does not match requested item dimCount", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

! Set Defaults
if (present(docopy)) then
   docopyInt=docopy
else
  docopyInt=ESMF_DATA_REF
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
 
 if (localDE>=localDeCount) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- localDE too big", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 

 if (localDE<0) then 
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

    call ESMF_GridGetItemIntoArray(grid, staggerloc, item, array, &
                                    ESMF_DATA_REF, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return

    ! Obtain the native F90 array pointer via the LocalArray interface 
    allocate(larrayList(localDeCount))
 
    call ESMF_ArrayGet(array, larrayList=larrayList, rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return
 
    call ESMF_LocalArrayGet(larrayList(localDE+1), fptr, doCopy, rc=localrc) 
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
    exclusiveCountArg=ESMF_InterfaceIntCreate(exclusiveCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalLBoundArg=ESMF_InterfaceIntCreate(computationalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalUBoundArg=ESMF_InterfaceIntCreate(computationalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalCountArg=ESMF_InterfaceIntCreate(computationalCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalLBoundArg=ESMF_InterfaceIntCreate(totalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalUBoundArg = ESMF_InterfaceIntCreate(totalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalCountArg = ESMF_InterfaceIntCreate(totalCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_GridGetItemBounds(grid, localDE, tmp_staggerloc, item, &
      exclusiveLBoundArg, exclusiveUBoundArg, exclusiveCountArg, &
      computationalLBoundArg, computationalUBoundArg, computationalCountArg,&
      totalLBoundArg, totalUBoundArg, totalCountArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deallocate interface ints
    call ESMF_InterfaceIntDestroy(exclusiveLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully 
    if (present(rc)) rc = ESMF_SUCCESS 

    end subroutine ESMF_GridGetItem3DR8


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetItemBounds"
!BOP
! !IROUTINE: ESMF_GridGetItem -  Get Grid item bounds

! !INTERFACE:
  ! Private name; call using ESMF_GridGetItem()
      subroutine ESMF_GridGetItemBounds(grid, localDE, staggerloc, item, &
          exclusiveLBound, exclusiveUBound, exclusiveCount,                   &
          computationalLBound, computationalUBound, computationalCount,       &
          totalLBound, totalUBound, totalCount, rc)

!
! !ARGUMENTS:
      type(ESMF_Grid),        intent(in)            :: grid
      integer,                intent(in)            :: localDE
      type (ESMF_StaggerLoc), intent(in),  optional :: staggerloc
      type (ESMF_GridItem),   intent(in)            :: item
      integer,        target, intent(out), optional :: exclusiveLBound(:)
      integer,        target, intent(out), optional :: exclusiveUBound(:)
      integer,        target, intent(out), optional :: exclusiveCount(:)
      integer,        target, intent(out), optional :: computationalLBound(:)
      integer,        target, intent(out), optional :: computationalUBound(:)
      integer,        target, intent(out), optional :: computationalCount(:)
      integer,        target, intent(out), optional :: totalLBound(:)
      integer,        target, intent(out), optional :: totalUBound(:)
      integer,        target, intent(out), optional :: totalCount(:)
      integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!  This method gets information about the range of index space which a particular
!  piece of item data occupies.  In other words, this method returns the 
!  bounds of the item arrays.  Note that unlike the output from the 
!  Array, these values also include the undistributed dimensions and are
!  ordered to reflect the order of the indices in the item. So, for example,
!  {\tt totalLBound} and {\tt totalUBound} should match the bounds of the Fortran array
!  retrieved by {\tt ESMF\_GridGetItem}. 
!
!The arguments are:
!\begin{description}
!\item[{grid}]
!    Grid to get the information from.
!\item[{localDE}]
!     The local DE from which to get the information. {\tt [0,..,localDeCount-1]}
!\item[{staggerloc}]
!     The stagger location to get the information for. 
!     Please see Section~\ref{sec:opt:staggerloc} for a list 
!     of predefined stagger locations. If not present, defaults to
!     ESMF\_STAGGERLOC\_CENTER.
!\item[{item}]
!     The item to get the information for. Please see Section~\ref{sec:opt:griditem} for a 
!          list of valid items.   
!\item[{[exclusiveLBound]}]
!     Upon return this holds the lower bounds of the exclusive region.
!     {\tt exclusiveLBound} must be allocated to be of size equal to the item dimCount.
!     Please see Section~\ref{sec:grid:usage:bounds} for a description
!     of the regions and their associated bounds and counts. 
!\item[{[exclusiveUBound]}]
!     Upon return this holds the upper bounds of the exclusive region.
!     {\tt exclusiveUBound} must be allocated to be of size equal to the item dimCount.
!     Please see Section~\ref{sec:grid:usage:bounds} for a description
!     of the regions and their associated bounds and counts. 
!\item[{[exclusiveCount]}]
!     Upon return this holds the number of items in the exclusive region per dimension
!     (i.e. {\tt exclusiveUBound-exclusiveLBound+1}). {\tt exclusiveCount} must
!     be allocated to be of size equal to the item dimCount.
!     Please see Section~\ref{sec:grid:usage:bounds} for a description
!     of the regions and their associated bounds and counts. 
!\item[{[computationalLBound]}]
!     Upon return this holds the lower bounds of the stagger region.
!     {\tt computationalLBound} must be allocated to be of size equal to the item dimCount.
!     Please see Section~\ref{sec:grid:usage:bounds} for a description
!     of the regions and their associated bounds and counts. 
!\item[{[computationalUBound]}]
!     Upon return this holds the upper bounds of the stagger region.
!     {\tt computationalUBound} must be allocated to be of size equal to the item dimCount.
!     Please see Section~\ref{sec:grid:usage:bounds} for a description
!     of the regions and their associated bounds and counts. 
!\item[{[computationalCount]}]
!     Upon return this holds the number of items in the computational region per dimension
!     (i.e. {\tt computationalUBound-computationalLBound+1}). {\tt computationalCount}
!      must be allocated to be of size equal to the item dimCount.
!     Please see Section~\ref{sec:grid:usage:bounds} for a description
!     of the regions and their associated bounds and counts. 
!\item[{[totalLBound]}]
!     Upon return this holds the lower bounds of the total region.
!     {\tt totalLBound} must be allocated to be of size equal to the item dimCount.
!     Please see Section~\ref{sec:grid:usage:bounds} for a description
!     of the regions and their associated bounds and counts. 
!\item[{[totalUBound]}]
!     Upon return this holds the upper bounds of the total region.
!     {\tt totalUBound} must be allocated to be of size equal to the item dimCount.
!     Please see Section~\ref{sec:grid:usage:bounds} for a description
!     of the regions and their associated bounds and counts. 
!\item[{[totalCount]}]
!     Upon return this holds the number of items in the total region per dimension
!     (i.e. {\tt totalUBound-totalLBound+1}). {\tt totalCount} must
!      be allocated to be of size equal to the item dimCount.
!     Please see Section~\ref{sec:grid:usage:bounds} for a description
!     of the regions and their associated bounds and counts. 
!\item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!\end{description}
!
!EOP

    integer :: localrc ! local error status
    type(ESMF_InterfaceInt) :: exclusiveLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: exclusiveUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: exclusiveCountArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalCountArg ! helper variable
    type(ESMF_InterfaceInt) :: totalLBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: totalUBoundArg ! helper variable
    type(ESMF_InterfaceInt) :: totalCountArg ! helper variable
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
    exclusiveCountArg=ESMF_InterfaceIntCreate(exclusiveCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalLBoundArg=ESMF_InterfaceIntCreate(computationalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalUBoundArg=ESMF_InterfaceIntCreate(computationalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalCountArg=ESMF_InterfaceIntCreate(computationalCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalLBoundArg=ESMF_InterfaceIntCreate(totalLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalUBoundArg = ESMF_InterfaceIntCreate(totalUBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalCountArg = ESMF_InterfaceIntCreate(totalCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_GridGetItemBounds(grid, localDE, tmp_staggerloc, item,  &
      exclusiveLBoundArg, exclusiveUBoundArg, exclusiveCountArg, &
      computationalLBoundArg, computationalUBoundArg, computationalCountArg,&
      totalLBoundArg, totalUBoundArg, totalCountArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deallocate interface ints
    call ESMF_InterfaceIntDestroy(exclusiveLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalUBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetItemBounds


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetItemIntoArray"
!BOP
! !IROUTINE: ESMF_GridGetItem - Get item and put into an ESMF Array

! !INTERFACE:
  ! Private name; call using ESMF_GridGetItem()
      subroutine ESMF_GridGetItemIntoArray(grid, staggerloc, item, array, &
                            docopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type (ESMF_StaggerLoc), intent(in),optional  :: staggerloc
      type (ESMF_GridItem), intent(in)   :: item
      type(ESMF_Array), intent(out) :: array
      type(ESMF_CopyFlag), intent(in), optional :: docopy ! NOT IMPLEMENTED
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    This method allows the user to get access to the ESMF Array holding
!    item data at a particular stagger location. This is useful, for example, 
!    to set the item values. To have an Array to access, the item Array
!    must have already been allocated, for example by {\tt ESMF\_GridAddItem} or
!    {\tt ESMF\_GridSetItem}.
!
!     The arguments are:
!     \begin{description}
!     \item[{staggerloc}]
!          The stagger location from which to get the arrays. 
!          Please see Section~\ref{sec:opt:staggerloc} for a list 
!          of predefined stagger locations. If not present, defaults to ESMF\_STAGGERLOC\_CENTER.
!     \item[{item}]
!          The item from which to get the arrays. Please see Section~\ref{sec:opt:griditem} for a 
!          list of valid items.  
!     \item[{array}]
!          An array into which to put the item infomation. 
!     \item[{[doCopy]}]
!          If not specified, default to {\tt ESMF\_DATA\_REF}, in this case 
!          {\tt array} will contain a reference to the Grid item Arrays.
!           Please see Section~\ref{opt:copyflag} for
!          further description and a list of valid values. 
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

    ! Call C++ Subroutine
    call c_ESMC_gridgetitemintoarray(grid%this,tmp_staggerloc, item, &
      array, docopy, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


    ! Set Array as created
    call ESMF_ArraySetInitCreated(array,localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetItemIntoArray


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetStatus"
!BOP
! !IROUTINE: ESMF_GridGetStatus - Return the status of the Grid

! !INTERFACE:
     function ESMF_GridGetStatus(grid)
!
! !RETURN VALUE:
     type(ESMF_GridStatus) :: ESMF_GridGetStatus
!
! !ARGUMENTS:
     type(ESMF_Grid) :: grid

!
! !DESCRIPTION:
! Returns the status of the passed in Grid object. 
!
! The arguments are:
! \begin{description}
! \item[{grid}]
!      The grid to return the status from. 
! \end{description}
!
!EOP

    integer :: rc ! fake return code for INIT_CHECK_DEEP

    ! init grid status
    ESMF_GridGetStatus=ESMF_GRIDSTATUS_UNINIT

    ! check variables
    ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

    ! Call C++ Subroutine to get the status
    call c_ESMC_gridgetstatus(grid%this, ESMF_GridGetStatus)

      end function ESMF_GridGetStatus


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSerialize"

!BOPI
! !IROUTINE: ESMF_GridSerialize - Serialize grid info into a byte stream
!
! !INTERFACE:
      subroutine ESMF_GridSerialize(grid, buffer, length, offset, &
                                    attreconflag, inquireflag, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid 
      character, pointer, dimension(:) :: buffer
      integer, intent(inout) :: length
      integer, intent(inout) :: offset
      type(ESMF_AttReconcileFlag), intent(in), optional :: attreconflag
      type(ESMF_InquireFlag), intent(in), optional :: inquireflag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Takes an {\tt ESMF\_Grid} object and adds all the information needed
!      to  recreate the object based on this information.  
!      Expected to be used by {\tt ESMF\_StateReconcile()}.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           {\tt ESMF\_Grid} object to be serialized.
!     \item [buffer]
!           Data buffer which will hold the serialized information.
!     \item [length]
!           Current length of buffer, in bytes.  If the serialization
!           process needs more space it will allocate it and update
!           this length.
!     \item [offset]
!           Current write offset in the current buffer.  This will be
!           updated by this routine and return pointing to the next
!           available byte in the buffer.
!     \item[{[attreconflag]}]
!           Flag to tell if Attribute serialization is to be done
!     \item[{[inquireflag]}]
!           Flag to tell if serialization is to be done (ESMF_NOINQUIRE)
!           or if this is simply a size inquiry (ESMF_INQUIREONLY)
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc
      type(ESMF_AttReconcileFlag) :: lattreconflag
      type(ESMF_InquireFlag) :: linquireflag

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      ! deal with optional attreconflag and inquireflag
      if (present(attreconflag)) then
        lattreconflag = attreconflag
      else
        lattreconflag = ESMF_ATTRECONCILE_OFF
      endif

      if (present (inquireflag)) then
        linquireflag = inquireflag
      else
        linquireflag = ESMF_NOINQUIRE
      end if

      call c_ESMC_GridSerialize(grid, buffer, length, offset, &
                                 lattreconflag, linquireflag, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      ! return success
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridSerialize

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridDeserialize"

!BOPI
! !IROUTINE: ESMF_GridDeserialize - Deserialize a byte stream into a Grid
!
! !INTERFACE:
      function ESMF_GridDeserialize(buffer, offset, &
                                    attreconflag, rc) 
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridDeserialize   
!
! !ARGUMENTS:
      character, pointer, dimension(:) :: buffer
      integer, intent(inout) :: offset
      type(ESMF_AttReconcileFlag), optional :: attreconflag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Takes a byte-stream buffer and reads the information needed to
!      recreate a Grid object.  Recursively calls the deserialize routines
!      needed to recreate the subobjects.
!      Expected to be used by {\tt ESMF\_StateReconcile()}.
!
!     The arguments are:
!     \begin{description}
!     \item [buffer]
!           Data buffer which holds the serialized information.
!     \item [offset]
!           Current read offset in the current buffer.  This will be
!           updated by this routine and return pointing to the next
!           unread byte in the buffer.
!     \item[{[attreconflag]}]
!           Flag to tell if Attribute serialization is to be done
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc
      type(ESMF_Grid) :: grid
      type(ESMF_AttReconcileFlag) :: lattreconflag

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if  (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! deal with optional attreconflag
      if (present(attreconflag)) then
        lattreconflag = attreconflag
      else
        lattreconflag = ESMF_ATTRECONCILE_OFF
      endif

      ! Call into C++ to Deserialize the Grid
      call c_ESMC_GridDeserialize(grid%this, buffer, offset, &
        lattreconflag, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      ! Set return value
      ESMF_GridDeserialize = grid

     ! Set init status
      ESMF_INIT_SET_CREATED(ESMF_GridDeserialize)

      if  (present(rc)) rc = ESMF_SUCCESS


      end function ESMF_GridDeserialize

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridMatch()"
!BOP
! !IROUTINE: ESMF_GridMatch - Check if two Grid objects match

! !INTERFACE:
  function ESMF_GridMatch(grid1, grid2, rc)
!
! !RETURN VALUE:
    logical :: ESMF_GridMatch
      
! !ARGUMENTS:
    type(ESMF_Grid),  intent(in)              :: grid1
    type(ESMF_Grid),  intent(in)              :: grid2
    integer,          intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Check if {\tt grid1} and {\tt grid2} match. Returns
!      .true. if Grid objects match, .false. otherwise. This
!      method current just checks if grid1 and grid2 are the
!      same object, future work will do a more complex check.
!
!     The arguments are:
!     \begin{description}
!     \item[grid1] 
!          {\tt ESMF\_Grid} object.
!     \item[grid2] 
!          {\tt ESMF\_Grid} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer      :: localrc      ! local return code
    integer      :: matchResult

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! init to one setting in case of error
    ESMF_GridMatch = .false.
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid1, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid2, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_GridMatch(grid1, grid2, matchResult, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (matchResult .eq. 1) then
       ESMF_GridMatch = .true.
    else
       ESMF_GridMatch = .false.
    endif

    if (present(rc)) rc = ESMF_SUCCESS
    
  end function ESMF_GridMatch
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetFromDistGrid"
!BOPI
! !IROUTINE: ESMF_GridSet - Set the values in a Grid which has been created with CreateEmpty 

! !INTERFACE:
  ! Private name; call using ESMF_GridSet()
    subroutine ESMF_GridSetFromDistGrid(grid, name, coordTypeKind, distgrid, & 
                 distgridToGridMap, distDim,			             &
                 coordDimCount, coordDimMap, minIndex, maxIndex,             &
                 localArbIndexCount, localArbIndex,                                   &
                 gridEdgeLWidth, gridEdgeUWidth, gridAlign, gridMemLBound,   &
                 indexflag, destroyDistgrid, destroyDELayout, rc)
!
! !RETURN VALUE:

!
! !ARGUMENTS:
       type(ESMF_Grid),       intent(inout)           :: grid
       character (len=*),     intent(in),   optional  :: name
       type(ESMF_TypeKind),   intent(in),   optional  :: coordTypeKind
       type(ESMF_DistGrid),   intent(in),   optional  :: distgrid
       integer,               intent(in),   optional  :: distgridToGridMap(:)
       integer,               intent(in),   optional  :: distDim(:)
       integer,               intent(in),   optional  :: coordDimCount(:)
       integer,               intent(in),   optional  :: coordDimMap(:,:)
       integer,               intent(in),   optional  :: minIndex(:)
       integer,               intent(in),   optional  :: maxIndex(:)
       integer,               intent(in),   optional  :: localArbIndexCount
       integer,               intent(in),   optional  :: localArbIndex(:,:)
       integer,               intent(in),   optional  :: gridEdgeLWidth(:)
       integer,               intent(in),   optional  :: gridEdgeUWidth(:)
       integer,               intent(in),   optional  :: gridAlign(:)
       integer,               intent(in),   optional  :: gridMemLBound(:)
       type(ESMF_IndexFlag),  intent(in),   optional  :: indexflag
       logical,               intent(in),   optional  :: destroyDistgrid
       logical,               intent(in),   optional  :: destroyDELayout
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
!      distributed over DEs. 
! \item[{[distgridToGridMap]}] 
!      List that has as dimCount elements.
!      The elements map each dimension of distgrid to a dimension in the grid.
!       (i.e. the values should range from 1 to dimCount). If not specified, the default
!       is to map all of distgrid's dimensions against the dimensions of the
!       grid in sequence. 
! \item[{[minIndex]}] 
!      Tuple to start the index ranges at. If not present, defaults
!      to /1,1,1,.../.
! \item[{[maxIndex]}] 
!      The upper extend of the grid index ranges.
! \item[{[localArbIndex]}] 
!      This 2D array specifies the indices of the local grid cells.  The 
!      dimensions should be localArbIndexCount * number of grid dimensions
!      where localArbIndexCount is the input argument specified below
! \item[{localArbIndexCount}] 
!      number of grid cells in the local DE
! \item[{[gridEdgeLWidth]}] 
!      The padding around the lower edges of the grid. This padding is between
!      the index space corresponding to the cells and the boundary of the 
!      the exclusive region. This extra space is to contain the extra
!      padding for non-center stagger locations, and should be big enough
!      to hold any stagger in the grid. 
! \item[{[gridEdgeUWidth]}] 
!      The padding around the upper edges of the grid. This padding is between
!      the index space corresponding to the cells and the boundary of the 
!      the exclusive region. This extra space is to contain the extra
!      padding for non-center stagger locations, and should be big enough
!      to hold any stagger in the grid. 
! \item[{[gridAlign]}] 
!     Specification of how the stagger locations should align with the cell
!     index space (can be overridden by the individual staggerAligns). If
!     the {\tt gridEdgeWidths} are not specified than this parameter
!     implies the EdgeWidths.
! \item[{[gridMemLBound]}] 
!      Specifies the lower index range of the memory of every DE in this Grid. 
!      Only used when indexflag is {\tt ESMF\_INDEX\_USER}. May be overridden
!      by staggerMemLBound. 
! \item[{[indexflag]}]
!      Indicates the indexing scheme to be used in the new Grid. Please see
!      Section~\ref{opt:indexflag} for the list of options. If not present,
!      defaults to ESMF\_INDEX\_DELOCAL.
! \item[{[destroyDistgrid]}]
!      If true, when the Grid is destroyed the DistGrid will be destroyed also. 
!      Defaults to false. 
! \item[{[destroyDELayout]}]
!      If true, when the Grid is destroyed the DELayout will be destroyed also. 
!      Defaults to false. 
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
    integer :: localrc ! local error status
    integer :: nameLen 
    type(ESMF_InterfaceInt) :: gridEdgeLWidthArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: gridEdgeUWidthArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: gridAlignArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: gridMemLBoundArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: distgridToGridMapArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: distDimArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: coordDimCountArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: coordDimMapArg ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: minIndexArg ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: maxIndexArg ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: localArbIndexArg ! Language Interface Helper Var
    integer :: intDestroyDistgrid,intDestroyDELayout


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

    !! gridEdgeLWidth and gridEdgeUWidth
    gridEdgeLWidthArg = ESMF_InterfaceIntCreate(gridEdgeLWidth, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    gridEdgeUWidthArg = ESMF_InterfaceIntCreate(gridEdgeUWidth, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    gridAlignArg = ESMF_InterfaceIntCreate(gridAlign, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    gridMemLBoundArg = ESMF_InterfaceIntCreate(gridMemLBound, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    !! distgridToGridMap
    distgridToGridMapArg = ESMF_InterfaceIntCreate(distgridToGridMap, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    !! distDim
    distDimArg = ESMF_InterfaceIntCreate(distDim, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    !! Description of array factorization
    coordDimCountArg = ESMF_InterfaceIntCreate(coordDimCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    coordDimMapArg = ESMF_InterfaceIntCreate(farray2D=coordDimMap, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    !! Index bound and localArbIndex array
    minIndexArg = ESMF_InterfaceIntCreate(minIndex, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    maxIndexArg = ESMF_InterfaceIntCreate(maxIndex, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    localArbIndexArg = ESMF_InterfaceIntCreate(farray2D=localArbIndex, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    !! Convert destroyDistGrid flag
    if (present(destroyDistgrid)) then
        if (destroyDistgrid) then
           intDestroyDistgrid=1
        else
           intDestroyDistgrid=0
         endif
    else
           intDestroyDistgrid=0
    endif

    !! Convert destroyDELayout flag
    if (present(destroyDELayout)) then
        if (destroyDELayout) then
           intDestroyDELayout=1
        else
           intDestroyDELayout=0
         endif
    else
           intDestroyDELayout=0
    endif

    ! Call C++ Subroutine to do the create
    call c_ESMC_gridsetfromdistgrid(grid%this, nameLen, name, &
      coordTypeKind, distgrid, &
      distgridToGridMapArg, distDimArg, &
      coordDimCountArg, coordDimMapArg, &
      minIndexArg, maxIndexArg, localArbIndexArg, localArbIndexCount,  &
      gridEdgeLWidthArg, gridEdgeUWidthArg, gridAlignArg, &
      gridMemLBoundArg, indexflag,  intDestroyDistGrid, intDestroyDELayout, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deallocate helper variables
    call ESMF_InterfaceIntDestroy(gridEdgeUWidthArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(gridEdgeLWidthArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(gridAlignArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(gridMemLBoundArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(distgridToGridMapArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(distDimArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(coordDimCountArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(coordDimMapArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(minIndexArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(maxIndexArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(localArbIndexArg, rc=localrc)
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
!    If not specified, default to {\tt ESMF\_DATA\_REF}, in this case the Grid 
!    coordinate Array will be set to a reference to {\tt array}. Please see 
!    Section~\ref{opt:copyflag} for further description and a list of
!    valid values. 
!    [THE ESMF\_DATA\_COPY OPTION IS CURRENTLY NOT IMPLEMENTED] 
!\item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!\end{description}
!
!EOP
    integer :: tmp_staggerloc
    integer :: localrc ! local error status
    type(ESMF_GridDecompType) :: decompType

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayGetInit, array, rc)
!    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_GridGetInit, grid, rc)

    call ESMF_GridGetDecompType(grid, decompType, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
   
    ! handle staggerloc
    if (present(staggerloc)) then
       if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. &
	  (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
                 ESMF_CONTEXT, rc) 
           return
	else
       	   tmp_staggerloc=staggerloc%staggerloc
	endif
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
#define ESMF_METHOD "ESMF_GridSetCmmitShapeTileIrreg"
!BOP
! !IROUTINE: ESMF_GridSetCommitShapeTile - Set and complete a Grid with an irregular distribution

! !INTERFACE:
  ! Private name; call using ESMF_GridSetCommitShapeTile()
     subroutine ESMF_GridSetCmmitShapeTileIrreg(grid, name,coordTypeKind, minIndex,  &
                        countsPerDEDim1, countsPerDeDim2, countsPerDEDim3, &
                        connDim1, connDim2, connDim3, &
                        poleStaggerLoc1, poleStaggerLoc2, poleStaggerLoc3, &
                        bipolePos1, bipolePos2, bipolePos3, &
                        coordDep1, coordDep2, coordDep3, &
                        gridEdgeLWidth, gridEdgeUWidth, gridAlign, gridMemLBound, &
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
       integer,               intent(in),   optional  :: gridEdgeLWidth(:)
       integer,               intent(in),   optional  :: gridEdgeUWidth(:)
       integer,               intent(in),   optional  :: gridAlign(:)
       integer,               intent(in),   optional  :: gridMemLBound(:)
       type(ESMF_IndexFlag),  intent(in),   optional  :: indexflag
       integer,               intent(in),   optional  :: petMap(:,:,:)
       integer,               intent(out),  optional  :: rc
!
! !DESCRIPTION:
!
! This method sets information into an empty Grid and then commits it to 
! create a single tile, irregularly distributed grid 
! (see Figure \ref{fig:GridDecomps}).
! To specify the irregular distribution, the user passes in an array 
! for each grid dimension, where the length of the array is the number
! of DEs in the dimension.   Up to three dimensions can be specified, 
! using the countsPerDEDim1, countsPerDEDim2, countsPerDEDim3 arguments.
! The index of each array element corresponds to a DE number.  The 
! array value at the index is the number of grid cells on the DE in 
! that dimension.  The dimCount of the grid is equal to the number of 
! countsPerDEDim<> arrays that are specified. 
!
! Section \ref{example:2DIrregUniGrid} shows an example
! of using this method to create a 2D Grid with uniformly spaced 
! coordinates.  This creation method can also be used as the basis for
! grids with rectilinear coordinates or curvilinear coordinates.
!
! For consistency's sake the {\tt ESMF\_GridSetCommitShapeTile()} call
! should be executed in the same set or a subset of the PETs in which the
! {\tt ESMF\_GridCreateEmpty()} call was made. If the call
! is made in a subset, the Grid objects outside that subset will
! still be "empty" and not usable. 
!
! The arguments are:
! \begin{description}
! \item[{grid}]
!     The empty {\tt ESMF\_Grid} to set information into and then commit.
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
!     arrays map to. If not present the default is 1,2,...,grid rank.  
! \item[{[coordDep2]}] 
!     This array specifies the dependence of the second 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the second
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is 1,2,...,grid rank.  
! \item[{[coordDep3]}] 
!     This array specifies the dependence of the third 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the third
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is 1,2,...,grid rank. 
! \item[{[gridEdgeLWidth]}] 
!      The padding around the lower edges of the grid. This padding is between
!      the index space corresponding to the cells and the boundary of the 
!      the exclusive region. This extra space is to contain the extra
!      padding for non-center stagger locations, and should be big enough
!      to hold any stagger in the grid. 
! \item[{[gridEdgeUWidth]}] 
!      The padding around the upper edges of the grid. This padding is between
!      the index space corresponding to the cells and the boundary of the 
!      the exclusive region. This extra space is to contain the extra
!      padding for non-center stagger locations, and should be big enough
!      to hold any stagger in the grid. 
! \item[{[gridAlign]}] 
!     Specification of how the stagger locations should align with the cell
!     index space (can be overridden by the individual staggerAligns). If
!     the {\tt gridEdgeWidths} are not specified than this parameter
!     implies the EdgeWidths.
! \item[{[gridMemLBound]}] 
!      Specifies the lower index range of the memory of every DE in this Grid. 
!      Only used when indexflag is {\tt ESMF\_INDEX\_USER}. May be overridden
!      by staggerMemLBound. 
! \item[{[indexflag]}]
!      Indicates the indexing scheme to be used in the new Grid. Please see
!      Section~\ref{opt:indexflag} for the list of options. If not present,
!      defaults to ESMF\_INDEX\_DELOCAL.
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
    integer, pointer     :: coordDimCount(:)
    integer, pointer     :: coordDimMap(:,:)
    integer              :: localrc
    integer              :: dimCount,i,maxSizeDEDim
    integer, pointer     :: distgridToGridMap(:), deDimCount(:)
    integer, pointer     :: minIndexLocal(:)
    integer, pointer     :: maxIndexLocal(:)
    integer, pointer     :: gridEdgeLWidthLocal(:)
    integer, pointer     :: gridEdgeUWidthLocal(:)
    integer, pointer     :: gridAlignLocal(:)
    integer, pointer     :: countsPerDEDim1Local(:)
    integer, pointer     :: countsPerDEDim2Local(:)
    integer, pointer     :: countsPerDEDim3Local(:)
    integer, pointer     :: deBlockList(:,:,:),minPerDEDim(:,:),maxPerDEDim(:,:)
    integer              :: deCount
    integer              :: d,i1,i2,i3,k
    type(ESMF_GridConn)  :: connDim1Local(2)
    type(ESMF_GridConn)  :: connDim2Local(2)
    type(ESMF_GridConn)  :: connDim3Local(2)
    integer              :: connCount, petListCount 
    integer              :: top

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Compute the Grid DimCount and Derivatives ---------------------------------------------------
    ! dimCount
    if (present(countsPerDEDim3)) then
	dimCount=3
    else
	dimCount=2
    endif

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

    if ((dimCount .lt. 3) .and. present(connDim3)) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- connDim3 not allowed when grid is less than dimCount 3", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if ((dimCount .lt. 3) .and. present(poleStaggerLoc3)) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- poleStaggerLoc3 not allowed when grid is less than dimCount 3", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if ((dimCount .lt. 3) .and. present(bipolePos3)) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- bipolePos3 not allowed when grid is less than dimCount 3", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif


    if ((dimCount .lt. 3) .and. present(coordDep3)) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- coordDep3 not allowed when grid is less than dimCount 3", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if (present(coordDep1)) then
       if ((size(coordDep1) < 1) .or. (size(coordDep1)>dimCount)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- coordDep1 size incompatible with grid dimCount", & 
               ESMF_CONTEXT, rc) 
          return 
       endif
    endif

    if (present(coordDep2)) then
       if ((size(coordDep2) < 1) .or. (size(coordDep2)>dimCount)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- coordDep2 size incompatible with grid dimCount", & 
               ESMF_CONTEXT, rc) 
          return 
       endif
    endif

    if (present(coordDep3)) then
       if ((size(coordDep3) < 1) .or. (size(coordDep3)>dimCount)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- coordDep3 size incompatible with grid dimCount", & 
               ESMF_CONTEXT, rc) 
          return 
       endif
    endif

    if (present(minIndex)) then
       if (size(minIndex) .ne. dimCount) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- minIndex size must equal grid dimCount", & 
               ESMF_CONTEXT, rc) 
          return 
       endif
    endif


    if (present(petMap)) then
       if (dimCount .gt. 2) then
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



    ! Check DimCount of gridWidths and Aligns
    if (present(gridEdgeLWidth)) then
        if (size(gridEdgeLWidth) .ne. dimCount) then
           call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
                     "- gridEdgeLWidth must be of size equal to Grid dimCount", & 
                     ESMF_CONTEXT, rc) 
              return
        endif 
    endif

    if (present(gridEdgeUWidth)) then
        if (size(gridEdgeUWidth) .ne. dimCount) then
           call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
                     "- gridEdgeUWidth must be of size equal to Grid dimCount", & 
                     ESMF_CONTEXT, rc) 
              return
        endif 
    endif

    if (present(gridAlign)) then
        if (size(gridAlign) .ne. dimCount) then
           call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
                     "- gridAlign must be of size equal to Grid dimCount", & 
                     ESMF_CONTEXT, rc) 
              return
        endif 
    endif

   ! make sure connected dimensions don't have an edge width
   if (present(connDim1)) then
      if (size(connDim1) .eq. 1) then
         if (connDim1(1) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeLWidth)) then
               if (gridEdgeLWidth(1) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have LWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
            if (present(gridEdgeUWidth)) then
               if (gridEdgeUWidth(1) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have UWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
      else if (size(connDim1) .eq. 2) then
         if (connDim1(1) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeLWidth)) then
               if (gridEdgeLWidth(1) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have LWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
         if (connDim1(2) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeUWidth)) then
               if (gridEdgeUWidth(1) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have UWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
      endif
   endif

   ! make sure connected dimensions don't have an edge width
   if (present(connDim2)) then
      if (size(connDim2) .eq. 1) then
         if (connDim2(1) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeLWidth)) then
               if (gridEdgeLWidth(2) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have LWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
            if (present(gridEdgeUWidth)) then
               if (gridEdgeUWidth(2) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have UWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
      else if (size(connDim2) .eq. 2) then
         if (connDim2(1) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeLWidth)) then
               if (gridEdgeLWidth(2) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have LWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
         if (connDim2(2) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeUWidth)) then
               if (gridEdgeUWidth(2) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have UWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
      endif
   endif


   ! make sure connected dimensions don't have an edge width
   if (present(connDim3)) then
      if (size(connDim3) .eq. 1) then
         if (connDim3(1) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeLWidth)) then
               if (gridEdgeLWidth(3) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have LWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
            if (present(gridEdgeUWidth)) then
               if (gridEdgeUWidth(3) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have UWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
      else if (size(connDim3) .eq. 2) then
         if (connDim3(1) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeLWidth)) then
               if (gridEdgeLWidth(3) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have LWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
         if (connDim3(2) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeUWidth)) then
               if (gridEdgeUWidth(3) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have UWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
      endif
   endif


   ! check for gridMemLBound issues
   if (present(gridMemLBound)) then
      if (.not. present(indexflag)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                "- when using gridMemLBound must specify indexflag=ESMF_INDEX_USER ", & 
                 ESMF_CONTEXT, rc) 
              return
      else if (.not. (indexflag .eq. ESMF_INDEX_USER)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                "- when using gridMemLBound must specify indexflag=ESMF_INDEX_USER ", & 
                 ESMF_CONTEXT, rc) 
              return
      endif
   else
      if (present(indexflag)) then
         if (indexflag .eq. ESMF_INDEX_USER) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                "- when using indexflag=ESMF_INDEX_USER must provide gridMemLBound ", & 
                   ESMF_CONTEXT, rc) 
              return
         endif
      endif
   endif


   ! Check for non-valid connection types here


   !TODO: Consider making some of these a separate local subroutine (particularly if you're going to 
   !      have 3 of these ShapeCreate subroutines with only minor changes


    ! Copy vales for countsPerDEDim --------------------------------------------
    allocate(countsPerDEDim1Local(size(countsPerDEDim1)), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating minIndexLocal", &
                                     ESMF_CONTEXT, rc)) return
    countsPerDEDim1Local=countsPerDEDim1

    allocate(countsPerDEDim2Local(size(countsPerDEDim2)), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating minIndexLocal", &
                                     ESMF_CONTEXT, rc)) return
    countsPerDEDim2Local=countsPerDEDim2

    if (dimCount .gt. 2) then
       allocate(countsPerDEDim3Local(size(countsPerDEDim3)), stat=localrc)
       if (ESMF_LogMsgFoundAllocError(localrc, "Allocating minIndexLocal", &
                                      ESMF_CONTEXT, rc)) return
       countsPerDEDim3Local=countsPerDEDim3
    endif


    ! Set Defaults -------------------------------------------------------------

    ! Set default for minIndex 
    allocate(minIndexLocal(dimCount), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating minIndexLocal", &
                                     ESMF_CONTEXT, rc)) return

    if (present(minIndex)) then
       minIndexLocal(:)=minIndex(:)
    else
       do i=1,dimCount
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
       connDim1Local(1)=ESMF_GRIDCONN_NONE ! if not present then default to no connection
       connDim1Local(2)=ESMF_GRIDCONN_NONE
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
       connDim2Local(1)=ESMF_GRIDCONN_NONE ! if not present then default to no connection
       connDim2Local(2)=ESMF_GRIDCONN_NONE
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
       connDim3Local(1)=ESMF_GRIDCONN_NONE ! if not present then default to no connection
       connDim3Local(2)=ESMF_GRIDCONN_NONE
    endif


    ! check for not implemented functionality
    if (connDim1Local(1) .ne. ESMF_GRIDCONN_NONE .or. &
        connDim1Local(2) .ne. ESMF_GRIDCONN_NONE) then
       call ESMF_LogMsgSetError(ESMF_RC_NOT_IMPL, & 
                 "- Only ESMF_GRIDCONN_NONE Grid connection implemented so far", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if (connDim2Local(1) .ne. ESMF_GRIDCONN_NONE .or. &
        connDim2Local(2) .ne. ESMF_GRIDCONN_NONE) then
       call ESMF_LogMsgSetError(ESMF_RC_NOT_IMPL, & 
                 "- Only ESMF_GRIDCONN_NONE Grid connection implemented so far", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if (connDim3Local(1) .ne. ESMF_GRIDCONN_NONE .or. &
        connDim3Local(2) .ne. ESMF_GRIDCONN_NONE) then
       call ESMF_LogMsgSetError(ESMF_RC_NOT_IMPL, & 
                 "- Only ESMF_GRIDCONN_NONE Grid connection implemented so far", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

   ! Make alterations to size due to GridEdgeWidths ----------------------------
    allocate(gridEdgeLWidthLocal(dimCount), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating gridEdgeLWidthLocal", &
                                     ESMF_CONTEXT, rc)) return
    allocate(gridEdgeUWidthLocal(dimCount), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating gridEdgeUWidthLocal", &
                                     ESMF_CONTEXT, rc)) return
    allocate(gridAlignLocal(dimCount), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating gridAlignLocal", &
                                     ESMF_CONTEXT, rc)) return

    call ESMF_GridLUADefault(dimCount, &
                             gridEdgeLWidth, gridEdgeUWidth, gridAlign, &
                             gridEdgeLWidthLocal, gridEdgeUWidthLocal, gridAlignLocal, &
                             rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


#if 0
    ! Modify lower bound
    do i=1,dimCount
       minIndexLocal(i)=minIndexLocal(i)-gridEdgeLWidthLocal(i)
    enddo


    ! Modify lower size
    countsPerDEDim1Local(1)=countsPerDEDim1Local(1)+gridEdgeLWidthLocal(1)

    countsPerDEDim2Local(1)=countsPerDEDim2Local(1)+gridEdgeLWidthLocal(2)
  
    if (dimCount .gt. 2) then
       countsPerDEDim3Local(1)=countsPerDEDim3Local(1)+gridEdgeLWidthLocal(3)
    endif


    ! Modify upper size
    top=size(countsPerDEDim1Local)
    countsPerDEDim1Local(top)=countsPerDEDim1Local(top)+gridEdgeUWidthLocal(1)

    top=size(countsPerDEDim2Local)
    countsPerDEDim2Local(top)=countsPerDEDim2Local(top)+gridEdgeUWidthLocal(2)
  
    if (dimCount .gt. 2) then
       top=size(countsPerDEDim3Local)
       countsPerDEDim3Local(top)=countsPerDEDim3Local(top)+gridEdgeUWidthLocal(3)
    endif
#endif


   ! Calc minIndex,maxIndex,distgridToGridMap for DistGrid -----------------------------------
    ! Set default for maxIndex 
    allocate(maxIndexLocal(dimCount), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating maxIndexLocal", &
                                     ESMF_CONTEXT, rc)) return

    maxIndexLocal(1)=sum(countsPerDEDim1Local)+minIndexLocal(1)-1
    maxIndexLocal(2)=sum(countsPerDEDim2Local)+minIndexLocal(2)-1

    if (dimCount .gt. 2) then
      maxIndexLocal(3)=sum(countsPerDEDim3Local)+minIndexLocal(3)-1
    endif

   allocate(distgridToGridMap(dimCount), stat=localrc)
   if (ESMF_LogMsgFoundAllocError(localrc, "Allocating distgridToGridMap", &
               ESMF_CONTEXT, rc)) return
   do i=1,dimCount
     distgridToGridMap(i)=i
   enddo    
          
  ! Setup deBlockList for DistGrid ------------------------------------------------
  ! count de blocks
  deCount=1
  deCount=deCount*size(countsPerDEDim1Local) 
  deCount=deCount*size(countsPerDEDim2Local)
  if (dimCount .gt. 2) then
     deCount=deCount*size(countsPerDEDim3Local)
  endif 
 
  ! Calc the max size of a DEDim
  maxSizeDEDim=1
  if (size(countsPerDEDim1Local) .gt. maxSizeDEDim) then
      maxSizeDEDim=size(countsPerDEDim1Local)
  endif
  if (size(countsPerDEDim2Local) .gt. maxSizeDEDim) then
      maxSizeDEDim=size(countsPerDEDim2Local)
  endif
  if (dimCount .gt. 2) then
      if (size(countsPerDEDim3Local) .gt. maxSizeDEDim) then
         maxSizeDEDim=size(countsPerDEDim3Local)
      endif
  endif
  

  ! generate deblocklist
  allocate(maxPerDEDim(dimCount,maxSizeDEDim), stat=localrc)
  if (ESMF_LogMsgFoundAllocError(localrc, "Allocating maxPerDEDim", &
              ESMF_CONTEXT, rc)) return
  allocate(minPerDEDim(dimCount,maxSizeDEDim), stat=localrc)
  if (ESMF_LogMsgFoundAllocError(localrc, "Allocating minPerDEDim", &
              ESMF_CONTEXT, rc)) return
 allocate(deDimCount(dimCount), stat=localrc)
  if (ESMF_LogMsgFoundAllocError(localrc, "Allocating maxPerDEDim", &
              ESMF_CONTEXT, rc)) return


  ! Calc the maximum end of each DE in a Dim, and the size of each DEDim
  d=1
  deDimCount(d)=size(countsPerDEDim1Local)
  minPerDeDim(d,1)=minIndexLocal(d)
  maxPerDeDim(d,1)=minIndexLocal(d)+countsPerDEDim1Local(1)-1
  do i=2,deDimCount(d) 
     minPerDEDim(d,i)=maxPerDEDim(d,i-1)+1
     maxPerDEDim(d,i)=minPerDEDim(d,i)+countsPerDEDim1Local(i)-1
  enddo

  d=2
  deDimCount(d)=size(countsPerDEDim2Local)
  minPerDeDim(d,1)=minIndexLocal(d)
  maxPerDeDim(d,1)=minIndexLocal(d)+countsPerDEDim2Local(1)-1
  do i=2,deDimCount(d) 
     minPerDEDim(d,i)=maxPerDEDim(d,i-1)+1
     maxPerDEDim(d,i)=minPerDEDim(d,i)+countsPerDEDim2Local(i)-1
  enddo

  if (dimCount .gt. 2) then
    d=3
    deDimCount(d)=size(countsPerDEDim3Local)
    minPerDeDim(d,1)=minIndexLocal(d)
    maxPerDeDim(d,1)=minIndexLocal(d)+countsPerDEDim3Local(1)-1
    do i=2,deDimCount(d) 
       minPerDEDim(d,i)=maxPerDEDim(d,i-1)+1
       maxPerDEDim(d,i)=minPerDEDim(d,i)+countsPerDEDim3Local(i)-1
    enddo
  endif

  ! allocate deblocklist
  allocate(deBlockList(dimCount,2,deCount), stat=localrc)
  if (ESMF_LogMsgFoundAllocError(localrc, "Allocating deBlockList", &
              ESMF_CONTEXT, rc)) return

  ! Fill in DeBlockList
  if (dimCount .eq. 2) then
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
  else if (dimCount .eq. 3) then
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
      if (dimCount .gt. 2) then
	 k=1
     	 do i3=1,size(countsPerDEDim3Local)
         do i2=1,size(countsPerDEDim2Local)
         do i1=1,size(countsPerDEDim1Local)
            petList(k)=petMap(i1,i2,i3)
            k=k+1
         enddo
         enddo
         enddo
      else 
	 k=1
     	 do i3=1,1
         do i2=1,size(countsPerDEDim2Local)
         do i1=1,size(countsPerDEDim1Local)
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
    distgrid=ESMF_DistGridCreate(minIndex=minIndexLocal, maxIndex=maxIndexLocal, &
               deBlockList=deBlockList, delayout=delayout, indexflag=indexflag, rc=localrc)   
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


   ! Convert coordDeps to coordDimCount and coordDimMap -------------------------------
   allocate(coordDimCount(dimCount), stat=localrc)
   if (ESMF_LogMsgFoundAllocError(localrc, "Allocating coordDimCount", &
              ESMF_CONTEXT, rc)) return
   allocate(coordDimMap(dimCount,dimCount), stat=localrc)
   if (ESMF_LogMsgFoundAllocError(localrc, "Allocating coordDimMap", &
              ESMF_CONTEXT, rc)) return

   if (present(coordDep1)) then
      coordDimCount(1)=size(coordDep1)
      coordDimMap(1,:)=0
      do i=1,size(coordDep1)
         coordDimMap(1,i)=coordDep1(i)
      enddo
   else 
      coordDimCount(1)=dimCount
      do i=1,dimCount
         coordDimMap(1,i)=i      
      enddo
   endif

   if (present(coordDep2)) then
      coordDimCount(2)=size(coordDep2)
      coordDimMap(2,:)=0
      do i=1,size(coordDep2)
         coordDimMap(2,i)=coordDep2(i)
      enddo
   else 
      coordDimCount(2)=dimCount
      do i=1,dimCount
         coordDimMap(2,i)=i      
      enddo
   endif

   if (dimCount .gt. 2) then
      if (present(coordDep3)) then 
         coordDimCount(3)=size(coordDep3)
          coordDimMap(3,:)=0
          do i=1,size(coordDep3)
             coordDimMap(3,i)=coordDep3(i)
          enddo
      else 
        coordDimCount(3)=dimCount
        do i=1,dimCount
	   coordDimMap(3,i)=i      
        enddo
      endif
   endif

   ! Create Grid from specification -----------------------------------------------
   call ESMF_GridSetFromDistGrid(grid, name, coordTypeKind, &
                                    distgrid=distgrid, distgridToGridMap=distgridToGridMap, &
                                    coordDimCount=coordDimCount, coordDimMap=coordDimMap, &
                                    gridEdgeLWidth=gridEdgeLWidthLocal, &
                                    gridEdgeUWidth=gridEdgeUWidthLocal, &
                                    gridAlign=gridAlignLocal, &
                                    gridMemLBound=gridMemLBound, &
                                    indexflag=indexflag, &
                                    destroyDistGrid=.true., &
                                    destroyDELayout=.true., &
                                    rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


    ! Commit Grid -----------------------------------------------------------------
    call ESMF_GridCommit(grid, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


    ! Clean up memory
    deallocate(maxIndexLocal)
    deallocate(minIndexLocal)
    deallocate(coordDimCount)
    deallocate(coordDimMap)
    deallocate(distgridToGridMap)
    deallocate(maxPerDEDim)
    deallocate(minPerDEDim)
    deallocate(deDimCount)
    deallocate(deBlockList)
    deallocate(gridEdgeLWidthLocal)
    deallocate(gridEdgeUWidthLocal)
    deallocate(gridAlignLocal)
    deallocate(countsPerDEDim1Local) 
    deallocate(countsPerDEDim2Local) 
    if (dimCount .gt. 2) then
       deallocate(countsPerDEDim3Local) 
    endif


    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    end subroutine ESMF_GridSetCmmitShapeTileIrreg



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetCmmitShapeTileReg"
!BOP
! !IROUTINE: ESMF_GridSetCommitShapeTile - Set and complete a Grid with a regular distribution

! !INTERFACE:
  ! Private name; call using ESMF_GridSetCommitShapeTile()
      subroutine ESMF_GridSetCmmitShapeTileReg(grid, name, coordTypeKind, &
                        regDecomp, decompFlag, minIndex, maxIndex, &
                        connDim1, connDim2, connDim3, &
                        poleStaggerLoc1, poleStaggerLoc2, poleStaggerLoc3, &
                        bipolePos1, bipolePos2, bipolePos3, &
                        coordDep1, coordDep2, coordDep3, &
                        gridEdgeLWidth, gridEdgeUWidth, gridAlign, &
                        gridMemLBound, indexflag, petMap, rc)

!
! !ARGUMENTS:
       type(ESMF_Grid),       intent(inout)              :: grid
       character (len=*),     intent(in),   optional  :: name 
       type(ESMF_TypeKind),   intent(in),   optional  :: coordTypeKind
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
       integer,               intent(in),   optional  :: gridEdgeLWidth(:)
       integer,               intent(in),   optional  :: gridEdgeUWidth(:)
       integer,               intent(in),   optional  :: gridAlign(:)
       integer,               intent(in),   optional  :: gridMemLBound(:)
       type(ESMF_IndexFlag),  intent(in),   optional  :: indexflag
       integer,               intent(in),   optional  :: petMap(:,:,:)
       integer,               intent(out),  optional  :: rc
!
! !DESCRIPTION:
!
! This method sets information into an empty Grid and then commits it to 
! create a single tile, regularly distributed grid 
! (see Figure \ref{fig:GridDecomps}).
! To specify the distribution, the user passes in an array 
! ({\tt regDecomp}) specifying the number of DEs to divide each 
! dimension into. If the number of DEs is 1 than the dimension is undistributed.
! The array {\tt decompFlag} indicates how the division into DEs is to
! occur.  The default is to divide the range as evenly as possible.
!
! For consistency's sake the {\tt ESMF\_GridSetCommitShapeTile()} call
! should be executed in the same set or a subset of the PETs in which the
! {\tt ESMF\_GridCreateEmpty()} call was made. If the call
! is made in a subset, the Grid objects outside that subset will
! still be "empty" and not usable. 
!
! The arguments are:
! \begin{description}
! \item[{grid}]
!     {\tt ESMF\_Grid} to set information into and then commit.  
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
!      is {\tt ESMF\_DECOMP\_HOMOGEN} in all dimensions. Please see
!      Section~\ref{opt:decompflag} for a full description of the 
!      possible options. 
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
!     arrays map to. If not present the default is 1,2,...,grid rank.  
! \item[{[coordDep2]}] 
!     This array specifies the dependence of the second 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the second
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is 1,2,...,grid rank.  
! \item[{[coordDep3]}] 
!     This array specifies the dependence of the third 
!     coordinate component on the three index dimensions
!     described by {\tt coordsPerDEDim1,2,3}. The size of the 
!     array specifies the number of dimensions of the third
!     coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. If not present the default is 1,2,...,grid rank.  
! \item[{[gridEdgeLWidth]}] 
!      The padding around the lower edges of the grid. This padding is between
!      the index space corresponding to the cells and the boundary of the 
!      the exclusive region. This extra space is to contain the extra
!      padding for non-center stagger locations, and should be big enough
!      to hold any stagger in the grid. 
! \item[{[gridEdgeUWidth]}] 
!      The padding around the upper edges of the grid. This padding is between
!      the index space corresponding to the cells and the boundary of the 
!      the exclusive region. This extra space is to contain the extra
!      padding for non-center stagger locations, and should be big enough
!      to hold any stagger in the grid. 
! \item[{[gridAlign]}] 
!     Specification of how the stagger locations should align with the cell
!     index space (can be overridden by the individual staggerAligns). If
!     the {\tt gridEdgeWidths} are not specified than this parameter
!     implies the EdgeWidths.
! \item[{[gridMemLBound]}] 
!      Specifies the lower index range of the memory of every DE in this Grid. 
!      Only used when indexflag is {\tt ESMF\_INDEX\_USER}. May be overridden
!      by staggerMemLBound. 
! \item[{[indexflag]}]
!      Indicates the indexing scheme to be used in the new Grid. Please see
!      Section~\ref{opt:indexflag} for the list of options. If not present,
!      defaults to ESMF\_INDEX\_DELOCAL.
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
    integer, pointer     :: undistLBound(:)
    integer, pointer     :: undistUBound(:)
    integer, pointer     :: coordDimCount(:)
    integer, pointer     :: coordDimMap(:,:)
    integer              :: localrc
    integer              :: dimCount,i,maxSizeDEDim
    integer, pointer     :: regDecompDG(:)
    type(ESMF_DecompFlag), pointer :: decompflagDG(:)
    integer, pointer     :: regDecompLocal(:)
    type(ESMF_DecompFlag), pointer :: decompflagLocal(:)
    integer, pointer     :: distgridToGridMap(:), deDimCount(:)
    integer, pointer     :: minIndexLocal(:), maxIndexLocal(:)
    integer, pointer     :: gridEdgeLWidthLocal(:)
    integer, pointer     :: gridEdgeUWidthLocal(:)
    integer, pointer     :: gridAlignLocal(:)
    integer              :: deCount
    integer              :: d,i1,i2,i3,k
    type(ESMF_GridConn)  :: connDim1Local(2)
    type(ESMF_GridConn)  :: connDim2Local(2)
    type(ESMF_GridConn)  :: connDim3Local(2)
    integer              :: connCount, petListCount


    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Compute the Grid DimCount and Derivatives ---------------------------------------------------
    ! dimCount
    dimCount=size(maxIndex)
    if ((dimCount < 2) .or. (dimCount > 3)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- maxIndex size and thus Grid dimCount must be either 2 or 3 when using create shape ", & 
               ESMF_CONTEXT, rc) 
         return 
    endif

    ! Argument Consistency Checking --------------------------------------------------------------
    if (present(regDecomp)) then
        if (size(regDecomp) .lt. dimCount) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
                    "- regDecomp size doesn't match Grid dimCount ", & 
                    ESMF_CONTEXT, rc) 
            return 
        endif
    endif

    if (present(decompFlag)) then
        if (size(decompFlag) .lt. dimCount) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
                    "- decompFlag size doesn't match Grid dimCount ", & 
                    ESMF_CONTEXT, rc) 
            return 
        endif
    endif

    if ((dimCount .lt. 3) .and. present(connDim3)) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- connDim3 not allowed when grid is less than dimCount 3", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if ((dimCount .lt. 3) .and. present(poleStaggerLoc3)) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- poleStaggerLoc3 not allowed when grid is less than dimCount 3", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if ((dimCount .lt. 3) .and. present(bipolePos3)) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- bipolePos3 not allowed when grid is less than dimCount 3", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif


    if ((dimCount .lt. 3) .and. present(coordDep3)) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- coordDep3 not allowed when grid is less than dimCount 3", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if (present(coordDep1)) then
       if ((size(coordDep1) < 1) .or. (size(coordDep1)>dimCount)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- coordDep1 size incompatible with grid dimCount", & 
               ESMF_CONTEXT, rc) 
          return 
       endif
    endif

    if (present(coordDep2)) then
       if ((size(coordDep2) < 1) .or. (size(coordDep2)>dimCount)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- coordDep2 size incompatible with grid dimCount", & 
               ESMF_CONTEXT, rc) 
          return 
       endif
    endif

    if (present(coordDep3)) then
       if ((size(coordDep3) < 1) .or. (size(coordDep3)>dimCount)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- coordDep3 size incompatible with grid dimCount", & 
               ESMF_CONTEXT, rc) 
          return 
       endif
    endif

    if (present(minIndex)) then
       if (size(minIndex) .ne. dimCount) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- minIndex size must equal grid dimCount", & 
               ESMF_CONTEXT, rc) 
          return 
       endif
    endif



    ! Check DimCount of gridWidths and Aligns
    if (present(gridEdgeLWidth)) then
        if (size(gridEdgeLWidth) .ne. dimCount) then
           call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
                     "- gridEdgeLWidth must be of size equal to Grid dimCount", & 
                     ESMF_CONTEXT, rc) 
              return
        endif 
    endif

    if (present(gridEdgeUWidth)) then
        if (size(gridEdgeUWidth) .ne. dimCount) then
           call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
                     "- gridEdgeUWidth must be of size equal to Grid dimCount", & 
                     ESMF_CONTEXT, rc) 
              return
        endif 
    endif

    if (present(gridAlign)) then
        if (size(gridAlign) .ne. dimCount) then
           call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
                     "- gridAlign must be of size equal to Grid dimCount", & 
                     ESMF_CONTEXT, rc) 
              return
        endif 
    endif


   ! make sure connected dimensions don't have an edge width
   if (present(connDim1)) then
      if (size(connDim1) .eq. 1) then
         if (connDim1(1) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeLWidth)) then
               if (gridEdgeLWidth(1) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have LWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
            if (present(gridEdgeUWidth)) then
               if (gridEdgeUWidth(1) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have UWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
      else if (size(connDim1) .eq. 2) then
         if (connDim1(1) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeLWidth)) then
               if (gridEdgeLWidth(1) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have LWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
         if (connDim1(2) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeUWidth)) then
               if (gridEdgeUWidth(1) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have UWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
      endif
   endif

   ! make sure connected dimensions don't have an edge width
   if (present(connDim2)) then
      if (size(connDim2) .eq. 1) then
         if (connDim2(1) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeLWidth)) then
               if (gridEdgeLWidth(2) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have LWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
            if (present(gridEdgeUWidth)) then
               if (gridEdgeUWidth(2) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have UWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
      else if (size(connDim2) .eq. 2) then
         if (connDim2(1) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeLWidth)) then
               if (gridEdgeLWidth(2) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have LWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
         if (connDim2(2) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeUWidth)) then
               if (gridEdgeUWidth(2) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have UWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
      endif
   endif


   ! make sure connected dimensions don't have an edge width
   if (present(connDim3)) then
      if (size(connDim3) .eq. 1) then
         if (connDim3(1) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeLWidth)) then
               if (gridEdgeLWidth(3) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have LWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
            if (present(gridEdgeUWidth)) then
               if (gridEdgeUWidth(3) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have UWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
      else if (size(connDim3) .eq. 2) then
         if (connDim3(1) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeLWidth)) then
               if (gridEdgeLWidth(3) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have LWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
         if (connDim3(2) .ne. ESMF_GRIDCONN_NONE) then
            if (present(gridEdgeUWidth)) then
               if (gridEdgeUWidth(3) .gt. 0) then
                   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                     "- Connected dimensions must have UWidth 0", & 
                 ESMF_CONTEXT, rc) 
		return
               endif
            endif
         endif
      endif
   endif

   ! check for gridMemLBound issues
   if (present(gridMemLBound)) then
      if (.not. present(indexflag)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                "- when using gridMemLBound must specify indexflag=ESMF_INDEX_USER ", & 
                 ESMF_CONTEXT, rc)  
              return
      else if (.not.(indexflag .eq. ESMF_INDEX_USER)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                "- when using gridMemLBound must specify indexflag=ESMF_INDEX_USER ", & 
                 ESMF_CONTEXT, rc) 
              return
      endif
   else
      if (present(indexflag)) then
         if (indexflag .eq. ESMF_INDEX_USER) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                "- when using indexflag=ESMF_INDEX_USER must provide gridMemLBound ", & 
                   ESMF_CONTEXT, rc) 
              return
         endif
      endif
   endif


   ! Check for non-valid connection types here

   !TODO: Consider making some of these a separate local subroutine (particularly if you're going to 
   !      have 3 of these ShapeCreate subroutines with only minor changes


    ! Set Defaults ------------------------------------------------------------------

    ! Set default for minIndex
    allocate(minIndexLocal(dimCount), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating minIndexLocal", &
                                     ESMF_CONTEXT, rc)) return

    if (present(minIndex)) then
       minIndexLocal(:)=minIndex(:)
    else
       do i=1,dimCount
          minIndexLocal(i)=1
       enddo
    endif


    ! Set default for maxIndex
    allocate(maxIndexLocal(dimCount), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating maxIndexLocal", &
                                     ESMF_CONTEXT, rc)) return
    maxIndexLocal(:)=maxIndex(:)


    ! Set default for regDecomp 
    allocate(regDecompLocal(dimCount), stat=localrc)
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
       do i=2,dimCount
          regDecompLocal(i)=1
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
       connDim1Local(1)=ESMF_GRIDCONN_NONE ! if not present then default to no connection
       connDim1Local(2)=ESMF_GRIDCONN_NONE
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
       connDim2Local(1)=ESMF_GRIDCONN_NONE ! if not present then default to no connection
       connDim2Local(2)=ESMF_GRIDCONN_NONE
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
       connDim3Local(1)=ESMF_GRIDCONN_NONE ! if not present then default to no connection
       connDim3Local(2)=ESMF_GRIDCONN_NONE
    endif



    ! check for not implemented functionality
    if (connDim1Local(1) .ne. ESMF_GRIDCONN_NONE .or. &
        connDim1Local(2) .ne. ESMF_GRIDCONN_NONE) then
       call ESMF_LogMsgSetError(ESMF_RC_NOT_IMPL, & 
                 "- Only ESMF_GRIDCONN_NONE Grid connection implemented so far", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if (connDim2Local(1) .ne. ESMF_GRIDCONN_NONE .or. &
        connDim2Local(2) .ne. ESMF_GRIDCONN_NONE) then
       call ESMF_LogMsgSetError(ESMF_RC_NOT_IMPL, & 
                 "- Only ESMF_GRIDCONN_NONE Grid connection implemented so far", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if (connDim3Local(1) .ne. ESMF_GRIDCONN_NONE .or. &
        connDim3Local(2) .ne. ESMF_GRIDCONN_NONE) then
       call ESMF_LogMsgSetError(ESMF_RC_NOT_IMPL, & 
                 "- Only ESMF_GRIDCONN_NONE Grid connection implemented so far", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif


  if (present(petMap)) then
     if (dimCount .gt. 2) then
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

   ! Modify Bounds by GridEdgeUWidth and GridEdgeLWidth  -------------------------
   ! setup maxIndexLocal to hold modified bounds
    allocate(gridEdgeLWidthLocal(dimCount), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating gridEdgeLWidthLocal", &
                                     ESMF_CONTEXT, rc)) return
    allocate(gridEdgeUWidthLocal(dimCount), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating gridEdgeUWidthLocal", &
                                     ESMF_CONTEXT, rc)) return
    allocate(gridAlignLocal(dimCount), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating gridAlignLocal", &
                                     ESMF_CONTEXT, rc)) return

    call ESMF_GridLUADefault(dimCount, &
                             gridEdgeLWidth, gridEdgeUWidth, gridAlign, &
                             gridEdgeLWidthLocal, gridEdgeUWidthLocal, gridAlignLocal, &
                             rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

#if 0
    ! Modify lower bound
    do i=1,dimCount
       minIndexLocal(i)=minIndexLocal(i)-gridEdgeLWidthLocal(i)
    enddo

    ! Modify upper bound
    do i=1,dimCount
       maxIndexLocal(i)=maxIndexLocal(i)+gridEdgeUWidthLocal(i)
    enddo
#endif


   ! Set default for decomp flag based on gridEdgeWidths -----------------------------------
   ! NOTE: This is a temporary fix until we have something better implemented in distGrid

    ! Set default for decompFlag 
    allocate(decompFlagLocal(dimCount), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating decompFlagLocal", &
                                     ESMF_CONTEXT, rc)) return

    if (present(decompFlag)) then
        decompFlagLocal(:)=decompFlag(:)
    else
        decompFlagLocal(:)=ESMF_DECOMP_HOMOGEN
    endif


   allocate(distgridToGridMap(dimCount), stat=localrc)
   if (ESMF_LogMsgFoundAllocError(localrc, "Allocating distgridToGridMap", &
               ESMF_CONTEXT, rc)) return          
   do i=1,dimCount
     distgridToGridMap(i)=i
   enddo    


   ! Setup Connections between patch sides ----------------------------------------

   ! CONNECTIONS DON'T WORK YET SO NOT IMPLEMENTED


   ! Process PetMap --------------------------------------------------------------
   !! Calculate deCount
   deCount=1
   do i=1,dimCount
      deCount=deCount*regDecompLocal(i)
   enddo

   ! create DELayout based on presence of petMap
   if (present(petMap)) then
      !! Allocate petList
      allocate(petList(deCount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating petList", &
              ESMF_CONTEXT, rc)) return


      !! copy petMap to petList
      if (dimCount .gt. 2) then
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
    distgrid=ESMF_DistGridCreate(minIndex=minIndexLocal, maxIndex=maxIndexLocal, &
              regDecomp=regDecompLocal, decompFlag=decompFlagLocal, delayout=delayout,&
              indexflag=indexflag, &
#if 0
              regDecompFirstExtra=gridEdgeLWidthLocal, &
              regDecompLastExtra=gridEdgeUWidthLocal, &
#endif
              rc=localrc)   
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return



   ! Convert coordDeps to coordDimCount and coordDimMap -------------------------------
   allocate(coordDimCount(dimCount), stat=localrc)
   if (ESMF_LogMsgFoundAllocError(localrc, "Allocating coordDimCount", &
              ESMF_CONTEXT, rc)) return
   allocate(coordDimMap(dimCount,dimCount), stat=localrc)
   if (ESMF_LogMsgFoundAllocError(localrc, "Allocating coordDimMap", &
              ESMF_CONTEXT, rc)) return

   if (present(coordDep1)) then
      coordDimCount(1)=size(coordDep1)
      coordDimMap(1,:)=0
      do i=1,size(coordDep1)
         coordDimMap(1,i)=coordDep1(i)
      enddo
   else 
      coordDimCount(1)=dimCount
      do i=1,dimCount
         coordDimMap(1,i)=i      
      enddo
   endif

   if (present(coordDep2)) then
      coordDimCount(2)=size(coordDep2)
      coordDimMap(2,:)=0
      do i=1,size(coordDep2)
         coordDimMap(2,i)=coordDep2(i)
      enddo
   else 
      coordDimCount(2)=dimCount
      do i=1,dimCount
         coordDimMap(2,i)=i      
      enddo
   endif

   if (dimCount .gt. 2) then
      if (present(coordDep3)) then 
         coordDimCount(3)=size(coordDep3)
          coordDimMap(3,:)=0
          do i=1,size(coordDep3)
             coordDimMap(3,i)=coordDep3(i)
          enddo
      else 
        coordDimCount(3)=dimCount
        do i=1,dimCount
	   coordDimMap(3,i)=i      
        enddo
      endif
   endif

  
   ! Create Grid from specification -----------------------------------------------
   call ESMF_GridSetFromDistGrid(grid, name=name, coordTypeKind=coordTypeKind, &
                                    distgrid=distgrid, distgridToGridMap=distgridToGridMap, &
                                    coordDimCount=coordDimCount, coordDimMap=coordDimMap, &
                                    gridEdgeLWidth=gridEdgeLWidthLocal, &
                                    gridEdgeUWidth=gridEdgeUWidthLocal, &
                                    gridAlign=gridAlignLocal, &
                                    gridMemLBound=gridMemLBound, &
                                    indexflag=indexflag, &
                                    rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


    ! Commit Grid -----------------------------------------------------------------
    call ESMF_GridCommit(grid, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


    ! Clean up memory
    deallocate(regDecompLocal)
    deallocate(decompFlagLocal)
    deallocate(coordDimCount)
    deallocate(coordDimMap)
    deallocate(minIndexLocal)
    deallocate(maxIndexLocal)
    deallocate(distgridToGridMap)
    deallocate(gridEdgeLWidthLocal)
    deallocate(gridEdgeUWidthLocal)
    deallocate(gridAlignLocal)

 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    end subroutine ESMF_GridSetCmmitShapeTileReg

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetCmmitShapeTileArb"
!BOP
! !IROUTINE: ESMF_GridSetCommitShapeTile - Create a Grid with an arbitrary distribution

! !INTERFACE:
  ! Private name; call using ESMF_GridSetCommitShapeTile()
      subroutine ESMF_GridSetCmmitShapeTileArb(grid, name,coordTypeKind, &
			      minIndex, maxIndex, localArbIndex, localArbIndexCount, &
                        connDim1, connDim2, connDim3, &
                        poleStaggerLoc1, poleStaggerLoc2, poleStaggerLoc3, &
                        bipolePos1, bipolePos2, bipolePos3, &
                        coordDep1, coordDep2, coordDep3, &
                        distDim, rc)
!
! !ARGUMENTS:

       type(ESMF_Grid),       intent(inout)           :: grid
       character (len=*),     intent(in),   optional  :: name 
       type(ESMF_TypeKind),   intent(in),   optional  :: coordTypeKind
       integer,               intent(in),   optional  :: minIndex(:)
       integer,               intent(in)              :: maxIndex(:)
       integer,               intent(in)              :: localArbIndex(:,:)
       integer,               intent(in)   	      :: localArbIndexCount
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
       integer,               intent(in),   optional  :: distDim(:)
       integer,               intent(out),  optional  :: rc
!
! !DESCRIPTION:
!
! This method set an empty grid as a single tile, arbitrarily distributed grid 
! (see Figure \ref{fig:GridDecomps}).
! To specify the arbitrary distribution, the user passes in an 2D array 
! of local indices, where the first dimension is the number of local grid cells
! specified by localArbIndexCount and the second dimension is the number of distributed
! dimensions.
!
! {\tt distDim} specifies which grid dimensions are arbitrarily distributed. The 
! size of {\tt distDim} has to agree with the size of the second dimension of 
! {\tt localArbIndex}. 
!
! For consistency's sake the {\tt ESMF\_GridSetCommitShapeTile()} call
! should be executed in the same set or a subset of the PETs in which the
! {\tt ESMF\_GridCreateEmpty()} call was made. If the call
! is made in a subset, the Grid objects outside that subset will
! still be "empty" and not usable. 
!
! The arguments are:
! \begin{description}
! \item[{[grid]}]
!     The empty {\tt ESMF\_Grid} to set information into and then commit.
! \item[{[name]}]
!          {\tt ESMF\_Grid} name.
! \item[{[coordTypeKind]}] 
!     The type/kind of the grid coordinate data. 
!     If not specified then the type/kind will be 8 byte reals. 
! \item[{[minIndex]}] 
!      Tuple to start the index ranges at. If not present, defaults
!      to /1,1,1,.../.
! \item[{[maxIndex]}] 
!      The upper extend of the grid index ranges.
! \item[{[localArbIndex]}] 
!      This 2D array specifies the indices of the local grid cells.  The 
!      dimensions should be localArbIndexCount * number of Distributed grid dimensions
!      where localArbIndexCount is the input argument specified below
! \item[{localArbIndexCount}] 
!      number of grid cells in the local DE
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
!     The size of the array specifies the number of dimensions of the 
!     first coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. The format should be /ESMF\_GRID\_ARBDIM/ where
!     /ESMF\_GRID\_ARBDIM/ is mapped to the collapsed 1D dimension from all
!     the arbitrarily distributed dimensions.  n is the dimension that 
!     is not distributed (if exists).  
!     If not present the default is /ESMF\_GRID\_ARBDIM/ if the first dimension
!     is arbitararily distributed, or /n/ if not distributed (i.e. n=1)
! \item[{[coordDep2]}] 
!     The size of the array specifies the number of dimensions of the 
!     second coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. The format should be /ESMF\_GRID\_ARBDIM/ where
!     /ESMF\_GRID\_ARBDIM/ is mapped to the collapsed 1D dimension from all
!     the arbitrarily distributed dimensions.  n is the dimension that 
!     is not distributed (if exists).  
!     If not present the default is /ESMF\_GRID\_ARBDIM/ if this dimension
!     is arbitararily distributed, or /n/ if not distributed (i.e. n=2)
! \item[{[coordDep3]}] 
!     The size of the array specifies the number of dimensions of the 
!     third coordinate component array. The values specify which
!     of the index dimensions the corresponding coordinate
!     arrays map to. The format should be /ESMF\_GRID\_ARBDIM/ where
!     /ESMF\_GRID\_ARBDIM/ is mapped to the collapsed 1D dimension from all
!     the arbitrarily distributed dimensions.  n is the dimension that 
!     is not distributed (if exists).  
!     If not present the default is /ESMF\_GRID\_ARBDIM/ if this dimension
!     is arbitararily distributed, or /n/ if not distributed (i.e. n=3)
! \item[{[distDim]}]
!       This array specifies which dimensions are arbitrarily distributed.
!       The size of the array specifies the total distributed dimensions.
!       if not specified, defaults is all dimensions will be arbitrarily
!       distributed.  The size has to agree with the size of the second
!       dimension of {\tt localArbIndex}.
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
    type(ESMF_DistGrid)  :: distgrid
    type(ESMF_DELayout)  :: delayout
    integer, pointer     :: petList(:)
    integer, pointer     :: undistLBound(:)
    integer, pointer     :: undistUBound(:)
    integer, pointer     :: coordDimCount(:)
    integer, pointer     :: coordDimMap(:,:)
    integer              :: localrc
    integer              :: dimCount,distDimCount,undistDimCount
    integer, pointer     :: deDimCount(:)
    integer, pointer     :: minIndexLocal(:)
    integer, pointer     :: maxIndexLocal(:)
    integer              :: i,j,d,f,i1,i2,i3,k,ind,ud
    type(ESMF_GridConn)  :: connDim1Local(2)
    type(ESMF_GridConn)  :: connDim2Local(2)
    type(ESMF_GridConn)  :: connDim3Local(2)
    integer              :: connCount, petListCount 
    integer              :: top
    integer, pointer     :: distSize(:)
    integer, pointer     :: distDimLocal(:)
    logical, pointer     :: isDist(:)
    integer, pointer     :: local1DIndices(:)
    logical              :: found

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Compute the Grid DimCount and Derivatives ---------------------------------------------------
    ! dimCount
    dimCount=size(maxIndex)
    if ((dimCount < 2) .or. (dimCount > 3)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- maxIndex size and thus Grid dimCount must be either 2 or 3 when using create shape ", & 
               ESMF_CONTEXT, rc) 
         return 
    endif

    ! number of distributed dimension, distDimCount, is determined by the second dim of 
    ! localArbIndex
    distDimCount = size(localArbIndex,2)
    if (distDimCount > dimCount) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
               "- the second dim of localArbIndex must be equal or less than grid dimension", & 
               ESMF_CONTEXT, rc) 
         return 
    endif

    allocate(distDimLocal(distDimCount), stat=localrc)
    allocate(isDist(dimCount), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating distDimLocal or isDist", &
                                     ESMF_CONTEXT, rc)) return

    isDist(:)=.false.
    ! check distribution info
    if (present(distDim)) then
       if (size(distDim) .ne. distDimCount) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
                 "- distDim must match with the second dimension of localArbIndex", & 
                 ESMF_CONTEXT, rc) 
            return 
       endif
       distDimLocal(:)=distDim(:)
       do i=1,distDimCount
	  isDist(distDimLocal(i))=.true.
       enddo
    else
       do i=1,distDimCount
         distDimLocal(i)=i
       enddo
       isDist(1:distDimCount)=.true.
    endif

    ! Argument Consistency Checking --------------------------------------------------------------
    if ((dimCount .lt. 3) .and. present(connDim3)) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- connDim3 not allowed when grid is less than dimCount 3", & 
                 ESMF_CONTEXT, rc) 
       return
    endif

    if ((dimCount .lt. 3) .and. present(poleStaggerLoc3)) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- poleStaggerLoc3 not allowed when grid is less than dimCount 3", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if ((dimCount .lt. 3) .and. present(bipolePos3)) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- bipolePos3 not allowed when grid is less than dimCount 3", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if (present(minIndex)) then
       if (size(minIndex) .ne. dimCount) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, &
               "- minIndex size must equal grid dimCount", & 
               ESMF_CONTEXT, rc) 
          return 
       endif
    endif

 
 
   ! Check for non-valid connection types here

    ! Set Defaults -------------------------------------------------------------
    ! Set default for minIndex 
    allocate(minIndexLocal(dimCount), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating minIndexLocal", &
                                     ESMF_CONTEXT, rc)) return

    if (present(minIndex)) then
       minIndexLocal(:)=minIndex(:)
    else
       do i=1,dimCount
          minIndexLocal(i)=1
       enddo
    endif

    ! Set default for maxIndex
    allocate(maxIndexLocal(dimCount), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating maxIndexLocal", &
                                     ESMF_CONTEXT, rc)) return
    maxIndexLocal(:)=maxIndex(:)

    allocate(distSize(distDimCount),stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating distSize", &
                                     ESMF_CONTEXT, rc)) return

    do i=1,distDimCount   
       ind = distDimLocal(i)
       distSize(i)=maxIndexLocal(ind)-minIndexLocal(ind)+1
    enddo

    ! dimCounts of the undistributed part of the grid
    undistDimCount=dimCount-distDimCount

    ! can't have all undistributed dimensions
    if (distDimCount .eq. 0) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- Need to have at least one distributed dimension", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    ! Check localArbIndex dimension matched with localArbIndexCount and diskDimCount
    if (size(localArbIndex, 1) .ne. localArbIndexCount) then
       call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- localArbIndex 1st dimension has to match with localArbIndexCount", & 
                 ESMF_CONTEXT, rc) 
       return
    endif

    allocate(local1DIndices(localArbIndexCount), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating local1DIndices", &
                                  ESMF_CONTEXT, rc)) return

    ! convert localArbIndex into 1D index array for DistGrid
    if (localArbIndexCount .gt. 0) then
       do i = 1, localArbIndexCount
          local1DIndices(i) = localArbIndex(i,1)-1
	  if (distDimCount .ge. 2) then 
	     do j = 2, distDimCount
	        local1DIndices(i) = local1DIndices(i)*distSize(j) + localArbIndex(i,j)-1
	     enddo
	  endif
          local1DIndices(i) = local1DIndices(i)+1
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
       connDim1Local(1)=ESMF_GRIDCONN_NONE ! if not present then default to no connection
       connDim1Local(2)=ESMF_GRIDCONN_NONE
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
       connDim2Local(1)=ESMF_GRIDCONN_NONE ! if not present then default to no connection
       connDim2Local(2)=ESMF_GRIDCONN_NONE
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
       connDim3Local(1)=ESMF_GRIDCONN_NONE ! if not present then default to no connection
       connDim3Local(2)=ESMF_GRIDCONN_NONE
    endif


    ! check for not implemented functionality
    if (connDim1Local(1) .ne. ESMF_GRIDCONN_NONE .or. &
        connDim1Local(2) .ne. ESMF_GRIDCONN_NONE) then
       call ESMF_LogMsgSetError(ESMF_RC_NOT_IMPL, & 
                 "- Only ESMF_GRIDCONN_NONE Grid connection implemented so far", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if (connDim2Local(1) .ne. ESMF_GRIDCONN_NONE .or. &
        connDim2Local(2) .ne. ESMF_GRIDCONN_NONE) then
       call ESMF_LogMsgSetError(ESMF_RC_NOT_IMPL, & 
                 "- Only ESMF_GRIDCONN_NONE Grid connection implemented so far", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

    if (connDim3Local(1) .ne. ESMF_GRIDCONN_NONE .or. &
        connDim3Local(2) .ne. ESMF_GRIDCONN_NONE) then
       call ESMF_LogMsgSetError(ESMF_RC_NOT_IMPL, & 
                 "- Only ESMF_GRIDCONN_NONE Grid connection implemented so far", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif

   ! Setup Connections between patch sides ----------------------------------------

   ! CONNECTIONS DON'T WORK YET SO NOT IMPLEMENTED

   ! Convert coordDeps to coordDimCount and coordDimMap -------------------------------
   allocate(coordDimCount(dimCount), stat=localrc)
   if (ESMF_LogMsgFoundAllocError(localrc, "Allocating coordDimCount", &
              ESMF_CONTEXT, rc)) return
   allocate(coordDimMap(dimCount,dimCount), stat=localrc)
   if (ESMF_LogMsgFoundAllocError(localrc, "Allocating coordDimMap", &
              ESMF_CONTEXT, rc)) return

   if (present(coordDep1)) then
      ! error checking, if this dimension is arbitrary, one of the 
      ! coordinate dimension has to be be ESMF_GRID_ARBDIM
      if (isDist(1)) then
	found = .false.
	do i=1,size(coordDep1)
	  if (coordDep1(i) .eq. ESMF_GRID_ARBDIM) found = .true.
        enddo
	if (.not. found) then
           call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- coordDep1 does not contain ESMF_GRID_ARBDIM", & 
                 ESMF_CONTEXT, rc) 
	    return
        endif
      endif	
      coordDimCount(1)=size(coordDep1)
      coordDimMap(1,:)=0
      do i=1,size(coordDep1)
         coordDimMap(1,i)=coordDep1(i)
      enddo
   else 
      coordDimCount(1)=1
      ! ESMF_GRID_ARBDIM if 1 is distributed, otherwise 1
      if (isDist(1)) then
        coordDimMap(1,1)=ESMF_GRID_ARBDIM      
      else
	coordDimMap(1,1)=1
      endif
   endif

   if (present(coordDep2)) then
      ! error checking, one of the dimensions has to be ESMF_GRID_ARBDIM
      ! if dimension 2 is arbitrary
      if (isDist(2)) then
	found = .false.
	do i=1,size(coordDep2)
	  if (coordDep2(i) .eq. ESMF_GRID_ARBDIM) found = .true.
        enddo
	if (.not. found) then
           call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- coordDep2 does not contain ESMF_GRID_ARBDIM", & 
                 ESMF_CONTEXT, rc) 
	    return
        endif
      endif	
      coordDimCount(2)=size(coordDep2)
      coordDimMap(2,:)=0
      do i=1,size(coordDep2)
         coordDimMap(2,i)=coordDep2(i)
      enddo
   else 
      coordDimCount(2)=1
      ! ESMF_GRID_ARBDIM if 1 is distributed, otherwise 1
      if (isDist(2)) then
        coordDimMap(2,1)=ESMF_GRID_ARBDIM      
      else
	coordDimMap(2,1)=2
      endif
   endif

   if (dimCount .gt. 2) then
      if (present(coordDep3)) then 
        ! error checking, one of the dimensions has to be ESMF_GRID_ARBDIM
        ! if dimension 3 is arbitrary
        if (isDist(3)) then
	  found = .false.
	  do i=1,size(coordDep3)
	    if (coordDep3(i) .eq. ESMF_GRID_ARBDIM) found = .true.
          enddo
	  if (.not. found) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- coordDep3 does not contain ESMF_GRID_ARBDIM", & 
                 ESMF_CONTEXT, rc) 
	    return
          endif
        endif	
        coordDimCount(3)=size(coordDep3)
        coordDimMap(3,:)=0
        do i=1,size(coordDep3)
           coordDimMap(3,i)=coordDep3(i)
        enddo
      else 
        coordDimCount(3)=1
        ! ESMF_GRID_ARBDIM if 1 is distributed, otherwise 1
        if (isDist(3)) then
          coordDimMap(3,1)=ESMF_GRID_ARBDIM      
        else
	  coordDimMap(3,1)=3
        endif
      endif
   endif


   ! Calc undistLBound, undistUBound for Grid -----------------------------------------------
   if (undistDimCount .gt. 0) then
     allocate(undistLBound(undistDimCount), stat=localrc)
     if (ESMF_LogMsgFoundAllocError(localrc, "Allocating undistLBound", &
              ESMF_CONTEXT, rc)) return
     allocate(undistUBound(undistDimCount), stat=localrc)
     if (ESMF_LogMsgFoundAllocError(localrc, "Allocating undistUBound", &
              ESMF_CONTEXT, rc)) return     
   
      ! Fill in undistLBound, undistUBound
      ud=1
      do i=1,dimCount
         if (.not. isDist(i)) then
           undistLBound(ud)=minIndexLocal(i)
           undistUBound(ud)=maxIndexLocal(i)
           ud=ud+1
         endif
      enddo
   endif
  

   ! Create DistGrid --------------------------------------------------------------
   if (undistDimCount .gt. 0) then 
       distgrid=ESMF_DistGridCreate(local1DIndices, 1, undistLBound, undistUBound, rc=localrc)   
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
   else
      distgrid=ESMF_DistGridCreate(local1DIndices, rc=localrc)   
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
   endif


   ! Create Grid from specification -----------------------------------------------
   call ESMF_GridSetFromDistGrid(grid, name, coordTypeKind, &
                               distgrid, distDim=distDimLocal, &
			       coordDimCount=coordDimCount, coordDimMap=coordDimMap, &
                               minIndex=minIndexLocal, maxIndex=maxIndexLocal, &
 	    		       localArbIndexCount=localArbIndexCount, localArbIndex=localArbIndex, &
                               destroyDistGrid=.true., &
                               destroyDELayout=.false., &
			       rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Clean up memory
    deallocate(minIndexLocal)
    deallocate(maxIndexLocal)
    deallocate(local1DIndices)
    deallocate(isDist)
    deallocate(distDimLocal)
    deallocate(coordDimCount)
    deallocate(coordDimMap)
    if (undistDimCount .gt. 0) then
       deallocate(undistLBound)
       deallocate(undistUBound)
    endif
    deallocate(distSize)

    ! Commit Grid -----------------------------------------------------------------
    call ESMF_GridCommit(grid, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    end subroutine ESMF_GridSetCmmitShapeTileArb


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetItemFromArray"
!BOP
! !IROUTINE: ESMF_GridSetItem - Set item using ESMF Array

! !INTERFACE:
      subroutine ESMF_GridSetItemFromArray(grid, staggerloc, item, &
                            array, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid),        intent(in)            :: grid
      type (ESMF_StaggerLoc), intent(in), optional  :: staggerloc 
      type (ESMF_GridItem),   intent(in)            :: item
      type(ESMF_Array),       intent(in)            :: array
      type(ESMF_CopyFlag),    intent(in), optional  :: docopy ! NOT IMPLEMENTED
      integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   This method sets the passed in Array as the holder of the item data
!   for stagger location {\tt staggerloc} and coordinate {\tt coord}. If the location
!   already contains an Array, then this one overwrites it. 
!    
!   Eventually there should be an Add, Get,... like for the Coords to make things
!   easy for the user (except restricted to just I4??)
!
!     The arguments are:
!\begin{description}
!\item[{staggerloc}]
!    The stagger location into which to copy the arrays. 
!    Please see Section~\ref{sec:opt:staggerloc} for a list 
!    of predefined stagger locations. If not present, defaults to
!    ESMF\_STAGGERLOC\_CENTER.
!\item[{item}]
!    The item into which to copy the arrays. Please see Section~\ref{sec:opt:griditem} for a 
!          list of valid items.   
!\item[{array}]
!    An array to set the grid item information from.
!\item[{[doCopy]}]
!    If not specified, default to {\tt ESMF\_DATA\_REF}, in this case the Grid 
!    coordinate Array will be set to a reference to {\tt array}. Please see 
!    Section~\ref{opt:copyflag} for further description and a list of
!    valid values. 
!    [THE ESMF\_DATA\_COPY OPTION IS CURRENTLY NOT IMPLEMENTED] 
!\item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!\end{description}
!
!EOP
    integer :: tmp_staggerloc
    integer :: localrc ! local error status
    type(ESMF_GridDecompType) :: decompType

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayGetInit, array, rc)
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_GridGetInit, grid, rc)

    call ESMF_GridGetDecompType(grid, decompType, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! handle staggerloc
    if (present(staggerloc)) then
       if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. &
	  (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
                 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
                 ESMF_CONTEXT, rc) 
           return
	else
       	   tmp_staggerloc=staggerloc%staggerloc
	endif
    else
        tmp_staggerloc=ESMF_STAGGERLOC_CENTER%staggerloc
    endif

    ! Call C++ Subroutine 
    call c_ESMC_gridsetitemfromarray(grid%this,tmp_staggerloc, item, &
      array, docopy, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridSetItemFromArray


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
!      it must also have been finished with 
!      {\tt ESMF\_GridSetCommitShapeTile}
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


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridConnEqual"
!BOPI
! !IROUTINE: ESMF_GridConnEqual - Equality of GridConns
!
! !INTERFACE:
      function ESMF_GridConnEqual(GridConn1, GridConn2)

! !RETURN VALUE:
      logical :: ESMF_GridConnEqual

! !ARGUMENTS:

      type (ESMF_GridConn), intent(in) :: &
         GridConn1,      &! Two igrid statuses to compare for
         GridConn2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF GridConn statuses to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[GridConn1, GridConn2]
!          Two igrid statuses to compare for equality
!     \end{description}
!
!EOPI

      ESMF_GridConnEqual = (GridConn1%gridconn == &
                              GridConn2%gridconn)

      end function ESMF_GridConnEqual
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridConnNotEqual"
!BOPI
! !IROUTINE: ESMF_GridConnNotEqual - Non-equality of GridConns
!
! !INTERFACE:
      function ESMF_GridConnNotEqual(GridConn1, GridConn2)

! !RETURN VALUE:
      logical :: ESMF_GridConnNotEqual

! !ARGUMENTS:

      type (ESMF_GridConn), intent(in) :: &
         GridConn1,      &! Two GridConn Statuses to compare for
         GridConn2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF GridConn statuses to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[GridConn1, GridConn2]
!          Two statuses of GridConns to compare for inequality
!     \end{description}
!
!EOPI

      ESMF_GridConnNotEqual = (GridConn1%gridconn /= &
                                 GridConn2%gridconn)

      end function ESMF_GridConnNotEqual


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridDecompEqual"
!BOPI
! !IROUTINE: ESMF_GridDecompEqual - Equality of GridDecomps
!
! !INTERFACE:
      function ESMF_GridDecompEqual(GridDecomp1, GridDecomp2)

! !RETURN VALUE:
      logical :: ESMF_GridDecompEqual

! !ARGUMENTS:

      type (ESMF_GridDecompType), intent(in) :: &
         GridDecomp1,      &! Two igrid statuses to compare for
         GridDecomp2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF_GridDecompType statuses to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[GridDecomp1, GridDecomp2]
!          Two igrid statuses to compare for equality
!     \end{description}
!
!EOPI

      ESMF_GridDecompEqual = (GridDecomp1%griddecomptype == &
                              GridDecomp2%griddecomptype)

      end function ESMF_GridDecompEqual
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridDecompNotEqual"
!BOPI
! !IROUTINE: ESMF_GridDecompNotEqual - Non-equality of GridDecomps
!
! !INTERFACE:
      function ESMF_GridDecompNotEqual(GridDecomp1, GridDecomp2)

! !RETURN VALUE:
      logical :: ESMF_GridDecompNotEqual

! !ARGUMENTS:

      type (ESMF_GridDecompType), intent(in) :: &
         GridDecomp1,      &! Two GridDecomp Statuses to compare for
         GridDecomp2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF_GridDecompType statuses to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[GridDecomp1, GridDecomp2]
!          Two statuses of GridDecomps to compare for inequality
!     \end{description}
!
!EOPI

      ESMF_GridDecompNotEqual = (GridDecomp1%griddecomptype /= &
                                 GridDecomp2%griddecomptype)

      end function ESMF_GridDecompNotEqual


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridLUADefault"
!BOPI
! !IROUTINE: ESMF_GridLUADefault

! !INTERFACE:
      subroutine ESMF_GridLUADefault(dimCount, &
                                     lWidthIn, uWidthIn, alignIn, &
                                     lWidthOut, uWidthOut, alignOut, &
                                     rc)
!
! !ARGUMENTS:
       integer,               intent(in)              :: dimCount
       integer,       target, intent(in),   optional  :: lWidthIn(:)
       integer,       target, intent(in),   optional  :: uWidthIn(:)
       integer,       target, intent(in),   optional  :: alignIn(:)
       integer,       target, intent(out)             :: lWidthOut(:)
       integer,       target, intent(out)             :: uWidthOut(:)
       integer,       target, intent(out)             :: alignOut(:)
       integer,               intent(out),  optional  :: rc
!
! !DESCRIPTION:
! This routine sets the default values of the lwidth, uwidth, and align
! based on the user's passed in values for these. 
!
! The arguments are:
! \begin{description}
! \item[{[lWidthIn]}]
!     The lower width from the user.
! \item[{[uWidthIn]}]
!     The upper width from the user.
! \item[{[alignIn]}]
!     The lower width from the user.
! \item[{[lWidthOut]}]
!     The lower width based on user input.
! \item[{[uWidthIn]}]
!     The upper width based on user input.
! \item[{[alignIn]}]
!     The lower width based on user input.
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
    integer :: localrc ! local error status
    type(ESMF_InterfaceInt) :: lWidthInArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: uWidthInArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: alignInArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: lWidthOutArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: uWidthOutArg  ! Language Interface Helper Var
    type(ESMF_InterfaceInt) :: alignOutArg  ! Language Interface Helper Var


    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! turn to interfaceint
    lWidthInArg = ESMF_InterfaceIntCreate(lWidthIn, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    uWidthInArg = ESMF_InterfaceIntCreate(uWidthIn, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    alignInArg = ESMF_InterfaceIntCreate(alignIn, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    lWidthOutArg = ESMF_InterfaceIntCreate(lWidthOut, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    uWidthOutArg = ESMF_InterfaceIntCreate(uWidthOut, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    alignOutArg = ESMF_InterfaceIntCreate(alignOut, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call C++ Subroutine for the default
    call c_ESMC_gridluadefault(dimCount, &
                               lWidthInArg, uWidthInArg, alignInArg, &
                               lWidthOutArg, uWidthOutArg, alignOutArg, &
                               localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deallocate helper variables
    call ESMF_InterfaceIntDestroy(lWidthInArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(uWidthInArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(alignInArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(lWidthOutArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(uWidthOutArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(alignOutArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_GridLUADefault

!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridStatusEqual"
!BOPI
! !IROUTINE: ESMF_GridStatusEqual - Equality of GridStatus statuses
!
! !INTERFACE:
      function ESMF_GridStatusEqual(GridStatus1, GridStatus2)

! !RETURN VALUE:
      logical :: ESMF_GridStatusEqual

! !ARGUMENTS:

      type (ESMF_GridStatus), intent(in) :: &
         GridStatus1,      &! Two igrid statuses to compare for
         GridStatus2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF GridStatus statuses to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[GridStatus1, GridStatus2]
!          Two igrid statuses to compare for equality
!     \end{description}
!
!EOPI

      ESMF_GridStatusEqual = (GridStatus1%gridstatus == &
                              GridStatus2%gridstatus)

      end function ESMF_GridStatusEqual
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridStatusNotEqual"
!BOPI
! !IROUTINE: ESMF_GridStatusNotEqual - Non-equality of GridStatus statuses
!
! !INTERFACE:
      function ESMF_GridStatusNotEqual(GridStatus1, GridStatus2)

! !RETURN VALUE:
      logical :: ESMF_GridStatusNotEqual

! !ARGUMENTS:

      type (ESMF_GridStatus), intent(in) :: &
         GridStatus1,      &! Two GridStatus Statuses to compare for
         GridStatus2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF GridStatus statuses to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[GridStatus1, GridStatus2]
!          Two statuses of GridStatuss to compare for inequality
!     \end{description}
!
!EOPI

      ESMF_GridStatusNotEqual = (GridStatus1%gridstatus /= &
                                 GridStatus2%gridstatus)

      end function ESMF_GridStatusNotEqual


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridStatusGreater"
!BOPI
! !IROUTINE: ESMF_GridStatusGreater - Equality of GridStatus statuses
!
! !INTERFACE:
      function ESMF_GridStatusGreater(GridStatus1, GridStatus2)

! !RETURN VALUE:
      logical :: ESMF_GridStatusGreater

! !ARGUMENTS:

      type (ESMF_GridStatus), intent(in) :: &
         GridStatus1,      &! Two igrid statuses to compare for
         GridStatus2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF GridStatus statuses to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[GridStatus1, GridStatus2]
!          Two igrid statuses to compare for equality
!     \end{description}
!
!EOPI

      ESMF_GridStatusGreater = (GridStatus1%gridstatus .gt. &
                              GridStatus2%gridstatus)

      end function ESMF_GridStatusGreater
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridStatusLess"
!BOPI
! !IROUTINE: ESMF_GridStatusLess - Non-equality of GridStatus statuses
!
! !INTERFACE:
      function ESMF_GridStatusLess(GridStatus1, GridStatus2)

! !RETURN VALUE:
      logical :: ESMF_GridStatusLess

! !ARGUMENTS:

      type (ESMF_GridStatus), intent(in) :: &
         GridStatus1,      &! Two GridStatus Statuses to compare for
         GridStatus2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF GridStatus statuses to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[GridStatus1, GridStatus2]
!          Two statuses of GridStatuss to compare for inequality
!     \end{description}
!
!EOPI

      ESMF_GridStatusLess = (GridStatus1%gridstatus .lt. &
                                 GridStatus2%gridstatus)

      end function ESMF_GridStatusLess

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridStatusGreaterEqual"
!BOPI
! !IROUTINE: ESMF_GridStatusGreaterEqual - Greater than or equal of GridStatus statuses
!
! !INTERFACE:
      function ESMF_GridStatusGreaterEqual(GridStatus1, GridStatus2)

! !RETURN VALUE:
      logical :: ESMF_GridStatusGreaterEqual

! !ARGUMENTS:

      type (ESMF_GridStatus), intent(in) :: &
         GridStatus1,      &! Two igrid statuses to compare for
         GridStatus2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF GridStatus statuses to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[GridStatus1, GridStatus2]
!          Two igrid statuses to compare
!     \end{description}
!
!EOPI

      ESMF_GridStatusGreaterEqual = (GridStatus1%gridstatus .ge. &
                              GridStatus2%gridstatus)

      end function ESMF_GridStatusGreaterEqual
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridStatusLessEqual"
!BOPI
! !IROUTINE: ESMF_GridStatusLessEqual - Less than or equal of GridStatus statuses
!
! !INTERFACE:
      function ESMF_GridStatusLessEqual(GridStatus1, GridStatus2)

! !RETURN VALUE:
      logical :: ESMF_GridStatusLessEqual

! !ARGUMENTS:

      type (ESMF_GridStatus), intent(in) :: &
         GridStatus1,      &! Two GridStatus Statuses to compare for
         GridStatus2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF GridStatus statuses to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[GridStatus1, GridStatus2]
!          Two statuses of GridStatuss to compare
!     \end{description}
!
!EOPI

      ESMF_GridStatusLessEqual = (GridStatus1%gridstatus .le. &
                                 GridStatus2%gridstatus)

      end function ESMF_GridStatusLessEqual

#if 0
! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridTest()"
!BOPI
! !IROUTINE: ESMF_GridTest - Test Grid internals

! !INTERFACE:
  subroutine ESMF_GridTest(grid, rc)
!
! !ARGUMENTS:
    type(ESMF_Grid), intent(in)              :: grid
    integer,         intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!  TEST SUBROUTINE FOR INTERNAL ESMF USE ONLY
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit, grid, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_GridTest(grid, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_GridTest
!------------------------------------------------------------------------------
#endif


#undef  ESMF_METHOD

end module ESMF_GridMod
