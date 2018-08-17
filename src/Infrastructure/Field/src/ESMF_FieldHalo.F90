! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_FieldHalo.F90"
!==============================================================================
!
! ESMF Field Halo Module
module ESMF_FieldHaloMod
!
!==============================================================================
!
! This file contains the Fortran implementation of Field Halo methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!BOPI
! !MODULE: ESMF_FieldHaloMod
!

!   Fortran API of Field Halo
!
!------------------------------------------------------------------------------

! !USES:
  use ESMF_UtilTypesMod     ! ESMF utility types
  use ESMF_InitMacrosMod    ! ESMF initializer macros
  use ESMF_BaseMod          ! ESMF base class
  use ESMF_LogErrMod        ! ESMF error handling
  use ESMF_ArrayMod
  use ESMF_FieldMod
  use ESMF_FieldGetMod
  use ESMF_VMMod
  use ESMF_DELayoutMod
  use ESMF_RHandleMod
  
  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private
      
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:
  public ESMF_FieldHalo
  public ESMF_FieldHaloRelease
  public ESMF_FieldHaloStore
  public ESMF_FieldIsCreated          ! Check if a Field object is created

!EOPI
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id$'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



!==================== communication calls ===========================


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldHalo()"
!BOP
! !IROUTINE: ESMF_FieldHalo - Execute a FieldHalo operation
!
! !INTERFACE:
  subroutine ESMF_FieldHalo(field, routehandle, keywordEnforcer,  &
                            routesyncflag, finishedflag, checkflag, rc)
!
! !ARGUMENTS:
    type(ESMF_Field),          intent(inout)          :: field
    type(ESMF_RouteHandle),    intent(inout)          :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords for the below
    type(ESMF_RouteSync_Flag), intent(in),  optional  :: routesyncflag
    logical,                   intent(out), optional  :: finishedflag
    logical,                   intent(in),  optional  :: checkflag
    integer,                   intent(out), optional  :: rc
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Execute a precomputed Field halo operation for {\tt field}. 
!   The {\tt field} argument must match the Field used during 
!   {\tt ESMF\_FieldHaloStore()} in {\em type}, {\em kind}, and 
!   memory layout of the {\em gridded} dimensions. However, the size, number, 
!   and index order of {\em ungridded} dimensions may be different. See section
!   \ref{RH:Reusability} for a more detailed discussion of RouteHandle 
!   reusability.
!
!   See {\tt ESMF\_FieldHaloStore()} on how to precompute {\tt routehandle}.
!
!   This call is {\em collective} across the current VM.
!
!   \begin{description}
!   \item [field]
!     {\tt ESMF\_Field} containing data to be haloed.
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item [{[routesyncflag]}]
!     Indicate communication option. Default is {\tt ESMF\_ROUTESYNC\_BLOCKING},
!     resulting in a blocking operation.
!     See section \ref{const:routesync} for a complete list of valid settings.
!   \item [{[finishedflag]}]
!     \begin{sloppypar}
!     Used in combination with {\tt routesyncflag = ESMF\_ROUTESYNC\_NBTESTFINISH}.
!     Returned {\tt finishedflag} equal to {\tt .true.} indicates that all
!     operations have finished. A value of {\tt .false.} indicates that there
!     are still unfinished operations that require additional calls with
!     {\tt routesyncflag = ESMF\_ROUTESYNC\_NBTESTFINISH}, or a final call with
!     {\tt routesyncflag = ESMF\_ROUTESYNC\_NBWAITFINISH}. For all other {\tt routesyncflag}
!     settings the returned value in {\tt finishedflag} is always {\tt .true.}.
!     \end{sloppypar}
!   \item [{[checkflag]}]
!     If set to {\tt .TRUE.} the input Field pair will be checked for
!     consistency with the precomputed operation provided by {\tt routehandle}.
!     If set to {\tt .FALSE.} {\em (default)} only a very basic input check
!     will be performed, leaving many inconsistencies undetected. Set
!     {\tt checkflag} to {\tt .FALSE.} to achieve highest performance.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! local variables
    type(ESMF_Array)        :: array

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit, routehandle, rc)
    
    call ESMF_FieldGet(field, array=array, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Call into the Array interface, which will sort out optional arguments
    call ESMF_ArrayHalo(array, routehandle=routehandle, routesyncflag=routesyncflag, &
      finishedflag=finishedflag, checkflag=checkflag, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_FieldHalo
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldHaloRelease()"
!BOP
! !IROUTINE: ESMF_FieldHaloRelease - Release resources associated with a Field halo operation
!
! !INTERFACE:
  subroutine ESMF_FieldHaloRelease(routehandle, keywordEnforcer, noGarbage, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(inout)           :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),   optional  :: noGarbage
    integer,                intent(out),  optional  :: rc
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[8.0.0] Added argument {\tt noGarbage}.
!   The argument provides a mechanism to override the default garbage collection
!   mechanism when destroying an ESMF object.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
!   Release resources associated with a Field halo operation.
!   After this call {\tt routehandle} becomes invalid.
!
!   \begin{description}
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item[{[noGarbage]}]
!     If set to {\tt .TRUE.} the object will be fully destroyed and removed
!     from the ESMF garbage collection system. Note however that under this 
!     condition ESMF cannot protect against accessing the destroyed object 
!     through dangling aliases -- a situation which may lead to hard to debug 
!     application crashes.
! 
!     It is generally recommended to leave the {\tt noGarbage} argument
!     set to {\tt .FALSE.} (the default), and to take advantage of the ESMF 
!     garbage collection system which will prevent problems with dangling
!     aliases or incorrect sequences of destroy calls. However this level of
!     support requires that a small remnant of the object is kept in memory
!     past the destroy call. This can lead to an unexpected increase in memory
!     consumption over the course of execution in applications that use 
!     temporary ESMF objects. For situations where the repeated creation and 
!     destruction of temporary objects leads to memory issues, it is 
!     recommended to call with {\tt noGarbage} set to {\tt .TRUE.}, fully 
!     removing the entire temporary object from memory.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments, deal with optional Array args
    ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit, routehandle, rc)
        
    ! Call into the RouteHandle code
    call ESMF_RouteHandleRelease(routehandle, noGarbage=noGarbage, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_FieldHaloRelease
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldHaloStore()"
!BOP
! !IROUTINE: ESMF_FieldHaloStore - Store a FieldHalo operation
!
! !INTERFACE:
    subroutine ESMF_FieldHaloStore(field, routehandle, keywordEnforcer,  &
      startregion, haloLDepth, haloUDepth, rc)
!
! !ARGUMENTS:
    type(ESMF_Field),            intent(inout)           :: field
    type(ESMF_RouteHandle),      intent(inout)           :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_StartRegion_Flag), intent(in),    optional :: startregion
    integer,                     intent(in),    optional :: haloLDepth(:)
    integer,                     intent(in),    optional :: haloUDepth(:)
    integer,                     intent(out),   optional :: rc
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Store a Field halo operation over the data in {\tt field}. By default,
!   i.e. without specifying {\tt startregion}, {\tt haloLDepth} and
!   {\tt haloUDepth}, all elements in the total Field region that lie outside
!   the exclusive region will be considered potential destination elements for
!   halo. However, only those elements that have a corresponding halo source
!   element, i.e. an exclusive element on one of the DEs, will be updated under
!   the halo operation. Elements that have no associated source remain 
!   unchanged under halo.
!
!   Specifying {\tt startregion} allows to change the shape of the 
!   effective halo region from the inside. Setting this flag to
!   {\tt ESMF\_STARTREGION\_COMPUTATIONAL} means that only elements outside 
!   the computational region of the Field are considered for potential
!   destination elements for the halo operation. The default is {\tt ESMF\_STARTREGION\_EXCLUSIVE}.
!
!   The {\tt haloLDepth} and {\tt haloUDepth} arguments allow to reduce
!   the extent of the effective halo region. Starting at the region specified
!   by {\tt startregion}, the {\tt haloLDepth} and {\tt haloUDepth}
!   define a halo depth in each direction. Note that the maximum halo region is
!   limited by the total Field region, independent of the actual
!   {\tt haloLDepth} and {\tt haloUDepth} setting. The total Field region is
!   local DE specific. The {\tt haloLDepth} and {\tt haloUDepth} are interpreted
!   as the maximum desired extent, reducing the potentially larger region
!   available for the halo operation.
!
!   The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
!   {\tt ESMF\_FieldHalo()} on any Field that matches 
!   {\tt field} in {\em type}, {\em kind}, and 
!   memory layout of the {\em gridded} dimensions. However, the size, number, 
!   and index order of {\em ungridded} dimensions may be different. See section
!   \ref{RH:Reusability} for a more detailed discussion of RouteHandle 
!   reusability.
!  
!   This call is {\em collective} across the current VM.  
!
!   \begin{description}
!   \item [field]
!     {\tt ESMF\_Field} containing data to be haloed. The data in this Field may be
!     destroyed by this call.
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item [{[startregion]}]
!     \begin{sloppypar}
!     The start of the effective halo region on every DE. The default
!     setting is {\tt ESMF\_STARTREGION\_EXCLUSIVE}, rendering all non-exclusive
!     elements potential halo destination elements.
!     See section \ref{const:startregion} for a complete list of
!     valid settings.
!     \end{sloppypar}
!   \item[{[haloLDepth]}] 
!     This vector specifies the lower corner of the effective halo
!     region with respect to the lower corner of {\tt startregion}.
!     The size of {\tt haloLDepth} must equal the number of distributed Array
!     dimensions.
!   \item[{[haloUDepth]}] 
!     This vector specifies the upper corner of the effective halo
!     region with respect to the upper corner of {\tt startregion}.
!     The size of {\tt haloUDepth} must equal the number of distributed Array
!     dimensions.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                         :: localrc        ! local return code

    ! local variables
    type(ESMF_Array)                :: array

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc)
    
    ! query the field for its internal array
    call ESMF_FieldGet(field, array=array, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the Array interface, which will sort out optional arguments
    call ESMF_ArrayHaloStore(array, routehandle=routehandle, &
      startregion=startregion, haloLDepth=haloLDepth, &
      haloUDepth=haloUDepth, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_FieldHaloStore
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldIsCreated()"
!BOP
! !IROUTINE: ESMF_FieldIsCreated - Check whether a Field object has been created

! !INTERFACE:
  function ESMF_FieldIsCreated(field, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_FieldIsCreated
!
! !ARGUMENTS:
    type(ESMF_Field), intent(in)            :: field
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt field} has been created. Otherwise return 
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is 
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[field]
!     {\tt ESMF\_Field} queried.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------    
    ESMF_FieldIsCreated = .false.   ! initialize
    if (present(rc)) rc = ESMF_SUCCESS
    if (ESMF_FieldGetInit(field)==ESMF_INIT_CREATED) &
      ESMF_FieldIsCreated = .true.
  end function
!------------------------------------------------------------------------------

end module ESMF_FieldHaloMod
