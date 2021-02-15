! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_ArrayHa.F90"
!==============================================================================
!
! ESMF Array Module
module ESMF_ArrayHaMod
!
!==============================================================================
!
! This file contains the Fortran wrapper code for the C++ implementation of
!  the Array class.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!BOPI
! !MODULE: ESMF_ArrayMod
!

!   Fortran API wrapper of C++ implemenation of Array
!
!------------------------------------------------------------------------------

! !USES:
  use iso_c_binding

  use ESMF_UtilTypesMod     ! ESMF utility types
  use ESMF_InitMacrosMod    ! ESMF initializer macros
  use ESMF_BaseMod          ! ESMF base class
  use ESMF_LogErrMod        ! ESMF error handling
  use ESMF_LocalArrayMod
  use ESMF_ArraySpecMod
  use ESMF_VMMod
  use ESMF_DELayoutMod
  use ESMF_DistGridMod
  use ESMF_RHandleMod
  use ESMF_F90InterfaceMod  ! ESMF Fortran-C++ interface helper
  use ESMF_IOUtilMod
  
  ! class sub modules
  use ESMF_ArrayCreateMod   ! contains the ESMF_Array derived type definition
  use ESMF_ArrayGetMod      ! contains the ESMF_ArrayGet procedures
  
  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private
      
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:
  public ESMF_ArrayHalo
  public ESMF_ArrayHaloRelease
  public ESMF_ArrayHaloStore
  public ESMF_ArrayIsCreated
  public ESMF_ArrayPrint
  public ESMF_ArrayRead
  public ESMF_ArrayRedist
  public ESMF_ArrayRedistRelease
  public ESMF_ArrayRedistStore

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


! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_ArrayRedistStore -- Generic interface

! !INTERFACE:
  interface ESMF_ArrayRedistStore

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_ArrayRedistStoreI4
    module procedure ESMF_ArrayRedistStoreI8
    module procedure ESMF_ArrayRedistStoreR4
    module procedure ESMF_ArrayRedistStoreR8
    module procedure ESMF_ArrayRedistStoreI4TP
    module procedure ESMF_ArrayRedistStoreI8TP
    module procedure ESMF_ArrayRedistStoreR4TP
    module procedure ESMF_ArrayRedistStoreR8TP
    module procedure ESMF_ArrayRedistStoreNF
    module procedure ESMF_ArrayRedistStoreNFTP
!EOPI

  end interface

!------------------------------------------------------------------------------
! ! Interoperability interfaces

#ifndef ESMF_NO_F2018ASSUMEDTYPE

  interface

    subroutine c_ESMC_ArrayRedistStore(srcArray, dstArray, routehandle, &
      srcToDstTransposeMap, typekind, factor, ignoreUnmatched, pipelineDepth, &
      rc)
      import                :: ESMF_Array, ESMF_RouteHandle
      import                :: ESMF_InterArray, ESMF_TypeKind_Flag, ESMF_Logical
      type(ESMF_Array)      :: srcArray, dstArray
      type(ESMF_RouteHandle):: routehandle
      type(ESMF_InterArray) :: srcToDstTransposeMap
      type(ESMF_TypeKind_Flag):: typekind
      type(*)               :: factor
      type(ESMF_Logical)    :: ignoreUnmatched
      integer               :: pipelineDepth
      integer               :: rc
    end subroutine

  end interface

#endif

!------------------------------------------------------------------------------

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayHalo()"
!BOP
! !IROUTINE: ESMF_ArrayHalo - Execute an Array halo operation
!
! !INTERFACE:
  subroutine ESMF_ArrayHalo(array, routehandle, keywordEnforcer, &
    routesyncflag, finishedflag, cancelledflag, checkflag, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),          intent(inout)         :: array
    type(ESMF_RouteHandle),    intent(inout)         :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_RouteSync_Flag), intent(in),  optional :: routesyncflag
    logical,                   intent(out), optional :: finishedflag
    logical,                   intent(out), optional :: cancelledflag
    logical,                   intent(in),  optional :: checkflag
    integer,                   intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Execute a precomputed Array halo operation for {\tt array}.
!   The {\tt array} argument must match the respective Array
!   used during {\tt ESMF\_ArrayHaloStore()} in {\em type}, {\em kind}, and 
!   memory layout of the {\em distributed} dimensions. However, the size,
!   number, and index order of {\em undistributed} dimensions may be different.
!   See section \ref{RH:Reusability} for a more detailed discussion of
!   RouteHandle reusability.
!
!   See {\tt ESMF\_ArrayHaloStore()} on how to precompute {\tt routehandle}.
!
!   This call is {\em collective} across the current VM.
!
!   \begin{description}
!   \item [array]
!     {\tt ESMF\_Array} containing data to be haloed.
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
!   \item [{[cancelledflag]}]
!     A value of {\tt .true.} indicates that were cancelled communication
!     operations. In this case the data in the {\tt dstArray} must be considered
!     invalid. It may have been partially modified by the call. A value of
!     {\tt .false.} indicates that none of the communication operations was
!     cancelled. The data in {\tt dstArray} is valid if {\tt finishedflag} 
!     returns equal {\tt .true.}.
!   \item [{[checkflag]}]
!     If set to {\tt .TRUE.} the input Array pair will be checked for
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
    type(ESMF_RouteSync_Flag)     :: opt_routesyncflag ! helper variable
    type(ESMF_Logical)      :: opt_finishedflag   ! helper variable
    type(ESMF_Logical)      :: opt_cancelledflag  ! helper variable
    type(ESMF_Logical)      :: opt_checkflag! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit, routehandle, rc)
    
    ! Set default flags
    opt_routesyncflag = ESMF_ROUTESYNC_BLOCKING
    if (present(routesyncflag)) opt_routesyncflag = routesyncflag
    opt_checkflag = ESMF_FALSE
    if (present(checkflag)) opt_checkflag = checkflag
    
    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayHalo(array, routehandle, &
      opt_routesyncflag, opt_finishedflag, opt_cancelledflag, opt_checkflag, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! translate back finishedflag
    if (present(finishedflag)) then
      finishedflag = opt_finishedflag
    endif
    
    ! translate back cancelledflag
    if (present(cancelledflag)) then
      cancelledflag = opt_cancelledflag
    endif
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayHalo
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayHaloRelease()"
!BOP
! !IROUTINE: ESMF_ArrayHaloRelease - Release resources associated with Array halo operation
!
! !INTERFACE:
  subroutine ESMF_ArrayHaloRelease(routehandle, keywordEnforcer, noGarbage, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(inout)         :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),  optional :: noGarbage
    integer,                intent(out), optional :: rc
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
!   Release resources associated with an Array halo operation. 
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

  end subroutine ESMF_ArrayHaloRelease
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayHaloStore()"
!BOP
! !IROUTINE: ESMF_ArrayHaloStore - Precompute an Array halo operation
!
! !INTERFACE:
    subroutine ESMF_ArrayHaloStore(array, routehandle, keywordEnforcer, &
      startregion, haloLDepth, haloUDepth, pipelineDepth, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),            intent(inout)           :: array
    type(ESMF_RouteHandle),      intent(inout)           :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_StartRegion_Flag), intent(in),    optional :: startregion
    integer,                     intent(in),    optional :: haloLDepth(:)
    integer,                     intent(in),    optional :: haloUDepth(:)
    integer,                     intent(inout), optional :: pipelineDepth
    integer,                     intent(out),   optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[6.1.0] Added argument {\tt pipelineDepth}.
!              The new argument provide access to the tuning parameter
!              affecting the sparse matrix execution.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
!   Store an Array halo operation over the data in {\tt array}. By default,
!   i.e. without specifying {\tt startregion}, {\tt haloLDepth} and
!   {\tt haloUDepth}, all elements in the total Array region that lie outside
!   the exclusive region will be considered potential destination elements for
!   halo. However, only those elements that have a corresponding halo source
!   element, i.e. an exclusive element on one of the DEs, will be updated under
!   the halo operation. Elements that have no associated source remain 
!   unchanged under halo.
!
!   Specifying {\tt startregion} allows the shape of the effective halo region 
!   to be changed from the inside. Setting this flag to
!   {\tt ESMF\_STARTREGION\_COMPUTATIONAL} means that only elements outside 
!   the computational region of the Array are considered for potential
!   destination elements for the halo operation. The default is 
!   {\tt ESMF\_STARTREGION\_EXCLUSIVE}.
!
!   The {\tt haloLDepth} and {\tt haloUDepth} arguments allow to reduce
!   the extent of the effective halo region. Starting at the region specified
!   by {\tt startregion}, the {\tt haloLDepth} and {\tt haloUDepth}
!   define a halo depth in each direction. Note that the maximum halo region is
!   limited by the total Array region, independent of the actual
!   {\tt haloLDepth} and {\tt haloUDepth} setting. The total Array region is
!   local DE specific. The {\tt haloLDepth} and {\tt haloUDepth} are interpreted
!   as the maximum desired extent, reducing the potentially larger region
!   available for the halo operation.
!
!   The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
!   {\tt ESMF\_ArrayHalo()} on any Array that matches 
!   {\tt array} in {\em type}, {\em kind}, and 
!   memory layout of the {\em distributed} dimensions. However, the size,
!   number, and index order of {\em undistributed} dimensions may be different.
!   See section \ref{RH:Reusability} for a more detailed discussion of
!   RouteHandle reusability.
!
!   This call is {\em collective} across the current VM.  
!
!   \begin{description}
!   \item [array]
!     {\tt ESMF\_Array} containing data to be haloed. The data in the halo
!     region may be destroyed by this call.
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
!   \item [{[pipelineDepth]}]
!     The {\tt pipelineDepth} parameter controls how many messages a PET
!     may have outstanding during a halo exchange. Larger values
!     of {\tt pipelineDepth} typically lead to better performance. However,
!     on some systems too large a value may lead to performance degradation,
!     or runtime errors.
!
!     Note that the pipeline depth has no effect on the bit-for-bit
!     reproducibility of the results. However, it may affect the performance
!     reproducibility of the exchange.
!
!     The {\tt ESMF\_ArraySMMStore()} method implements an auto-tuning scheme
!     for the {\tt pipelineDepth} parameter. The intent on the 
!     {\tt pipelineDepth} argument is "{\tt inout}" in order to 
!     support both overriding and accessing the auto-tuning parameter.
!     If an argument $>= 0$ is specified, it is used for the 
!     {\tt pipelineDepth} parameter, and the auto-tuning phase is skipped.
!     In this case the {\tt pipelineDepth} argument is not modified on
!     return. If the provided argument is $< 0$, the {\tt pipelineDepth}
!     parameter is determined internally using the auto-tuning scheme. In this
!     case the {\tt pipelineDepth} argument is re-set to the internally
!     determined value on return. Auto-tuning is also used if the optional 
!     {\tt pipelineDepth} argument is omitted.
!
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                       :: localrc          ! local return code
    type(ESMF_StartRegion_Flag)   :: opt_startregion  ! helper variable
    type(ESMF_InterArray)         :: haloLDepthArg    ! helper variable
    type(ESMF_InterArray)         :: haloUDepthArg    ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc)
    
    ! Set default flags
    opt_startregion = ESMF_STARTREGION_EXCLUSIVE
    if (present(startregion)) opt_startregion = startregion

    ! Deal with (optional) array arguments
    haloLDepthArg = ESMF_InterArrayCreate(haloLDepth, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    haloUDepthArg = ESMF_InterArrayCreate(haloUDepth, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayHaloStore(array, routehandle, opt_startregion, &
      haloLDepthArg, haloUDepthArg, pipelineDepth, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! garbage collection
    call ESMF_InterArrayDestroy(haloLDepthArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(haloUDepthArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayHaloStore
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayIsCreated()"
!BOP
! !IROUTINE: ESMF_ArrayIsCreated - Check whether an Array object has been created

! !INTERFACE:
  function ESMF_ArrayIsCreated(array, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_ArrayIsCreated
!
! !ARGUMENTS:
    type(ESMF_Array), intent(in)            :: array
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,          intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt array} has been created. Otherwise return 
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is 
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[array]
!     {\tt ESMF\_Array} queried.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------    
    ESMF_ArrayIsCreated = .false.   ! initialize
    if (present(rc)) rc = ESMF_SUCCESS
    if (ESMF_ArrayGetInit(array)==ESMF_INIT_CREATED) &
      ESMF_ArrayIsCreated = .true.
  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayPrint()"
!BOP
! !IROUTINE: ESMF_ArrayPrint - Print Array information

! !INTERFACE:
  subroutine ESMF_ArrayPrint(array, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_Array), intent(in)            :: array
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,          intent(out), optional :: rc  
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Print internal information of the specified {\tt ESMF\_Array} object. \\
!
!   The arguments are:
!   \begin{description}
!   \item[array] 
!     {\tt ESMF\_Array} object.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc)
    
    ! Flush before crossing language interface to ensure correct output order
    call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface
    call c_ESMC_ArrayPrint(array, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_ArrayPrint
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayRead"
!BOP
! !IROUTINE: ESMF_ArrayRead - Read Array data from a file
! \label{api:ArrayRead}
!
! !INTERFACE:
  subroutine ESMF_ArrayRead(array, fileName, keywordEnforcer, variableName, &
    timeslice, iofmt, rc)
!   ! We need to terminate the strings on the way to C++
!
! !ARGUMENTS:
    type(ESMF_Array),      intent(inout)         :: array
    character(*),          intent(in)            :: fileName
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(*),          intent(in),  optional :: variableName
    integer,               intent(in),  optional :: timeslice
    type(ESMF_IOFmt_Flag), intent(in),  optional :: iofmt
    integer,               intent(out), optional :: rc
!
! !DESCRIPTION:
!   Read Array data from file and put it into an {\tt ESMF\_Array} object.
!   For this API to be functional, the environment variable {\tt ESMF\_PIO}
!   should be set to "internal" when the ESMF library is built.
!   Please see the section on Data I/O,~\ref{io:dataio}.
! 
!   Limitations:
!   \begin{itemize}
!     \item Only single tile Arrays are supported.
!     \item Not supported in {\tt ESMF\_COMM=mpiuni} mode.
!   \end{itemize}
!
!  The arguments are:
!  \begin{description}
!   \item[array]
!    The {\tt ESMF\_Array} object in which the read data is returned.
!   \item[fileName]
!    The name of the file from which Array data is read.
!   \item[{[variableName]}]
!    Variable name in the file; default is the "name" of Array.
!    Use this argument only in the I/O format (such as NetCDF) that
!    supports variable name. If the I/O format does not support this
!    (such as binary format), ESMF will return an error code.
!   \item[{[timeslice]}]
!    The time-slice number of the variable read from file.
!   \item[{[iofmt]}]
!    \begin{sloppypar}
!    The I/O format.  Please see Section~\ref{opt:iofmtflag} for the list
!    of options. If not present, file names with a {\tt .bin} extension will
!    use {\tt ESMF\_IOFMT\_BIN}, and file names with a {\tt .nc} extension
!    will use {\tt ESMF\_IOFMT\_NETCDF}.  Other files default to
!    {\tt ESMF\_IOFMT\_NETCDF}.
!    \end{sloppypar}
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!  \end{description}
!
!EOP
!------------------------------------------------------------------------------
    ! Local vars
    integer              :: localrc              ! local return code
    integer              :: len_varName          ! helper variable
    type(ESMF_IOFmt_Flag) :: opt_iofmt           ! helper variable
    integer              :: file_ext_p

    ! Initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

#ifdef ESMF_PIO

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc)

    ! Set iofmt based on file name extension (if present)
    if (present (iofmt)) then
      opt_iofmt = iofmt
    else
      if (index (fileName, '.') > 0) then
        file_ext_p = index (fileName, '.', back=.true.)
        select case (fileName(file_ext_p:))
        case ('.nc')
          opt_iofmt = ESMF_IOFMT_NETCDF
        case ('.bin')
          opt_iofmt = ESMF_IOFMT_BIN
        case default
          opt_iofmt = ESMF_IOFMT_NETCDF
        end select
      else
        opt_iofmt = ESMF_IOFMT_NETCDF
      end if
    end if

    ! Get string length
    if (present(variableName)) then
      len_varName = len_trim (variableName)
    else
      len_varName = 0
    endif

    ! Call into the C++ interface, which will call IO object
    call c_esmc_arrayread(array, fileName,                    &
        variableName, len_varName, timeslice, opt_iofmt, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,        &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
#else
    ! Return indicating PIO not present
    call ESMF_LogSetError(rcToCheck=ESMF_RC_LIB_NOT_PRESENT, &
      msg="ESMF must be compiled with PIO support to support I/O methods", &
      ESMF_CONTEXT, rcToReturn=rc)
#endif

  end subroutine ESMF_ArrayRead
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayRedist()"
!BOP
! !IROUTINE: ESMF_ArrayRedist - Execute an Array redistribution
!
! !INTERFACE:
  subroutine ESMF_ArrayRedist(srcArray, dstArray, routehandle, keywordEnforcer, &
    routesyncflag, finishedflag, cancelledflag, zeroregion, checkflag, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),          intent(in),    optional :: srcArray
    type(ESMF_Array),          intent(inout), optional :: dstArray
    type(ESMF_RouteHandle),    intent(inout)           :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_RouteSync_Flag), intent(in),    optional :: routesyncflag
    logical,                   intent(out),   optional :: finishedflag
    logical,                   intent(out),   optional :: cancelledflag
    type(ESMF_Region_Flag),    intent(in),    optional :: zeroregion
    logical,                   intent(in),    optional :: checkflag
    integer,                   intent(out),   optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[7.1.0r] Added argument {\tt zeroregion} to allow user to control
!              how the destination array is zero'ed out. This is especially
!              useful in cases where the source and destination arrays do not
!              cover the identical index space.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
!   \begin{sloppypar}
!   Execute a precomputed Array redistribution from {\tt srcArray}
!   to {\tt dstArray}.
!   Both {\tt srcArray} and {\tt dstArray} must match the respective Arrays
!   used during {\tt ESMF\_ArrayRedisttore()} in {\em type}, {\em kind}, and 
!   memory layout of the {\em distributed} dimensions. However, the size,
!   number, and index order of {\em undistributed} dimensions may be different.
!   See section \ref{RH:Reusability} for a more detailed discussion of
!   RouteHandle reusability.
!   \end{sloppypar}
!
!   The {\tt srcArray} and {\tt dstArray} arguments are optional in support of
!   the situation where {\tt srcArray} and/or {\tt dstArray} are not defined on
!   all PETs. The {\tt srcArray} and {\tt dstArray} must be specified on those
!   PETs that hold source or destination DEs, respectively, but may be omitted
!   on all other PETs. PETs that hold neither source nor destination DEs may
!   omit both arguments.
!
!   It is erroneous to specify the identical Array object for {\tt srcArray} and
!   {\tt dstArray} arguments.
!
!   See {\tt ESMF\_ArrayRedistStore()} on how to precompute 
!   {\tt routehandle}.
!
!   This call is {\em collective} across the current VM.
!
!   \begin{description}
!   \item [{[srcArray]}]
!     {\tt ESMF\_Array} with source data.
!   \item [{[dstArray]}]
!     {\tt ESMF\_Array} with destination data.
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
!   \item [{[cancelledflag]}]
!     A value of {\tt .true.} indicates that were cancelled communication
!     operations. In this case the data in the {\tt dstArray} must be considered
!     invalid. It may have been partially modified by the call. A value of
!     {\tt .false.} indicates that none of the communication operations was
!     cancelled. The data in {\tt dstArray} is valid if {\tt finishedflag} 
!     returns equal {\tt .true.}.
!   \item [{[zeroregion]}]
!     \begin{sloppypar}
!     If set to {\tt ESMF\_REGION\_TOTAL} the total regions of
!     all DEs in {\tt dstArray} will be initialized to zero before updating the 
!     elements with the results of the sparse matrix multiplication. If set to
!     {\tt ESMF\_REGION\_EMPTY} the elements in {\tt dstArray} will not be
!     modified prior to the sparse matrix multiplication and results will be
!     added to the incoming element values. Setting {\tt zeroregion} to 
!     {\tt ESMF\_REGION\_SELECT} will only zero out those elements in the 
!     destination Array that will be updated by the sparse matrix
!     multiplication. See section \ref{const:region} for a complete list of
!     valid settings. The default is {\tt ESMF\_REGION\_SELECT}.
!     \end{sloppypar}
!   \item [{[checkflag]}]
!     If set to {\tt .TRUE.} the input Array pair will be checked for
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
    integer                   :: localrc            ! local return code
    type(ESMF_Array)          :: opt_srcArray       ! helper variable
    type(ESMF_Array)          :: opt_dstArray       ! helper variable
    type(ESMF_RouteSync_Flag) :: opt_routesyncflag  ! helper variable
    type(ESMF_Logical)        :: opt_finishedflag   ! helper variable
    type(ESMF_Logical)        :: opt_cancelledflag  ! helper variable
    type(ESMF_Region_Flag)    :: opt_zeroregion     ! helper variable
    type(ESMF_Logical)        :: opt_checkflag      ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments, deal with optional Array args
    ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit, routehandle, rc)
    if (present(srcArray)) then
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, srcArray, rc)
      opt_srcArray = srcArray
    else
      call ESMF_ArraySetThisNull(opt_srcArray, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    if (present(dstArray)) then
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, dstArray, rc)
      opt_dstArray = dstArray
    else
      call ESMF_ArraySetThisNull(opt_dstArray, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    
    ! Set default flags
    opt_routesyncflag = ESMF_ROUTESYNC_BLOCKING
    if (present(routesyncflag)) opt_routesyncflag = routesyncflag
    opt_zeroregion = ESMF_REGION_SELECT
    if (present(zeroregion)) opt_zeroregion = zeroregion
    opt_checkflag = ESMF_FALSE
    if (present(checkflag)) opt_checkflag = checkflag
        
    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayRedist(opt_srcArray, opt_dstArray, routehandle, &
      opt_routesyncflag, opt_finishedflag, opt_cancelledflag, opt_zeroregion, &
      opt_checkflag, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! translate back finishedflag
    if (present(finishedflag)) then
      finishedflag = opt_finishedflag
    endif
    
    ! translate back cancelledflag
    if (present(cancelledflag)) then
      cancelledflag = opt_cancelledflag
    endif
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayRedist
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayRedistRelease()"
!BOP
! !IROUTINE: ESMF_ArrayRedistRelease - Release resources associated with Array redistribution
!
! !INTERFACE:
  subroutine ESMF_ArrayRedistRelease(routehandle, keywordEnforcer, noGarbage, rc)
! 
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(inout)         :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),  optional :: noGarbage
    integer,                intent(out), optional :: rc
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
!   Release resources associated with an Array redistribution. After this call
!   {\tt routehandle} becomes invalid.
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

  end subroutine ESMF_ArrayRedistRelease
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayRedistStore - Precompute Array redistribution with local factor argument
!
! !INTERFACE:
! ! Private name; call using ESMF_ArrayRedistStore()
! subroutine ESMF_ArrayRedistStore<type><kind>(srcArray, dstArray, &
!   routehandle, factor, keywordEnforcer, srcToDstTransposeMap, &
!   ignoreUnmatchedIndices, pipelineDepth, rc)
!
! !ARGUMENTS:
!   type(ESMF_Array),       intent(in)              :: srcArray
!   type(ESMF_Array),       intent(inout)           :: dstArray
!   type(ESMF_RouteHandle), intent(inout)           :: routehandle
!   <type>(ESMF_KIND_<kind>),intent(in)             :: factor
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!   integer,                intent(in),    optional :: srcToDstTransposeMap(:)
!   logical,                intent(in),    optional :: ignoreUnmatchedIndices
!   integer,                intent(inout), optional :: pipelineDepth
!   integer,                intent(out),   optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[6.1.0] Added argument {\tt pipelineDepth}.
!              The new argument provide access to the tuning parameter
!              affecting the sparse matrix execution.
! \item[7.0.0] Added argument {\tt transposeRoutehandle} to allow a handle to
!              the transposed redist operation to be returned.\newline
!              Added argument {\tt ignoreUnmatchedIndices} to support situations 
!              where not all elements between source and destination Arrays 
!              match.
! \item[7.1.0r] Removed argument {\tt transposeRoutehandle} and provide it
!              via interface overloading instead. This allows argument 
!              {\tt srcArray} to stay strictly intent(in) for this entry point.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
! \label{ArrayRedistStoreTK}
! {\tt ESMF\_ArrayRedistStore()} is a collective method across all PETs of the
! current Component. The interface of the method is overloaded, allowing 
! -- in principle -- each PET to call into {\tt ESMF\_ArrayRedistStore()}
! through a different entry point. Restrictions apply as to which combinations
! are sensible. All other combinations result in ESMF run time errors. The
! complete semantics of the {\tt ESMF\_ArrayRedistStore()} method, as provided
! through the separate entry points shown in \ref{ArrayRedistStoreTK} and
! \ref{ArrayRedistStoreNF}, is described in the following paragraphs as a whole.
!
! Store an Array redistribution operation from {\tt srcArray} to {\tt dstArray}.
! Interface \ref{ArrayRedistStoreTK} allows PETs to specify a {\tt factor}
! argument. PETs not specifying a {\tt factor} argument call into interface
! \ref{ArrayRedistStoreNF}. If multiple PETs specify the {\tt factor} argument,
! its type and kind, as well as its value must match across all PETs. If none
! of the PETs specify a {\tt factor} argument the default will be a factor of
! 1. The resulting factor is applied to all of the source data during
! redistribution, allowing scaling of the data, e.g. for unit transformation.
!  
! Both {\tt srcArray} and {\tt dstArray} are interpreted as sequentialized 
! vectors. The sequence is defined by the order of DistGrid dimensions and the
! order of tiles within the DistGrid or by user-supplied arbitrary sequence
! indices. See section \ref{Array:SparseMatMul} for details on the definition
! of {\em sequence indices}.
!
! Source Array, destination Array, and the factor may be of different
! <type><kind>. Further, source and destination Arrays may differ in shape,
! however, the number of elements must match. 
!  
! If {\tt srcToDstTransposeMap} is not specified the redistribution corresponds
! to an identity mapping of the sequentialized source Array to the
! sequentialized destination Array. If the {\tt srcToDstTransposeMap}
! argument is provided it must be identical on all PETs. The
! {\tt srcToDstTransposeMap} allows source and destination Array dimensions to
! be transposed during the redistribution. The number of source and destination
! Array dimensions must be equal under this condition and the size of mapped
! dimensions must match.
!  
! It is erroneous to specify the identical Array object for {\tt srcArray} and
! {\tt dstArray} arguments. 
!
!   The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
!   {\tt ESMF\_ArrayRedist()} on any pair of Arrays that matches 
!   {\tt srcArray} and {\tt dstArray} in {\em type}, {\em kind}, and 
!   memory layout of the {\em distributed} dimensions. However, the size,
!   number, and index order of {\em undistributed} dimensions may be different.
!   See section \ref{RH:Reusability} for a more detailed discussion of
!   RouteHandle reusability.
!
! This method is overloaded for:\newline
! {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8},\newline 
! {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}.
! \newline
!  
! This call is {\em collective} across the current VM.  
!
!   \begin{description}
!
!   \item [srcArray]
!     {\tt ESMF\_Array} with source data.
!
!   \item [dstArray]
!     {\tt ESMF\_Array} with destination data. The data in this Array may be
!     destroyed by this call.
!
!   \item [routehandle]
!     Handle to the precomputed Route.
!
!   \item [factor]
!     Factor by which to multiply source data.
!
!   \item [{[srcToDstTransposeMap]}]
!     List with as many entries as there are dimensions in {\tt srcArray}. Each
!     entry maps the corresponding {\tt srcArray} dimension against the 
!     specified {\tt dstArray} dimension. Mixing of distributed and
!     undistributed dimensions is supported.
!
!   \item [{[ignoreUnmatchedIndices]}]
!     A logical flag that affects the behavior for when not all elements match
!     between the {\tt srcArray} and {\tt dstArray} side. The default setting
!     is {\tt .false.}, indicating that it is an error when such a situation is 
!     encountered. Setting {\tt ignoreUnmatchedIndices} to {\tt .true.} ignores
!     unmatched indices.
!
!   \item [{[pipelineDepth]}]
!     The {\tt pipelineDepth} parameter controls how many messages a PET
!     may have outstanding during a redist exchange. Larger values
!     of {\tt pipelineDepth} typically lead to better performance. However,
!     on some systems too large a value may lead to performance degradation,
!     or runtime errors.
!
!     Note that the pipeline depth has no effect on the bit-for-bit
!     reproducibility of the results. However, it may affect the performance
!     reproducibility of the exchange.
!
!     The {\tt ESMF\_ArraySMMStore()} method implements an auto-tuning scheme
!     for the {\tt pipelineDepth} parameter. The intent on the 
!     {\tt pipelineDepth} argument is "{\tt inout}" in order to 
!     support both overriding and accessing the auto-tuning parameter.
!     If an argument $>= 0$ is specified, it is used for the 
!     {\tt pipelineDepth} parameter, and the auto-tuning phase is skipped.
!     In this case the {\tt pipelineDepth} argument is not modified on
!     return. If the provided argument is $< 0$, the {\tt pipelineDepth}
!     parameter is determined internally using the auto-tuning scheme. In this
!     case the {\tt pipelineDepth} argument is re-set to the internally
!     determined value on return. Auto-tuning is also used if the optional 
!     {\tt pipelineDepth} argument is omitted.
!
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayRedistStoreI4()"
!BOPI
! !IROUTINE: ESMF_ArrayRedistStore - Precompute Array redistribution
!
! !INTERFACE:
  ! Private name; call using ESMF_ArrayRedistStore()
  subroutine ESMF_ArrayRedistStoreI4(srcArray, dstArray, routehandle, &
    factor, keywordEnforcer, srcToDstTransposeMap, ignoreUnmatchedIndices, &
    pipelineDepth, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),           intent(in)              :: srcArray
    type(ESMF_Array),           intent(inout)           :: dstArray
    type(ESMF_RouteHandle),     intent(inout)           :: routehandle
    integer(ESMF_KIND_I4),      intent(in)              :: factor
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                    intent(in),    optional :: srcToDstTransposeMap(:)
    logical,                    intent(in),    optional :: ignoreUnmatchedIndices
    integer,                    intent(inout), optional :: pipelineDepth
    integer,                    intent(out),   optional :: rc
!
!EOPI
!------------------------------------------------------------------------------
    integer               :: localrc                  ! local return code
    type(ESMF_InterArray) :: srcToDstTransposeMapArg  ! index helper
    type(ESMF_Logical)    :: opt_ignoreUnmatched      ! helper variable
    integer, allocatable  :: dstToSrcTransposeMap(:)  ! helper variable
    integer               :: i

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, srcArray, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, dstArray, rc)
    
    ! Deal with (optional) array arguments
    srcToDstTransposeMapArg = ESMF_InterArrayCreate(srcToDstTransposeMap, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set default flags
    opt_ignoreUnmatched = ESMF_FALSE
    if (present(ignoreUnmatchedIndices)) opt_ignoreUnmatched = ignoreUnmatchedIndices

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayRedistStore(srcArray, dstArray, routehandle, &
      srcToDstTransposeMapArg, ESMF_TYPEKIND_I4, factor, opt_ignoreUnmatched, &
      pipelineDepth, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! garbage collection
    call ESMF_InterArrayDestroy(srcToDstTransposeMapArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayRedistStoreI4
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayRedistStoreI8()"
!BOPI
! !IROUTINE: ESMF_ArrayRedistStore - Precompute Array redistribution
!
! !INTERFACE:
  ! Private name; call using ESMF_ArrayRedistStore()
  subroutine ESMF_ArrayRedistStoreI8(srcArray, dstArray, routehandle, &
    factor, keywordEnforcer, srcToDstTransposeMap, ignoreUnmatchedIndices, &
    pipelineDepth, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),           intent(in)              :: srcArray
    type(ESMF_Array),           intent(inout)           :: dstArray
    type(ESMF_RouteHandle),     intent(inout)           :: routehandle
    integer(ESMF_KIND_I8),      intent(in)              :: factor
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                    intent(in),    optional :: srcToDstTransposeMap(:)
    logical,                    intent(in),    optional :: ignoreUnmatchedIndices
    integer,                    intent(inout), optional :: pipelineDepth
    integer,                    intent(out),   optional :: rc
!
!EOPI
!------------------------------------------------------------------------------
    integer               :: localrc                  ! local return code
    type(ESMF_InterArray) :: srcToDstTransposeMapArg  ! index helper
    type(ESMF_Logical)    :: opt_ignoreUnmatched      ! helper variable
    integer, allocatable  :: dstToSrcTransposeMap(:)  ! helper variable
    integer               :: i

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, srcArray, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, dstArray, rc)
    
    ! Deal with (optional) array arguments
    srcToDstTransposeMapArg = ESMF_InterArrayCreate(srcToDstTransposeMap, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set default flags
    opt_ignoreUnmatched = ESMF_FALSE
    if (present(ignoreUnmatchedIndices)) opt_ignoreUnmatched = ignoreUnmatchedIndices

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayRedistStore(srcArray, dstArray, routehandle, &
      srcToDstTransposeMapArg, ESMF_TYPEKIND_I8, factor, opt_ignoreUnmatched, &
      pipelineDepth, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! garbage collection
    call ESMF_InterArrayDestroy(srcToDstTransposeMapArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayRedistStoreI8
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayRedistStoreR4()"
!BOPI
! !IROUTINE: ESMF_ArrayRedistStore - Precompute Array redistribution
!
! !INTERFACE:
  ! Private name; call using ESMF_ArrayRedistStore()
  subroutine ESMF_ArrayRedistStoreR4(srcArray, dstArray, routehandle, &
    factor, keywordEnforcer, srcToDstTransposeMap, ignoreUnmatchedIndices, &
    pipelineDepth, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),           intent(in)              :: srcArray
    type(ESMF_Array),           intent(inout)           :: dstArray
    type(ESMF_RouteHandle),     intent(inout)           :: routehandle
    real(ESMF_KIND_R4),         intent(in)              :: factor
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                    intent(in),    optional :: srcToDstTransposeMap(:)
    logical,                    intent(in),    optional :: ignoreUnmatchedIndices
    integer,                    intent(inout), optional :: pipelineDepth
    integer,                    intent(out),   optional :: rc
!
!EOPI
!------------------------------------------------------------------------------
    integer               :: localrc                  ! local return code
    type(ESMF_InterArray) :: srcToDstTransposeMapArg  ! index helper
    type(ESMF_Logical)    :: opt_ignoreUnmatched      ! helper variable
    integer, allocatable  :: dstToSrcTransposeMap(:)  ! helper variable
    integer               :: i

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, srcArray, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, dstArray, rc)
    
    ! Deal with (optional) array arguments
    srcToDstTransposeMapArg = ESMF_InterArrayCreate(srcToDstTransposeMap, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set default flags
    opt_ignoreUnmatched = ESMF_FALSE
    if (present(ignoreUnmatchedIndices)) opt_ignoreUnmatched = ignoreUnmatchedIndices

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayRedistStore(srcArray, dstArray, routehandle, &
      srcToDstTransposeMapArg, ESMF_TYPEKIND_R4, factor, opt_ignoreUnmatched, &
      pipelineDepth, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! garbage collection
    call ESMF_InterArrayDestroy(srcToDstTransposeMapArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayRedistStoreR4
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayRedistStoreR8()"
!BOPI
! !IROUTINE: ESMF_ArrayRedistStore - Precompute Array redistribution
!
! !INTERFACE:
  ! Private name; call using ESMF_ArrayRedistStore()
  subroutine ESMF_ArrayRedistStoreR8(srcArray, dstArray, routehandle, &
    factor, keywordEnforcer, srcToDstTransposeMap, ignoreUnmatchedIndices, &
    pipelineDepth, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),           intent(in)              :: srcArray
    type(ESMF_Array),           intent(inout)           :: dstArray
    type(ESMF_RouteHandle),     intent(inout)           :: routehandle
    real(ESMF_KIND_R8),         intent(in)              :: factor
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                    intent(in),    optional :: srcToDstTransposeMap(:)
    logical,                    intent(in),    optional :: ignoreUnmatchedIndices
    integer,                    intent(inout), optional :: pipelineDepth
    integer,                    intent(out),   optional :: rc
!
!EOPI
!------------------------------------------------------------------------------
    integer               :: localrc                  ! local return code
    type(ESMF_InterArray) :: srcToDstTransposeMapArg  ! index helper
    type(ESMF_Logical)    :: opt_ignoreUnmatched      ! helper variable
    integer, allocatable  :: dstToSrcTransposeMap(:)  ! helper variable
    integer               :: i

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, srcArray, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, dstArray, rc)
    
    ! Deal with (optional) array arguments
    srcToDstTransposeMapArg = ESMF_InterArrayCreate(srcToDstTransposeMap, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set default flags
    opt_ignoreUnmatched = ESMF_FALSE
    if (present(ignoreUnmatchedIndices)) opt_ignoreUnmatched = ignoreUnmatchedIndices

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayRedistStore(srcArray, dstArray, routehandle, &
      srcToDstTransposeMapArg, ESMF_TYPEKIND_R8, factor, opt_ignoreUnmatched, &
      pipelineDepth, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! garbage collection
    call ESMF_InterArrayDestroy(srcToDstTransposeMapArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayRedistStoreR8
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayRedistStore - Precompute Array redistribution and transpose with local factor argument
!
! !INTERFACE:
! ! Private name; call using ESMF_ArrayRedistStore()
! subroutine ESMF_ArrayRedistStore<type><kind>TP(srcArray, dstArray, &
!   routehandle, transposeRoutehandle, factor, keywordEnforcer, &
!   srcToDstTransposeMap, ignoreUnmatchedIndices, pipelineDepth, rc)
!
! !ARGUMENTS:
!   type(ESMF_Array),       intent(inout)           :: srcArray
!   type(ESMF_Array),       intent(inout)           :: dstArray
!   type(ESMF_RouteHandle), intent(inout)           :: routehandle
!   type(ESMF_RouteHandle), intent(inout)           :: transposeRoutehandle
!   <type>(ESMF_KIND_<kind>),intent(in)             :: factor
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!   integer,                intent(in),    optional :: srcToDstTransposeMap(:)
!   logical,                intent(in),    optional :: ignoreUnmatchedIndices
!   integer,                intent(inout), optional :: pipelineDepth
!   integer,                intent(out),   optional :: rc
!
! !DESCRIPTION:
! \label{ArrayRedistStoreTKTP}
! {\tt ESMF\_ArrayRedistStore()} is a collective method across all PETs of the
! current Component. The interface of the method is overloaded, allowing 
! -- in principle -- each PET to call into {\tt ESMF\_ArrayRedistStore()}
! through a different entry point. Restrictions apply as to which combinations
! are sensible. All other combinations result in ESMF run time errors. The
! complete semantics of the {\tt ESMF\_ArrayRedistStore()} method, as provided
! through the separate entry points shown in \ref{ArrayRedistStoreTKTP} and
! \ref{ArrayRedistStoreNFTP}, is described in the following paragraphs as a whole.
!
! Store an Array redistribution operation from {\tt srcArray} to {\tt dstArray}.
! Interface \ref{ArrayRedistStoreTKTP} allows PETs to specify a {\tt factor}
! argument. PETs not specifying a {\tt factor} argument call into interface
! \ref{ArrayRedistStoreNFTP}. If multiple PETs specify the {\tt factor} argument,
! its type and kind, as well as its value must match across all PETs. If none
! of the PETs specify a {\tt factor} argument the default will be a factor of
! 1. The resulting factor is applied to all of the source data during
! redistribution, allowing scaling of the data, e.g. for unit transformation.
!  
! Both {\tt srcArray} and {\tt dstArray} are interpreted as sequentialized 
! vectors. The sequence is defined by the order of DistGrid dimensions and the
! order of tiles within the DistGrid or by user-supplied arbitrary sequence
! indices. See section \ref{Array:SparseMatMul} for details on the definition
! of {\em sequence indices}.
!
! Source Array, destination Array, and the factor may be of different
! <type><kind>. Further, source and destination Arrays may differ in shape,
! however, the number of elements must match. 
!  
! If {\tt srcToDstTransposeMap} is not specified the redistribution corresponds
! to an identity mapping of the sequentialized source Array to the
! sequentialized destination Array. If the {\tt srcToDstTransposeMap}
! argument is provided it must be identical on all PETs. The
! {\tt srcToDstTransposeMap} allows source and destination Array dimensions to
! be transposed during the redistribution. The number of source and destination
! Array dimensions must be equal under this condition and the size of mapped
! dimensions must match.
!  
! It is erroneous to specify the identical Array object for {\tt srcArray} and
! {\tt dstArray} arguments. 
!
!   The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
!   {\tt ESMF\_ArrayRedist()} on any pair of Arrays that matches 
!   {\tt srcArray} and {\tt dstArray} in {\em type}, {\em kind}, and 
!   memory layout of the {\em distributed} dimensions. However, the size,
!   number, and index order of {\em undistributed} dimensions may be different.
!   See section \ref{RH:Reusability} for a more detailed discussion of
!   RouteHandle reusability.
!
! This method is overloaded for:\newline
! {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8},\newline 
! {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}.
! \newline
!  
! This call is {\em collective} across the current VM.  
!
!   \begin{description}
!
!   \item [srcArray]
!     {\tt ESMF\_Array} with source data. The data in this Array may be
!     destroyed by this call.
!
!   \item [dstArray]
!     {\tt ESMF\_Array} with destination data. The data in this Array may be
!     destroyed by this call.
!
!   \item [routehandle]
!     Handle to the precomputed Route.
!
!   \item [transposeRoutehandle]
!     Handle to the transposed matrix operation. The transposed operation goes
!     from {\tt dstArray} to {\tt srcArray}.
!
!   \item [factor]
!     Factor by which to multiply source data.
!
!   \item [{[srcToDstTransposeMap]}]
!     List with as many entries as there are dimensions in {\tt srcArray}. Each
!     entry maps the corresponding {\tt srcArray} dimension against the 
!     specified {\tt dstArray} dimension. Mixing of distributed and
!     undistributed dimensions is supported.
!
!   \item [{[ignoreUnmatchedIndices]}]
!     A logical flag that affects the behavior for when not all elements match
!     between the {\tt srcArray} and {\tt dstArray} side. The default setting
!     is {\tt .false.}, indicating that it is an error when such a situation is 
!     encountered. Setting {\tt ignoreUnmatchedIndices} to {\tt .true.} ignores
!     unmatched indices.
!
!   \item [{[pipelineDepth]}]
!     The {\tt pipelineDepth} parameter controls how many messages a PET
!     may have outstanding during a redist exchange. Larger values
!     of {\tt pipelineDepth} typically lead to better performance. However,
!     on some systems too large a value may lead to performance degradation,
!     or runtime errors.
!
!     Note that the pipeline depth has no effect on the bit-for-bit
!     reproducibility of the results. However, it may affect the performance
!     reproducibility of the exchange.
!
!     The {\tt ESMF\_ArraySMMStore()} method implements an auto-tuning scheme
!     for the {\tt pipelineDepth} parameter. The intent on the 
!     {\tt pipelineDepth} argument is "{\tt inout}" in order to 
!     support both overriding and accessing the auto-tuning parameter.
!     If an argument $>= 0$ is specified, it is used for the 
!     {\tt pipelineDepth} parameter, and the auto-tuning phase is skipped.
!     In this case the {\tt pipelineDepth} argument is not modified on
!     return. If the provided argument is $< 0$, the {\tt pipelineDepth}
!     parameter is determined internally using the auto-tuning scheme. In this
!     case the {\tt pipelineDepth} argument is re-set to the internally
!     determined value on return. Auto-tuning is also used if the optional 
!     {\tt pipelineDepth} argument is omitted.
!
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayRedistStoreI4TP()"
!BOPI
! !IROUTINE: ESMF_ArrayRedistStore - Precompute Array redistribution
!
! !INTERFACE:
  ! Private name; call using ESMF_ArrayRedistStore()
  subroutine ESMF_ArrayRedistStoreI4TP(srcArray, dstArray, routehandle, &
    transposeRoutehandle, factor, keywordEnforcer, srcToDstTransposeMap, &
    ignoreUnmatchedIndices, pipelineDepth, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),           intent(inout)           :: srcArray
    type(ESMF_Array),           intent(inout)           :: dstArray
    type(ESMF_RouteHandle),     intent(inout)           :: routehandle
    type(ESMF_RouteHandle),     intent(inout)           :: transposeRoutehandle
    integer(ESMF_KIND_I4),      intent(in)              :: factor
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                    intent(in),    optional :: srcToDstTransposeMap(:)
    logical,                    intent(in),    optional :: ignoreUnmatchedIndices
    integer,                    intent(inout), optional :: pipelineDepth
    integer,                    intent(out),   optional :: rc
!
!EOPI
!------------------------------------------------------------------------------
    integer               :: localrc                  ! local return code
    type(ESMF_InterArray) :: srcToDstTransposeMapArg  ! index helper
    type(ESMF_Logical)    :: opt_ignoreUnmatched      ! helper variable
    integer, allocatable  :: dstToSrcTransposeMap(:)  ! helper variable
    integer               :: i

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, srcArray, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, dstArray, rc)
    
    ! Deal with (optional) array arguments
    srcToDstTransposeMapArg = ESMF_InterArrayCreate(srcToDstTransposeMap, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set default flags
    opt_ignoreUnmatched = ESMF_FALSE
    if (present(ignoreUnmatchedIndices)) opt_ignoreUnmatched = ignoreUnmatchedIndices

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayRedistStore(srcArray, dstArray, routehandle, &
      srcToDstTransposeMapArg, ESMF_TYPEKIND_I4, factor, opt_ignoreUnmatched, &
      pipelineDepth, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! Compute the transposeRoutehandle
    ! Invert the srcToDstTransposeMap
    if (present(srcToDstTransposeMap)) then
      allocate(dstToSrcTransposeMap(size(srcToDstTransposeMap)))
      do i=1, size(srcToDstTransposeMap)
        dstToSrcTransposeMap(srcToDstTransposeMap(i))=i
      enddo
      ! garbage collection
      call ESMF_InterArrayDestroy(srcToDstTransposeMapArg, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! create new srcToDstTransposeMapArg
      srcToDstTransposeMapArg = ESMF_InterArrayCreate(dstToSrcTransposeMap,&
        rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayRedistStore(dstArray, srcArray, transposeRoutehandle, &
      srcToDstTransposeMapArg, ESMF_TYPEKIND_I4, factor, opt_ignoreUnmatched, &
      pipelineDepth, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! Mark transposeRoutehandle object as being created
    call ESMF_RouteHandleSetInitCreated(transposeRoutehandle, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! cleanup
    if (present(srcToDstTransposeMap)) then
      deallocate(dstToSrcTransposeMap)
    endif
    
    ! garbage collection
    call ESMF_InterArrayDestroy(srcToDstTransposeMapArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayRedistStoreI4TP
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayRedistStoreI8TP()"
!BOPI
! !IROUTINE: ESMF_ArrayRedistStore - Precompute Array redistribution
!
! !INTERFACE:
  ! Private name; call using ESMF_ArrayRedistStore()
  subroutine ESMF_ArrayRedistStoreI8TP(srcArray, dstArray, routehandle, &
    transposeRoutehandle, factor, keywordEnforcer, srcToDstTransposeMap, &
    ignoreUnmatchedIndices, pipelineDepth, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),           intent(inout)           :: srcArray
    type(ESMF_Array),           intent(inout)           :: dstArray
    type(ESMF_RouteHandle),     intent(inout)           :: routehandle
    type(ESMF_RouteHandle),     intent(inout)           :: transposeRoutehandle
    integer(ESMF_KIND_I8),      intent(in)              :: factor
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                    intent(in),    optional :: srcToDstTransposeMap(:)
    logical,                    intent(in),    optional :: ignoreUnmatchedIndices
    integer,                    intent(inout), optional :: pipelineDepth
    integer,                    intent(out),   optional :: rc
!
!EOPI
!------------------------------------------------------------------------------
    integer               :: localrc                  ! local return code
    type(ESMF_InterArray) :: srcToDstTransposeMapArg  ! index helper
    type(ESMF_Logical)    :: opt_ignoreUnmatched      ! helper variable
    integer, allocatable  :: dstToSrcTransposeMap(:)  ! helper variable
    integer               :: i

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, srcArray, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, dstArray, rc)
    
    ! Deal with (optional) array arguments
    srcToDstTransposeMapArg = ESMF_InterArrayCreate(srcToDstTransposeMap, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set default flags
    opt_ignoreUnmatched = ESMF_FALSE
    if (present(ignoreUnmatchedIndices)) opt_ignoreUnmatched = ignoreUnmatchedIndices

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayRedistStore(srcArray, dstArray, routehandle, &
      srcToDstTransposeMapArg, ESMF_TYPEKIND_I8, factor, opt_ignoreUnmatched, &
      pipelineDepth, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Compute the transposeRoutehandle
    ! Invert the srcToDstTransposeMap
    if (present(srcToDstTransposeMap)) then
      allocate(dstToSrcTransposeMap(size(srcToDstTransposeMap)))
      do i=1, size(srcToDstTransposeMap)
        dstToSrcTransposeMap(srcToDstTransposeMap(i))=i
      enddo
      ! garbage collection
      call ESMF_InterArrayDestroy(srcToDstTransposeMapArg, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! create new srcToDstTransposeMapArg
      srcToDstTransposeMapArg = ESMF_InterArrayCreate(dstToSrcTransposeMap,&
        rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayRedistStore(dstArray, srcArray, transposeRoutehandle, &
      srcToDstTransposeMapArg, ESMF_TYPEKIND_I4, factor, opt_ignoreUnmatched, &
      pipelineDepth, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! Mark transposeRoutehandle object as being created
    call ESMF_RouteHandleSetInitCreated(transposeRoutehandle, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! cleanup
    if (present(srcToDstTransposeMap)) then
      deallocate(dstToSrcTransposeMap)
    endif
    
    ! garbage collection
    call ESMF_InterArrayDestroy(srcToDstTransposeMapArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayRedistStoreI8TP
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayRedistStoreR4TP()"
!BOPI
! !IROUTINE: ESMF_ArrayRedistStore - Precompute Array redistribution
!
! !INTERFACE:
  ! Private name; call using ESMF_ArrayRedistStore()
  subroutine ESMF_ArrayRedistStoreR4TP(srcArray, dstArray, routehandle, &
    transposeRoutehandle, factor, keywordEnforcer, srcToDstTransposeMap, &
    ignoreUnmatchedIndices, pipelineDepth, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),           intent(inout)           :: srcArray
    type(ESMF_Array),           intent(inout)           :: dstArray
    type(ESMF_RouteHandle),     intent(inout)           :: transposeRoutehandle
    type(ESMF_RouteHandle),     intent(inout)           :: routehandle
    real(ESMF_KIND_R4),         intent(in)              :: factor
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                    intent(in),    optional :: srcToDstTransposeMap(:)
    logical,                    intent(in),    optional :: ignoreUnmatchedIndices
    integer,                    intent(inout), optional :: pipelineDepth
    integer,                    intent(out),   optional :: rc
!
!EOPI
!------------------------------------------------------------------------------
    integer               :: localrc                  ! local return code
    type(ESMF_InterArray) :: srcToDstTransposeMapArg  ! index helper
    type(ESMF_Logical)    :: opt_ignoreUnmatched      ! helper variable
    integer, allocatable  :: dstToSrcTransposeMap(:)  ! helper variable
    integer               :: i

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, srcArray, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, dstArray, rc)
    
    ! Deal with (optional) array arguments
    srcToDstTransposeMapArg = ESMF_InterArrayCreate(srcToDstTransposeMap, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set default flags
    opt_ignoreUnmatched = ESMF_FALSE
    if (present(ignoreUnmatchedIndices)) opt_ignoreUnmatched = ignoreUnmatchedIndices

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayRedistStore(srcArray, dstArray, routehandle, &
      srcToDstTransposeMapArg, ESMF_TYPEKIND_R4, factor, opt_ignoreUnmatched, &
      pipelineDepth, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Compute the transposeRoutehandle
    ! Invert the srcToDstTransposeMap
    if (present(srcToDstTransposeMap)) then
      allocate(dstToSrcTransposeMap(size(srcToDstTransposeMap)))
      do i=1, size(srcToDstTransposeMap)
        dstToSrcTransposeMap(srcToDstTransposeMap(i))=i
      enddo
      ! garbage collection
      call ESMF_InterArrayDestroy(srcToDstTransposeMapArg, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! create new srcToDstTransposeMapArg
      srcToDstTransposeMapArg = ESMF_InterArrayCreate(dstToSrcTransposeMap,&
        rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayRedistStore(dstArray, srcArray, transposeRoutehandle, &
      srcToDstTransposeMapArg, ESMF_TYPEKIND_I4, factor, opt_ignoreUnmatched, &
      pipelineDepth, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! Mark transposeRoutehandle object as being created
    call ESMF_RouteHandleSetInitCreated(transposeRoutehandle, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! cleanup
    if (present(srcToDstTransposeMap)) then
      deallocate(dstToSrcTransposeMap)
    endif
    
    ! garbage collection
    call ESMF_InterArrayDestroy(srcToDstTransposeMapArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayRedistStoreR4TP
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayRedistStoreR8TP()"
!BOPI
! !IROUTINE: ESMF_ArrayRedistStore - Precompute Array redistribution
!
! !INTERFACE:
  ! Private name; call using ESMF_ArrayRedistStore()
  subroutine ESMF_ArrayRedistStoreR8TP(srcArray, dstArray, routehandle, &
    transposeRoutehandle, factor, keywordEnforcer, srcToDstTransposeMap, &
    ignoreUnmatchedIndices, pipelineDepth, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),           intent(inout)           :: srcArray
    type(ESMF_Array),           intent(inout)           :: dstArray
    type(ESMF_RouteHandle),     intent(inout)           :: routehandle
    type(ESMF_RouteHandle),     intent(inout)           :: transposeRoutehandle
    real(ESMF_KIND_R8),         intent(in)              :: factor
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                    intent(in),    optional :: srcToDstTransposeMap(:)
    logical,                    intent(in),    optional :: ignoreUnmatchedIndices
    integer,                    intent(inout), optional :: pipelineDepth
    integer,                    intent(out),   optional :: rc
!
!EOPI
!------------------------------------------------------------------------------
    integer               :: localrc                  ! local return code
    type(ESMF_InterArray) :: srcToDstTransposeMapArg  ! index helper
    type(ESMF_Logical)    :: opt_ignoreUnmatched      ! helper variable
    integer, allocatable  :: dstToSrcTransposeMap(:)  ! helper variable
    integer               :: i

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, srcArray, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, dstArray, rc)
    
    ! Deal with (optional) array arguments
    srcToDstTransposeMapArg = ESMF_InterArrayCreate(srcToDstTransposeMap, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set default flags
    opt_ignoreUnmatched = ESMF_FALSE
    if (present(ignoreUnmatchedIndices)) opt_ignoreUnmatched = ignoreUnmatchedIndices

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayRedistStore(srcArray, dstArray, routehandle, &
      srcToDstTransposeMapArg, ESMF_TYPEKIND_R8, factor, opt_ignoreUnmatched, &
      pipelineDepth, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Compute the transposeRoutehandle
    ! Invert the srcToDstTransposeMap
    if (present(srcToDstTransposeMap)) then
      allocate(dstToSrcTransposeMap(size(srcToDstTransposeMap)))
      do i=1, size(srcToDstTransposeMap)
        dstToSrcTransposeMap(srcToDstTransposeMap(i))=i
      enddo
      ! garbage collection
      call ESMF_InterArrayDestroy(srcToDstTransposeMapArg, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! create new srcToDstTransposeMapArg
      srcToDstTransposeMapArg = ESMF_InterArrayCreate(dstToSrcTransposeMap,&
        rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayRedistStore(dstArray, srcArray, transposeRoutehandle, &
      srcToDstTransposeMapArg, ESMF_TYPEKIND_I4, factor, opt_ignoreUnmatched, &
      pipelineDepth, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! Mark transposeRoutehandle object as being created
    call ESMF_RouteHandleSetInitCreated(transposeRoutehandle, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! cleanup
    if (present(srcToDstTransposeMap)) then
      deallocate(dstToSrcTransposeMap)
    endif
    
    ! garbage collection
    call ESMF_InterArrayDestroy(srcToDstTransposeMapArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayRedistStoreR8TP
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayRedistStore()"
!BOP
! !IROUTINE: ESMF_ArrayRedistStore - Precompute Array redistribution without local factor argument
!
! !INTERFACE:
  ! Private name; call using ESMF_ArrayRedistStore()
  subroutine ESMF_ArrayRedistStoreNF(srcArray, dstArray, routehandle, &
    keywordEnforcer, srcToDstTransposeMap, ignoreUnmatchedIndices, &
    pipelineDepth, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),       intent(in)              :: srcArray
    type(ESMF_Array),       intent(inout)           :: dstArray
    type(ESMF_RouteHandle), intent(inout)           :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),    optional :: srcToDstTransposeMap(:)
    logical,                intent(in),    optional :: ignoreUnmatchedIndices
    integer,                intent(inout), optional :: pipelineDepth
    integer,                intent(out),   optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[6.1.0] Added argument {\tt pipelineDepth}.
!              The new argument provide access to the tuning parameter
!              affecting the sparse matrix execution.
! \item[7.0.0] Added argument {\tt transposeRoutehandle} to allow a handle to
!              the transposed redist operation to be returned.\newline
!              Added argument {\tt ignoreUnmatchedIndices} to support situations 
!              where not all elements between source and destination Arrays 
!              match.
! \item[7.1.0r] Removed argument {\tt transposeRoutehandle} and provide it
!              via interface overloading instead. This allows argument 
!              {\tt srcArray} to stay strictly intent(in) for this entry point.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
! \label{ArrayRedistStoreNF}
! {\tt ESMF\_ArrayRedistStore()} is a collective method across all PETs of the
! current Component. The interface of the method is overloaded, allowing 
! -- in principle -- each PET to call into {\tt ESMF\_ArrayRedistStore()}
! through a different entry point. Restrictions apply as to which combinations
! are sensible. All other combinations result in ESMF run time errors. The
! complete semantics of the {\tt ESMF\_ArrayRedistStore()} method, as provided
! through the separate entry points shown in \ref{ArrayRedistStoreTK} and
! \ref{ArrayRedistStoreNF}, is described in the following paragraphs as a whole.
!
! Store an Array redistribution operation from {\tt srcArray} to {\tt dstArray}.
! Interface \ref{ArrayRedistStoreTK} allows PETs to specify a {\tt factor}
! argument. PETs not specifying a {\tt factor} argument call into interface
! \ref{ArrayRedistStoreNF}. If multiple PETs specify the {\tt factor} argument,
! its type and kind, as well as its value must match across all PETs. If none
! of the PETs specify a {\tt factor} argument the default will be a factor of
! 1. The resulting factor is applied to all of the source data during
! redistribution, allowing scaling of the data, e.g. for unit transformation.
!  
! Both {\tt srcArray} and {\tt dstArray} are interpreted as sequentialized 
! vectors. The sequence is defined by the order of DistGrid dimensions and the
! order of tiles within the DistGrid or by user-supplied arbitrary sequence
! indices. See section \ref{Array:SparseMatMul} for details on the definition
! of {\em sequence indices}.
!
! Source Array, destination Array, and the factor may be of different
! <type><kind>. Further, source and destination Arrays may differ in shape,
! however, the number of elements must match. 
!  
! If {\tt srcToDstTransposeMap} is not specified the redistribution corresponds
! to an identity mapping of the sequentialized source Array to the
! sequentialized destination Array. If the {\tt srcToDstTransposeMap}
! argument is provided it must be identical on all PETs. The
! {\tt srcToDstTransposeMap} allows source and destination Array dimensions to
! be transposed during the redistribution. The number of source and destination
! Array dimensions must be equal under this condition and the size of mapped
! dimensions must match.
!  
! It is erroneous to specify the identical Array object for {\tt srcArray} and
! {\tt dstArray} arguments. 
!
!   The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
!   {\tt ESMF\_ArrayRedist()} on any pair of Arrays that matches 
!   {\tt srcArray} and {\tt dstArray} in {\em type}, {\em kind}, and 
!   memory layout of the {\em distributed} dimensions. However, the size,
!   number, and index order of {\em undistributed} dimensions may be different.
!   See section \ref{RH:Reusability} for a more detailed discussion of
!   RouteHandle reusability.
!
! This call is {\em collective} across the current VM.  
!
!   \begin{description}
!
!   \item [srcArray]
!     {\tt ESMF\_Array} with source data.
!
!   \item [dstArray]
!     {\tt ESMF\_Array} with destination data. The data in this Array may be
!     destroyed by this call.
!
!   \item [routehandle]
!     Handle to the precomputed Route.
!
!   \item [{[srcToDstTransposeMap]}]
!     List with as many entries as there are dimensions in {\tt srcArray}. Each
!     entry maps the corresponding {\tt srcArray} dimension against the 
!     specified {\tt dstArray} dimension. Mixing of distributed and
!     undistributed dimensions is supported.
!
!   \item [{[ignoreUnmatchedIndices]}]
!     A logical flag that affects the behavior for when not all elements match
!     between the {\tt srcArray} and {\tt dstArray} side. The default setting
!     is {\tt .false.}, indicating that it is an error when such a situation is 
!     encountered. Setting {\tt ignoreUnmatchedIndices} to {\tt .true.} ignores
!     unmatched indices.
!
!   \item [{[pipelineDepth]}]
!     The {\tt pipelineDepth} parameter controls how many messages a PET
!     may have outstanding during a redist exchange. Larger values
!     of {\tt pipelineDepth} typically lead to better performance. However,
!     on some systems too large a value may lead to performance degradation,
!     or runtime errors.
!
!     Note that the pipeline depth has no effect on the bit-for-bit
!     reproducibility of the results. However, it may affect the performance
!     reproducibility of the exchange.
!
!     The {\tt ESMF\_ArraySMMStore()} method implements an auto-tuning scheme
!     for the {\tt pipelineDepth} parameter. The intent on the 
!     {\tt pipelineDepth} argument is "{\tt inout}" in order to 
!     support both overriding and accessing the auto-tuning parameter.
!     If an argument $>= 0$ is specified, it is used for the 
!     {\tt pipelineDepth} parameter, and the auto-tuning phase is skipped.
!     In this case the {\tt pipelineDepth} argument is not modified on
!     return. If the provided argument is $< 0$, the {\tt pipelineDepth}
!     parameter is determined internally using the auto-tuning scheme. In this
!     case the {\tt pipelineDepth} argument is re-set to the internally
!     determined value on return. Auto-tuning is also used if the optional 
!     {\tt pipelineDepth} argument is omitted.
!
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                  ! local return code
    type(ESMF_InterArray) :: srcToDstTransposeMapArg  ! index helper
    type(ESMF_Logical)    :: opt_ignoreUnmatched      ! helper variable
    integer, allocatable  :: dstToSrcTransposeMap(:)  ! helper variable
    integer               :: i

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, srcArray, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, dstArray, rc)

    ! Deal with (optional) array arguments
    srcToDstTransposeMapArg = ESMF_InterArrayCreate(srcToDstTransposeMap, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set default flags
    opt_ignoreUnmatched = ESMF_FALSE
    if (present(ignoreUnmatchedIndices)) opt_ignoreUnmatched = ignoreUnmatchedIndices

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayRedistStoreNF(srcArray, dstArray, routehandle, &
      srcToDstTransposeMapArg, opt_ignoreUnmatched, pipelineDepth, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! garbage collection
    call ESMF_InterArrayDestroy(srcToDstTransposeMapArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayRedistStoreNF
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayRedistStore()"
!BOP
! !IROUTINE: ESMF_ArrayRedistStore - Precompute Array redistribution and transpose without local factor argument
!
! !INTERFACE:
  ! Private name; call using ESMF_ArrayRedistStore()
  subroutine ESMF_ArrayRedistStoreNFTP(srcArray, dstArray, routehandle, &
    transposeRoutehandle, keywordEnforcer, srcToDstTransposeMap, &
    ignoreUnmatchedIndices, pipelineDepth, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),       intent(inout)           :: srcArray
    type(ESMF_Array),       intent(inout)           :: dstArray
    type(ESMF_RouteHandle), intent(inout)           :: routehandle
    type(ESMF_RouteHandle), intent(inout)           :: transposeRoutehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),    optional :: srcToDstTransposeMap(:)
    logical,                intent(in),    optional :: ignoreUnmatchedIndices
    integer,                intent(inout), optional :: pipelineDepth
    integer,                intent(out),   optional :: rc
!
! !DESCRIPTION:
! \label{ArrayRedistStoreNFTP}
! {\tt ESMF\_ArrayRedistStore()} is a collective method across all PETs of the
! current Component. The interface of the method is overloaded, allowing 
! -- in principle -- each PET to call into {\tt ESMF\_ArrayRedistStore()}
! through a different entry point. Restrictions apply as to which combinations
! are sensible. All other combinations result in ESMF run time errors. The
! complete semantics of the {\tt ESMF\_ArrayRedistStore()} method, as provided
! through the separate entry points shown in \ref{ArrayRedistStoreTKTP} and
! \ref{ArrayRedistStoreNFTP}, is described in the following paragraphs as a whole.
!
! Store an Array redistribution operation from {\tt srcArray} to {\tt dstArray}.
! Interface \ref{ArrayRedistStoreTKTP} allows PETs to specify a {\tt factor}
! argument. PETs not specifying a {\tt factor} argument call into interface
! \ref{ArrayRedistStoreNFTP}. If multiple PETs specify the {\tt factor} argument,
! its type and kind, as well as its value must match across all PETs. If none
! of the PETs specify a {\tt factor} argument the default will be a factor of
! 1. The resulting factor is applied to all of the source data during
! redistribution, allowing scaling of the data, e.g. for unit transformation.
!  
! Both {\tt srcArray} and {\tt dstArray} are interpreted as sequentialized 
! vectors. The sequence is defined by the order of DistGrid dimensions and the
! order of tiles within the DistGrid or by user-supplied arbitrary sequence
! indices. See section \ref{Array:SparseMatMul} for details on the definition
! of {\em sequence indices}.
!
! Source Array, destination Array, and the factor may be of different
! <type><kind>. Further, source and destination Arrays may differ in shape,
! however, the number of elements must match. 
!  
! If {\tt srcToDstTransposeMap} is not specified the redistribution corresponds
! to an identity mapping of the sequentialized source Array to the
! sequentialized destination Array. If the {\tt srcToDstTransposeMap}
! argument is provided it must be identical on all PETs. The
! {\tt srcToDstTransposeMap} allows source and destination Array dimensions to
! be transposed during the redistribution. The number of source and destination
! Array dimensions must be equal under this condition and the size of mapped
! dimensions must match.
!  
! It is erroneous to specify the identical Array object for {\tt srcArray} and
! {\tt dstArray} arguments. 
!
!   The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
!   {\tt ESMF\_ArrayRedist()} on any pair of Arrays that matches 
!   {\tt srcArray} and {\tt dstArray} in {\em type}, {\em kind}, and 
!   memory layout of the {\em distributed} dimensions. However, the size,
!   number, and index order of {\em undistributed} dimensions may be different.
!   See section \ref{RH:Reusability} for a more detailed discussion of
!   RouteHandle reusability.
!
! This call is {\em collective} across the current VM.  
!
!   \begin{description}
!
!   \item [srcArray]
!     {\tt ESMF\_Array} with source data. The data in this Array may be
!     destroyed by this call.
!
!   \item [dstArray]
!     {\tt ESMF\_Array} with destination data. The data in this Array may be
!     destroyed by this call.
!
!   \item [routehandle]
!     Handle to the precomputed Route.
!
!   \item [transposeRoutehandle]
!     Handle to the transposed matrix operation. The transposed operation goes
!     from {\tt dstArray} to {\tt srcArray}.
!
!   \item [{[srcToDstTransposeMap]}]
!     List with as many entries as there are dimensions in {\tt srcArray}. Each
!     entry maps the corresponding {\tt srcArray} dimension against the 
!     specified {\tt dstArray} dimension. Mixing of distributed and
!     undistributed dimensions is supported.
!
!   \item [{[ignoreUnmatchedIndices]}]
!     A logical flag that affects the behavior for when not all elements match
!     between the {\tt srcArray} and {\tt dstArray} side. The default setting
!     is {\tt .false.}, indicating that it is an error when such a situation is 
!     encountered. Setting {\tt ignoreUnmatchedIndices} to {\tt .true.} ignores
!     unmatched indices.
!
!   \item [{[pipelineDepth]}]
!     The {\tt pipelineDepth} parameter controls how many messages a PET
!     may have outstanding during a redist exchange. Larger values
!     of {\tt pipelineDepth} typically lead to better performance. However,
!     on some systems too large a value may lead to performance degradation,
!     or runtime errors.
!
!     Note that the pipeline depth has no effect on the bit-for-bit
!     reproducibility of the results. However, it may affect the performance
!     reproducibility of the exchange.
!
!     The {\tt ESMF\_ArraySMMStore()} method implements an auto-tuning scheme
!     for the {\tt pipelineDepth} parameter. The intent on the 
!     {\tt pipelineDepth} argument is "{\tt inout}" in order to 
!     support both overriding and accessing the auto-tuning parameter.
!     If an argument $>= 0$ is specified, it is used for the 
!     {\tt pipelineDepth} parameter, and the auto-tuning phase is skipped.
!     In this case the {\tt pipelineDepth} argument is not modified on
!     return. If the provided argument is $< 0$, the {\tt pipelineDepth}
!     parameter is determined internally using the auto-tuning scheme. In this
!     case the {\tt pipelineDepth} argument is re-set to the internally
!     determined value on return. Auto-tuning is also used if the optional 
!     {\tt pipelineDepth} argument is omitted.
!
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                  ! local return code
    type(ESMF_InterArray) :: srcToDstTransposeMapArg  ! index helper
    type(ESMF_Logical)    :: opt_ignoreUnmatched      ! helper variable
    integer, allocatable  :: dstToSrcTransposeMap(:)  ! helper variable
    integer               :: i

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, srcArray, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, dstArray, rc)
    
    ! Deal with (optional) array arguments
    srcToDstTransposeMapArg = ESMF_InterArrayCreate(srcToDstTransposeMap, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set default flags
    opt_ignoreUnmatched = ESMF_FALSE
    if (present(ignoreUnmatchedIndices)) opt_ignoreUnmatched = ignoreUnmatchedIndices

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayRedistStoreNF(srcArray, dstArray, routehandle, &
      srcToDstTransposeMapArg, opt_ignoreUnmatched, pipelineDepth, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Compute the transposeRoutehandle
    ! Invert the srcToDstTransposeMap
    if (present(srcToDstTransposeMap)) then
      allocate(dstToSrcTransposeMap(size(srcToDstTransposeMap)))
      do i=1, size(srcToDstTransposeMap)
        dstToSrcTransposeMap(srcToDstTransposeMap(i))=i
      enddo
      ! garbage collection
      call ESMF_InterArrayDestroy(srcToDstTransposeMapArg, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! create new srcToDstTransposeMapArg
      srcToDstTransposeMapArg = ESMF_InterArrayCreate(dstToSrcTransposeMap,&
        rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayRedistStoreNF(dstArray, srcArray, transposeRoutehandle, &
      srcToDstTransposeMapArg, opt_ignoreUnmatched, pipelineDepth, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! Mark transposeRoutehandle object as being created
    call ESMF_RouteHandleSetInitCreated(transposeRoutehandle, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! cleanup
    if (present(srcToDstTransposeMap)) then
      deallocate(dstToSrcTransposeMap)
    endif
    
    ! garbage collection
    call ESMF_InterArrayDestroy(srcToDstTransposeMapArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayRedistStoreNFTP
!------------------------------------------------------------------------------


end module ESMF_ArrayHaMod
