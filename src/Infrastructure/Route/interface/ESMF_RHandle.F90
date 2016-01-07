! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_RHandle.F90"
!==============================================================================
!
! ESMF RHandle Module
module ESMF_RHandleMod
!
!==============================================================================
!
! This file contains the Fortran wrapper code for the C++ implementation of
!  the RouteHandle class.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!BOPI
! !MODULE: ESMF_RHandleMod
!
! !DESCRIPTION:
!
!   Fortran API wrapper of C++ implementation of RouteHandle
!
!------------------------------------------------------------------------------

! !USES:
  use ESMF_UtilTypesMod     ! ESMF utility types
  use ESMF_InitMacrosMod    ! ESMF initializer macros
  use ESMF_BaseMod          ! ESMF base class
  use ESMF_LogErrMod        ! ESMF error handling
  use ESMF_IOUtilMod
  
  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
! !  ESMF_RouteHandleType
! !  MUST STAY IN SYNC WITH C++ header file
!
  integer, parameter :: ESMF_UNINITIALIZEDHANDLE=1, &
                        ESMF_ARRAYSPARSEMATMULHANDLE=2

!------------------------------------------------------------------------------
! !  ESMF_RouteHandle
!
  type ESMF_RouteHandle
#ifndef ESMF_NO_SEQUENCE
    sequence
#endif
    private
    type(ESMF_Pointer) :: this    ! opaque pointer to C++ class data
    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_RouteHandle
  public ESMF_UNINITIALIZEDHANDLE, ESMF_ARRAYSPARSEMATMULHANDLE

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
  public ESMF_RouteHandleGetInit
  public ESMF_RouteHandleSetInitCreated

  public ESMF_RouteHandleCreate
  public ESMF_RouteHandleDestroy

  public ESMF_RouteHandleIsCreated
  
  public ESMF_RouteHandleRelease

  public ESMF_RouteHandlePrepXXE
  public ESMF_RouteHandleAppendClear
  
  public ESMF_RouteHandleGet
  public ESMF_RouteHandleSet
 
  public ESMF_RouteHandleValidate
  public ESMF_RouteHandlePrint
  
  public ESMF_RouteHandleOptimize

  public ESMF_RouteHandleCopyThis
 
!EOPI
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id$'


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleGetInit"
!BOPI
! !IROUTINE: ESMF_RouteHandleGetInit - Get the Init status 

! !INTERFACE:
  function ESMF_RouteHandleGetInit(d)
!
! !RETURN VALUE:
    ESMF_INIT_TYPE :: ESMF_RouteHandleGetInit
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(in),optional :: d
!
! !DESCRIPTION:
!   Get the init status
!
!   The arguments are:
!   \begin{description}
!   \item[d] 
!     The class to be queried 
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    if (present(d)) then
      ESMF_RouteHandleGetInit=ESMF_INIT_GET(d)
    else
      ESMF_RouteHandleGetInit=ESMF_INIT_CREATED
    endif 
  end function ESMF_RouteHandleGetInit
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleSetInitCreated()"
!BOPI
! !IROUTINE: ESMF_RouteHandleSetInitCreated - Set RouteHandle init code to "CREATED"

! !INTERFACE:
  subroutine ESMF_RouteHandleSetInitCreated(rh, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(inout)           :: rh
    integer,                intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!   Set init code in RouteHandle object to "CREATED".
!
!   The arguments are:
!   \begin{description}
!   \item[rh] 
!     Specified {\tt ESMF\_RouteHandle} object.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Set init code
    ESMF_INIT_SET_CREATED(rh)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_RouteHandleSetInitCreated
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleCreate"
!BOPI
! !IROUTINE: ESMF_RouteHandleCreate - Create a new RouteHandle

! !INTERFACE:
  function ESMF_RouteHandleCreate(rc)
!
! !RETURN VALUE:
    type(ESMF_RouteHandle) :: ESMF_RouteHandleCreate
!
! !ARGUMENTS:
    integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!   Allocates memory for a new {\tt ESMF\_RouteHandle} object and 
!   constructs its internals.
!
!   The arguments are:
!   \begin{description}
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_RouteHandle)  :: rhandle

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    rhandle%this = ESMF_NULL_POINTER

    ! Call C++ create code
    call c_ESMC_RouteHandleCreate(rhandle, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set return values
    ESMF_RouteHandleCreate = rhandle

    ESMF_INIT_SET_CREATED(ESMF_RouteHandleCreate)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_RouteHandleCreate
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleDestroy"
!BOPI
! !IROUTINE: ESMF_RouteHandleDestroy - Release resources associated with a RouteHandle

! !INTERFACE:
  subroutine ESMF_RouteHandleDestroy(rhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(inout) :: rhandle   
    integer, intent(out), optional :: rc        
!
! !DESCRIPTION:
!   Destroys an {\tt ESMF\_RouteHandle}, releaseing the resources associated
!   with the object.
!
!   The arguments are:
!   \begin{description}
!   \item[rhandle] 
!     The {\tt ESMF\_RouteHandle} to be destroyed.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check input variable
    ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,rhandle,rc)

    ! was handle already destroyed?
    if (rhandle%this .eq. ESMF_NULL_POINTER) then
      if (present(rc)) rc = ESMF_SUCCESS
      return
    endif 

    ! Call C++ destroy code
    call c_ESMC_RouteHandleDestroy(rhandle, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! nullify pointer
    rhandle%this = ESMF_NULL_POINTER
    ESMF_INIT_SET_DELETED(rhandle)
    
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_RouteHandleDestroy
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleIsCreated()"
!BOP
! !IROUTINE: ESMF_RouteHandleIsCreated - Check whether a RouteHandle object has been created

! !INTERFACE:
  function ESMF_RouteHandleIsCreated(routehandle, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_RouteHandleIsCreated
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(in)            :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt routehandle} has been created. Otherwise return 
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is 
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[routehandle]
!     {\tt ESMF\_RouteHandle} queried.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------    
    ESMF_RouteHandleIsCreated = .false.   ! initialize
    if (present(rc)) rc = ESMF_SUCCESS
    if (ESMF_RouteHandleGetInit(routehandle)==ESMF_INIT_CREATED) &
      ESMF_RouteHandleIsCreated = .true.
  end function
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleRelease"
!BOPI
! !IROUTINE: ESMF_RouteHandleRelease - Release all RouteHandle resources

! !INTERFACE:
  subroutine ESMF_RouteHandleRelease(routehandle, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(inout) :: routehandle   
    integer, intent(out), optional :: rc        
!
! !DESCRIPTION:
!   Same as {\tt ESMF\_RouteHandleDestroy}.
!
!   The arguments are:
!   \begin{description}
!   \item[routehandle] 
!     The {\tt ESMF\_RouteHandle} to be released.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check input variable
    ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,routehandle,rc)

    call ESMF_RouteHandleDestroy(routehandle, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc))  return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_RouteHandleRelease
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandlePrepXXE"
!BOPI
! !IROUTINE: ESMF_RouteHandlePrepXXE - Prepare RouteHandle for XXE based comms

! !INTERFACE:
  subroutine ESMF_RouteHandlePrepXXE(rhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(inout) :: rhandle
    integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!   Set an {\tt ESMF\_RouteHandle} attribute with the given value.
!
!   The arguments are:
!   \begin{description}
!   \item[rhandle] 
!     {\tt ESMF\_RouteHandle} to be prepared.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,rhandle,rc)
    
    call c_ESMC_RouteHandlePrepXXE(rhandle, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_RouteHandlePrepXXE
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleAppendClear"
!BOPI
! !IROUTINE: ESMF_RouteHandleAppendClear - Append XXE based RouteHandle and clear

! !INTERFACE:
  subroutine ESMF_RouteHandleAppendClear(rhandle, appendRoutehandle, &
    rraShift, vectorLengthShift, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(inout) :: rhandle
    type(ESMF_RouteHandle), intent(inout) :: appendRoutehandle
    integer, intent(in)                   :: rraShift
    integer, intent(in)                   :: vectorLengthShift
    integer, intent(out), optional        :: rc            

!
! !DESCRIPTION:
!   Set an {\tt ESMF\_RouteHandle} attribute with the given value.
!
!   The arguments are:
!   \begin{description}
!   \item[rhandle] 
!     {\tt ESMF\_RouteHandle} to be appended to.
!   \item[appendRoutehandle] 
!     {\tt ESMF\_RouteHandle} to be appended and cleared.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,rhandle,rc)
    
    call c_ESMC_RouteHandleAppendClear(rhandle, appendRoutehandle, &
      rraShift, vectorLengthShift, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_RouteHandleAppendClear
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleGet"
!BOPI
! !IROUTINE: ESMF_RouteHandleGet - Get values from a RouteHandle

! !INTERFACE:
  subroutine ESMF_RouteHandleGet(rhandle, htype, name, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(in) :: rhandle
    integer, intent(out), optional :: htype
    character(len=*), intent(out), optional :: name
    integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Returns information about an {\tt ESMF\_RouteHandle}.
!
!     The arguments are:
!     \begin{description}
!     \item[rhandle] 
!          {\tt ESMF\_RouteHandle} to be queried.
!     \item[{[htype]}]
!          Route type.
!     \item [{[name]}]
!          Name of the RouteHandle object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,rhandle,rc)

    if (present(htype)) then
      call c_ESMC_RouteHandleGetType(rhandle, htype, localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    if (present(name)) then
      call c_ESMC_GetName(rhandle, name, localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_RouteHandleGet
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleSet"
!BOPI
! !IROUTINE: ESMF_RouteHandleSet - Set values in a RouteHandle

! !INTERFACE:
  subroutine ESMF_RouteHandleSet(rhandle, htype, name, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(in) :: rhandle
    integer, intent(in), optional :: htype
    character(len = *), intent(in),   optional  :: name    
    integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!   Set an {\tt ESMF\_RouteHandle} attribute with the given value.
!
!   The arguments are:
!   \begin{description}
!   \item[rhandle] 
!     {\tt ESMF\_RouteHandle} to be modified.
!   \item[{[htype]}]
!     Route type.
!   \item [{[name]}]
!     The RouteHandle name.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,rhandle,rc)
    
    if (present(htype)) then
      call c_ESMC_RouteHandleSetType(rhandle, htype, localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    if (present(name)) then
      call c_ESMC_SetName(rhandle, "RouteHandle", name, localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_RouteHandleSet
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleValidate"
!BOPI
! !IROUTINE: ESMF_RouteHandleValidate - Check internal consistency of a RouteHandle

! !INTERFACE:
  subroutine ESMF_RouteHandleValidate(rhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(in)            :: rhandle       
    integer,                intent(out), optional :: rc            
!
! !DESCRIPTION:
!   Validates that an {\tt ESMF\_RouteHandle} is internally consistent.
!
!   The arguments are:
!   \begin{description}
!   \item[rhandle] 
!     {\tt ESMF\_RouteHandle} to be queried.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,rhandle,rc)

    call c_ESMC_RouteHandleValidate(rhandle, localrc)   
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set return values
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_RouteHandleValidate
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandlePrint"
!BOP
! !IROUTINE: ESMF_RouteHandlePrint - Print the contents of a RouteHandle

! !INTERFACE:
  subroutine ESMF_RouteHandlePrint(rhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(in)            :: rhandle      
    integer,                intent(out), optional :: rc           
!
! !DESCRIPTION:
!   Print information about an {\tt ESMF\_RouteHandle}.
!
!   The arguments are:
!   \begin{description}
!   \item[rhandle] 
!     {\tt ESMF\_RouteHandle} to print contents of.
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

    ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,rhandle,rc)

    call ESMF_UtilIOUnitFlush (ESMF_UtilIOStdout, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call c_ESMC_RouteHandlePrint(rhandle, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set return values
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_RouteHandlePrint
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleOptimize"
!BOPI
! !IROUTINE: ESMF_RouteHandleOptimize - Optimization based on a RouteHandle

! !INTERFACE:
  subroutine ESMF_RouteHandleOptimize(rhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(in)            :: rhandle      
    integer,                intent(out), optional :: rc           
!
! !DESCRIPTION:
!   Optimize communications based on the information available in the
!   {\tt ESMF\_RouteHandle} object.
!
!   The arguments are:
!   \begin{description}
!   \item[rhandle] 
!     {\tt ESMF\_RouteHandle} holding the communication patter for which the
!     optimization is carried out.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,rhandle,rc)

    call c_ESMC_RouteHandleOptimize(rhandle, localrc)   
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set return values
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_RouteHandleOptimize
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleCopyThis()"
!BOPI
! !IROUTINE: ESMF_RouteHandleCopyThis - Copy RouteHandle this member

! !INTERFACE:
  subroutine ESMF_RouteHandleCopyThis(rhandleIn, rhandleOut, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(in)              :: rhandleIn
    type(ESMF_RouteHandle), intent(inout)           :: rhandleOut
    integer,                intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!     Copy RouteHandle this member. Do not set init code.
!
!     The arguments are:
!     \begin{description}
!     \item[rhandleIn] 
!          Input {\tt ESMF\_RouteHandle} object.
!     \item[rhandleOut] 
!          Output {\tt ESMF\_RouteHandle} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL
    
    ! Copy this member
    rhandleOut%this = rhandleIn%this

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_RouteHandleCopyThis
!------------------------------------------------------------------------------

end module ESMF_RHandleMod
