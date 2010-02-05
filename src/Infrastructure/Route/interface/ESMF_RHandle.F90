! $Id: ESMF_RHandle.F90,v 1.47.2.1 2010/02/05 19:59:59 svasquez Exp $
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
    sequence
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
  
  public ESMF_RouteHandleRelease

  public ESMF_RouteHandleGet
  public ESMF_RouteHandleSet
 
  public ESMF_RouteHandleValidate
  public ESMF_RouteHandlePrint
 
!EOPI
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_RHandle.F90,v 1.47.2.1 2010/02/05 19:59:59 svasquez Exp $'


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
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return

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
! !IROUTINE: ESMF_RouteHandleDestroy - Free all resources associated with a RouteHandle 

! !INTERFACE:
  subroutine ESMF_RouteHandleDestroy(rhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(inout) :: rhandle   
    integer, intent(out), optional :: rc        
!
! !DESCRIPTION:
!   Destroys an {\tt ESMF\_RouteHandle} object previously allocated
!   via an {\tt ESMF_RouteHandleCreate()} routine.
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
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return

    ! nullify pointer
    rhandle%this = ESMF_NULL_POINTER
    ESMF_INIT_SET_DELETED(rhandle)
    
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_RouteHandleDestroy
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
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc))  return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_RouteHandleRelease
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
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
    endif

    if (present(name)) then
      call c_ESMC_GetName(rhandle, name, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
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
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
    endif

    if (present(name)) then
      call c_ESMC_SetName(rhandle, "RouteHandle", name, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
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
    type(ESMF_RouteHandle), intent(in) :: rhandle       
    integer, intent(out), optional :: rc            
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

    ! See if this has been created yet or not.
    if ((rhandle%this).eq.ESMF_NULL_POINTER) then
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      return
    endif

    ! TODO: the following code is commented out because the C-side
    !       validate routine is empty
    !    call c_ESMC_RouteHandleValidate(rhandle, localrc)   
    !if (ESMF_LogMsgFoundError(localrc, &
    !                           ESMF_ERR_PASSTHRU, &
    !                           ESMF_CONTEXT, rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_RouteHandleValidate
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandlePrint"
!BOPI
! !IROUTINE: ESMF_RouteHandlePrint - Print the contents of a RouteHandle

! !INTERFACE:
      subroutine ESMF_RouteHandlePrint(rhandle, options, rc)
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(in) :: rhandle      
      character (len=*), intent(in), optional :: options      
      integer, intent(out), optional :: rc           
!
! !DESCRIPTION:
!     Print information about an {\tt ESMF\_RouteHandle}. \\
!
!     Note:  Many {\tt ESMF\_<class>Print} methods are implemented in C++.
!     On some platforms/compilers there is a potential issue with interleaving
!     Fortran and C++ output to {\tt stdout} such that it doesn't appear in
!     the expected order.  If this occurs, the {\tt ESMF\_IOUnitFlush()} method
!     may be used on unit 6 to get coherent output.  \\
!
!     The arguments are:
!     \begin{description}
!     \item[rhandle] 
!          {\tt ESMF\_RouteHandle} to print contents of.
!     \item[{[options]}]
!          Print options that control the type of information and level of 
!          detail. 
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    character (len=6) :: defaultopts      ! default print options
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,rhandle,rc)

    defaultopts = "brief"

    if(present(options)) then
      call c_ESMC_RouteHandlePrint(rhandle, options, localrc)   
    else
      call c_ESMC_RouteHandlePrint(rhandle, defaultopts, localrc)
    endif

    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return

    ! Set return values
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_RouteHandlePrint
!------------------------------------------------------------------------------

end module ESMF_RHandleMod
