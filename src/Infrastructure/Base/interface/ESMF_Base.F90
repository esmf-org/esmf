! $Id: ESMF_Base.F90,v 1.142.2.1 2010/02/05 19:53:57 svasquez Exp $
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
#define ESMF_FILENAME "ESMF_Base.F90"
!==============================================================================
!
! ESMF Base Module
!
! (all lines between the !BOP and !EOP markers will be included in the
! automated document processing.)
!------------------------------------------------------------------------------
! one blank line for protex processing - in case all routines here are
! marked internal (BOPI/EOPI), the output file will still have contents.
!BOP

!EOP

!------------------------------------------------------------------------------
! module definition

module ESMF_BaseMod
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!BOPI
! !MODULE: ESMF_BaseMod - Base class for all ESMF classes
!
! !DESCRIPTION:
!
! The code in this file implements the Base defined type
!  and functions which operate on all types.  This is an
!  interface to the actual C++ base class implementation 
!  in the ../src dir.
!
! See the ESMF Developers Guide document for more details.
!
!------------------------------------------------------------------------------
! !USES:
  use ESMF_UtilTypesMod     ! ESMF utility types
  use ESMF_InitMacrosMod    ! ESMF initializer macros
  use ESMF_LogErrMod        ! ESMF error handling

  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
!

  ! Contains pointer to real Base object which is defined in C++
  type ESMF_Base
  sequence
  !private
    type(ESMF_Pointer) :: this
    ESMF_INIT_DECLARE
  end type


!------------------------------------------------------------------------------
!
! !DESCRIPTION:
!     The following routines apply to any type in the system.  
!     The attribute routines can be inherited as-is.  The other
!     routines need to be specialized by the higher level objects.
!
! !PUBLIC MEMBER FUNCTIONS:
!
!   Classes
       public ESMF_Base

!   Base class methods
       public ESMF_BaseCreate
       public ESMF_BaseDestroy
   
!      public ESMF_BaseGetInstCount

!      public ESMF_BaseSetID
!      public ESMF_BaseGetID

!      public ESMF_BaseSetRefCount
!      public ESMF_BaseGetRefCount

       public ESMF_BaseSetBaseStatus
       public ESMF_BaseGetBaseStatus

       public ESMF_BaseSetStatus
       public ESMF_BaseGetStatus

       public ESMF_BasePrint
       public ESMF_BaseValidate
       
       public ESMF_BaseGetInit
       public ESMF_BaseSetInitCreated

!   Virtual methods to be defined by derived classes
!      public ESMF_Read
!      public ESMF_Write
!      public ESMF_Validate
!      public ESMF_Print

!  Misc methods - work on Base object but apply to any type
      public ESMF_SetName
      public ESMF_GetName

!

!==============================================================================
!
! INTERFACE BLOCKS
!
!
!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
      character(*), parameter, private :: version = &
               '$Id: ESMF_Base.F90,v 1.142.2.1 2010/02/05 19:53:57 svasquez Exp $'
!------------------------------------------------------------------------------

      contains

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! 
! Base methods
!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BaseCreate"
!BOPI
! !IROUTINE:  ESMF_BaseCreate - Create and initialize a Base object
!
! !INTERFACE:
  subroutine ESMF_BaseCreate(base, superclass, name, nattr, rc)
!
! !ARGUMENTS:
      type(ESMF_Base) :: base                 
      character(len=*), intent(in) :: superclass
      character(len=*), intent(in), optional :: name
      integer, intent(in), optional :: nattr 
      integer, intent(out), optional :: rc     

!
! !DESCRIPTION:
!     Set initial state on a Base object.
!
!     \begin{description}
!     \item [base]
!           An {\tt ESMF\_Base} derived type.  It is expected that all 
!           specialized derived types will include an {\tt ESMF\_Base} 
!           object as the first entry.
!     \item [superclass]
!           The name of the superclass, e.g. {\tt "IGrid"}, {\tt "Array"}.
!           This sets the scope for unique object names.
!     \item [{[name]}]
!           If given, the unique name for this object.  If not given,
!           a unique name will be generated.  
!     \item [{[nattr]}]
!           If given, the initial number of attributes to allocate space for.
!           Additional attributes can be added at any time, but it will be
!           more efficient if space is allocated at create time.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOPI

    integer :: status, allocNAttrs

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    allocNAttrs = 0   ! default value, overwrite if argument specified
    if (present(nattr)) allocNAttrs = nattr

    if (present(name)) then
        call c_ESMC_BaseCreate(base , superclass, name, allocNattrs, status)
    else
        !!call c_ESMC_BaseCreate(base , superclass, ESMF_NULL_POINTER, &
        call c_ESMC_BaseCreate(base , superclass, "", allocNattrs, status)
    endif
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Set init code
    ESMF_INIT_SET_CREATED(base)
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_BaseCreate


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BaseDestroy"
!BOPI
! !IROUTINE:  ESMF_BaseDestroy - Release resources from a Base object
!
! !INTERFACE:
  subroutine ESMF_BaseDestroy(base, rc)
!
! !ARGUMENTS:
    type(ESMF_Base)                         :: base                 
    integer,        intent(out),  optional  :: rc     

!
! !DESCRIPTION:
!     Release resources held by a Base object.
!
!     \begin{description}
!     \item [base]
!           An {\tt ESMF\_Base} derived type to be deleted.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

    logical :: rcpresent                          ! Return code present   
    integer :: status

    ! Initialize return code
    rcpresent = .FALSE.
    if(present(rc)) then
      rcpresent = .TRUE.
      rc = ESMF_RC_NOT_IMPL
    endif

    ! check input parameters
    ESMF_INIT_CHECK_DEEP(ESMF_BaseGetInit,base,rc)

    call c_ESMC_BaseDestroy(base , status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set init code
    ESMF_INIT_SET_DELETED(base)
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_BaseDestroy


!-------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_SetName"
!BOPI
! !IROUTINE:  ESMF_SetName - set the name of this object
!
! !INTERFACE:
  subroutine ESMF_SetName(base, name, namespace, rc)
!
! !ARGUMENTS:
      type(ESMF_Base) :: base                 
      character (len = *), intent(in), optional :: name   
      character (len = *), intent(in), optional :: namespace
      integer, intent(out), optional :: rc     

!
! !DESCRIPTION:
!     Associate a name with any object in the system.
!
!     \begin{description}
!     \item [base]
!           In the Fortran interface this must be an {\tt ESMF\_Base}
!           derived type object.  It is expected that all specialized 
!           derived types will include a {\tt ESMF\_Base} object as the 
!           first entry.
!     \item [{[name]}]
!           Object name.  An error will be returned if a duplicate name 
!           is specified.  If a name is not given a unique name will be
!           generated and can be queried by the {\tt ESMF\_GetName} routine.
!     \item [{[namespace]}]
!           Object namespace (e.g. "Application", "Component", "IGrid", etc).
!           If given, the name will be checked that it is unique within
!           this namespace.  If not given, the generated name will be 
!           unique within this namespace.  If namespace is not specified,
!           a default "global" namespace will be assumed and the same rules
!           for names will be followed.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! 
!EOPI
      logical :: rcpresent                          ! Return code present   
      integer :: status

      ! Initialize return code; assume routine not implemented
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_RC_NOT_IMPL
      endif
      status = ESMF_RC_NOT_IMPL

      ! TODO: remove this once everyone is initializing their Base objects.
      ! cheat for old code for now.
      if (base%isInit .ne. ESMF_INIT_CREATED) then 
          call ESMF_BaseCreate(base, namespace, name, 0, status)
          if (rcpresent) rc = status
          return
      endif
      ! end cheat

      call c_ESMC_SetName(base , namespace, name, status)

      if (rcpresent) rc = status

  end subroutine ESMF_SetName

!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GetName"
!BOPI
! !IROUTINE:  ESMF_GetName - get the name of this object
!
! !INTERFACE:
  subroutine ESMF_GetName(base, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: base
      character (len = *), intent(out) :: name
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!     Return the name of any type in the system.
!
!     The arguments are:
!     \begin{description}
!     \item[base]
!       Any ESMF type.
!     \item[name]
!       The name of the ESMF type.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
      integer :: status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      call c_ESMC_GetName(base , name, status)
      if (present(rc)) rc = status

  end subroutine ESMF_GetName


!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BaseSetBaseStatus"
!BOPI
! !IROUTINE:  ESMF_BaseSetBaseStatus - set the baseStatus
!
! !INTERFACE:
  subroutine ESMF_BaseSetBaseStatus(base, baseStatus, rc)
!
! !ARGUMENTS:
      type(ESMF_Base),    intent(in)            :: base
      type(ESMF_Status),  intent(in)            :: baseStatus
      integer,            intent(out), optional :: rc

!
! !DESCRIPTION:
!     Set the baseStatus
!
!     The arguments are:
!     \begin{description}
!     \item[base]
!       Any ESMF type.
!     \item[baseStatus]
!       baseStatus to set.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
      integer :: localrc

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      call c_ESMC_BaseSetBaseStatus(base, baseStatus, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      
      ! return successfully
      if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_BaseSetBaseStatus


!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BaseGetBaseStatus"
!BOPI
! !IROUTINE:  ESMF_BaseGetBaseStatus - get the baseStatus
!
! !INTERFACE:
  subroutine ESMF_BaseGetBaseStatus(base, baseStatus, rc)
!
! !ARGUMENTS:
      type(ESMF_Base),    intent(in)            :: base
      type(ESMF_Status),  intent(out)           :: baseStatus
      integer,            intent(out), optional :: rc

!
! !DESCRIPTION:
!     Return the baseStatus
!
!     The arguments are:
!     \begin{description}
!     \item[base]
!       Any ESMF type.
!     \item[baseStatus]
!       baseStatus to set.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
      integer :: localrc

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      call c_ESMC_BaseGetBaseStatus(base, baseStatus, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      
      ! return successfully
      if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_BaseGetBaseStatus
  
  
!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BaseSetStatus"
!BOPI
! !IROUTINE:  ESMF_BaseSetStatus - set the status
!
! !INTERFACE:
  subroutine ESMF_BaseSetStatus(base, status, rc)
!
! !ARGUMENTS:
      type(ESMF_Base),    intent(in)            :: base
      type(ESMF_Status),  intent(in)            :: status
      integer,            intent(out), optional :: rc

!
! !DESCRIPTION:
!     Set the status
!
!     The arguments are:
!     \begin{description}
!     \item[base]
!       Any ESMF type.
!     \item[status]
!       status to set.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
      integer :: localrc

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      call c_ESMC_BaseSetStatus(base, status, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      
      ! return successfully
      if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_BaseSetStatus


!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BaseGetStatus"
!BOPI
! !IROUTINE:  ESMF_BaseGetStatus - get the status
!
! !INTERFACE:
  subroutine ESMF_BaseGetStatus(base, status, rc)
!
! !ARGUMENTS:
      type(ESMF_Base),    intent(in)            :: base
      type(ESMF_Status),  intent(out)           :: status
      integer,            intent(out), optional :: rc

!
! !DESCRIPTION:
!     Get the ststus
!
!     The arguments are:
!     \begin{description}
!     \item[base]
!       Any ESMF type.
!     \item[status]
!       status returned.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
      integer :: localrc

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      call c_ESMC_BaseGetStatus(base, status, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      
      ! return successfully
      if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_BaseGetStatus
  
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Print routine
!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BasePrint"
!BOPI
! !IROUTINE:  ESMF_BasePrint - Call into C++ code to print base object
!
! !INTERFACE:
  subroutine ESMF_BasePrint(base, options, rc)
!
! !ARGUMENTS:
    type(ESMF_Base),  intent(in)              :: base
    character(len=*), intent(in),   optional  :: options
    integer,          intent(out),  optional  :: rc
!
! !DESCRIPTION:
!     Interface to call through to C++ and print base object values. \\
!
!     Note:  Many {\tt ESMF\_<class>Print} methods are implemented in C++.
!     On some platforms/compilers there is a potential issue with interleaving
!     Fortran and C++ output to {\tt stdout} such that it doesn't appear in
!     the expected order.  If this occurs, the {\tt ESMF\_IOUnitFlush()} method
!     may be used on unit 6 to get coherent output.  \\
!
!     The arguments are:
!     \begin{description}
!     \item[base]
!       Any ESMF type.
!     \item[options]
!       Print options.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI
    integer                     :: localrc
    character(len=ESMF_MAXSTR)  :: opts

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_BaseGetInit, base, rc)

    if (present(options)) then
        opts = options
    else
        opts = ''
    endif

    call c_ESMC_BasePrint(base , opts, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_BasePrint

!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BaseValidate"
!BOPI
! !IROUTINE:  ESMF_BaseValidate - Call into C++ code to print base object
!
! !INTERFACE:
  subroutine ESMF_BaseValidate(base, options, rc)
!
! !ARGUMENTS:
    type(ESMF_Base),  intent(in)              :: base
    character(len=*), intent(in),   optional  :: options
    integer,          intent(out),  optional  :: rc
!
! !DESCRIPTION:
!  Interface to call through to C++ and validate base object values.
!
!     The arguments are:
!     \begin{description}
!     \item[base]
!       Any ESMF type.
!     \item[options]
!       Validate options.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI
    integer                     :: localrc
    character(len=ESMF_MAXSTR)  :: opts

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_BaseGetInit, base, rc)

    if (present(options)) then
        opts = options
    else
        opts = ''
    endif

    call c_ESMC_BaseValidate(base , opts, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_BaseValidate
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BaseGetInit"
!BOPI
! !IROUTINE: ESMF_BaseGetInit - Internal access routine for init code
!
! !INTERFACE:
  function ESMF_BaseGetInit(base) 
!
! !RETURN VALUE:
    ESMF_INIT_TYPE :: ESMF_BaseGetInit   
!
! !ARGUMENTS:
    type(ESMF_Base), intent(in), optional :: base
!
! !DESCRIPTION:
!      Access deep object init code.
!
!     The arguments are:
!     \begin{description}
!     \item [base]
!           Base object.
!     \end{description}
!
!EOPI

    if (present(base)) then
      ESMF_BaseGetInit = ESMF_INIT_GET(base)
    else
      ESMF_BaseGetInit = ESMF_INIT_CREATED
    endif

  end function ESMF_BaseGetInit
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_BaseSetInitCreated()"
!BOPI
! !IROUTINE: ESMF_BaseSetInitCreated - Set Base init code to "CREATED"

! !INTERFACE:
  subroutine ESMF_BaseSetInitCreated(base, rc)
!
! !ARGUMENTS:
    type(ESMF_Base), intent(inout) :: base
    integer, intent(out), optional :: rc
!
!
! !DESCRIPTION:
! Set init code in Base object to "CREATED".
!
! The arguments are:
! \begin{description}
! \item[base]
! Specified {\tt ESMF\_Base} object.
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! Set init code
    ESMF_INIT_SET_CREATED(base)

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_BaseSetInitCreated
!------------------------------------------------------------------------------


end module ESMF_BaseMod
