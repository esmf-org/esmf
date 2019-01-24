! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
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
  use ESMF_IOUtilMod        ! ESMF I/O utilities
  use ESMF_LogErrMod        ! ESMF error handling
  use ESMF_VMMod            ! ESMF VM

  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
!

  ! Contains pointer to real Base object which is defined in C++
  type ESMF_Base
#ifndef ESMF_NO_SEQUENCE
  sequence
#endif
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
       public ESMF_BaseGetID
       public ESMF_BaseSetVMId
       public ESMF_BaseGetVMId

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

       public ESMF_BaseSerialize
       public ESMF_BaseDeserialize
       public ESMF_BaseDeserializeIDVMId

!   Virtual methods to be defined by derived classes
!      public ESMF_Read
!      public ESMF_Write
!      public ESMF_Validate
!      public ESMF_Print

!  Misc methods - work on Base object but apply to any type
      public ESMF_SetName
      public ESMF_GetName
      public ESMF_GetVM
      public ESMF_IsProxy

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
               '$Id$'
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

    integer :: localrc, allocNAttrs

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    allocNAttrs = 0   ! default value, overwrite if argument specified
    if (present(nattr)) allocNAttrs = nattr

    if (present(name)) then
        call c_ESMC_BaseCreate(base , superclass, name, allocNattrs, localrc)
    else
        !!call c_ESMC_BaseCreate(base , superclass, ESMF_NULL_POINTER, &
        call c_ESMC_BaseCreate(base , superclass, "", allocNattrs, localrc)
    endif
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
  subroutine ESMF_BaseDestroy(base, noGarbage, rc)
!
! !ARGUMENTS:
    type(ESMF_Base)                        :: base                 
    logical,        intent(in),   optional :: noGarbage
    integer,        intent(out),  optional :: rc     

!
! !DESCRIPTION:
!     Release resources held by a Base object.
!
!     \begin{description}
!     \item [base]
!           An {\tt ESMF\_Base} derived type to be deleted.
! \item[{[noGarbage]}]
!      If set to {\tt .TRUE.} the object will be fully destroyed and removed
!      from the ESMF garbage collection system. Note however that under this 
!      condition ESMF cannot protect against accessing the destroyed object 
!      through dangling aliases -- a situation which may lead to hard to debug 
!      application crashes.
! 
!      It is generally recommended to leave the {\tt noGarbage} argument
!      set to {\tt .FALSE.} (the default), and to take advantage of the ESMF 
!      garbage collection system which will prevent problems with dangling
!      aliases or incorrect sequences of destroy calls. However this level of
!      support requires that a small remnant of the object is kept in memory
!      past the destroy call. This can lead to an unexpected increase in memory
!      consumption over the course of execution in applications that use 
!      temporary ESMF objects. For situations where the repeated creation and 
!      destruction of temporary objects leads to memory issues, it is 
!      recommended to call with {\tt noGarbage} set to {\tt .TRUE.}, fully 
!      removing the entire temporary object from memory.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

    logical :: rcpresent                          ! Return code present   
    integer :: localrc
    type(ESMF_Logical)      :: opt_noGarbage  ! helper variable

    ! Initialize return code
    rcpresent = .FALSE.
    if(present(rc)) then
      rcpresent = .TRUE.
      rc = ESMF_RC_NOT_IMPL
    endif

    ! check input parameters
    ESMF_INIT_CHECK_DEEP(ESMF_BaseGetInit,base,rc)

    ! Set default flags
    opt_noGarbage = ESMF_FALSE
    if (present(noGarbage)) opt_noGarbage = noGarbage

    ! Call into the C++ interface
    call c_ESMC_BaseDestroy(base, opt_noGarbage, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
      integer :: localrc

      ! Initialize return code; assume routine not implemented
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_RC_NOT_IMPL
      endif
      localrc = ESMF_RC_NOT_IMPL

      ! TODO: remove this once everyone is initializing their Base objects.
      ! cheat for old code for now.
      if (base%isInit .ne. ESMF_INIT_CREATED) then 
          call ESMF_BaseCreate(base, namespace, name, 0, localrc)
          if (rcpresent) rc = localrc
          return
      endif
      ! end cheat

      call c_ESMC_SetName(base , namespace, name, localrc)

      if (rcpresent) rc = localrc

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
      integer :: localrc

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      call c_ESMC_GetName(base , name, localrc)
      if (present(rc)) rc = localrc

  end subroutine ESMF_GetName


!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BaseGetId"
!BOPI
! !IROUTINE:  ESMF_BaseGetId - get the ESMF object ID of this object
!
! !INTERFACE:
  subroutine ESMF_BaseGetId(base, id, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in)             :: base
      integer,         intent(out)            :: id
      integer,         intent(out), optional  :: rc

!
! !DESCRIPTION:
!     Return the ESMF object Id.
!
!     The arguments are:
!     \begin{description}
!     \item[base]
!       Any ESMF type.
!     \item[id]
!       The object ID of the Base object.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
      integer :: localrc

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      call c_ESMC_GetId (base , id, localrc)
      if (present(rc)) rc = localrc

  end subroutine ESMF_BaseGetId


!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GetVM"
!BOPI
! !IROUTINE:  ESMF_GetVM - get the VM of this object
!
! !INTERFACE:
  subroutine ESMF_GetVM(base, vm, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in)             :: base
      type(ESMF_VM),   intent(out)            :: vm
      integer,         intent(out), optional  :: rc

!
! !DESCRIPTION:
!     Return the vm of any type in the system.
!
!     The arguments are:
!     \begin{description}
!     \item[base]
!       Any ESMF type.
!     \item[vm]
!       The vm on which the Base object was created.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
      integer :: localrc

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      call c_ESMC_GetVM(base , vm, localrc)
      if (present(rc)) rc = localrc

  end subroutine ESMF_GetVM


!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BaseSetVMId"
!BOPI
! !IROUTINE:  ESMF_SetVMId - get the VM Id of this object
!
! !INTERFACE:
  subroutine ESMF_BaseSetVMId (base, vmid, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(inout)          :: base
      type(ESMF_VMId), intent(in)             :: vmid
      integer,         intent(out), optional  :: rc

!
! !DESCRIPTION:
!     Set the VMId of any type in the system.
!
!     The arguments are:
!     \begin{description}
!     \item[base]
!       Base object of any ESMF type.
!     \item[vmid]
!       The vmid of the Base object.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
      integer :: localrc

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      call c_ESMC_SetVMID(base , vmid, localrc)
      if (present(rc)) rc = localrc

  end subroutine ESMF_BaseSetVMId

!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BaseGetVMId"
!BOPI
! !IROUTINE:  ESMF_GetVMId - get the VM Id of this object
!
! !INTERFACE:
  subroutine ESMF_BaseGetVMId (base, vmid, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in)             :: base
      type(ESMF_VMId), intent(out)            :: vmid
      integer,         intent(out), optional  :: rc

!
! !DESCRIPTION:
!     Return the VMId of any type in the system.
!
!     The arguments are:
!     \begin{description}
!     \item[base]
!       Any ESMF type.
!     \item[vmid]
!       The vmid of the Base object.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
      integer :: localrc

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      call c_ESMC_GetVMID(base , vmid, localrc)
      if (present(rc)) rc = localrc

  end subroutine ESMF_BaseGetVMId


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
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
  recursive subroutine ESMF_BaseGetStatus(base, status, rc)
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
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
  subroutine ESMF_BasePrint(base, options, filename, rc)
!
! !ARGUMENTS:
    type(ESMF_Base),  intent(in)              :: base
    character(len=*), intent(in),   optional  :: options
    character(len=*), intent(in),   optional  :: filename
    integer,          intent(out),  optional  :: rc
!
! !DESCRIPTION:
!     Interface to call through to C++ and print base object values. \\
!
!     The arguments are:
!     \begin{description}
!     \item[base]
!       Any ESMF type.
!     \item[options]
!       Print options.
!     \item[{[filename]}]
!       Used to determine whether to write to file or stdout.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI
    integer                     :: localrc
    integer                     :: ignore_iostat
    character(len=ESMF_MAXSTR)  :: opts
    type(ESMF_Logical)          :: tofile

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

    tofile = present (filename)

    call ESMF_UtilIOUnitFlush (unit=ESMF_UtilIOstdout, rc=ignore_iostat)
    ! Ignore iostat, because sometimes stdout is not open at this point
    ! and some compilers FLUSH statements will complain.

    call c_ESMC_BasePrint(base, 0, opts, tofile, filename, ESMF_TRUE, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BaseSerialize"
!BOPI
! !IROUTINE: ESMF_BaseSerialize - serialize into a byte stream
!
! !INTERFACE:
  subroutine ESMF_BaseSerialize(base, buffer, offset, &
      attreconflag, inquireflag, rc) 
!
! !ARGUMENTS:
    type(ESMF_Base), intent(in)    :: base
    character,       intent(inout) :: buffer(:)
    integer,         intent(inout) :: offset
    type(ESMF_AttReconcileFlag), intent(in) :: attreconflag
    type(ESMF_InquireFlag),      intent(in) :: inquireflag
    integer,         intent(out), optional  :: rc 
!
! !DESCRIPTION:
!      Takes an {\tt ESMF\_Base} object and adds all the information needed
!      to save the information to a file or recreate the object based on this
!      information.   Expected to be used by {\tt ESMF\_StateReconcile()}.
!
!     The arguments are:
!     \begin{description}
!     \item [base]
!           {\tt ESMF\_Base} object to be serialized.
!     \item [buffer]
!           Data buffer which will hold the serialized information.
!     \item [offset]
!           Current write offset in the current buffer.  This will be
!           updated by this routine and return pointing to the next
!           available byte in the buffer.  Note that the offset might be
!           overestimated when the ESMF\_INQUIREONLY option is used.
!     \item[attreconflag]
!           Flag to tell if Attribute serialization is to be done
!     \item[inquireflag]
!           Flag to tell if serialization is to be done (ESMF\_NOINQUIRE)
!           or if this is simply a size inquiry (ESMF\_INQUIREONLY)
!     \item [rc]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
    integer :: localrc

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    call c_ESMC_BaseSerialize(base, buffer, size (buffer), offset, &
        attreconflag, inquireflag, localrc)
    if (ESMF_LogFoundError(localrc, &
        msg="Top level Base serialize", &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    ! Return successfully
    if (present (rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_BaseSerialize
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BaseDeserialize"
!BOPI
! !IROUTINE: ESMF_BaseDeserialize - Deserialize from a buffer
!
! !INTERFACE:
  function ESMF_BaseDeserialize (buffer, offset, attreconflag, rc) 
!
! !RETURN VALUE:
    type(ESMF_Base) :: ESMF_BaseDeserialize
!
! !ARGUMENTS:
    character,       intent(in)    :: buffer(:)
    integer,         intent(inout) :: offset
    type(ESMF_AttReconcileFlag), intent(in) :: attreconflag
    integer,         intent(out), optional  :: rc
!
! !DESCRIPTION:
!      Recreates a {\tt ESMF\_Base} object from a serialized byte stream.
!      Expected to be used by {\tt ESMF\_StateReconcile()}.
!
!     The arguments are:
!     \begin{description}
!     \item [buffer]
!           Data buffer of serialized information.
!     \item [offset]
!           Current read offset in the current buffer.  This will be
!           updated by this routine and return pointing to the next
!           available byte in the buffer.
!     \item[attreconflag]
!           Flag to tell if Attribute deserialization is to be done
!     \item [rc]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
    integer :: localrc

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    call c_ESMC_BaseDeserialize(ESMF_BaseDeserialize,  &
        buffer, offset, &
        attreconflag, localrc)
    if (ESMF_LogFoundError(localrc, &
        msg="Top level Base Deserialize", &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    ! Return successfully
    if (present (rc)) rc = ESMF_SUCCESS

  end function ESMF_BaseDeserialize
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BaseDeserializeIDVMId"
!BOPI
! !IROUTINE: ESMF_BaseDeserializeIDVMId - Deserialize from a buffer
!
! !INTERFACE:
  subroutine ESMF_BaseDeserializeIDVMId (buffer, offset, ID, VMId, objname, rc)
!
! !ARGUMENTS:
    character,       intent(in)    :: buffer(:)
    integer,         intent(in)    :: offset
    integer,         intent(out)   :: ID
    type(ESMF_VMId), intent(inout) :: VMId
    character(*),    intent(out)   :: objname
    integer,         intent(out)   :: rc
!
! !DESCRIPTION:
!      Obtains the ID and VMId from a {\tt ESMF\_Base} object in a
!      serialized byte stream. Expected to be used by {\tt ESMF\_StateReconcile()}
!      and friends.
!
!     The arguments are:
!     \begin{description}
!     \item [buffer]
!           Data buffer of serialized information.
!     \item [offset]
!           Current read offset in the current buffer.
!     \item[ID]
!           Returns the ESMF object ID
!     \item[VMId]
!           Returns the ESMF object VMId
!     \item [rc]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
    integer :: localrc

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    rc = ESMF_RC_NOT_IMPL

    call c_ESMC_BaseDeserialize_idvmid(buffer, offset, &
        ID, VMId, objname, localrc)
    if (ESMF_LogFoundError(localrc, &
        msg="Base ID/VMId inquiry", &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    ! Return successfully
    rc = ESMF_SUCCESS

  end subroutine ESMF_BaseDeserializeIDVMId
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IsProxy"
!BOPI
! !IROUTINE: ESMF_IsProxy - Internal access routine to determine whether proxy
!
! !INTERFACE:
  function ESMF_IsProxy(base, rc) 
!
! !RETURN VALUE:
    logical :: ESMF_IsProxy   
!
! !ARGUMENTS:
    type(ESMF_Base), intent(in),  optional :: base
    integer,         intent(out), optional :: rc
!
! !DESCRIPTION:
!      Access proxyflag and return true/false accordingly
!
!     The arguments are:
!     \begin{description}
!     \item [{[base]}]
!           Base object.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
    integer             :: localrc
    type(ESMF_Logical)  :: isProxy

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    rc = ESMF_RC_NOT_IMPL

    ESMF_IsProxy = .false. ! initialize
    
    if (.not.present(base)) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, &
        msg="Base object must be present.", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    else
      call c_ESMC_IsProxy(base, isProxy, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ESMF_IsProxy = isProxy
    endif

    ! Return successfully
    rc = ESMF_SUCCESS

  end function ESMF_IsProxy
!------------------------------------------------------------------------------


end module ESMF_BaseMod
