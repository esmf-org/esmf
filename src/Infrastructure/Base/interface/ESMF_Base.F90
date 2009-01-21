! $Id: ESMF_Base.F90,v 1.134.2.4 2009/01/21 21:25:19 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
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
!    ! Dummy structure which must just be big enough to hold the values.
!    ! actual data values will always be accessed on the C++ side.

  type ESMF_Attribute
  sequence
  private
    character(len=ESMF_MAXSTR)  :: attr_name
    type(ESMF_DataValue)        :: attr_value
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
       public ESMF_Base, ESMF_Attribute

!   Base class methods
       public ESMF_BaseCreate
       public ESMF_BaseDestroy
   
!      public ESMF_BaseGetInstCount

!      public ESMF_BaseSetID
!      public ESMF_BaseGetID

!      public ESMF_BaseSetRefCount
!      public ESMF_BaseGetRefCount

!      public ESMF_BaseSetStatus
!      public ESMF_BaseGetStatus

       public ESMF_BasePrint
       public ESMF_BaseValidate
       
       public ESMF_BaseGetInit

!   Virtual methods to be defined by derived classes
!      public ESMF_Read
!      public ESMF_Write
!      public ESMF_Validate
!      public ESMF_Print

!  Attribute methods
      public ESMF_BaseAttSet
      public ESMF_BaseAttGet
      public ESMF_BaseAttGetCount
      public ESMF_BaseAttGetbyNumber
      public ESMF_BaseAttGetNameList
      public ESMF_BaseAttSetList
      public ESMF_BaseAttGetList
      public ESMF_BaseAttSetObjectList
      public ESMF_BaseAttGetObjectList
      public ESMF_BaseAttCopy
      public ESMF_BaseAttCopyAll

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
               '$Id: ESMF_Base.F90,v 1.134.2.4 2009/01/21 21:25:19 cdeluca Exp $'
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
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BaseAttSet"
!BOPI
! !IROUTINE:  ESMF_BaseAttSet - set attribute on an ESMF type
!
! !INTERFACE:
  subroutine ESMF_BaseAttSet(base, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: base
      character (len = *), intent(in) :: name 
      type(ESMF_DataValue), intent(in) :: value 
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!     Associate a (name,value) pair with any type in the system.
!
!     The arguments are:
!     \begin{description}
!     \item[base]
!       Any ESMF type.
!     \item[name]
!       The name of the attribute to set.
!     \item[value]
!       The value of the attribute.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

    integer :: status 

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    status = ESMF_RC_NOT_IMPL
      
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_BaseGetInit, base, rc)
      
    !call c_ESMC_BaseAttSet(base , name, value, status) 
    if (present(rc)) rc = status

  end subroutine ESMF_BaseAttSet


!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BaseAttGet"
!BOPI
! !IROUTINE:  ESMF_BaseAttGet - get attribute from an ESMF type
!
! !INTERFACE:
  subroutine ESMF_BaseAttGet(base, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: base 
      character (len = *), intent(in) :: name 
      type(ESMF_DataValue), intent(out) :: value 
      integer, intent(out), optional :: rc 

!
! !DESCRIPTION: Get a (name,value) pair with any type in the system.
!
!
!     The arguments are:
!     \begin{description}
!     \item[base]
!       Any ESMF type.
!     \item[name]
!       The name of the attribute to get.
!     \item[value]
!       The value of the attribute.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

    integer :: status 

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    status = ESMF_RC_NOT_IMPL
      
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_BaseGetInit, base, rc)
      
    !call c_ESMC_BaseAttGet(base , name, value, status) 
    if (present(rc)) rc = status

  end subroutine ESMF_BaseAttGet


!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BaseAttGetCount"
!BOPI
! !IROUTINE:  ESMF_BaseAttGetCount - get an ESMF object's number of attributes
!
! !INTERFACE:
  subroutine ESMF_BaseAttGetCount(anytype, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: anytype             ! any ESMF type
      integer, intent(out) :: count                      ! attribute count
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Returns number of attributes present.
!
!     The arguments are:
!     \begin{description}
!     \item[anytype]
!       Any ESMF type.
!     \item[count]
!       The number of attributes.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

    integer :: status 

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    status = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_BaseGetInit, anytype, rc)

    !call c_ESMC_BaseAttGetCount(base , count, status) 
    if (present(rc)) rc = status

  end subroutine ESMF_BaseAttGetCount


!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BaseAttGetbyNumber"
!BOPI
! !IROUTINE:  ESMF_BaseAttGetbyNumber - get an object attribute by number
!
! !INTERFACE:
  subroutine ESMF_BaseAttGetbyNumber(anytype, number, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: anytype
      integer, intent(in) :: number
      character (len = *), intent(in) :: name
      type(ESMF_DataValue), intent(out) :: value
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
! Allows the caller to get attributes by number instead of by name.
! This can be useful in iterating through all attributes in a loop.
!
!
!     The arguments are:
!     \begin{description}
!     \item[anytype]
!       Any ESMF type.
!     \item[number]
!       The attribute number.
!     \item[name]
!       The attribute name.
!     \item[value]
!       The attribute value.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!
!EOPI
    integer :: status 

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    status = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_BaseGetInit, anytype, rc)
    
    !call c_ESMC_BaseAttGetbyNumber(base, number, name, value, status) 
    if (present(rc)) rc = status

  end subroutine ESMF_BaseAttGetbyNumber


!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BaseAttGetNameList"
!BOPI
! !IROUTINE:  ESMF_BaseAttGetNameList - get an object attribute name list
!
! !INTERFACE:
  subroutine ESMF_BaseAttGetNameList(anytype, count, namelist, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: anytype
      integer, intent(out) :: count
      character (len = *), dimension (:), intent(inout) :: namelist
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
! Return a list of all attribute names without returning the values.
!
!     The arguments are:
!     \begin{description}
!     \item[anytype]
!       Any ESMF type.
!     \item[count]
!       The number of attributes.
!     \item[namelist]
!       The list of attribute names.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

      !TODO: when code added here, change (inout) for namelist to just out.
      ! absoft compiler was unhappy.

  end subroutine ESMF_BaseAttGetNameList


!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BaseAttSetList"
!BOPI
! !IROUTINE:  ESMF_BaseAttSetList - set an ESMF object's attributes 
!
! !INTERFACE:
  subroutine ESMF_BaseAttSetList(anytype, namelist, valuelist, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: anytype
      character (len = *), dimension (:), intent(in) :: namelist
      type(ESMF_DataValue), dimension (:), intent(in) :: valuelist
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
! Set multiple attributes on an object in one call.  Depending on what is
! allowed by the interface, all attributes may have to have the same type.
!
!
!     The arguments are:
!     \begin{description}
!     \item[anytype]
!       Any ESMF type.
!     \item[namelist]
!       The list of attribute names.
!     \item[valuelist]
!       The list of attribute values.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

  end subroutine ESMF_BaseAttSetList


!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BaseAttGetList"
!BOPI
! !IROUTINE:  ESMF_BaseAttGetList - get an objects attributes
!
! !INTERFACE:
  subroutine ESMF_BaseAttGetList(anytype, namelist, valuelist, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: anytype
      character (len = *), dimension (:), intent(in) :: namelist
      type(ESMF_DataValue), dimension (:), intent(out) :: valuelist
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
! Get multiple attributes from an object in a single call.
!
!
!     The arguments are:
!     \begin{description}
!     \item[anytype]
!       Any ESMF type.
!     \item[namelist]
!       The list of attribute names.
!     \item[valuelist]
!       The list of attribute values.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

  end subroutine ESMF_BaseAttGetList


!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BaseAttSetObjectList"
!BOPI
! !IROUTINE:  ESMF_BaseAttSetObjectList - set an attribute on multiple ESMF objects 
!
! !INTERFACE:
  subroutine ESMF_BaseAttSetObjectList(anytypelist, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), dimension (:), intent(in) :: anytypelist
      character (len = *), intent(in) :: name
      type(ESMF_DataValue), dimension (:), intent(in) :: value
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
! Set the same attribute on multiple objects in one call.
!
!     The arguments are:
!     \begin{description}
!     \item[anytypelist]
!       A list of any ESMF types.
!     \item[name]
!       The attribute name.
!     \item[value]
!       The attribute value.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

  end subroutine ESMF_BaseAttSetObjectList


!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BaseAttGetObjectList"
!BOPI
! !IROUTINE:  ESMF_BaseAttGetObjectList - get an attribute from multiple ESMF objects 
!
! !INTERFACE:
  subroutine ESMF_BaseAttGetObjectList(anytypelist, name, valuelist, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), dimension (:), intent(in) :: anytypelist
      character (len = *), intent(in) :: name
      type(ESMF_DataValue), dimension (:), intent(out) :: valuelist
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
! Get the same attribute name from multiple objects in one call.
!
!     The arguments are:
!     \begin{description}
!     \item[anytypelist]
!       The list of any ESMF types.
!     \item[name]
!       The attribute name.
!     \item[valuelist]
!       The list of attribute values.
!     \item[{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

  end subroutine ESMF_BaseAttGetObjectList


!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BaseAttCopy"
!BOPI
! !IROUTINE:  ESMF_BaseAttCopy - copy an attribute between two objects
!
! !INTERFACE:
  subroutine ESMF_BaseAttCopy(name, source, destination, rc)
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name
      type(ESMF_Base), intent(in) :: source
      type(ESMF_Base), intent(in) :: destination
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
! The specified attribute associated with the source object is
! copied to the destination object.  << does this assume overwriting the
! attribute if it already exists in the output or does this require yet
! another arg to say what to do with collisions? >>
! 
!     The arguments are:
!     \begin{description}
!     \item[name]
!       The attribute name.
!     \item[source]
!       The source ESMF object.
!     \item[destination]
!       The destination ESMF object.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

  end subroutine ESMF_BaseAttCopy


!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BaseAttCopyAll"
!BOPI
! !IROUTINE:  ESMF_BaseAttCopyAll - copy attributes between two objects
!
! !INTERFACE:
  subroutine ESMF_BaseAttCopyAll(source, destination, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: source
      type(ESMF_Base), intent(in) :: destination
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
! All attributes associated with the source object are copied to the
! destination object.  Some attributes will have to be considered
! {\tt read only} and won't be updated by this call.  (e.g. an attribute
! like {\tt name} must be unique and therefore can't be duplicated.)
!
!
!     The arguments are:
!     \begin{description}
!     \item[source]
!       The source ESMF object.
!     \item[destination]
!       The destination ESMF object.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

  end subroutine ESMF_BaseAttCopyAll

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
!     Interface to call through to C++ and print base object values.
!
!     Note:  Many {\tt ESMF\_<class>Print} methods are implemented in C++.
!     On some platforms/compilers there is a potential issue with interleaving
!     Fortran and C++ output to {\tt stdout} such that it doesn't appear in
!     the expected order.  If this occurs, it is recommended to use the
!     standard Fortran call {\tt flush(6)} as a workaround until this issue
!     is fixed in a future release. 
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

end module ESMF_BaseMod
