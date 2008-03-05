! $Id: ESMF_Attribute.F90,v 1.1 2008/03/05 17:21:14 rokuingh Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_Attribute.F90"
!==============================================================================
!
! ESMF Attribute Module
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

module ESMF_AttributeMod
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!BOPI
! !MODULE: ESMF_AttributeMod - Attribute class
!
! !DESCRIPTION:
!
! The code in this file implements the Attribute defined type
!  and functions.  This is an
!  interface to the actual C++ Attribute class implementation 
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
      public ESMF_Attribute, ESMF_Base

!  Attribute methods
      public ESMF_AttributeSet
      public ESMF_AttributeGet
      public ESMF_AttributeGetCount
      public ESMF_AttributeGetbyNumber
      public ESMF_AttributeGetNameList
      public ESMF_AttributeSetList
      public ESMF_AttributeGetList
      public ESMF_AttributeSetObjectList
      public ESMF_AttributeGetObjectList
      public ESMF_AttributeCopy
      public ESMF_AttributeCopyAll


!==============================================================================
!
! INTERFACE BLOCKS
!
!
!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
      character(*), parameter, private :: version = &
               '$Id: ESMF_Attribute.F90,v 1.1 2008/03/05 17:21:14 rokuingh Exp $'
!------------------------------------------------------------------------------

      contains

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AttributeSet"
!BOPI
! !IROUTINE:  ESMF_AttributeSet - set attribute on an ESMF type
!
! !INTERFACE:
  subroutine ESMF_AttributeSet(base, name, value, rc)
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
    !ESMF_INIT_CHECK_DEEP(ESMF_BaseGetInit, base, rc)
      
    !call c_ESMC_AttributeSet(base , name, value, status) 
    if (present(rc)) rc = status

  end subroutine ESMF_AttributeSet


!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AttributeGet"
!BOPI
! !IROUTINE:  ESMF_AttributeGet - get attribute from an ESMF type
!
! !INTERFACE:
  subroutine ESMF_AttributeGet(base, name, value, rc)
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
    !ESMF_INIT_CHECK_DEEP(ESMF_BaseGetInit, base, rc)
      
    !call c_ESMC_AttributeGet(base , name, value, status) 
    if (present(rc)) rc = status

  end subroutine ESMF_AttributeGet


!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AttributeGetCount"
!BOPI
! !IROUTINE:  ESMF_AttributeGetCount - get an ESMF object's number of attributes
!
! !INTERFACE:
  subroutine ESMF_AttributeGetCount(anytype, count, rc)
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
    !ESMF_INIT_CHECK_DEEP(ESMF_BaseGetInit, anytype, rc)

    !call c_ESMC_AttributeGetCount(base , count, status) 
    if (present(rc)) rc = status

  end subroutine ESMF_AttributeGetCount


!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AttributeGetbyNumber"
!BOPI
! !IROUTINE:  ESMF_AttributeGetbyNumber - get an object attribute by number
!
! !INTERFACE:
  subroutine ESMF_AttributeGetbyNumber(anytype, number, name, value, rc)
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
    !ESMF_INIT_CHECK_DEEP(ESMF_BaseGetInit, anytype, rc)
    
    !call c_ESMC_AttributeGetbyNumber(base, number, name, value, status) 
    if (present(rc)) rc = status

  end subroutine ESMF_AttributeGetbyNumber


!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AttributeGetNameList"
!BOPI
! !IROUTINE:  ESMF_AttributeGetNameList - get an object attribute name list
!
! !INTERFACE:
  subroutine ESMF_AttributeGetNameList(anytype, count, namelist, rc)
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

  end subroutine ESMF_AttributeGetNameList


!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AttributeSetList"
!BOPI
! !IROUTINE:  ESMF_AttributeSetList - set an ESMF object's attributes 
!
! !INTERFACE:
  subroutine ESMF_AttributeSetList(anytype, namelist, valuelist, rc)
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

  end subroutine ESMF_AttributeSetList


!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AttributeGetList"
!BOPI
! !IROUTINE:  ESMF_AttributeGetList - get an objects attributes
!
! !INTERFACE:
  subroutine ESMF_AttributeGetList(anytype, namelist, valuelist, rc)
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

  end subroutine ESMF_AttributeGetList


!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AttributeSetObjectList"
!BOPI
! !IROUTINE:  ESMF_AttributeSetObjectList - set an attribute on multiple ESMF objects 
!
! !INTERFACE:
  subroutine ESMF_AttributeSetObjectList(anytypelist, name, value, rc)
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

  end subroutine ESMF_AttributeSetObjectList


!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AttributeGetObjectList"
!BOPI
! !IROUTINE:  ESMF_AttributeGetObjectList - get an attribute from multiple ESMF objects 
!
! !INTERFACE:
  subroutine ESMF_AttributeGetObjectList(anytypelist, name, valuelist, rc)
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

  end subroutine ESMF_AttributeGetObjectList


!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AttributeCopy"
!BOPI
! !IROUTINE:  ESMF_AttributeCopy - copy an attribute between two objects
!
! !INTERFACE:
  subroutine ESMF_AttributeCopy(name, source, destination, rc)
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

  end subroutine ESMF_AttributeCopy


!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AttributeCopyAll"
!BOPI
! !IROUTINE:  ESMF_AttributeCopyAll - copy attributes between two objects
!
! !INTERFACE:
  subroutine ESMF_AttributeCopyAll(source, destination, rc)
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

  end subroutine ESMF_AttributeCopyAll

!-------------------------------------------------------------------------

end module ESMF_AttributeMod

