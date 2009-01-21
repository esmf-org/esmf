! $Id: ESMF_Attribute.F90,v 1.1.2.8 2009/01/21 21:25:24 cdeluca Exp $
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
! The code in this file overloads Attribute types defined in
! other classes with shared, simple public interfaces.
!
! See the ESMF Developers Guide document for more details.
!
!------------------------------------------------------------------------------
! !USES:
  use ESMF_UtilTypesMod     ! ESMF utility types
  use ESMF_InitMacrosMod    ! ESMF initializer macros
  use ESMF_LogErrMod        ! ESMF error handling
  use ESMF_FieldBundleMod
  use ESMF_FieldMod
  use ESMF_GridMod
  use ESMF_ArrayMod
  use ESMF_StateMod


  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
!
! !DESCRIPTION:
!     The following routines apply to any type in the system.  
!     The Attribute routines can be inherited as-is.  The other
!     routines need to be specialized by the higher level objects.
!
! !PUBLIC MEMBER FUNCTIONS:
!

!  Attribute methods
      public ESMF_AttributeSet
      public ESMF_AttributeGet

!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
      character(*), parameter, private :: version = &
               '$Id: ESMF_Attribute.F90,v 1.1.2.8 2009/01/21 21:25:24 cdeluca Exp $'
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_AttributeGet - Get an Attribute
!
! !INTERFACE:
      interface ESMF_AttributeGet 
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldBundleGetI4Attr
        module procedure ESMF_FieldBundleGetI4ListAttr
        module procedure ESMF_FieldBundleGetI8Attr
        module procedure ESMF_FieldBundleGetI8ListAttr
        module procedure ESMF_FieldBundleGetR4Attr
        module procedure ESMF_FieldBundleGetR4ListAttr
        module procedure ESMF_FieldBundleGetR8Attr
        module procedure ESMF_FieldBundleGetR8ListAttr
        module procedure ESMF_FieldBundleGetLogAttr
        module procedure ESMF_FieldBundleGetLogListAttr
        module procedure ESMF_FieldBundleGetCharAttr

        module procedure ESMF_FieldGetInt4Attr
        module procedure ESMF_FieldGetInt4ListAttr
        module procedure ESMF_FieldGetInt8Attr
        module procedure ESMF_FieldGetInt8ListAttr
        module procedure ESMF_FieldGetReal4Attr
        module procedure ESMF_FieldGetReal4ListAttr
        module procedure ESMF_FieldGetReal8Attr
        module procedure ESMF_FieldGetReal8ListAttr
        module procedure ESMF_FieldGetLogicalAttr
        module procedure ESMF_FieldGetLogicalListAttr
        module procedure ESMF_FieldGetCharAttr

        module procedure ESMF_GridGetInt4Attr
        module procedure ESMF_GridGetInt4ListAttr
        module procedure ESMF_GridGetInt8Attr
        module procedure ESMF_GridGetInt8ListAttr
        module procedure ESMF_GridGetReal4Attr
        module procedure ESMF_GridGetReal4ListAttr
        module procedure ESMF_GridGetReal8Attr
        module procedure ESMF_GridGetReal8ListAttr
        module procedure ESMF_GridGetLogicalAttr
        module procedure ESMF_GridGetLogicalListAttr
        module procedure ESMF_GridGetCharAttr

        module procedure ESMF_ArrayGetInt4Attr
        module procedure ESMF_ArrayGetInt4ListAttr
        module procedure ESMF_ArrayGetInt8Attr
        module procedure ESMF_ArrayGetInt8ListAttr
        module procedure ESMF_ArrayGetReal4Attr
        module procedure ESMF_ArrayGetReal4ListAttr
        module procedure ESMF_ArrayGetReal8Attr
        module procedure ESMF_ArrayGetReal8ListAttr
        module procedure ESMF_ArrayGetLogicalAttr
        module procedure ESMF_ArrayGetLogicalListAttr
        module procedure ESMF_ArrayGetCharAttr

        module procedure ESMF_StateGetInt4Attr
        module procedure ESMF_StateGetInt4ListAttr
        module procedure ESMF_StateGetInt8Attr
        module procedure ESMF_StateGetInt8ListAttr
        module procedure ESMF_StateGetReal4Attr
        module procedure ESMF_StateGetReal4ListAttr
        module procedure ESMF_StateGetReal8Attr
        module procedure ESMF_StateGetReal8ListAttr
        module procedure ESMF_StateGetLogicalAttr
        module procedure ESMF_StateGetLogicalListAttr
        module procedure ESMF_StateGetCharAttr

!----------------------------------------------------------------------

        module procedure ESMF_FieldBundleGetAttByName
        module procedure ESMF_FieldBundleGetAttByNum

        module procedure ESMF_FieldGetAttrInfoByName
        module procedure ESMF_FieldGetAttrInfoByNum

        module procedure ESMF_GridGetAttrInfoByName
        module procedure ESMF_GridGetAttrInfoByNum

        module procedure ESMF_ArrayGetAttrInfoByName
        module procedure ESMF_ArrayGetAttrInfoByNum

        module procedure ESMF_StateGetAttrInfoByName
        module procedure ESMF_StateGetAttrInfoByNum

!----------------------------------------------------------------------

        module procedure ESMF_FieldBundleGetAttCount
        module procedure ESMF_FieldGetAttributeCount
        module procedure ESMF_GridGetAttributeCount
        module procedure ESMF_ArrayGetAttributeCount
        module procedure ESMF_StateGetAttributeCount


! !DESCRIPTION:
!     This interface provides a single entry point for methods that retrieve
!     attributes and attribute information.
 
!EOPI
      end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_AttributeSet - Set an attribute
!
! !INTERFACE:
      interface ESMF_AttributeSet
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldBundleSetI4Attr
        module procedure ESMF_FieldBundleSetI4ListAttr
        module procedure ESMF_FieldBundleSetI8Attr
        module procedure ESMF_FieldBundleSetI8ListAttr
        module procedure ESMF_FieldBundleSetR4Attr
        module procedure ESMF_FieldBundleSetR4ListAttr
        module procedure ESMF_FieldBundleSetR8Attr
        module procedure ESMF_FieldBundleSetR8ListAttr
        module procedure ESMF_FieldBundleSetLogAttr
        module procedure ESMF_FieldBundleSetLogListAttr
        module procedure ESMF_FieldBundleSetCharAttr

        module procedure ESMF_FieldSetInt4Attr
        module procedure ESMF_FieldSetInt4ListAttr
        module procedure ESMF_FieldSetInt8Attr
        module procedure ESMF_FieldSetInt8ListAttr
        module procedure ESMF_FieldSetReal4Attr
        module procedure ESMF_FieldSetReal4ListAttr
        module procedure ESMF_FieldSetReal8Attr
        module procedure ESMF_FieldSetReal8ListAttr
        module procedure ESMF_FieldSetLogicalAttr
        module procedure ESMF_FieldSetLogicalListAttr
        module procedure ESMF_FieldSetCharAttr

        module procedure ESMF_GridSetInt4Attr
        module procedure ESMF_GridSetInt4ListAttr
        module procedure ESMF_GridSetInt8Attr
        module procedure ESMF_GridSetInt8ListAttr
        module procedure ESMF_GridSetReal4Attr
        module procedure ESMF_GridSetReal4ListAttr
        module procedure ESMF_GridSetReal8Attr
        module procedure ESMF_GridSetReal8ListAttr
        module procedure ESMF_GridSetLogicalAttr
        module procedure ESMF_GridSetLogicalListAttr
        module procedure ESMF_GridSetCharAttr

        module procedure ESMF_ArraySetInt4Attr
        module procedure ESMF_ArraySetInt4ListAttr
        module procedure ESMF_ArraySetInt8Attr
        module procedure ESMF_ArraySetInt8ListAttr
        module procedure ESMF_ArraySetReal4Attr
        module procedure ESMF_ArraySetReal4ListAttr
        module procedure ESMF_ArraySetReal8Attr
        module procedure ESMF_ArraySetReal8ListAttr
        module procedure ESMF_ArraySetLogicalAttr
        module procedure ESMF_ArraySetLogicalListAttr
        module procedure ESMF_ArraySetCharAttr

        module procedure ESMF_StateSetInt4Attr
        module procedure ESMF_StateSetInt4ListAttr
        module procedure ESMF_StateSetInt8Attr
        module procedure ESMF_StateSetInt8ListAttr
        module procedure ESMF_StateSetReal4Attr
        module procedure ESMF_StateSetReal4ListAttr
        module procedure ESMF_StateSetReal8Attr
        module procedure ESMF_StateSetReal8ListAttr
        module procedure ESMF_StateSetLogicalAttr
        module procedure ESMF_StateSetLogicalListAttr
        module procedure ESMF_StateSetCharAttr

! !DESCRIPTION:
!     This interface provides a single entry point for methods that attach
!     attributes to FieldBundles and other objects.
 
!EOPI
      end interface
!
!------------------------------------------------------------------------------

      contains

!------------------------------------------------------------------------------

! This method is here to prevent the compiler from complaining about no
! methods in the module.

      subroutine ESMF_BlankRoutine(rc)
      integer, intent(out), optional :: rc

      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      end subroutine ESMF_BlankRoutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AttributeGet  - Retrieve the value of an Attribute
!
! !INTERFACE:
!     subroutine ESMF_AttributeGet(<object>, name, <value argument>, rc)
!
! !ARGUMENTS:
!     <object>, see below for supported values
!     character (len = *), intent(in) :: name
!     <value argument>, see below for supported values
!     integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Returns the value of an Attribute given its name.
!
!     Supported values for <object> are:
!     \begin{description}
!     \item type(ESMF\_Array), intent(inout) :: array
!     \item type(ESMF\_Grid), intent(inout) :: grid
!     \item type(ESMF\_Field), intent(inout) :: field
!     \item type(ESMF\_FieldBundle), intent(inout) :: fieldbundle
!     \item type(ESMF\_State), intent(inout) :: state
!     \end{description}
!
!     Supported values for <value argument> are:
!     \begin{description}
!     \item integer(ESMF\_KIND\_I4), intent(out) :: value
!     \item integer(ESMF\_KIND\_I8), intent(out) :: value
!     \item real (ESMF\_KIND\_R4), intent(out) :: value
!     \item real (ESMF\_KIND\_R8), intent(out) :: value
!     \item type(ESMF\_Logical), intent(out) :: value
!     \item character (len = *), intent(out), value
!     \end{description}
!
!     The arguments are:
!     \begin{description}
!     \item [<object>]
!           The object containing the Attribute.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [<value argument>]
!           The value of the named Attribute.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AttributeGet  - Retrieve the values of a list Attribute
!
! !INTERFACE:
!     subroutine ESMF_AttributeGet(<object>, name, count, <value argument>, rc)
!
! !ARGUMENTS:
!     <object>, see below for supported values
!     character (len = *), intent(in) :: name
!     integer, intent(in) :: count
!     <value argument>, see below for supported values
!     integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Returns the values of a list Attribute given its name.
!
!     Supported values for <object> are:
!     \begin{description}
!     \item type(ESMF\_Array), intent(inout) :: array
!     \item type(ESMF\_Grid), intent(inout) :: grid
!     \item type(ESMF\_Field), intent(inout) :: field
!     \item type(ESMF\_FieldBundle), intent(inout) :: fieldbundle
!     \item type(ESMF\_State), intent(inout) :: state
!     \end{description}
!
!     Supported values for <value argument> are:
!     \begin{description}
!     \item integer(ESMF\_KIND\_I4), dimension(:), intent(out) :: valueList
!     \item integer(ESMF\_KIND\_I8), dimension(:), intent(out) :: valueList
!     \item real (ESMF\_KIND\_R4), dimension(:), intent(out) :: valueList
!     \item real (ESMF\_KIND\_R8), dimension(:), intent(out) :: valueList
!     \item type(ESMF\_Logical), dimension(:), intent(out) :: valueList
!     \end{description}
!
!     The arguments are:
!     \begin{description}
!     \item [<object>]
!           The object containing the Attribute.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [<value argument>]
!           The value of the named Attribute.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AttributeGet - Query the number of Attributes
!
! !INTERFACE:
!     subroutine ESMF_AttributeGet(<object>, count, rc)
!
! !ARGUMENTS:
!     <object>, see below for supported values
!     integer, intent(out) :: count
!     integer, intent(out), optional :: rc
!
!
! !DESCRIPTION:
!     Returns the number of Attributes associated with the given object
!     in the argument {\tt count}.
!
!     Supported values for <object> are:
!     \begin{description}
!     \item type(ESMF\_Array), intent(inout) :: array
!     \item type(ESMF\_Grid), intent(inout) :: grid
!     \item type(ESMF\_Field), intent(inout) :: field
!     \item type(ESMF\_FieldBundle), intent(inout) :: fieldbundle
!     \item type(ESMF\_State), intent(inout) :: state
!     \end{description}
!
!     The arguments are:
!     \begin{description}
!     \item [<object>]
!           The object to be queried.
!     \item [count]
!           The number of Attributes associated with this object.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AttributeGet - Query an Attribute by name
!
! !INTERFACE:
!     subroutine ESMF_AttributeGet(<object>, name, typekind, count, rc)
!
! !ARGUMENTS:
!      
!       <object>, see below for supported values
!       character(len=*), intent(in) :: name
!       type(ESMF_TypeKind), intent(out), optional :: typekind
!       integer, intent(out), optional :: count
!       integer, intent(out), optional :: rc
! 
!
! !DESCRIPTION:
!     Returns information associated with the named Attribute,
!     including {\tt typekind} and {\tt count}.
!
!     Supported values for <object> are:
!     \begin{description}
!     \item type(ESMF\_Array), intent(inout) :: array
!     \item type(ESMF\_Grid), intent(inout) :: grid
!     \item type(ESMF\_Field), intent(inout) :: field
!     \item type(ESMF\_FieldBundle), intent(inout) :: fieldbundle
!     \item type(ESMF\_State), intent(inout) :: state
!     \end{description}
!
!     The arguments are:
!     \begin{description}
!     \item [<object>]
!           The object containing the Attribute.
!     \item [name]
!           The name of the Attribute to query.
!     \item [{[typekind]}]
!           The typekind of the Attribute.
!     \item [{[count]}]
!           The number of items in this Attribute.  For character types,
!           the length of the character string.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AttributeGet - Query an Attribute by index number
!
! !INTERFACE:
!      subroutine ESMF_AttributeGet(<object>, attributeIndex, name, &
!        typekind, itemcount, rc)
!
! !ARGUMENTS:
!      <object>, see below for supported values
!      integer, intent(in) :: attributeIndex
!      character(len=*), intent(out) :: name
!      type(ESMF_TypeKind), intent(out), optional :: typekind
!      integer, intent(out), optional :: itemcount
!      integer, intent(out), optional :: rc
!
!
! !DESCRIPTION:
!     Returns information associated with the indexed Attribute,
!     including {\tt name}, {\tt typekind} and {\tt itemcount}.
!
!     Supported values for <object> are:
!     \begin{description}
!     \item type(ESMF\_Array), intent(inout) :: array
!     \item type(ESMF\_Grid), intent(inout) :: grid
!     \item type(ESMF\_Field), intent(inout) :: field
!     \item type(ESMF\_FieldBundle), intent(inout) :: fieldbundle
!     \item type(ESMF\_State), intent(inout) :: state
!     \end{description}
!
!     The arguments are:
!     \begin{description}
!     \item [<object>]
!           The object to be queried.
!     \item [attributeIndex]
!           The index number of the Attribute to query.
!     \item [name]
!           Returns the name of the Attribute.
!     \item [{[typekind]}]
!           The typekind of the Attribute.
!     \item [{[itemcount]}]
!           Returns the number of items in this Attribute.  For character types,
!           this is the length of the character string.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AttributeSet - Set the value of an Attribute
!
! !INTERFACE:
!     subroutine ESMF_AttributeSet(<object>, name, <value argument>, rc)
!
! !ARGUMENTS:
!     <object>, see below for supported values
!     character (len = *), intent(in) :: name
!     <value argument>, see below for supported values
!     integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Sets the value of an Attribute.
!
!     Supported values for <object> are:
!     \begin{description}
!     \item type(ESMF\_Array), intent(inout) :: array
!     \item type(ESMF\_Grid), intent(inout) :: grid
!     \item type(ESMF\_Field), intent(inout) :: field
!     \item type(ESMF\_FieldBundle), intent(inout) :: fieldbundle
!     \item type(ESMF\_State), intent(inout) :: state
!     \end{description}
!
!     Supported values for the <value argument> are:
!     \begin{description}
!     \item integer(ESMF\_KIND\_I4), intent(in) :: value
!     \item integer(ESMF\_KIND\_I8), intent(in) :: value
!     \item real (ESMF\_KIND\_R4), intent(in) :: value
!     \item real (ESMF\_KIND\_R8), intent(in) :: value
!     \item type(ESMF\_Logical), intent(in) :: value
!     \item character (len = *), intent(in), value
!     \end{description}
!
!     The arguments are:
!     \begin{description}
!     \item [<object>]
!           The object containing the Attribute to set.
!     \item [name]
!           The name of the Attribute to set.
!     \item [<value argument>]
!           The value of the Attribute to set.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AttributeSet - Set the values of a list Attribute
!
! !INTERFACE:
!     subroutine ESMF_AttributeSet(<object>, name, count, <value argument>, rc)
!
! !ARGUMENTS:
!     <object>, see below for supported values
!     character (len = *), intent(in) :: name
!     integer, intent(in) :: count
!     <value argument>, see below for supported values
!     integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Sets the value of an Attribute.
!
!     Supported values for <object> are:
!     \begin{description}
!     \item type(ESMF\_Array), intent(inout) :: array
!     \item type(ESMF\_Grid), intent(inout) :: grid
!     \item type(ESMF\_Field), intent(inout) :: field
!     \item type(ESMF\_FieldBundle), intent(inout) :: fieldbundle
!     \item type(ESMF\_State), intent(inout) :: state
!     \end{description}
!
!     Supported values for the <value argument> are:
!     \begin{description}
!     \item integer(ESMF\_KIND\_I4), dimension(:), intent(in) :: valueList
!     \item integer(ESMF\_KIND\_I8), dimension(:), intent(in) :: valueList
!     \item real (ESMF\_KIND\_R4), dimension(:), intent(in) :: valueList
!     \item real (ESMF\_KIND\_R8), dimension(:), intent(in) :: valueList
!     \item type(ESMF\_Logical), dimension(:), intent(in) :: valueList
!     \end{description}
!
!     The arguments are:
!     \begin{description}
!     \item [<object>]
!           The object containing the Attribute to set.
!     \item [name]
!           The name of the Attribute to set.
!     \item [count]
!           The number of values in the Attribute.
!     \item [<value argument>]
!           The value of the Attribute to set.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
!------------------------------------------------------------------------------

end module ESMF_AttributeMod

